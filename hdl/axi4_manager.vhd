-----------------------------------------------------------------------------   
-- Entity:      axi4_manager
-- File:        axi4_manager.vhd
-- Author:      Francis Fuentes Diaz (BSC-CNS)
-- Description: AXI4 full manager entity.
------------------------------------------------------------------------------ 
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library bsc;
use bsc.axi4_pkg.all;

-----------------------------------------------------------------------------
-- AXI4 FULL Manager - generic bus manager bridge
--
-- Manager specifications and considerations during integration:
--
-- - AXI4_DATA_WIDTH >= dbits.
-- - dbits <= 128.
-- - BM size requests <= 4096 (4kB, limited by AXI4 addressing rule).
-- - Little endian data structure
-- - Only INC burst mode is implemented.
-- - Unaligned access is supported by aligning the address with AXI4_DATA_WIDTH and 
--   delivering only the requested data on reads and using the write strobe on AXI writes.
-- - The Manager only execute bursts with the AXI size that use the whole AXI data bus width.
--
-- This AXI4 Manager interface translates and manage the requests made by the BM component,
-- using the BM input and output buses, to the AXI4 network, using the AXI4 input and output buses.
--
-- The BM component has an opaque vision of what the interface requests to the AXI network, since 
-- it only receives the data that it has been requested. Be it requests with unaligned or even multiple 
-- subordinates access (only 2 as maximum), the interface filters it to deliver only the requested data.
-- 
-- All I/O ports are buffered on registers to promote higher maximum frequency of operation at 
-- implementation, with minimum logic between register and BM or AXI bus.
-- 
-- The interface may send control and data when the valid flag is not asserted. These signals 
-- must be discarded since the valid flag is low.
-- 
-- The minimum latency for read transactions is:
--  1 clock cycle  for the BM component request being active. (BM control is registered)
--  3 clock cycles for computing and buffering the AXI control data for the handshake.
--  1 clock cycle  for executing the handshake with an AXI subordinate.
--  1 clock cycle  for reading first AXI beat.
--  4 clock cycles for buffering (AXI input, BM output) and managing the data.
--  1 clock cycle  for delivering the first BM transfer.
-- From requesting a read transaction to receiving the first BM transfer, the execution has a latency of
-- 11 clock cycles, including both request and data send clock cycles. Further data transfer occurs without 
-- interruption if the AXI subordinate is able to transfer enough data to sustain the interface transfering 
-- to the BM component.
-- Of course, this performance can only be achieved on direct communication with subordinates that have an 
-- active grant read request by default, transmit first beat of data on the clock cycle after the handshake 
-- process and on that beat, there's enough data to execute a BM transfer of dbits bits.
-- 
-----------------------------------------------------------------------------

entity axi4_manager is
  generic (
    -- Bus Manager configuration
    dbits           : integer range 32 to  128  := 32;      -- BM data bus width [Only power of 2s are allowed]
    -- AXI Manager configuration
    axi_id          : integer                   := 0;       -- AXI manager index
    MAX_SIZE_BURST  : integer range 32 to 4096  := 4096;    -- Maximum size of a BM transaction. [Only power of 2s are allowed]
    -- Injector configuration
    ASYNC_RST       : boolean                   := FALSE    -- Allow asynchronous reset
  );
  port (
    rstn            : in  std_ulogic;         -- Reset
    clk             : in  std_ulogic;         -- Clock
    -- AXI interface signals
    axi4mi          : in  axi4_in_type;       -- AXI4 manager input 
    axi4mo          : out axi4_out_type;      -- AXI4 manager output
    -- BM component signals
    bm_in           : in  bm_in_type;         -- BM interface input
    bm_out          : out bm_out_type;        -- BM interface output
    -- Manager settings
    bypass_rd_bm    : in  std_logic           -- Skip BM transfers, discard all read data and write full ones. WIP
  );
end entity axi4_manager;

architecture rtl of axi4_manager is
  -----------------------------------------------------------------------------
  -- Constant declaration
  -----------------------------------------------------------------------------

  -- AXI constants
  constant AXI4_DATA_WIDTH  : integer := axi4mo.w_data'length;      -- AXI data bus width
  constant AXI4_DATA_BYTE   : integer := log_2(AXI4_DATA_WIDTH/8);  -- AXI data bytes addressing bus width
  constant AXI4_FDATA_BYTE  : integer := max((AXI4_DATA_BYTE, log_2(dbits/8)))+1; -- To acommodate dbits larger than AXI data bus widths.

  constant FIXED            : std_logic_vector(1 downto 0) := "00"; -- AXI burst modes: FIXED
  constant INC              : std_logic_vector(1 downto 0) := "01"; --                  INCREMENTAL
  constant WRAP             : std_logic_vector(1 downto 0) := "10"; --                  WRAP
  constant size_array       : array_integer(0 to 7) := (1, 2, 4, 8, 16, 32, 64, 128); -- AXI size transfer modes
  constant mask_array       : array_128vector(0 to 7) := ( 
    -- Array with all the possible base byte masks for AXI transfers.
    0 => (           0 => '1', others => '0'),
    1 => (  1 downto 0 => '1', others => '0'),
    2 => (  3 downto 0 => '1', others => '0'),
    3 => (  7 downto 0 => '1', others => '0'),
    4 => ( 15 downto 0 => '1', others => '0'),
    5 => ( 31 downto 0 => '1', others => '0'),
    6 => ( 63 downto 0 => '1', others => '0'),
    7 => (127 downto 0 => '1', others => '0')
    );

    -- The Mult_bursts_subor flag indicates when TRUE that the implementation allows for the possible requirement of multiple bursts to the same 
    -- subordinate (same 4kB memory space) to satisfy the size requested by the BM component (maximum 4096 bytes, encoded as 0xFFF in bm_in.bm_size).
    constant Mult_bursts_subor      : boolean                    := DATA_WIDTH < 128;

  -----------------------------------------------------------------------------
  -- Records and types
  -----------------------------------------------------------------------------
  --
  -- This interface, when idle, accepts read and write transaction requests from the BM bus, listening the starting address 
  -- and the total transfer size of the operation. This last must be lower or equal to the set by MAX_SIZE_BURST-1 and it 
  -- is encoded as an unsigned integer decremented by one respect the real size, so 0x0 means 1 byte transfer.
  --
  -- Once the request has been granted (deassert granting new requests for that type of transaction until finished), the interface processes 
  -- the control data to check if the total transfer surpasses the 4kB address boundary, splitting the transaction in two batches 
  -- in such situations. This is done to arrange the transfer sizes of the batches for the first and second AXI subordinates, having 
  -- each different 4kB address allocation spaces.
  -- 
  -- Then, the interface sets a size mode to use the whole data bus width of the AXI side (AXI4_DATA_WIDTH), computes the number of beats 
  -- (burst length) to satisfy the request transfer and aligns the address requested (bm_addr) with the AXI data bus width (AXI4_DATA_WIDTH).
  -- In case a single burst is not enough to satisfy the transfer size for the subordinate in question, the maximum length will be set (256 beats).
  -- However, this is only possible in implementations that have an AXI4_DATA_WIDTH < 128 bits. Thus, this is managed with the "Mult_bursts_subor"
  -- flag to only proceed with the correspondent checks to generate multiple bursts to the same subordinate in the implementation if it's the case.
  --
  -- Since the BM component may request access to an unaligned address respect the AXI4_DATA_WIDTH slot, the interface rearrenges the data, to only 
  -- transfer to the BM component the data from the address requested on read transactions and to start writting from the address requested using 
  -- the byte strobe (wr.axi_strb) on write transactions.
  --
  -- After a 
  --
  -- The following table points out the actions done at each stage of the operation:
  -- 
  -- IDLE     -> Accept new requests from BM component.
  -- COMPUTE1 -> Compute if 4KB address space overflow occurs calculating the ending address of the BM request.
  --             In the case it does, assert two_subor flag so a second burst is generated after completing the first.
  --             Also, split the total byte size of the transfer in both first and last bursts taking into account the 4kB boundary.
  --             Register the number of bytes to transfer at the last BM transfer of the whole transaction. This value is used to generate
  --             the mask applied on this last BM transfer, so unrequested data is not sent to the BM component.
  -- COMPUTE2 -> Set the AXI size mode to use the whole AXI data bus width (log2(AXI4_DATA_WIDTH/8)). 
  --             Also, generate an aligned address with the AXI4_DATA_WIDTH slot to be used at the AXI network handshake.
  --             Set the burst mode (at the moment, there's only INC).
  --             Decide burst length (number of beats) taking into the number of bytes to transfer and the address requested.
  --          WR:Set the strobe bits for WRITE transaction.
  -- HANDSHAKE-> 
  --          RD:Waits for "RREADY" to load data from the AXI data bus. Loads counter with LSB aligned starting address,
  --             so the first AXI beat takes into account that it may be reading from an unaligned.
  -- TRANSFER ->
  --          RD:Reads the AXI data bus and register it onto a buffer register, which it is written onto a FIFO register.
  --             In the case where all FIFO registers are full, it stops the read on the AXI data bus by deasserting rd.axi_ready.
  --             Manages each AXI beat transfer logic by asserting the r_ready flag on
  --          WR:
  -- TRANSFER2->
  --          RD:Depending if it has been the last beat of the burst and if another burst must be made to complete the BM request,
  --             it returns to idle, compute2 or transfer1. In the latter case, an index is increased to change the fifo register,
  --             so new read data does not overwrite previous data that has yet to be transfered to the BM component.
  --             Every time transfer1 fills a fifo register, the BM transfer logic starts to process the read data and delivers it 
  --             to the BM component in order and only the data that it has been requested. once it finishes with the register, it deasserts 
  --             a flag specific of that register indicating that more data can be read onto it. However, this process can be skipped if the 
  --             skip_BM_transf input signal is asserted, discarding all remaining read data (WIP).
  --          WR:
  --
  -- Outside of 
  -- the FSM ->
  --         RD:A FIFO memory is used to store each beat transfered from the AXI read transaction, no matter from which subordinate.
  --            The BM transfer logic checks out if any more data is available from the RD FIFO to BM transfer, sending the data and 
  --            BM control signals bm_valid and bm_done. This last indicating the completion of the whole transaction.
  --            

  type rd_data_buffer is array (natural range <>) of std_logic_vector( AXI4_DATA_WIDTH - 1 downto 0   );
  type transf_state is (idle, compute1, compute2, compute3, handshake, transfer);

  type transfer_rd_operation is record
    state         : transf_state; -- State of the read transaction.
    bm_grant      : std_logic;    -- Grant signals to BM component.
    bm_error      : std_logic;    -- Error flag register due to SLVERR or DECERR subordinate response.

    axi_mode      : std_logic_vector(1 downto 0); -- AXI output parameter: burst mode (FIXED, INC, WRAP).
    axi_size      : std_logic_vector(2 downto 0); -- AXI output parameter: size mode of each beat in the burst.
    axi_len       : std_logic_vector(7 downto 0); -- AXI output parameter: number of beats in the burst.
    axi_addr      : std_logic_vector(ADDR_WIDTH      - 1 downto 0); -- AXI output parameter: Starting pointer of the AXI burst.
    axi_valid     : std_logic;                    -- AXI output parameter: valid flag for output control signals (addr, len, size, mode).
    axi_ready     : std_logic;                    -- AXI output parameter: ready flag for input read data.
    axi_last      : std_logic;                    -- AXI input  parameter: last transaction flag of the burst.
    axi_counter   : std_logic_vector(AXI4_FDATA_BYTE     downto 0); -- Used to count the number of bytes transfered/discarded from AXI transfer.

    first_beat    : std_logic; -- Flag asserted when computing first beat of the first burst.
    two_subor     : std_logic; -- Flag asserted when two bursts are required to complete the transaction due to surpassing the 4KB boundary.
    addr_end      : std_logic_vector(11 downto 0);-- Last address to access in INC mode. Only used to separate logic between computation1 and 2 stages.

    bm_size       : std_logic_vector(sel(12, 11, Mult_bursts_subor) downto 0);-- Original size being requested by BM component, and then first burst size.
    rem_size      : std_logic_vector(11 downto 0);-- Remaining size for second burst (4KB outbounds access)
    bm_addr       : std_logic_vector(ADDR_WIDTH      - 1 downto 0); -- Starting pointer requested by BM component.

    fifo          : rd_data_buffer(rd_n_fifo_regs    - 1 downto 0); -- AXI data bus registers which filters narrow reads from data_tmp.
    fifo_full     : std_logic_vector(rd_n_fifo_regs  - 1 downto 0); -- When asserted, the fifo(axi_index) is full or at a BM transfer.
    fifo_last     : std_logic_vector(rd_n_fifo_regs  - 1 downto 0); -- When asserted, that the fifo(axi_index) is the last BM transfer.
    axi_index     : std_logic_vector(log_2(rd_n_fifo_regs) - 1 downto 0); -- Unsigned index of fifo registers for the AXI side.
    bm_index      : std_logic_vector(log_2(rd_n_fifo_regs) - 1 downto 0); -- Unsigned index of fifo registers for the BM side.

    bm_first      : std_logic;                                      -- Flag asserted until the first BM transfer has been done.
    bm_counter    : std_logic_vector(AXI4_FDATA_BYTE     downto 0); -- Used to count the number of bytes to BM transfer.
    start_strb    : std_logic_vector(AXI4_FDATA_BYTE     downto 0); -- Number of right-bytes to skip when reading or writing data_bus at start of BM transfer.
    end_strb      : std_logic_vector(AXI4_FDATA_BYTE     downto 0); -- Number of bytes to read from data_bus at the last BM transfer.
    bm_unsg_mask  : std_logic_vector(log_2(dbits/8)  - 1 downto 0); -- Unsigned number to generate the last BM transfer mask.
    bm_mask       : std_logic_vector(dbits/8         - 1 downto 0); -- What dbits byte lanes to use at BM transfer (all, except in the last transfer).
    bm_valid      : std_logic;                                      -- Valid read data to transfer into BM bus from data_bus(dbits-1 downto 0).
    bm_done       : std_logic;                                      -- Asserted at the last rd.bm_valid pulse of the whole transfer.

    axi_data_buffer   : std_logic_vector(AXI4_DATA_WIDTH - 1 downto 0); -- AXI data bus register used to separate AXI network from interface.
    axi_bmask_tmp     : unsigned(AXI4_DATA_WIDTH/8   - 1 downto 0); -- Base mask to be shifted and filter narrow transfers from AXI read data.
    axi_valid_buffer  : std_logic;                                  -- AXI subordinate r_valid buffer used as delayed signal.
    axi_ready_buffer  : std_logic;                                  -- Manager r_ready buffer used as delayed signal.
    axi_index_buffer  : std_logic_vector(log_2(rd_n_fifo_regs) - 1 downto 0); -- AXI fifo index buffer.
    fifo_full_buffer  : std_logic_vector(rd_n_fifo_regs - 1 downto 0);        -- fifo_full buffered register.
    bm_data           : std_logic_vector(dbits       - 1 downto 0); -- BM data register used to separate shifting from the bm_data_buffer.
    bm_data_buffer    : std_logic_vector(dbits       - 1 downto 0); -- BM data output buffer to maximize frequency of operation.
    bm_valid_buffer   : std_logic;                                  -- BM valid signal output delayed to be in sync with bm_data_buffer.
    bm_done_buffer    : std_logic;                                  -- BM done signal output delayed to be in sync with bm_data_buffer.
  end record;

  type transfer_wr_operation is record
    state         : transf_state; -- State of the read transaction.
    bm_grant      : std_logic;    -- Grant signals to BM component.
    bm_error      : std_logic;    -- Error flag register due to SLVERR or DECERR subordinate response.

    axi_mode      : std_logic_vector(1 downto 0); -- AXI output parameter: burst mode (FIXED, INC, WRAP).
    axi_size      : std_logic_vector(2 downto 0); -- AXI output parameter: size mode of each beat in the burst.
    axi_len       : std_logic_vector(7 downto 0); -- AXI output parameter: number of beats in the burst.
    axi_addr      : std_logic_vector(ADDR_WIDTH      - 1 downto 0); -- AXI output parameter: Starting pointer of the AXI burst.
    axi_valid     : std_logic;                    -- AXI output parameter: valid flag for output control signals (addr, len, size, mode).
    axi_valid_data: std_logic;                    -- AXI output parameter: valid write flag for output data.
    axi_last      : std_logic;                    -- AXI output parameter: last transaction flag of the burst.
    axi_strobe    : std_logic_vector(AXI4_DATA_WIDTH/8 - 1 downto 0); -- AXI output parameter: What AXI data lanes to read from during AXI transfer.
    axi_data_buffer   : std_logic_vector(AXI4_DATA_WIDTH - 1 downto 0); -- AXI data bus register used to separate AXI network from interface.

    two_subor     : std_logic; -- Flag asserted when two bursts are required to complete the transaction due to surpassing the 4KB boundary.

    bm_size       : std_logic_vector(sel(12, 11, Mult_bursts_subor) downto 0);-- Original size being requested by BM component, and then first burst size.
    rem_size      : std_logic_vector(11 downto 0);-- Remaining size for second burst (4KB outbounds access)
    bm_addr       : std_logic_vector(ADDR_WIDTH      - 1 downto 0); -- Starting pointer requested by BM component.

    fifo          : rd_data_buffer(wr_n_fifo_regs    - 1 downto 0); -- AXI data bus registers which filters narrow reads from data_tmp.
    fifo_full     : std_logic_vector(wr_n_fifo_regs  - 1 downto 0); -- When asserted, the fifo(axi_index) is full or at a BM transfer.
    axi_index     : std_logic_vector(log_2(wr_n_fifo_regs) - 1 downto 0); -- Unsigned index of fifo registers for the AXI side.
    bm_index      : std_logic_vector(log_2(wr_n_fifo_regs) - 1 downto 0); -- Unsigned index of fifo registers for the BM side.

    bm_ready      : std_logic;                                      -- Buffer flag to signal data read on the BM bus.
    bm_done       : std_logic;                                      -- Buffer flag to signal end of the requested transaction on the BM bus.
    bm_counter    : std_logic_vector(AXI4_FDATA_BYTE     downto 0); -- Used to count how many bytes have been filled on the actual fifo register.

    bm_data_buffer : std_logic_vector(dbits          - 1 downto 0); -- BM data bus buffer, used to separate it from the BM component.
    bm_ready_buffer: std_logic;                                     -- Buffer register of bm_ready. Used to exerce delayed logic on bm_data_buffer.
  end record;

  constant RST_TRANSF_RD_OP : transfer_rd_operation := (
    state             => idle,
    bm_grant          => '1',
    bm_error          => '0',
    axi_mode          => (others => '0'),
    axi_size          => (others => '0'),
    axi_len           => (others => '0'),
    axi_addr          => (others => '0'),
    axi_valid         => '0',
    axi_ready         => '0',
    axi_last          => '0',
    axi_counter       => (others => '0'),
    first_beat        => '1',
    two_subor         => '0',
    addr_end          => (others => '0'),
    bm_size           => (others => '0'),
    rem_size          => (others => '0'),
    bm_addr           => (others => '0'),
    fifo              => (others => (others => '0')),
    fifo_full         => (others => '0'),
    fifo_last         => (others => '0'),
    axi_index         => (others => '0'),
    bm_index          => (others => '0'),
    bm_first          => '1',
    bm_counter        => (others => '0'),
    start_strb        => (others => '0'),
    end_strb          => (others => '0'),
    bm_unsg_mask      => (others => '0'),
    bm_mask           => (others => '1'),
    bm_valid          => '0',
    bm_done           => '0',
    axi_data_buffer      => (others => '0'),
    axi_bmask_tmp     => (others => '0'),
    axi_valid_buffer  => '0',
    axi_ready_buffer  => '0',
    axi_index_buffer  => (others => '0'),
    fifo_full_buffer  => (others => '0'),
    bm_data           => (others => '0'),
    bm_data_buffer    => (others => '0'),
    bm_valid_buffer   => '0',
    bm_done_buffer    => '0'
  );

  constant RST_TRANSF_WR_OP : transfer_wr_operation := (
    state             => idle,
    bm_grant          => '1',
    bm_error          => '0',
    axi_mode          => (others => '0'),
    axi_size          => (others => '0'),
    axi_len           => (others => '0'),
    axi_addr          => (others => '0'),
    axi_valid         => '0',
    axi_valid_data    => '0',
    axi_last          => '0',
    axi_strobe        => (others => '0'),
    axi_data_buffer   => (others => '0'),
    two_subor         => '0',
    bm_size           => (others => '0'),
    rem_size          => (others => '0'),
    bm_addr           => (others => '0'),
    fifo              => (others => (others => '0')),
    fifo_full         => (others => '0'),
    axi_index         => (others => '0'),
    bm_index          => (others => '0'),
    bm_ready          => '0',
    bm_done           => '0',
    bm_counter        => (others => '0'),
    bm_data_buffer    => (others => '0'),
    bm_ready_buffer   => '0'
  );

  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------

  -- Registers for write/read control
  signal wr : transfer_wr_operation;
  signal rd : transfer_rd_operation;


  -----------------------------------------------------------------------------
  -- Function/procedure declaration
  -----------------------------------------------------------------------------

  -- To compute the number of beats required in the burst, the size requested is added to the LSB of the 
  -- starting address requested. This sum, contains the number of beats on the MSB.
  -- The line that determines LSB and MSB is defined by the size mode being set. However, since this one 
  -- is fixed (it's always AXI4_DATA_WIDTH), this logic is also fixed.

  -- In case of implementations where AXI4_DATA_WIDTH < 128 bits, BM transactions with a size request higher than 
  -- 256*AXI4_DATA_WIDTH/8 will require multiple bursts, since the maximum number of beats allowed is 256 (encoded as 0xFF).
  -- Thus, this function assigns the maximum number of beats if the bm_size is higher than what it can transfer in one burst 
  -- and the process will generate another burst to satisfy the BM request.

  procedure decide_len(
    bm_size   : in  std_logic_vector(11 downto 0);              -- Transfer size of the burst (actual num needs +1)
    bm_addr   : in  std_logic_vector(rd.bm_addr'high downto 0); -- LSB of the BM request address access
    burst_len : out std_logic_vector(7 downto 0)                -- AXI burst length (actual num of beats needs +1)
    ) is
      variable len_temp : std_logic_vector(12 downto 0);
      variable one      : std_logic;
  begin
    -- Number of beats required = MSB(LSB of the starting BM address + number of bytes to BM transfer)
    -- Since the size mode applied is always the same, this can be static (no need of a shifter).
    len_temp  := add_vector(bm_addr(AXI4_DATA_BYTE - 1 downto 0), bm_size, len_temp'length);

    one := '0';
    for k in 12 downto AXI4_DATA_BYTE+7 loop
      one     := len_temp(k) or one;
    end loop;
    
    burst_len := one & len_temp( sel(len_temp'high, AXI4_DATA_BYTE+6, len_temp'high < AXI4_DATA_BYTE+6) downto AXI4_DATA_BYTE );

  end procedure decide_len;

  
begin -- rtl

  -----------------
  -- Assignments --
  -----------------
  
  -- Advance eXtensible Interface (interconnect bus)
    -- Write address channel out
  axi4mo.aw_id        <= std_logic_vector(to_unsigned(axi_id, axi4mo.aw_id'length));
  axi4mo.aw_addr      <= wr.axi_addr(wr.axi_addr'high downto AXI4_DATA_BYTE) & (AXI4_DATA_BYTE-1 downto 0 => '0'); -- Address
  axi4mo.aw_region    <= (others => '0');
  axi4mo.aw_len       <= wr.axi_len;  -- Number of beats
  axi4mo.aw_size      <= wr.axi_size; -- Beat size
  axi4mo.aw_burst     <= wr.axi_mode; -- Burst mode
  axi4mo.aw_lock      <= '0';
  axi4mo.aw_cache     <= (others => '0');
  axi4mo.aw_prot      <= (others => '0');
  axi4mo.aw_qos       <= (others => '0');
  axi4mo.aw_valid     <= wr.axi_valid;
    -- Write data channel out
  axi4mo.w_data       <= wr.axi_data_buffer;
  axi4mo.w_strb       <= wr.axi_strobe;
  axi4mo.w_last       <= wr.axi_last;
  axi4mo.w_valid      <= wr.axi_valid_data;
    -- Write response channel out
  --axi4mo.b_ready      <= ;
    -- Read address channel out
  axi4mo.ar_id        <= std_logic_vector(to_unsigned(axi_id, axi4mo.aw_id'length));
  axi4mo.ar_addr      <= rd.axi_addr; -- Starting address
  axi4mo.ar_region    <= (others => '0');
  axi4mo.ar_len       <= rd.axi_len;  -- Number of beats
  axi4mo.ar_size      <= rd.axi_size; -- Beat size
  axi4mo.ar_burst     <= rd.axi_mode; -- Burst mode
  axi4mo.ar_lock      <= '0';
  axi4mo.ar_cache     <= (others => '0');
  axi4mo.ar_prot      <= (others => '0');
  axi4mo.ar_qos       <= (others => '0');
  axi4mo.ar_valid     <= rd.axi_valid;
    -- Read data channel out
  axi4mo.r_ready      <= rd.axi_ready;
    -- Write address channel in
  --axi4mi.aw_ready;-- used as input
  --  -- Write data channel in
  --axi4mi.w_ready; -- used as input
  --  -- Write response channel in
  --axi4mi.b_id;    --
  --axi4mi.b_resp;  --
  --axi4mi.b_valid; --
  --  -- Read address channel in
  --axi4mi.ar_ready;-- used as input
  --  -- Read data channel in
  --axi4mi.r_id;    -- used as input
  --axi4mi.r_data;  -- used as input
  --axi4mi.r_resp;  -- used as input
  --axi4mi.r_last;  -- used as input
  --axi4mi.r_valid; -- used as input

  -- Bus Master (injector)
  -- Write channel
  --bm_in.wr_addr;  -- used as input
  --bm_in.wr_size;  -- used as input
  --bm_in.wr_req;   -- used as input
  --bm_in.wr_data;
  bm_out.wr_req_grant <= wr.bm_grant;
  bm_out.wr_full      <= not(wr.bm_ready);
  bm_out.wr_done      <= wr.bm_done;
  bm_out.wr_err       <= wr.bm_error;

  -- Read channel
  --bm_in.rd_addr;  -- used as input
  --bm_in.rd_size;  -- used as input
  --bm_in.rd_req;   -- used as input
  bm_out.rd_req_grant <= rd.bm_grant;
  bm_out.rd_data      <= (rd.bm_data_buffer & (127-dbits downto 0 => '0'));
  bm_out.rd_valid     <= rd.bm_valid_buffer;
  bm_out.rd_done      <= rd.bm_done_buffer;
  bm_out.rd_err       <= rd.bm_error;

  
  -----------------------------------------------------------------------------
  -- Sequential process
  -----------------------------------------------------------------------------

  ------------------
  -- READ PROCESS --
  ------------------
  read_proc : process (clk, rstn) -- Variables used as connections between combinational logic, functions and registers.
    variable rd_addr_end  : std_logic_vector( 12                         downto 0 );-- max end address LSB at INC mode (4kB check)
    variable rd_axi_len   : std_logic_vector( rd.axi_len'range                    );-- AXI burst length
    variable rd_axi_next_index    : std_logic_vector(rd.axi_index'range           );-- Next buffer index

    variable rd_data_fwidth: std_logic_vector(AXI4_DATA_WIDTH + dbits - 1 downto 0);-- BM full shifted data bus
    variable rd_bm_counter        : std_logic_vector(rd.bm_counter'range          );-- Next BM counter
    variable rd_fifo_offset       : std_logic_vector(rd.bm_counter'range          );-- A fifo offset to simplify
    variable rd_bm_shift          : std_logic                                      ;-- BM shift flag
    variable rd_data_empty        : boolean                                        ;-- rd.fifo(bm_index) register depleted flag
    variable rd_bm_next_index     : std_logic_vector(rd.bm_index'range            );-- Next AXI counter
    variable rd_bm_done           : std_logic                                      ;-- Last BM transfer, generate BM mask.

    -- Variables that are only used on implementations where AXI4_DATA_WIDTH < 128 bits.
    variable rd_next_bm_size      : std_logic_vector(rd.bm_size'range             );-- Num of bytes left to transfer on the next clk cycle of this burst.
    
  begin
    -- Default variable values
    rd_addr_end         := (others => '0');
    rd_axi_len          := (others => '0');
    rd_axi_next_index   := (others => '0');
    rd_data_fwidth      := (others => '0');
    rd_bm_counter       := (others => '0');
    rd_fifo_offset      := (others => '0');
    rd_bm_shift         := '0';
    rd_data_empty       := FALSE;
    rd_bm_next_index    := (others => '0');
    rd_bm_done          := '0';
    if(Mult_bursts_subor) then
      rd_next_bm_size   := (others => '0');
    end if;

    if (rstn = '0' and ASYNC_RST) then
      rd                    <= RST_TRANSF_RD_OP;
    elsif rising_edge(clk) then
      if (rstn = '0') then
        rd                  <= RST_TRANSF_RD_OP;
      else
        ---------------------------
        -- AXI RD TRANSFER LOGIC --
        ---------------------------
        case rd.state is
          when idle => -- Worst delay path: BM component output
            -- Reset registers
            rd              <= RST_TRANSF_RD_OP;
          
            -- Load request information from BM
            if (bm_in.rd_req = '1' and rd.bm_grant = '1') then
              rd.bm_grant   <= '0';           -- Deassert granting requests for BM component
              rd.bm_addr    <= bm_in.rd_addr; -- Load starting address request
              if(Mult_bursts_subor) then -- Load BM size to transfer (-1 from real size)
                rd.bm_size    <= '0' & bm_in.rd_size; 
              else
                rd.bm_size    <= bm_in.rd_size;
              end if;
              -- Next, check 4kB out of bounds access
              rd.state      <= compute1;
            end if;
          
          
          when compute1 => -- Worst delay path: ADD 12+12, SUB 13-13
            -- Check if transaction will access two 4KB address regions
            rd_addr_end     := add_vector(rd.bm_size, rd.bm_addr(11 downto 0), rd_addr_end'length);
            rd.two_subor    <= rd_addr_end(12);
          
            -- If transaction must be split in two bursts, due to 4KB overflow boundary, calculate...
            if(rd_addr_end(12) = '1') then
              -- first burst size (-1 from real size)  MAX=4095, MIN=0
              rd.bm_size    <= sub_vector(x"FFF", rd.bm_addr(11 downto 0), rd.bm_size'length);
              -- second burst size (-1 from real size) MAX=4094, MIN=0
              rd.rem_size   <= rd_addr_end(rd.rem_size'range);
            end if;
          
            -- Number of bytes to transfer at the last BM transfer = LSB of BM size requested
            rd.end_strb     <= add_vector(rd_addr_end(AXI4_DATA_BYTE-1 downto 0), 1, rd.end_strb'length);

            -- Save the mask value to apply at the last BM transfer from the total requested size from the BM component
            rd.bm_unsg_mask <= rd.bm_size(rd.bm_unsg_mask'range);
          
            -- Next, compute size mode
            rd.state        <= compute2;
          
          
          when compute2 =>
            -- Set AXI size mode to AXI4_DATA_WIDTH and align the starting address with the AXI4_DATA_WIDTH slot.
            rd.axi_size     <= std_logic_vector(to_unsigned(AXI4_DATA_BYTE, rd.axi_size'length));
            rd.axi_addr     <= rd.bm_addr(rd.bm_addr'high downto AXI4_DATA_BYTE) & (AXI4_DATA_BYTE - 1 downto 0 => '0');

            -- Set the burst transfer mode.
            rd.axi_mode     <= INC;

            -- Compute how many beats will be necessary to transfer the requested data in this burst.
            decide_len(rd.bm_size(11 downto 0), rd.bm_addr, rd_axi_len);
            rd.axi_len      <= rd_axi_len;
          
            -- Save the number of bytes to discard unrequested data at the first BM transfer = LSB BM unaligned address + BM data bus width bytes
            if(rd.first_beat = '1') then
              rd.start_strb <= ((rd.start_strb'length - AXI4_DATA_BYTE - 1) downto 0 => '0') & rd.bm_addr(AXI4_DATA_BYTE - 1 downto 0);
            end if;
          
            -- Next, the handshake step.
            rd.axi_valid    <= '1'; -- Request AXI read burst.
            rd.state        <= handshake;
            
          
          when handshake =>
            -- Having all computation steps and proper registration separated allows maximum 
            -- frequency of operation when including this AXI manager interface at the interconnect
            -- bus. For a read burst, ar AXI control buses is used to send the control data.
            if (axi4mi.ar_ready = '1') then
              rd.axi_valid  <= '0'; -- At request being granted, deassert request
              rd.axi_ready  <= not( rd.fifo_full(to_integer(unsigned(rd.axi_index))) ); -- Read data from AXI bus flag
            
              -- Next, beat transfers
              rd.state      <= transfer;
            end if;
          
          
          when transfer => -- AXI transfer --
          -- The AXI transfer occurs by buffering the read data onto the rd.axi_data_buffer register when the subordinate flags valid data.
          -- Then, the rd.axi_data_buffer is filtered onto an fifo register, which will be used at the BM transfer. However, due to this single clock 
          -- cycle delay between AXI read and data filtering, both valid and the mask signals are also buffered on the "buffer" registers (these
          -- are outside of the state machine). 

              -- Check if subordinate is delivering valid data with the same AXI ID as requested and if manager is listening.
            if (axi4mi.r_valid = '1' and axi4mi.r_id = std_logic_vector(to_unsigned(axi_id, axi4mi.r_id'length)) and rd.axi_ready = '1') then

              -- Register inputs to separate AXI network from further computation.
              rd.axi_data_buffer  <= axi4mi.r_data;
              rd.axi_last         <= axi4mi.r_last;
              rd.bm_error         <= axi4mi.r_resp(1);

              -- Compute next FIFO index.
              rd_axi_next_index := add_vector(rd.axi_index, 1, rd.axi_index'length);
              rd.axi_index      <= rd_axi_next_index;

              -- Deassert the ready flag to finish AXI burst if it is the last beat or if there is not enough space in the FIFO at the moment.
              rd.axi_ready      <= not( axi4mi.r_last or rd.fifo_full(to_integer(unsigned(rd_axi_next_index))) );
              
              -- Signal the present rd.fifo register as prepared for BM transfer, for whenever it is due to buffers.
              rd.fifo_full(to_integer(unsigned(rd.axi_index))) <= '1';

              -- The last AXI transfer of the whole transaction will set the proper rd.fifo_last bit, so the BM transfer logic knows when to end.
              -- However, there's a distinction between AXI4_DATA_WIDTH <= 64 bits and higher data widths. If there's still data to transfer 
              -- because this burst has not been enough, do not assert the last BM transfer on the FIFO register and update rd.bm_size.
              if(Mult_bursts_subor) then
                rd_next_bm_size := sub_vector(rd.bm_size, AXI4_DATA_WIDTH/8, rd_next_bm_size'length);
                rd.bm_size      <= rd_next_bm_size;
                rd.fifo_last(to_integer(unsigned(rd.axi_index))) <= not(rd.two_subor) and axi4mi.r_last
                                                                    and rd_next_bm_size(12);
              else
                rd.fifo_last(to_integer(unsigned(rd.axi_index))) <= not(rd.two_subor) and axi4mi.r_last;
              end if;


            else
            -- When the manager is not listening to new data, be it the AXI burst has finished, there is not enough 
            -- space in FIFO or subordinate is not sending valid data yet.

              -- If the AXI burst has finished (rd.axi_ready must be deasserted by now), set the control data for a second
              -- burst if it is necessary (rd.two_subor = 1) or wait until the last BM transfer resets the whole read block.
              if(rd.axi_last = '1') then
                rd.first_beat <= '0';

                -- However, there's a distinction between implementations with an AXI4_DATA_WIDTH <= 64 bits and higher data widths. 
                -- If there's still data to transfer because this burst has not been enough, generate another burst to the same subordinate.
                if(Mult_bursts_subor) then
                  if(rd.bm_size(rd.bm_size'high) = '0') then
                    -- The address bit range comes from that in AXI4 the max length is 256 (255 encoded), thus, +8 bit positions.
                    rd.bm_addr    <= add_vector(rd.bm_addr(rd.bm_addr'high downto 8 + AXI4_DATA_BYTE), 1, rd.bm_addr'high - 7 - AXI4_DATA_BYTE) 
                                      & (7 + AXI4_DATA_BYTE downto 0 => '0');
                    rd.state      <= compute2;

                  else
                    if(rd.two_subor = '1') then
                      rd.two_subor  <= '0';
                      rd.axi_last   <= '0';
                      rd.bm_size    <= '0' & rd.rem_size;
                      rd.bm_addr    <= add_vector(rd.bm_addr(rd.bm_addr'high downto 12), 1, 20) & (11 downto 0 => '0');
                      rd.state      <= compute2;
                    end if;
                  end if;

                -- If AXI4_DATA_WIDTH > 64 bits, forget about the rd.bm_size check, since these bus widths can achieve 4kB access in one burst.
                else
                  if(rd.two_subor = '1') then
                    rd.two_subor  <= '0';
                    rd.axi_last   <= '0';
                    rd.bm_size    <= rd.rem_size;
                    rd.bm_addr    <= add_vector(rd.bm_addr(rd.bm_addr'high downto 12), 1, 20) & (11 downto 0 => '0');
                    rd.state      <= compute2;
                  end if;
                end if;

              -- In case it is not the last AXI beat, check if the present FIFO register can be used for another read beat.
              else
                if( rd.fifo_full(to_integer(unsigned(rd.axi_index))) = '0' ) then
                  rd.axi_ready    <= '1'; -- If there's empty registers in the buffer, use them to read another beat.
                  rd.axi_counter  <= (others => '0');
                end if;
              end if;

            end if;


          when others =>

        end case;


      -------------
      -- RD FIFO --
      -------------
        -- After each AXI data read, put the data to the next available fifo register.
        if(rd.axi_valid_buffer = '1' and rd.axi_ready_buffer = '1') then
            rd.fifo(to_integer(unsigned(rd.axi_index_buffer))) <= rd.axi_data_buffer;
        end if;


      -----------------------
      -- BM TRANSFER LOGIC --
      -----------------------
      -- The BM transfer logic transfers from the RD FIFO to the BM component at dbits bits per clock cycle throughput without pause, unless  
      -- there's not enough data in the FIFO to BM transfer. This is achieved by shifting rd.fifo(bm_index) onto itself and the rd.bm_data 
      -- used as output (after masking for last BM transfer), while there's enough data in the present FIFO register. 
      -- Then, when the present FIFO register does not have enough data to fill rd.bm_data for a BM transfer, the next FIFO register is used 
      -- with rd.fifo(bm_next_index) with also the remaining data on present FIFO register, if there is. 
      -- After fully reading a FIFO register, this one is set free to be used at new AXI beat readings.

        -- Set combinational variables --
        -- Check if this is the last BM transfer before rd.fifo(bm_index) is depleted after a transfer of dbits bits.
        rd_data_empty       := rd.bm_counter <= std_logic_vector(to_unsigned(dbits/8, rd.bm_counter'length));
        rd_bm_next_index    := add_vector(rd.bm_index, 1, rd.bm_index'length);

        -- Check if there's data to BM transfer. Delayed with a buffer register so the last AXI beat transfer is finished on this FIFO register.
        if(rd.fifo_full_buffer(to_integer(unsigned(rd.bm_index))) = '1') then

        -- Before any BM transfer, shift the FIFO register, not to read from, but to put the requested data on dbits to higher, flushing unrequested 
        -- data read by the AXI side. Then, initilize the rd.bm_counter with [the whole AXI data bus width bytes value or with the number of bytes
        -- to read from the last FIFO register to BM transfer] minus the shift performed on this rd.bm_first cycle.
          if(rd.bm_first = '1') then

            -- The initial counter is set by rd.end_strb if there's only one fifo register to transfer. 
            if(rd.fifo_last(to_integer(unsigned(rd.bm_index))) = '1') then
              rd_bm_counter := rd.end_strb;
            else -- Otherwise, the initial counter is the AXI data bus bytes.
              rd_bm_counter := std_logic_vector(to_unsigned(AXI4_DATA_WIDTH/8, rd.bm_counter'length));
            end if;

            -- Decrement the shift to the initial counter and use it to initilize rd.bm_counter.
            rd.bm_counter   <= sub_vector(rd_bm_counter, rd.start_strb, rd.bm_counter'length);
            -- Shift the content of the FIFO register so the requested data starts on dbits position of rd_data_fwidth.
            rd_data_fwidth  := (8*to_integer(unsigned(rd.start_strb)) - 1 downto 0 => '0')
                              & rd.fifo(to_integer(unsigned(rd.bm_index)))(AXI4_DATA_WIDTH - 1 downto 8*to_integer(unsigned(rd.start_strb)))
                              & (dbits - 1 downto 0 => '0');
            rd_bm_shift     := '1';

            -- Deassert the first BM transfer flag, since it's only required to shift rd.start_strb before than the first BM transfer.
            rd.bm_first     <= '0';

          else -- rd.bm_first = '0' check

            -- START BM TRANSFER --
            if(rd_data_empty) then
              if(rd.fifo_full_buffer(to_integer(unsigned(rd_bm_next_index))) = '1') then

              -- In case this cycle will deplete the present fifo register (rd_data_empty = 1) and the next fifo register is prepared 
              -- to be transfered, use the remaining data (rd.bm_counter lower or equal to AXI data bus bytes) as LSB and take the 
              -- lacking bytes to fill dbits from the next fifo register, while also shifting its content for posterior transfers.
              -- (since the shifting index k surpasses the bytes available in the next fifo register on unaligned starting transfers, 
              -- fill the remaining byte positions with zeros, that, in number of bytes, will be always lower than the bytes in dbits)
                for k in AXI4_DATA_WIDTH/8 + dbits/8 - 1 downto 0 loop
                  if( rd.bm_counter > std_logic_vector(to_unsigned(k, rd.bm_counter'length)) ) then
                    rd_data_fwidth(8*k+7 downto 8*k) := rd.fifo(to_integer(unsigned(rd.bm_index)))
                                                      (8*k+7 downto 8*k);
                  else
                    rd_fifo_offset := sub_vector(k, rd.bm_counter, rd.bm_counter'length);
                    if( to_integer(unsigned(rd_fifo_offset)) < AXI4_DATA_WIDTH/8) then
                      rd_data_fwidth(8*k+7 downto 8*k) := rd.fifo(to_integer(unsigned(rd_bm_next_index)))
                                                        (       8*to_integer(unsigned(rd_fifo_offset))+7 
                                                        downto  8*to_integer(unsigned(rd_fifo_offset)) );
                    else
                      rd_data_fwidth(8*k+7 downto 8*k) := x"00";
                    end if;
                  end if;
                end loop;

                -- Set the present fifo register to be prepared for new AXI transfer and change the fifo register rd.bm_index.
                rd.fifo_full(to_integer(unsigned(rd.bm_index))) <= '0';
                rd.bm_index       <= rd_bm_next_index;
                rd_bm_shift       := '1';

                -- Compute the number of bytes that'll be left to read on the next fifo register.
                -- In case where the next fifo register is the last, counter = end_strb - ( dbits/8 - bm_counter ) = ( end_strb + bm_counter ) - dbits/8
                -- Otherwise, it must be taken the whole AXI data bus, counter = AXI_DATA/8 - ( dbits/8 - bm_counter ) = bm_counter + AXI_DATA/8 - dbits/8
                if(rd.fifo_last(to_integer(unsigned(rd_bm_next_index))) = '1') then
                  rd.bm_counter   <= sub_vector(add_vector(rd.end_strb, rd.bm_counter, rd.bm_counter'length), dbits/8, rd.bm_counter'length);

                  -- In addition, if the number of bytes read on this clock cycle from rd.fifo(rd_bm_next_index) include all the data required 
                  -- by rd.end_strb, end the transfer on the next clock cycle. (done, mask and return to idle)
                  if(sub_vector(dbits/8, rd.bm_counter, rd.bm_counter'length) >= rd.end_strb) then
                    rd_bm_done    := '1';
                    rd.bm_done    <= '1';
                    rd_bm_shift   := '1';
                    rd.state      <= idle;
                  end if;

                else
                  rd.bm_counter   <= add_vector(rd.bm_counter, AXI4_DATA_WIDTH/8 - dbits/8, rd.bm_counter'length);
                end if;              

              else   -- rd.fifo_full(rd_bm_next_index) = 0 check

              -- In case there's not enough data to read from the fifo, check if this is the last fifo register. If it is, check how 
              -- many bytes of remaining data are left. If it's lower than dbits/8, this is the last BM transfer. However, if it's 
              -- exactly dbits/8 and it's the last fifo register, delay the end of the BM transfer for another clock cycle (do not 
              -- increase the rd.bm_counter, let the execution return to this step to execute the end of the BM transfer).
                if(rd.fifo_last(to_integer(unsigned(rd.bm_index))) = '1') then 
                  rd.bm_counter   <= sub_vector(rd.bm_counter, dbits/8, rd.bm_counter'length);
                  rd_data_fwidth  := (dbits - 1 downto 0 => '0') & rd.fifo(to_integer(unsigned(rd.bm_index)));
                  rd_bm_shift     := '1';
                  rd_bm_done      := '1';
                  rd.bm_done      <= '1';
                  rd.state        <= idle;
                else
              -- Otherwise, if there's not enough data and this is not the last fifo register, wait for the next AXI transfer.
                  rd_bm_shift     := '0';
                end if;

              end if; -- rd.fifo_full(rd_bm_next_index) end of check
            else  -- rd_empty_data = 0 check

            -- In case there's still data on the present fifo register for a dbits transfer (more than dbits), shift dbits.
              rd_data_fwidth      := (dbits - 1 downto 0 => '0') & rd.fifo(to_integer(unsigned(rd.bm_index)));
              rd.bm_counter       <= sub_vector(rd.bm_counter, dbits/8, rd.bm_counter'length);
              rd_bm_shift         := '1';

            end if; -- rd_empty_data end of check
          end if;   -- bm_first end of check

          rd.bm_data        <= rd_data_fwidth(dbits - 1 downto 0);
          rd.bm_valid       <= rd_bm_shift and not(rd.bm_first); -- BM transfer every cycle there's a shift but the first.

          if(rd_bm_shift = '1') then
            if(rd.bm_first = '1') then
              rd.fifo(to_integer(unsigned(rd.bm_index)))        <= rd_data_fwidth(rd_data_fwidth'high downto dbits);
            else
              if(rd_data_empty) then
                rd.fifo(to_integer(unsigned(rd_bm_next_index))) <= rd_data_fwidth(rd_data_fwidth'high downto dbits);
              else
                rd.fifo(to_integer(unsigned(rd.bm_index)))      <= rd_data_fwidth(rd_data_fwidth'high downto dbits);
              end if;
            end if;
          end if;

        end if;

        -- Generate the BM byte mask to only deliver the bytes requested.
        if(rd_bm_done = '1') then
          for k in 0 to rd.bm_mask'length - 1 loop
            if(rd.bm_unsg_mask >= std_logic_vector(to_unsigned(k, log_2(dbits/8)))) then
              rd.bm_mask(k) <= '1';
            else 
              rd.bm_mask(k) <= '0';
            end if;
          end loop;
        end if;
        
        -- Apply the BM byte mask to the read data output of the BM component.
        for k in dbits/8 - 1 downto 0 loop
          rd.bm_data_buffer(8*k+7 downto 8*k) <= rd.bm_data(8*k+7 downto 8*k) and (7 downto 0 => rd.bm_mask(k));
        end loop;


      ----------------------
      -- BUFFER REGISTERS --
      ----------------------

        -- Reset of buffer signals and state at last BM transfer.
        if(rd.bm_done_buffer = '1') then
          rd.axi_valid_buffer <= '0';
          rd.axi_ready_buffer <= '0';
          rd.axi_index_buffer <= (others => '0');
          rd.fifo_full_buffer <= (others => '0');
          rd.bm_valid_buffer  <= '0';
          rd.bm_done_buffer   <= '0';
        else
        -- Registration of buffer signals for read transactions.
          rd.axi_valid_buffer <= axi4mi.r_valid; -- Delayed to mask the last AXI beat transfer from the previous axi_index.
          rd.axi_ready_buffer <= rd.axi_ready;   -- Delayed to mask the last AXI beat transfer from the previous axi_index.
          rd.axi_index_buffer <= rd.axi_index;   -- Delayed to mask the last AXI beat transfer from the previous axi_index.
          rd.fifo_full_buffer <= rd.fifo_full;   -- Delayed full vector so BM transfers are executed a cycle after the AXI transfer.
          rd.bm_valid_buffer  <= rd.bm_valid;    -- Delayed BM valid data flag so the mask and output is separated from BM transfer logic.
          rd.bm_done_buffer   <= rd.bm_done;
        end if;


      end if;
    end if;
  end process read_proc;



  -------------------
  -- WRITE PROCESS --
  -------------------
  write_proc : process (clk, rstn) -- Variables used as connections between combinational logic, functions and registers.
    variable wr_addr_end  : std_logic_vector( 12                         downto 0 );-- max end address LSB at INC mode (4kB check)
    variable wr_axi_len   : std_logic_vector( wr.axi_len'range                    );-- AXI burst length
    variable wr_axi_last  : std_logic                                              ;-- AXI last beat signal previous to buffering
    variable wr_axi_next_index    : std_logic_vector(wr.axi_index'range           );-- Next FIFO index on AXI side
    variable wr_axi_read  : boolean                                                ;-- The data buffer has been read flag

    variable wr_bm_next_counter   : std_logic_vector(wr.bm_counter'range          );-- Next BM counter
    variable wr_fifo_offset       : std_logic_vector(wr.bm_counter'range          );-- A fifo offset to simplify
    variable wr_bm_next_index     : std_logic_vector(wr.bm_index'range            );-- Next FIFO index on BM side
    
  begin
    -- Default variable values
    wr_addr_end         := (others => '0');
    wr_axi_len          := (others => '0');
    wr_axi_last         := '0';
    wr_axi_next_index   := (others => '0');
    wr_axi_read         := FALSE;
    wr_bm_next_counter  := (others => '0');
    wr_fifo_offset      := (others => '0');
    wr_bm_next_index    := (others => '0');

    if (rstn = '0' and ASYNC_RST) then
      wr                    <= RST_TRANSF_WR_OP;
    elsif rising_edge(clk) then
      if (rstn = '0') then
        wr                  <= RST_TRANSF_WR_OP;
      else
        ---------------------------
        -- AXI WR TRANSFER LOGIC --
        ---------------------------
        case wr.state is
          when idle => -- Worst delay path: BM component output
            -- Reset registers
            wr              <= RST_TRANSF_WR_OP;
          
            -- Load request information from BM
            if (bm_in.wr_req = '1' and wr.bm_grant = '1') then
              wr.bm_grant   <= '0';           -- Deassert granting requests for BM component
              wr.bm_addr    <= bm_in.wr_addr; -- Load starting address request
              if(Mult_bursts_subor) then -- Load BM size to transfer (-1 from real size)
                wr.bm_size  <= '0' & bm_in.wr_size;
              else
                wr.bm_size  <= bm_in.wr_size;
              end if;
              -- Next, check 4kB out of bounds access
              wr.state      <= compute1;
            end if;
          
          
          when compute1 => -- Worst delay path: ADD 12+12, SUB 13-13
            -- Check if transaction will access two 4KB address regions
            wr_addr_end     := add_vector(wr.bm_size, wr.bm_addr(11 downto 0), wr_addr_end'length);
            wr.two_subor    <= wr_addr_end(12);
          
            -- If transaction must be split in two bursts, due to 4KB overflow boundary, calculate...
            if(wr_addr_end(12) = '1') then
              -- first burst size (-1 from real size)  MAX=4095, MIN=0
              wr.bm_size    <= sub_vector(x"FFF", wr.bm_addr(11 downto 0), wr.bm_size'length);
              -- second burst size (-1 from real size) MAX=4094, MIN=0
              wr.rem_size   <= wr_addr_end(wr.rem_size'range);
            end if;

            -- Next, compute size mode
            wr.state        <= compute2;
          
          
          when compute2 =>
            -- Set AXI size mode to AXI4_DATA_WIDTH and align the starting address with the AXI4_DATA_WIDTH slot.
            wr.axi_size     <= std_logic_vector(to_unsigned(AXI4_DATA_BYTE, wr.axi_size'length));
            wr.axi_addr     <= wr.bm_addr(wr.bm_addr'high downto AXI4_DATA_BYTE) & (AXI4_DATA_BYTE - 1 downto 0 => '0');
            
            -- Set the burst transfer mode.
            wr.axi_mode     <= INC;

            -- Compute how many beats will be necessary to transfer the requested data in this burst.
            decide_len(wr.bm_size(11 downto 0), wr.bm_addr, wr_axi_len);
            wr.axi_len      <= wr_axi_len;

            -- Add to the BM size transfer the starting position of the AXI4_DATA_WIDTH slot, so it can be used for high cap strobe.
            wr.bm_size      <= add_vector(wr.bm_size, wr.bm_addr(AXI4_DATA_BYTE - 1 downto 0), wr.bm_size'length);

            -- Next, the handshake step.
            wr.axi_valid    <= '1'; -- Request AXI write burst.
            wr.state        <= handshake;
          
          
          when handshake =>
            -- Having all computation steps and proper registration separated allows maximum 
            -- frequency of operation when including this AXI manager interface at the interconnect
            -- bus. For a write burst, AW AXI control buses are used to send the burst control data.
            if (axi4mi.aw_ready = '1') then
              wr.axi_valid  <= '0'; -- At request being granted, deassert AXI handshake request
            
              -- Next, beat transfers
              wr.state      <= transfer;
            end if;
          
          
          when transfer => -- AXI transfer --
          -- The AXI transfer sets the AXI valid write flag anytime there's data available in a buffer register that reads from the FIFO, 
          -- freeing the FIFO register for BM fransfers. In addition, the size left to transfer wr.bm_size is decreased with the number of 
          -- bytes transfered.

            -- Load the variables for last AXI beat in the burst, next FIFO index at the AXI side and the read flag (the AXI network has read the data buffer).
            wr_axi_last         := to_std_logic(wr.axi_len = (wr.axi_len'range => '0'));
            wr_axi_next_index   := add_vector(wr.axi_index, 1, wr.axi_index'length);
            wr_axi_read         := wr.axi_valid_data = '1' and axi4mi.w_ready = '1';

            -- Set the AXI strobe for the next AXI write beat. The wr.bm_addr LSB are used to write zeros on the LSB of the strobe for the 
            -- first beat. After that, wr.bm_addr is zeroed so this takes effect only on the first beat (the second burst already has zeroed 
            -- the LSB from wr.bm_addr). 
            for k in wr.axi_strobe'range loop
              if( k < to_integer(unsigned(wr.bm_addr(AXI4_DATA_BYTE-1 downto 0))) or k > to_integer(unsigned(wr.bm_size)) ) then
                wr.axi_strobe(k)    <= '0';
              else
                wr.axi_strobe(k)    <= '1';
              end if;
            end loop;

            -- On the first beat read, zero the wr.bm_addr LSB used to set up the lower bound of the strobe for the first beat.
            if(wr_axi_read) then
              wr.bm_addr(AXI4_DATA_BYTE-1 downto 0) <= (AXI4_DATA_BYTE-1 downto 0 => '0');
            end if;

            -- When the interface is not sending valid data on the buffer to read from or it is and the AXI interconnect has read it, read from the next available register in the
            -- FIFO onto the data buffer, setting also the related signals and update both beats left (length), the FIFO index and the number of bytes left on to transfer on this 
            -- subordinate. However, if the data buffer is the last beat, do not continue with another AXI beat. 
            if( (wr_axi_read or wr.axi_valid_data = '0') and wr.axi_last = '0' ) then
              if(wr.fifo_full(to_integer(unsigned(wr.axi_index))) = '1') then
                wr.axi_valid_data   <= '1';
                wr.axi_data_buffer  <= wr.fifo(to_integer(unsigned(wr.axi_index)));
                wr.axi_last         <= wr_axi_last;
                wr.fifo_full(to_integer(unsigned(wr.axi_index)))  <= '0';
                wr.axi_len          <= sub_vector(wr.axi_len, 1, wr.axi_len'length);
                wr.bm_size          <= sub_vector(wr.bm_size, AXI4_DATA_WIDTH/8, wr.bm_size'length);
                wr.axi_index        <= wr_axi_next_index;
              else
                wr.axi_valid_data   <= '0';
              end if;
            end if;

            -- At burst end, deassert the AXI write valid and last beat flags when subordinate is listening to the last beat.
            if(wr_axi_read and wr.axi_last = '1') then
              wr.axi_valid_data <= '0';
              wr.axi_last       <= '0';

              -- Then, if the implementation requires to check if multiple bursts to the same subordinate are required to allow 
              -- 4kB access, check if all data of this burst has been sent. If all data for this subordinate has been sent, check 
              -- if access to another subordinate is required (wr.two_subor). If not, return to idle.
              if(Mult_bursts_subor) then

                if(wr.bm_size(wr.bm_size'high) = '0') then
                  wr.bm_addr      <= add_vector(wr.bm_addr(wr.bm_addr'high downto 8 + AXI4_DATA_BYTE), 1, wr.bm_addr'high - 7 - AXI4_DATA_BYTE) 
                                        & (7 + AXI4_DATA_BYTE downto 0 => '0');
                  wr.state        <= compute2;
                else

                  if(wr.two_subor = '1') then
                    wr.two_subor  <= '0';
                    wr.bm_size    <= '0' & wr.rem_size;
                    wr.bm_addr    <= add_vector(wr.bm_addr(wr.bm_addr'high downto 12), 1, 20) & (11 downto 0 => '0');
                    wr.state      <= compute2;
                  else
                    wr.bm_done    <= '1';
                    wr.state      <= idle;
                  end if;

                end if;

              else
              -- Otherwise, the implementation has an AXI4_DATA_WIDTH > 64 bits allowing 4kB access in a single burst, so disregard 
              -- the multiple burst to the same subordinate check and proceed with the two subordinate acces check (wr.two_subor).
                if(wr.two_subor = '1') then
                  wr.two_subor    <= '0';
                  wr.bm_size      <= wr.rem_size;
                  wr.bm_addr      <= add_vector(wr.bm_addr(wr.bm_addr'high downto 12), 1, 20) & (11 downto 0 => '0');
                  wr.state        <= compute2;
                else
                  wr.bm_done      <= '1';
                  wr.state        <= idle;
                end if;
              end if;

            end if;


          when others =>

        end case;


      ---------------------------------
      -- WR FIFO + BM TRANSFER LOGIC --
      ---------------------------------

      -- The BM transfer logic starts by initilizating the wr.bm_counter with the number of LSB bytes as 
      -- buffers the data read from the BM component onto wr.bm_ready. Then, checks
      case wr.state is
        when compute1 =>
          wr.bm_counter <= ((wr.bm_counter'length - AXI4_DATA_BYTE - 1) downto 0 => '0') & wr.bm_addr(AXI4_DATA_BYTE - 1 downto 0);
          wr.bm_ready   <= '1';
      
        when compute2 | handshake | transfer =>
          wr_bm_next_index    := add_vector(wr.bm_index, 1, wr.bm_index'length);
          wr_bm_next_counter  := add_vector(wr.bm_counter, dbits/8, wr.bm_counter'length);

          -- Stop reading data from BM if the next FIFO register is full.
          wr.bm_ready <= not(wr.fifo_full(to_integer(unsigned(wr_bm_next_index))));

          -- Read BM data if the wr.bm_ready flag register is asserted.
          if(wr.bm_ready = '1') then
            wr.bm_data_buffer   <= bm_in.wr_data(bm_in.wr_data'high downto bm_in.wr_data'high - dbits + 1);
          end if;

          -- If data has been read on the previous clock cycle, write it into the FIFO and update wr.bm_counter.
          if(wr.bm_ready_buffer = '1') then

            -- Write every byte from the wr.bm_data_buffer into the present FIFO register. If the present 
            -- register is full, then write the bytes left on the next FIFO register.
            for k in dbits/8 - 1 downto 0 loop
              wr_fifo_offset := add_vector(wr.bm_counter, k, wr.bm_counter'length);
              
              if(wr_fifo_offset < std_logic_vector(to_unsigned(AXI4_DATA_WIDTH/8, wr_fifo_offset'length))) then
                wr.fifo(to_integer(unsigned(wr.bm_index)))( 8*to_integer(unsigned(wr_fifo_offset)) + 7 
                                                            downto 8*to_integer(unsigned(wr_fifo_offset)) ) 
                  <= wr.bm_data_buffer(8*k+7 downto 8*k);
              else
                wr.fifo(to_integer(unsigned(wr_bm_next_index)))( 8*to_integer(unsigned(wr_fifo_offset(log_2(dbits/8)-1 downto 0))) + 7 
								downto 8*to_integer(unsigned(wr_fifo_offset(log_2(dbits/8)-1 downto 0))) )
                  <= wr.bm_data_buffer(8*k+7 downto 8*k);
              end if;

            end loop;
            
            -- If the counter surpasess the number of bytes in the AXI data bus width, that means the present FIFO register
            -- has been filled, so change to the next FIFO register and mark the present as full. Also, maintain the number
            -- of bytes that have been written on the next FIFO register on wr.bm_counter.
            if(wr_bm_next_counter >= std_logic_vector(to_unsigned(AXI4_DATA_WIDTH/8, wr.bm_counter'length))) then
              wr.bm_counter <= (wr.bm_counter'length - log_2(dbits/8) - 1 downto 0 => '0') 
                                & wr_bm_next_counter(log_2(dbits/8) - 1 downto 0);
              wr.bm_index   <= wr_bm_next_index;
              wr.fifo_full(to_integer(unsigned(wr.bm_index))) <= '1';
            else
              wr.bm_counter <= wr_bm_next_counter;
            end if;

          end if;

        when others =>

      end case;

      ----------------------
      -- BUFFER REGISTERS --
      ----------------------
      wr.bm_ready_buffer  <= wr.bm_ready;
      
      
      end if;
    end if;
  end process write_proc;

  -----------------------------------------------------------------------------
  -- Combinational process
  -----------------------------------------------------------------------------


end architecture rtl;
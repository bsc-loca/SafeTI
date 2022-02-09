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
-- Manager specifications:
--
-- - MAX_SIZE_BURST <= 4096 (4kB, limited by AXI4 addressing rule)
-- - AXI4_DATA_WIDTH >= dbits.
-- - dbits <= 128.
-- - BM size requests <= MAX_SIZE_BURST.
-- - Little endian data structure
-- - Only INC burst mode is implemented.
-- - Unaligned read access is supported, but doesn't execute two narrow transfers on the same AXI4_DATA_WIDTH slot.
-- - Unaligned write access is supported, accessing AXI4_DATA_WIDTH/8 aligned address and using the AXI write strobe.
--
-- This AXI4 Manager interface translates and manage the requests made by the BM component,
-- using the BM input and output buses, to the AXI4 network, using the AXI4 input and output buses.
--
-- The BM component has an opaque vision of what the interface requests to the AXI network, since 
-- it only receives the data that it has been requested. Be it requests with unaligned or even multiple 
-- subordinates access (only 2 as maximum), the interface filters it to deliver only the requested data.
-- 
-- All I/O ports are buffered on registers to promote higher maximum frequency of operation at 
-- implementation, with minimum logic between register and BM or AXI bus. (The biggest expected delay
-- from I/O ports is the BM read data port, since a byte mask is being applied at the output.)
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
    skip_BM_transf  : in  std_logic           -- Skip BM transfers, discard all read data and write full ones. WIP
  );
end entity axi4_manager;

architecture rtl of axi4_manager is
  -----------------------------------------------------------------------------
  -- Constant declaration
  -----------------------------------------------------------------------------

  -- Constants that should be equal to what is written at injector_pkg.vhd
  constant INT_BURST_WIDTH  : integer := log_2(MAX_SIZE_BURST);     -- Width to hold maximum beat size number
  constant AXI4_DATA_WIDTH  : integer := axi4mo.w_data'length;      -- AXI data bus width
  constant AXI4_DATA_BYTE   : integer := log_2(AXI4_DATA_WIDTH/8);  -- AXI data bytes addressing bus width
  constant AXI4_FDATA_BYTE  : integer := max((AXI4_DATA_BYTE, log_2(dbits/8)))+1; -- To acommodate dbits larger than AXI data bus widths.

  -- AXI constants
  constant FIXED            : std_logic_vector(1 downto 0) := "00"; -- AXI burst modes: FIXED
  constant INC              : std_logic_vector(1 downto 0) := "01"; --                  INCREMENTAL
  constant WRAP             : std_logic_vector(1 downto 0) := "10"; --                  WRAP
  constant size_array       : array_integer(0 to 7) := (1, 2, 4, 8, 16, 32, 64, 128); -- AXI size transfer modes
  constant size_mode_appl   : std_logic_vector(0 to 6) := (
    -- Generate an array indicating what size modes are applicable due to the constraints of
    -- both the AXI data bus width and the maximum burst size of the implementation.
    0 => to_std_logic( AXI4_DATA_WIDTH/8 >= 128 and MAX_SIZE_BURST >= 128 ),
    1 => to_std_logic( AXI4_DATA_WIDTH/8 >=  64 and MAX_SIZE_BURST >=  64 ),
    2 => to_std_logic( AXI4_DATA_WIDTH/8 >=  32 and MAX_SIZE_BURST >=  32 ),
    3 => to_std_logic( AXI4_DATA_WIDTH/8 >=  16 and MAX_SIZE_BURST >=  16 ),
    4 => to_std_logic( AXI4_DATA_WIDTH/8 >=   8 and MAX_SIZE_BURST >=   8 ),
    5 => to_std_logic( AXI4_DATA_WIDTH/8 >=   4 and MAX_SIZE_BURST >=   4 ), -- These should be unnecessary
    6 => to_std_logic( AXI4_DATA_WIDTH/8 >=   2 and MAX_SIZE_BURST >=   2 )  -- These should be unnecessary
    );
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

  -----------------------------------------------------------------------------
  -- Records and types
  -----------------------------------------------------------------------------
  --
  -- This interface, when idle, accepts read and write transaction requests from the BM bus, listening the starting address 
  -- and the total transfer size of the operatio. This last must be lower or equal to the set by MAX_SIZE_BURST-1 and it 
  -- is encoded as an unsigned integer decremented by one respect the real size, so 0 means 1 byte transfer.
  --
  -- Once the request has been granted (deasserting new requests until finished), it processes the information to check if 
  -- the total transfer surpasses the 4kB address boundary, splitting the transaction in two bursts in such situations.
  -- The interface sets up the correspondent AXI control data for the first or only trasaction burst, including burst mode,
  -- size mode, burst length and the aligned starting address respect to the size mode being set. Then, proceeds to execute
  -- the handshake with the AXI interconnect network, which continues with the transfer of data in beats of AXI4_DATA_WIDTH bits 
  -- on the AXI side and dbits bits on the BM side, maintaining the data streams whenever it is possible.
  -- 
  -- The transfers characteristics are set by the size and address of the BM request, in such a way that the interface will 
  -- configure the burst to have the minimum AXI length (number of beats) possible, setting the AXI size mode to be equal or 
  -- the next power of 2 of the size requested. This last, however, is limited by the width of the AXI data bus AXI4_DATA_WIDTH.
  --
  -- Unaligned address access is managed by calculating an aligned address access with the size mode being set on the burst.
  -- Then, the interface treats the input data to be transfered so the access is done as expected on write transactions, by 
  -- using the AXI data strobe and an aligned address access, and on read transactions, by discarding and masking unrequested 
  -- data. The algorithm being applied allows communication with components that only support single narrow transfer on the same 
  -- AXI4_DATA_WIDTH slot. Thus, the BM component is completly ignorant of how the transaction is really performed at the 
  -- AXI network, by only reading or writing the data it requests to the manager interface.
  --
  -- Once the burst is completed, a second is generated to finish the total transfer size requested by BM component if the 
  -- total transfer size did surpass the 4kB boundary. This execution repeats compute2, compute3, handshake and transfer steps.
  --
  -- After the total transaction is completed, the interface returns to the idle state to accept new BM requests.
  --
  -- The following table points out the actions done at each stage of the operation:
  -- 
  -- IDLE     -> Accept new requests from BM component.
  -- COMPUTE1 -> Compute if 4KB address space overflow occurs calculating the ending address of the BM request.
  --             In the case it does, assert two_burst flag so a second burst is generated after completing the first.
  --             Also, split the total byte size of the transfer in both first and last bursts taking into account the 4kB boundary.
  --             Compute the number of bytes to transfer at the last beat of the whole transaction (last burst).
  -- COMPUTE2 -> Decide AXI size mode for the burst and generate the starting address aligned with the size mode for AXI request.
  --             Compute the number of bytes to skip at the transfer of the first beat of the whole transaction (first burst).
  --             Recalculate the ending address using the BM starting address requested and the byte size of the present burst.
  -- COMPUTE3 -> Set burst mode (at the moment, there's only INC).
  --             Decide burst length (number of beats) taking into account the size mode and the recalculated ending address.
  --          RD:Assert "ARVALID" since all required information for the handshake process is now registered.
  --          WR:Set the strobe bits for WRITE transaction.
  -- HANDSHAKE-> 
  --          RD:Waits for "RREADY" to load data from the AXI data bus. Loads counter with LSB aligned starting address,
  --             so the first AXI beat takes into account that it may be reading from an unaligned.
  -- TRANSFER ->
  --          RD:Reads the AXI data bus and register it onto a buffer register, which it is filtered with the appropiated byte mask,
  --             depending on the size mode of the burst, onto one of the FIFO registers.
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
    axi_addr      : std_logic_vector(31 downto 0);-- AXI output parameter: Starting pointer of the AXI burst.
    axi_valid     : std_logic;                    -- AXI output parameter: valid flag for output control signals (addr, len, size, mode).
    axi_ready     : std_logic;                    -- AXI output parameter: ready flag for input read data.
    axi_last      : std_logic;                    -- AXI input  parameter: last transaction flag of the burst.
    axi_strobe    : std_logic_vector(AXI4_DATA_WIDTH/8 - 1 downto 0); -- What AXI data lanes to read from during AXI transfer.
    axi_counter   : std_logic_vector(AXI4_FDATA_BYTE     downto 0); -- Used to count the number of bytes transfered/discarded from AXI transfer.

    first_beat    : std_logic; -- Flag asserted when computing first beat of the first burst.
    two_burst     : std_logic; -- Flag asserted when two bursts are required to complete the transaction due to surpassing the 4KB boundary.
    addr_end      : std_logic_vector(11 downto 0);-- Last address to access in INC mode. Only used to separate logic between computation1 and 2 stages.

    bm_size       : std_logic_vector(INT_BURST_WIDTH - 1 downto 0); -- Original size being requested by BM component, and then first burst size.
    rem_size      : std_logic_vector(INT_BURST_WIDTH - 1 downto 0); -- Remaining size for second burst (4KB outbounds access)
    bm_addr       : std_logic_vector(31 downto 0);                  -- Starting pointer requested by BM component.

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

    axi_data_tmp  : std_logic_vector(AXI4_DATA_WIDTH - 1 downto 0); -- AXI data bus register used to separate AXI network from interface.
    axi_bmask_tmp     : unsigned(AXI4_DATA_WIDTH/8   - 1 downto 0); -- Base mask to be shifted and filter narrow transfers from AXI read data.
    axi_valid_buffer  : std_logic;                                  -- AXI subordinate r_valid buffer used as delayed signal.
    axi_ready_buffer  : std_logic;                                  -- Manager r_ready buffer used as delayed signal.
    axi_index_buffer  : std_logic_vector(log_2(rd_n_fifo_regs) - 1 downto 0); -- AXI fifo index buffer.
    fifo_full_buffer  : std_logic_vector(rd_n_fifo_regs - 1 downto 0);    -- fifo_full buffered register.
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
    axi_addr      : std_logic_vector(31 downto 0);-- AXI output parameter: Starting pointer of the AXI burst.
    axi_valid     : std_logic;                    -- AXI output parameter: valid flag for output control signals (addr, len, size, mode).
    axi_valid_data: std_logic;                    -- AXI output parameter: valid write flag for output data.
    axi_last      : std_logic;                    -- AXI output parameter: last transaction flag of the burst.
    axi_strobe    : std_logic_vector(AXI4_DATA_WIDTH/8 - 1 downto 0); -- What AXI data lanes to read from during AXI transfer.
    axi_counter   : std_logic_vector(AXI4_FDATA_BYTE     downto 0); -- Used to count the number of bytes transfered/discarded from AXI transfer.

    first_beat    : std_logic; -- Flag asserted when computing first beat of the first burst.
    two_burst     : std_logic; -- Flag asserted when two bursts are required to complete the transaction due to surpassing the 4KB boundary.
    addr_end      : std_logic_vector(11 downto 0);-- Last address to access in INC mode. Only used to separate logic between computation1 and 2 stages.

    bm_size       : std_logic_vector(INT_BURST_WIDTH - 1 downto 0); -- Original size being requested by BM component, and then first burst size.
    rem_size      : std_logic_vector(INT_BURST_WIDTH - 1 downto 0); -- Remaining size for second burst (4KB outbounds access)
    bm_addr       : std_logic_vector(31 downto 0);                  -- Starting pointer requested by BM component.

    fifo          : rd_data_buffer(wr_n_fifo_regs    - 1 downto 0); -- AXI data bus registers which filters narrow reads from data_tmp.
    fifo_full     : std_logic_vector(wr_n_fifo_regs  - 1 downto 0); -- When asserted, the fifo(axi_index) is full or at a BM transfer.
    axi_index     : std_logic_vector(log_2(wr_n_fifo_regs) - 1 downto 0); -- Unsigned index of fifo registers for the AXI side.
    bm_index      : std_logic_vector(log_2(wr_n_fifo_regs) - 1 downto 0); -- Unsigned index of fifo registers for the BM side.

    bm_ready      : std_logic;                                      -- Buffer flag to signal data read on the BM bus.
    bm_counter    : std_logic_vector(AXI4_FDATA_BYTE     downto 0); -- Used to count how many bytes have been filled on the actual fifo register.

    --data_bus    : std_logic_vector(AXI4_DATA_WIDTH + dbits - 1 downto 0); -- Lanes register to transfer between AXI and BM data buses
    --start_strb  : std_logic_vector(AXI4_DATA_BYTE  - 1 downto 0); -- Number of right-bytes to skip when reading or writing data_bus at start of BM transfer.
    --end_strb    : std_logic_vector(AXI4_DATA_BYTE  - 1 downto 0); -- Number of bytes to read from data_bus at the last BM transfer.
    --counter     : std_logic_vector(AXI4_DATA_BYTE      downto 0); -- Used to count the number of bytes transfered/discarded from AXI or BM.
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
    axi_strobe        => (others => '0'),
    axi_counter       => (others => '0'),
    first_beat        => '1',
    two_burst         => '0',
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
    axi_data_tmp      => (others => '0'),
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
    axi_counter       => (others => '0'),
    first_beat        => '0',
    two_burst         => '0',
    addr_end          => (others => '0'),
    bm_size           => (others => '0'),
    rem_size          => (others => '0'),
    bm_addr           => (others => '0'),
    fifo              => (others => (others => '0')),
    fifo_full         => (others => '0'),
    axi_index         => (others => '0'),
    bm_index          => (others => '0'),
    bm_ready          => '0',
    bm_counter        => (others => '0')
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

  -- The procedure decide_size sets the AXI size mode to be equal or to the next higher power 
  -- of 2 respect the size being requested by the BM component in that burst. This way, 
  -- transfers with a size lower than the AXI data bus width will require only a single beat.
  -- This feature allows compatibility with subordinates that do not allow narrow transfers.
  --
  -- The particular widths of the addresses come from how much the size mode can modify the 
  -- address requested by BM, in order to align it with the size mode, leaving a starting address
  -- always equal or lower. Since the max size mode is 128, only 7 address bits are required.
  -- 
  -- In addition, the selection of the size mode is also constrained by the widths of the AXI 
  -- data bus (AXI4_DATA_WIDTH), which is managed by the size_mode_appl constant vector.

  procedure decide_size(
    size_bm   : in  std_logic_vector(INT_BURST_WIDTH - 1 downto 0); -- Size requested by BM-1
    addr_bm   : in  std_logic_vector(AXI4_DATA_BYTE  - 1 downto 0); -- LSB Address being requested by BM
    size_axi  : out std_logic_vector(2 downto 0);                   -- Size mode to set
    addr_axi  : out std_logic_vector(AXI4_DATA_BYTE  - 1 downto 0)  -- LSB Address to request at AXI
  ) is
    variable n      : std_logic_vector(0 to 6); -- 128, 64, 32, 16, 8, 4 and 2 byte size modes flags
    variable addr1  : std_logic_vector(6 downto 0);
    variable addr2  : std_logic_vector(6 downto 0);
  begin
    -- Check if size is greater or equal than 128, 64, 32, 16, 8, 4, 2, while also
    -- taking into account if AXI4_DATA_WIDTH and MAX_SIZE_BURST allow it.
    n(0)  := or_vector(size_bm(size_bm'length-1 downto 6));  -- 128 bytes
    n(1)  := ( size_bm(5) or n(0) );                         -- 64 bytes
    n(2)  := ( size_bm(4) or n(1) );                         -- 32 bytes
    n(3)  := ( size_bm(3) or n(2) );                         -- 16 bytes
    n(4)  := ( size_bm(2) or n(3) );                         --  8 bytes
    n(5)  := ( size_bm(1) or n(4) );                         --  4 bytes
    n(6)  := ( size_bm(0) or n(5) );                         --  2 bytes

    addr1 := (addr1'length - addr_bm'length - 1 downto 0 => '0') & addr_bm;

    -- Set the AXI mode size as: AXI mode to apply >= bytes to BM transfer > next lower power of 2.
    -- For example, for a BM transfer of 16 bytes (bm_in.rd_size = 15), size_axi is b'100 (16 bytes).
    -- For a transfer of 15 bytes is still b'100, while for a transfer of 17 bytes is b'101 (32 bytes).
    -- Of course, this is only if AXI4_DATA_WIDTH/8 is equal or greater than 32. If not, is it will be b'100.
    -- Also, create the AXI address aligned with the AXI size mode using bm_in.rd_addr.
    if   (n(0) = '1' and size_mode_appl(0) = '1') then
      size_axi  := "111"; -- 128 bytes size mode
      addr2     := "000" & x"0";
    elsif(n(1) = '1' and size_mode_appl(1) = '1') then
      size_axi  := "110"; --  64 bytes size mode
      addr2     := addr1(6) & "00" & x"0";
    elsif(n(2) = '1' and size_mode_appl(2) = '1') then
      size_axi  := "101"; --  32 bytes size mode
      addr2     := addr1(6 downto 5) & '0' & x"0";
    elsif(n(3) = '1' and size_mode_appl(3) = '1') then
      size_axi  := "100"; --  16 bytes size mode
      addr2     := addr1(6 downto 4) & x"0";
    elsif(n(4) = '1' and size_mode_appl(4) = '1') then
      size_axi  := "011"; --   8 bytes size mode
      addr2     := addr1(6 downto 3) & "000";
    elsif(n(5) = '1' and size_mode_appl(5) = '1') then
      size_axi  := "010"; --   4 bytes size mode
      addr2     := addr1(6 downto 2) & "00";
    elsif(n(6) = '1' and size_mode_appl(6) = '1') then
      size_axi  := "001"; --   2 bytes size mode
      addr2     := addr1(6 downto 1) & '0';
    else
      size_axi  := "000"; --   1 byte  size mode
      addr2     := addr1;
    end if;

    addr_axi := addr2(addr_axi'range);

  end procedure decide_size;


  -- The number of bytes to burst is used to decide the number of beats at the burst. This is computed
  -- by applying a number of right-shifts equal to the AXI's size mode (the codification, no its actual
  -- decoded size meaning) to the size to burst. 
  --
  -- In addition, if the burst will access to two different AXI4_DATA_WIDTH spaces of the same subordinate, 
  -- due to unaligned address request, the numbers of beats must be increased by one. Furthermore, the AXI mode
  -- that use the whole AXI data bus width also requires an additional beat on unaligned address requests.

  procedure decide_len(
    size      : in  std_logic_vector(INT_BURST_WIDTH - 1 downto 0); -- Transfer size of the burst (actual num needs +1)
    size_mode : in  std_logic_vector(2 downto 0); -- AXI size mode being set
    bm_addr   : in  std_logic_vector(31 downto 0);-- LSB of the BM request address access
    addr_end  : in  std_logic_vector(rd.addr_end'range);-- LSB of the ending address of the present burst
    burst_len : out std_logic_vector(7 downto 0)  -- AXI burst length (actual num of beats needs +1)
    ) is
      variable len_temp : std_logic_vector(size'range); -- Temp var because VHDL is really strong /typed/
      variable len      : std_logic_vector(8 downto 0); -- The max number of beats allowed at AXI4 is 256
      variable one      : std_logic;                    -- +1 beat under special conditions
  begin
    -- Number of beats required = bytes to transfer-1 >> size_mode
    -- This is only true for aligned address access and works because the size mode applied is always
    -- equal or greater than the real size (bm_size + 1).
    len_temp  := std_logic_vector(shift_right( unsigned(size), to_integer(unsigned(size_mode)) ));

    -- However, in unaligned transactions, sizes lower than AXI4_DATA_WIDTH may require two bursts if 
    -- they write onto two different AXI4_DATA_WIDTH spaces. Thus, this is signaled by the two_width 
    -- input flag, computed by calculating the ending address with the size of the present burst.
    --
    -- Furthermore, unaligned address access with an AXI mode that use the whole AXI data bus width 
    -- also requires an additional beat, since there will be an additional AXI4_DATA_WIDTH memory access.
    if(size_array(to_integer(unsigned(size_mode))) = AXI4_DATA_WIDTH/8) then
      one := to_std_logic( bm_addr(AXI4_DATA_BYTE - 1 downto 0) /= (AXI4_DATA_BYTE - 1 downto 0 => '0') );
    else
      one := to_std_logic( rd.bm_addr(11 downto AXI4_DATA_BYTE) /= addr_end(11 downto AXI4_DATA_BYTE) );
    end if;

    burst_len := add_vector(len_temp, '0' & one, burst_len'length);

  end procedure decide_len;


  -- Sets the strobe signal to ones with a number of right-zeros equal to the value encoded on byte_strb.

  --procedure decide_strobe(
  --  variable byte_strb  : in  std_logic_vector(wr.start_strb'range);
  --  variable axi_strb   : out std_logic_vector(wr.axi_strobe'range)
  --  ) is
  --    variable strb : std_logic_vector(wr.axi_strobe'range) := (others => '1');
  --begin
  --  strb      := std_logic_vector(shift_left( unsigned(strb), to_integer(unsigned(byte_strb)) ));
  --  axi_strb  := strb;
  --end procedure decide_strobe;

  
begin -- rtl

  -----------------
  -- Assignments --
  -----------------
  
  -- Advance eXtensible Interface (interconnect bus)
    -- Write address channel out
  axi4mo.aw_id        <= std_logic_vector(to_unsigned(axi_id, axi4mo.aw_id'length));
  axi4mo.aw_addr      <= wr.axi_addr(31 downto AXI4_DATA_BYTE) & (AXI4_DATA_BYTE-1 downto 0 => '0'); -- Address
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
  --axi4mo.w_data       <= wr.fifo(to_integer(unsigned(wr.axi_index)));
  --axi4mo.w_strb       <= wr.axi_strobe;
  --axi4mo.w_last       <= wr.axi_last;
  --axi4mo.w_valid      <= wr.axi_valid_data;
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
  --axi4mi.aw_ready;--
  --  -- Write data channel in
  --axi4mi.w_ready; --
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
  --bm_out.wr_req_grant;
  --bm_out.wr_full <= not(wr.bm_ready);
  --bm_out.wr_done;
  --bm_out.wr_err;

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
    variable rd_addr_strt : std_logic_vector( AXI4_DATA_BYTE - 1         downto 0 );-- AXI LSB starting address
    variable rd_axi_size  : std_logic_vector( rd.axi_size'range                   );-- AXI size mode
    variable rd_axi_len   : std_logic_vector( rd.axi_len'range                    );-- AXI burst length
    variable rd_axi_next_counter  : std_logic_vector(rd.axi_counter'range         );-- Next AXI counter
    variable rd_axi_next_index    : std_logic_vector(rd.axi_index'range           );-- Next buffer index

    variable rd_data_fwidth: std_logic_vector(AXI4_DATA_WIDTH + dbits - 1 downto 0);-- BM full shifted data bus
    variable rd_bm_counter        : std_logic_vector(rd.bm_counter'range          );-- Next BM counter
    variable rd_fifo_offset       : std_logic_vector(rd.bm_counter'range          );-- A fifo offset to simplify
    variable rd_bm_shift          : std_logic                                      ;-- BM shift flag
    variable rd_data_empty        : boolean                                        ;-- rd.fifo(bm_index) register depleted flag
    variable rd_bm_next_index     : std_logic_vector(rd.bm_index'range            );-- Next AXI counter
    variable rd_bm_done           : std_logic                                      ;-- Last BM transfer, generate BM mask.
    
  begin
    -- Default variable values
    rd_addr_end         := (others => '0');
    rd_addr_strt        := (others => '0');
    rd_axi_size         := (others => '0');
    rd_axi_len          := (others => '0');
    rd_axi_next_counter := (others => '0');
    rd_axi_next_index   := (others => '0');
    rd_data_fwidth      := (others => '0');
    rd_bm_counter       := (others => '0');
    rd_fifo_offset      := (others => '0');
    rd_bm_shift         := '0';
    rd_data_empty       := FALSE;
    rd_bm_next_index    := (others => '0');
    rd_bm_done          := '0';

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
              rd.bm_size    <= bm_in.rd_size; -- Load BM size to transfer (real is +1)
              -- Next, check 4kB out of bounds access
              rd.state      <= compute1;
            end if;
          
          
          when compute1 => -- Worst delay path: ADD 12+12, SUB 13-13
            -- Check if transaction will access two 4KB address regions
            rd_addr_end     := add_vector(rd.bm_size, rd.bm_addr(11 downto 0), rd_addr_end'length);
            rd.two_burst    <= rd_addr_end(12);
          
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
          
          
          when compute2 => -- Worst delay path: DECIDE_SIZE()
            -- Decide AXI size mode and update starting address with aligned address to that size mode.
            decide_size(rd.bm_size, rd.bm_addr(AXI4_DATA_BYTE-1 downto 0), rd_axi_size, rd_addr_strt);
            rd.axi_size     <= rd_axi_size;
            rd.axi_addr     <= rd.bm_addr(31 downto AXI4_DATA_BYTE) & rd_addr_strt;

            -- Compute the ending address of this burst (using the BM pointer) with the BM size of this burst to check if two different 
            -- AXI4_DATA_WIDTH spaces from the same subordinate are required to be accessed.
            rd.addr_end     <= add_vector(rd.bm_size, rd.bm_addr(11 downto 0), rd.addr_end'length);
          
            -- Save the number of bytes to discard unrequested data at the first BM transfer = LSB BM unaligned address + BM data bus width bytes
            if(rd.first_beat = '1') then
              rd.start_strb <= ((rd.start_strb'length - AXI4_DATA_BYTE - 1) downto 0 => '0') & rd.bm_addr(AXI4_DATA_BYTE-1 downto 0);
            end if;
          
            -- Next compute state
            rd.state        <= compute3;
          
          
          when compute3 => -- Worst delay path: DECIDE_LEN()
            -- Compute how many beats will be necessary to transfer due to axi_size mode.
            decide_len(rd.bm_size, rd.axi_size, rd.bm_addr, rd.addr_end, rd_axi_len);
            rd.axi_len      <= rd_axi_len;
            rd.bm_size      <= add_vector(rd.bm_size, 1, rd.bm_size'length); -- Update size to be real size.
          
            -- Set the burst transfer mode.
            rd.axi_mode     <= INC;
          
            -- Next, the handshake step.
            rd.axi_valid    <= '1'; -- Request AXI read burst.
            rd.state        <= handshake;
          
          
          when handshake =>
            -- Having all computation steps and proper registration separated allows maximum 
            -- frequency of operation when including this AXI manager interface at the interconnect
            -- bus. For a read burst, ar AXI control buses is used to send the control data.
            if (axi4mi.ar_ready = '1') then
              rd.axi_valid  <= '0'; -- At request being granted, deassert request
              rd.axi_ready  <= '1'; -- Read data from AXI bus flag
            
              -- Initilize counter with the byte lane it is starting the AXI read operation
              rd.axi_counter   <= (rd.axi_counter'length-AXI4_DATA_BYTE-1 downto 0 => '0') & rd.axi_addr(AXI4_DATA_BYTE-1 downto 0);

              -- Create the LSB mask to be shifted on transfer stage to generate the byte mask for the AXI read transfer.
              rd.axi_bmask_tmp <= unsigned(mask_array(to_integer(unsigned(rd.axi_size)))(rd.axi_bmask_tmp'range));
            
              -- Next, beat transfers
              rd.state      <= transfer;
            end if;
          
          
          when transfer => -- AXI transfer --
          -- The AXI transfer occurs by buffering the read data onto the rd.axi_data_tmp register when the subordinate flags valid data.
          -- Then, the rd.axi_data_tmp is filtered onto an fifo register, which will be used at the BM transfer. However, due to this single clock 
          -- cycle delay between AXI read and data filtering, both valid and the mask signals are also buffered on the "buffer" registers (these
          -- are outside of the state machine). 
          -- The counter on this step is used to pause the AXI read transfer, due to the present fifo register being full 
          -- while also setting the shift of the rd.axi_strobe to mask the appropiate byte lanes in narrow transfers.

              -- Check if subordinate is delivering valid data with the same AXI ID as requested and if manager is listening.
            if (axi4mi.r_valid = '1' and axi4mi.r_id = std_logic_vector(to_unsigned(axi_id, axi4mi.r_id'length)) and rd.axi_ready = '1') then

              -- Register inputs to separate AXI bus from further computation.
              rd.axi_data_tmp     <= axi4mi.r_data;
              rd.axi_last         <= axi4mi.r_last;
              rd.bm_error         <= axi4mi.r_resp(1);
            
              -- Count how many bytes are being read into a fifo register. Increase the counter in AXI size mode steps.
              rd_axi_next_counter := add_vector(rd.axi_counter, size_array(to_integer(unsigned(rd.axi_size))), rd.axi_counter'length);
              rd.axi_counter      <= rd_axi_next_counter;

              -- Using the next counter, set the mask to filter the byte lanes to read from at the following clock cycle.
              rd.axi_strobe       <= std_logic_vector(shift_left(rd.axi_bmask_tmp, to_integer(unsigned(rd.axi_counter))));
            
              -- Be it the last beat of the burst or fifo register is full, change the fifo index for the next AXI transfer.
              if(axi4mi.r_last = '1' or rd_axi_next_counter(AXI4_DATA_BYTE) = '1') then 
                -- Compute next fifo index.
                rd_axi_next_index := add_vector(rd.axi_index, 1, rd.axi_index'length);
                rd.axi_index      <= rd_axi_next_index;

                -- Deassert the ready flag to finish AXI burst if it is the last beat or if there is not enough space in the fifo.
                rd.axi_ready      <= not( axi4mi.r_last or rd.fifo_full(to_integer(unsigned(rd_axi_next_index))) );
                
                -- Signal the present rd.fifo register as prepared for BM transfer, for whenever it is due to buffers.
                rd.fifo_full(to_integer(unsigned(rd.axi_index))) <= '1';
                
                -- The last AXI transfer of the whole transaction will set the proper rd.fifo_last bit, so the BM transfer logic knows when to end.
                rd.fifo_last(to_integer(unsigned(rd.axi_index))) <= not(rd.two_burst) and axi4mi.r_last;
              end if;

            else
            -- When the manager is not listening to new data, be it the AXI burst has finished, there is not enough 
            -- space in FIFO or subordinate is not sending valid data yet.

              -- If the AXI burst has finished (rd.axi_ready must be deasserted by now), set the control data for a second
              -- burst if it is necessary (rd.two_burst = 1) or wait until the last BM transfer resets the whole read block.
              if(rd.axi_last = '1') then
                if(rd.two_burst = '1') then
                  rd.two_burst  <= '0';
                  rd.first_beat <= '0';
                  rd.axi_last   <= '0';
                  rd.bm_size    <= rd.rem_size;
                  rd.bm_addr    <= add_vector(rd.bm_addr(31 downto 12), 1, 20) & (11 downto 0 => '0');
                  rd.state      <= compute2;
                end if;
              else  -- In case it is not the last AXI beat, check if the present fifo register can be used for another read beat.
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
        -- During each AXI data read, filter the data to only read the expected byte lanes at that clock cycle.
        if(rd.axi_valid_buffer = '1' and rd.axi_ready_buffer = '1') then
          for k in rd.axi_strobe'range loop
            rd.fifo(to_integer(unsigned(rd.axi_index_buffer)))(k*8+7 downto k*8) 
              <= rd.axi_data_tmp(k*8+7 downto k*8) and (7 downto 0 => rd.axi_strobe(k));
          end loop;
        end if;


      -----------------------
      -- BM TRANSFER LOGIC --
      -----------------------
      -- The BM transfer logic transfers from the RD FIFO to the BM component at dbits bits per clock cycle throughput without pause, unless  
      -- there's not enough data in the fifo to BM transfer. This is achieved by shifting rd.fifo(bm_index) onto itself and the rd.bm_data 
      -- used as output (after masking for last BM transfer), until there's not enough data in the present fifo register. Then, the next  
      -- fifo register is used with rd.fifo(bm_next_index) with also the remaining data on present fifo register. After fully reading a fifo 
      -- register, this one is set free to be used at new AXI beat readings.

        -- Set combinational variables --
        -- Check if this is the last BM transfer before rd.fifo(bm_index) is depleted after a transfer of dbits bits.
        rd_data_empty       := rd.bm_counter <= std_logic_vector(to_unsigned(dbits/8, rd.bm_counter'length));
        rd_bm_next_index    := add_vector(rd.bm_index, 1, rd.bm_index'length);

        -- Check if there's data to BM transfer. Delayed with a buffer register so the last AXI beat transfer is finished on this fifo register.
        if(rd.fifo_full_buffer(to_integer(unsigned(rd.bm_index))) = '1') then

        -- Initialize rd_bm_counter before any BM transfer with AXI data bytes or with the ending strobe if there's only one single BM transfer.
        -- Then shift the fifo register, not to read data from, but to flush unrequested data (totally in next clock cycle) and update rd.bm_counter.
          if(rd.bm_first = '1') then

            -- The initial counter is set by rd.end_strb if there's only one fifo register to transfer. Otherwise, the initial counter is the AXI data bytes.
            if(rd.fifo_last(to_integer(unsigned(rd.bm_index))) = '1') then
              rd_bm_counter := rd.end_strb;
            else
              rd_bm_counter := std_logic_vector(to_unsigned(AXI4_DATA_WIDTH/8, rd.bm_counter'length));
            end if;

            -- Decrement the shift to the initial counter and use it to initilize rd.bm_counter.
            rd.bm_counter   <= sub_vector(rd_bm_counter, rd.start_strb, rd.bm_counter'length);
            --
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

                  -- In addition, if the number of bytes read on this clock cycle from rd.fifo(rd_axi_next_index) include all the data required 
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
                  --if(dbits/8 = to_integer(unsigned(rd.bm_counter))) then
                    rd_bm_done    := '1';
                    rd.bm_done    <= '1';
                    rd.state      <= idle;
                  --end if;
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
    variable wr_addr_strt : std_logic_vector( AXI4_DATA_BYTE - 1         downto 0 );-- AXI LSB starting address
    variable wr_axi_size  : std_logic_vector( wr.axi_size'range                   );-- AXI size mode
    variable wr_axi_len   : std_logic_vector( wr.axi_len'range                    );-- AXI burst length
    variable wr_axi_last  : std_logic                                              ;-- AXI last beat signal previous to buffering
    variable wr_axi_decrement     : std_logic_vector(wr.axi_counter'range         );-- Number of bytes send to write on the present beat
    variable wr_axi_next_index    : std_logic_vector(wr.axi_index'range           );-- Next buffer index

    variable wr_data_fwidth: std_logic_vector(AXI4_DATA_WIDTH + dbits - 1 downto 0);-- BM full shifted data bus
    variable wr_bm_counter        : std_logic_vector(wr.bm_counter'range          );-- Next BM counter
    variable wr_fifo_offset       : std_logic_vector(wr.bm_counter'range          );-- A fifo offset to simplify
    variable wr_bm_shift          : std_logic                                      ;-- BM shift flag
    variable wr_data_empty        : boolean                                        ;-- wr.fifo(bm_index) register depleted flag
    variable wr_bm_next_index     : std_logic_vector(wr.bm_index'range            );-- Next AXI counter
    variable wr_bm_done           : std_logic                                      ;-- Last BM transfer, generate BM mask.
    
  begin
    -- Default variable values
    wr_addr_end         := (others => '0');
    wr_addr_strt        := (others => '0');
    wr_axi_size         := (others => '0');
    wr_axi_len          := (others => '0');
    wr_axi_next_index   := (others => '0');
    wr_data_fwidth      := (others => '0');
    wr_bm_counter       := (others => '0');
    wr_fifo_offset      := (others => '0');
    wr_bm_shift         := '0';
    wr_data_empty       := FALSE;
    wr_bm_next_index    := (others => '0');
    wr_bm_done          := '0';

    if (rstn = '0' and ASYNC_RST) then
      wr                    <= RST_TRANSF_WR_OP;
    elsif rising_edge(clk) then
      if (rstn = '0') then
        wr                  <= RST_TRANSF_WR_OP;
      else
        ---------------------------
        -- AXI RD TRANSFER LOGIC --
        ---------------------------
        case wr.state is
          when idle => -- Worst delay path: BM component output
            -- Reset registers
            wr              <= RST_TRANSF_WR_OP;
          
            -- Load request information from BM
            if (bm_in.wr_req = '1' and wr.bm_grant = '1') then
              wr.bm_grant   <= '0';           -- Deassert granting requests for BM component
              wr.bm_addr    <= bm_in.wr_addr; -- Load starting address request
              wr.bm_size    <= bm_in.wr_size; -- Load BM size to transfer (real is +1)
              -- Next, check 4kB out of bounds access
              wr.state      <= compute1;
            end if;
          
          
          when compute1 => -- Worst delay path: ADD 12+12, SUB 13-13
            -- Check if transaction will access two 4KB address regions
            wr_addr_end     := add_vector(wr.bm_size, wr.bm_addr(11 downto 0), wr_addr_end'length);
            wr.two_burst    <= wr_addr_end(12);
          
            -- If transaction must be split in two bursts, due to 4KB overflow boundary, calculate...
            if(wr_addr_end(12) = '1') then
              -- first burst size (-1 from real size)  MAX=4095, MIN=0
              wr.bm_size    <= sub_vector(x"FFF", wr.bm_addr(11 downto 0), wr.bm_size'length);
              -- second burst size (-1 from real size) MAX=4094, MIN=0
              wr.rem_size   <= wr_addr_end(wr.rem_size'range);
            end if;
          
            -- Next, compute size mode
            wr.state        <= compute2;
          
          
          when compute2 => -- Worst delay path: DECIDE_SIZE()
            -- Decide AXI size mode and update starting address with aligned address to that size mode.
            decide_size(wr.bm_size, wr.bm_addr(AXI4_DATA_BYTE-1 downto 0), wr_axi_size, wr_addr_strt);
            wr.axi_size     <= wr_axi_size;

            -- Compute the ending address of this burst (using the BM pointer) with the BM size of this burst to check if two different 
            -- AXI4_DATA_WIDTH spaces from the same subordinate are required to be accessed.
            wr.addr_end     <= add_vector(wr.bm_size, wr.bm_addr(11 downto 0), wr.addr_end'length);
          
            -- Next compute state
            wr.state        <= compute3;
          
          
          when compute3 => -- Worst delay path: DECIDE_LEN()
            -- Compute how many beats will be necessary to transfer due to axi_size mode.
            decide_len(wr.bm_size, wr.axi_size, wr.bm_addr, wr.addr_end, wr_axi_len);
            wr.axi_len      <= wr_axi_len;

            -- Add to the BM size the offset on the AXI4_DATA_WIDTH slot, so at the last AXI transfer the mask is correct.
            wr.bm_size    <= add_vector(wr.bm_size, wr.bm_addr(AXI4_DATA_BYTE-1 downto 0), wr.bm_size'length); -- Real size still -1
          
            -- Set the burst transfer mode.
            wr.axi_mode     <= INC;
          
            -- Next, the handshake step.
            wr.axi_valid    <= '1'; -- Request AXI write burst.
            wr.state        <= handshake;
          
          
          when handshake =>
            -- Having all computation steps and proper registration separated allows maximum 
            -- frequency of operation when including this AXI manager interface at the interconnect
            -- bus. For a write burst, aw AXI control buses is used to send the control data.
            if (axi4mi.aw_ready = '1') then
              wr.axi_valid  <= '0'; -- At request being granted, deassert AXI handshake request
            
              -- Next, beat transfers
              wr.state      <= transfer;
            end if;
          
          
          when transfer => -- AXI transfer --
          -- The AXI transfer sets the AXI valid write flag anytime there's data available in the fifo. For each beat transfered to the AXI network, the fifo
          -- register is set to be overwritten by the BM transfer logic. In addition, the AXI length is decreased by one to generate the AXI last write flag.
          -- At the last beat, it is checked if another burst is required, which prompts the SFM to generate another burst.

            -- Load the variables for last AXI beat in the burst and next fifo index at the AXI side.
            wr_axi_last         := to_std_logic(wr.axi_len = (wr.axi_len'range => '0'));
            wr_axi_next_index   := add_vector(wr.axi_index, 1, wr.axi_index'length);

            -- Set the AXI strobe for the next AXI write beat.
            for k in wr.axi_strobe'range loop
              if( k < to_integer(unsigned(wr.bm_addr(AXI4_DATA_BYTE-1 downto 0))) or k > to_integer(unsigned(wr.bm_size)) ) then
                wr.axi_strobe(k)  <= '0';
              else
                wr.axi_strobe(k)  <= '1';
                wr_axi_decrement  := add_vector(wr_axi_decrement, 1, wr_axi_decrement'length);
              end if;
            end loop;

            -- At burst finish, deassert the AXI write valid and last beat flags when subordinate is listening.
            if(wr.axi_last = '1') then
              if(axi4mi.w_ready = '1') then
                wr.axi_valid_data <= '0';
                wr.axi_last       <= '0';

                -- Set the present fifo register to be overwritten with new data at the BM transfer logic.
                wr.fifo_full(to_integer(unsigned(wr.axi_index))) <= '0';
                wr.axi_index      <= wr_axi_next_index;

                -- Update size left to write
                wr.bm_size        <= sub_vector(wr.bm_size, wr_axi_decrement, wr.bm_size'length);

                -- LSB from wr.bm_addr are no longer needed.
                wr.bm_addr(AXI4_DATA_BYTE-1 downto 0) <= (others => '0');

                -- In the case there's another burst, generate the control data for the next handshake.
                if(wr.two_burst = '1') then
                  wr.two_burst  <= '0';
                  wr.first_beat <= '0';
                  wr.bm_size    <= wr.rem_size;
                  wr.bm_addr    <= add_vector(wr.bm_addr(31 downto 12), 1, 20) & (11 downto 0 => '0');
                  wr.state      <= compute2;
                else
                -- If not, return to accept new BM write transaction request.
                  wr.state      <= idle;
                end if;
              end if;


            -- If the manager is not sending data, check if there's available data to send.
            elsif(wr.axi_valid_data = '0') then
              wr.axi_valid_data <= wr.fifo_full(to_integer(unsigned(wr.axi_index)));
              wr.axi_last       <= wr_axi_last;


            -- Check if the subordinate is listening for the data the manager is sending.
            elsif (axi4mi.w_ready = '1') then

              -- Assert the AXI valid write flag if the next fifo register has data to send.
              wr.axi_valid_data <= wr.fifo_full(to_integer(unsigned(wr_axi_next_index)));
              wr.axi_last       <= wr_axi_last;

              -- Set the present fifo register to be overwritten with new data at the BM transfer logic.
              wr.fifo_full(to_integer(unsigned(wr.axi_index))) <= '0';
              wr.axi_index      <= wr_axi_next_index;

              -- Update size left to write
              wr.bm_size        <= sub_vector(wr.bm_size, wr_axi_decrement, wr.bm_size'length);

              -- LSB from wr.bm_addr are no longer needed.
              wr.bm_addr(AXI4_DATA_BYTE-1 downto 0) <= (others => '0');
              
              -- Decrease the number of beats left to execute.
              wr.axi_len        <= sub_vector(wr.axi_len, 1, wr.axi_len'length);

            end if;


          when others =>

        end case;


      -------------
      -- WR FIFO --
      -------------
      case wr.state is
        when compute1 =>
          wr.bm_counter <= add_vector(wr.bm_addr(AXI4_DATA_BYTE - 1 downto 0), 1, wr.bm_counter'length);
          wr.bm_ready   <= '1';
      
        when compute2 | compute3 | handshake | transfer =>
          if( wr.bm_ready = '1') then
            wr.bm_counter <= (others => '0');

          end if;

        when others =>

      end case;
      
      
      end if;
    end if;
  end process write_proc;

  -----------------------------------------------------------------------------
  -- Combinational process
  -----------------------------------------------------------------------------


end architecture rtl;
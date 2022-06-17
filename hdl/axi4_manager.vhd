-----------------------------------------------------------------------------   
-- Entity:      axi4_manager
-- File:        axi4_manager.vhd
-- Author:      Francisco Javier Fuentes Diaz (BSC-CNS)
-- Description: AXI4 FULL Manager entity.
------------------------------------------------------------------------------ 
--  Changelog:
--              - v0.8.6  May  5, 2022.
--                Added input signals for setting the AXI burst parameters as fixed address 
--                transactions, CACHE and PROT, so they can be configured by the BM component.
--                The Injector_implementation option has been dropped, since the functionality 
--                did not save particulary many resources while not being that useful if the BM 
--                component scales correctly with dbits.
--
--              - v0.8.5  Mar 29, 2022.
--                Now the rd.bm_valid is asserted correctly on discarded data read transactions
--                due to Injector_implementation being TRUE.
--                All AXI parametrizable variables, as bus widths, have been moved to generics.
--                This, while making easier the integration of the interface, it also has 
--                expanded the bus widths to the maximum allowed by the protocol, in order to 
--                accomodate such cases. However, the synthesis tool should remove unnused wiring.
--
--              - v0.8.2  Mar 11, 2022.
--                Configurable parameters of the number of FIFO registers and injector 
--                mode have been moved as generics, allowing multiple instances with 
--                different properties. AXI infrastructure properties are still general, thou.
--
--              - v0.8.1  Mar  1, 2022.
--                I/O types for the BM component have been renamed to bm_miso and
--                bm_mosi, while the AXI I/O types have been renamed axi4_miso and
--                axi4_mosi. In this aspect, the design perspective is that the BM
--                component is the "Manager" for the interface, while the interface is 
--                the subordinate for the BM component. Thus, the BM componente outputs 
--                MOSI signals to the interface, and the interface outputs MISO signals 
--                to the BM component.
--                Bug solved where the write strobe would be incorrect at the last beat of  
--                the burst if the beat was not transmited (w_ready = 1) on that cycle.
--                Modifications have been made to achieve synthesis in isolated module mode.
--
--              - v0.8    Feb 25, 2022. 
--                First deliver of the AXI Manager interface as a working module.
--                It has passed manual debugging. There may be errors not found, 
--                specially on write transactions. Contact the author through GitLab 
--                opening an issue ticket specifying the request data and the problem.
--                Synthesis optimization is yet to be done.
--
------------------------------------------------------------------------------ 

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library safety;
use safety.axi4_pkg.all; -- <- It contains the configuration of the interface.

-----------------------------------------------------------------------------
-- AXI4 FULL Manager - bus manager bridge
--
-- Manager interface features:
--
-- - Only INC burst mode is available and every burst generated use all AXI data lanes available per beat.
-- - Full support for unaligned address requests. This includes even if two subordinates are accessed in a 
--   single transaction request.
-- - Number of bytes per BM transaction requests allowed is 4096 (encoded as 0xFFF).
-- - BM done output flag assertion at the end of every transaction on its appropiated bus (reads or writes).
-- - Extensible BM component and AXI data buses width 8, 16, 32, 64, 128, 256, 512 and 1024 bits (read 
--   further considerations on integration of the interface for limitations).
-- - Continuous data transmission if there's enough data supply throughput from the least data bus bottleneck.
--
--
-- Manager interface and considerations during integration:
--
-- - DATA_WIDTH >= dbits. The AXI data bus width must be greater or equal to the BM data bus.
--
-- - BM size requests <= 4096 (4kB, limited by AXI4 addressing rule). The maximum number of bytes 
--   per BM transaction is 4096 (encoded as 0xFFF).
--
-- - Little endian data structure. Higher bit position links to higher memory positions.
--
-- - Only INC burst mode is implemented. This is the mode where the addressing increases every beat 
--   at AXI data width bytes steps on the AXI side.
--
-- - Unaligned access by BM requests are supported through aligning the address with AXI data bus width,
--   while delivering only the requested data on read transactions and using the write strobe on AXI 
--   write transactions.
--
-- - The Manager only execute bursts with the AXI size that use the whole AXI data bus width.
--
-- - This interface requires the signaling of the last AXI beat to be at the correct beat on read 
--   transactions, or unrequested data could be read instead of the requested.
--
-- - The interface may send control and data when the valid flag is not asserted. These signals 
--   must be discarded since the valid flag is low.
--
--
-- This AXI4 Manager interface translates and manage the requests made by the BM component,
-- using the BM input and output buses, to the AXI4 network, using the AXI4 input and output buses.
--
-- The BM component has an opaque vision of what the interface requests to the AXI network, since 
-- it only receives the data that it has been requested. Be it requests with unaligned or even to multiple 
-- subordinates access (only 2 as maximum), the interface filters it to deliver only the requested data.
-- 
-- Most I/O ports are buffered on registers to promote higher maximum frequency of operation at 
-- implementation, with minimum logic between register and BM or AXI bus. In any case, there is not
-- a direct I/O path that could greatly impact the fMAX once the interface is integrated on the project.
-- 
-- If the BM request implies access to two different subordinates, the Manager interface will generate two 
-- batches of data that are distribuited in one or multiple bursts each, with the appropiated addressing 
-- and burst length (number of beats in the burst) for each burst). Multiple bursts for the same batch
-- depends on the AXI data bus width, since implemenations with an AXI data bus width lower than 128 bits 
-- require multiple bursts to the same subordinate to be able to access the 4kB of data (4096 addresses) 
-- it can allocate (limit listed by the AXI4 protocol).
-- 
-- 
-----------------------------------------------------------------------------

entity axi4_manager is
  generic (
    -- AXI Manager configuration
    ID_R_WIDTH      : integer range  0 to   32  := 4;       -- AXI ID's bus width.
    ID_W_WIDTH      : integer range  0 to   32  := 4;       -- AXI ID's bus width.
    ADDR_WIDTH      : integer range 12 to   64  := 32;      -- AXI address bus width. (Tested only for 32 bits)
    DATA_WIDTH      : integer range  8 to 1024  := 128;     -- AXI data bus width. [Only power of 2s are allowed]
    axi_id          : integer range  0 to 32**2-1 := 0;     -- AXI manager burst index [Must be < ID_X_WIDTH**2-1]

    -- Bus Manager (BM) configuration
    dbits           : integer range  8 to 1024  := 32;      -- BM data bus width [Only power of 2s are allowed and <= AXI_DATA_WIDTH]

    -- Interface configuration
    rd_n_fifo_regs  : integer range  2 to  256  := 4;       -- Number of FIFO registers to use at AXI read transactions.  [Only power of 2s are allowed]
    wr_n_fifo_regs  : integer range  2 to  256  := 4;       -- Number of FIFO registers to use at AXI write transactions. [Only power of 2s are allowed]
    ASYNC_RST       : boolean                   := FALSE    -- Allow asynchronous reset
  );
  port (
    rstn                      : in  std_ulogic; -- Reset
    clk                       : in  std_ulogic; -- Clock
    -- AXI interface signals
    axi4mi                    : in  axi4_miso;  -- AXI4 manager input 
    axi4mo                    : out axi4_mosi;  -- AXI4 manager output
    -- BM component signals
    bm_in                     : in  bm_mosi;    -- BM interface input
    bm_out                    : out bm_miso     -- BM interface output
  );
end entity axi4_manager;

architecture rtl of axi4_manager is
  -----------------------------------------------------------------------------
  -- Constant declaration
  -----------------------------------------------------------------------------

    -- AXI constants
    constant AXI4_DATA_BYTE   : integer := log_2(DATA_WIDTH/8);  -- Number of bits required to address the AXI data bus bytes.
    constant AXI4_FDATA_BYTE  : integer := max((AXI4_DATA_BYTE, log_2(dbits/8)))+1; -- To acommodate dbits larger than AXI data bus widths. WIP

    -- The Mult_bursts_subor flag indicates when TRUE that the implementation allows for the possible requirement of multiple bursts to the same 
    -- subordinate (same 4kB memory space) to satisfy the size requested by the BM component (maximum 4096 bytes, encoded as 0xFFF in bm_in.bm_size).
    constant Mult_bursts_subor: boolean := DATA_WIDTH < 128;

  -----------------------------------------------------------------------------
  -- Records and types
  -----------------------------------------------------------------------------
  --
  -- This interface, when idle, accepts read and write transaction requests from the BM bus, listening the starting address and the total 
  -- transfer size of the operation. This last must be lower or equal to 4096 and it is encoded as an unsigned integer decremented by one 
  -- respect the real size. So a bm_in.rd_data 0x000 means 1 byte transfer, and the maximum is 0xFFF that is 4096 bytes to transfer.
  --
  -- Once the request has been granted (deassert granting new requests for that type of transaction until finished), the interface processes 
  -- the control data to check if the total transfer surpasses the 4kB address boundary, splitting the transaction in two batches 
  -- in such situations. This is done to arrange the transfer sizes of the batches for the first and second AXI subordinates, having 
  -- each different 4kB address allocation spaces.
  -- 
  -- Then, the interface sets an AXI size mode to use the whole data bus width of the AXI side (DATA_WIDTH), computes the number of beats 
  -- (burst length) to satisfy the request transfer and aligns the address requested (bm_addr) with the AXI data bus width (DATA_WIDTH).
  -- In case a single burst is not enough to satisfy the transfer size for the subordinate in question, the maximum length will be set (256 beats).
  -- However, this can only happen at implementations that have an DATA_WIDTH < 128 bits. Thus, this is managed with the "Mult_bursts_subor"
  -- flag to only proceed with the correspondent checks to generate multiple bursts to the same subordinate in the implementation if it's the case.
  --
  -- Since the BM component may request access to an unaligned address respect the DATA_WIDTH slot, the interface rearrenges the data to only 
  -- transfer to the BM component the data from the address requested on read transactions and to start writting from the address requested using 
  -- the byte strobe (wr.axi_strobe) on write transactions, even though the AXI Manager interface may request access to more data bytes.
  --
  -- The interface depends on the assertion of the AXI last signal on the last beat of the burst on read transactions in order to stop the transfer. 
  -- Otherwise, the interface may read data from the AXI data bus that should not and may transfered to the BM component instead of the requested data.
  -- 
  --
  -- The following table summarizes the actions done at each stage of the operation in both and on each type of transaction:
  -- 
  -- IDLE     -> Accept new requests from BM component.
  -- COMPUTE1 -> Compute if 4KB address space overflow occurs calculating the ending address of the BM request.
  --             In the case it does, assert two_subor flag so a second data batch is processed after completing the first.
  --             Also, split the total byte size of the transfer in both first and last batches taking into account the 4kB boundary.
  --          RD:Register the number of bytes to transfer at the last BM transfer of the whole transaction. This value is used to generate
  --             the mask applied on this last BM transfer, so unrequested data is not sent to the BM component.
  -- COMPUTE2 -> Set the AXI size mode to use the whole AXI data bus width (encoded as log2(DATA_WIDTH/8)). 
  --             Also, generate an aligned address with the DATA_WIDTH slot to be used at the AXI network handshake.
  --             Set the burst mode (at the moment, this interface does only apply INC bursts).
  --             Decide burst length (number of beats) taking into the number of bytes to transfer and the address requested.
  --             Assert the AX valid signal (X is R for read or W for write transactions) to process the handshake of the burst.
  -- HANDSHAKE-> Waits for a AXI X ready input signal assertion so the handshake is executed and the AXI transfer can start.
  -- TRANSFER -> On the end of a burst on both transaction types (read transactions require the assertion of the last beat input signal 
  --             by part of the subordinate on the correct beat), the interface will check if any more data must be requested to the 
  --             actual subordinate. In affirmative case, another burst will be generated with the leftover number of bytes to transfer
  --             and the appropiated address. However, if it's not the case, be it because the BM size request is low enough or the 
  --             AXI data bus width is greater than 64 bits (4kB access is achievable in a single burst), the interface will generate 
  --             another burst to the next subordinate (next 4kB address space) with the leftover size requested. On both cases, this means 
  --             to return to the COMPUTE2 state with a different bm_size and bm_addr. 
  --             Once there's not leftover data to be transfered, it returns to the idle state, accepting new requests.
  --          RD:Reads the AXI data bus and register it onto rd.axi_data_buffer if the AXI network reports valid data.
  --             After each beat (AXI data bus read), a FIFO register loads the data buffer, while also setting the rd.fifo_full flag 
  --             for that particular register and increments the FIFO index, so the next FIFO register loads another beat from the buffer.
  --             In the case where all FIFO registers are full, the interface stops the read on the AXI data bus by deasserting rd.axi_ready.
  --          WR:Set the strobe bits for WRITE transaction in accordance with the bytes that must be written on this beat.
  --             Load into wr.axi_data_buffer the data stored on the FIFO register, while asserting the wr.axi_valid_data so a beat is 
  --             transfered to the AXI network on the next clock cycle.
  --             For each transfer between the FIFO register and the wr.axi_data_buffer, the counters of the number of bytes left of the 
  --             batch and the number of beats of the burst are decremented appropiatedly.
  -- Outside of 
  -- the FSM ->
  --         RD:A FIFO memory is used to store each beat transfered from the AXI read transaction, no matter from which subordinate.
  --            The BM transfer logic checks out if any more data is available from the RD FIFO to BM transfer, sending the data and 
  --            BM control signals bm_valid and bm_done. This last indicating the completion of the whole transaction at the last BM transfer.
  --            The BM transfer logic rearrenges the data to only transfer the data requested, even if the request is unaligned with 
  --            the AXI data bus width. This also includes applying a byte mask on the last BM transfer.
  --            Each BM transfer is done between the FIFO and the rd.bm_data_buffer (which in between is the byte mask), so the BM 
  --            component connection has slack for a high fMAX synthesis. In addition, the rd.bm_valid flag is high only when this 
  --            buffer contains valid data to be read by the BM component.
  --         WR:The wr.bm_data_buffer is used to read from the BM component the data to transfer in the write transaction.
  --            This is rearrenged and loaded into the FIFO register depending on the address it has been requested and how filled it is. 
  --            In case where the FIFO is full, the interface will deassert the wr.bm_ready so the bm_out.data_full is high.
  --            If there's not enough space on the present FIFO register, it will continue the write on the next. Due to this, it is 
  --            more efficient to have at least 4 registers on the WR FIFO, where on the RD FIFO it doesn't matter if it's 2 registers.
  --            
  --

  type rd_data_buffer is array (natural range <>) of std_logic_vector( DATA_WIDTH - 1 downto 0 );
  type transf_state is (idle, compute1, compute2, compute3, handshake, transfer);

  type transfer_rd_operation is record
    state         : transf_state; -- State of the read transaction.
    bm_grant      : std_logic;    -- Grant signals to BM component.
    bm_error      : std_logic;    -- Error flag register due to SLVERR or DECERR subordinate response.

    axi_mode      : std_logic_vector(1 downto 0); -- AXI output parameter: burst mode (FIXED, INC, WRAP).
    axi_cache     : std_logic_vector(3 downto 0); -- AXI output parameter: cache mode.
    axi_prot      : std_logic_vector(2 downto 0); -- AXI output parameter: privilage level access.
    axi_size      : std_logic_vector(2 downto 0); -- AXI output parameter: size mode of each beat in the burst.
    axi_len       : std_logic_vector(7 downto 0); -- AXI output parameter: number of beats in the burst.
    axi_addr      : std_logic_vector( ADDR_WIDTH     - 1 downto 0); -- AXI output parameter: Starting pointer of the AXI burst.
    axi_valid     : std_logic;                    -- AXI output parameter: valid flag for output control signals (addr, len, size, mode).
    axi_ready     : std_logic;                    -- AXI output parameter: ready flag for input read data.
    axi_last      : std_logic;                    -- AXI input  parameter: last transaction flag of the burst.

    first_beat    : std_logic; -- Flag asserted when computing first beat of the first burst.
    two_subor     : std_logic; -- Flag asserted when two bursts are required to complete the transaction due to surpassing the 4KB boundary.

    bm_size       : std_logic_vector(sel(12, 11, Mult_bursts_subor) downto 0);-- Original size being requested by BM component, and then first burst size.
    rem_size      : std_logic_vector(11 downto 0);-- Remaining size for second burst (4KB outbounds access)
    bm_addr       : std_logic_vector( ADDR_WIDTH     - 1 downto 0); -- Starting pointer requested by BM component.

    fifo          : rd_data_buffer( rd_n_fifo_regs   - 1 downto 0); -- AXI data bus registers which filters narrow reads from data_tmp.
    fifo_full     : std_logic_vector( rd_n_fifo_regs - 1 downto 0); -- When asserted, the fifo(axi_index) is full or at a BM transfer.
    fifo_last     : std_logic_vector( rd_n_fifo_regs - 1 downto 0); -- When asserted, that the fifo(axi_index) is the last BM transfer.
    axi_index     : std_logic_vector(log_2(rd_n_fifo_regs) - 1 downto 0); -- Unsigned index of FIFO registers for the AXI side.
    bm_index      : std_logic_vector(log_2(rd_n_fifo_regs) - 1 downto 0); -- Unsigned index of FIFO registers for the BM side.

    bm_first      : std_logic;                                      -- Flag asserted until the first BM transfer has been done.
    bm_counter    : std_logic_vector( AXI4_FDATA_BYTE    downto 0); -- Used to count the number of bytes to BM transfer.
    start_shift   : std_logic_vector( AXI4_FDATA_BYTE    downto 0); -- Number of right-bytes to skip when reading or writing data_bus at start of BM transfer.
    end_strb      : std_logic_vector( AXI4_FDATA_BYTE    downto 0); -- Number of bytes to read from data_bus at the last BM transfer.
    bm_unsg_mask  : std_logic_vector( log_2(dbits/8) - 1 downto 0); -- Unsigned number to generate the last BM transfer mask.
    bm_mask       : std_logic_vector( dbits/8        - 1 downto 0); -- What dbits byte lanes to use at BM transfer (all, except in the last transfer).
    bm_valid      : std_logic;                                      -- Valid read data to transfer into BM bus from data_bus(dbits-1 downto 0).
    bm_done       : std_logic;                                      -- Asserted at the last rd.bm_valid pulse of the whole transfer.

    axi_data_buffer   : std_logic_vector( DATA_WIDTH - 1 downto 0); -- AXI data bus register used to separate AXI network from interface.
    axi_valid_buffer  : std_logic;                                  -- AXI subordinate r_valid buffer used as delayed signal.
    axi_ready_buffer  : std_logic;                                  -- Manager r_ready buffer used as delayed signal.
    axi_index_buffer  : std_logic_vector(log_2(rd_n_fifo_regs) - 1 downto 0); -- AXI fifo index buffer.
    fifo_full_buffer  : std_logic_vector(rd_n_fifo_regs - 1 downto 0);        -- fifo_full buffered register.
    bm_data           : std_logic_vector( dbits      - 1 downto 0); -- BM data register used to separate shifting from the bm_data_buffer.
    bm_data_buffer    : std_logic_vector( dbits      - 1 downto 0); -- BM data output buffer to maximize frequency of operation.
    bm_valid_buffer   : std_logic;                                  -- BM valid signal output delayed to be in sync with bm_data_buffer.
    bm_done_buffer    : std_logic;                                  -- BM done signal output delayed to be in sync with bm_data_buffer.
  end record;

  type transfer_wr_operation is record
    state         : transf_state; -- State of the read transaction.
    bm_grant      : std_logic;    -- Grant signals to BM component.
    bm_error      : std_logic;    -- Error flag register due to SLVERR or DECERR subordinate response.

    axi_mode      : std_logic_vector(1 downto 0); -- AXI output parameter: burst mode (FIXED, INC, WRAP).
    axi_cache     : std_logic_vector(3 downto 0); -- AXI output parameter: cache mode.
    axi_prot      : std_logic_vector(2 downto 0); -- AXI output parameter: privilage level access.
    axi_size      : std_logic_vector(2 downto 0); -- AXI output parameter: size mode of each beat in the burst.
    axi_len       : std_logic_vector(7 downto 0); -- AXI output parameter: number of beats in the burst. Also, the beats left to send to axi_data_buffer.
    axi_addr      : std_logic_vector( ADDR_WIDTH     - 1 downto 0); -- AXI output parameter: Starting pointer of the AXI burst.
    axi_valid     : std_logic;                    -- AXI output parameter: valid flag for output control signals (addr, len, size, mode).
    axi_valid_data: std_logic;                    -- AXI output parameter: valid write flag for output data.
    axi_last      : std_logic;                    -- AXI output parameter: last transaction flag of the burst.
    axi_strobe    : std_logic_vector( DATA_WIDTH/8   - 1 downto 0); -- AXI output parameter: What AXI data lanes to read from during AXI transfer.
    axi_data_buffer   : std_logic_vector( DATA_WIDTH - 1 downto 0); -- AXI data bus register used to separate AXI network from interface.
    axi_first     : std_logic;                    -- First AXI beat of the burst flag used to compute the write strobe for that beat.

    two_subor     : std_logic; -- Flag asserted when two bursts are required to complete the transaction due to surpassing the 4KB boundary.

    bm_size       : std_logic_vector(sel(12, 11, Mult_bursts_subor) downto 0);-- Original size being requested by BM component, and then first burst size.
    rem_size      : std_logic_vector(11 downto 0);-- Remaining size for second burst (4KB outbounds access)
    bm_addr       : std_logic_vector( ADDR_WIDTH     - 1 downto 0); -- Starting pointer requested by BM component.

    fifo          : rd_data_buffer(   wr_n_fifo_regs - 1 downto 0); -- AXI data bus registers which filters narrow reads from data_tmp.
    fifo_full     : std_logic_vector( wr_n_fifo_regs - 1 downto 0); -- When asserted, the fifo(axi_index) is full or at a BM transfer.
    axi_index     : std_logic_vector(log_2(wr_n_fifo_regs) - 1 downto 0); -- Unsigned index of FIFO registers for the AXI side.
    bm_index      : std_logic_vector(log_2(wr_n_fifo_regs) - 1 downto 0); -- Unsigned index of FIFO registers for the BM side.

    bm_ready      : std_logic;                                      -- Buffer flag to signal data read on the BM bus.
    bm_done       : std_logic;                                      -- Buffer flag to signal end of the requested transaction on the BM bus.
    bm_counter    : std_logic_vector( AXI4_FDATA_BYTE    downto 0); -- Used to count how many bytes have been filled on the actual FIFO register.

    bm_data_buffer : std_logic_vector( dbits         - 1 downto 0); -- BM data bus buffer, used to separate it from the BM component.
    bm_ready_buffer: std_logic;                                     -- Buffer register of bm_ready. Used to exerce delayed logic on bm_data_buffer.
  end record;

  constant RST_TRANSF_RD_OP : transfer_rd_operation := (
    state             => idle,
    bm_grant          => '1',
    bm_error          => '0',
    axi_mode          => "01",
    axi_cache         => (others => '0'),
    axi_prot          => (others => '0'),
    axi_size          => (others => '0'),
    axi_len           => (others => '0'),
    axi_addr          => (others => '0'),
    axi_valid         => '0',
    axi_ready         => '0',
    axi_last          => '0',
    first_beat        => '1',
    two_subor         => '0',
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
    start_shift       => (others => '0'),
    end_strb          => (others => '0'),
    bm_unsg_mask      => (others => '0'),
    bm_mask           => (others => '1'),
    bm_valid          => '0',
    bm_done           => '0',
    axi_data_buffer   => (others => '0'),
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
    axi_mode          => "01",
    axi_cache         => (others => '0'),
    axi_prot          => (others => '0'),
    axi_size          => (others => '0'),
    axi_len           => (others => '0'),
    axi_addr          => (others => '0'),
    axi_valid         => '0',
    axi_valid_data    => '0',
    axi_last          => '0',
    axi_strobe        => (others => '0'),
    axi_data_buffer   => (others => '0'),
    axi_first         => '1',
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
  -- is fixed (it's always DATA_WIDTH), this logic is also fixed.
  --
  -- In case of implementations where DATA_WIDTH < 128 bits, BM transactions with a size request higher than 
  -- 256*DATA_WIDTH/8 will require multiple bursts, since the maximum number of beats allowed is 256 (encoded as 0xFF).
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
  axi4mo.aw_id        <= (axi4mo.aw_id'high   downto ID_W_WIDTH => '0') & std_logic_vector( to_unsigned(axi_id, ID_W_WIDTH) );
  axi4mo.aw_addr      <= (axi4mo.aw_addr'high downto ADDR_WIDTH => '0') & wr.axi_addr(ADDR_WIDTH-1 downto AXI4_DATA_BYTE) & (AXI4_DATA_BYTE-1 downto 0 => '0'); -- Address
  axi4mo.aw_region    <= (others => '0');
  axi4mo.aw_len       <= wr.axi_len;  -- Number of beats
  axi4mo.aw_size      <= wr.axi_size; -- Beat size
  axi4mo.aw_burst     <= wr.axi_mode; -- Burst mode
  axi4mo.aw_lock      <= '0';
  axi4mo.aw_cache     <= wr.axi_cache;
  axi4mo.aw_prot      <= wr.axi_prot;
  axi4mo.aw_qos       <= (others => '0');
  axi4mo.aw_valid     <= wr.axi_valid;
    -- Write data channel out
  axi4mo.w_data       <= (axi4mo.w_data'high downto DATA_WIDTH   => '0') & wr.axi_data_buffer;
  axi4mo.w_strb       <= (axi4mo.w_strb'high downto DATA_WIDTH/8 => '0') & wr.axi_strobe;
  axi4mo.w_last       <= wr.axi_last;
  axi4mo.w_valid      <= wr.axi_valid_data;
    -- Write response channel out
  axi4mo.b_ready      <= '1';
    -- Read address channel out
  axi4mo.ar_id        <= (axi4mo.ar_id'high   downto ID_R_WIDTH => '0') & std_logic_vector(to_unsigned(axi_id, ID_R_WIDTH));
  axi4mo.ar_addr      <= (axi4mo.ar_addr'high downto ADDR_WIDTH => '0') & rd.axi_addr(ADDR_WIDTH-1 downto AXI4_DATA_BYTE) & (AXI4_DATA_BYTE-1 downto 0 => '0'); -- Starting address
  axi4mo.ar_region    <= (others => '0');
  axi4mo.ar_len       <= rd.axi_len;  -- Number of beats
  axi4mo.ar_size      <= rd.axi_size; -- Beat size
  axi4mo.ar_burst     <= rd.axi_mode; -- Burst mode
  axi4mo.ar_lock      <= '0';
  axi4mo.ar_cache     <= rd.axi_cache;
  axi4mo.ar_prot      <= rd.axi_prot;
  axi4mo.ar_qos       <= (others => '0');
  axi4mo.ar_valid     <= rd.axi_valid;
    -- Read data channel out
  axi4mo.r_ready      <= rd.axi_ready;
    -- Write address channel in
  --axi4mi.aw_ready;-- used as input
  --  -- Write data channel in
  --axi4mi.w_ready; -- used as input
  --  -- Write response channel in
  --axi4mi.b_id;    -- used as input
  --axi4mi.b_resp;  -- used as input
  --axi4mi.b_valid; -- used as input
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
  --bm_in.wr_data;  -- used as input
  bm_out.wr_req_grant <= wr.bm_grant;
  bm_out.wr_full      <= not(wr.bm_ready);
  bm_out.wr_done      <= wr.bm_done;
  bm_out.wr_err       <= wr.bm_error;

  -- Read channel
  --bm_in.rd_addr;  -- used as input
  --bm_in.rd_size;  -- used as input
  --bm_in.rd_req;   -- used as input
  bm_out.rd_req_grant <= rd.bm_grant;
  bm_out.rd_data      <= (1023-dbits downto 0 => '0') & rd.bm_data_buffer;
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
    variable rd_addr_end  : std_logic_vector( 12                     downto 0 );-- max end address LSB at INC mode (4kB check)
    variable rd_axi_len   : std_logic_vector( rd.axi_len'range                );-- AXI burst length
    variable rd_axi_next_index    : std_logic_vector(rd.axi_index'range       );-- Next buffer index

    variable rd_data_fwidth: std_logic_vector(DATA_WIDTH + dbits - 1 downto 0 );-- BM full shifted data bus
    variable rd_bm_counter        : std_logic_vector(rd.bm_counter'range      );-- Next BM counter
    variable rd_fifo_offset       : std_logic_vector(rd.bm_counter'range      );-- A fifo offset to simplify
    variable rd_bm_shift          : std_logic                                  ;-- BM shift flag
    variable rd_data_empty        : boolean                                    ;-- rd.fifo(bm_index) register depleted flag
    variable rd_bm_next_index     : std_logic_vector(rd.bm_index'range        );-- Next AXI counter
    variable rd_bm_done           : std_logic                                  ;-- Last BM transfer, generate BM mask.

    -- Variables that are only used on implementations where DATA_WIDTH < 128 bits.
    variable rd_next_bm_size      : std_logic_vector(rd.bm_size'range             );-- Num of bytes left to transfer on the next clk cycle of this burst.
    
  begin
    if (rstn = '0' and ASYNC_RST) then
       -- Default variable values
      rd                    <= RST_TRANSF_RD_OP;
      rd_data_fwidth        := (others => '0');
      rd_bm_counter         := (others => '0');
      rd_bm_shift           := '0';
      rd_bm_done            := '0';
    elsif rising_edge(clk) then
      if (rstn = '0') then
        rd                  <= RST_TRANSF_RD_OP;
        rd_data_fwidth      := (others => '0');
        rd_bm_counter       := (others => '0');
        rd_bm_shift         := '0';
        rd_bm_done          := '0';
      else
        rd_data_fwidth      := (others => '0');
        rd_bm_counter       := (others => '0');
        rd_bm_shift         := '0';
        rd_bm_done          := '0';
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
              rd.bm_addr    <= bm_in.rd_addr(rd.bm_addr'range); -- Load starting address request
              rd.bm_size    <= std_logic_vector(resize(unsigned(bm_in.rd_size), rd.bm_size'length)); -- Load BM size to transfer (-1 from real size)
              rd.axi_mode   <= '0' & not(bm_in.rd_fixed_addr);
              rd.axi_cache  <= bm_in.rd_axi_cache;
              rd.axi_prot   <= bm_in.rd_axi_prot;

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
            -- Set AXI size mode to DATA_WIDTH and align the starting address with the DATA_WIDTH slot.
            rd.axi_size     <= std_logic_vector(to_unsigned(AXI4_DATA_BYTE, rd.axi_size'length));
            rd.axi_addr     <= rd.bm_addr(rd.bm_addr'high downto AXI4_DATA_BYTE) & (AXI4_DATA_BYTE - 1 downto 0 => '0');

            -- Compute how many beats will be necessary to transfer the requested data in this burst.
            decide_len(rd.bm_size(11 downto 0), rd.bm_addr, rd_axi_len);
            rd.axi_len      <= rd_axi_len;
          
            -- Save the number of bytes to discard unrequested data at the first BM transfer = LSB BM unaligned address + BM data bus width bytes
            if(rd.first_beat = '1') then
              rd.start_shift <= ((rd.start_shift'length - AXI4_DATA_BYTE - 1) downto 0 => '0') & rd.bm_addr(AXI4_DATA_BYTE - 1 downto 0);
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
          -- The AXI transfer occurs by buffering the read data onto the rd.axi_data_buffer register when the subordinate asserts the valid data flag.
          -- Then, the rd.axi_data_buffer is loaded onto a FIFO register, which will be used at the BM transfer. However, due to this single clock 
          -- cycle delay between AXI read and data load, both valid and the mask signals are also buffered on the "buffer" registers (these
          -- are outside of the state machine and updated with each clock pulse). 

              -- Check if subordinate is delivering valid data with the same AXI ID as requested and if manager is listening.
            if (axi4mi.r_valid = '1' and axi4mi.r_id(ID_R_WIDTH-1 downto 0) = std_logic_vector(to_unsigned(axi_id, ID_R_WIDTH)) and rd.axi_ready = '1') then

              -- Register inputs to separate AXI network from further computation.
              rd.axi_data_buffer  <= axi4mi.r_data(rd.axi_data_buffer'range);
              rd.axi_last         <= axi4mi.r_last;
              rd.bm_error         <= axi4mi.r_resp(1);

              -- Compute next FIFO index.
              rd_axi_next_index := add_vector(rd.axi_index, 1, rd.axi_index'length);
              rd.axi_index      <= rd_axi_next_index;

              -- Deassert the ready flag to finish AXI burst if it is the last beat or if there is not enough space in the FIFO at the moment.
              rd.axi_ready    <= not( axi4mi.r_last or rd.fifo_full(to_integer(unsigned(rd_axi_next_index))) );
              -- Signal the present rd.FIFO register as prepared for BM transfer, for whenever it is due to buffers.
              rd.fifo_full(to_integer(unsigned(rd.axi_index))) <= '1';

              -- The last AXI transfer of the whole transaction will set the proper rd.fifo_last bit, so the BM transfer logic knows when to end.
              -- However, there's a distinction between DATA_WIDTH <= 64 bits and higher data widths. If there's still data to transfer 
              -- because this burst has not been enough, do not assert the last BM transfer on the FIFO register and update rd.bm_size.
              if(Mult_bursts_subor) then
                rd_next_bm_size := sub_vector(rd.bm_size, DATA_WIDTH/8, rd_next_bm_size'length);
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

                -- However, there's a distinction between implementations with an DATA_WIDTH <= 64 bits and higher data widths. 
                -- If there's still data to transfer because this burst has not been enough, generate another burst to the same subordinate.
                if(rd.bm_size(rd.bm_size'high) = '0' and Mult_bursts_subor) then
                  -- The address bit range comes from that in AXI4 the max length is 256 (255 encoded), thus, +8 bit positions.
                  rd.bm_addr    <= add_vector(rd.bm_addr(rd.bm_addr'high downto 8 + AXI4_DATA_BYTE), 1, rd.bm_addr'high - 7 - AXI4_DATA_BYTE) 
                                    & (7 + AXI4_DATA_BYTE downto 0 => '0');
                  rd.state      <= compute2;
                else
                  if(rd.two_subor = '1') then
                    rd.two_subor  <= '0';
                    rd.axi_last   <= '0';
                    rd.bm_size    <= std_logic_vector(resize(unsigned(rd.rem_size), rd.bm_size'length));
                    rd.bm_addr    <= add_vector(rd.bm_addr(rd.bm_addr'high downto 12), 1, rd.bm_addr'high - 11) & (11 downto 0 => '0');
                    rd.state      <= compute2;
                  end if;
                end if;

              -- In case it is not the last AXI beat, check if the present FIFO register can be used for another read beat.
              else
                if( rd.fifo_full(to_integer(unsigned(rd.axi_index))) = '0' ) then
                  rd.axi_ready    <= '1'; -- If there's empty registers in the buffer, use them to read another beat.
                end if;
              end if;

            end if;


          when others =>

        end case;


      -------------
      -- RD FIFO --
      -------------
        -- After each AXI data read, put the data to the next available FIFO register.
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

            -- The initial counter is set by rd.end_strb if there's only one FIFO register to transfer. 
            if(rd.fifo_last(to_integer(unsigned(rd.bm_index))) = '1') then
              rd_bm_counter := rd.end_strb;
            else -- Otherwise, the initial counter is the AXI data bus bytes.
              rd_bm_counter := std_logic_vector(to_unsigned(DATA_WIDTH/8, rd.bm_counter'length));
            end if;

            -- Decrement the shift to the initial counter and use it to initilize rd.bm_counter.
            rd.bm_counter   <= sub_vector(rd_bm_counter, rd.start_shift, rd.bm_counter'length);
            -- Shift the content of the FIFO register so the requested data starts on dbits position of rd_data_fwidth.
            rd_data_fwidth  := std_logic_vector(shift_right( unsigned(std_logic_vector'(rd.fifo(to_integer(unsigned(rd.bm_index))) 
                                                    & (dbits - 1 downto 0 => '0'))), 8*to_integer(unsigned(rd.start_shift)) ));
            rd_bm_shift     := '1';

            -- Deassert the first BM transfer flag, since it's only required to shift rd.start_shift before than the first BM transfer.
            rd.bm_first     <= '0';

          else -- rd.bm_first = '0' check

            -- START BM TRANSFER --
            if(rd_data_empty) then
              if(rd.fifo_full_buffer(to_integer(unsigned(rd_bm_next_index))) = '1') then

              -- In case this cycle will deplete the present FIFO register (rd_data_empty = 1) and the next FIFO register is prepared 
              -- to be transfered, use the remaining data (rd.bm_counter lower or equal to AXI data bus bytes) as LSB and take the 
              -- lacking bytes to fill dbits from the next FIFO register, while also shifting its content for posterior transfers.
              -- (since the shifting index k surpasses the bytes available in the next FIFO register on unaligned starting transfers, 
              -- fill the remaining byte positions with zeros, that, in number of bytes, will be always lower than the bytes in dbits)
                for k in DATA_WIDTH/8 + dbits/8 - 1 downto 0 loop
                  if( rd.bm_counter > std_logic_vector(to_unsigned(k, rd.bm_counter'length)) and DATA_WIDTH/8 - 1 > k) then
                    rd_data_fwidth(8*k+7 downto 8*k) := rd.fifo(to_integer(unsigned(rd.bm_index)))(8*k+7 downto 8*k);
                  else
                    rd_fifo_offset := sub_vector(k, rd.bm_counter, rd.bm_counter'length);
                    if( to_integer(unsigned(rd_fifo_offset)) < DATA_WIDTH/8 ) then
                      rd_data_fwidth(8*k+7 downto 8*k) := rd.fifo(to_integer(unsigned(rd_bm_next_index)))
                                                        (       8*to_integer(unsigned(rd_fifo_offset))+7 
                                                        downto  8*to_integer(unsigned(rd_fifo_offset)) );
                    else
                      rd_data_fwidth(8*k+7 downto 8*k) := x"00";
                    end if;
                  end if;
                end loop;

                -- Set the present FIFO register to be prepared for new AXI transfer and change the FIFO register rd.bm_index.
                rd.fifo_full(to_integer(unsigned(rd.bm_index))) <= '0';
                rd.bm_index       <= rd_bm_next_index;
                rd_bm_shift       := '1';

                -- Compute the number of bytes that'll be left to read on the next FIFO register.
                -- In case where the next FIFO register is the last, counter = end_strb - ( dbits/8 - bm_counter ) = ( end_strb + bm_counter ) - dbits/8
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
                  rd.bm_counter   <= add_vector(rd.bm_counter, DATA_WIDTH/8 - dbits/8, rd.bm_counter'length);
                end if;              

              else   -- rd.fifo_full(rd_bm_next_index) = 0 check

              -- In case there's not enough data to read from the FIFO, check if this is the last FIFO register to BM transfer. If it 
              -- is, check how many bytes of remaining data are left. If it's lower than dbits/8, this is the last BM transfer. However, 
              -- if it's exactly dbits/8 and it's the last FIFO register, delay the end of the BM transfer for another clock cycle 
              -- (do not increase the rd.bm_counter, let the execution return to this step to execute the end of the BM transfer).
                if(rd.fifo_last(to_integer(unsigned(rd.bm_index))) = '1') then 
                  rd.bm_counter   <= sub_vector(rd.bm_counter, dbits/8, rd.bm_counter'length);
                  rd_data_fwidth  := (dbits - 1 downto 0 => '0') & rd.fifo(to_integer(unsigned(rd.bm_index)));
                  rd_bm_shift     := '1';
                  rd_bm_done      := '1';
                  rd.bm_done      <= '1';
                  rd.state        <= idle;
                else
              -- Otherwise, if there's not enough data and this is not the last FIFO register, wait for the next AXI transfer.
                  rd_bm_shift     := '0';
                end if;

              end if; -- rd.fifo_full(rd_bm_next_index) end of check
            else  -- rd_empty_data = 0 check

            -- In case there's still data on the present FIFO register for a dbits transfer (more than dbits), shift dbits.
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

        -- Reset buffer signals at last BM transfer.
        if(rd.bm_done_buffer = '1') then
          rd.axi_valid_buffer <= '0';
          rd.axi_ready_buffer <= '0';
          rd.axi_index_buffer <= (others => '0');
          rd.fifo_full_buffer <= (others => '0');
          rd.bm_valid_buffer  <= '0';
          rd.bm_done_buffer   <= '0';
        else
        -- Registration of buffer signals for read transactions.
          rd.axi_valid_buffer <= axi4mi.r_valid;  -- Delayed to read the previous AXI beat transfer from axi_data_buffer onto the FIFO.
          rd.axi_ready_buffer <= rd.axi_ready;    -- Delayed to read the previous AXI beat transfer from axi_data_buffer onto the FIFO.
          rd.axi_index_buffer <= rd.axi_index;    -- Delayed to read the previous AXI beat transfer from axi_data_buffer onto the FIFO.
          rd.fifo_full_buffer <= rd.fifo_full;    -- Delayed full vector so BM transfers are executed a cycle after the AXI transfer.
          rd.bm_valid_buffer  <= rd.bm_valid;     -- Delayed BM valid data flag so the mask and output is separated from BM transfer logic.
          rd.bm_done_buffer   <= rd.bm_done;      -- Delayed BM done flag to signal at the last BM transfer.
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
    if (rstn = '0' and ASYNC_RST) then
      wr                    <= RST_TRANSF_WR_OP;
      wr_addr_end           := (others => '0');
      wr_axi_len            := (others => '0');
      wr_axi_last           := '0';
      wr_axi_next_index     := (others => '0');
      wr_axi_read           := FALSE;
      wr_bm_next_counter    := (others => '0');
      wr_fifo_offset        := (others => '0');
      wr_bm_next_index      := (others => '0');
    elsif rising_edge(clk) then
      if (rstn = '0') then
        wr                  <= RST_TRANSF_WR_OP;
        wr_addr_end         := (others => '0');
        wr_axi_len          := (others => '0');
        wr_axi_last         := '0';
        wr_axi_next_index   := (others => '0');
        wr_axi_read         := FALSE;
        wr_bm_next_counter  := (others => '0');
        wr_fifo_offset      := (others => '0');
        wr_bm_next_index    := (others => '0');
      else
        wr_addr_end         := (others => '0');
        wr_axi_len          := (others => '0');
        wr_axi_last         := '0';
        wr_axi_next_index   := (others => '0');
        wr_axi_read         := FALSE;
        wr_bm_next_counter  := (others => '0');
        wr_fifo_offset      := (others => '0');
        wr_bm_next_index    := (others => '0');
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
              wr.bm_addr    <= bm_in.wr_addr(wr.bm_addr'range); -- Load starting address request
              wr.bm_size    <= std_logic_vector(resize(unsigned(bm_in.wr_size), wr.bm_size'length));-- Load BM size to transfer (-1 from real size)
              wr.axi_mode   <= '0' & not(bm_in.wr_fixed_addr);
              wr.axi_cache  <= bm_in.wr_axi_cache;
              wr.axi_prot   <= bm_in.wr_axi_prot;

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
            -- Set AXI size mode to DATA_WIDTH and align the starting address with the DATA_WIDTH slot.
            wr.axi_size     <= std_logic_vector(to_unsigned(AXI4_DATA_BYTE, wr.axi_size'length));
            wr.axi_addr     <= wr.bm_addr(wr.bm_addr'high downto AXI4_DATA_BYTE) & (AXI4_DATA_BYTE - 1 downto 0 => '0');

            -- Compute how many beats will be necessary to transfer the requested data in this burst.
            decide_len(wr.bm_size(11 downto 0), wr.bm_addr, wr_axi_len);
            wr.axi_len      <= wr_axi_len;

            -- Add to the BM size transfer the starting position of the DATA_WIDTH slot, so it can be used for high cap strobe.
            wr.bm_size      <= add_vector(wr.bm_size, wr.bm_addr(AXI4_DATA_BYTE - 1 downto 0), wr.bm_size'length);

            -- Next, the handshake step.
            wr.axi_valid    <= '1'; -- Request AXI write burst.
            wr.state        <= handshake;
          
          
          when handshake =>
            -- Having all computation steps and proper registration separated allows maximum 
            -- frequency of operation when including this AXI manager interface at the interconnect
            -- bus. For a write burst, AW AXI control buses are used to send the burst control data.
            if (axi4mi.aw_ready = '1') then
              wr.axi_valid  <= '0'; -- At request being granted, deassert AXI handshake request.
              wr.axi_first  <= '1'; -- Reset calculation of the first AXI strobe flag for multiple bursts.
            
              -- Next, beat transfers
              wr.state      <= transfer;
            end if;
          
          
          when transfer => -- AXI transfer --
          -- The AXI transfer sets the AXI valid write flag anytime there's data available in a buffer register that reads from the FIFO, 
          -- freeing the FIFO register for BM fransfers. In addition, the size left to transfer wr.bm_size is decreased with the number of 
          -- bytes transfered.

            -- Load the variables for last AXI beat in the burst, next FIFO index at the AXI side and the read flag.
            wr_axi_last         := to_std_logic(wr.axi_len = (wr.axi_len'range => '0'));
            wr_axi_next_index   := add_vector(wr.axi_index, 1, wr.axi_index'length);
            wr_axi_read         := wr.axi_valid_data = '1' and axi4mi.w_ready = '1';
            wr.axi_first        <= '0';

            -- Set the AXI strobe for the next AXI write beat. The wr.bm_addr LSB is used to write zeros on the LSB of the strobe for the 
            -- first beat. To take this effect only in the first beat of the burst to transfer, the wr.axi_first flag is used. 
            -- The bytes left to AXI transfer wr.bm_size is used to set the upper limit of the strobe, writing zeros on the MSB of the strobe. 
            -- However, to stop it from updating the strobe after wr.axi_last has been asserted, an IF function has been added with wr_axi_read.
            if( (wr.axi_last = '0' and wr_axi_read) or wr.axi_first = '1') then
              for k in wr.axi_strobe'range loop
                if( (k < to_integer(unsigned(wr.bm_addr(AXI4_DATA_BYTE-1 downto 0))) and wr.axi_first = '1')
                      or k > to_integer(unsigned(wr.bm_size)) ) then
                  wr.axi_strobe(k)  <= '0';
                else
                  wr.axi_strobe(k)  <= '1';
                end if;
              end loop;
            end if;

            -- On the first beat read, zero the wr.bm_addr LSB used to set up the lower bound of the strobe for the first beat.
            if(wr_axi_read) then
              wr.bm_addr(AXI4_DATA_BYTE-1 downto 0) <= (AXI4_DATA_BYTE-1 downto 0 => '0');
            end if;

            -- When the interface is not sending valid data on the buffer to read from or it is and the AXI interconnect has read it, read from 
            -- the next available register in the FIFO onto the data buffer, setting also the related signals and update both beats left (length), 
            -- the FIFO index and the number of bytes left on to transfer on this subordinate. However, if the data buffer is the last beat, do 
            -- not follow with another AXI beat. 
            if( (wr_axi_read or wr.axi_valid_data = '0') and wr.axi_last = '0' ) then
              if(wr.fifo_full(to_integer(unsigned(wr.axi_index))) = '1') then
                wr.axi_valid_data   <= '1';
                wr.axi_last         <= wr_axi_last;
                wr.axi_len          <= sub_vector(wr.axi_len, 1, wr.axi_len'length);
                wr.axi_index        <= wr_axi_next_index;
                wr.bm_size          <= sub_vector(wr.bm_size, DATA_WIDTH/8, wr.bm_size'length);
                wr.fifo_full(to_integer(unsigned(wr.axi_index)))  <= '0';
                wr.axi_data_buffer<= wr.fifo(to_integer(unsigned(wr.axi_index)));
              else
                wr.axi_valid_data   <= '0';
              end if;
            end if;

            -- At burst end, deassert the AXI write valid and last beat flags when subordinate is listening to the last beat.
            if(wr_axi_read and wr.axi_last = '1') then
              wr.axi_valid_data   <= '0';
              wr.axi_last         <= '0';

              -- Then, if the implementation requires to check if multiple bursts to the same subordinate are required to allow 
              -- 4kB access, check if all data of this present batch has been sent (using bm_size'high as indicator). If all data 
              -- for this subordinate has been sent, check if access to another subordinate is required (wr.two_subor). If not, return to idle.
              if(wr.bm_size(wr.bm_size'high) = '0' and Mult_bursts_subor) then
                wr.bm_addr      <= add_vector(wr.bm_addr(wr.bm_addr'high downto 8 + AXI4_DATA_BYTE), 1, wr.bm_addr'high - 7 - AXI4_DATA_BYTE) 
                                      & (7 + AXI4_DATA_BYTE downto 0 => '0');
                wr.state        <= compute2;
              else
                if(wr.two_subor = '1') then
                  wr.two_subor  <= '0';
                  wr.bm_size    <= std_logic_vector(resize(unsigned(wr.rem_size), wr.bm_size'length));
                  wr.bm_addr    <= add_vector(wr.bm_addr(wr.bm_addr'high downto 12), 1, wr.bm_addr'high - 11) & (11 downto 0 => '0');
                  wr.state      <= compute2;
                else
                  wr.bm_done    <= '1';
                  wr.state      <= idle;
                end if;
              end if;

            end if;


          when others =>

        end case;


      ---------------------------------
      -- WR FIFO + BM TRANSFER LOGIC --
      ---------------------------------

      -- Read BM data if the wr.bm_ready flag register is high.
      if(wr.bm_ready = '1') then
        wr.bm_data_buffer   <= bm_in.wr_data(bm_in.wr_data'high downto bm_in.wr_data'high - dbits + 1);
      end if;

      -- The BM transfer logic starts by initilizating the wr.bm_counter with at what position of the FIFO register must load 
      -- wr.bm_data_buffer (the bm_in.wr_data buffer), that is loading it with the LSB of the address requested.
      -- Then, starts accepting data from the BM component to fill the FIFO registers. If a FIFO register will be filled on that 
      -- clock cycle, leftover data will be stored at the LSB of the next FIFO register, which will be used on the next clock 
      -- cycle since the FIFO BM index is increased.
      -- However, the logic will stall new reads (assert the bm_out.wr_full) to the wr.bm_data_buffer if the next FIFO register is 
      -- yet to be sent to the AXI interconnect, even though there may be enough space on the actual FIFO register. Thus, the  
      -- bottleneck can be reduced by implementing 4 FIFO registers instead of the minimum 2 on wr_n_fifo_regs.
      case wr.state is
        when compute1 =>
          wr.bm_counter <= ((wr.bm_counter'length - AXI4_DATA_BYTE - 1) downto 0 => '0') & wr.bm_addr(AXI4_DATA_BYTE - 1 downto 0);
          wr.bm_ready   <= '1';
      
        when compute2 | handshake | transfer =>
          wr_bm_next_index    := add_vector(wr.bm_index, 1, wr.bm_index'length);
          wr_bm_next_counter  := add_vector(wr.bm_counter, dbits/8, wr.bm_counter'length);

          -- Stop reading data from BM if the next FIFO register is full.
          wr.bm_ready <= not(wr.fifo_full(to_integer(unsigned(wr_bm_next_index))));

          -- If data has been read on the previous clock cycle, write it into the FIFO and update wr.bm_counter.
          if(wr.bm_ready_buffer = '1') then

            -- Write every byte from the wr.bm_data_buffer into the present FIFO register. If the present 
            -- register is full, then write the bytes left on the next FIFO register.
            for k in dbits/8 - 1 downto 0 loop
              wr_fifo_offset := add_vector(wr.bm_counter, k, wr_fifo_offset'length);
              
              if( to_integer(unsigned(wr_fifo_offset)) < DATA_WIDTH/8 ) then
                wr.fifo(to_integer(unsigned(wr.bm_index)))
                       (      8*to_integer(unsigned(wr_fifo_offset)) + 7 
                       downto 8*to_integer(unsigned(wr_fifo_offset)) ) <= wr.bm_data_buffer(8*k+7 downto 8*k);
              else
                wr.fifo(to_integer(unsigned(wr_bm_next_index)))
                       (      8*to_integer(unsigned(wr_fifo_offset(log_2(dbits/8)-1 downto 0))) + 7 
								       downto 8*to_integer(unsigned(wr_fifo_offset(log_2(dbits/8)-1 downto 0))) ) <= wr.bm_data_buffer(8*k+7 downto 8*k);
              end if;

            end loop;
            
            -- If the counter surpasess the number of bytes in the AXI data bus width, that means the present FIFO register
            -- has been filled, so change to the next FIFO register and mark the present as full. Also, maintain the number
            -- of bytes that have been written on the next FIFO register on wr.bm_counter.
            if(wr_bm_next_counter >= std_logic_vector(to_unsigned(DATA_WIDTH/8, wr.bm_counter'length))) then
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


      -------------------------
      -- WRITE RESPONSE READ --
      -------------------------

      -- This logic always listen for write responses, not matter if a write transaction is not ongoing.
      if(axi4mi.b_valid = '1' and axi4mi.b_id(ID_W_WIDTH-1 downto 0) = std_logic_vector(to_unsigned(axi_id, ID_W_WIDTH))) then
        wr.bm_error <= axi4mi.b_resp(1);
      end if;


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
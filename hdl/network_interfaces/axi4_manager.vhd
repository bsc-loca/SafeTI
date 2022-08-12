-----------------------------------------------------------------------------   
-- Entity:      axi4_manager
-- File:        axi4_manager.vhd
-- Author:      Francisco Javier Fuentes Diaz (BSC-CNS)
-- Description: AXI4 FULL Manager entity.
------------------------------------------------------------------------------ 
--  Changelog:
--              - v0.8.9  11 July, 2022.
--                The interface has been reworked, in such a way where the logic has been split 
--                between handshake (hs) and burst (burst) state machines, in order to allow granting 
--                transactions on the IB side even when a transaction is ongoing at the AXI side.
--                This means that the interface may now generate requests at the AXI side during a 
--                burst transfer of the previous IB transaction.
--                If the burst is granted, that burst will not initiate until the ongoing one ends.
--                Enabling "injector_mode" allows to simulate the IB signaling without really making 
--                any data transfer between the IB component and the interface, making read data to be 
--                discarded and write data being zeroed. This is the only way to finish the transaction 
--                on the IB side the earliest so it can generate the next request even when on the AXI 
--                side may be ongoing the last requested transaction.
--                Due to some bugs that have been detected, this version is expected to be only used 
--                with the "injector_mode" enabled as TRUE. Future updates will allow the interface to 
--                work as a real interface and transfer data between the IB component and the AXI network.
--
--
------------------------------------------------------------------------------ 

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library safety;
use safety.axi4_pkg.all;

-----------------------------------------------------------------------------
-- AXI4 FULL Manager - bus manager bridge
--
-- Manager interface features:
--
-- - Only INC and FIXED burst modes are available and every burst generated use all AXI data lanes available per beat.
-- - Full support for unaligned address requests. This includes even if two subordinates are accessed in a 
--   single transaction request or if multiple bursts are required on the same subordinate.
-- - Maximum number of bytes per IB transaction request is 4096 bytes (encoded as 0xFFF).
-- - Extensible IB component and AXI data buses width 8, 16, 32, 64, 128, 256, 512 and 1024 bits (read 
--   further considerations on integration of the interface for limitations).
--
--
-- Manager interface and considerations during integration:
--
-- - DATA_WIDTH >= dbits. The AXI data bus width must be greater or equal to the IB data bus.
--
-- - IB size requests <= 4096 (4kB, limited by AXI4 addressing rule). The maximum number of bytes 
--   per IB transaction is 4096 bytes (encoded as 0xFFF).
--
-- - Little endian data structure. Higher bit position links to higher memory positions.
--
-- - Unaligned access by IB requests are supported through aligning the address with the AXI data bus 
--   width, while delivering only the requested data on read transactions and using the write strobe 
--   on AXI write transactions.
--
-- - The Manager only requests bursts with the AXI size code that uses the whole AXI data bus width.
--
-- - This interface requires the signaling of the last AXI beat to be at the correct beat on read 
--   transactions, or unrequested data could be read instead of the requested.
--
-- - The interface may send control and data when the valid flag is not asserted. These signals 
--   must be discarded since the valid flag is low.
--
--
-- This AXI4 Manager interface translates and manage the requests made by the IB component,
-- using the IB input and output buses, to the AXI4 network, using the AXI4 input and output buses.
--
-- The IB component has an opaque vision of what the interface requests to the AXI network, since 
-- it only receives the data that it has been requested. Be it requests with unaligned or even to multiple 
-- subordinates access (only 2 as maximum), the interface filters it to deliver only the requested data.
-- 
-- If the IB request implies access to two different subordinates, the Manager interface will generate two 
-- batches of data that are distribuited in one or multiple bursts each, with the appropiated addressing 
-- and burst length (number of beats in the burst) for each burst). Multiple bursts for the same batch
-- depends on the AXI data bus width, since implemenations with an AXI data bus width lower than 128 bits 
-- require multiple bursts to the same subordinate to be able to access the 4kB of data (4096 addresses) 
-- it can allocate (limit fixed by the AXI4 communication protocol).
-- 
-- 
-----------------------------------------------------------------------------

entity axi4_manager is
  generic (
    -- AXI Manager configuration
    ID_R_WIDTH      : integer range  0 to   32    := 4;     -- AXI ID's bus width.
    ID_W_WIDTH      : integer range  0 to   32    := 4;     -- AXI ID's bus width.
    ADDR_WIDTH      : integer range 12 to   64    := 32;    -- AXI address bus width. (Tested only for 32 bits)
    DATA_WIDTH      : integer range  8 to 1024    := 128;   -- AXI data bus width. [Only power of 2s are allowed]
    axi_id          : integer range  0 to 32**2-1 := 0;     -- AXI manager burst index [Must be < ID_X_WIDTH**2-1]
    --injector_mode   : boolean                     := TRUE;  -- Simulate read IB transfers to save on footprint --TO BE IMPLEMENTED

    -- Interface Bus (IB) configuration
    dbits           : integer range  8 to 1024    := 32;    -- IB data bus width [Only power of 2s are allowed and <= AXI_DATA_WIDTH]

    -- Interface configuration
    rd_n_fifo_regs  : integer range  2 to  256    := 2;     -- Number of FIFO registers to use at AXI read transactions.
    wr_n_fifo_regs  : integer range  2 to  256    := 2;     -- Number of FIFO registers to use at AXI write transactions.
    ASYNC_RST       : boolean                     := TRUE   -- Allow asynchronous reset
  );
  port (
    rstn                      : in  std_ulogic; -- Reset
    clk                       : in  std_ulogic; -- Clock
    -- AXI interface signals
    axi4mi                    : in  axi4_miso;  -- AXI4 manager input 
    axi4mo                    : out axi4_mosi;  -- AXI4 manager output
    -- IB component signals
    ib_in                     : in  ib_mosi;    -- IB interface input
    ib_out                    : out ib_miso     -- IB interface output
  );
end entity axi4_manager;

architecture rtl of axi4_manager is
  -----------------------------------------------------------------------------
  -- Constant declaration
  -----------------------------------------------------------------------------

    -- Parametric constants
    constant DBITS_DATA_BYTE  : integer := log_2(dbits/8);      -- Number of bits required to address the IB data bus bytes.
    constant AXI4_DATA_BYTE   : integer := log_2(DATA_WIDTH/8); -- Number of bits required to address the AXI data bus bytes.
    constant MAX_BURST_BYTE   : integer := log_2(4096);         -- Number of bits required to address the maximum transfer size by the protocol.

    -- The Mult_bursts_subor flag indicates when TRUE that the implementation allows for the possible requirement of multiple bursts to the same 
    -- subordinate (same 4kB memory space) to satisfy the size requested by the IB component (maximum 4096 bytes, encoded as 0xFFF in ib_in.ib_size).
    constant Mult_bursts_subor: boolean := DATA_WIDTH < 128;

  -----------------------------------------------------------------------------
  -- Records and types
  -----------------------------------------------------------------------------
  --
  -- This interface, accepts RD (read) and WR (write) transaction requests from the IB bus, listening to the starting address and the total transfer 
  -- size of the operation. This last must be lower or equal to 4096 and it is encoded as an unsigned integer decremented by one respect the real size. 
  -- So the codification of ib_in.rd_data 0x000 means 1 byte transfer, and the maximum is 0xFFF that is 4096 bytes to transfer.
  --
  -- Once the request has been granted, the interface processes the control data to check if the total transfer surpasses the 4kB address boundary, 
  -- splitting the transaction in two batches in such situations. This is done to arrange the transfer sizes of the batches for the first and 
  -- second AXI subordinates, having each different 4kB address allocation spaces. Note that after a IB transaction request has been granted, the 
  -- grant flag will be deasserted until the first burst of the granted transaction has been granted by the AXI network.
  -- 
  -- If the "injector_mode" is set to TRUE, the interface will start the correct IB signaling of the "rd_valid", "rd_done", "wr_full" and "wr_done"
  -- flags right away as it would on the real transaction, instead of waiting for the data read or write response, since all read data is discarded and 
  -- writing data are set to zeroes. This allows for an earlier termination of the transaction on the IB side while on the AXI is ongoing. However, any 
  -- error that may be found during the transaction may not propagate correctly to the IB component, since this may have finished the transaction by then.
  --
  -- For the burst requests, the interface always sets an AXI size mode to use the whole data bus width of the AXI side (DATA_WIDTH), computes the 
  -- number of beats (burst length) to satisfy the request transfer and aligns the address requested (ib_addr) with the AXI data bus width (DATA_WIDTH).
  -- In case a single burst is not enough to satisfy the transfer size for the subordinate in question, the maximum length will be set (256 beats).
  -- However, this can only happen at implementations that have an DATA_WIDTH < 128 bits. Thus, this is managed with the "Mult_bursts_subor"
  -- flag to only proceed with the correspondent checks to generate multiple bursts to the same subordinate in the implementation if it is the case.
  --
  -- The interface depends on the assertion of the AXI last signal on the last beat of the burst on read transactions in order to stop the transfer. 
  -- Otherwise, the interface may read data from the AXI data bus that should not and may transfered to the IB component instead of the requested data.
  -- 
  --
  -- Each transaction logic has three loops that work in parallel, which are MAIN, BURST and ib_TRANSFER, which has the following stages:
  -- MAIN   -> The MAIN loop manages IB transaction requests, performs the 4kB check and generates the request of the first AXI burst of the transaction.
  --  IDLE      ->  Grants IB transaction request, taking the control data for the generation of the transaction and advancing to the CHECK_4kB stage. 
  --                On WR transactions, it waits until the previous transaction has finished on the IB side to grant a new IB transaction.
  --                On RD transactions this is not necessary, since read data is sent to the IB component at the moment is avaiable on the FIFO buffer.
  --  CHECK_4kB ->  Computes a 4kB address check to ensure that, if the transaction will require to comunicate with two different 4kB boundaries, the
  --                transaction is split between two batches (ib_size and rem_size) so a burst finishes and another is generated on the 4kB address. 
  --                This stage takes one clock cycle and always continues to the PREPARE_HS stage.
  --  PREPARE_HS->  Sets the AXI address to be aligned with the AXI data bus width (DATA_WIDTH bits) and does not request the AXI burst nor continue 
  --                to the HANDSHAKE stage until the BURST loop is at the IDLE stage or at the last AXI burst of the previous transaction.
  --  HANDSHAKE ->  Executes the handshake of the first burst of the last requested IB transaction, sending the corresponding data to the BURST loop 
  --                once this has finished the last AXI burst of the previous transaction completely. 
  --  
  -- BURST  -> The BURST loop manages the data given by the MAIN loop to request the multiple bursts, if necessary, to the AXI network.
  --  IDLE       -> Wait for MAIN loop control data of a new transaction. Once the first AXI burst handshake of the transaction happens, it continues to 
  --                the TRANSFER stage.
  --  TRANSFER   -> Sets the AXI control signals of VALID and READY for WR and RD transactions respectively, including the writing strobe, during the 
  --                data transfer. If the internal FIFO buffer is full, this will put on standby the ongoing burst until empty space is made.
  --                Once the burst is complete, the control data for a new burst is computed to burst to the same (case that only can happen 
  --                when DATA_WIDTH < 128 bits, which is managed with the "Mult_bursts_subor" constant) or to another subordinate if necessary and
  --                continues to the PREPARE_HS stage. If the whole transaction has been finished on the AXI side, it returns to the IDLE stage.
  --                The data is stored on the top of the FIFO on RD transactions, while is sent from the bottom of the FIFO on WR transactions,
  --                unless the "injector_mode" is set to TRUE, which discards read data and zeroes write data.
  --  PREPARE_HS -> Computes the number of beats of the next AXI burst (LEN), and on the case of WR transactions, the next beat strobe.
  --                Since the BURST loop has priority over the MAIN loop on AXI handshakes, it always requests the burst and continues to the 
  --                HANDSHAKE stage on the next clock cycle.
  --  HANDSHAKE  -> Waits for the burst grant and proceeds with the TRANSFER stage.
  --  
  -- ib_TRANSFER -> The IB transfer logic, that also uses the "wr/rd_burst" record, manages the data to be transfered between the IB component 
  --                and the internal FIFO.
  --                If the "injector_mode" is set to TRUE, the IB signaling is simulated to complete the transaction as fast as possible, 
  --                lacking the correct error propagation from the AXI transaction.
  --                However, if the "injector_mode" is set to FALSE, the read data will be taken from the bottom of the FIFO at the time is 
  --                available during RD transactions, and the write data will be input into the top of the FIFO. (WIP)
  --                Each clock cycle, the FIFO buffers rolls downwards the data to be read or writen, which is transfered at the moment it 
  --                gets to the bottom. In case the beat transfer cannot be executed (only on AXI side, thus, WR operations), the roll does 
  --                not push the data word, since this one has not been transfered, but continues to roll from top to bottom if the lower 
  --                FIFO register is empty.
  --

  type rd_data_buffer   is array (natural range <>) of std_logic_vector( DATA_WIDTH - 1 downto 0 );
  type size_counter_arr is array (0 to 1) of unsigned(ib_in.rd_size'high + 1 downto 0);
  type main_state       is (idle, check_4kB, prepare_hs, handshake);
  type burst_state      is (idle, transfer, prepare_hs, handshake);

  -- Handshake type for read transactions. It's duplicated on rd_main and rd_burst to allow granting IB transactions while bursting.
  type rd_handshake_operation is record
    burst_4kb     : std_logic; -- Flag asserted when two bursts are required to complete the transaction due to surpassing the 4KB boundary.
    ib_size       : std_logic_vector(sel(12, 11, Mult_bursts_subor) downto 0);-- Original size being requested by IB component, and then first burst size.
    rem_size      : std_logic_vector(11 downto 0);-- Remaining size for second burst (4KB outbounds access)
    ib_addr       : std_logic_vector( ADDR_WIDTH     - 1 downto 0); -- Starting pointer requested by IB component.

    axi_mode      : std_logic_vector(1 downto 0); -- AXI output parameter: burst mode (FIXED, INC, WRAP).
    axi_cache     : std_logic_vector(3 downto 0); -- AXI output parameter: cache mode.
    axi_prot      : std_logic_vector(2 downto 0); -- AXI output parameter: privilage level access.
    axi_qos       : std_logic_vector(3 downto 0); -- AXI output parameter: Quality of Service signaling.
    axi_region    : std_logic_vector(3 downto 0); -- AXI output parameter: memory region access.
    axi_size      : std_logic_vector(2 downto 0); -- AXI output parameter: size mode of each beat in the burst.
    axi_len       : std_logic_vector(7 downto 0); -- AXI output parameter: number of beats in the burst.
    axi_addr      : std_logic_vector( ADDR_WIDTH     - 1 downto 0); -- AXI output parameter: Starting pointer of the AXI burst.
    axi_valid     : std_logic;                    -- AXI output parameter: valid flag for output control signals (addr, len, size, mode).

    start_shift   : unsigned( AXI4_DATA_BYTE - 1 downto 0); -- Number of right-bytes to skip when reading or writing data_bus at start of IB transfer.
    end_strb      : unsigned( AXI4_DATA_BYTE - 1 downto 0); -- Number of bytes to read from data_bus at the last IB transfer.
  end record rd_handshake_operation; 

  -- Initial handshake type for read transactions.
  type rd_main_operation is record
    state         : main_state;             -- State of the main handshake on read transactions.
    ib_grant      : std_logic;              -- Grant transaction request to IB component.
    hs            : rd_handshake_operation; -- Handshake control data to be AXI granted and sent to burst operation type.
  end record rd_main_operation;

  -- Burst type for read transactions.
  type rd_burst_operation is record
    state         : burst_state;            -- State of the read transaction.
    ib_error      : std_logic;              -- Error flag register due to SLVERR or DECERR subordinate response.

    hs            : rd_handshake_operation; -- Control data for AXI handshake
    axi_ready     : std_logic;              -- AXI output parameter: ready flag for input read data.

    fifo          : rd_data_buffer(   rd_n_fifo_regs - 1 downto 0); -- AXI data bus registers which filters narrow reads from data_tmp.
    fifo_full     : std_logic_vector( rd_n_fifo_regs - 1 downto 0); -- When asserted, the FIFO register is full or at a IB transfer.
    fifo_last     : std_logic_vector( rd_n_fifo_regs - 1 downto 0); -- When asserted, the FIFO register is the last IB transfer batch.

    ib_first      : std_logic;                                      -- Flag asserted until the first IB transfer has been done.
    ib_counter    : unsigned( AXI4_DATA_BYTE             downto 0); -- Used to count the number of bytes to IB transfer.
    ib_fifo_last  : std_logic;                                      -- IB transfers of the last AXI beat for the whole transaction flag.
    ib_valid      : std_logic;                                      -- Valid read data to transfer into IB bus from data_bus(dbits-1 downto 0).
    ib_done       : std_logic;                                      -- Asserted at the last rd_burst.ib_valid pulse of the whole transfer.

    ib_fifo_full  : std_logic_vector(rd_n_fifo_regs - 1 downto 0); -- fifo_full buffered register.
    data_fwidth   : std_logic_vector(DATA_WIDTH + dbits - 1 downto 0 ); -- TODO: Rearrenged data to be read using injector_mode = FALSE.
  end record rd_burst_operation;

  -- Handshake type for read transactions. It's duplicated on rd_main and rd_burst.
  type wr_handshake_operation is record
    burst_4kb     : std_logic; -- Flag asserted when two bursts are required to complete the transaction due to surpassing the 4KB boundary.
    ib_size       : std_logic_vector(sel(12, 11, Mult_bursts_subor) downto 0);-- Original size being requested by IB component, and then first burst size.
    rem_size      : std_logic_vector(11 downto 0);-- Remaining size for second burst (4KB outbounds access)
    ib_addr       : std_logic_vector( ADDR_WIDTH     - 1 downto 0); -- Starting pointer requested by IB component.

    axi_mode      : std_logic_vector(1 downto 0); -- AXI output parameter: burst mode (FIXED, INC, WRAP).
    axi_cache     : std_logic_vector(3 downto 0); -- AXI output parameter: cache mode.
    axi_prot      : std_logic_vector(2 downto 0); -- AXI output parameter: privilage level access.
    axi_qos       : std_logic_vector(3 downto 0); -- AXI output parameter: Quality of Service signaling.
    axi_region    : std_logic_vector(3 downto 0); -- AXI output parameter: memory region access.
    axi_size      : std_logic_vector(2 downto 0); -- AXI output parameter: size mode of each beat in the burst.
    axi_len       : std_logic_vector(7 downto 0); -- AXI output parameter: number of beats in the burst decremented by 1.
    axi_addr      : std_logic_vector( ADDR_WIDTH     - 1 downto 0); -- AXI output parameter: Starting pointer of the AXI burst.
    axi_strobe    : std_logic_vector( DATA_WIDTH/8   - 1 downto 0); -- AXI output parameter: What AXI data lanes to read from during AXI transfer.
    axi_valid     : std_logic;                    -- AXI output parameter: valid flag for output control signals (addr, len, size, mode).
  end record wr_handshake_operation; 

  type wr_main_operation is record
    state         : main_state;       -- State of the main handshake on write transactions.
    ib_grant      : std_logic;        -- Grant signals to IB component.
    hs            : wr_handshake_operation;
  end record wr_main_operation;

  type wr_burst_operation is record
    state         : burst_state;      -- State of the ongoing burst.
    ib_error      : std_logic;        -- Error flag register due to SLVERR or DECERR subordinate response.
    hs            : wr_handshake_operation;       -- Control data for AXI handshake.

    axi_valid_data: std_logic;                    -- AXI output parameter: valid write flag for output data.
    axi_last      : std_logic;                    -- Last beat of the burst flag.

    fifo          : rd_data_buffer(   wr_n_fifo_regs - 1 downto 0); -- AXI data bus registers which filters narrow reads from data_tmp.
    fifo_full     : std_logic_vector( wr_n_fifo_regs - 1 downto 0); -- When asserted, the FIFO register is full or at a IB transfer.

    ib_active     : std_logic;                                      -- IB transfer active.
    ib_full_size  : unsigned(12 downto 0);                          -- IB size requested.
    ib_counter    : unsigned( AXI4_DATA_BYTE         - 1 downto 0); -- Used to count how many bytes have been filled on the actual FIFO register.

    ib_data_buffer : std_logic_vector( dbits         - 1 downto 0); -- IB data bus buffer, used to separate it from the IB component.
  end record wr_burst_operation;

  constant RST_RD_HANDSHAKE : rd_handshake_operation  := (
    burst_4kb         => '0',
    ib_size           => (others => '0'),
    rem_size          => (others => '0'),
    ib_addr           => (others => '0'),
    axi_mode          => "01",
    axi_cache         => (others => '0'),
    axi_prot          => (others => '0'),
    axi_qos           => (others => '0'),
    axi_region        => (others => '0'),
    axi_size          => (others => '0'),
    axi_len           => (others => '0'),
    axi_addr          => (others => '0'),
    axi_valid         => '0',
    start_shift       => (others => '0'),
    end_strb          => (others => '0')
  );

  constant RST_RD_MAIN      : rd_main_operation       := (
    state             => idle,
    ib_grant          => '0',
    hs                => RST_RD_HANDSHAKE
  );

  constant RST_RD_BURST     : rd_burst_operation      := (
    state             => idle,
    ib_error          => '0',
    hs                => RST_RD_HANDSHAKE,
    axi_ready         => '0',
    fifo              => (others => (others => '0')),
    fifo_full         => (others => '0'),
    fifo_last         => (others => '0'),
    ib_first          => '1',
    ib_counter        => (others => '0'),
    ib_fifo_last      => '0',
    ib_valid          => '0',
    ib_done           => '0',
    ib_fifo_full      => (others => '0'),
    data_fwidth       => (others => '0')
  );

  constant RST_WR_HANDSHAKE : wr_handshake_operation  := (
    burst_4kb         => '0',
    ib_size           => (others => '0'),
    rem_size          => (others => '0'),
    ib_addr           => (others => '0'),
    axi_mode          => "01",
    axi_cache         => (others => '0'),
    axi_prot          => (others => '0'),
    axi_qos           => (others => '0'),
    axi_region        => (others => '0'),
    axi_size          => (others => '0'),
    axi_len           => (others => '0'),
    axi_addr          => (others => '0'),
    axi_strobe        => (others => '0'),
    axi_valid         => '0'
  );

  constant RST_WR_MAIN      : wr_main_operation       := (
    state             => idle,
    ib_grant          => '0',
    hs                => RST_WR_HANDSHAKE
  );

  constant RST_WR_BURST     : wr_burst_operation      := (
    state             => idle,
    ib_error          => '0',
    hs                => RST_WR_HANDSHAKE,
    axi_valid_data    => '0',
    axi_last          => '0',
    fifo              => (others => (others => '0')),
    fifo_full         => (others => '0'),
    ib_active         => '0',
    ib_full_size      => (others => '0'),
    ib_counter        => (others => '0'),
    ib_data_buffer    => (others => '0')
  );

  -- Uncomment generic and delete this constant when injector_mode = FALSE is implemented.
  constant injector_mode : boolean := TRUE;


  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------

  -- Registers for write/read control.
  signal rd_main  : rd_main_operation;
  signal rd_burst : rd_burst_operation;
  signal wr_main  : wr_main_operation;
  signal wr_burst : wr_burst_operation;

  -- Handshake grant signals for main and burst loops for each transaction type.
  signal rd_main_axi_grant  : std_logic;
  signal rd_burst_axi_grant : std_logic;
  signal wr_burst_axi_grant : std_logic;
  signal wr_main_axi_grant  : std_logic;

  -- Injector mode registers.
  signal rd_size  : size_counter_arr;


  -----------------------------------------------------------------------------
  -- Function/procedure declaration
  -----------------------------------------------------------------------------

  -- To compute the number of beats required in the burst (length), the size requested is added to the LSB 
  -- of the starting address that has been requested. This sum contains the number of beats on the field 
  -- over AXI4_DATA_BYTE, since each beat use the whole AXI data bus width (DATA_WIDTH).
  function decide_len(
    ib_size   : std_logic_vector(MAX_BURST_BYTE - 1 downto 0);      -- Transfer size of the burst (real num needs +1)
    ib_addr   : std_logic_vector(rd_main.hs.ib_addr'high downto 0)  -- IB request address
  ) return std_logic_vector is 
      variable len_temp : std_logic_vector(MAX_BURST_BYTE - 1 downto 0);
  begin
    -- Number of bytes to AXI transfer (it cannot surpass 4kB) = IB address + IB size
    len_temp  := add_vector(ib_addr(AXI4_DATA_BYTE - 1 downto 0), ib_size, len_temp'length);

    -- If AXI DATA_WIDTH > 128 bits, the length will never surpass 256 (xFF), but requires padding with zeros.
    if(DATA_WIDTH > 128) then
      return (AXI4_DATA_BYTE + 7 downto MAX_BURST_BYTE => '0') & len_temp(MAX_BURST_BYTE - 1 downto AXI4_DATA_BYTE);
    -- If AXI DATA_WIDTH = 128 bits, the length will never surpass 256 (xFF), but it doesn't require padding.
    elsif(DATA_WIDTH = 128) then
      return len_temp(MAX_BURST_BYTE - 1 downto AXI4_DATA_BYTE);
    -- If AXI DATA_WIDTH < 128 bits, the length is on the MSB starting from AXI4_DATA_BYTE. However, if any bit 
    -- higher than AXI4_DATA_BYTE + 7 is asserted, the length will be 256 beats (xFF) and additional bursts will
    -- be necessary for this batch (thus, Mult_bursts_subor flag is TRUE).
    elsif(len_temp(MAX_BURST_BYTE downto AXI4_DATA_BYTE + 8) = (MAX_BURST_BYTE downto AXI4_DATA_BYTE + 8 => '0')) then
      return len_temp(AXI4_DATA_BYTE + 7 downto AXI4_DATA_BYTE);
    else
      return x"FF";
    end if;

  end function decide_len;

  
begin -- rtl

  -----------------
  -- Assignments --
  -----------------
  
  -- The assignments to the AXI interconnect bus are set in such way where the burst 
  -- handshake requests are prioritized over the ones requested by main.
  handshake_priority : process (rd_main, rd_burst, wr_main, wr_burst, axi4mi)
  begin
    -- Advance eXtensible Interface (interconnect bus)
      -- Write address channel out
    if(wr_burst.hs.axi_valid = '1') then
      axi4mo.aw_id      <= (axi4mo.aw_id'high   downto ID_W_WIDTH => '0') & std_logic_vector( to_unsigned(axi_id, ID_W_WIDTH) );
      axi4mo.aw_addr    <= (axi4mo.aw_addr'high downto ADDR_WIDTH => '0') & wr_burst.hs.axi_addr(ADDR_WIDTH-1 downto AXI4_DATA_BYTE) 
                          & (AXI4_DATA_BYTE-1 downto 0 => '0'); -- Aligned starting address of the burst
      axi4mo.aw_region  <= (others => '0');
      axi4mo.aw_len     <= wr_burst.hs.axi_len;  -- Number of beats
      axi4mo.aw_size    <= wr_burst.hs.axi_size; -- Beat size
      axi4mo.aw_burst   <= wr_burst.hs.axi_mode; -- Burst mode
      axi4mo.aw_lock    <= '0';
      axi4mo.aw_cache   <= wr_burst.hs.axi_cache;
      axi4mo.aw_prot    <= wr_burst.hs.axi_prot;
      axi4mo.aw_qos     <= (others => '0');
      axi4mo.aw_valid   <= wr_burst.hs.axi_valid;
    elsif(wr_main.hs.axi_valid = '1') then
      axi4mo.aw_id      <= (axi4mo.aw_id'high   downto ID_W_WIDTH => '0') & std_logic_vector( to_unsigned(axi_id, ID_W_WIDTH) );
      axi4mo.aw_addr    <= (axi4mo.aw_addr'high downto ADDR_WIDTH => '0') & wr_main.hs.axi_addr(ADDR_WIDTH-1 downto AXI4_DATA_BYTE) 
                          & (AXI4_DATA_BYTE-1 downto 0 => '0'); -- Aligned starting address of the burst
      axi4mo.aw_region  <= (others => '0');
      axi4mo.aw_len     <= wr_main.hs.axi_len;  -- Number of beats
      axi4mo.aw_size    <= wr_main.hs.axi_size; -- Beat size
      axi4mo.aw_burst   <= wr_main.hs.axi_mode; -- Burst mode
      axi4mo.aw_lock    <= '0';
      axi4mo.aw_cache   <= wr_main.hs.axi_cache;
      axi4mo.aw_prot    <= wr_main.hs.axi_prot;
      axi4mo.aw_qos     <= (others => '0');
      axi4mo.aw_valid   <= wr_main.hs.axi_valid;
    else
      axi4mo.aw_id      <= (others => '0');
      axi4mo.aw_addr    <= (others => '0');
      axi4mo.aw_region  <= (others => '0');
      axi4mo.aw_len     <= (others => '0');
      axi4mo.aw_size    <= (others => '0');
      axi4mo.aw_burst   <= (others => '0');
      axi4mo.aw_lock    <= '0';
      axi4mo.aw_cache   <= (others => '0');
      axi4mo.aw_prot    <= (others => '0');
      axi4mo.aw_qos     <= (others => '0');
      axi4mo.aw_valid   <= '0';
    end if;
      -- Write data channel out
    axi4mo.w_data       <= (axi4mo.w_data'high downto DATA_WIDTH   => '0') & wr_burst.fifo(0);
    axi4mo.w_strb       <= (axi4mo.w_strb'high downto DATA_WIDTH/8 => '0') & wr_burst.hs.axi_strobe;
    axi4mo.w_last       <= wr_burst.axi_last;
    axi4mo.w_valid      <= wr_burst.axi_valid_data;
      -- Write response channel out
    axi4mo.b_ready      <= '1';
      -- Read address channel out
    if(rd_burst.hs.axi_valid = '1') then
      axi4mo.ar_id      <= (axi4mo.ar_id'high   downto ID_R_WIDTH => '0') & std_logic_vector(to_unsigned(axi_id, ID_R_WIDTH));
      axi4mo.ar_addr    <= (axi4mo.ar_addr'high downto ADDR_WIDTH => '0') & rd_burst.hs.axi_addr(ADDR_WIDTH-1 downto AXI4_DATA_BYTE) 
                           & (AXI4_DATA_BYTE-1 downto 0 => '0'); -- Aligned starting address of the burst
      axi4mo.ar_region  <= (others => '0');
      axi4mo.ar_len     <= rd_burst.hs.axi_len;  -- Number of beats
      axi4mo.ar_size    <= rd_burst.hs.axi_size; -- Beat size
      axi4mo.ar_burst   <= rd_burst.hs.axi_mode; -- Burst mode
      axi4mo.ar_lock    <= '0';
      axi4mo.ar_cache   <= rd_burst.hs.axi_cache;
      axi4mo.ar_prot    <= rd_burst.hs.axi_prot;
      axi4mo.ar_qos     <= (others => '0');
      axi4mo.ar_valid   <= rd_burst.hs.axi_valid;
    elsif(rd_main.hs.axi_valid = '1') then
      axi4mo.ar_id      <= (axi4mo.ar_id'high   downto ID_R_WIDTH => '0') & std_logic_vector(to_unsigned(axi_id, ID_R_WIDTH));
      axi4mo.ar_addr    <= (axi4mo.ar_addr'high downto ADDR_WIDTH => '0') & rd_main.hs.axi_addr(ADDR_WIDTH-1 downto AXI4_DATA_BYTE) 
                           & (AXI4_DATA_BYTE-1 downto 0 => '0'); -- Aligned starting address of the burst
      axi4mo.ar_region  <= (others => '0');
      axi4mo.ar_len     <= rd_main.hs.axi_len;
      axi4mo.ar_size    <= rd_main.hs.axi_size;
      axi4mo.ar_burst   <= rd_main.hs.axi_mode;
      axi4mo.ar_lock    <= '0';
      axi4mo.ar_cache   <= rd_main.hs.axi_cache;
      axi4mo.ar_prot    <= rd_main.hs.axi_prot;
      axi4mo.ar_qos     <= (others => '0');
      axi4mo.ar_valid   <= rd_main.hs.axi_valid;
    else
      axi4mo.ar_id      <= (others => '0');
      axi4mo.ar_addr    <= (others => '0');
      axi4mo.ar_region  <= (others => '0');
      axi4mo.ar_len     <= (others => '0');
      axi4mo.ar_size    <= (others => '0');
      axi4mo.ar_burst   <= (others => '0');
      axi4mo.ar_lock    <= '0';
      axi4mo.ar_cache   <= (others => '0');
      axi4mo.ar_prot    <= (others => '0');
      axi4mo.ar_qos     <= (others => '0');
      axi4mo.ar_valid   <= '0';
    end if;
      -- Read data channel out
    axi4mo.r_ready      <= rd_burst.axi_ready;
      -- Write address channel in
    if(wr_burst.hs.axi_valid = '1') then
      wr_burst_axi_grant  <= axi4mi.aw_ready;
      wr_main_axi_grant   <= '0';
    elsif(wr_main.hs.axi_valid = '1') then
      wr_burst_axi_grant  <= '0';
      wr_main_axi_grant   <= axi4mi.aw_ready;
    else
      wr_burst_axi_grant  <= '0';
      wr_main_axi_grant   <= '0';
    end if;
      -- Write data channel in
    --axi4mi.w_ready; -- used as input
      -- Write response channel in
    --axi4mi.b_id;    -- used as input
    --axi4mi.b_resp;  -- used as input
    --axi4mi.b_valid; -- used as input
      -- Read address channel in
    if(rd_burst.hs.axi_valid = '1') then
      rd_burst_axi_grant  <= axi4mi.ar_ready;
      rd_main_axi_grant   <= '0';
    elsif(rd_main.hs.axi_valid = '1') then
      rd_burst_axi_grant  <= '0';
      rd_main_axi_grant   <= axi4mi.ar_ready;
    else
      rd_burst_axi_grant  <= '0';
      rd_main_axi_grant   <= '0';
    end if;
      -- Read data channel in
    --axi4mi.r_id;    -- used as input
    --axi4mi.r_data;  -- used as input
    --axi4mi.r_resp;  -- used as input
    --axi4mi.r_last;  -- used as input
    --axi4mi.r_valid; -- used as input

    -- Bus Master (injector)
      -- Write channel
    --ib_in.wr_addr;  -- used as input
    --ib_in.wr_size;  -- used as input
    --ib_in.wr_req;   -- used as input
    --ib_in.wr_data;  -- used as input
    ib_out.wr_req_grant <= wr_main.ib_grant;
    --ib_out.wr_full      <= not(wr_burst.ib_ready);  -- Not registered, direct signalling
    --ib_out.wr_done      <= wr_burst.ib_done;        -- Not registered, direct signalling
    ib_out.wr_err       <= wr_burst.ib_error;

      -- Read channel
    --ib_in.rd_addr;  -- used as input
    --ib_in.rd_size;  -- used as input
    --ib_in.rd_req;   -- used as input
    ib_out.rd_req_grant <= rd_main.ib_grant;
    ib_out.rd_data      <= (1023 downto dbits => '0') & rd_burst.data_fwidth(dbits - 1 downto 0);
    ib_out.rd_valid     <= rd_burst.ib_valid;
    ib_out.rd_done      <= rd_burst.ib_done;
    ib_out.rd_err       <= rd_burst.ib_error;
  end process handshake_priority;
  
  -----------------------------------------------------------------------------
  -- Sequential process
  -----------------------------------------------------------------------------

  ------------------
  -- READ PROCESS --
  ------------------
  read_proc : process (clk, rstn) -- Variables used as connections between combinational logic, functions and registers.
    variable rd_addr_end          : std_logic_vector( 12             downto 0 );-- max end address LSB at INC mode (4kB check)

    variable rd_hs_done           : std_logic;                                  -- Flag to indicate the main handshake has been done.
    variable rd_fifo_free_bottom  : std_logic;                                  -- Recursive flag to know if lower FIFO registers are free.
    variable rd_data_empty        : boolean;                                    -- rd.fifo(ib_index) register depleted flag. TODO: MAYBE ERASE THIS

    -- Variables only used on implementations where DATA_WIDTH < 128 bits.
    variable rd_next_ib_size      : std_logic_vector(rd_burst.hs.ib_size'range);-- Num of bytes left to transfer on the next clk cycle of this burst.
    
  begin
    if (rstn = '0' and ASYNC_RST) then
       -- Default variable values
      rd_main               <= RST_RD_MAIN;
      rd_burst              <= RST_RD_BURST;
      rd_hs_done            := '0';
      rd_fifo_free_bottom   := '0';
      rd_next_ib_size       := (others => '0');
      rd_size               <= (others => (others => '0'));
    elsif rising_edge(clk) then
      if (rstn = '0') then
        rd_main             <= RST_RD_MAIN;
        rd_burst            <= RST_RD_BURST;
        rd_hs_done          := '0';
        rd_fifo_free_bottom := '0';
        rd_next_ib_size     := (others => '0');
        rd_size             <= (others => (others => '0'));
      else
        -- Default var values
        rd_hs_done          := '0';
        rd_fifo_free_bottom := '0';
        rd_next_ib_size     := (others => '0');

        -----------------------------
        -- RD MAIN HANDSHAKE LOGIC --
        -----------------------------
        case rd_main.state is
          when idle =>
            -- Reset registers
            rd_main           <= RST_RD_MAIN;
            rd_main.ib_grant  <= '1';
          
            -- Load request information from BM
            if (ib_in.rd_req = '1' and rd_main.ib_grant = '1') then
              rd_main.ib_grant      <= '0'; -- Deassert granting requests for IB component
              rd_main.hs.ib_addr    <= ib_in.rd_addr(rd_main.hs.ib_addr'range); -- Load starting address request
              rd_main.hs.ib_size    <= (rd_main.hs.ib_size'high downto ib_in.rd_size'length => '0') & ib_in.rd_size; -- Size -1 from real
              rd_main.hs.axi_mode   <= '0' & not(ib_in.rd_fixed_addr);
              rd_main.hs.axi_cache  <= ib_in.rd_axi_cache;
              rd_main.hs.axi_prot   <= ib_in.rd_axi_prot;
              rd_main.hs.axi_qos    <= ib_in.rd_axi_qos;
              rd_main.hs.axi_region <= ib_in.rd_axi_region;

              -- When injector mode is enabled, load real size on the queue.
              if(injector_mode) then
                rd_size(1)          <= unsigned('0' & ib_in.rd_size) + 1; -- Real size
              end if;

              -- Next, check 4kB out of bounds access
              rd_main.state         <= check_4kB;
            end if;
          
          
          when check_4kB =>
            -- Check if transaction will access two 4KB address regions
            rd_addr_end             := add_vector(rd_main.hs.ib_size, rd_main.hs.ib_addr(11 downto 0), rd_addr_end'length);
            rd_main.hs.burst_4kb    <= rd_addr_end(12);
          
            -- If transaction must be split in two bursts due to 4KB overflow boundary, calculate...
            if(rd_addr_end(12) = '1') then
              -- first burst size (-1 from real size)  MAX=4095, MIN=0
              rd_main.hs.ib_size    <= sub_vector(x"FFF", rd_main.hs.ib_addr(11 downto 0), rd_main.hs.ib_size'length);
              -- second burst size (-1 from real size) MAX=4094, MIN=0
              rd_main.hs.rem_size   <= rd_addr_end(rd_main.hs.rem_size'range);
            end if;
          
            -- Number of bytes to transfer at the last IB transfer = LSB ending address + 1 due to size being -1 from real
            rd_main.hs.end_strb     <= unsigned(rd_addr_end(AXI4_DATA_BYTE - 1 downto 0)) + 1;
          
            -- Next, compute size mode
            rd_main.state           <= prepare_hs;
          
          
          when prepare_hs =>
            -- Set AXI size mode to DATA_WIDTH and align the starting address with the DATA_WIDTH slot.
            rd_main.hs.axi_size     <= std_logic_vector(to_unsigned(AXI4_DATA_BYTE, rd_main.hs.axi_size'length));
            rd_main.hs.axi_addr     <= rd_main.hs.ib_addr(rd_main.hs.ib_addr'high downto AXI4_DATA_BYTE) & (AXI4_DATA_BYTE - 1 downto 0 => '0');

            -- Compute how many beats will be necessary to transfer the requested data in this burst.
            rd_main.hs.axi_len      <= decide_len(rd_main.hs.ib_size(11 downto 0), rd_main.hs.ib_addr);
          
            -- Save the number of bytes to discard unrequested data at the first IB transfer = LSB IB unaligned address + IB data bus width bytes
            rd_main.hs.start_shift  <= unsigned(rd_main.hs.ib_addr(AXI4_DATA_BYTE - 1 downto 0));
          
            -- Request AXI read burst when burst loop is on idle or bursting the last beat of the last burst. To verify the later when 
            -- Mult_bursts_subor = TRUE, an additional check must be performed (rd_burst.hs.ib_size < 256*DATA_WIDTH/8).
            if( rd_burst.state = idle 
            or(not(Mult_bursts_subor) and axi4mi.r_last = '1' and rd_burst.hs.burst_4kb = '0'           )
            or(    Mult_bursts_subor  and axi4mi.r_last = '1' and rd_burst.hs.burst_4kb = '0' 
                                      and rd_burst.hs.ib_size(rd_burst.hs.ib_size'high downto AXI4_DATA_BYTE + 8) 
                                          = (rd_burst.hs.ib_size'high downto AXI4_DATA_BYTE + 8 => '0') ) ) then
              rd_main.hs.axi_valid  <= '1';
              rd_main.state         <= handshake;
            end if;
            
          
          when handshake =>
            if( (rd_main_axi_grant and rd_main.hs.axi_valid) = '1') then
              rd_main.hs.axi_valid  <= '0'; -- At request being granted, deassert request
              rd_hs_done            := '1';
            end if;

            -- Once the burst is granted and the burst logic is on idle, send the control data to the burst logic.
            if(rd_burst.state = idle and (rd_main.hs.axi_valid = '0' or rd_hs_done = '1')) then
              rd_burst.hs           <= rd_main.hs;-- Pass to burst loop the handshake control data.
              rd_burst.state        <= transfer;  -- Start transfering data
              rd_burst.axi_ready    <= '1';
              rd_burst.hs.axi_valid <= '0';

              rd_main               <= RST_RD_MAIN; -- Return idle to keep granting IB requests.
              rd_main.ib_grant      <= '1';
            end if;

        end case;
          
        -----------------------------
        -- RD BURST TRANSFER LOGIC --
        -----------------------------
        case rd_burst.state is
          when idle =>
            null;

          when transfer => -- AXI transfer --
          -- The AXI transfer occurs by buffering the read data onto the rd_burst.axi_data_buffer register when the subordinate asserts the valid data flag.
          -- Then, the rd_burst.axi_data_buffer is loaded onto a FIFO register, which will be used at the IB transfer. However, due to this single clock 
          -- cycle delay between AXI read and data load, both valid and the mask signals are also buffered on the "buffer" registers (these
          -- are outside of the state machine and updated with each clock pulse). 

              -- Check if subordinate is delivering valid data with the same AXI ID as requested and if manager is listening.
            if (axi4mi.r_valid = '1' and axi4mi.r_id(ID_R_WIDTH-1 downto 0) = std_logic_vector(to_unsigned(axi_id, ID_R_WIDTH)) 
                and rd_burst.axi_ready = '1') then

              -- Register inputs to separate AXI network from further computation.
              rd_burst.fifo(rd_n_fifo_regs - 1) <= axi4mi.r_data(DATA_WIDTH - 1 downto 0);
              rd_burst.ib_error                 <= axi4mi.r_resp(1);

              -- Signal the present rd_burst.FIFO register as prepared for IB transfer, for whenever it is due to FIFO queue.
              rd_burst.fifo_full(rd_n_fifo_regs - 1)   <= '1';

              -- Deassert the ready flag to finish the AXI burst if it is the last beat or if there is not enough space in the FIFO at the moment.
              if( axi4mi.r_last = '1' or rd_burst.fifo_full(rd_n_fifo_regs - 2 downto 0) = (rd_n_fifo_regs - 2 downto 0 => '1') ) then
                rd_burst.axi_ready    <= '0';
              end if;

              -- The last AXI transfer of the whole transaction will set the proper rd_burst.fifo_last bit, so the IB transfer logic knows when to end.
              -- However, there's a distinction between DATA_WIDTH <= 64 bits and higher data widths. If there's still data to transfer to the same 
              -- subordinate because this burst has not been enough, do not assert the last IB transfer on the FIFO register and update rd_burst.ib_size.
              if(Mult_bursts_subor) then
                rd_next_ib_size     := sub_vector(rd_burst.hs.ib_size, DATA_WIDTH/8, rd_next_ib_size'length);
                rd_burst.hs.ib_size <= rd_next_ib_size;
                -- Mark as last FIFO register to be read by IB if there is not another burst due to 4kB surpass, the last beat has been
                -- received as last of the burst and there's no more data for this burst (rd_next_ib_size is overflown due to the subtraction).
                rd_burst.fifo_last(rd_n_fifo_regs - 1)  <= not(rd_burst.hs.burst_4kb) and axi4mi.r_last and rd_next_ib_size(rd_next_ib_size'high);
              else
                rd_burst.fifo_last(rd_n_fifo_regs - 1)  <= not(rd_burst.hs.burst_4kb) and axi4mi.r_last;
              end if;

              -- On last AXI beat...
              if(axi4mi.r_last = '1') then
                -- Compute new handshake if multiple bursts are necessary to complete this data batch:
                if( Mult_bursts_subor and rd_next_ib_size(rd_next_ib_size'high) = '0' ) then
                  if(rd_burst.hs.axi_mode = INC) then
                    rd_burst.hs.axi_addr  <= add_vector(rd_burst.hs.axi_addr(rd_burst.hs.ib_addr'high downto 8 + AXI4_DATA_BYTE), 1, 
                                              rd_burst.hs.axi_addr'high - 7 - AXI4_DATA_BYTE) & (7 + AXI4_DATA_BYTE downto 0 => '0');
                  end if;
                  rd_burst.state    <= prepare_hs;

                -- Or compute a new handshake if there's another batch of data due to the 4kB split. 
                elsif(rd_burst.hs.burst_4kb = '1') then
                  if(Mult_bursts_subor) then
                    rd_burst.hs.ib_size   <= '0' & rd_burst.hs.rem_size;
                  else
                    rd_burst.hs.ib_size   <= rd_burst.hs.rem_size;
                  end if;
                  if(rd_burst.hs.axi_mode = INC) then
                    rd_burst.hs.axi_addr  <= add_vector(rd_burst.hs.axi_addr(rd_burst.hs.axi_addr'high downto 12), 1, 
                                              rd_burst.hs.axi_addr'high - 11) & (11 downto 0 => '0');
                  end if;
                  rd_burst.state    <= prepare_hs;
                else
                -- The transaction has finished on the AXI side.
                  rd_burst.state    <= idle;
                end if;
              end if;
            -- In case no read is happening, assert AXI ready if there's free space on the FIFO.
            elsif( (rd_burst.fifo_full(rd_n_fifo_regs - 2 downto 0) /= (rd_n_fifo_regs - 2 downto 0 => '1')) ) then
              rd_burst.axi_ready    <= '1';
            end if;


          when prepare_hs =>
            -- Compute how many beats will be necessary to transfer the requested data in this burst.
            rd_burst.hs.axi_len     <= decide_len(rd_burst.hs.ib_size(11 downto 0), rd_burst.hs.axi_addr);
          
            -- Next, the handshake step.
            rd_burst.hs.axi_valid   <= '1'; -- Request AXI read burst.
            rd_burst.state          <= handshake;


          when handshake =>
            -- Request burst transaction with priority over main loop.
            if( (rd_burst_axi_grant and rd_burst.hs.axi_valid) = '1') then
              rd_burst.hs.axi_valid   <= '0'; -- At request being granted, deassert request.
              -- Clean the 4kB flag when all burst of the same subordinate have finished.
              if( not(Mult_bursts_subor and rd_burst.hs.ib_size(rd_burst.hs.ib_size'high) /= '1') ) then
                rd_burst.hs.burst_4kb <= '0';
              end if;
              if( (rd_burst.fifo_full(rd_n_fifo_regs - 2 downto 0) /= (rd_n_fifo_regs - 2 downto 0 => '1')) ) then
                rd_burst.axi_ready    <= '1';
              end if;
              rd_burst.state          <= transfer;
            end if;

        end case;


      -------------------------
      -- RD IB DATA TRANSFER --
      -------------------------
        -- WIP: Due to technical difficulties that must be solved in the future, the IB transfer logic must be revised.
        --      Thus, for the moment, use the injector_mode that simulates the correct number of readings without actually 
        --      transfering the data.
      -- The injector mode, when enabled, discards all read data while simulates the correct IB signaling for the transaction.
        if(injector_mode) then
          rd_burst.data_fwidth  <= (others => '0'); -- To not infer latches.
          rd_burst.ib_valid     <= '0';
          rd_burst.ib_done      <= '0';
          if(rd_size(0) /= (rd_size(0)'range => '0')) then
            rd_burst.ib_valid   <= '1';
            rd_size(0)          <= rd_size(0) - dbits/8;
            if(rd_size(0) <= dbits/8) then
              rd_burst.ib_done  <= '1';
              rd_size(0)        <= (others => '0');
            end if;
          else -- Roll next size if there isn't one "transferring".
            rd_size(0)          <= rd_size(1);
            if(rd_size(1) /= (rd_size(1)'range => '0')) then
              rd_size(1)        <= (others => '0');
            end if;
          end if;

        else -- INJECTOR_MODE = FALSE

      -- The IB transfer logic transfers from the RD FIFO to the IB component at dbits bits per clock cycle throughput without pause, unless  
      -- there's not enough data in the FIFO to IB transfer. This is achieved by loading rd_burst.fifo(ib_index) onto rd_burst.data_fwidth 
      -- used as output, which transfers data and shifts while there's enough to perform a IB transfer (rd_burst.ib_counter >= dbits/8). 
      -- When the rd_burst.data_fwidth does not have enough data for a IB transfer, the next FIFO register is read and positioned after the  
      -- data left to be read, freeing the FIFO register since it has been read for new AXI beat readings.

--          -- Set combinational variables --
--          -- Check if this is the last IB transfer before rd_burst.fifo(ib_index) is depleted after a transfer of dbits bits.
--          rd_data_empty       := rd_burst.ib_counter < to_unsigned(dbits/8, rd_burst.ib_counter'length);
--
--          -- Check if there's data to IB transfer. Delayed with a buffer register so the last AXI beat transfer is finished on this FIFO register.
--          if(rd_burst.ib_fifo_full(0) = '1' or rd_burst.ib_first = '0') then
--
--          -- Before any IB transfer, load and shift the FIFO register to put the requested data on 0 to higher, flushing unrequested data read 
--          -- by the AXI side. Then, initilize the rd_burst.ib_counter with [the whole AXI data bus width bytes value or with the number of bytes
--          -- to read from the last FIFO register to IB transfer] minus the shift performed on this rd_burst.ib_first cycle.
--            if(rd_burst.ib_first = '1') then
--
--              -- Deassert the first IB transfer flag, since it's only required to shift rd_burst.start_shift before than the first IB transfer.
--              rd_burst.ib_first   <= '0';
--
--              -- The initial counter is set by rd_burst.end_strb if there's only one FIFO register to transfer. 
--              if(rd_burst.fifo_last(0) = '1') then
--                rd_burst.ib_counter <= rd_burst.hs.end_strb - rd_burst.hs.start_shift;
--              else -- Otherwise, the initial counter is the AXI data bus bytes.
--                rd_burst.ib_counter <= to_unsigned(DATA_WIDTH/8, rd_burst.ib_counter'length) - rd_burst.hs.start_shift;
--              end if;
--
--              -- Load, shift and free the FIFO register. Also, record if this is the last AXI beat of the whole IB transaction.
--              rd_burst.data_fwidth  <= (rd_burst.data_fwidth'high downto DATA_WIDTH - 8*to_integer(rd_burst.hs.start_shift) => '0') 
--                                       & rd_burst.fifo(0)(DATA_WIDTH - 1 downto DATA_WIDTH - 8*to_integer(rd_burst.hs.start_shift));
--              rd_burst.fifo_full(0) <= '0';
--              rd_fifo_free_bottom   := '1';
--              rd_burst.ib_fifo_last <= rd_burst.fifo_last(0);
--
--          else -- rd_burst.ib_first = '0' check
--            if(not rd_data_empty) then
--
--              -- There's enough data to IB transfer on the next cycle, so shift a IB transfer.
--              rd_burst.data_fwidth  <= std_logic_vector(shift_right(unsigned(rd_burst.data_fwidth), dbits/8));
--
--              -- Update IB counter.
--              rd_burst.ib_counter   <= rd_burst.ib_counter - dbits/8;
--
--            else  -- There isn't enough data on rd_burst.data_fwidth to IB transfer.
--
--              -- Last IB transfer
--              if(rd_burst.ib_fifo_last = '1') then
--                -- TODO: Apply a mask, both in here and on the first IB transfer.
--                rd_burst.data_fwidth  <= (rd_burst.data_fwidth'high downto dbits => '0') & rd_burst.data_fwidth(dbits - 1 downto 0);
--                rd_burst.ib_counter   <= (others => '0');
--                rd_burst.ib_first     <= '1';
--
--              elsif(rd_burst.ib_fifo_full(0) = '1' or rd_burst.ib_fifo_last = '1') then
--
--              -- In case this cycle will deplete rd_burst.data_fwidth and the next FIFO register is prepared to be transfered,
--              -- use the remaining data (rd_burst.ib_counter lower or equal to AXI data bus bytes) as LSB and take the 
--              -- lacking bytes to fill dbits from the next FIFO register, while also shifting its content for posterior transfers.
--              -- (since the shifting index k surpasses the bytes available in the next FIFO register on unaligned starting transfers, 
--              -- fill the remaining byte positions with zeros, that, in number of bytes, will be always lower than the bytes in dbits)
--                rd_burst.data_fwidth  <= (DATA_WIDTH + 8*to_integer(rd_burst.ib_counter) - 1 downto dbits => '0') 
--                                          & rd_burst.fifo(0) 
--                                          & rd_burst.data_fwidth(8*to_integer(rd_burst.ib_counter) - 1 downto dbits);
--
--                -- Set the present FIFO register to be prepared for new AXI transfer and change the FIFO register rd_burst.ib_index.
--                rd_burst.fifo_full(0) <= '0';
--
--                -- Compute the number of bytes that'll be left to read on the next FIFO register.
--                -- In case where the next FIFO register is the last, counter = end_strb - ( dbits/8 - ib_counter ) = ( end_strb + ib_counter ) - dbits/8
--                -- Otherwise, it must be taken the whole AXI data bus, counter = AXI_DATA/8 - ( dbits/8 - ib_counter ) = ib_counter + AXI_DATA/8 - dbits/8
--                if(rd_burst.fifo_last(1) = '1') then
--                  rd_burst.ib_counter <= rd_burst.hs.end_strb + rd_burst.ib_counter - dbits/8;
--
--                  -- In addition, if the number of bytes read on this clock cycle from rd_burst.fifo(rd_ib_next_index) include all the data required 
--                  -- by rd_burst.end_strb, end the transfer on the next clock cycle. (done, mask and return to idle)
--                  if(to_unsigned(dbits/8, rd_burst.ib_counter'length) - rd_burst.ib_counter >= rd_burst.hs.end_strb) then
--                    rd_burst.ib_done  <= '1';
--                    rd_burst.state    <= idle;
--                  end if;
--
--                else
--                  rd_burst.ib_counter <= rd_burst.ib_counter + DATA_WIDTH/8 - dbits/8;
--                end if;              
--
--              else   -- rd_burst.fifo_full(rd_ib_next_index) = 0 check
--
--              -- In case there's not enough data to read from the FIFO, check if this is the last FIFO register to IB transfer. If it 
--              -- is, check how many bytes of remaining data are left. If it's lower than dbits/8, this is the last IB transfer. However, 
--              -- if it's exactly dbits/8 and it's the last FIFO register, delay the end of the IB transfer for another clock cycle 
--              -- (do not increase the rd_burst.ib_counter, let the execution return to this step to execute the end of the IB transfer).
--                if(rd_burst.fifo_last(0) = '1') then 
--                  rd_burst.ib_counter <= rd_burst.ib_counter - dbits/8;
--                  rd_burst.ib_done  <= '1';
--                  rd_burst.state    <= idle;
--                else
--              -- Otherwise, if there's not enough data and this is not the last FIFO register, wait for the next AXI transfer.
--                end if;
--
--              end if; -- rd_burst.fifo_full(rd_ib_next_index) end of check
--            end if; -- rd_empty_data end of check
--          end if;   -- ib_first end of check
--
--          if(rd_burst.ib_counter >= 2*dbits/8 or rd_burst.ib_fifo_last = '1' or rd_burst.ib_fifo_full(0) = '1' ) then
--            rd_burst.ib_valid     <= '1';
--          else
--            rd_burst.ib_valid     <= '0';
--          end if;
--
        end if; -- INJECTOR_MODE CHECK


        -- Delayed signals, to be revised.
        rd_burst.ib_fifo_full <= rd_burst.fifo_full;
        

      -------------
      -- RD FIFO --
      -------------
        -- On each clock cycle, push new data from top to the bottom on the FIFO, if bottom is or will be free.
        if(not(injector_mode)) then
          for k in 1 to rd_n_fifo_regs - 1 loop
            rd_fifo_free_bottom := rd_burst.fifo_full(k - 1) or rd_fifo_free_bottom;
            if(rd_fifo_free_bottom = '0') then
              rd_burst.fifo_full(k)     <= '0';
              rd_burst.fifo_full(k - 1) <= rd_burst.fifo_full(k);
              rd_burst.fifo_last(k - 1) <= rd_burst.fifo_last(k);
              rd_burst.fifo(k - 1)      <= rd_burst.fifo(k);
            end if;
          end loop;
        else -- But, when injector_mode enabled, discard all data.
          rd_burst.fifo_full            <= (others => '0');
        end if;

      end if;
    end if;
  end process read_proc;



  -------------------
  -- WRITE PROCESS --
  -------------------
  write_proc : process (clk, rstn) -- Variables used as connections between combinational logic, functions and registers.
    variable wr_addr_end          : std_logic_vector( 12               downto 0 );  -- max end address LSB at INC mode (4kB check)
    variable wr_fifo_free_bottom  : std_logic;                                      -- Free of the FIFO bottom

    -- Variables only used on implementations where DATA_WIDTH < 128 bits.
    variable wr_next_ib_size      : std_logic_vector(wr_burst.hs.ib_size'range);-- Num of bytes left to transfer on the next clk cycle of this burst.
    
  begin
    if(rstn = '0' and ASYNC_RST) then
      wr_main               <= RST_WR_MAIN;
      wr_burst              <= RST_WR_BURST;
      wr_addr_end           := (others => '0');
      wr_fifo_free_bottom   := '0';
    elsif rising_edge(clk) then
      if (rstn = '0') then
        wr_main             <= RST_WR_MAIN;
        wr_burst            <= RST_WR_BURST;
        wr_addr_end         := (others => '0');
        wr_fifo_free_bottom := '0';
      else
        wr_addr_end         := (others => '0');
        wr_fifo_free_bottom := '0';

      -----------------------------
      -- WR MAIN HANDSHAKE LOGIC --
      -----------------------------
        case wr_main.state is
          when idle =>
            -- Grant IB if previous IB transaction has been completed.
            wr_main.ib_grant        <= not(wr_burst.ib_active);
          
            -- Load request information from BM
            if (ib_in.wr_req = '1' and wr_main.ib_grant = '1') then
              wr_main.ib_grant      <= '0';           -- Deassert granting requests for IB component
              wr_main.hs.ib_addr    <= ib_in.wr_addr(wr_main.hs.ib_addr'range); -- Load starting address request
              wr_main.hs.ib_size    <= (wr_main.hs.ib_size'high downto ib_in.wr_size'length => '0') & ib_in.wr_size; -- Size -1 from real
              wr_main.hs.axi_mode   <= '0' & not(ib_in.wr_fixed_addr);
              wr_main.hs.axi_cache  <= ib_in.wr_axi_cache;
              wr_main.hs.axi_prot   <= ib_in.wr_axi_prot;
              wr_main.hs.axi_qos    <= ib_in.wr_axi_qos;
              wr_main.hs.axi_region <= ib_in.wr_axi_region;

              -- Start the IB transfer on the next clock cycle.
              wr_burst.ib_active    <= '1';
              wr_burst.ib_full_size <= unsigned(add_vector(ib_in.wr_size, 1, wr_burst.ib_full_size'length)); -- Real size

              -- Next, check 4kB out of bounds access
              wr_main.state         <= check_4kB;
            end if;
          
          
          when check_4kB => -- Worst delay path: ADD 12+12, SUB 13-13
            -- Check if transaction will access two 4KB address regions
            wr_addr_end             := add_vector(wr_main.hs.ib_size, wr_main.hs.ib_addr(11 downto 0), wr_addr_end'length);
            wr_main.hs.burst_4kb    <= wr_addr_end(12);
          
            -- If transaction must be split in two bursts, due to 4KB overflow boundary, calculate...
            if(wr_addr_end(12) = '1') then
              -- first burst size  (-1 from real size) MAX=4095, MIN=0
              wr_main.hs.ib_size    <= sub_vector(x"FFF", wr_main.hs.ib_addr(11 downto 0), wr_main.hs.ib_size'length);
              -- second burst size (-1 from real size) MAX=4094, MIN=0
              wr_main.hs.rem_size   <= wr_addr_end(wr_main.hs.rem_size'range);
            end if;

            -- Next, compute size mode
            wr_main.state           <= prepare_hs;
          
          
          when prepare_hs =>
            -- Set AXI size mode to DATA_WIDTH and align the starting address with the DATA_WIDTH slot.
            wr_main.hs.axi_size     <= std_logic_vector(to_unsigned(AXI4_DATA_BYTE, wr_main.hs.axi_size'length));
            wr_main.hs.axi_addr     <= wr_main.hs.ib_addr(wr_main.hs.ib_addr'high downto AXI4_DATA_BYTE) & (AXI4_DATA_BYTE - 1 downto 0 => '0');

            -- Compute how many beats will be necessary to transfer the requested data in this burst.
            wr_main.hs.axi_len      <= decide_len(wr_main.hs.ib_size(11 downto 0), wr_main.hs.ib_addr);

            -- Generate the mask to be shifted for the first AXI transfer.
            for k in wr_main.hs.axi_strobe'range loop
              if(k > to_integer(unsigned(wr_main.hs.ib_size))) then
                wr_main.hs.axi_strobe(k) <= '0';
              else
                wr_main.hs.axi_strobe(k) <= '1';
              end if;
            end loop;
            
            -- Request AXI write burst when burst loop is on idle or bursting the last beat of the last burst. To verify the later when 
            -- Mult_bursts_subor = TRUE, an additional check must be performed (wr_burst.hs.ib_size < DATA_WIDTH).
            if( wr_burst.state = idle 
            or(not(Mult_bursts_subor) and wr_burst.hs.burst_4kb = '0'                                   ) 
            or(    Mult_bursts_subor  and wr_burst.hs.burst_4kb = '0' 
                                      and wr_burst.hs.ib_size(wr_burst.hs.ib_size'high downto AXI4_DATA_BYTE + 8) 
                                          = (wr_burst.hs.ib_size'high downto AXI4_DATA_BYTE + 8 => '0') ) ) then
              wr_main.hs.axi_valid      <= '1'; -- Request AXI write burst.
              wr_main.state             <= handshake;
            end if;
          
          
          when handshake =>
            -- Having all computation steps and proper registration separated allows maximum 
            -- frequency of operation when including this AXI manager interface at the interconnect
            -- bus. For a write burst, AW AXI control buses are used to send the burst control data.
            if( (wr_main_axi_grant and wr_main.hs.axi_valid) = '1') then
              wr_main.hs.axi_valid      <= '0'; -- At request being granted, deassert AXI handshake request.
            end if;

            if(wr_burst.state = idle and (wr_main.hs.axi_valid = '0' or (wr_main_axi_grant and wr_main.hs.axi_valid) = '1')) then
              wr_burst.hs               <= wr_main.hs;
              wr_burst.hs.axi_valid     <= '0';
              -- Add ib_address to first batch size so it's aligned with DATA_WIDTH. This, will be used for the last beat strobe.
              wr_burst.hs.ib_size       <= add_vector(wr_main.hs.ib_size, wr_main.hs.ib_addr(AXI4_DATA_BYTE - 1 downto 0), 
                                           wr_burst.hs.ib_size'length);
              -- Shift and set the strobe for the first beat.
              wr_burst.hs.axi_strobe    <= std_logic_vector(shift_left( unsigned(wr_main.hs.axi_strobe), 
                                            to_integer(unsigned(wr_main.hs.ib_addr(AXI4_DATA_BYTE - 1 downto 0))) ));
              -- Assert last beat if there's only one beat in the next burst.
              if(wr_main.hs.axi_len = (wr_main.hs.axi_len'range => '0')) then
                wr_burst.axi_last       <= '1';
              end if;
              wr_burst.state            <= transfer;
              
              -- Next, beat transfers
              wr_main.state             <= idle;
            end if;

        end case;


      -----------------------------
      -- WR BURST TRANSFER LOGIC --
      -----------------------------
        case wr_burst.state is 
          when idle =>
            null;


          when transfer => -- AXI transfer --
          -- The AXI transfer sets the AXI valid write flag anytime there's data available in a buffer register that reads from the FIFO, 
          -- freeing the FIFO register for IB fransfers. In addition, the size left to transfer wr.ib_size is decreased with the number of 
          -- bytes transfered.

            -- Load the variables for last AXI beat in the burst, next FIFO index at the AXI side and the read flag.
            wr_fifo_free_bottom         := wr_burst.axi_valid_data and axi4mi.w_ready;
            wr_burst.fifo_full(0)       <= wr_fifo_free_bottom;

            -- On the first beat read, zero the wr.ib_addr LSB used to set up the lower bound of the strobe for the first beat.
            -- Generate the mask to be shifted for the first AXI transfer.
            if(wr_fifo_free_bottom = '1') then
              for k in wr_burst.hs.axi_strobe'range loop
                if(k + DATA_WIDTH/8 > to_integer(unsigned(wr_burst.hs.ib_size))) then
                  wr_burst.hs.axi_strobe(k)   <= '0';
                else
                  wr_burst.hs.axi_strobe(k)   <= '1';
                end if;
              end loop;
            end if;

            -- Each AXI transfer, update size left on this batch.
            if(wr_fifo_free_bottom = '1') then
              wr_next_ib_size           := sub_vector(wr_burst.hs.ib_size, DATA_WIDTH/8, wr_burst.hs.ib_size'length);
              wr_burst.hs.ib_size       <= wr_next_ib_size;
              if(injector_mode) then
                wr_burst.axi_valid_data <= '1';
              else
                wr_burst.axi_valid_data <= wr_burst.fifo_full(1);
              end if;
            elsif(injector_mode) then
              wr_burst.axi_valid_data   <= '1';
            else
              wr_burst.axi_valid_data   <= wr_burst.fifo_full(0);
            end if;

            -- Update length when a beat has been sent.
            if(wr_fifo_free_bottom = '1') then
              wr_burst.hs.axi_len       <= sub_vector(wr_burst.hs.axi_len, 1, wr_burst.hs.axi_len'length);
            end if;

            -- Check if it's the last beat (length = 0 or length = 1 but that one has been sent).
            if( (wr_burst.hs.axi_len(wr_burst.hs.axi_len'high downto 1) = (wr_burst.hs.axi_len'high downto 1 => '0')  
               and wr_fifo_free_bottom = '1') ) then
                wr_burst.axi_last       <= '1';
            end if;

            -- Last beat has been sent, do next batch or return to idle.
            if( (wr_fifo_free_bottom and wr_burst.axi_last) = '1') then
              wr_burst.axi_last         <= '0';
              wr_burst.axi_valid_data   <= '0';

              -- If multiple bursts to same subordinate may be necessary, check for those cases.
              if(Mult_bursts_subor and wr_next_ib_size(wr_next_ib_size'high) = '0') then
                  if(wr_burst.hs.axi_mode = INC) then
                    wr_burst.hs.axi_addr    <= add_vector(wr_burst.hs.axi_addr(wr_burst.hs.axi_addr'high downto 8 + AXI4_DATA_BYTE), 1, 
                                               wr_burst.hs.axi_addr'high - 7 - AXI4_DATA_BYTE) & (7 + AXI4_DATA_BYTE downto 0 => '0');
                  end if;
                  wr_burst.state        <= prepare_hs;
              -- If second burst/batch must be processed.
              elsif(wr_burst.hs.burst_4kb = '1') then
                if(Mult_bursts_subor) then
                  wr_burst.hs.ib_size   <= '0' & wr_burst.hs.rem_size;
                else
                  wr_burst.hs.ib_size   <= wr_burst.hs.rem_size;
                end if;
                if(wr_burst.hs.axi_mode = INC) then
                  wr_burst.hs.axi_addr  <= add_vector(wr_burst.hs.axi_addr(wr_burst.hs.axi_addr'high downto 12), 1, 
                                           wr_burst.hs.axi_addr'high - 11) & (11 downto 0 => '0');
                end if;
                wr_burst.state          <= prepare_hs;
              else
              -- Whole transaction has finished, return to idle.
                wr_burst.state          <= idle;
              end if;
            end if;


          when prepare_hs =>
            -- Compute how many beats will be necessary to transfer the requested data in this burst.
            wr_burst.hs.axi_len         <= decide_len(wr_burst.hs.ib_size(11 downto 0), wr_burst.hs.axi_addr);
            wr_burst.hs.axi_valid       <= '1'; -- Request AXI write burst.
            -- Generate the mask the first AXI beat of secondary bursts.
            for k in wr_burst.hs.axi_strobe'range loop
              if(k > to_integer(unsigned(wr_burst.hs.ib_size))) then
                wr_burst.hs.axi_strobe(k)   <= '0';
              else
                wr_burst.hs.axi_strobe(k)   <= '1';
              end if;
            end loop;

            wr_burst.state              <= handshake;


          when handshake =>
            -- Request burst transaction with priority over main loop.
            if( (wr_burst_axi_grant and wr_burst.hs.axi_valid) = '1') then
              wr_burst.hs.axi_valid     <= '0'; -- At request being granted, deassert request.
              -- Clean the 4kB flag when all burst of the same subordinate have finished.
              if( not(Mult_bursts_subor and wr_burst.hs.ib_size(wr_burst.hs.ib_size'high) /= '1') ) then
                wr_burst.hs.burst_4kb   <= '0';
              end if;
              -- Assert last beat if there's only one beat in the next burst.
              if(wr_burst.hs.axi_len = (wr_burst.hs.axi_len'range => '0')) then
                wr_burst.axi_last       <= '1';
              end if;
              wr_burst.state            <= transfer;
            end if;


        end case;


      -------------------------
      -- WR IB DATA TRANSFER --
      -------------------------
        if(injector_mode) then
          wr_burst.fifo(wr_n_fifo_regs - 1 ) <= (others => '0');
          if(wr_burst.ib_active = '1') then
            ib_out.wr_full              <= '0';
            if(wr_burst.ib_full_size >= to_unsigned(dbits/8, wr_burst.ib_full_size'length)) then
              wr_burst.ib_full_size     <= wr_burst.ib_full_size - dbits/8;
            elsif(wr_burst.ib_full_size /= ( wr_burst.ib_full_size'range => '0')) then
              wr_burst.ib_full_size     <= (others => '0');
              ib_out.wr_full            <= '0';
            else
              wr_burst.ib_full_size     <= (others => '0');
              ib_out.wr_done            <= '1';
              wr_burst.ib_active        <= '0';
            end if;
          else
            ib_out.wr_done              <= '0';
            ib_out.wr_full              <= '1';
          end if;
        else
          -- TODO: BUFFER IB DATA INPUT, REARRANGE AND PUT IT ON THE FIFO'S TOP
        end if;


      -------------
      -- WR FIFO --
      -------------
        -- On each clock cycle, push new data from top to the bottom on the FIFO, if bottom is or will be free.
        for k in 1 to wr_n_fifo_regs - 1 loop
          wr_fifo_free_bottom := wr_burst.fifo_full(k - 1) or wr_fifo_free_bottom;
          if(wr_fifo_free_bottom = '0') then
            wr_burst.fifo_full(k)     <= '0';
            wr_burst.fifo_full(k - 1) <= wr_burst.fifo_full(k);
            wr_burst.fifo(k - 1)      <= wr_burst.fifo(k);
          end if;
        end loop;

        
      -------------------------
      -- WRITE RESPONSE READ --
      -------------------------
        -- This logic always listen for write responses, not matter if a write transaction is not ongoing.
        if(axi4mi.b_valid = '1' and axi4mi.b_id(ID_W_WIDTH-1 downto 0) = std_logic_vector(to_unsigned(axi_id, ID_W_WIDTH))) then
          wr_burst.ib_error <= axi4mi.b_resp(1);
        end if;
      
      
      end if;
    end if;
  end process write_proc;

  -----------------------------------------------------------------------------
  -- Combinational process
  -----------------------------------------------------------------------------


end architecture rtl;
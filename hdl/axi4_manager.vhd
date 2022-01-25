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
-- AXI4 FULL manager - generic bus master bridge
--
-- This AXI4 manager interface does not allow to have a BM bus wider than the AXI bus
-- (AXI4_DATA_WIDTH must be greater or equal to dbits). It may not tolerate transactions
-- of a width higher than the MAX_SIZE_BURST. It applies little endian data structure.
-----------------------------------------------------------------------------

entity axi4_manager is
  generic (
    -- Bus Manager configuration
    dbits           : integer range 32 to  128  := 32;      -- BM data bus width (must be a power of 2)
    -- AXI Manager configuration
    axi_id          : integer                   := 0;       -- AXI master index
    MAX_SIZE_BURST  : integer range 32 to 4096  := 4096;    -- Maximum size of a BM transaction. (default=4096)
    -- Injector configuration
    ASYNC_RST       : boolean                   := FALSE    -- Allow asynchronous reset
  );
  port (
    rstn            : in  std_ulogic;         -- Reset
    clk             : in  std_ulogic;         -- Clock
    -- AXI interface signals
    axi4mi          : in  axi4_in_type;       -- AXI4 master input 
    axi4mo          : out axi4_out_type;      -- AXI4 master output
    -- Bus master signals
    bm_in           : in  bm_in_type;         -- BM master input
    bm_out          : out bm_out_type         -- BM master output
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
    -- both the AXI data bus width and the maximum burst size being set.
    0 => to_std_logic( AXI4_DATA_WIDTH/8 >= 128 and MAX_SIZE_BURST >= 128 ),
    1 => to_std_logic( AXI4_DATA_WIDTH/8 >=  64 and MAX_SIZE_BURST >=  64 ),
    2 => to_std_logic( AXI4_DATA_WIDTH/8 >=  32 and MAX_SIZE_BURST >=  32 ),
    3 => to_std_logic( AXI4_DATA_WIDTH/8 >=  16 and MAX_SIZE_BURST >=  16 ),
    4 => to_std_logic( AXI4_DATA_WIDTH/8 >=   8 and MAX_SIZE_BURST >=   8 ),
    5 => to_std_logic( AXI4_DATA_WIDTH/8 >=   4 and MAX_SIZE_BURST >=   4 ), -- These should be unnecessary
    6 => to_std_logic( AXI4_DATA_WIDTH/8 >=   2 and MAX_SIZE_BURST >=   2 )  -- These should be unnecessary
    );
  constant mask_array       : array_128vector(0 to 7) := ( 
    -- Array with the various possible byte masks. Necessary as a constant because my VHDL skills are limited on fully parametric systems.
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
  -- and the total transfer size of the operation (this last must be lower or equal to the set by MAX_SIZE_BURST-1).
  -- Once the request has been granted (deasserting new requests until finished), it processes the information to check if 
  -- the total transfer surpasses the 4kB address boundary, splitting the transaction in two bursts in such situations.
  -- The interface sets up the correspondent AXI control data for the first or only trasaction burst, including burst mode,
  -- size mode, burst length and the aligned starting address respect to the size mode being set. Then, proceeds to execute
  -- the handshake with the AXI interconnect network.
  -- 
  -- During write operations, the write strobe is used to manage BM's misaligned starting addresses. In read operations, 
  -- however, the unrequested read data is discarded through shifts until the last BM transfer, where a strobe is used.
  -- On both operations, the BM component is completly ignorant of how the transaction is really performed at the network, 
  -- by only reading or writing the data it requests to the interface.
  --
  -- Once the burst is completed, a second is generated to finish the total transfer size requested by BM component if the 
  -- total transfer size did surpass the 4kB boundary. This execution repeats computing, handshake and transfer steps.
  --
  -- After the total transaction is completed, the interface returns to the idle state to accept new requests from BM.
  --
  -- The following table points out the actions done at each stage of the operation:
  -- 
  -- IDLE     -> Accept new requests from BM component.
  -- COMPUTE1 -> Compute if 4KB address space overflow occurs.
  --             In the case it does, assert two_burst flag so a second burst is generated after completing the first.
  --             Compute the number of bytes to transfer at the last beat of the whole transaction (last burst).
  -- COMPUTE2 -> Decide AXI size mode for the burst and generate the aligned starting address.
  --             Compute the number of bytes to skip at the transfer of the first beat of the whole transaction (first burst).
  -- COMPUTE3 -> Set burst mode (at the moment, there's only INC).
  --             Decide burst length (number of beats) taking into account the size mode.
  --          RD:Assert "ARVALID" since all required information is now registered.
  --          WR:Set the strobe bits for WRITE transaction.
  -- HANDSHAKE-> 
  --          RD:Waits for "RREADY" to load data from the AXI data bus. Loads counter with LSB aligned starting address,
  --             so the first AXI transfer takes into account that it is reading from a higher starting address than 0.
  -- TRANSFER1->
  --          RD:Reads many beats from the AXI data bus onto the data_tmp register. This register is then ORed with the 
  --             data_bus on following clock cycles, separating AXI from the interface circuits to maximize frequency of 
  --             operation. The number of beats known to perform is regulated by the counter, so it only reads up to 
  --             filling data_bus register or the transaction completion. It is only then, that it continues to TRANSFER2.
  --          WR:
  -- TRANSFER2->
  --          RD:Executes last OR to have a AXI4_DATA_WIDTH worth of requested, unrequested and zeros read data in data_bus.
  --             Loads into the counter the number of bytes to process at TRANSFER3, be it AXI4_DATA_WIDTH + dbits or lower. 
  --             The second case only applies on the last AXI transaction, where the MSB bytes of data_bus may not be requested,
  --             which the counter is then loaded with the LSB ending address computed in stage COMPUTE1.
  --          WR:
  -- TRANSFER3->
  --          RD:Transfers the read data to BM component at dbits steps. The first clock cycle at this stage is used to skip
  --             right unrequested, zeros and already read bytes from previous BM transfers. Following clock cycles, shift
  --             the data to the right at dbits steps, delivering at each clock cycle a dbits worth of data through data_bus.
  --             This is repeated until there is no enough data to deliver. Then, unless it is the last transfer of the whole 
  --             transaction (where a last BM transfer happens to send any data)

  type rd_data_buffer is array (natural range <>) of std_logic_vector( AXI4_DATA_WIDTH - 1 downto 0   );
  type transf_state is (idle, compute1, compute2, compute3, handshake, transfer1, transfer2, transfer3);

  type transfer_rd_operation is record
    state       : transf_state; -- State of the operation
    bm_grant    : std_logic;    -- Grant signals to BM component
    bm_error    : std_logic;    -- Error (at the moment, inside of the manager)

    axi_mode    : std_logic_vector(1 downto 0); -- AXI parameter: burst mode (FIXED, INC, WRAP)
    axi_size    : std_logic_vector(2 downto 0); -- AXI parameter: size mode of each beat in the burst
    axi_len     : std_logic_vector(7 downto 0); -- AXI parameter: number of beats in the burst
    axi_strobe  : std_logic_vector(AXI4_DATA_WIDTH/8 - 1 downto 0); -- What AXI lanes to use at transfer
    axi_valid   : std_logic;                    -- AXI parameter: valid addr/data/control
    axi_ready   : std_logic;                    -- AXI parameter: ready to read data/data has been written
    axi_last    : std_logic;                    -- AXI parameter: last transaction of the burst
    axi_addr    : std_logic_vector(31 downto 0);-- Address register (BM requested at first, then changes to AXI starting address)

    first_beat  : std_logic; -- Flag asserted when computing first beat of the first burst
    two_burst   : std_logic; -- Need to slice transfer in two bursts due to surpassing the 4KB boundary. Flushed after first burst

    bm_size     : std_logic_vector(INT_BURST_WIDTH - 1 downto 0); -- Original size being requested by BM component, and then first burst size
    rem_size    : std_logic_vector(INT_BURST_WIDTH - 2 downto 0); -- Remaining size for second burst (4KB outbounds access)

    axi_data_tmp: std_logic_vector(AXI4_DATA_WIDTH - 1 downto 0); -- AXI data bus register used to separate AXI network from interface.
    axi_data_bus: rd_data_buffer(rd_n_buffer_regs  - 1 downto 0); -- AXI data bus registers which filters narrow reads from data_tmp.
    buffer_index: std_logic_vector(log_2(rd_n_buffer_regs) - 1 downto 0); -- Unsigned index of axi_data_bus registers.
    buffer_full : std_logic_vector(rd_n_buffer_regs- 1 downto 0); -- When asserted, the axi_data_bus(buffer_index) is full or at a BM transfer.
    data_fbus   : std_logic_vector(AXI4_DATA_WIDTH + dbits - 1 downto 0); -- Full AXI+BM data bus used to BM data transfer.
    start_strb  : std_logic_vector(AXI4_FDATA_BYTE     downto 0); -- Number of right-bytes to skip when reading or writing data_bus at start of BM transfer.
    end_strb    : std_logic_vector(AXI4_FDATA_BYTE     downto 0); -- Number of bytes to read from data_bus at the last BM transfer.
    counter_axi : std_logic_vector(AXI4_FDATA_BYTE     downto 0); -- Used to count the number of bytes transfered/discarded from AXI or BM.
    counter_bm  : std_logic_vector(AXI4_FDATA_BYTE     downto 0); -- Used to count the number of bytes transfered/discarded from AXI or BM.
    bm_mask     : std_logic_vector(dbits/8 - 1 downto 0);         -- What dbits byte lanes to use at BM transfer
    bm_valid    : std_logic;                                      -- Valid read data to transfer into BM bus from data_bus(dbits-1 downto 0)
    bm_done     : std_logic;                                      -- Last bm_valid asserted clock cycle of the whole transfer also asserts bm_done.
  end record;

  type transfer_wr_operation is record
    state       : transf_state; -- State of the operation
    bm_grant    : std_logic;    -- Grant signals to BM component
    --error       : std_logic;    -- Error ()

    axi_mode    : std_logic_vector(1 downto 0); -- AXI parameter: burst mode (FIXED, INC, WRAP)
    axi_size    : std_logic_vector(2 downto 0); -- AXI parameter: size mode of each beat in the burst
    axi_len     : std_logic_vector(7 downto 0); -- AXI parameter: number of beats in the burst
    axi_strobe  : std_logic_vector(AXI4_DATA_WIDTH/8 - 1 downto 0); -- What AXI lanes to use at transfer
    axi_valid   : std_logic;                    -- AXI parameter: valid addr/data/control
    axi_ready   : std_logic;                    -- AXI parameter: ready to read data/data has been written
    axi_last    : std_logic;                    -- AXI parameter: last transaction of the burst
    axi_addr    : std_logic_vector(31 downto 0); -- Address register (BM requested, then changes to starting address)

    first_beat  : std_logic; -- Flag asserted when computing first beat of the first burst
    two_burst   : std_logic; -- Need to slice transfer in two bursts due to surpassing the 4KB boundary. Flushed after first burst

    bm_size     : std_logic_vector(INT_BURST_WIDTH - 1 downto 0); -- Original size being requested by BM component, and first burst size
    rem_size    : std_logic_vector(INT_BURST_WIDTH - 2 downto 0); -- Remaining size for second burst (4KB boundary overflow)

    --data_full   : std_logic; -- Cache is full, do not receive any more data
    data_bus    : std_logic_vector(AXI4_DATA_WIDTH + dbits - 1 downto 0); -- Lanes register to transfer between AXI and BM data buses
    start_strb  : std_logic_vector(AXI4_DATA_BYTE  - 1 downto 0); -- Number of right-bytes to skip when reading or writing data_bus at start of BM transfer.
    end_strb    : std_logic_vector(AXI4_DATA_BYTE  - 1 downto 0); -- Number of bytes to read from data_bus at the last BM transfer.
    counter     : std_logic_vector(AXI4_DATA_BYTE      downto 0); -- Used to count the number of bytes transfered/discarded from AXI or BM.
  end record;

  constant RST_TRANSF_OP : transfer_rd_operation := (
    state       => idle,
    bm_grant    => '1',
    bm_error    => '0',
    axi_mode    => (others => '0'),
    axi_size    => (others => '0'),
    axi_len     => (others => '0'),
    axi_strobe  => (others => '0'),
    axi_valid   => '0',
    axi_ready   => '0',
    axi_last    => '0',
    axi_addr    => (others => '0'),
    first_beat  => '1',
    two_burst   => '0',
    bm_size     => (others => '0'),
    rem_size    => (others => '0'),
    axi_data_tmp=> (others => '0'),
    axi_data_bus=> (others => (others => '0')),
    buffer_index=> (others => '0'),
    buffer_full => (others => '0'),
    data_fbus   => (others => '0'),
    start_strb  => (others => '0'),
    end_strb    => (others => '0'),
    counter_axi => (others => '0'),
    counter_bm  => (others => '0'),
    bm_mask     => (others => '0'),
    bm_valid    => '0',
    bm_done     => '0'
  );

  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------

  -- Registers for write/read control
  signal wr : transfer_wr_operation;
  signal rd : transfer_rd_operation;

  -- Buffer signals for AXI transfer at read transactions
  signal rd_AXI_bmask_tmp     : unsigned(AXI4_DATA_WIDTH/8-1 downto 0);
  signal rd_AXI_valid_delayed : std_logic;
  -- Buffer signals for BM transfer at read transactions
  signal rd_bm_data_delayed   : std_logic_vector(dbits - 1 downto 0);
  signal rd_bm_valid_delayed  : std_logic;
  signal rd_bm_done_delayed   : std_logic;

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
    size_axi  : out std_logic_vector(2 downto 0); -- Size mode to set
    addr_axi  : out std_logic_vector(AXI4_DATA_BYTE  - 1 downto 0)  -- LSB Address to request at AXI
  ) is
    variable n      : std_logic_vector(0 to 6); -- 128, 64, 32, 16, 8, 4 and 2 byte size modes flags
    variable addr1  : std_logic_vector(6 downto 0) := (others => '0');
    variable addr2  : std_logic_vector(6 downto 0) := (others => '0');
  begin
    -- Check if size is greater or equal than 128, 64, 32, 16, 8, 4, 2, while also
    -- taking into account if AXI4_DATA_WIDTH and MAX_SIZE_BURST allow it.
    n(0) := or_vector(size_bm(size_bm'length-1 downto 6));  -- 128 bytes
    n(1) := ( size_bm(5) or n(0) );                         -- 64 bytes
    n(2) := ( size_bm(4) or n(1) );                         -- 32 bytes
    n(3) := ( size_bm(3) or n(2) );                         -- 16 bytes
    n(4) := ( size_bm(2) or n(3) );                         --  8 bytes
    n(5) := ( size_bm(1) or n(4) );                         --  4 bytes
    n(6) := ( size_bm(0) or n(5) );                         --  2 bytes

    addr1(addr_bm'range) := addr_bm;

    -- Set the AXI mode size as: AXI mode to apply >= bytes to BM transfer > next lower power of 2.
    -- For example, for a BM transfer of 16 bytes (bm_in.rd_size = 15), size_axi is b'100 (16 bytes).
    -- For a transfer of 15 bytes is still b'100, while for a transfer of 17 bytes is b'101 (32 bytes).
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


  -- To decide the number of beats at the burst, the number of bytes requested by the BM component 
  -- is used, by applying a right shift to it a number equal to the AXI's size mode (the actual 
  -- codification, no it's actual size meaning). In addition, size values between power of 2s and 
  -- multiple of 128 require a +1 to compute the correct number of beats required to execute.
  --
  -- However, the AXI's codification of the number of beats is the actual number of beats -1. So
  -- at the end, the actual calculation subtracts 1 to those that are equal to a power of 2 or 
  -- multiple of 128. This is checked by an OR that affects depending the size_mode being set.

  procedure decide_len(
    size      : in  std_logic_vector(INT_BURST_WIDTH - 1 downto 0); -- Transfer size requested by BM
    size_mode : in  std_logic_vector(2 downto 0); -- AXI size mode being set
    burst_len : out std_logic_vector(7 downto 0)  -- AXI burst length (actual num of beats needs +1)
    ) is
      variable len_temp : std_logic_vector(size'range); -- Temp var because VHDL is really strong /typed/
      variable len      : std_logic_vector(8 downto 0); -- The max number of beats allowed at AXI4 is 256
      variable one      : std_logic_vector(7 downto 0); -- Vector identifying if size is not multiple of...
  begin
    -- Number of beats required = size requested >> size_mode (-1 if size = size_array(size_mode) or multiple)
    len_temp  := std_logic_vector(shift_right( unsigned(size), to_integer(unsigned(size_mode)) ));
    len       := len_temp(len'range);

    -- However, sizes equal to the AXI size mode being applied (the decoded value) must decrease once its len value.
    one(0) := size(0);                                      -- Size is   1 byte  when asserted
    one(1) := size(1) and not(size(0));                     -- Size is   2 bytes when asserted
    one(2) := size(2) and not(or_vector(size(1 downto 0))); -- Size is   4 bytes when asserted
    one(3) := size(3) and not(or_vector(size(2 downto 0))); -- Size is   8 bytes when asserted
    one(4) := size(4) and not(or_vector(size(3 downto 0))); -- Size is  16 bytes when asserted
    one(5) := size(5) and not(or_vector(size(4 downto 0))); -- Size is  32 bytes when asserted
    one(6) := size(6) and not(or_vector(size(5 downto 0))); -- Size is  64 bytes when asserted
    one(7) := size(7) and not(or_vector(size(6 downto 0))); -- Size is 128 bytes when asserted

    burst_len := sub_vector(len, '0' & one(to_integer(unsigned(size_mode))), burst_len'length);
  end procedure decide_len;


  -- Sets the strobe signal to ones with a number of right-zeros equal to the value encoded on byte_strb.

  procedure decide_strobe(
    variable byte_strb  : in  std_logic_vector(wr.start_strb'range);
    variable axi_strb   : out std_logic_vector(wr.axi_strobe'range)
    ) is
      variable strb : std_logic_vector(wr.axi_strobe'range) := (others => '1');
  begin
    strb      := std_logic_vector(shift_left( unsigned(strb), to_integer(unsigned(byte_strb)) ));
    axi_strb  := strb;
  end procedure decide_strobe;

  
begin -- rtl

  -----------------
  -- Assignments --
  -----------------
  
  -- Advance eXtensible Interface (interconnect bus)
    -- Write address channel out
  axi4mo.aw_id        <= std_logic_vector(to_unsigned(axi_id, axi4mo.aw_id'length));
  --axi4mo.aw_addr      <= wr.axi_addr when wr.axi_valid = '1' else (others => '0'); -- Address
  --axi4mo.aw_region    <= (others => '0');
  --axi4mo.aw_len       <= wr.axi_len  when wr.axi_valid = '1' else (others => '0'); -- Number of beats
  --axi4mo.aw_size      <= wr.axi_size when wr.axi_valid = '1' else (others => '0'); -- Beat size
  --axi4mo.aw_burst     <= wr.axi_mode when wr.axi_valid = '1' else (others => '0'); -- Burst mode
  --axi4mo.aw_lock      <= (others => '0');
  --axi4mo.aw_cache     <= (others => '0');
  --axi4mo.aw_prot      <= (others => '0'); -- Required
  --axi4mo.aw_qos       <= (others => '0');
  --axi4mo.aw_valid     <= ;
    -- Write data channel out
  --axi4mo.w_data       <= ;
  --axi4mo.w_strb       <= ;
  --axi4mo.w_last       <= ;
  --axi4mo.w_valid      <= ;
    -- Write response channel out
  --axi4mo.b_ready      <= ;
    -- Read address channel out
  axi4mo.ar_id        <= std_logic_vector(to_unsigned(axi_id, axi4mo.aw_id'length));
  axi4mo.ar_addr      <= rd.axi_addr when rd.axi_valid = '1' else (others => '0'); -- Address
  axi4mo.ar_region    <= (others => '0');
  axi4mo.ar_len       <= rd.axi_len  when rd.axi_valid = '1' else (others => '0'); -- Number of beats
  axi4mo.ar_size      <= rd.axi_size when rd.axi_valid = '1' else (others => '0'); -- Beat size
  axi4mo.ar_burst     <= rd.axi_mode when rd.axi_valid = '1' else "01"; -- Burst mode
  axi4mo.ar_lock      <= '0';
  axi4mo.ar_cache     <= (others => '0');
  axi4mo.ar_prot      <= (others => '0');
  axi4mo.ar_qos       <= (others => '0');
  axi4mo.ar_valid     <= rd.axi_valid;
    -- Read data channel out
  axi4mo.r_ready      <= rd.axi_ready;
    -- Write address channel in
  --axi4mi.aw_ready     <= axi4mi.aw_ready;
  --  -- Write data channel in
  --axi4mi.w_ready      <= axi4mi.w_ready;
  --  -- Write response channel in
  --axi4mi.b_id         <= axi4mi.b_id;
  --axi4mi.b_resp       <= axi4mi.b_resp;
  --axi4mi.b_valid      <= axi4mi.b_valid;
  --  -- Read address channel in
  --axi4mi.ar_ready     <= axi4mi.ar_ready;
  --  -- Read data channel in
  --axi4mi.r_id         <= axi4mi.r_id;
  --axi4mi.r_data       <= axi4mi.r_data;
  --axi4mi.r_resp       <= axi4mi.r_resp;
  --axi4mi.r_last       <= axi4mi.r_last;
  --axi4mi.r_valid      <= axi4mi.r_valid;

  -- Bus Master (injector)
  -- Write channel
  --bm_in.wr_addr;
  --bm_in.wr_size;
  --bm_in.wr_req;
  --bm_out.wr_req_grant;
  --bm_in.wr_data(127 downto 128 - dbits);
  --bm_out.wr_full;
  --bm_out.wr_done;
  --bm_out.wr_err;

  -- Read channel
  --bm_in.rd_addr;  -- used because input
  --bm_in.rd_size;  -- used because input
  --bm_in.rd_req;   -- used because input
  bm_out.rd_req_grant <= rd.bm_grant;
  bm_out.rd_data      <= (rd_bm_data_delayed & (127-dbits downto 0 => '0'));
  bm_out.rd_valid     <= rd_bm_valid_delayed;
  bm_out.rd_done      <= rd_bm_done_delayed;
  bm_out.rd_err       <= rd.bm_error;

  
  -----------------------------------------------------------------------------
  -- Sequential process
  -----------------------------------------------------------------------------

  -- READ PROCESS
  read_proc : process (clk, rstn) -- Variables used as connections between combinational logic, functions and registers.
    variable addr_end   : std_logic_vector( 12                   downto 0 );-- max end address LSB at INC mode (4kB check)
    variable addr_strt  : std_logic_vector( AXI4_DATA_BYTE-1     downto 0 );-- AXI LSB starting address
    variable unsg_skip  : std_logic_vector( AXI4_FDATA_BYTE      downto 0 );-- unsigned number of bytes to skip
    variable axi_size   : std_logic_vector( rd.axi_size'range             );-- AXI size mode
    variable axi_len    : std_logic_vector( rd.axi_len'range              );-- AXI burst length
    variable rd_next_AXI_counter  : std_logic_vector(rd.counter_axi'range );-- Next AXI counter
    variable rd_next_index        : std_logic_vector(rd.buffer_index'range);-- Next buffer index
    variable rd_next_BM_counter   : std_logic_vector(rd.counter_bm'range  );-- Next BM counter
    variable rd_data_empty        : boolean;                                -- data_bus emptied flag
    variable data_bm    : std_logic_vector( rd.data_fbus'range            );-- Shifted full bus data for BM transfer
  begin
    if (rstn = '0' and ASYNC_RST) then
      rd                    <= RST_TRANSF_OP;
      rd_AXI_valid_delayed  <= '0';
    elsif rising_edge(clk) then
      if (rstn = '0') then
        rd                  <= RST_TRANSF_OP;
        rd_AXI_valid_delayed<= '0';
      else
        rd_AXI_valid_delayed<= '0'; -- By default, do not compute an AXI read data shift.
        case rd.state is
          when idle => -- Worst delay path: BM component output
            -- Reset registers
            rd              <= RST_TRANSF_OP;
          
            -- Load request information from BM
            if (bm_in.rd_req = '1' and rd.bm_grant = '1') then
              rd.bm_grant   <= '0';           -- Deassert granting requests for BM component
              rd.axi_addr   <= bm_in.rd_addr; -- Load starting address
              rd.bm_size    <= bm_in.rd_size; -- Load BM size to transfer (real is +1)
              -- Next, check 4kB out of bounds access
              rd.state      <= compute1;
            end if;
          
          
          when compute1 => -- Worst delay path: ADD 11+12, SUB 13-12
            -- Check if transaction will access two 4KB address regions
            addr_end        := add_vector(rd.bm_size, rd.axi_addr(11 downto 0), addr_end'length);
            rd.two_burst    <= addr_end(12);
          
            -- If transaction must be split in two bursts, due to 4KB overflow boundary, calculate...
            if(addr_end(12) = '1') then
              -- first burst size (-1 from real size)  MAX=4095, MIN=0
              rd.bm_size    <= sub_vector(x"FFF", rd.axi_addr(11 downto 0), rd.bm_size'length);
              -- second burst size (-1 from real size) MAX=4094, MIN=0
              rd.rem_size   <= sub_vector(addr_end, x"FFE", rd.rem_size'length);
            end if;
          
            -- Number of bytes to transfer at the last BM transfer = LSB of last address + 1 + BM data bytes width
            rd.end_strb     <= add_vector(addr_end(AXI4_DATA_BYTE-1 downto 0), 1 + dbits/8, rd.end_strb'length);
          
            -- Next, compute size mode
            rd.state        <= compute2;
          
          
          when compute2 => -- Worst delay path: DECIDE_SIZE
            -- Decide AXI size mode and update starting address with aligned address to that size mode
            decide_size(rd.bm_size, rd.axi_addr(AXI4_DATA_BYTE-1 downto 0), axi_size, addr_strt);
            rd.axi_size     <= axi_size;
            rd.axi_addr(AXI4_DATA_BYTE-1 downto 0) <= addr_strt;
            rd.bm_size      <= add_vector(rd.bm_size, 1, rd.bm_size'length); -- Update size to be real size.
          
            if(rd.first_beat = '1') then
              -- Number of right-bytes to discard unrequested data at first BM transfer = LSB BM unaligned address + BM data bytes width
              rd.start_strb <= add_vector(rd.axi_addr(AXI4_DATA_BYTE-1 downto 0), dbits/8, rd.start_strb'length);
              rd.first_beat <= '0';
              -- On second burst, the number of right-bytes to discard at first BM transfer = rd.start_strb from previous burst
            end if;
          
            -- Next compute state
            rd.state        <= compute3;
          
          
          when compute3 => -- Worst delay path: DECIDE_LEN(MUX 8to1, ORx6, SUB 9-1)
            -- Compute how many beats will be necessary to transfer due to axi_size mode
            decide_len(rd.bm_size, rd.axi_size, axi_len);
            rd.axi_len      <= axi_len;
          
            -- Set the burst transfer mode
            rd.axi_mode     <= INC;
          
            -- Next, the handshake step
            rd.axi_valid    <= '1'; -- Request AXI read
            rd.state        <= handshake;
          
          
          when handshake =>
            -- Having all computation steps and proper registration separated allows maximum 
            -- frequency of operation when including this AXI master interface at the interconnect
            -- bus. For a read transaction, only axi_addr, axi_mode, axi_size and axi_len are sent.
            if (axi4mi.ar_ready = '1') then
              rd.axi_valid  <= '0'; -- At request being granted, deassert request
              rd.axi_ready  <= '1'; -- Read data from AXI bus flag
            
              -- Initilize counter with the byte lane it is starting the AXI read operation
              rd.counter_axi   <= (rd.counter_axi'length-AXI4_DATA_BYTE-1 downto 0 => '0') & rd.axi_addr(AXI4_DATA_BYTE-1 downto 0);

              -- Create the LSB mask to be shifted on transfer1 to generate the byte mask for the AXI read transfer.
              rd_AXI_bmask_tmp <= unsigned(mask_array(to_integer(unsigned(rd.axi_size)))(rd_AXI_bmask_tmp'range));
            
              -- Next, beat transfers
              rd.state      <= transfer1;
            end if;
          
          
          when transfer1 => -- AXI transfer --
          -- The AXI transfer occurs by buffering the read data onto the axi_data_tmp register when the subordinate indicates that its valid data.
          -- Then, the axi_data_tmp is filtered onto the axi_data_bus register, which will be used at the BM transfer. However, due to this single 
          -- clock cycle delay between AXI read and data filtering, both valid and the mask signals are also buffered on the "delayed" registers. 
          -- The counter on this step is used to pause (data_bus full) and to know what byte lanes should read from setting the mask rd.axi_strobe.

              -- Check if subordinate is delivering valid data with the same AXI ID as requested.
            if (axi4mi.r_valid = '1' and axi4mi.r_id = std_logic_vector(to_unsigned(axi_id, axi4mi.r_id'length))) then
              -- Likewise, data register is separated from further computation.
              rd.axi_data_tmp     <= axi4mi.r_data;
              rd.axi_last         <= axi4mi.r_last;
            
              -- Count how many bytes are being read into axi_data_bus. Increase the counter in AXI size mode steps.
              rd_next_AXI_counter := add_vector(rd.counter_axi, size_array(to_integer(unsigned(rd.axi_size))), rd.counter_axi'length);
              rd.counter_axi      <= rd_next_AXI_counter;

              -- Using the actual counter, set the mask to filter the byte lanes to read from at the following clock cycle.
              rd.axi_strobe  <= std_logic_vector(shift_left(rd_AXI_bmask_tmp, to_integer(unsigned(rd.counter_axi))));
            
              -- Be it the last beat of the burst or axi_data_bus register is full, pause AXI transfer and proceed to BM transfer.
              if(axi4mi.r_last = '1' or rd_next_AXI_counter(AXI4_DATA_BYTE) = '1') then 
                -- Next, the BM transfer step
                rd.state      <= transfer2;
                rd.axi_ready  <= '0'; -- Pause or finish AXI transfer
              end if;

              -- rd.counter should always be lower or equal than the number of bytes the AXI data bus has, at least during an AXI read. 
              -- In case something goes wild and it's higher due to an internal error of the manager, output a read error pulse.
            elsif(rd.counter_axi > std_logic_vector(to_unsigned(AXI4_DATA_WIDTH/8, rd.counter_axi'length))) then
              rd.bm_error   <= '1';
              rd.state      <= idle;
            end if;


          when transfer2 => -- Single clock cycle delay to execute the last read from rd.data_tmp -> rd.axi_data_bus --
            -- Increment index to change the register to store the next AXI read and mark the actual one as full to BM transfer.
            rd_next_index   := add_vector(rd.buffer_index, 1, rd.buffer_index'length); -- Overflows are allowed and expected.
            rd.buffer_index <= rd_next_index;
            rd.buffer_full(to_integer(unsigned(rd.buffer_index))) <= '1';

            if(rd.axi_last = '1') then      -- Check if is the last AXI beat of the burst.
              if(or_vector(rd.buffer_full) = '0') then -- Check if all BM transfers have been done.
                if(rd.two_burst = '1') then -- If it is, check if there is another burst.
                  rd.state    <= compute2;  -- If there is, compute the data for the handshake of the burst.
                else
                  rd.state    <= idle;      -- If the transaction has been completed, return to idle.
                end if;
              end if;
            else  -- In case it is not the last AXI beat, check if the next buffer register can be used for another read beat.
              if( rd.buffer_full(to_integer(unsigned(rd_next_index))) = '0' ) then
                rd.axi_ready  <= '1';       -- If there's empty registers in the buffer, use them to read another beat.
                rd.counter_axi<= (others => '0');
                rd.state      <= transfer1;
              end if;
            end if;


            -- REWORK FROM HERE

            -- Initialize counter with the number of bytes to transfer to BM data bus at...
            if(rd.two_burst = '0' and rd.axi_last = '1') then   -- the last AXI transfer of the whole transaction.
              rd.counter_bm   <= rd.end_strb; -- (only last bytes left to read + dbits/8)
            else -- any other time.
              rd.counter_bm   <= std_logic_vector(to_unsigned(AXI4_DATA_WIDTH/8 + dbits/8, rd.counter_bm'length)); -- (whole data_bus register)
            end if;
            rd.bm_mask        <= (others => '1');
            
          
          
          when transfer3 => -- BM transfer --
            -- Check if this is the last BM transfer before data_bus is depleted for a dbits transfer.
            rd_data_empty     := rd.counter_bm < std_logic_vector(to_unsigned(2*dbits/8, rd.counter_bm'length));
            -- At the start of a BM transfer (bm_valid is 0), discard unrequested or already read data by...
            if(rd.bm_valid = '0') then
              unsg_skip       := rd.start_strb; -- setting a shift equal to rd.start_strb.
              rd.bm_valid     <= '1';           -- Also, execute BM transfer starting next clock cycle.
            else
              if(rd_data_empty) then
                if(rd.axi_last = '0' or (rd.axi_last = '1' and rd.two_burst = '1')) then
                  unsg_skip   := sub_vector(rd.counter_bm, dbits/8, unsg_skip'length);
                else
                  unsg_skip   := rd.counter_bm(unsg_skip'range);
                end if;
              else
                unsg_skip     := std_logic_vector(to_unsigned(dbits/8, unsg_skip'length));
              end if;
            end if;

            -- During subsequent BM transfers, shift to transfer dbits if the data left to shift is long enough.
            if(rd_data_empty) then -- If there is not enough data, check if it's the last AXI transaction of the actual burst...
              if(rd.axi_last = '1') then -- at affirmative case, check if there will be another AXI burst...
                if(rd.two_burst = '1') then -- if there is, set the request size for the next AXI burst, the address,
                  rd.bm_size    <= rd.rem_size; -- deassert the rd.two_burst flag and return to compute2.
                  rd.axi_addr   <= add_vector(rd.axi_addr(31 downto AXI4_DATA_BYTE), 1, 32-AXI4_DATA_BYTE) & (AXI4_DATA_BYTE-1 downto 0 => '0');
                  rd.two_burst  <= '0';
                  rd.state      <= compute2;
                else  -- If this is the last AXI burst of the whole transaction, shift all data left to read to 0
                      -- position, set the mask to only read the requested data, assert bm_done flag and return to idle.
                  rd.bm_done  <= '1'; 
                  rd.state    <= idle;
                end if;
              else
                -- If it's not the last transfer of the AXI burst, this BM transfer will be the one that depletes rd.data_fbus, requiring an
                -- additional AXI beat. Thus, the leftover data must be kept between dbits-1 and 0 to maintain it for the next BM transfer
                rd.bm_valid   <= '0';
                rd.state      <= transfer1; -- and return to transfer1.
              end if;
            end if;
          
            -- Update counter decreasing the bytes that have been read or skipped.
            rd_next_BM_counter  := sub_vector(rd.counter_bm, unsg_skip, rd.counter_bm'length);
            if(rd_data_empty and rd.axi_last = '0') then
              rd.counter_bm     <= (others => '0');
            else
              rd.counter_bm     <= rd_next_BM_counter;
            end if;
            
            -- Save the number of bytes that have been already read, to skip on next starting BM transfers while keeping unread data.
            rd.start_strb     <= sub_vector(dbits/8, unsg_skip, rd.start_strb'length);
          
            -- Shift number of bytes.
            data_bm           := std_logic_vector(shift_right(unsigned(rd.data_fbus), to_integer(unsigned(unsg_skip))*8));
            rd.data_fbus      <= data_bm;
            
          when others =>

        end case;



        -- During each AXI data read, filter the data to only read the expected byte lanes at that clock cycle.
        if(rd_AXI_valid_delayed = '1') then
          for k in rd.axi_strobe'range loop
            rd.axi_data_bus(to_integer(unsigned(rd.buffer_index)))(k*8+7 downto k*8) 
              <= rd.axi_data_tmp(k*8+7 downto k*8) and (7 downto 0 => rd.axi_strobe(k));
          end loop;
        end if;

        -- During a BM data transfer at a read transaction, set the mask for the last transfer.
        for k in rd.bm_mask'range loop
          if(rd.counter_bm > std_logic_vector(to_unsigned(k, rd.counter_bm'length))) then
            rd.bm_mask(k) <= '1';
          else 
            rd.bm_mask(k) <= '0';
          end if;
        end loop;

        -- Registration of delayed signals for read transactions
        rd_AXI_valid_delayed<= axi4mi.r_valid;
        rd_bm_valid_delayed <= rd.bm_valid;
        rd_bm_done_delayed  <= rd.bm_done;
        for k in dbits/8 - 1 downto 0 loop
          rd_bm_data_delayed(8*k+7 downto 8*k) <= rd.data_fbus(8*k+7 downto 8*k) and (7 downto 0 => rd.bm_mask(k));
        end loop;


      end if;
    end if;
  end process read_proc;


  -----------------------------------------------------------------------------
  -- Combinational process
  -----------------------------------------------------------------------------


end architecture rtl;
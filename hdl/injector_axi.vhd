-----------------------------------------------------------------------------   
-- Entity:      injector_axi
-- File:        injector_axi.vhd
-- Author:      Francis Fuentes
-- Description: injector top level entity.
------------------------------------------------------------------------------ 
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library bsc;
use bsc.injector_pkg.all;

-----------------------------------------------------------------------------
-- Top level entity for injector.
-- This is a wrapper which integrates injector core to the
-- AXI4 master - generic bus master bridge
--
-- This AXI4 master interface does not allow to have an BM bus wider than the AXI bus
-- (AXI4_DATA_WIDTH must be greater or equal to dbits). It may not tolerate transactions
-- of a width higher than the MAX_SIZE_BEAT.
-----------------------------------------------------------------------------

entity injector_axi is
  generic (
    tech          : integer range 0 to numTech          := typeTech; -- Target technology
    -- APB configuration  
    pindex        : integer                             := 0;       -- APB configuartion slave index
    paddr         : integer                             := 0;       -- APB configuartion slave address
    pmask         : integer                             := 16#FFF#; -- APB configuartion slave mask
    pirq          : integer range 0 to APB_IRQ_NMAX - 1 := 1;       -- APB configuartion slave irq
    -- Bus master configuration
    dbits         : integer range 32 to  128            := 32;      -- Data width of BM and FIFO (must be a power of 2)
    -- AXI Master configuration
    axi_id        : integer                             := 0;       -- AXI master index
    MAX_SIZE_BEAT : integer range 64 to 4096            := 1024;    -- Maximum size of a beat at a burst transaction.
    -- Injector configuration
    ASYNC_RST     : boolean                              := FALSE -- Allow asynchronous reset flag
  );
  port (
    rstn          : in  std_ulogic;         -- Reset
    clk           : in  std_ulogic;         -- Clock
    -- APB interface signals
    apbi          : in  apb_slave_in_type;  -- APB slave input
    apbo          : out apb_slave_out_type; -- APB slave output
    -- AXI interface signals
    axi4mi        : in  axi4_in_type;       -- AXI4 master input 
    axi4mo        : out axi4_out_type       -- AXI4 master output
  );
end entity injector_axi;

architecture rtl of injector_axi is
  -----------------------------------------------------------------------------
  -- Constant declaration
  -----------------------------------------------------------------------------

  -- Constants that should be equal to what is written at injector_pkg.vhd
  constant INT_BURST_WIDTH  : integer := log_2(MAX_SIZE_BEAT) + 1; -- Width to hold maximum beat size number
  constant AXI4_DATA_WIDTH  : integer := axi4mo.w_data'length;     -- AXI data bus width

  -- AXI constants
  constant FIXED            : std_logic_vector(1 downto 0) := "00"; -- AXI burst modes: FIXED
  constant INC              : std_logic_vector(1 downto 0) := "01"; --                  INCREMENTAL
  constant WRAP             : std_logic_vector(1 downto 0) := "10"; --                  WRAP
  constant size_array       : array_integer(0 to 7) := (1, 2, 4, 8, 16, 32, 64, 128); -- AXI size transfer modes
  constant size_mode_appl   : std_logic_vector(0 to 6) := (
    -- Generate an array indicating what size modes are applicable due to the constrains of
    -- both the AXI and BM bus widths implementation (are constants) and the max size beat.
    0 => to_std_logic( dbits >= 128 and AXI4_DATA_WIDTH >= 128 and MAX_SIZE_BEAT >= 128 ),
    1 => to_std_logic( dbits >=  64 and AXI4_DATA_WIDTH >=  64 and MAX_SIZE_BEAT >=  64 ),
    2 => to_std_logic( dbits >=  32 and AXI4_DATA_WIDTH >=  32 and MAX_SIZE_BEAT >=  32 ), -- These should be unnecessary
    3 => to_std_logic( dbits >=  16 and AXI4_DATA_WIDTH >=  16 and MAX_SIZE_BEAT >=  16 ), -- These should be unnecessary
    4 => to_std_logic( dbits >=   8 and AXI4_DATA_WIDTH >=   8 and MAX_SIZE_BEAT >=   8 ), -- These should be unnecessary
    5 => to_std_logic( dbits >=   4 and AXI4_DATA_WIDTH >=   4 and MAX_SIZE_BEAT >=   4 ), -- These should be unnecessary
    6 => to_std_logic( dbits >=   2 and AXI4_DATA_WIDTH >=   2 and MAX_SIZE_BEAT >=   2 )  -- These should be unnecessary
    );

  -----------------------------------------------------------------------------
  -- Records and types
  -----------------------------------------------------------------------------

  -- The interface, when idle, accepts any transaction from the BM bus (write and read), listening to what address and 
  -- size may the operation be. When a request has been granted, this information is processed at the compute1 state, 
  -- where the size is split in two bursts if a 4KB address space overflow is detected. Then
  -- burst mode/size/length and starting address is set for first burst. Here 4KB address space overflow is checked 
  -- (asserting the two_burst flag) and
  -- 
  -- IDLE     -> Accept new requests from BM component.
  -- COMPUTE1 -> Compute if 4KB address space overflow occurs.
  --             In the case it does, generates two burst size cycles.
  -- COMPUTE2 -> Decide AXI size mode for the burst, generate the aligned starting address.
  --             Count how many '0' must be input by the right (31 downto 0) to set at the strobe
  --             for the first transfer of the first burst, written on byte_strb.
  -- COMPUTE3 -> Set burst mode (at the moment, there's only  INC).
  --             Decide burst length (number of beats) taking into account the size mode.
  --          WR:Set the strobe bits for WRITE transaction. (READ uses the byte_strb value)
  --          RD:Assert "ARVALID" since all required information is now registered.
  -- HANDSHAKE-> 
  --          RD:Checks for "RREADY" to load at a register de data bus directly.
  -- TRANSFER1->
  --          RD:Shifts the loaded data to drop the bytes not in strobe using byte_strb.
  --          WR:

  type transf_state is (idle, compute1, compute2, compute3, handshake, transfer);

  type transfer_operation is record
    state       : transf_state; -- State of the operation
    grant       : std_logic;    -- Grant signals to BM component
    error       : std_logic;    -- Error ()

    axi_mode    : std_logic_vector(1 downto 0); -- AXI parameter: burst mode (FIXED, INC, WRAP)
    axi_size    : std_logic_vector(2 downto 0); -- AXI parameter: size mode of each beat in the burst
    axi_len     : std_logic_vector(7 downto 0); -- AXI parameter: number of beats in the burst
    axi_strobe  : std_logic_vector(AXI4_DATA_WIDTH/8 - 1 downto 0); -- What AXI lanes to use at transfer
    axi_valid   : std_logic;                    -- AXI parameter: valid addr/data/control
    axi_ready   : std_logic;                    -- AXI parameter: ready to read data/data has been written
    axi_last    : std_logic;                    -- AXI parameter: last transaction of the burst
    axi_addr    : std_logic_vector(31 downto 0); -- Address register (BM requested, then changes to starting address)

    --addr_curr   : std_logic_vector(31 downto 0); -- Starting address
    first_burst : std_logic; -- Flag asserted when computing first burst
    two_burst   : std_logic; -- Need to slice transfer in two bursts due to surpassing the 4KB boundary. Flushed after first burst

    bm_size     : std_logic_vector(INT_BURST_WIDTH - 1 downto 0); -- Original size being requested by BM component
    curr_size   : std_logic_vector(INT_BURST_WIDTH - 1 downto 0); -- Remaining size for actual burst (first, then second)
    rem_size    : std_logic_vector(INT_BURST_WIDTH - 2 downto 0); -- Remaining size for second burst (4KB boundary overflow)

    data_full   : std_logic; -- Cache is full, do not receive any more data
    data_bus    : std_logic_vector(AXI4_DATA_WIDTH - 1 downto 0); -- Lanes register
    byte_strobe : std_logic_vector( 6 downto 0); -- unsigned value of how many bytes to left unnused at first transfer
  end record;

  constant RST_TRANSF_OP : transfer_operation := (
    state       => idle,
    grant       => '0',
    error       => '0',
    axi_mode    => (others => '0'),
    axi_size    => (others => '0'),
    axi_len     => (others => '0'),
    axi_strobe  => (others => '0'),
    axi_valid   => '0',
    axi_ready   => '0',
    axi_last    => '0',
    axi_addr    => (others => '0'),
    --addr_curr   => (others => '0'),
    first_burst => '1',
    two_burst   => '0',
    bm_size     => (others => '0'),
    curr_size   => (others => '0'),
    rem_size    => (others => '0'),
    data_full   => '0',
    data_bus    => (others => '0'),
    byte_strobe => (others => '0')
  );

  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------

  -- I/O Injector and AXI interface
  signal bm_in   : bm_in_type;
  signal bm_out  : bm_out_type;
  signal axi_in  : axi4_in_type;
  --signal axi_out : axi4_out_type;

  -- Signals/registers to manage things
  signal rd_dbits : std_logic_vector(dbits - 1 downto 0); -- Used to addapt the 128bit bm.rd_data bus

  -- Registers for write/read control
  signal wr : transfer_operation;
  signal rd : transfer_operation;

  -----------------------------------------------------------------------------
  -- Function/procedure declaration
  -----------------------------------------------------------------------------

  -- The procedure decide_size sets the AXI size mode to be equal or the next lower power of 2
  -- respect the size being requested by the BM component. This way, transfers between powers of
  -- 2 will require more than a single transfer if the size is not equal to a power of 2. 
  -- A single transfer could be achieved by requesting the next high power of 2 and using the
  -- strobe logic to only transfer the correct bytes, but the logic of this procedure would be 
  -- more complicated, decreasing the frequency of operation or requiring an additional
  -- computing step previous to the transfer, which would lower performance anyways. Also, 
  -- great part could be done with a loop, but it's for the best to do it manually if it can be 
  -- done, just to not find yourself with long synthesis runs due to loops. 
  --
  -- The particular widths of the addresses come from how much the size mode can modify the 
  -- address requested by BM, in order to align it with the size mode, leaving a starting address
  -- always equal or lower. Since the max size mode is 128, only 7 address bits are required.
  -- 
  -- In addition, the selection of the size mode is also constrained by the widths of the BM 
  -- component (dbits) and AXI data bus (AXI4_DATA_WIDTH), which is managed by the size_mode_appl 
  -- constant vector.
  procedure decide_size(
    size_bm   : in  std_logic_vector(INT_BURST_WIDTH - 1 downto 0); -- Size requested by BM
    addr_bm   : in  std_logic_vector(6 downto 0); -- LSB Address being requested by BM
    size_axi  : out std_logic_vector(2 downto 0); -- Size mode to set
    addr_axi  : out std_logic_vector(6 downto 0)  -- LSB Address to request at AXI
  ) is
    variable n : std_logic_vector(0 to 6); -- 128, 64, 32, 16, 8, 4 and 2 byte size modes flags
  begin
    -- Check if size is greater or equal than 128, 64, 32, 16, 8, 4, 2, while also
    -- tacking into account if dbits, AXI4_DATA_WIDTH, MAX_SIZE_BEAT allow it.
    n(0) := or_vector(size_bm(size_bm'length-1 downto 7)) and size_mode_appl(0); -- 128 bytes
    n(1) := ( size_bm(6) or n(0) ) and size_mode_appl(1); -- 64 bytes
    n(2) := ( size_bm(5) or n(1) ) and size_mode_appl(2); -- 32 bytes
    n(3) := ( size_bm(4) or n(2) ) and size_mode_appl(3); -- 16 bytes
    n(4) := ( size_bm(3) or n(3) ) and size_mode_appl(4); --  8 bytes
    n(5) := ( size_bm(2) or n(4) ) and size_mode_appl(5); --  4 bytes
    n(6) := ( size_bm(1) or n(5) ) and size_mode_appl(6); --  2 bytes

    if   (n(0) = '1') then
      size_axi := "111"; -- 128 bytes size mode
      addr_axi := "000" & x"0";
    elsif(n(1) = '1') then
      size_axi := "110"; --  64 bytes size mode
      addr_axi := addr_bm(6) & "00" & x"0";
    elsif(n(2) = '1') then
      size_axi := "101"; --  32 bytes size mode
      addr_axi := addr_bm(6 downto 5) & "0" & x"0";
    elsif(n(3) = '1') then
      size_axi := "100"; --  16 bytes size mode
      addr_axi := addr_bm(6 downto 4) & x"0";
    elsif(n(4) = '1') then
      size_axi := "011"; --   8 bytes size mode
      addr_axi := addr_bm(6 downto 3) & "000";
    elsif(n(5) = '1') then
      size_axi := "010"; --   4 bytes size mode
      addr_axi := addr_bm(6 downto 2) & "00";
    elsif(n(6) = '1') then
      size_axi := "001"; --   2 bytes size mode
      addr_axi := addr_bm(6 downto 1) & "0";
    else
      size_axi := "000"; --   1 byte  size mode
      addr_axi := addr_bm;
    end if;

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

    -- However, non-size AXI mode sizes must
    one(0) := '0';                -- Size not multiple of   1 byte  when asserted
    one(1) := size(0);            -- Size not multiple of   2 bytes when asserted
    one(2) := size(1) or one(1);  -- Size not multiple of   4 bytes when asserted
    one(3) := size(2) or one(2);  -- Size not multiple of   8 bytes when asserted
    one(4) := size(3) or one(3);  -- Size not multiple of  16 bytes when asserted
    one(5) := size(4) or one(4);  -- Size not multiple of  32 bytes when asserted
    one(6) := size(5) or one(5);  -- Size not multiple of  64 bytes when asserted
    one(7) := size(6) or one(6);  -- Size not multiple of 128 bytes when asserted

    burst_len := sub_vector(len, '0' & one(to_integer(unsigned(size_mode))), burst_len'length);
  end procedure decide_len;

  -- Sets the strobe signal to ones with a number of right-zeros equal to the value encoded on byte_strb.
  procedure decide_strobe(
    variable byte_strb  : in  std_logic_vector(rd.byte_strobe'range);
    variable axi_strb   : out std_logic_vector(rd.axi_strobe'range)
    ) is
      variable strb : std_logic_vector(rd.axi_strobe'range) := (others => '1');
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
  axi4mo.aw_id      <= std_logic_vector(to_unsigned(axi_id, axi4mo.aw_id'length));
  --axi4mo.aw_addr    <= wr.axi_addr when wr.axi_valid = '1' else (others => '0'); -- Address
  --axi4mo.aw_region  <= (others => '0');
  --axi4mo.aw_len     <= wr.axi_len  when wr.axi_valid = '1' else (others => '0'); -- Number of beats
  --axi4mo.aw_size    <= wr.axi_size when wr.axi_valid = '1' else (others => '0'); -- Beat size
  --axi4mo.aw_burst   <= wr.axi_mode when wr.axi_valid = '1' else (others => '0'); -- Burst mode
  --axi4mo.aw_lock    <= (others => '0');
  --axi4mo.aw_cache   <= (others => '0');
  --axi4mo.aw_prot    <= (others => '0'); -- Required
  --axi4mo.aw_qos     <= (others => '0');
  --axi4mo.aw_valid   <= ;
    -- Write data channel out
  --axi4mo.w_data     <= ;
  --axi4mo.w_strb     <= ;
  --axi4mo.w_last     <= ;
  --axi4mo.w_valid    <= ;
    -- Write response channel out
  --axi4mo.b_ready    <= ;
    -- Read address channel out
  axi4mo.ar_id      <= std_logic_vector(to_unsigned(axi_id, axi4mo.aw_id'length));
  axi4mo.ar_addr    <= rd.axi_addr when rd.axi_valid = '1' else (others => '0'); -- Address
  --axi4mo.ar_region  <= (others => '0');
  axi4mo.ar_len     <= rd.axi_len  when rd.axi_valid = '1' else (others => '0'); -- Number of beats
  axi4mo.ar_size    <= rd.axi_size when rd.axi_valid = '1' else (others => '0'); -- Beat size
  axi4mo.ar_burst   <= rd.axi_mode when rd.axi_valid = '1' else "01"; -- Burst mode
  --axi4mo.ar_lock    <= (others => '0');
  --axi4mo.ar_cache   <= (others => '0');
  --axi4mo.ar_prot    <= (others => '0'); -- Required
  --axi4mo.ar_qos     <= (others => '0');
  axi4mo.ar_valid        <= rd.axi_valid;
    -- Read data channel out
  axi4mo.r_ready         <= rd.axi_ready;
    -- Write address channel in
  axi_in.aw_ready       <= axi4mi.aw_ready;
    -- Write data channel in
  axi_in.w_ready         <= axi4mi.w_ready;
    -- Write response channel in
  axi_in.b_id            <= axi4mi.b_id;
  axi_in.b_resp          <= axi4mi.b_resp;
  axi_in.b_valid         <= axi4mi.b_valid;
    -- Read address channel in
  axi_in.ar_ready        <= axi4mi.ar_ready;
    -- Read data channel in
  axi_in.r_id            <= axi4mi.r_id;
  axi_in.r_data          <= axi4mi.r_data;
  axi_in.r_resp          <= axi4mi.r_resp;
  axi_in.r_last          <= axi4mi.r_last;
  axi_in.r_valid         <= axi4mi.r_valid;

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
  --bm_in.rd_addr;
  --bm_in.rd_size;
  --bm_in.rd_req;
  bm_out.rd_req_grant <= rd.grant;
  bm_out.rd_data      <= (rd_dbits & (128-dbits downto 0 => '0'));
  --bm_out.rd_valid   <= ;
  --bm_out.rd_done <= rd.;
  --bm_out.rd_err <= rd.;


  -----------------------------------------------------------------------------
  -- Sequential process
  -----------------------------------------------------------------------------

  -- READ PROCESS
  read_proc : process (clk, rstn)
    variable addr_end  : std_logic_vector(12 downto 0); -- max end address LSB at INC mode
    variable addr_strt : std_logic_vector( 6 downto 0); -- AXI LSB starting address
    variable byte_strb : std_logic_vector( rd.byte_strobe'range ); -- byte strobe
    variable axi_size  : std_logic_vector( rd.axi_size'range    ); -- AXI size mode
    variable axi_len   : std_logic_vector( rd.axi_len'range     ); -- AXI burst length
    variable axi_strb  : std_logic_vector( rd.axi_strobe'range  ); -- AXI strobe
  begin
    if (rstn = '0') then
      rd       <= RST_TRANSF_OP;
      rd_dbits <= (others => '0');

    elsif (clk = '1' and clk'event) then
      case rd.state is
        when idle => -- Worst delay path: BM component output
          -- Reset registers
          rd       <= RST_TRANSF_OP;
          rd_dbits <= (others => '0');

          -- Load request information from BM
          if (bm_in.rd_req = '1' and rd.grant = '1') then
            rd.grant      <= '0';           -- Deassert granting requests for BM component
            rd.bm_size    <= bm_in.rd_size; -- Load BM size to transfer (real is +1)
            rd.axi_addr   <= bm_in.rd_addr; -- Load starting address
            rd.state      <= compute1;      -- Change to next state
          else
            rd.grant <= '1'; -- Allow new transaction requests
          end if;


        when compute1 => -- Worst delay path: ADD 11+12, SUB 13-12
          -- Check if transaction will access two 4KB address regions
          addr_end        := add_vector(rd.bm_size, rd.axi_addr(11 downto 0), addr_end'length);
          rd.two_burst    <= addr_end(12);

          -- If transaction must be split in two bursts, due to 4KB overflow boundary, calculate...
          if(addr_end(12) = '1') then
            -- first burst size (doesn't require +1)  MAX=4096, MIN=0
            rd.bm_size    <= sub_vector('1'&"000", rd.axi_addr(11 downto 0), rd.bm_size'length);
            -- second burst size (doesn't require +1) MAX=4095, MIN=0
            rd.rem_size   <= sub_vector(addr_end, x"FFF", rd.rem_size'length);
          else
            -- if not, update burst size so it doesn't require +1
            rd.bm_size    <= add_vector(rd.bm_size, 1, rd.bm_size'length);
          end if;

          -- Next compute step
          rd.state        <= compute2;


        when compute2 => -- Worst delay path: DECIDE_SIZE, SUB 7-7
          -- Decide AXI size mode and update start address with aligned address
          decide_size(rd.bm_size, rd.axi_addr(6 downto 0), axi_size, addr_strt);
          rd.axi_size     <= axi_size;
          rd.axi_addr(6 downto 0) <= addr_strt;

          if(rd.first_burst = '1') then
          -- BM requested address - AXI address to request = Number of right-zeros bytes to strobe at the first transfer
            byte_strb     := sub_vector(rd.axi_addr(6 downto 0), addr_strt, byte_strb'length);
          else -- If is second burst, the number of left-zeros is "AXI_DWIDTH/8 - rem_size" if rem_size < AXI_DWIDTH/8
            if(rd.axi_strobe'length < to_integer(unsigned(rd.rem_size))) then
              byte_strb   := sub_vector(std_logic_vector(to_unsigned(rd.axi_strobe'length, 7)), rd.rem_size(6 downto 0), byte_strb'length);
            else 
              byte_strb   := (others => '1');
            end if;
          end if;
          rd.byte_strobe  <= byte_strb;

          -- Next compute state
          rd.state        <= compute3;


        when compute3 => -- Worst delay path: DECIDE_LEN(MUX 8to1, ORx6, SUB 9-1)
          -- Set starting strobe
          decide_strobe(byte_strb, axi_strb);
          rd.axi_strobe   <= axi_strb;

          -- Compute how many beats will be necessary to transfer the axi_size
          decide_len(rd.bm_size, rd.axi_size, axi_len);
          rd.axi_len      <= axi_len;

          -- Set the burst transfer mode
          rd.axi_mode     <= INC;

          -- Next, the handshake step
          rd.axi_valid    <= '1'; -- Request AXI read
          rd.state        <= handshake;


        when handshake =>
          -- Having all computation steps and proper registration allows maximum frequency
          -- of operation when including this AXI master interface at the interconnect bus.
          -- For a read transaction, only axi_addr, axi_mode, axi_size and axi_len are sent.
          if (axi_in.ar_ready = '1') then
            rd.axi_ready  <= '1'; -- Read from AXI bus flag
            rd.axi_valid  <= '0'; -- At request being granted, deassert request
            rd.state      <= transfer;
          end if;


        when transfer =>
          if (rd.axi_ready = '1' and axi_in.ar_ready = '1') then
            rd.data_bus   <= axi_in.r_data;
            rd.axi_last   <= axi_in.r_last;
            rd.axi_ready  <= '0';
          end if;
          

        when others =>

      end case;


    end if;
  end process read_proc;


  -----------------------------------------------------------------------------
  -- Combinational process
  -----------------------------------------------------------------------------


  -----------------------------------------------------------------------------
  -- Component instantiation
  -----------------------------------------------------------------------------

  -- injector core
  core : injector
    generic map (
      pindex        => pindex,
      paddr         => paddr,
      pmask         => pmask,
      pirq          => pirq,
      dbits         => dbits,
      MAX_SIZE_BEAT => MAX_SIZE_BEAT
      )
    port map (
      rstn    => rstn,
      clk     => clk,
      apbi    => apbi,
      apbo    => apbo,
      bm0_in  => bm_in,
      bm0_out => bm_out
      );

  -- BM0
  --bm0 : generic_bm_axi
  --  generic map(
  --    async_reset      => ASYNC_RST,
  --    bm_dw            => dbits,
  --    be_dw            => AXIDW,
  --    be_rd_pipe       => 0,
  --    max_size         => 1024,
  --    max_burst_length => max_burst_length,
  --    burst_chop_mask  => burst_chop_mask,
  --    bm_info_print    => 1
  --    )        
  --  port map (
  --    clk              => clk,
  --    rstn             => rstn,
  --    --Bus master domain
  --    --Read Channel
  --    bmrd_addr        => bm_in.rd_addr,
  --    bmrd_size        => bm_in.rd_size,
  --    bmrd_req         => bm_in.rd_req,
  --    bmrd_req_granted => bm_out.rd_req_grant,
  --    bmrd_data        => bm_out.rd_data(127 downto 128-dbits),
  --    bmrd_valid       => bm_out.rd_valid,
  --    bmrd_done        => bm_out.rd_done,
  --    bmrd_error       => bm_out.rd_err,
  --    --Write Channel
  --    bmwr_addr        => bm_in.wr_addr,
  --    bmwr_size        => bm_in.wr_size,
  --    bmwr_req         => bm_in.wr_req,
  --    bmwr_req_granted => bm_out.wr_req_grant,
  --    bmwr_data        => bm_in.wr_data(127 downto 128-dbits),
  --    bmwr_full        => bm_out.wr_full,
  --    bmwr_done        => bm_out.wr_done,
  --    bmwr_error       => bm_out.wr_err
  --    );

end architecture rtl;
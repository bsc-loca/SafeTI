-----------------------------------------------------------------------------   
-- Entity:        tb_injector_axi
-- File:          tb_injector_axi.vhd
-- Author:        Francis Fuentes
-- Description:   Testbench AXI4 manager interface + injector top level entity.
-- Compatibility: This TB requires VHDL2008. However, it is compatible with older
--                compilers by comenting VHDL2008 and uncommenting !VHDL2008 lines.
------------------------------------------------------------------------------ 
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library safety;
use safety.injector_pkg.all;
use safety.tb_injector_pkg.all;
use safety.axi4_pkg.axi4_miso;
use safety.axi4_pkg.axi4_mosi;
use safety.axi4_pkg.axi4_manager;
use std.env.all; -- VHDL2008

-----------------------------------------------------------------------------
-- Top level testbench entity for SafeTI with AXI4 Manager interface.
--
-- This testbench setting is not thought to be used as an automated testing tool, since the tb logic 
-- is not prepared to fetch for AXI manager's. In addition, it requires a AXI4 Subordinate module that 
-- is not included with this testbench, which can be procured by generating a Full AXI4 Manager from Xilinx
-- Vivado IP core Wizard.
--
-- At the moment, only particular tests (test X) are used, where the user may change the size_vector 
-- and addr_vector to test specific cases. It also includes a software reset test to check that the 
-- injector internals are reset without getting stuck any ongoing AXI burst.
-- 
-----------------------------------------------------------------------------

entity tb_injector_axi is
  generic (
    -- SafeTI configuration
    PC_LEN            : integer range  2 to   10    :=    4;      -- Set the maximum number of programmable descriptor words to 2^PC_LEN
    MAX_SIZE_BURST    : integer range  8 to 4096    := 4096;      -- Maximum size of a beat at a burst transaction.
    -- AXI4 Configuration
    ID_R_WIDTH        : integer range  0 to   32    := 4;         -- AXI ID's bus width.
    ID_W_WIDTH        : integer range  0 to   32    := 4;         -- AXI ID's bus width.
    ADDR_WIDTH        : integer range 12 to   64    := 32;        -- AXI address bus width. (Tested only for 32 bits)
    DATA_WIDTH        : integer range  8 to 1024    := 128;       -- AXI data bus width. [Only power of 2s are allowed]
    axi_id            : integer range  0 to 32**2-1 := 0;         -- AXI manager burst index [Must be < ID_X_WIDTH**2-1]
    rd_n_fifo_regs    : integer range  2 to  256    := 4;         -- Number of FIFO registers to use at AXI read transactions.  [Only power of 2s are allowed]
    wr_n_fifo_regs    : integer range  2 to  256    := 4;         -- Number of FIFO registers to use at AXI write transactions. [Only power of 2s are allowed]
    -- Asynchronous reset configuration
    ASYNC_RST         : boolean                     := TRUE       -- Allow asynchronous reset flag (default=TRUE)
    );

end entity tb_injector_axi;

architecture rtl of tb_injector_axi is
  -----------------------------------------------------------------------------
  -- Constant declaration
  -----------------------------------------------------------------------------

  -- Testbench thresholds (maximum number of clock cycles allowed for the signals to be asserted before an error message)
  -- constant req_threshold  : integer                       := 2;  -- Requests (injector asserted) and granted requests (testbench asserted) (default=2)
  constant descr_compl_thereshold : integer               := 4096*2;  -- Waiting threshold for descriptor completition flag (injector asserted)

  -- Pointers
  constant inj_base_addr  : std_logic_vector(31 downto 0) := X"0000_0000";  -- Location of the injector at APB memory is not important, since sel and en are used


  -- Injector configurations
  constant irq_desc_compl_en : std_logic                  := '0';           -- Enable interruption at descriptor completion
  constant irq_prog_compl_en : std_logic                  := '1';           -- Enable interruption at program completion

  -- Freeze at interruption, Interruption enabled due to error at the network, at the injector or 
  -- due to program completion, Queue mode, SW reset, enable injector.

  -- Injector core configuration with Queue mode disabled:
  constant inj_conf       : std_logic_vector(31 downto 0) := X"0000_00" & "0" & "0" & "11" & irq_prog_compl_en & "0" & "0" & "1";

  -- Injector reset:
  constant inj_rst        : std_logic_vector(31 downto 0) := X"0000_00" & "0" & "0" & "000" & "0" & "1" & "0";


  -- AXI TEST X (Unaligned address and different size tests)
  --constant size_vector    : array_integer(0 to 1) := (4096, 0);
  --constant addr_vector    : addr_bank(0 to 16)    := (X"0000_0000", X"0000_0001", X"0000_0002", X"0000_0003", X"0000_0004", X"0000_0005", X"0000_0006", 
  --X"0000_0007", X"0000_0008", X"0000_0009", X"0000_000A", X"0000_000B", X"0000_000C", X"0000_000D", X"0000_000E", X"0000_000F", X"0000_0000");
  constant size_vector    : array_integer(0 to 40)  := (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,
                                                       64,128,256,512,1024,2048,4095,4096,4097);
  constant addr_vector    : addr_bank(0 to 3)       := (X"0000_0000", X"0000_0001", X"0000_000F", X"0000_0010");


  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------

  -- Injector I/O + Initial input values
  signal clk    : std_ulogic    := '0';
  signal rstn   : std_ulogic    := '0';

  signal apbi   : apb_slave_in  := DEF_INJ_APB;
  signal apbo   : apb_slave_out;

  signal axi4mi : axi4_miso;
  signal axi4mo : axi4_mosi;

  signal ib_axi_req_rd : std_logic;
  signal ib_axi_req_wr : std_logic;

  -- I/O Injector and AXI interface
  signal ib_out_injector  : safety.injector_pkg.ib_mosi;
  signal ib_in_injector   : safety.injector_pkg.ib_miso;
  signal ib_in_manager    : safety.axi4_pkg.ib_mosi;
  signal ib_out_manager   : safety.axi4_pkg.ib_miso;
  signal ib_out_test      : safety.injector_pkg.ib_miso;

  -- Testbench BM I/O
  signal ib_in  : safety.injector_pkg.ib_mosi;
  signal ib_out : safety.injector_pkg.ib_miso := DEF_INJ_IB;

  -- Selector between Injector BM to AXI manager (TRUE) or testbench (FALSE)
  signal AXI_com  : boolean := FALSE;
  signal test_vect: descriptor_bank_tb(0 to 0);

  -- Control test signals, used to error if the execution doesn't continue after a threshold
  signal limit_rd_req_grant : integer   := 0;
  signal limit_wr_req_grant : integer   := 0;
  signal limit_rd_req       : integer   := 0;
  signal limit_wr_req       : integer   := 0;
  signal limit_descr_compl  : integer   := 0;
  signal wait_descr_compl   : std_logic := '0';


  -----------------------------------------------------------------------------
  -- Component declaration
  -----------------------------------------------------------------------------

  -- AXI4 subordinate memory wrapper
  component subordinate_v1_0 is
    generic (
      C_S00_AXI_ID_WIDTH     : integer := 1;
      C_S00_AXI_DATA_WIDTH   : integer := 32;
      C_S00_AXI_ADDR_WIDTH   : integer := 10;
      C_S00_AXI_AWUSER_WIDTH : integer := 0;
      C_S00_AXI_ARUSER_WIDTH : integer := 0;
      C_S00_AXI_WUSER_WIDTH  : integer := 0;
      C_S00_AXI_RUSER_WIDTH  : integer := 0;
      C_S00_AXI_BUSER_WIDTH  : integer := 0
    );
    port (
      s00_AXI_aclk      : in  std_ulogic;
      s00_AXI_aresetn   : in  std_ulogic;
      s00_AXI_awid      : in  std_logic_vector(C_S00_AXI_ID_WIDTH-1 downto 0);
      s00_AXI_awaddr    : in  std_logic_vector(C_S00_AXI_ADDR_WIDTH-1 downto 0);
      s00_AXI_awlen     : in  std_logic_vector(7 downto 0);
      s00_AXI_awsize    : in  std_logic_vector(2 downto 0);
      s00_AXI_awburst   : in  std_logic_vector(1 downto 0);
      s00_AXI_awlock    : in  std_logic;
      s00_AXI_awcache   : in  std_logic_vector(3 downto 0);
      s00_AXI_awprot    : in  std_logic_vector(2 downto 0);
      s00_AXI_awqos     : in  std_logic_vector(3 downto 0);
      s00_AXI_awregion  : in  std_logic_vector(3 downto 0);
      s00_AXI_awuser    : in  std_logic_vector(C_S00_AXI_AWUSER_WIDTH-1 downto 0);
      s00_AXI_awvalid   : in  std_logic;
      s00_AXI_awready   : out std_logic;
      s00_AXI_wdata     : in  std_logic_vector(C_S00_AXI_DATA_WIDTH-1 downto 0);
      s00_AXI_wstrb     : in  std_logic_vector((C_S00_AXI_DATA_WIDTH/8)-1 downto 0);
      s00_AXI_wlast     : in  std_logic;
      s00_AXI_wuser     : in  std_logic_vector(C_S00_AXI_WUSER_WIDTH-1 downto 0);
      s00_AXI_wvalid    : in  std_logic;
      s00_AXI_wready    : out std_logic;
      s00_AXI_bid       : out std_logic_vector(C_S00_AXI_ID_WIDTH-1 downto 0);
      s00_AXI_bresp     : out std_logic_vector(1 downto 0);
      s00_AXI_buser     : out std_logic_vector(C_S00_AXI_BUSER_WIDTH-1 downto 0);
      s00_AXI_bvalid    : out std_logic;
      s00_AXI_bready    : in  std_logic;
      s00_AXI_arid      : in  std_logic_vector(C_S00_AXI_ID_WIDTH-1 downto 0);
      s00_AXI_araddr    : in  std_logic_vector(C_S00_AXI_ADDR_WIDTH-1 downto 0);
      s00_AXI_arlen     : in  std_logic_vector(7 downto 0);
      s00_AXI_arsize    : in  std_logic_vector(2 downto 0);
      s00_AXI_arburst   : in  std_logic_vector(1 downto 0);
      s00_AXI_arlock    : in  std_logic;
      s00_AXI_arcache   : in  std_logic_vector(3 downto 0);
      s00_AXI_arprot    : in  std_logic_vector(2 downto 0);
      s00_AXI_arqos     : in  std_logic_vector(3 downto 0);
      s00_AXI_arregion  : in  std_logic_vector(3 downto 0);
      s00_AXI_aruser    : in  std_logic_vector(C_S00_AXI_ARUSER_WIDTH-1 downto 0);
      s00_AXI_arvalid   : in  std_logic;
      s00_AXI_arready   : out std_logic;
      s00_AXI_rid       : out std_logic_vector(C_S00_AXI_ID_WIDTH-1 downto 0);
      s00_AXI_rdata     : out std_logic_vector(C_S00_AXI_DATA_WIDTH-1 downto 0);
      s00_AXI_rresp     : out std_logic_vector(1 downto 0);
      s00_AXI_rlast     : out std_logic;
      s00_AXI_ruser     : out std_logic_vector(C_S00_AXI_RUSER_WIDTH-1 downto 0);
      s00_AXI_rvalid    : out std_logic;
      s00_AXI_rready    : in  std_logic
    );
  end component subordinate_v1_0;

begin  -- rtl

  -----------------
  -- Assignments --
  -----------------

  -- Clock generation
  clk <= not clk after T/2;

  -- BM interconnect switch used to load descriptors from testbench.
  ib_in_manager.rd_addr       <= ((63 downto 32 => '0') & ib_out_injector.rd_addr) when AXI_com else (others => '0');
  ib_in_manager.rd_size       <= ib_out_injector.rd_size when AXI_com else (others => '0');
  ib_in_manager.rd_req        <= ib_out_injector.rd_req  when AXI_com else '0';
  ib_in_manager.wr_addr       <= ((63 downto 32 => '0') & ib_out_injector.wr_addr) when AXI_com else (others => '0');
  ib_in_manager.wr_size       <= ib_out_injector.wr_size when AXI_com else (others => '0');
  ib_in_manager.wr_req        <= ib_out_injector.wr_req  when AXI_com else '0';
  ib_in_manager.wr_data       <= ((1023 downto 128 => '0') & ib_out_injector.wr_data) when AXI_com else (others => '0');

  ib_in_manager.rd_fixed_addr <= ib_out_injector.rd_fix_addr;
  ib_in_manager.rd_axi_cache  <= "0011";
  ib_in_manager.rd_axi_prot   <= "001";
  ib_in_manager.wr_fixed_addr <= ib_out_injector.wr_fix_addr;
  ib_in_manager.wr_axi_cache  <= "0011";
  ib_in_manager.wr_axi_prot   <= "001";

  ib_in.rd_addr               <= ib_out_injector.rd_addr when not(AXI_com) else (others => '0');
  ib_in.rd_size               <= ib_out_injector.rd_size when not(AXI_com) else (others => '0');
  ib_in.rd_req                <= ib_out_injector.rd_req  when not(AXI_com) else '0';
  ib_in.wr_addr               <= ib_out_injector.wr_addr when not(AXI_com) else (others => '0');
  ib_in.wr_size               <= ib_out_injector.wr_size when not(AXI_com) else (others => '0');
  ib_in.wr_req                <= ib_out_injector.wr_req  when not(AXI_com) else '0';
  ib_in.wr_data               <= ib_out_injector.wr_data when not(AXI_com) else (others => '0');


  ib_in_injector.rd_data      <= ib_out_manager.rd_data(DATA_WIDTH-1 downto 0) & (127 downto DATA_WIDTH => '0') when AXI_com else ib_out.rd_data;
  ib_in_injector.rd_req_grant <= ib_out_manager.rd_req_grant when AXI_com else ib_out.rd_req_grant;
  ib_in_injector.rd_valid     <= ib_out_manager.rd_valid     when AXI_com else ib_out.rd_valid    ;
  ib_in_injector.rd_done      <= ib_out_manager.rd_done      when AXI_com else ib_out.rd_done     ;
  ib_in_injector.rd_err       <= ib_out_manager.rd_err       when AXI_com else ib_out.rd_err      ;
  ib_in_injector.wr_req_grant <= ib_out_manager.wr_req_grant when AXI_com else ib_out.wr_req_grant;
  ib_in_injector.wr_full      <= ib_out_manager.wr_full      when AXI_com else ib_out.wr_full     ;
  ib_in_injector.wr_done      <= ib_out_manager.wr_done      when AXI_com else ib_out.wr_done     ;
  ib_in_injector.wr_err       <= ib_out_manager.wr_err       when AXI_com else ib_out.wr_err      ;

  ib_out_test.rd_valid        <= ib_out_manager.rd_valid;
  ib_out_test.rd_done         <= ib_out_manager.rd_done;

  
  -----------------------------------------------------------------------------
  -- Sequential process
  -----------------------------------------------------------------------------

  test : process
  begin

    ----------------------------------------
    --               TEST X               --
    ----------------------------------------
    for k in 0 to 1 loop -- 0 for reads, 1 for writes
    for m in addr_vector'range loop -- addr_vector'range
    for j in size_vector'range loop -- size_vector'range

      apbi.sel  <= '1';     -- Set injector at the APB bus to write configuration
      AXI_com   <= FALSE;   -- Change BM connections to testbench, so no AXI communication is established
      test_vect(0) <= descriptor_rd_wr( size_vector(j), 0, std_logic_vector(to_unsigned(sel(1, 2, k=0), 5)), addr_vector(m), '0', '1');
      wait until rising_edge(clk);
      rstn      <= '1';

      -- Load descriptors for test X
      load_descriptors(clk, inj_base_addr, test_vect, apbi);

      -- Configure and start injector
      write_address(clk, apbo, apbi, inj_base_addr, inj_conf);

      -- Test all descriptors from TEST 1 once
        -- Change BM connections to AXI manager, to establish AXI communication and transaction.
      wait until rising_edge(clk);
      AXI_com   <= TRUE;

      -- Wait for descriptor completion or crash if takes too long.
      wait_descr_compl <= '1';
      wait until rising_edge(apbo.irq);
      wait_descr_compl <= '0';
      
      wait for 20 ns;
      rstn      <= '0';

    end loop;
    end loop;
    end loop;

    wait for 1000 ns;
    report "The Test X has finished on time: " & time'image(now) & "\nGonna execute software reset test.";

    ----------------------------------------
    --            TEST RESET              --
    ----------------------------------------
    rstn        <= '1';

    apbi.sel    <= '1';     -- Set injector at the APB bus to write configuration
    AXI_com     <= FALSE;   -- Change BM connections to testbench, so no AXI communication is established
    test_vect(0) <= descriptor_rd_wr( 4096, 0, OP_READ, X"0000_0000", '0', '1');
    wait until rising_edge(clk);

    -- Load descriptors for test
    load_descriptors(clk, inj_base_addr, test_vect, apbi);

    -- Configure and start injector
    write_address(clk, apbo, apbi, inj_base_addr, inj_conf);

    -- Change BM connections to AXI manager, to establish AXI communication and transaction.
    wait until rising_edge(clk);
    AXI_com   <= TRUE;

    -- Let the AXI burst start
    wait until rising_edge(axi4mo.ar_valid);
    wait until rising_edge(clk); wait until rising_edge(clk); wait until rising_edge(clk);

    -- Stop injector in middle of a burst
    write_address(clk, apbo, apbi, inj_base_addr, x"0000_0000");

    -- Wait for ongoing AXI transaction to finish
    wait until rising_edge(axi4mi.r_last);
    wait for 100 ns;


    -- Resume injector execution
    write_address(clk, apbo, apbi, inj_base_addr, inj_conf);

    wait until rising_edge(axi4mo.ar_valid);
    wait until rising_edge(clk); wait until rising_edge(clk); wait until rising_edge(clk);

    -- Reset injector
    write_address(clk, apbo, apbi, inj_base_addr, inj_rst);

    -- Wait for ongoing AXI transaction to finish
    wait until rising_edge(axi4mi.r_last);
    wait for 100 ns;

    stop;

  end process test;


  -- Counters used to count how many clk cycles X signals get stuck
  interrupt_test : process(clk)
  begin 
    if(clk = '1' and clk'event) then
      -- Increment counters if the signal stays asserted
  --     if(ib_out.rd_req_grant = '1') then limit_rd_req_grant <= limit_rd_req_grant + 1; 
  --       else limit_rd_req_grant <= 0; end if;
  --     if(ib_out.wr_req_grant = '1') then limit_wr_req_grant <= limit_wr_req_grant + 1;
  --       else limit_wr_req_grant <= 0; end if;
  --     if(ib_in.rd_req = '1') then limit_rd_req <= limit_rd_req + 1;
  --       else limit_rd_req <= 0; end if;
  --     if(ib_in.wr_req = '1') then limit_wr_req <= limit_wr_req + 1;
  --       else limit_wr_req <= 0; end if;
      if(wait_descr_compl = '1') then limit_descr_compl <= limit_descr_compl + 1;
        else limit_descr_compl <= 0; end if;
    end if;

    -- Crash test with error if something gets stuck for Y threshold
  --   if(
  --     limit_rd_req_grant > req_threshold or
  --     limit_wr_req_grant > req_threshold
  --   ) then
  --     assert FALSE report "TEST GOT STUCK DUE TO REQUEST NOT BEING GRANTED!" severity failure;
  --   end if;

  --   if(
  --     limit_rd_req       > req_threshold or
  --     limit_wr_req       > req_threshold
  --   ) then
  --     assert FALSE report "TEST GOT STUCK DUE TO INJECTOR NOT REQUESTING TRANSACTION!" severity failure;
  --   end if;

    if(
      limit_descr_compl > descr_compl_thereshold
    ) then
      assert FALSE report "The the injector took too long to assert the descriptor completion flag (apbo.irq is 0)." severity failure;
    end if;

  end process interrupt_test;


  -----------------------------------------------------------------------------
  -- Component instantiation
  -----------------------------------------------------------------------------

  -- injector core
  core : injector_core
    generic map (
      PC_LEN          => PC_LEN,
      CORE_DATA_WIDTH => DATA_WIDTH,
      MAX_SIZE_BURST  => MAX_SIZE_BURST,
      ASYNC_RST       => ASYNC_RST
      )
    port map (
      rstn            => rstn,
      clk             => clk,
      apbi            => apbi,
      apbo            => apbo,
      ib_out          => ib_out_injector,
      ib_in           => ib_in_injector
      );

  -- AXI4 Manager interface
  AXI4_M0 : axi4_manager
  generic map (
    ID_R_WIDTH        => ID_R_WIDTH,
    ID_W_WIDTH        => ID_W_WIDTH,
    ADDR_WIDTH        => ADDR_WIDTH,
    DATA_WIDTH        => DATA_WIDTH,
    axi_id            => axi_id,
    dbits             => DATA_WIDTH,
    rd_n_fifo_regs    => rd_n_fifo_regs,
    wr_n_fifo_regs    => wr_n_fifo_regs,
    ASYNC_RST         => ASYNC_RST
  )
  port map (
    rstn              => rstn,
    clk               => clk,
    axi4mi            => axi4mi,
    axi4mo            => axi4mo,
    ib_in             => ib_in_manager,
    ib_out            => ib_out_manager
  );

  -- AXI4 subordinate memory 1024 bytes
  AXI4_S0 : subordinate_v1_0
  generic map (
    C_S00_AXI_ID_WIDTH      => ID_R_WIDTH,
    C_S00_AXI_DATA_WIDTH    => DATA_WIDTH,
    C_S00_AXI_ADDR_WIDTH    => 13, --axi4mo.aw_addr'length,
    C_S00_AXI_AWUSER_WIDTH  => 1,
    C_S00_AXI_ARUSER_WIDTH  => 1,
    C_S00_AXI_WUSER_WIDTH   => 1
  )
  port map (
    s00_AXI_aclk      => clk,
    s00_AXI_aresetn   => rstn,
    s00_AXI_awid      => axi4mo.aw_id(ID_R_WIDTH-1 downto 0),
    s00_AXI_awaddr    => axi4mo.aw_addr(12 downto 0),
    s00_AXI_awlen     => axi4mo.aw_len,
    s00_AXI_awsize    => axi4mo.aw_size,
    s00_AXI_awburst   => axi4mo.aw_burst,
    s00_AXI_awlock    => axi4mo.aw_lock,
    s00_AXI_awcache   => axi4mo.aw_cache,
    s00_AXI_awprot    => axi4mo.aw_prot,
    s00_AXI_awqos     => axi4mo.aw_qos,
    s00_AXI_awregion  => axi4mo.aw_region,
    s00_AXI_awuser    => "0",
    s00_AXI_awvalid   => axi4mo.aw_valid,
    s00_AXI_awready   => axi4mi.aw_ready,
    s00_AXI_wdata     => axi4mo.w_data(DATA_WIDTH-1 downto 0),
    s00_AXI_wstrb     => axi4mo.w_strb(DATA_WIDTH/8-1 downto 0),
    s00_AXI_wlast     => axi4mo.w_last,
    s00_AXI_wuser     => "0",
    s00_AXI_wvalid    => axi4mo.w_valid,
    s00_AXI_wready    => axi4mi.w_ready,
    s00_AXI_bid       => axi4mi.b_id(ID_R_WIDTH-1 downto 0),
    s00_AXI_bresp     => axi4mi.b_resp,
    --s00_AXI_buser     => "0",
    s00_AXI_bvalid    => axi4mi.b_valid,
    s00_AXI_bready    => axi4mo.b_ready,
    s00_AXI_arid      => axi4mo.ar_id(ID_R_WIDTH-1 downto 0),
    s00_AXI_araddr    => axi4mo.ar_addr(12 downto 0),
    s00_AXI_arlen     => axi4mo.ar_len,
    s00_AXI_arsize    => axi4mo.ar_size,
    s00_AXI_arburst   => axi4mo.ar_burst,
    s00_AXI_arlock    => axi4mo.ar_lock,
    s00_AXI_arcache   => axi4mo.ar_cache,
    s00_AXI_arprot    => axi4mo.ar_prot,
    s00_AXI_arqos     => axi4mo.ar_qos,
    s00_AXI_arregion  => axi4mo.ar_region,
    s00_AXI_aruser    => "0",
    s00_AXI_arvalid   => axi4mo.ar_valid,
    s00_AXI_arready   => axi4mi.ar_ready,
    s00_AXI_rid       => axi4mi.r_id(ID_R_WIDTH-1 downto 0),
    s00_AXI_rdata     => axi4mi.r_data(DATA_WIDTH-1 downto 0),
    s00_AXI_rresp     => axi4mi.r_resp,
    s00_AXI_rlast     => axi4mi.r_last,
    --s00_AXI_ruser     => "0",
    s00_AXI_rvalid    => axi4mi.r_valid,
    s00_AXI_rready    => axi4mo.r_ready
  );

  -- Zero not used signals
  axi4mi.b_id(axi4mi.b_id'high downto ID_R_WIDTH)     <= (others => '0');
  axi4mi.r_id(axi4mi.r_id'high downto ID_R_WIDTH)     <= (others => '0');
  axi4mi.r_data(axi4mi.r_data'high downto DATA_WIDTH) <= (others => '0');


end architecture rtl;




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
library bsc;
use bsc.injector_pkg.all;
use bsc.tb_injector_pkg.all;
use bsc.axi4_pkg.axi4_in_type;
use bsc.axi4_pkg.axi4_out_type;
use bsc.axi4_pkg.axi4_manager;
use std.env.all; -- VHDL2008

-----------------------------------------------------------------------------
-- Top level testbench entity for AXI4 manager interface.
--
-- This testbench setting is not thought to be used as an automated testing tool, since the tb logic 
-- is not prepared yet to fetch for AXI manager's BM control signals.
-- Some generics may not work for different values from the default ones. 
-- 
-----------------------------------------------------------------------------

entity tb_injector_axi is
  generic (
    -- APB configuration  
    pindex            : integer                             := 6;         -- APB configuartion slave index (default=6)
    paddr             : integer                             := 16#850#;   -- APB configuartion slave address (default=16#850#)
    pmask             : integer                             := 16#FFF#;   -- APB configuartion slave mask (default=16#FFF#)
    pirq              : integer range  0 to APB_IRQ_NMAX-1  := 6;         -- APB configuartion slave irq (default=6)
    -- Bus master configuration
    dbits             : integer range 32 to 128             := bsc.injector_pkg.dbits; -- Data width of BM and FIFO (default=32)
    MAX_SIZE_BURST    : integer range 32 to 4096            := bsc.injector_pkg.MAX_SIZE_BURST;
    -- Injector configuration
    ASYNC_RST         : boolean                             := FALSE      -- Allow asynchronous reset flag (default=FALSE)
    );

end entity tb_injector_axi;

architecture rtl of tb_injector_axi is
  -----------------------------------------------------------------------------
  -- Constant declaration
  -----------------------------------------------------------------------------

  -- Testbench thresholds (maximum number of clock cycles allowed for the signals to be asserted before an error message)
  constant req_threshold  : integer                       := 2;  -- Requests (injector asserted) and granted requests (testbench asserted) (default=2)
  constant descr_compl_thereshold : integer               := 0;  -- Waiting threshold for descriptor completition flag (injector asserted) (default=0)

  -- Pointers
  constant apb_inj_addr   : std_logic_vector(31 downto 0) := X"000" & std_logic_vector(to_unsigned(paddr, 12)) & X"00"; -- Location of the injector at APB memory
  constant descr_addr1    : std_logic_vector(31 downto 0) := X"0100_0000";  -- First descriptor MSB address for test 1
  constant descr_addr2w   : std_logic_vector(31 downto 0) := X"0110_0000";  -- First descriptor MSB address for test 2 writes
  constant descr_addr2r   : std_logic_vector(31 downto 0) := X"0120_0000";  -- First descriptor MSB address for test 2 reads
  constant action_addr    : std_logic_vector(31 downto 0) := X"0000_0000";  -- Write/read address

  -- Injector configurations
  -- Injector reset
  constant inj_rst        : std_logic_vector(31 downto 0) := X"0000_00" & "00" & "000010";

  -- Test 1 configuration: Queue mode disabled, enable interrupt on error, interrupt enabled, (kick disabled), reset, start injector.
  constant inj_config1    : std_logic_vector(31 downto 0) := X"0000_00" & "00" & "011001";
  
  -- Test 2 configuration: (Queue mode disabled), enable interrupt on error, interrupt enabled, (kick disabled), reset, start injector.
  constant inj_config2    : std_logic_vector(31 downto 0) := X"0000_00" & "00" & "011001";

  -- AXI TEST X
  constant size_vector    : array_integer(0 to 6) := (64, 64, 33, 64, 65, 128, 129);
  --constant size_vector    : array_integer(0 to 15) := (1, 2, 3, 4, 5, 7, 8, 9, 15, 16, 17, 18, 31, 32, 33, 34);

  -- Descriptors to load into injector's fifo for test 1 (size, count, action, addr, addrfix, nextraddr, last)
  constant descriptors1   : descriptor_bank(0 to 6) := (
    write_descriptor(               4, 63,  RD,  action_addr, '0', add_vector(descr_addr1,   20, 32), '0' ), -- 64 write transactions of   4 bytes
    write_descriptor(               8, 31,  RD,  action_addr, '0', add_vector(descr_addr1,   40, 32), '0' ), -- 32 write transactions of   8 bytes
    write_descriptor(              16, 15,  RD,  action_addr, '0', add_vector(descr_addr1,   60, 32), '0' ), -- 16  read transactions of  16 bytes
    write_descriptor(              32,  7,  RD,  action_addr, '0', add_vector(descr_addr1,   80, 32), '0' ), --  8  read transactions of  32 bytes
    write_descriptor(              64,  3,  RD,  action_addr, '0', add_vector(descr_addr1,  100, 32), '0' ), --  4 write transactions of  64 bytes
    write_descriptor(             128,  1,  RD,  action_addr, '0', add_vector(descr_addr1,  120, 32), '0' ), --  2 write transactions of 128 bytes
    write_descriptor(             256,  0,  RD,  action_addr, '0', add_vector(descr_addr1,  140, 32), '1' )  --  1  read transaction  of 256 bytes
  );

  -- Descriptors to load into injector's fifo for test 2 write (size, count, action, addr, addrfix, nextraddr, last)
  constant descriptors2w  : descriptor_bank(0 to 5) := (
    write_descriptor(MAX_SIZE_BURST-3,  0, WRT,  action_addr, '0', add_vector(descr_addr2w,  20, 32), '0' ), -- Check if writes the correct ammount below size beat
    write_descriptor(  MAX_SIZE_BURST,  0, WRT,  action_addr, '0', add_vector(descr_addr2w,  40, 32), '0' ), -- Check if writes the correct ammount equal size beat
    write_descriptor(MAX_SIZE_BURST+3,  0, WRT,  action_addr, '0', add_vector(descr_addr2w,  60, 32), '0' ), -- Check if writes the correct ammount above size beat
    write_descriptor(               3,  0, WRT,  action_addr, '1', add_vector(descr_addr2w,  80, 32), '0' ), -- With fix addr, check if reads lower of a word
    write_descriptor(               4,  0, WRT,  action_addr, '1', add_vector(descr_addr2w, 100, 32), '0' ), -- With fix addr, check if reads a word
    write_descriptor(              15,  0, WRT,  action_addr, '1', add_vector(descr_addr2w, 120, 32), '1' )  -- With fix addr, check if it really fixes the addr
  );

  -- Descriptors to load into injector's fifo for test 2 read (size, count, action, addr, addrfix, nextraddr, last)
  constant descriptors2r  : descriptor_bank(0 to 5) := (
    write_descriptor(MAX_SIZE_BURST-3,  0,   RD, action_addr, '0', add_vector(descr_addr2r,  20, 32), '0' ), -- Check if writes the correct ammount below size beat
    write_descriptor(  MAX_SIZE_BURST,  0,   RD, action_addr, '0', add_vector(descr_addr2r,  40, 32), '0' ), -- Check if writes the correct ammount equal size beat
    write_descriptor(MAX_SIZE_BURST+3,  0,   RD, action_addr, '0', add_vector(descr_addr2r,  60, 32), '0' ), -- Check if writes the correct ammount above size beat
    write_descriptor(               3,  0,   RD, action_addr, '1', add_vector(descr_addr2r,  80, 32), '0' ), -- With fix addr, check if reads lower of a word
    write_descriptor(               4,  0,   RD, action_addr, '1', add_vector(descr_addr2r, 100, 32), '0' ), -- With fix addr, check if reads a word
    write_descriptor(              15,  0,   RD, action_addr, '1', add_vector(descr_addr2r, 120, 32), '1' )  -- With fix addr, check if it really fixes the addr
  );

  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------

  -- Injector I/O + Initial input values
  signal clk    : std_ulogic  := '0';
  signal rstn   : std_ulogic  := '0';

  signal apbi   : apb_slave_in_type := DEF_INJ_APB;
  signal apbo   : apb_slave_out_type;

  signal axi4mi : axi4_in_type;
  signal axi4mo : axi4_out_type;

  signal bm_axi_req_rd : std_logic;
  signal bm_axi_req_wr : std_logic;

  -- I/O Injector and AXI interface
  signal bm_in_injector   : bsc.injector_pkg.bm_in_type;
  signal bm_out_injector  : bsc.injector_pkg.bm_out_type;
  signal bm_in_manager    : bsc.axi4_pkg.bm_in_type;
  signal bm_out_manager   : bsc.axi4_pkg.bm_out_type;
  signal bm_out_test      : bsc.injector_pkg.bm_out_type;

  -- Testbench BM I/O
  signal bm_in  : bsc.injector_pkg.bm_in_type;
  signal bm_out : bsc.injector_pkg.bm_out_type := DEF_INJ_BM;

  -- Selector between Injector BM to AXI manager (TRUE) or testbench (FALSE)
  signal AXI_com  : boolean := FALSE;
  signal test_vect: descriptor_bank(0 to 0);

  -- AXI Manager interface configuration signals
  signal bm_skip  : std_logic := '0';

  -- APB configuration
  signal apb_sel: std_logic_vector(apbi.sel'range) := std_logic_vector(shift_left(to_unsigned(1, apbi.sel'length), apbi.sel'length-pindex-1));

  -- Control test signals, used to error if the execution doesn't continue after a threshold
  signal limit_rd_req_grant : integer   := 0;
  signal limit_wr_req_grant : integer   := 0;
  signal limit_rd_req       : integer   := 0;
  signal limit_wr_req       : integer   := 0;
  signal limit_descr_compl  : integer   := 0;
  signal wait_descr_compl   : std_logic := '0';


  -----------------------------------------------------------------------------
  -- Component instantiation
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
  bm_in_manager.rd_addr         <= bm_in_injector.rd_addr when AXI_com else (others => '0');
  bm_in_manager.rd_size         <= bm_in_injector.rd_size when AXI_com else (others => '0');
  bm_in_manager.rd_req          <= bm_in_injector.rd_req  when AXI_com else '0';
  bm_in_manager.wr_addr         <= bm_in_injector.wr_addr when AXI_com else (others => '0');
  bm_in_manager.wr_size         <= bm_in_injector.wr_size when AXI_com else (others => '0');
  bm_in_manager.wr_req          <= bm_in_injector.wr_req  when AXI_com else '0';
  bm_in_manager.wr_data         <= bm_in_injector.wr_data when AXI_com else (others => '0');

  bm_in.rd_addr                 <= bm_in_injector.rd_addr when not(AXI_com) else (others => '0');
  bm_in.rd_size                 <= bm_in_injector.rd_size when not(AXI_com) else (others => '0');
  bm_in.rd_req                  <= bm_in_injector.rd_req  when not(AXI_com) else '0';
  bm_in.wr_addr                 <= bm_in_injector.wr_addr when not(AXI_com) else (others => '0');
  bm_in.wr_size                 <= bm_in_injector.wr_size when not(AXI_com) else (others => '0');
  bm_in.wr_req                  <= bm_in_injector.wr_req  when not(AXI_com) else '0';
  bm_in.wr_data                 <= bm_in_injector.wr_data when not(AXI_com) else (others => '0');


  bm_out_injector.rd_data       <= bm_out_manager.rd_data      when AXI_com else bm_out.rd_data     ;
  bm_out_injector.rd_req_grant  <= bm_out_manager.rd_req_grant when AXI_com else bm_out.rd_req_grant;
  bm_out_injector.rd_valid      <= bm_out_manager.rd_valid     when AXI_com else bm_out.rd_valid    ;
  bm_out_injector.rd_done       <= bm_out_manager.rd_done      when AXI_com else bm_out.rd_done     ;
  bm_out_injector.rd_err        <= bm_out_manager.rd_err       when AXI_com else bm_out.rd_err      ;
  bm_out_injector.wr_req_grant  <= bm_out_manager.wr_req_grant when AXI_com else bm_out.wr_req_grant;
  bm_out_injector.wr_full       <= bm_out_manager.wr_full      when AXI_com else bm_out.wr_full     ;
  bm_out_injector.wr_done       <= bm_out_manager.wr_done      when AXI_com else bm_out.wr_done     ;
  bm_out_injector.wr_err        <= bm_out_manager.wr_err       when AXI_com else bm_out.wr_err      ;

  bm_out_test.rd_valid          <= bm_out_manager.rd_valid;
  bm_out_test.rd_done           <= bm_out_manager.rd_done;

  
  -----------------------------------------------------------------------------
  -- Sequential process
  -----------------------------------------------------------------------------

  test : process
  begin

    ----------------------------------------
    --               TEST X               --
    ----------------------------------------
    for j in size_vector'range loop

      apbi.sel  <= apb_sel; -- Set injector at the APB bus to write configuration
      AXI_com   <= FALSE;   -- Change BM connections to testbench, so no AXI communication is established
      bm_skip   <= '1';     -- Skip BM transfers to only test AXI communication requested by the injector
      test_vect(0) <= write_descriptor( size_vector(j),  0,  WRT,  action_addr, '0', add_vector(descr_addr1,   20, 32), '1' );
      wait until rising_edge(clk);
      rstn      <= '1';

      -- Configure injector for test X
      configure_injector(clk, apb_inj_addr, descr_addr1, inj_config1, apbo, apbi);

      -- Load descriptors for test X
      load_descriptors(clk, test_vect, descr_addr1, bm_in, bm_out);

      -- Test all descriptors from TEST 1 once
        -- Change BM connections to AXI manager, to establish AXI communication and transaction.
      wait until rising_edge(clk);
      AXI_com   <= TRUE;

      wait until rising_edge(bm_out_injector.rd_done); -- Wait for interface to complete transaction
      wait for 20 ns;
      rstn      <= '0';

    end loop;

    stop;
      
    -- Reset injector
    AXI_com   <= FALSE;   -- Change BM connections to testbench, so no AXI communication is established
    configure_injector(clk, apb_inj_addr, descr_addr1, inj_rst, apbo, apbi);

    -- Check if reset has worked
    apbi.sel    <= apb_sel; -- Set injector at the APB bus to write configuration
    apbi.en     <= '1';
    apbi.addr   <= apb_inj_addr; -- Read 0x00 ctrl debug register
    apbi.write  <= '0';
    wait until rising_edge(clk); wait until rising_edge(clk);
    if(or_vector(apbo.rdata) = '1') then
      assert FALSE report "Test X: Injector reset FAILED!" & LF & "         Injector has control data after reset." severity failure;
    else           report "Test X: Injector has been reset successfully!";
    end if;
    apbi.en     <= '0';
    wait for 1 us;

    -- To reset loaded descriptors, it is required to use the reset low input
    rstn        <= '0';
    wait until rising_edge(clk);  wait until rising_edge(clk);
    rstn        <= '1';



    ----------------------------------------
    --               TEST 1               --
    ----------------------------------------

    -- Configure injector for test 1
    report "Test 1: Load configuration and start injector!";
    AXI_com   <= FALSE;   -- Change BM connections to testbench, so no AXI communication is established
    configure_injector(clk, apb_inj_addr, descr_addr1, inj_config1, apbo, apbi);

    -- Load descriptors for test 1
    report "Test 1: Loading descriptor batch!";
    load_descriptors(clk, descriptors1, descr_addr1, bm_in, bm_out);
    AXI_com   <= TRUE;   -- Change BM connections to AXI subordinate

    report "Test 1 descriptor batch has been completed succesfully once!";  
    -- Test all descriptors from TEST 1 for second time (queue test)
    test_descriptor_batch(clk, bm_in, bm_out, descriptors1, MAX_SIZE_BURST, apbo.irq(pirq), wait_descr_compl);
    report "Test 1 descriptor batch has been completed succesfully twice!";


    -- Reset injector
    configure_injector(clk, apb_inj_addr, descr_addr1, inj_rst, apbo, apbi);

    -- Check if reset has worked
    apbi.sel    <= apb_sel; -- Set injector at the APB bus to write configuration
    apbi.en     <= '1';
    apbi.addr   <= apb_inj_addr; -- Read 0x00 ctrl debug register
    apbi.write  <= '0';
    wait until rising_edge(clk); wait until rising_edge(clk);
    if(or_vector(apbo.rdata) = '1') then
      assert FALSE report "Test 1: Injector reset FAILED!" & LF & "         Injector has control data after reset." severity failure;
    else           report "Test 1: Injector has been reset successfully!";
    end if;
    apbi.en     <= '0';
    wait for 1 us;

    -- To reset loaded descriptors, it is required to use the reset low input
    rstn        <= '0';
    wait until rising_edge(clk);  wait until rising_edge(clk);
    rstn        <= '1';

    stop;

    ----------------------------------------
    --               TEST 2               --
    ----------------------------------------

    -- Configure injector for test 2 write
    report "Test 2: Load write configuration and start injector!";
    configure_injector(clk, apb_inj_addr, descr_addr2w, inj_config2, apbo, apbi);

    -- Load descriptors for test 2 write
    report "Test 2: Loading write descriptor batch!";
    --load_descriptors(clk, descriptors2w, descr_addr2w, bm_in, bm_out);

    -- Test all descriptors from TEST 2 write
    --test_descriptor_batch(clk, bm_in, bm_out, descriptors2w, MAX_SIZE_BURST, apbo.irq(pirq), wait_descr_compl); 
    report "Test 2 descriptor write batch has been completed succesfully!";

    -- Check if the injector is looping execution (non-queue mode shoould not repeat descriptors)
    for i in 0 to 9 loop
      wait until rising_edge(clk); 
    end loop;
    --assert (bm_in.rd_req or bm_in.wr_req) = '0'   report "Test 2: Injector non-queue mode FAILED!" & LF 
    --                                  & "         The injector is looping descriptors even though it shouldn't due to non-queue mode operation." & LF
    --                                  & "         (Make sure the configuration of the injector does not assert the queue mode bit)" severity failure;

    -- To reset loaded descriptors, it is required to use the reset low input
    rstn        <= '0';
    wait until rising_edge(clk);  wait until rising_edge(clk);
    rstn        <= '1';
    

    -- Configure injector for test 2 read
    report "Test 2: Load read configuration and start injector!";
    configure_injector(clk, apb_inj_addr, descr_addr2r, inj_config2, apbo, apbi);

    -- Load descriptors for test 2 read
    report "Test 2: Loading read descriptor batch!";
    --load_descriptors(clk, descriptors2r, descr_addr2r, bm_in, bm_out);

    -- Test all descriptors from TEST 2 read
    --test_descriptor_batch(clk, bm_in, bm_out, descriptors2r, MAX_SIZE_BURST, apbo.irq(pirq), wait_descr_compl); 
    report "Test 2 descriptor read batch has been completed succesfully!";

    -- Check if the injector is looping execution (non-queue mode shoould not repeat descriptors)
    for i in 0 to 9 loop
      wait until rising_edge(clk); 
    end loop;
    --assert (bm_in.rd_req or bm_in.wr_req) = '0'   report "Test 2: Injector non-queue mode FAILED!" & LF 
    --                                  & "         The injector is looping descriptors even though it shouldn't due to non-queue mode operation." & LF
    --                                  & "         (Make sure the configuration of the injector does not assert the queue mode bit)" severity failure;

    -- To reset loaded descriptors, it is required to use the reset low input
    rstn        <= '0';
    wait until rising_edge(clk);  wait until rising_edge(clk);
    rstn        <= '1';


    wait for 1 us;
    report "TEST SUCCESSFULLY FINISHED!"; stop; -- VHDL2008
    assert FALSE report "TEST SUCCESSFULLY FINISHED!" severity failure; -- !VHDL2008

  end process test;


  -- Counters used to count how many clk cycles X signals get stuck
  -- interrupt_test : process(clk)
  -- begin 
  --   if(clk = '1' and clk'event) then
  --     -- Increment counters if the signal stays asserted
  --     if(bm_out.rd_req_grant = '1') then limit_rd_req_grant <= limit_rd_req_grant + 1; 
  --       else limit_rd_req_grant <= 0; end if;
  --     if(bm_out.wr_req_grant = '1') then limit_wr_req_grant <= limit_wr_req_grant + 1;
  --       else limit_wr_req_grant <= 0; end if;
  --     if(bm_in.rd_req = '1') then limit_rd_req <= limit_rd_req + 1;
  --       else limit_rd_req <= 0; end if;
  --     if(bm_in.wr_req = '1') then limit_wr_req <= limit_wr_req + 1;
  --       else limit_wr_req <= 0; end if;
  --     if(wait_descr_compl = '1') then limit_descr_compl <= limit_descr_compl + 1;
  --       else limit_descr_compl <= 0; end if;
  --   end if;

  --   -- Crash test with error if something gets stuck for Y threshold
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

  --   if(
  --     limit_descr_compl > descr_compl_thereshold
  --   ) then
  --     assert FALSE report "The testbench has finished descriptor but the injector has not set the completion flag (apbo.irq is 0)." severity failure;
  --   end if;

  -- end process interrupt_test;


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
      ASYNC_RST     => FALSE
    )
    port map (
      rstn          => rstn,
      clk           => clk,
      apbi          => apbi,
      apbo          => apbo,
      bm0_in        => bm_in_injector,
      bm0_out       => bm_out_injector
    );

  -- AXI4 Manager interface
  AXI4_M0 : axi4_manager
  generic map (
    dbits           => dbits,
    axi_id          => 3
  )
  port map (
    rstn            => rstn,
    clk             => clk,
    axi4mi          => axi4mi,
    axi4mo          => axi4mo,
    bm_in           => bm_in_manager,
    bm_out          => bm_out_manager,
    bm_in_bypass_rd => bm_skip
  );

  -- AXI4 subordinate memory 1024 bytes
  AXI4_S0 : subordinate_v1_0
  generic map (
    C_S00_AXI_ID_WIDTH      => axi4mo.aw_id'length,
    C_S00_AXI_DATA_WIDTH    => axi4mo.w_data'length,
    C_S00_AXI_ADDR_WIDTH    => 13, --axi4mo.aw_addr'length,
    C_S00_AXI_AWUSER_WIDTH  => 1,
    C_S00_AXI_ARUSER_WIDTH  => 1,
    C_S00_AXI_WUSER_WIDTH   => 1
  )
  port map (
    s00_AXI_aclk      => clk,
    s00_AXI_aresetn   => rstn,
    s00_AXI_awid      => axi4mo.aw_id,
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
    s00_AXI_wdata     => axi4mo.w_data,
    s00_AXI_wstrb     => axi4mo.w_strb,
    s00_AXI_wlast     => axi4mo.w_last,
    s00_AXI_wuser     => "0",
    s00_AXI_wvalid    => axi4mo.w_valid,
    s00_AXI_wready    => axi4mi.w_ready,
    s00_AXI_bid       => axi4mi.b_id,
    s00_AXI_bresp     => axi4mi.b_resp,
    --s00_AXI_buser     => "0",
    s00_AXI_bvalid    => axi4mi.b_valid,
    s00_AXI_bready    => axi4mo.b_ready,
    s00_AXI_arid      => axi4mo.ar_id,
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
    s00_AXI_rid       => axi4mi.r_id,
    s00_AXI_rdata     => axi4mi.r_data,
    s00_AXI_rresp     => axi4mi.r_resp,
    s00_AXI_rlast     => axi4mi.r_last,
    --s00_AXI_ruser     => "0",
    s00_AXI_rvalid    => axi4mi.r_valid,
    s00_AXI_rready    => axi4mo.r_ready
  );


end architecture rtl;




-----------------------------------------------------------------------------   
-- Entity:        tb_injector
-- File:          tb_injector.vhd
-- Author:        Francis Fuentes
-- Description:   Testbench injector top level entity.
-- Compatibility: This TB requires VHDL2008. However, it is compatible with older
--                compilers by comenting VHDL2008 and uncommenting !VHDL2008 lines.
------------------------------------------------------------------------------ 
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library safety;
use safety.injector_pkg.all;
use safety.tb_injector_pkg.all;
use std.env.all; -- VHDL2008

-----------------------------------------------------------------------------
-- Top level testbench entity for injector.
--
-- This testbench validates, previous to the AHB/AXI interface (on BM bus), the following:
--  Load of the descriptors (tests 1 and 2),
--  Transaction completion (test 1 and 2),
--  Transaction repetition (test 1),
--  Queue mode (test 1),
--  Injector reset (end of test 1),
--  Transaction size on fixed/unfixed address (test 2) on both read and writes.
-- 
-- Testbench specification:
-- The testbench simulates the behaviour from the SELENE's AHB interface when the injector
-- is in action (writes and reads only). Thus, the transfers are managed from the BM bus.
--
-- For each test (descriptor bank batch), the injector is initialized as if it were at the
-- SELENE platform by sending the equivalent APB signals. 
--
-- This testbench specifically tests the if the address being acted on is the programmed/expected, 
-- taking into account the "fixed address" and "bursts" features. Furthermore, the tests checks if
-- the injector asserts the descriptor completion flag when it's supposed to finish. 
-- The tests use descriptors with the 'enabled' and 'interrupt' flags asserted, meaning related 
-- features to these flags being desasserted are not validated by this testbench (skip disabled 
-- descriptors, for example).
--
-- In addition, the user may change the threshold of how many clock cycles are allowed to pass since
-- some signals assertions, including injector requests, testbench grant requests and testbench
-- waiting for the injector to assert the 'descriptor completed' interruption (this one is 0 by 
-- default because the testbench is arrenged to always check on the interruption when is expected
-- to be asserted).
--
-- Some generics may not work for different values from the default ones. 
-- 
-----------------------------------------------------------------------------

entity tb_injector is
  generic (
    -- SafeTI configuration
    dbits         : integer range 32 to  128                := 32;        -- Data width of BM and FIFO at injector. [Only power of 2s allowed]
    MAX_SIZE_BURST: integer range 32 to 4096                := 4096;      -- Maximum size of a beat at a burst transaction.
    -- APB configuration  
    pindex            : integer                             := 6;         -- APB configuartion slave index (default=6)
    paddr             : integer                             := 16#850#;   -- APB configuartion slave address (default=16#850#)
    pmask             : integer                             := 16#FFF#;   -- APB configuartion slave mask (default=16#FFF#)
    pirq              : integer range  0 to APB_IRQ_NMAX-1  := 6;         -- APB configuartion slave irq (default=6)
    -- Injector configuration
    ASYNC_RST         : boolean                             := FALSE      -- Allow asynchronous reset flag (default=FALSE)
    );

end entity tb_injector;

architecture rtl of tb_injector is
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
  constant action_addr    : std_logic_vector(31 downto 0) := X"0200_0000";  -- Write/read address

  -- Injector configurations
  -- Injector reset
  constant inj_rst        : std_logic_vector(31 downto 0) := X"0000_00" & "00" & "000010";

  -- Test 1 configuration: Queue mode enabled, enable interrupt on error, interrupt enabled, (kick disabled), reset, start injector.
  constant inj_config1    : std_logic_vector(31 downto 0) := X"0000_00" & "00" & "111001";
  
  -- Test 2 configuration: (Queue mode disabled), enable interrupt on error, interrupt enabled, (kick disabled), reset, start injector.
  constant inj_config2    : std_logic_vector(31 downto 0) := X"0000_00" & "00" & "011001";

  -- Descriptors to load into injector's fifo for test 1 (size, count, action, addr, addrfix, nextraddr, last)
  constant descriptors1   : descriptor_bank(0 to 6) := (
    write_descriptor(               4, 63, WRT, action_addr, '1', add_vector(descr_addr1,   20, 32), '0' ), -- 64 write transactions of   4 bytes
    write_descriptor(               8, 31, WRT, action_addr, '1', add_vector(descr_addr1,   40, 32), '0' ), -- 32 write transactions of   8 bytes
    write_descriptor(              16, 15,  RD, action_addr, '1', add_vector(descr_addr1,   60, 32), '0' ), -- 16  read transactions of  16 bytes
    write_descriptor(              32,  7,  RD, action_addr, '1', add_vector(descr_addr1,   80, 32), '0' ), --  8  read transactions of  32 bytes
    write_descriptor(              64,  3, WRT, action_addr, '1', add_vector(descr_addr1,  100, 32), '0' ), --  4 write transactions of  64 bytes
    write_descriptor(             128,  1, WRT, action_addr, '1', add_vector(descr_addr1,  120, 32), '0' ), --  2 write transactions of 128 bytes
    write_descriptor(             256,  0,  RD, action_addr, '1', add_vector(descr_addr1,  140, 32), '1' )  --  1  read transaction  of 256 bytes
  );

  -- Descriptors to load into injector's fifo for test 2 write (size, count, action, addr, addrfix, nextraddr, last)
  constant descriptors2w  : descriptor_bank(0 to 5) := (
    write_descriptor(MAX_SIZE_BURST-3,  0, WRT, action_addr, '0', add_vector(descr_addr2w,  20, 32), '0' ), -- Check if writes the correct ammount below size beat
    write_descriptor(  MAX_SIZE_BURST,  0, WRT, action_addr, '0', add_vector(descr_addr2w,  40, 32), '0' ), -- Check if writes the correct ammount equal size beat
    write_descriptor(MAX_SIZE_BURST+3,  0, WRT, action_addr, '0', add_vector(descr_addr2w,  60, 32), '0' ), -- Check if writes the correct ammount above size beat
    write_descriptor(               3,  0, WRT, action_addr, '1', add_vector(descr_addr2w,  80, 32), '0' ), -- With fix addr, check if reads lower of a word
    write_descriptor(               4,  0, WRT, action_addr, '1', add_vector(descr_addr2w, 100, 32), '0' ), -- With fix addr, check if reads a word
    write_descriptor(              15,  0, WRT, action_addr, '1', add_vector(descr_addr2w, 120, 32), '1' )  -- With fix addr, check if it really fixes the addr
  );

  -- Descriptors to load into injector's fifo for test 2 read (size, count, action, addr, addrfix, nextraddr, last)
  constant descriptors2r  : descriptor_bank(0 to 5) := (
    write_descriptor(MAX_SIZE_BURST-3,  0,  RD, action_addr, '0', add_vector(descr_addr2r,  20, 32), '0' ), -- Check if writes the correct ammount below size beat
    write_descriptor(  MAX_SIZE_BURST,  0,  RD, action_addr, '0', add_vector(descr_addr2r,  40, 32), '0' ), -- Check if writes the correct ammount equal size beat
    write_descriptor(MAX_SIZE_BURST+3,  0,  RD, action_addr, '0', add_vector(descr_addr2r,  60, 32), '0' ), -- Check if writes the correct ammount above size beat
    write_descriptor(               3,  0,  RD, action_addr, '1', add_vector(descr_addr2r,  80, 32), '0' ), -- With fix addr, check if reads lower of a word
    write_descriptor(               4,  0,  RD, action_addr, '1', add_vector(descr_addr2r, 100, 32), '0' ), -- With fix addr, check if reads a word
    write_descriptor(              15,  0,  RD, action_addr, '1', add_vector(descr_addr2r, 120, 32), '1' )  -- With fix addr, check if it really fixes the addr
  );


  -----------------------------------------------------------------------------
  -- Records and types
  -----------------------------------------------------------------------------



  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------

  -- Injector I/O + Initial input values
  signal clk    : std_ulogic  := '0';
  signal rstn   : std_ulogic  := '0';

  signal apbi   : apb_slave_in_type := DEF_INJ_APB;
  signal apbo   : apb_slave_out_type;

  signal bm_mosi: bm_mosi;
  signal bm_miso: bm_miso           := DEF_INJ_BM;

  -- APB configuration
  signal apb_sel: std_logic_vector(apbi.sel'range) := std_logic_vector(shift_left(to_unsigned(1, apbi.sel'length), apbi.sel'length-pindex-1));

  -- Control test signals, used to error if the execution doesn't continue after a threshold
  signal limit_rd_req_grant : integer := 0;
  signal limit_wr_req_grant : integer := 0;
  signal limit_rd_req       : integer := 0;
  signal limit_wr_req       : integer := 0;
  signal limit_descr_compl  : integer := 0;
  signal wait_descr_compl   : std_logic := '0';


  -----------------------------------------------------------------------------
  -- Function/procedure declaration
  -----------------------------------------------------------------------------


begin  -- rtl

  -----------------
  -- Assignments --
  -----------------

  -- Clock generation
    clk <= not clk after T/2;
  
  -----------------------------------------------------------------------------
  -- Sequential process
  -----------------------------------------------------------------------------

  test : process
  begin

    apbi.sel   <= apb_sel; -- Set injector at the APB bus to write configuration
    wait until rising_edge(clk);
    rstn       <= '1';

    ----------------------------------------
    --               TEST 1               --
    ----------------------------------------

    -- Configure injector for test 1
    report "Test 1: Load configuration and start injector!";
    configure_injector(clk, apb_inj_addr, descr_addr1, inj_config1, apbo, apbi);

    -- Load descriptors for test 1
    report "Test 1: Loading descriptor batch!";
    load_descriptors(clk, descriptors1, descr_addr1, bm_mosi, bm_miso);

    -- Test all descriptors from TEST 1 once
    test_descriptor_batch(clk, bm_mosi, bm_miso, descriptors1, MAX_SIZE_BURST, apbo.irq(pirq), wait_descr_compl);
    report "Test 1 descriptor batch has been completed succesfully once!";  
    -- Test all descriptors from TEST 1 for second time (queue test)
    test_descriptor_batch(clk, bm_mosi, bm_miso, descriptors1, MAX_SIZE_BURST, apbo.irq(pirq), wait_descr_compl);
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

    ----------------------------------------
    --               TEST 2               --
    ----------------------------------------

    -- Configure injector for test 2 write
    report "Test 2: Load write configuration and start injector!";
    configure_injector(clk, apb_inj_addr, descr_addr2w, inj_config2, apbo, apbi);

    -- Load descriptors for test 2 write
    report "Test 2: Loading write descriptor batch!";
    load_descriptors(clk, descriptors2w, descr_addr2w, bm_mosi, bm_miso);

    -- Test all descriptors from TEST 2 write
    test_descriptor_batch(clk, bm_mosi, bm_miso, descriptors2w, MAX_SIZE_BURST, apbo.irq(pirq), wait_descr_compl); 
    report "Test 2 descriptor write batch has been completed succesfully!";

    -- Check if the injector is looping execution (non-queue mode shoould not repeat descriptors)
    for i in 0 to 9 loop
      wait until rising_edge(clk); 
    end loop;
    assert (bm_mosi.rd_req or bm_mosi.wr_req) = '0'   report "Test 2: Injector non-queue mode FAILED!" & LF 
                                      & "         The injector is looping descriptors even though it shouldn't due to non-queue mode operation." & LF
                                      & "         (Make sure the configuration of the injector does not assert the queue mode bit)" severity failure;

    -- To reset loaded descriptors, it is required to use the reset low input
    rstn        <= '0';
    wait until rising_edge(clk);  wait until rising_edge(clk);
    rstn        <= '1';
    

    -- Configure injector for test 2 read
    report "Test 2: Load read configuration and start injector!";
    configure_injector(clk, apb_inj_addr, descr_addr2r, inj_config2, apbo, apbi);

    -- Load descriptors for test 2 read
    report "Test 2: Loading read descriptor batch!";
    load_descriptors(clk, descriptors2r, descr_addr2r, bm_mosi, bm_miso);

    -- Test all descriptors from TEST 2 read
    test_descriptor_batch(clk, bm_mosi, bm_miso, descriptors2r, MAX_SIZE_BURST, apbo.irq(pirq), wait_descr_compl); 
    report "Test 2 descriptor read batch has been completed succesfully!";

    -- Check if the injector is looping execution (non-queue mode shoould not repeat descriptors)
    for i in 0 to 9 loop
      wait until rising_edge(clk); 
    end loop;
    assert (bm_mosi.rd_req or bm_mosi.wr_req) = '0'   report "Test 2: Injector non-queue mode FAILED!" & LF 
                                      & "         The injector is looping descriptors even though it shouldn't due to non-queue mode operation." & LF
                                      & "         (Make sure the configuration of the injector does not assert the queue mode bit)" severity failure;

    -- To reset loaded descriptors, it is required to use the reset low input
    rstn        <= '0';
    wait until rising_edge(clk);  wait until rising_edge(clk);
    rstn        <= '1';


    wait for 1 us;
    report "TEST SUCCESSFULLY FINISHED!"; stop; -- VHDL2008
    assert FALSE report "TEST SUCCESSFULLY FINISHED!" severity failure; -- !VHDL2008

  end process test;


  -- Counters used to count how many clk cycles X signals get stuck
  interrupt_test : process(clk)
  begin 
    if(clk = '1' and clk'event) then
      -- Increment counters if the signal stays asserted
      if(bm_miso.rd_req_grant = '1') then limit_rd_req_grant <= limit_rd_req_grant + 1; 
        else limit_rd_req_grant <= 0; end if;
      if(bm_miso.wr_req_grant = '1') then limit_wr_req_grant <= limit_wr_req_grant + 1;
        else limit_wr_req_grant <= 0; end if;
      if(bm_mosi.rd_req = '1') then limit_rd_req <= limit_rd_req + 1;
        else limit_rd_req <= 0; end if;
      if(bm_mosi.wr_req = '1') then limit_wr_req <= limit_wr_req + 1;
        else limit_wr_req <= 0; end if;
      if(wait_descr_compl = '1') then limit_descr_compl <= limit_descr_compl + 1;
        else limit_descr_compl <= 0; end if;
    end if;

    -- Crash test with error if something gets stuck for Y threshold
    if(
      limit_rd_req_grant > req_threshold or
      limit_wr_req_grant > req_threshold
    ) then
      assert FALSE report "TEST GOT STUCK DUE TO REQUEST NOT BEING GRANTED!" severity failure;
    end if;

    if(
      limit_rd_req       > req_threshold or
      limit_wr_req       > req_threshold
    ) then
      assert FALSE report "TEST GOT STUCK DUE TO INJECTOR NOT REQUESTING TRANSACTION!" severity failure;
    end if;

    if(
      limit_descr_compl > descr_compl_thereshold
    ) then
      assert FALSE report "The testbench has finished descriptor but the injector has not set the completion flag (apbo.irq is 0)." severity failure;
    end if;

  end process interrupt_test;


  -----------------------------------------------------------------------------
  -- Component instantiation
  -----------------------------------------------------------------------------

  -- injector core
  core : injector
    generic map (
      dbits         => dbits,
      MAX_SIZE_BURST=> MAX_SIZE_BURST,
      pindex        => pindex,
      paddr         => paddr,
      pmask         => pmask,
      pirq          => pirq
      )
    port map (
      rstn          => rstn,
      clk           => clk,
      apbi          => apbi,
      apbo          => apbo,
      bm0_mosi      => bm_mosi,
      bm0_miso      => bm_miso
      );
  
end architecture rtl;




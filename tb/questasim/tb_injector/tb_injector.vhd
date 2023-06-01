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
-- This testbench validates, previous to the AHB/AXI interface (on IB bus), the following:
--  Load of the descriptors (tests 1 and 2),
--  Transaction completion (test 1 and 2),
--  Transaction repetition (test 1),
--  Queue mode (test 1),
--  Transaction size on fixed/unfixed address (test 2) on both read and writes.
-- 
-- Testbench specification:
-- The testbench simulates the behaviour from a compatible interface checking and setting 
-- the respective Interface Bus (IB) signals.
--
-- For each test (descriptor bank batch), the injector is initialized as if it were at the
-- SELENE platform by sending the equivalent APB signals. 
--
-- This testbench specifically tests the if the address being acted on is the programmed/expected, 
-- taking into account the "fixed address" and "bursts" features. Furthermore, the tests checks if
-- the injector asserts the descriptor completion flag when it's supposed to finish. 
-- However, the injector pipeline allow for requesting a transaction when the last is finishing on the 
-- same clock cycle, feature that this testbench does not validate.
--
-- In addition, the user may change the threshold of how many clock cycles are allowed to pass since
-- some signals assertions, including injector requests, testbench grant requests and testbench
-- waiting for the injector to assert the 'descriptor completed' interruption (this one is 0 by 
-- default because the testbench is arrenged to always check on the interruption when is expected
-- to be asserted).
-- 
-----------------------------------------------------------------------------

entity tb_injector is
  generic (
    -- SafeTI configuration
    PC_LEN            : integer range 2 to   10     :=    4;      -- Set the maximum number of programmable descriptor words to 2^PC_LEN
    CORE_DATA_WIDTH   : integer range 8 to 1024     :=   32;      -- Data width of the injector core. [Only power of 2s allowed]
    MAX_SIZE_BURST    : integer range 8 to 4096     := 4096;      -- Maximum size of a beat at a burst transaction.
    -- Injector configuration
    ASYNC_RST         : boolean                     := TRUE;      -- Allow asynchronous reset flag (default=TRUE)

    -- Validation options
    irq_desc_compl_en : std_logic                   := '0';       -- Enable interruption at descriptor completion
    irq_prog_compl_en : std_logic                   := '0'        -- Enable interruption at program completion
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
  constant inj_base_addr  : std_logic_vector(31 downto 0) := X"0000_0000";  -- Location of the injector at APB memory is not important, since sel and en are used
  constant action_addr1   : std_logic_vector(31 downto 0) := X"0200_0000";  -- Write/read address
  constant action_addr2   : std_logic_vector(31 downto 0) := X"0200_0004";  -- Write/read address


  -- Injector configurations
  -- Freeze at interruption, Interruption enabled due to error at the network, at the injector or 
  -- due to program completion, Queue mode, SW reset, enable injector.

  -- Injector core configuration with Queue mode disabled:
  constant inj_conf       : std_logic_vector(31 downto 0) := X"0000_00" & "0" & not(irq_desc_compl_en or irq_prog_compl_en) & "11" & irq_prog_compl_en & "0" & "0" & "1";

  -- Injector core configuration with Queue mode enabled:
  constant inj_conf_qmode : std_logic_vector(31 downto 0) := X"0000_00" & "0" & not(irq_desc_compl_en or irq_prog_compl_en) & "11" & irq_prog_compl_en & "1" & "0" & "1";

  -- Injector reset:
  constant inj_rst        : std_logic_vector(31 downto 0) := X"0000_00" & "0" & "0" & "000" & "0" & "1" & "0";


  -- Descriptors to load into injector's memory program for test 1 (size, count, action, addr, irq_compl, last)
  constant descriptors1   : descriptor_bank_tb(0 to 6) := (         -- Number of iterations / Transaction type  / Transaction size
    descriptor_rd_wr(               4, 63,     OP_WRITE, action_addr1, irq_desc_compl_en, '0' ), -- 64 write transactions of   4 bytes
    descriptor_rd_wr(               8, 31,     OP_WRITE, action_addr2, irq_desc_compl_en, '0' ), -- 32 write transactions of   8 bytes
    descriptor_rd_wr(              16, 15,      OP_READ, action_addr1, irq_desc_compl_en, '0' ), -- 16  read transactions of  16 bytes
    descriptor_rd_wr(              32,  7,      OP_READ, action_addr2, irq_desc_compl_en, '0' ), --  8  read transactions of  32 bytes
    descriptor_rd_wr(              64,  3,     OP_WRITE, action_addr1, irq_desc_compl_en, '0' ), --  4 write transactions of  64 bytes
    descriptor_rd_wr(             128,  1,     OP_WRITE, action_addr2, irq_desc_compl_en, '0' ), --  2 write transactions of 128 bytes
    descriptor_rd_wr(             256,  0,      OP_READ, action_addr1, irq_desc_compl_en, '1' )  --  1  read transaction  of 256 bytes
  );

  -- Descriptors to load into injector's memory program for test 2 write (size, count, action, addr, irq_compl, last)
  constant descriptors2w  : descriptor_bank_tb(0 to 5) := (
    descriptor_rd_wr(MAX_SIZE_BURST-4,  0,     OP_WRITE, action_addr1, irq_desc_compl_en, '0' ), -- Check if writes the correct ammount below maximum size
    descriptor_rd_wr(  MAX_SIZE_BURST,  0,     OP_WRITE, action_addr2, irq_desc_compl_en, '0' ), -- Check if writes the correct ammount equal maximum size
    descriptor_rd_wr(MAX_SIZE_BURST+4,  0,     OP_WRITE, action_addr1, irq_desc_compl_en, '0' ), -- Check if writes the correct ammount above maximum size
    descriptor_rd_wr(MAX_SIZE_BURST-4,  0, OP_WRITE_FIX, action_addr2, irq_desc_compl_en, '0' ), -- With fix addr, check if writes the correct ammount below max size
    descriptor_rd_wr(  MAX_SIZE_BURST,  0, OP_WRITE_FIX, action_addr1, irq_desc_compl_en, '0' ), -- With fix addr, check if writes the correct ammount equal max size
    descriptor_rd_wr(MAX_SIZE_BURST+4,  0, OP_WRITE_FIX, action_addr2, irq_desc_compl_en, '1' )  -- With fix addr, check if writes the correct ammount above max size
  );

  -- Descriptors to load into injector's memory program for test 2 read (size, count, action, addr, irq_compl, last)
  constant descriptors2r  : descriptor_bank_tb(0 to 5) := (
    descriptor_rd_wr(MAX_SIZE_BURST-4,  0,     OP_READ, action_addr1, irq_desc_compl_en, '0' ), -- Check if reads the correct ammount below maximum size
    descriptor_rd_wr(  MAX_SIZE_BURST,  0,     OP_READ, action_addr2, irq_desc_compl_en, '0' ), -- Check if reads the correct ammount equal maximum size
    descriptor_rd_wr(MAX_SIZE_BURST+4,  0,     OP_READ, action_addr1, irq_desc_compl_en, '0' ), -- Check if reads the correct ammount above maximum size
    descriptor_rd_wr(MAX_SIZE_BURST-4,  0, OP_READ_FIX, action_addr2, irq_desc_compl_en, '0' ), -- With fix addr, check if reads the correct ammount below maximum size
    descriptor_rd_wr(  MAX_SIZE_BURST,  0, OP_READ_FIX, action_addr1, irq_desc_compl_en, '0' ), -- With fix addr, check if reads the correct ammount equal maximum size
    descriptor_rd_wr(MAX_SIZE_BURST+4,  0, OP_READ_FIX, action_addr2, irq_desc_compl_en, '1' )  -- With fix addr, check if reads the correct ammount above maximum size
  );


  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------

  -- Injector I/O + Initial input values
  signal clk      : std_ulogic    := '0';
  signal rstn     : std_ulogic    := '0';

  signal apbi     : apb_slave_in  := DEF_INJ_APB;
  signal apbo     : apb_slave_out;

  signal ib_mosi  : ib_mosi;
  signal ib_miso  : ib_miso       := DEF_INJ_IB;

  -- Control test signals, used to error if the execution doesn't continue after a threshold
  signal limit_rd_req_grant : integer   := 0;
  signal limit_wr_req_grant : integer   := 0;
  signal limit_rd_req       : integer   := 0;
  signal limit_wr_req       : integer   := 0;
  signal limit_descr_compl  : integer   := 0;
  signal wait_descr_compl   : std_logic := '0';


begin  -- rtl

  -----------------
  -- Assignments --
  -----------------

  -- Clock generation
  clk       <= not clk after T/2;
  

  -----------------------------------------------------------------------------
  -- Sequential process
  -----------------------------------------------------------------------------

  test : process
  begin

    wait until rising_edge(clk);
    rstn       <= '1';
    apbi.sel   <= '1';

    ----------------------------------------
    --               TEST 1               --
    ----------------------------------------

    -- Load descriptors for test 1
    report "Test 1: Loading descriptor batch!";
    load_descriptors(clk, inj_base_addr, descriptors1, apbi);

    -- Test all descriptors from TEST 1 (Queue enabled)
    report "Test 1: Load configuration with Queue mode and start injector!";
    test_descriptor_batch(clk, rstn, ib_mosi, ib_miso, apbo, apbi, descriptors1, MAX_SIZE_BURST, 
      CORE_DATA_WIDTH, inj_base_addr, inj_conf_qmode, apbo.irq, wait_descr_compl);
    report "Test 1 descriptor batch has been completed succesfully with Queue mode enabled!";

    -- Load descriptors for test 1
    report "Test 1: Loading descriptor batch!";
    load_descriptors(clk, inj_base_addr, descriptors1, apbi);

    -- Test all descriptors from TEST 1 (Queue disabled)
    test_descriptor_batch(clk, rstn, ib_mosi, ib_miso, apbo, apbi, descriptors1, MAX_SIZE_BURST, 
      CORE_DATA_WIDTH, inj_base_addr, inj_conf, apbo.irq, wait_descr_compl);
    report "Test 1 descriptor batch has been completed succesfully with Queue mode disabled!";


    ----------------------------------------
    --               TEST 2               --
    ----------------------------------------

    -- Load descriptors for TEST 2 write
    report "Test 2: Loading write descriptor batch!";
    load_descriptors(clk, inj_base_addr, descriptors2w, apbi);
    
    -- Test all descriptors from TEST 2 write
    report "Test 2: Load write configuration and start injector!";
    test_descriptor_batch(clk, rstn, ib_mosi, ib_miso, apbo, apbi, descriptors2w, MAX_SIZE_BURST, 
      CORE_DATA_WIDTH, inj_base_addr, inj_conf, apbo.irq, wait_descr_compl); 
    report "Test 2 descriptor write batch has been completed succesfully!";


    -- Load descriptors for TEST 2 read
    report "Test 2: Loading read descriptor batch!";
    load_descriptors(clk, inj_base_addr, descriptors2r, apbi);

    -- Test all descriptors from TEST 2 read
    report "Test 2: Load read configuration and start injector!";
    test_descriptor_batch(clk, rstn, ib_mosi, ib_miso, apbo, apbi, descriptors2r, MAX_SIZE_BURST, 
      CORE_DATA_WIDTH, inj_base_addr, inj_conf, apbo.irq, wait_descr_compl); 
    report "Test 2 descriptor read batch has been completed succesfully!";


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
      if(ib_miso.rd_req_grant = '1' and ib_mosi.rd_req = '0') then limit_rd_req_grant <= limit_rd_req_grant + 1; 
        else limit_rd_req_grant <= 0; end if;
      if(ib_miso.wr_req_grant = '1' and ib_mosi.wr_req = '0') then limit_wr_req_grant <= limit_wr_req_grant + 1;
        else limit_wr_req_grant <= 0; end if;
      if(ib_mosi.rd_req = '1' and ib_miso.rd_req_grant = '0') then limit_rd_req <= limit_rd_req + 1;
        else limit_rd_req <= 0; end if;
      if(ib_mosi.wr_req = '1' and ib_miso.wr_req_grant = '0') then limit_wr_req <= limit_wr_req + 1;
        else limit_wr_req <= 0; end if;
      if(wait_descr_compl = '1') then limit_descr_compl <= limit_descr_compl + 1;
        else limit_descr_compl <= 0; end if;
      
      -- Check for interruption error (only when irq_desc_compl_en and irq_prog_compl_en are '0')
      if(irq_desc_compl_en = '0' and irq_prog_compl_en = '0') then
        assert apbo.irq = '0' report "INJECTOR CORE HAS AN ERROR!" severity failure;
      end if;
    end if;

    -- Crash test with error if something gets stuck for Y threshold
    if(
      limit_rd_req_grant > req_threshold or
      limit_wr_req_grant > req_threshold
    ) then
      assert FALSE report "TEST GOT STUCK DUE TO INJECTOR NOT REQUESTING TRANSACTION!" severity failure;
    end if;

    if(
      limit_rd_req       > req_threshold or
      limit_wr_req       > req_threshold
    ) then
      assert FALSE report "TEST GOT STUCK DUE TO INJECTOR TRANSACTION REQUEST NOT BEING RESPONDED!" severity failure;
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
  core : injector_core
    generic map (
      PC_LEN          => PC_LEN,
      CORE_DATA_WIDTH => CORE_DATA_WIDTH,
      MAX_SIZE_BURST  => MAX_SIZE_BURST,
      ASYNC_RST       => ASYNC_RST
      )
    port map (
      rstn            => rstn,
      clk             => clk,
      apbi            => apbi,
      apbo            => apbo,
      ib_out          => ib_mosi,
      ib_in           => ib_miso
      );
  

end architecture rtl;




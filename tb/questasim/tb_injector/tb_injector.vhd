-----------------------------------------------------------------------------   
-- Entity:      tb_injector
-- File:        tb_injector.vhd
-- Author:      Francis Fuentes
-- Description: Testbench injector top level entity.
------------------------------------------------------------------------------ 
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library bsc;
use bsc.injector_pkg.all;
use bsc.tb_injector_pkg.all;

-----------------------------------------------------------------------------
-- Top level testbench entity for injector.
--
-- This testbench tests, previous to the AHB/AXI interface (on BM bus), the following:
--  Load of the descriptors (tests 1 and 2),
--  Transaction repetition (test 1),
--  Queue mode (test 1),
--  Injector reset (end of test 1 and to change between read and write vectors on test 2),
--  Transaction size on fixed/unfixed address (test 2) on both read and writes.
-- 
-----------------------------------------------------------------------------

entity tb_injector is
  generic (
    tech              : integer range  0 to numTech         := 65;  -- Target technology
    -- APB configuration  
    pindex            : integer                             := 6;         -- APB configuartion slave index
    paddr             : integer                             := 16#850#;   -- APB configuartion slave address
    pmask             : integer                             := 16#FFF#;   -- APB configuartion slave mask
    pirq              : integer range  0 to APB_IRQ_NMAX-1  := 6;         -- APB configuartion slave irq
    -- Bus master configuration
    dbits             : integer range 32 to 128             := 32;        -- Data width of BM and FIFO    
    hindex            : integer                             := 5;         -- AHB master index
    MAX_SIZE_BEAT     : integer range 32 to 1024            := 1024;      -- Maximum size of a beat at a burst transaction.
    -- Injector configuration
    ASYNC_RST         : boolean                             := FALSE      -- Allow asynchronous reset flag
    );

end entity tb_injector;

architecture rtl of tb_injector is
  -----------------------------------------------------------------------------
  -- Constant declaration
  -----------------------------------------------------------------------------

  -- Pointers
  constant apb_inj_addr   : std_logic_vector(31 downto 0) := X"000" & std_logic_vector(to_unsigned(paddr, 12)) & X"00"; -- Location of the injector at APB memory
  constant descr_addr1    : std_logic_vector(31 downto 0) := X"0100_0000";  -- First descriptor MSB address for test 1
  constant descr_addr2w   : std_logic_vector(31 downto 0) := X"0110_0000";  -- First descriptor MSB address for test 2 writes
  constant descr_addr2r   : std_logic_vector(31 downto 0) := X"0120_0000";  -- First descriptor MSB address for test 2 reads
  constant action_addr    : std_logic_vector(31 downto 0) := X"0200_0000";  -- Write/read address

  -- Injector configurations
  -- Test 1 configuration: Queue mode enabled, enable interrupt on error, interrupt enabled, (kick disabled), reset, start injector.
  constant inj_config1    : std_logic_vector(31 downto 0) := X"0000_00" & "00" & "111001";
  
  -- Test 2 configuration: (Queue mode disabled), enable interrupt on error, interrupt enabled, (kick disabled), reset, start injector.
  constant inj_config2    : std_logic_vector(31 downto 0) := X"0000_00" & "00" & "011001";

  -- Descriptors to load into injector's fifo for test 1 (size, count, action, addr, addrfix, nextraddr, last)
  constant descriptors1   : descriptor_bank(0 to 6) := (
    write_descriptor(               4, 63, WRT,  action_addr, '1', add_vector(descr_addr1,   0, 32), '0'),-- 64 write transactions of   4 bytes
    write_descriptor(               8, 31, WRT,  action_addr, '1', add_vector(descr_addr1,  20, 32), '0'),-- 32 write transactions of   8 bytes
    write_descriptor(              16, 15,  RD,  action_addr, '1', add_vector(descr_addr1,  40, 32), '0'),-- 16  read transactions of  16 bytes
    write_descriptor(              32,  7,  RD,  action_addr, '1', add_vector(descr_addr1,  60, 32), '0'),--  8  read transactions of  32 bytes
    write_descriptor(              64,  3, WRT,  action_addr, '1', add_vector(descr_addr1,  80, 32), '0'),--  4 write transactions of  64 bytes
    write_descriptor(             128,  1, WRT,  action_addr, '1', add_vector(descr_addr1, 100, 32), '0'),--  2 write transactions of 128 bytes
    write_descriptor(             256,  0,  RD,  action_addr, '1', add_vector(descr_addr1, 120, 32), '1') --  1  read transaction  of 256 bytes
  );

  -- Descriptors to load into injector's fifo for test 2 write (size, count, action, addr, addrfix, nextraddr, last)
  constant descriptors2w  : descriptor_bank(0 to 5) := (
    write_descriptor( MAX_SIZE_BEAT-3,  0, WRT,  action_addr, '0', add_vector(descr_addr1,   0, 32), '0' ), -- Check if writes the correct ammount below size beat
    write_descriptor(   MAX_SIZE_BEAT,  0, WRT,  action_addr, '0', add_vector(descr_addr1,  20, 32), '0' ), -- Check if writes the correct ammount equal size beat
    write_descriptor( MAX_SIZE_BEAT+3,  0, WRT,  action_addr, '0', add_vector(descr_addr1,  40, 32), '0' ), -- Check if writes the correct ammount above size beat
    write_descriptor(               3,  0, WRT,  action_addr, '1', add_vector(descr_addr1,  60, 32), '0' ), -- With fix addr, check if reads lower of a word
    write_descriptor(               4,  0, WRT,  action_addr, '1', add_vector(descr_addr1,  80, 32), '0' ), -- With fix addr, check if reads a word
    write_descriptor(              15,  0, WRT,  action_addr, '1', add_vector(descr_addr1, 100, 32), '1' )  -- With fix addr, check if it really fixes the addr
  );

  -- Descriptors to load into injector's fifo for test 2 read (size, count, action, addr, addrfix, nextraddr, last)
  constant descriptors2r  : descriptor_bank(0 to 5) := (
    write_descriptor( MAX_SIZE_BEAT-3,  0,   RD, action_addr, '0', add_vector(descr_addr1,   0, 32), '0' ), -- Check if writes the correct ammount below size beat
    write_descriptor(   MAX_SIZE_BEAT,  0,   RD, action_addr, '0', add_vector(descr_addr1,  20, 32), '0' ), -- Check if writes the correct ammount equal size beat
    write_descriptor( MAX_SIZE_BEAT+3,  0,   RD, action_addr, '0', add_vector(descr_addr1,  40, 32), '0' ), -- Check if writes the correct ammount above size beat
    write_descriptor(               3,  0,   RD, action_addr, '1', add_vector(descr_addr1,  60, 32), '0' ), -- With fix addr, check if reads lower of a word
    write_descriptor(               4,  0,   RD, action_addr, '1', add_vector(descr_addr1,  80, 32), '0' ), -- With fix addr, check if reads a word
    write_descriptor(              15,  0,   RD, action_addr, '1', add_vector(descr_addr1, 100, 32), '1' )  -- With fix addr, check if it really fixes the addr
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

  signal apbo   : apb_slave_out_type;
  signal bm_in  : bm_in_type;

  signal apbi   : apb_slave_in_type := DEF_INJ_APB;
  signal bm_out : bm_out_type       := DEF_INJ_BM;

  -- APB configuration
  signal apb_sel: std_logic_vector(apbi.sel'range) := std_logic_vector(shift_left(to_unsigned(1, apbi.sel'length), apbi.sel'length-pindex-1));

  
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

    -- Configure injector for test 1
    apbi.sel   <= apb_sel;
    apbi.addr  <= add_vector(apb_inj_addr, 8, apbi.addr'length); -- Write 1st descriptor pointer
    apbi.wdata <= descr_addr1;  -- Test 1 descriptors pointer

    wait for T + 3 ns;
    rstn       <= '1';
    apbi.write <= '1';
    wait until falling_edge(clk);
    apbi.en    <= '1';
    wait until falling_edge(clk);
    apbi.en    <= '0';
    apbi.addr  <= apb_inj_addr; -- Configure and start injector
    apbi.wdata <= inj_config1;  -- Test 1 configuration
    wait until falling_edge(clk);
    apbi.en    <= '1';
    wait until falling_edge(clk);
    apbi.en    <= '0';

    wait until rising_edge(bm_in.rd_req);
    apbi.addr  <= (others => '0');
    apbi.write <= '0';

    -- Load descriptors
    if(bm_in.rd_addr = descr_addr1) then
      read_descr(descriptors1, bm_out.rd_data, bm_out.rd_valid, bm_out.rd_done, bm_in.rd_req, bm_out.rd_req_grant);
    else assert FALSE report "Test 1: load descriptors FAILED!" & LF & "         Injector is fetching descriptors at a wrong address." severity failure;
    end if;           report "Test 1: load descriptors passed!" & LF & "         Executing Test 1.";
    
    -- Check if the descriptors have been loaded correctly
    if(FALSE) then -- It seems that VHDL doesn't allow to tap into component signals. Check ram!=descriptors if found a way to do it.
      assert FALSE report "Test 1: load configuration FAILED!" & LF & "         Something wrong has happened while loading descriptors into the FIFO." severity failure;
    else           report "Test 1: load configuration passed!" & LF & "         Starting to load descriptors.";
    end if;

    test_descriptor_batch(bm_in, descriptors1, bm_out, MAX_SIZE_BEAT); -- Test all descriptors from TEST 1 once

    assert FALSE report "TEST FINISHED" severity failure;

  end process test;

  -- Everytime a read/write transaction is completed, the respective done flag is asserted for a single clock cycle.
  -- This action can be verified by reading the bit 0 at the APB memory 0x20 of the injector, 2 clock cycles after the desassertation of the done signal.
  completed_transaction : process (bm_out.rd_done, bm_out.wr_done)
  begin
    apbi.sel  <= apb_sel; -- <- Line probably not required
    apbi.en   <= '1';
    apbi.addr <= add_vector(apb_inj_addr, x"20", apbi.addr'length);
    wait for T;
    assert apbo.rdata(0) report "The injector is not reporting 'done' at dbg.sts after a write/read transaction!" severity failure;
    apbi.en   <= '0';
  end process;


  -----------------------------------------------------------------------------
  -- Component instantiation
  -----------------------------------------------------------------------------

  -- injector core
  core : injector
    generic map (
      tech          => tech,
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
  
end architecture rtl;




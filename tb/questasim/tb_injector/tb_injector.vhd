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
  constant apb_injector   : std_logic_vector(31 downto 0) := 16#000# & paddr & 16#00#; -- Location of the injector at APB memory
  constant descr_addr1    : std_logic_vector(31 downto 0) := X"0100_0000";  -- First descriptor MSB address for test 1
  constant descr_addr2w   : std_logic_vector(31 downto 0) := X"0110_0000";  -- First descriptor MSB address for test 2 writes
  constant descr_addr2r   : std_logic_vector(31 downto 0) := X"0120_0000";  -- First descriptor MSB address for test 2 reads
  constant action_addr    : std_logic_vector(31 downto 0) := X"0200_0000";  -- Write/read address

  -- Injector configurations
  -- Test 1 configuration: Queue mode enabled, enable interrupt on error, (interrupt disabled), (kick disabled), nreset, start injector.
  constant inj_config1    : std_logic_vector(31 downto 0) := X"0000_00" & "00" & "110011";
  
  -- Test 2 configuration: (Queue mode disabled), enable interrupt on error, (interrupt disabled), (kick disabled), nreset, start injector.
  constant inj_config2    : std_logic_vector(31 downto 0) := X"0000_00" & "00" & "010011";

  -- Descriptors to load into injector's fifo for test 1 (size, count, action, addr, addrfix, nextraddr, last)
  constant descriptors1   : descriptor_bank(0 to 7) := (
    write_descriptor(               4, 63, WRT,  action_addr, '1',  descr_addr1 +   0, '0' ) &  -- 64 write transactions of   4 bytes
    write_descriptor(               8, 31, WRT,  action_addr, '1',  descr_addr1 +  20, '0' ) &  -- 32 write transactions of   8 bytes
    write_descriptor(              16, 15,  RD,  action_addr, '1',  descr_addr1 +  40, '0' ) &  -- 16  read transactions of  16 bytes
    write_descriptor(              32,  7,  RD,  action_addr, '1',  descr_addr1 +  60, '0' ) &  --  8  read transactions of  32 bytes
    write_descriptor(              64,  3, WRT,  action_addr, '1',  descr_addr1 +  80, '0' ) &  --  4 write transactions of  64 bytes
    write_descriptor(             128,  1, WRT,  action_addr, '1',  descr_addr1 + 100, '0' ) &  --  2 write transactions of 128 bytes
    write_descriptor(             256,  0,  RD,  action_addr, '1',  descr_addr1 + 120, '1' )    --  1  read transaction  of 256 bytes
  );

  -- Descriptors to load into injector's fifo for test 2 write (size, count, action, addr, addrfix, nextraddr, last)
  constant descriptors2w  : descriptor_bank(0 to 5) := (
    write_descriptor( MAX_SIZE_BEAT-3,  0,  WRT,  action_addr, '0', descr_addr2w +   0, '0' ), -- Check if writes the correct ammount below size beat
    write_descriptor(   MAX_SIZE_BEAT,  0,  WRT,  action_addr, '0', descr_addr2w +  20, '0' ), -- Check if writes the correct ammount equal size beat
    write_descriptor( MAX_SIZE_BEAT+3,  0,  WRT,  action_addr, '0', descr_addr2w +  40, '0' ), -- Check if writes the correct ammount above size beat
    write_descriptor(               3,  0,  WRT,  action_addr, '1', descr_addr2w +  60, '0' ), -- With fix addr, check if reads lower of a word
    write_descriptor(               4,  0,  WRT,  action_addr, '1', descr_addr2w +  80, '0' ), -- With fix addr, check if reads a word
    write_descriptor(              15,  0,  WRT,  action_addr, '1', descr_addr2w + 100, '1' )  -- With fix addr, check if it really fixes the addr
  );

  -- Descriptors to load into injector's fifo for test 2 read (size, count, action, addr, addrfix, nextraddr, last)
  constant descriptors2r  : descriptor_bank(0 to 5) := (
    write_descriptor( MAX_SIZE_BEAT-3,  0,   RD,  action_addr, '0', descr_addr2r +   0, '0' ), -- Check if writes the correct ammount below size beat
    write_descriptor(   MAX_SIZE_BEAT,  0,   RD,  action_addr, '0', descr_addr2r +  20, '0' ), -- Check if writes the correct ammount equal size beat
    write_descriptor( MAX_SIZE_BEAT+3,  0,   RD,  action_addr, '0', descr_addr2r +  40, '0' ), -- Check if writes the correct ammount above size beat
    write_descriptor(               3,  0,   RD,  action_addr, '1', descr_addr2r +  60, '0' ), -- With fix addr, check if reads lower of a word
    write_descriptor(               4,  0,   RD,  action_addr, '1', descr_addr2r +  80, '0' ), -- With fix addr, check if reads a word
    write_descriptor(              15,  0,   RD,  action_addr, '1', descr_addr2r + 100, '1' )  -- With fix addr, check if it really fixes the addr
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
    apbi.sel   <= std_logic_vector(shift_left(16#0000_0001#, pindex), 32);
    apbi.addr  <= apb_injector + 8; -- Write 1st descriptor pointer
    apbi.wdata <= descr_addr1;      -- Test 1 descriptors pointer

    wait for T + 3 ns;
    rstn <= '1';
    wait for rising_edge(clk);
    apbi.write <= '1';
    wait for T;
    apbi.write <= '0';
    apbi.addr  <= apb_injector; -- Configure injector
    apbi.wdata <= inj_config1;  -- Test 1 configuration
    wait for T;
    apbi.write <= '1';
    wait for 3*T;

    if(FALSE) then
      assert TRUE report "Test 1: load configuration FAILED!" severity error;
    else          report "Test 1: load configuration passed!\n Starting to load descriptors.\n\n";
    end if;

    wait for rising_edge(bm_in.rd_req);
    if(bm_in.rd_addr = descr_addr1) then
        read_descr(descriptors1, bm_out.rd_data, bm_out.rd_valid, bm_out.rd_done, bm_in.rd_req, bm_out.rd_req_grant);
    else assert TRUE  report "Test 1: load descriptors FAILED!" severity error;
    end if;           report "Test 1: load descriptors passed!\n Executing Test 1.\n\n";

  end process test;


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




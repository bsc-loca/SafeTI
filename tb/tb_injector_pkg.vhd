-----------------------------------------------------------------------------   
-- Entity:      tb_injector_pkg
-- File:        tb_injector_pkg.vhd
-- Author:      Francis Fuentes
-- Description: Package for injector testbenches.
------------------------------------------------------------------------------ 
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library bsc;
use bsc.injector_pkg.all;

-----------------------------------------------------------------------------
-- Top level entity for injector.
-- This is a wrapper which integrates injector core to the
-- AHB master - generic bus master bridge
-----------------------------------------------------------------------------

package tb_injector_pkg is

  -----------------------------------------------------------------------------
  -- Constant declaration
  -----------------------------------------------------------------------------

  constant T : time := 5 ns; -- Clock cycle period
  
  -- Injector actions
  constant RD     : std_logic_vector(2 downto 0) := "000"; -- Read
  constant WRT    : std_logic_vector(2 downto 0) := "001"; -- Write
  constant DELY   : std_logic_vector(2 downto 0) := "010"; -- Delay (wait)

    -- APB input stimulus to start injector
  constant DEF_INJ_APB : apb_slave_in_type := (
    sel     => (others => '0'),
    en      => '0',
    addr    => (others => '0'),
    write   => '0',
    wdata   => (others => '0'),
    irq     => (others => '0'),
    ten     => '0',
    trst    => '1',
    scnen   => '0',
    touten  => '1',
    tinen   => (others => '0')
  );

  -- BM output (injector input) default state
  constant DEF_INJ_BM : bm_out_type := (
    rd_data   => (others => '0'),
    rd_req_grant => '1',
    rd_valid  => '0',
    rd_done   => '0',
    rd_err    => '0',
    wr_req_grant => '1',
    wr_full   => '1',
    wr_done   => '0',
    wr_err    => '0'
  );

  -----------------------------------------------------------------------------
  -- Records and types
  -----------------------------------------------------------------------------

  -- Since we are working with many descriptors, it's better to use both types
  type descriptor_words is array (0 to 4) of std_logic_vector(31 downto 0); -- 5 word descriptor
  type descriptor_bank  is array (natural range <>) of descriptor_words;    -- X number of descriptors

  -----------------------------------------------------------------------------
  -- Function/procedure declaration
  -----------------------------------------------------------------------------

  -- Function used to generate descriptors to be loaded into the injector
  function write_descriptor(size      : integer range 0 to 524287;      -- Total size of a transfer
                            count     : integer range 0 to 63;          -- Number of repeats of the transfer
                            action    : std_logic_vector(2 downto 0);   -- Transaction type (read, write, delay)
                            addr      : std_logic_vector(31 downto 0);  -- Initial address to apply transaction
                            addrfix   : std_ulogic;                     -- Address to write/read is fixed when asserted
                            nextaddr  : std_logic_vector(31 downto 0);  -- Address to load from the next descriptor
                            last      : std_ulogic                      -- Last descriptor flag
  ) return descriptor_words;

  -- Procedure used to simulate a memory fetch to load descriptors for a test
  procedure read_descr(
    variable descriptor_bank  : in  descriptor_bank;
    signal   rdata            : out std_logic_vector(31 downto 0);
    signal   valid            : out std_logic := '0';
    signal   done             : out std_logic := '0';
    signal   req              : in  std_logic;
    signal   req_grant        : out std_logic := '1'
  );

end package tb_injector_pkg;

package body tb_injector_pkg is

  -----------------------------------------------------------------------------
  -- Sequential process
  -----------------------------------------------------------------------------

  function write_descriptor(size      : integer range 0 to 524287;      -- Total size of a transfer
                            count     : integer range 0 to 63;          -- Number of repeats of the transfer
                            action    : std_logic_vector(2 downto 0);   -- Transaction type (read, write, delay)
                            addr      : std_logic_vector(31 downto 0);  -- Initial address to apply transaction
                            addrfix   : std_ulogic;                     -- Address to write/read is fixed when asserted
                            nextaddr  : std_logic_vector(31 downto 0);  -- Address to load from the next descriptor
                            last      : std_ulogic                      -- Last descriptor flag
  ) return descriptor_words is 
    variable descr_words          : descriptor_words  := (others => (others => '0'));
    variable src_addr, dest_addr  : std_ulogic        := '0';
    variable src_fix_addr, dest_fix_addr, size_std, count_std  : std_logic_vector(31 downto 0) := (others => '0');

  begin
    size_std  := std_logic_vector( unsigned(size,  size_std'length ) );
    count_std := std_logic_vector( unsigned(count, count_std'length) );

    case action is
      when READ =>
        src_addr      := addr;
        src_fix_addr  := addrfix;

      when WRITE =>
        dest_addr     := addr;
        dest_fix_addr := addrfix;

      when others =>
        null;
    end case;

    descr_words := (
      size_std              & --0x00 Injector control word
      count_std             &
      dest_fix_addr         & 
      src_fix_addr          &
      '1'                   & -- enable interrupt on descriptor completion
      action                &
      '1'                   , -- enable descriptor
      nextaddr(31 downto 1) & --0x04 Injector First descriptor pointer
      last                  , 
      dest_addr             , --0x08 Write address
      src_addr              , --0x0C Read address
      16#0000_0000#           --0x10 Descriptor status word (for future implementation)
      );

    return descr_words;
  end write_descriptor;


  procedure read_descr(
    variable descriptor_bank  : in  descriptor_bank;
    signal   rdata            : out std_logic_vector(31 downto 0);
    signal   valid            : out std_logic := '0';
    signal   done             : out std_logic := '0';
    signal   req              : in  std_logic;
    signal   req_grant        : out std_logic := '1'
  ) is 
    variable descriptor       :     descriptor_words;

  begin
    for j in descriptor_bank'range loop -- Loop between descriptors
      descriptor := descriptor_bank(j);
      wait for falling_edge(req);
      req_grant <= '0';

      for i in descriptor'range loop -- Loop for a single descriptor
        valid <= '0';
        rdata <= descriptor(i);
        wait for T;
        done  <= (i = (descriptor'length - 1));
        valid <= '1';
        wait for T;
      end loop;
      
      rdata <= (others => '0');
      done  <= '0';
      valid <= '0';
    end loop;
  end read_descr;


  -----------------------------------------------------------------------------
  -- Component instantiation
  -----------------------------------------------------------------------------
  


end package body tb_injector_pkg;




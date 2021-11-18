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

  -- Function used to generate a descriptor. They're created with the enable and interrupt flags asserted.
  function write_descriptor(size      : integer range 0 to 524287;      -- Total size of a transfer
                            count     : integer range 0 to 63;          -- Number of repeats of the transfer
                            action    : std_logic_vector(2 downto 0);   -- Transaction type (read, write, delay)
                            addr      : std_logic_vector(31 downto 0);  -- Initial address to apply transaction
                            addrfix   : std_ulogic;                     -- Address to write/read is fixed when asserted
                            nextaddr  : std_logic_vector(31 downto 0);  -- Address to load from the next descriptor
                            last      : std_ulogic                      -- Last descriptor flag
  ) return descriptor_words;

  -- Procedure used to simulate a memory fetch to load descriptors for a test.
  procedure read_descr(
    constant descriptor_bank  : in  descriptor_bank;
    signal   rdata            : out std_logic_vector(127 downto 0);
    signal   valid            : out std_logic;
    signal   done             : out std_logic;
    signal   req              : in  std_logic;
    signal   req_grant        : out std_logic
  );

  -- Procedure used to execute read/write transactions.
  procedure test_descriptor_batch(
    signal   bmin   : in  bm_in_type;
    signal   bmout  : out bm_out_type;
    constant descr  : in  descriptor_bank
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
    variable descr_words                  : descriptor_words  := (others => (others => '0'));
    variable src_fix_addr, dest_fix_addr  : std_ulogic        := '0';
    variable src_addr, dest_addr          : std_logic_vector(31 downto 0) := (others => '0');
    variable size_std                     : std_logic_vector(18 downto 0) := (others => '0');
    variable count_std                    : std_logic_vector(5 downto 0)  := (others => '0');

  begin
    size_std  := std_logic_vector( to_unsigned(size,  size_std'length ) );
    count_std := std_logic_vector( to_unsigned(count, count_std'length) );

    case action is
      when RD  =>
        src_addr      := addr;
        src_fix_addr  := addrfix;

      when WRT =>
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
      x"00000000"             --0x10 Descriptor status word (for future implementation)
      );

    return descr_words;
  end function write_descriptor;


  procedure read_descr(
    constant descriptor_bank  : in  descriptor_bank;
    signal   rdata            : out std_logic_vector(127 downto 0);
    signal   valid            : out std_logic;
    signal   done             : out std_logic;
    signal   req              : in  std_logic;
    signal   req_grant        : out std_logic
  ) is 
    variable descriptor       :     descriptor_words;

  begin
    for j in descriptor_bank'range loop -- Loop between descriptors
      descriptor := descriptor_bank(j);
      req_grant <= '1';
      done  <= '0';
      valid <= '0';
      wait until falling_edge(req);
      req_grant <= '0';

      for i in descriptor'range loop -- Loop for a single descriptor
        valid <= '0';
        rdata <= descriptor(i) & X"00000000_00000000_00000000";
        wait for T;
        if (i = (descriptor'length - 1)) then done <= '1';
        else done <= '0';
        end if;
        valid <= '1';
        wait for T;
      end loop;
      
      rdata <= (others => '0');
      done  <= '0';
      valid <= '0';
    end loop;
  end procedure read_descr;


  procedure test_descriptor_batch(
    signal   bmin       : in  bm_in_type;
    signal   bmout      : out bm_out_type;
    constant descr_bnk  : in  descriptor_bank;
    constant MAX_BEAT   : in  integer
  ) is
    variable descr_wrd  :     descriptor_words; -- Descriptor being executed
    variable tot_size   :     integer := 0; -- Remaining bytes to read/write from total transfer
    variable beat_size  :     integer := 0; -- Remaining bytes to read/write from beat transfer
    variable addr_off   :     integer := 0; -- Address offset where to execute the transfer beat
    variable addr_act   :     std_logic_vector(31 downto 0); -- Source/destination address

  begin
    -- Wait for transaction request
    wait until rising_edge(bmin.rd_req) or rising_edge(bmin.wr_req);

    -- Loop for every descriptor transaction
    for descr_num in descr_bnk'range loop
      descr_wrd := descr_bnk(descr_num);
      -- Loop descriptor transaction for number of repetitions
      for repet_count in to_integer(unsigned(descr_wrd(0)(12 downto 7))) to 0 loop
        addr_off := 0;


        -- Read transaction
        if(bmin.rd_req = '1') then
          tot_size := to_integer(unsigned(bmin.rd_size)) + 1;

          -- Due to maximum size beats in bursts, big transactions must be sliced in MAX_SIZE_BEAT beats.
          while (tot_size > 0) loop 

            -- Size management
            if(tot_size > MAX_BEAT) then 
              beat_size := MAX_BEAT;
              tot_size  := tot_size - MAX_BEAT;
            else 
              beat_size := tot_size;
              tot_size  := 0;
            end if;

            -- Compute address offset if the transfer is not address fixed
            if(descr_wrd(0)(5) = '0') then addr_off := to_integer(unsigned(bmin.rd_size)) + 1 - tot_size; end if;

            -- Check if injector is reading on the correct address
            addr_act := add_vector(descr_wrd(3), addr_off, addr_act'length);
            assert (bmin.rd_addr = addr_act) report  "Wrong address fetched for read transaction!" & LF & "Expected 0x"
                                      & to_hstring(addr_act) & " address, but injector fetched at 0x" 
                                      & to_hstring(bmin.rd_addr) & "." severity failure;
                                      
            -- Start reading beat
            wait until falling_edge(bmin.rd_req); -- Putting this line here allows to manage beats
            bmout.rd_req_grant <= '0';
            
            while (beat_size > 4) loop -- This loop may be unnecessary for the injector, but simulates SELENE platform behaviour
              beat_size := beat_size - 4;
              wait for T;       -- May change to random wait
              bmout.rd_valid <= '1';
              if(beat_size > 4) then bmout.rd_done <= '1'; end if; -- Last read must also have done asserted.
              wait for T;
              bmout.rd_valid <= '0';
              bmout.rd_done  <= '0';
            end loop;

            -- Finished reading a beat
            bmout.rd_req_grant <= '1'; -- Prepare for next reading beat

          end loop; -- BURST loop
        end if;


        -- Write transaction
        if(bmin.wr_req = '1') then
          tot_size := to_integer(unsigned(bmin.wr_size)) + 1;

          -- Due to maximum size beats in bursts, big transactions must be sliced in MAX_SIZE_BEAT beats.
          while (tot_size > 0) loop 

            -- Size management
            if(tot_size > MAX_BEAT) then
              beat_size := MAX_BEAT;
              tot_size  := tot_size - MAX_BEAT;
            else 
              beat_size := tot_size;
              tot_size  := 0;
            end if;

            -- Compute address offset if the transfer is not address fixed
            if(descr_wrd(0)(6) = '0') then addr_off := to_integer(unsigned(bmin.wr_size)) + 1 - tot_size; end if;

            -- Check if injector is writing on the correct address
            addr_act := add_vector(descr_wrd(2), addr_off, addr_act'length);
            assert (bmin.wr_addr = addr_act) report  "Wrong address fetched for read transaction!" & LF & "Expected 0x"
                                      & to_hstring(addr_act) & " address, but injector fetched at 0x" 
                                      & to_hstring(bmin.wr_addr) & "." severity failure;
            
            -- Start writting beat
            wait until falling_edge(bmin.wr_req); -- Putting this line here allows to manage beats
            bmout.wr_req_grant <= '0';

            while (beat_size > 4) loop 
              beat_size := beat_size - 4;
              bmout.wr_full <= '0';
              wait for T;       -- May change to random wait
              bmout.wr_full <= '1';
              wait for T;
              if(beat_size > 4) then bmout.wr_done <= '1'; end if; -- Last write must also have done raised.
            end loop;
            
            -- Finished reading a beat
            wait for T;
            bmout.wr_done <= '0';
            bmout.wr_req_grant <= '1'; -- Prepare for next writing beat

          end loop; -- BURST loop

        end if;
      end loop; -- Loop for repetitions
    end loop; -- Loop for descriptors

  end procedure test_descriptor_batch;

  -----------------------------------------------------------------------------
  -- Component instantiation
  -----------------------------------------------------------------------------
  


end package body tb_injector_pkg;




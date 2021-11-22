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
    rd_req_grant => '0',
    rd_valid  => '0',
    rd_done   => '0',
    rd_err    => '0',
    wr_req_grant => '0',
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

  -- Procedure used to configure and start injector through the APB bus (injector is APB slave, which requires the psel to be set)
  procedure configure_injector(
    signal   clk              : in  std_ulogic;
    constant apb_inj_addr     : in  std_logic_vector(31 downto 0);
    constant descr_addr       : in  std_logic_vector(31 downto 0);
    constant inj_config       : in  std_logic_vector(31 downto 0);
    signal   apbo             : in  apb_slave_out_type;
    signal   apbi             : out apb_slave_in_type
  );

  -- Procedure used to simulate a memory fetch to load descriptors for a test.
  procedure load_descriptors(
    signal   clk              : in  std_ulogic;
    constant descriptor_bank  : in  descriptor_bank;  -- Descriptor batch to offer at rdata
    constant descriptor_addr  : in  std_logic_vector(31 downto 0); -- 1st descriptor address
    signal   bm_in            : in  bm_in_type;       -- BM bus set by injector
    signal   bm_out           : out bm_out_type       -- BM bus set by testbench
  );

  -- Procedure used to execute read/write transactions.
  procedure test_descriptor_batch(
    signal   clk              : in  std_ulogic;
    signal   bmin             : in  bm_in_type;       -- BM bus set by injector
    signal   bmout            : out bm_out_type;      -- BM bus set by testbench
    constant descr_bnk        : in  descriptor_bank;  -- Descriptor batch to be executed
    constant MAX_BEAT         : in  integer;          -- Maximum length in bytes for a beat in burst
    signal   irq              : in  std_logic         -- Descriptor completion flag
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


  procedure configure_injector(
    signal   clk              : in  std_ulogic;
    constant apb_inj_addr     : in  std_logic_vector(31 downto 0);
    constant descr_addr       : in  std_logic_vector(31 downto 0);
    constant inj_config       : in  std_logic_vector(31 downto 0);
    signal   apbo             : in  apb_slave_out_type;
    signal   apbi             : out apb_slave_in_type
  ) is
  begin
    apbi.en    <= '0';
    apbi.addr  <= add_vector(apb_inj_addr, 8, apbi.addr'length); -- 1st descriptor pointer address
    apbi.wdata <= descr_addr;   -- 1st descriptor pointer
    wait until rising_edge(clk);
    apbi.en    <= '1';
    apbi.write <= '1';
    wait until rising_edge(clk);
    apbi.en    <= '0';
    apbi.addr  <= apb_inj_addr; -- Configure and start injector
    apbi.wdata <= inj_config;   -- Test 1 configuration
    wait until rising_edge(clk);
    report "Starting the injector!";
    apbi.en    <= '1';
    wait until rising_edge(clk);
    apbi.en    <= '0';
    apbi.write <= '0';
    apbi.addr  <= (others => '0');
    apbi.wdata <= (others => '0');
  end procedure configure_injector;


  procedure load_descriptors(
    signal   clk              : in  std_ulogic;
    constant descriptor_bank  : in  descriptor_bank;
    constant descriptor_addr  : in  std_logic_vector(31 downto 0);
    signal   bm_in            : in  bm_in_type;
    signal   bm_out           : out bm_out_type
  ) is 
    variable descriptor       :     descriptor_words;
    variable addr             :     std_logic_vector(31 downto 0);

  begin
    for j in descriptor_bank'range loop -- Loop for each descriptors
      
      descriptor := descriptor_bank(j);
      bm_out.rd_req_grant <= '1';
      bm_out.rd_done  <= '0';
      bm_out.rd_valid <= '0';
      wait until rising_edge(bm_in.rd_req);
      addr := add_vector(descriptor_addr, 20*j, bm_in.rd_addr'length);
      assert (bm_in.rd_addr = addr) report "The injector is fetching at the wrong address for descriptor to load!"
        & LF & "         It should be requesting the address 0x" & to_hstring(unsigned(addr))
        & ", but it is fetching at 0x" & to_hstring(unsigned(bm_in.rd_addr)) & " instead." severity failure;
      wait until falling_edge(bm_in.rd_req);
      bm_out.rd_req_grant <= '0';

      for i in descriptor'range loop -- Loop for each word of the descriptor

        bm_out.rd_valid <= '0';
        bm_out.rd_data <= descriptor(i) & X"00000000_00000000_00000000"; -- TODO: Adapt with dbits

        wait until rising_edge(clk);
        if (i = (descriptor'length - 1)) then bm_out.rd_done <= '1';
        else bm_out.rd_done <= '0';
        end if;
        bm_out.rd_valid <= '1';
        wait until rising_edge(clk);
      end loop;
      
      bm_out  <= DEF_INJ_BM;

    end loop;
  end procedure load_descriptors;


  procedure test_descriptor_batch(
    signal   clk        : in  std_ulogic;
    signal   bmin       : in  bm_in_type;
    signal   bmout      : out bm_out_type;
    constant descr_bnk  : in  descriptor_bank;
    constant MAX_BEAT   : in  integer;
    signal   irq        : in  std_logic
  ) is
    variable descr_wrd  :     descriptor_words; -- Descriptor being executed
    variable descr_num  :     integer := 0;     -- Var to select the descriptor being executed from batch
    variable tot_size   :     integer := 0;     -- Remaining bytes to read/write from total transfer
    variable beat_size  :     integer := 0;     -- Remaining bytes to read/write from beat transfer
    variable addr_off   :     integer := 0;     -- Address offset where to execute the transfer beat
    variable addr_act   :     std_logic_vector(31 downto 0); -- Source/destination address
    variable first_beat :     std_logic;        -- First beat indicator to manage multi-bursts (non-fixed address)

  begin

    bmout.rd_req_grant <= '1'; -- Allow to start first transaction not matter if is read or write
    bmout.wr_req_grant <= '1'; -- Allow to start first transaction not matter if is read or write

    -- Loop for every descriptor transaction
    for descr_num in descr_bnk'range loop
      descr_wrd := descr_bnk(descr_num);
      -- Loop descriptor transaction for number of repetitions
      for repet_count in 0 to to_integer(unsigned(descr_wrd(0)(12 downto 7))) loop
        addr_off := 0;
        first_beat := '1';

        -- Wait for transaction request
        wait until rising_edge(bmin.rd_req) or rising_edge(bmin.wr_req);
        --report "Descriptor number " & integer'image(descr_num) & " and repet_count = " & integer'image(repet_count); -- Debug line


        -- Read transaction
        if(bmin.rd_req = '1') then
          tot_size := to_integer(unsigned(descr_wrd(0)(31 downto 13)));

          -- Due to maximum size beats in bursts, big transactions must be sliced in MAX_SIZE_BEAT beats,
          while (tot_size > 0) loop -- or 4 byte beats if is a fixed address transaction.

            -- Each beat requires a handshake request process. The first one is done on the main loop.
            if(first_beat = '0') then wait until rising_edge(bmin.rd_req); end if;

            -- Compute address offset if the transfer is not address fixed
            if(descr_wrd(0)(5) = '0') then addr_off := to_integer(unsigned(descr_wrd(0)(31 downto 13))) - tot_size; end if;

            -- Size management
            if(descr_wrd(0)(5) = '0') then        -- In case of read without fixed address, the transfer is
              if(tot_size > MAX_BEAT) then        -- executed in burst mode, which has a size maximum of
                beat_size := MAX_BEAT;            -- MAX_BEAT bytes.
                tot_size  := tot_size - MAX_BEAT;
              else 
                beat_size := tot_size;            -- Or total size if the remaining transaction is of lower
                tot_size  := 0;                   -- size than the stablished MAX_BEAT size.
              end if;
            else                                  -- However, if the read transfer is address fixed, the
              beat_size := 4;                     -- transfer is executed in beats of 4 bytes, or dbits.
              tot_size  := tot_size - 4;
            end if;

            -- Check if injector is reading on the correct address
            addr_act := add_vector(descr_wrd(3), addr_off, addr_act'length);
            assert (bmin.rd_addr = addr_act) report  "Wrong address fetched for read transaction!" & LF & "Expected 0x"
                                      & to_hstring(unsigned(addr_act))     & " address, but injector fetched at 0x" 
                                      & to_hstring(unsigned(bmin.rd_addr)) & "." & LF & "This has happened at descriptor " 
                                      & integer'image(descr_num) & " with a repetition count of " & integer'image(repet_count) 
                                      & "." severity failure;
                                      
            -- Start reading beat
            wait until falling_edge(bmin.rd_req); -- Waiting for request deassertion allows to manage beats
            bmout.rd_req_grant <= '0';
            
            while (beat_size > 4) loop -- This loop may be unnecessary for the injector, but simulates SELENE platform behaviour
              beat_size := beat_size - 4;
              wait until rising_edge(clk);  -- May change to add random wait
              bmout.rd_valid <= '1';        -- Make sure only for a single clock cycle is asserted by beat
              wait until rising_edge(clk);
              bmout.rd_valid <= '0';
            end loop;

            -- Finishing reading a beat
            wait until rising_edge(clk);
            bmout.rd_valid <= '1';
            bmout.rd_done <= '1';
            wait until rising_edge(clk);
            bmout.rd_valid <= '0';
            bmout.rd_done <= '0';            
            bmout.rd_req_grant <= '1'; -- Prepare for next reading beat
            first_beat := '0';

          end loop; -- BURST loop
        end if;


        -- Write transaction
        if(bmin.wr_req = '1') then
          tot_size := to_integer(unsigned(descr_wrd(0)(31 downto 13)));

          -- Due to maximum size beats in bursts, big transactions must be sliced in MAX_SIZE_BEAT beats,
          while (tot_size > 0) loop -- or 4 byte beats if is a fixed address transaction.

            -- Each beat requires a handshake request process. The first one is done on the main loop.
            if(first_beat = '0') then wait until rising_edge(bmin.wr_req) ; end if;

            -- Compute address offset if the transfer is not address fixed
            if(descr_wrd(0)(6) = '0') then addr_off := to_integer(unsigned(descr_wrd(0)(31 downto 13))) - tot_size; end if;

            -- Size management
            if(descr_wrd(0)(6) = '0') then        -- In case of not fixed write address, the transfer is
              if(tot_size > MAX_BEAT) then        -- executed in burst mode, which has a size maximum of
                beat_size := MAX_BEAT;            -- MAX_BEAT bytes.
                tot_size  := tot_size - MAX_BEAT;
              else 
                beat_size := tot_size;            -- Or total size if the remaining transaction is of lower
                tot_size  := 0;                   -- size than the stablished MAX_BEAT size.
              end if;
            else                                  -- However, if the write transfer is address fixed, the
              beat_size := 4;                     -- transfer is executed in beats of 4 bytes, or dbits.
              tot_size  := tot_size - 4;
            end if;
            
            -- Check if injector is writing on the correct address
            addr_act := add_vector(descr_wrd(2), addr_off, addr_act'length);
            assert (bmin.wr_addr = addr_act) report  "Wrong address fetched for write transaction!" & LF & "Expected 0x"
                                      & to_hstring(unsigned(addr_act))     & " address, but injector fetched at 0x" 
                                      & to_hstring(unsigned(bmin.wr_addr)) & "." & LF & "This has happened at descriptor " 
                                      & integer'image(descr_num) & " with a repetition count of " & integer'image(repet_count) 
                                      & "." severity failure;
            
            -- Start writing beat
            wait until falling_edge(bmin.wr_req); -- Waiting for request deassertion allows to manage beats
            bmout.wr_req_grant <= '0';

            while (beat_size > 4) loop 
              beat_size := beat_size - 4;
              bmout.wr_full <= '0';
              wait for T;       -- May change to random wait
              bmout.wr_full <= '1';
              wait for T;
            end loop;
            
            -- Finishing writing a beat
            wait until rising_edge(clk);
            bmout.wr_done <= '1';
            wait until rising_edge(clk);
            bmout.wr_done <= '0';
            bmout.wr_req_grant <= '1'; -- Prepare for next writing beat
            first_beat := '0';

          end loop; -- BURST loop

        end if;
      end loop; -- Loop for repetitions

      -- Check if the injector reports descriptor completion
      wait until rising_edge(clk); wait until rising_edge(clk);
      assert (irq = '1') report "The testbench has finished descriptor " & integer'image(descr_num) & " but the injector has not (apbo.irq=0)." severity failure;
        
    end loop; -- Loop for descriptors

    -- Set BM bus to default state
    bmout <= DEF_INJ_BM;

  end procedure test_descriptor_batch;


  -----------------------------------------------------------------------------
  -- Component instantiation
  -----------------------------------------------------------------------------
  


end package body tb_injector_pkg;




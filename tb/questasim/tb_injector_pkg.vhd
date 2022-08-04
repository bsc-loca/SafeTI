-----------------------------------------------------------------------------   
-- Entity:        tb_injector_pkg
-- File:          tb_injector_pkg.vhd
-- Author:        Francis Fuentes
-- Description:   Package for injector testbenches.
-- Compatibility: This library uses VHDL2008 calls to offer better debugging support.
--                However, it can be compiled with older versions by comenting VHDL2008
--                and uncommenting !VHDL2008 lines.
------------------------------------------------------------------------------ 
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library safety;
use safety.injector_pkg.all;

-----------------------------------------------------------------------------
-- Library package for testbench top level entities. This file should include those
-- functions, types, procedures, constants and other objects that are commonly used
-- at the different injector's testbenches.
-----------------------------------------------------------------------------

package tb_injector_pkg is

  -----------------------------------------------------------------------------
  -- Constant declaration
  -----------------------------------------------------------------------------

  constant T : time := 5 ns; -- Clock cycle period

  -- APB input stimulus to start injector
  constant DEF_INJ_APB : apb_slave_in := (
    sel     => '0',
    en      => '0',
    addr    => (others => '0'),
    wr_en   => '0',
    wdata   => (others => '0'),
    irq     => '0'
  );

  -- BM output (injector input) default state
  constant DEF_INJ_BM : ib_miso := (
    rd_data       => (others => '0'),
    rd_req_grant  => '0',
    rd_valid      => '0',
    rd_done       => '0',
    rd_err        => '0',
    wr_req_grant  => '0',
    wr_full       => '1',
    wr_done       => '0',
    wr_err        => '0'
  );

  -----------------------------------------------------------------------------
  -- Records and types
  -----------------------------------------------------------------------------

  -- Since we are working with many descriptors, it's better to use both types
  type descriptor_words is array (0 to 1) of std_logic_vector(31 downto 0); -- 2 word per descriptor
  type descriptor_bank_tb  is array (natural range <>) of descriptor_words;    -- X number of descriptors
  type addr_bank        is array (natural range <>) of std_logic_vector(31 downto 0); -- X number of addresses

  -----------------------------------------------------------------------------
  -- Function/procedure declaration
  -----------------------------------------------------------------------------

  -- Function used to generate a descriptor. They're created with the enable and interrupt flags asserted.
  function descriptor_rd_wr(size      : integer range 1 to 524288;      -- Total size of a transfer
                            count     : integer range 0 to 63;          -- Number of repeats of the transfer
                            action    : std_logic_vector(4 downto 0);   -- Transaction type (read, write, delay)
                            addr      : std_logic_vector(31 downto 0);  -- Initial address to apply transaction
                            irq_compl : std_ulogic;                     -- Send APB interruption at descriptor completion
                            last      : std_ulogic                      -- Last descriptor flag
  ) return descriptor_words;

  -- IF function for when VHDL can not use if (like at constants).
  function sel              (A, B : integer; sel : boolean) return integer;

  -- Procedure used to configure and start injector through the APB bus (injector is APB slave, which requires the psel to be set)
  procedure configure_injector(
    signal   clk              : in  std_ulogic;
    constant apb_inj_addr     : in  std_logic_vector(31 downto 0);
    constant inj_config       : in  std_logic_vector(31 downto 0);
    signal   apbo             : in  apb_slave_out;
    signal   apbi             : out apb_slave_in
  );

  -- Procedure used to simulate a memory fetch to load descriptors for a test.
  procedure load_descriptors(
    signal   clk                : in  std_ulogic;
    constant apb_inj_addr       : in  std_logic_vector(31 downto 0); -- Injector's address location on APB memory
    constant descriptor_bank_tb : in  descriptor_bank_tb; -- Descriptor batch to offer at rdata
    signal   apbi               : out apb_slave_in        -- APB bus to write descriptors
  );

  -- Procedure used to execute read/write transactions.
  procedure test_descriptor_batch(
    signal   clk              : in  std_ulogic;
    signal   bmin             : in  ib_mosi;            -- BM bus set by injector
    signal   bmout            : out ib_miso;            -- BM bus set by testbench
    constant descr_bnk        : in  descriptor_bank_tb; -- Descriptor batch to be executed
    constant MAX_BURST        : in  integer;            -- Maximum length in bytes for a beat in burst
    constant dbits            : in  integer;            -- BM data bus width
    signal   irq              : in  std_logic;          -- Descriptor completion flag
    signal   wait_descr_compl : out std_logic           -- Testbench is waiting for descriptor completion flag signal
  );

end package tb_injector_pkg;



package body tb_injector_pkg is

  -----------------------------------------------------------------------------
  -- Sequential process
  -----------------------------------------------------------------------------

  function descriptor_rd_wr(size      : integer range 1 to 524288;      -- Total size of a transfer -1
                            count     : integer range 0 to 63;          -- Number of repeats of the transfer
                            action    : std_logic_vector(4 downto 0);   -- Transaction type (read, write, delay)
                            addr      : std_logic_vector(31 downto 0);  -- Initial address to apply transaction
                            irq_compl : std_ulogic;                     -- Send APB interruption at descriptor completion
                            last      : std_ulogic                      -- Last descriptor flag
  ) return descriptor_words is 
    variable descr_words  : descriptor_words  := (others => (others => '0'));
    variable size_std     : std_logic_vector(18 downto 0) := (others => '0');
    variable count_std    : std_logic_vector(5 downto 0)  := (others => '0');

  begin
    count_std := std_logic_vector( to_unsigned(count, count_std'length) );
    size_std  := std_logic_vector( to_unsigned(size - 1,  size_std'length) );

    descr_words := (
      size_std              & --0x00 Injector control word
      count_std             &
      irq_compl             & -- enable interrupt on descriptor completion
      action                &
      last                  , -- last descriptor flag
      addr                    --0x04 Action (read/write) address
      );

    return descr_words;
  end function descriptor_rd_wr;

  -- IF function that outputs the first input if the boolean is true, the second if false.
  function sel(A, B : integer; sel : boolean) return integer is
  begin
    if sel then return A;
    else return B;
    end if;
  end sel;

  procedure configure_injector(
    signal   clk              : in  std_ulogic;
    constant apb_inj_addr     : in  std_logic_vector(31 downto 0);
    constant inj_config       : in  std_logic_vector(31 downto 0);
    signal   apbo             : in  apb_slave_out;
    signal   apbi             : out apb_slave_in
  ) is
  begin
    apbi.en    <= '0';
    apbi.addr  <= apb_inj_addr; -- Configure and start injector
    apbi.wdata <= inj_config;
    wait until rising_edge(clk);
    report "Starting the injector!";
    apbi.en    <= '1';
    apbi.wr_en <= '1';
    wait until rising_edge(clk);
    apbi.en    <= '0';
    apbi.wr_en <= '0';
    apbi.addr  <= (others => '0');
    apbi.wdata <= (others => '0');
  end procedure configure_injector;


  procedure load_descriptors(
    signal   clk                : in  std_ulogic;
    constant apb_inj_addr       : in  std_logic_vector(31 downto 0);
    constant descriptor_bank_tb : in  descriptor_bank_tb;
    signal   apbi               : out apb_slave_in
  ) is 
    variable descriptor         :     descriptor_words;

  begin
    -- Set the signals to write descriptors through the APB serial register
    apbi.addr   <= std_logic_vector(unsigned(apb_inj_addr) + X"FC");
    apbi.en     <= '1';
    apbi.wr_en  <= '1';

    for j in descriptor_bank_tb'range loop -- Loop for each descriptors
      descriptor  := descriptor_bank_tb(j);

      for i in descriptor'range loop -- Loop for each word of the descriptor
        apbi.wdata <= descriptor(i); -- APB data bus is 32-bit

        wait until rising_edge(clk);
      end loop;
    end loop;

    apbi.addr   <= (others => '0');
    apbi.en     <= '0';
    apbi.wr_en  <= '0';

  end procedure load_descriptors;


  -- Since this procedure manages both writes and reads in a similar way, it could be reduced to a half in length by 
  -- nesting another procedure, which would contain "if" statements to differentiate between read and write 
  -- transactions on the bits they differ. However, having them sepparated may help to understand the procedure and 
  -- facilitate any modification if future injector versions implement more differences between transfer modes.
  procedure test_descriptor_batch(
    signal   clk              : in  std_ulogic;
    signal   bmin             : in  ib_mosi;
    signal   bmout            : out ib_miso;
    constant descr_bnk        : in  descriptor_bank_tb;
    constant MAX_BURST        : in  integer;
    constant dbits            : in  integer;
    signal   irq              : in  std_logic;
    signal   wait_descr_compl : out std_logic
  ) is
    variable descr_wrd    :     descriptor_words; -- Descriptor being executed
    variable descr_num    :     integer := 0;     -- Var to select the descriptor being executed from batch
    variable tot_size     :     integer := 0;     -- Remaining bytes to read/write from total transfer
    variable burst_size   :     integer := 0;     -- Size in bytes read/writen on each burst
    variable addr_act     :     std_logic_vector(31 downto 0); -- Source/destination address
    variable first_beat   :     std_logic;        -- First beat flag to manage multi-bursts (non-fixed address) transactions
  begin

    -- LOOP for every descriptor transaction
    for descr_num in descr_bnk'range loop
      descr_wrd := descr_bnk(descr_num);

      -- LOOP descriptor transaction for number of repetitions
      for repet_count in 0 to to_integer(unsigned(descr_wrd(0)(12 downto 7))) loop
        first_beat    := '1';
        tot_size      := to_integer(unsigned(descr_wrd(0)(31 downto 13))) + 1;
        addr_act      := descr_wrd(1)(31 downto 0);

        -- Wait for transaction request if is not asserted already
        if((bmin.rd_req or bmin.wr_req) = '0') then wait until rising_edge(bmin.rd_req) or rising_edge(bmin.wr_req); end if;
        --report "Descriptor number " & integer'image(descr_num) & " and repet_count = " & integer'image(repet_count); -- Debug line

        -- First request transaction grant for each descriptor repetition
        --wait until rising_edge(clk); -- Which is asserted one clock cycle after the injector request, making it to wait
        case descr_wrd(0)(5 downto 1) is
          when OP_READ  | OP_READ_FIX  => bmout.rd_req_grant <= '1';
          when OP_WRITE | OP_WRITE_FIX => bmout.wr_req_grant <= '1';
          when others => bmout              <= DEF_INJ_BM;
        end case;

        -- Read transaction routine
        if(bmin.rd_req = '1') then

          -- Due to maximum size beats in bursts, big transactions must be sliced in MAX_SIZE_BEAT beats,
          while (tot_size > 0) loop -- or dbits/8 byte beats if it's a fixed address transaction.

            -- Each burst requires a handshake request process. The first one is done on the main loop to allow select transaction mode routine.
            if(bmin.rd_req = '0' and first_beat = '0') then wait until rising_edge(bmin.rd_req); end if;

            -- Size management
            if(descr_wrd(0)(3) = '0') then        -- In case of read without fixed address, the transfer is
              if(tot_size > MAX_BURST) then       -- executed in burst mode, which has a size maximum of
                burst_size:= MAX_BURST;           -- MAX_BURST bytes.
                tot_size  := tot_size - MAX_BURST;
              else 
                burst_size:= tot_size;            -- Or total size if the remaining transaction is of lower
                tot_size  := 0;                   -- size than the stablished MAX_BURST size.
              end if;
            else                                  -- However, if the read transfer is address fixed, the
              burst_size  := dbits/8;             -- transfer is executed in beats of dbits/8.
              tot_size    := tot_size - dbits/8;
            end if;

            -- Check if injector is reading on the correct address
            assert (bmin.rd_addr = addr_act) report  "Wrong address fetched for read transaction!" & LF & "Expected 0x"         -- VHDL2008
                                      & to_hstring(unsigned(addr_act))     & " address, but injector fetched at 0x"             -- VHDL2008
                                      & to_hstring(unsigned(bmin.rd_addr)) & "." & LF & "This has happened at descriptor "      -- VHDL2008
                                      & integer'image(descr_num) & " with a repetition count of " & integer'image(repet_count)  -- VHDL2008
                                      & "." severity failure;                                                                   -- VHDL2008
            --assert (bmin.rd_addr = addr_act) report  "Wrong address fetched for write transaction!" severity failure; -- !VHDL2008

            -- Update action address for next beat
            addr_act := std_logic_vector(unsigned(addr_act) + burst_size);
            
            -- Start reading beat
            wait until falling_edge(bmin.rd_req); -- Waiting for request deassertion allows to manage beats
            bmout.rd_req_grant <= '0';
            
            while (burst_size> dbits/8) loop
              burst_size:= burst_size- dbits/8;
              bmout.rd_valid <= '1';
              wait until rising_edge(clk);
              bmout.rd_valid <= '0';
              wait until rising_edge(clk); -- This wait can be multiple
            end loop;

            -- Finishing reading a beat
            bmout.rd_valid <= '1';
            bmout.rd_done <= '1';
            wait until rising_edge(clk);
            bmout.rd_valid <= '0';
            bmout.rd_done <= '0';
            wait until rising_edge(clk);
            bmout.rd_req_grant <= '1'; -- Prepare for next reading beat
            first_beat := '0';

          end loop; -- BURST loop
        end if;


        -- Write transaction routine
        if(bmin.wr_req = '1') then

          -- Due to maximum size beats in bursts, big transactions must be sliced in MAX_SIZE_BEAT beats,
          while (tot_size > 0) loop -- or 4 byte beats if it's a fixed address transaction.
            
            -- Each burst requires a handshake request process. The first one is done on the main loop to allow select transaction mode routine.
            if(bmin.wr_req = '0' and first_beat = '0') then wait until rising_edge(bmin.wr_req) ; end if;
            
            -- Size management
            if(descr_wrd(0)(3) = '0') then        -- In case of not fixed write address, the transfer is
              if(tot_size > MAX_BURST) then       -- executed in burst mode, which has a size maximum of
                burst_size:= MAX_BURST;           -- MAX_BURST bytes.
                tot_size  := tot_size - MAX_BURST;
              else 
                burst_size:= tot_size;            -- Or total size if the remaining transaction is of lower
                tot_size  := 0;                   -- size than the stablished MAX_BURST size.
              end if;
            else                                  -- However, if the write transfer is address fixed, the
              burst_size:= dbits/8;               -- transfer is executed in beats of dbits/8.
              tot_size  := tot_size - dbits/8;
            end if;
            
            -- Check if injector is writing on the correct address
            assert (bmin.wr_addr = addr_act) report  "Wrong address fetched for write transaction!" & LF & "Expected 0x"       -- VHDL2008
                                      & to_hstring(unsigned(addr_act))     & " address, but injector fetched at 0x"            -- VHDL2008
                                      & to_hstring(unsigned(bmin.wr_addr)) & "." & LF & "This has happened at descriptor "     -- VHDL2008
                                      & integer'image(descr_num) & " with a repetition count of " & integer'image(repet_count) -- VHDL2008
                                      & "." severity failure;                                                                  -- VHDL2008
            --assert (bmin.wr_addr = addr_act) report  "Wrong address fetched for write transaction!" severity failure; -- !VHDL2008
            
            -- Update action address for next beat
            addr_act := std_logic_vector(unsigned(addr_act) + burst_size);

            -- Start writing beat
            wait until falling_edge(bmin.wr_req); -- Waiting for request deassertion allows to manage beats
            bmout.wr_req_grant <= '0';

            while (burst_size> dbits/8) loop -- This loop may be unnecessary for the injector, but simulates writing interface behaviour
              burst_size:= burst_size- dbits/8;
              bmout.wr_full <= '0';
              wait until rising_edge(clk);
              bmout.wr_full <= '1';
              wait until rising_edge(clk); -- This wait can be multiple
            end loop;
            
            -- Finishing writing a beat
            bmout.wr_full <= '0';
            wait until rising_edge(clk);
            bmout.wr_full <= '1';
            bmout.wr_done <= '1';
            wait until rising_edge(clk);
            bmout.wr_done <= '0';
            wait until rising_edge(clk); -- May change to add random wait, only for those with tot_size > 0 so as to not intefere with descr_compl check
            bmout.wr_req_grant <= '1'; -- Prepare for next writing beat
            first_beat := '0';
            
          end loop; -- BURST loop
        end if;
        -- Set BM bus to default state
        bmout <= DEF_INJ_BM;
      end loop; -- LOOP for descriptor repetitions

      -- Wait until injector reports descriptor completion
      if(irq /= '1') then 
        wait_descr_compl <= '1';
        wait until rising_edge(irq);
      end if;
      wait_descr_compl <= '0';
        
    end loop; -- LOOP for descriptors

  end procedure test_descriptor_batch;


  -----------------------------------------------------------------------------
  -- Component instantiation
  -----------------------------------------------------------------------------

end package body tb_injector_pkg;

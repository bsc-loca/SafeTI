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

  -- IB output (injector input) default state
  constant DEF_INJ_IB : ib_miso := (
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

  -- Procedure used to read from the APB bus, in which the injector is a slave.
  procedure read_address(
    signal   clk              : in  std_ulogic;
    signal   apbo             : in  apb_slave_out;
    signal   apbi             : out apb_slave_in;
    constant address          : in  std_logic_vector(31 downto 0);
    variable rd_data          : out std_logic_vector(31 downto 0)
  );

  -- Procedure used to write on the APB bus, in which the injector is a slave.
  procedure write_address(
    signal   clk                : in  std_ulogic;
    signal   apbo               : in  apb_slave_out;
    signal   apbi               : out apb_slave_in;
    constant address            : in  std_logic_vector(31 downto 0);
    constant wr_data            : in  std_logic_vector(31 downto 0)
  );

  -- Procedure used to simulate a memory fetch to load descriptors for a test.
  procedure load_descriptors(
    signal   clk                : in  std_ulogic;
    constant inj_base_addr      : in  std_logic_vector(31 downto 0); -- Injector's address location on APB memory
    constant descriptor_bank_tb : in  descriptor_bank_tb; -- Descriptor batch to offer at rdata
    signal   apbi               : out apb_slave_in        -- APB bus to write descriptors
  );

  -- Procedure used to execute read/write transactions.
  -- The validation is done by looking into the descriptor data and simulating what it should be happening.
  -- The "wait_descr_compl" flag allows to the main testbench to halt if the procedure or injector takes too 
  -- many clock cycles to be answered.
  procedure test_descriptor_batch(
    signal   clk                : in  std_ulogic;
    signal   rstn               : out std_ulogic;
    signal   ibin               : in  ib_mosi;            -- IB bus set by injector
    signal   ibout              : out ib_miso;            -- IB bus set by testbench
    signal   apbo               : in  apb_slave_out;      -- APB bus
    signal   apbi               : out apb_slave_in;       -- APB bus
    constant descr_bnk          : in  descriptor_bank_tb; -- Descriptor batch to be executed
    constant MAX_BURST          : in  integer;            -- Maximum length in bytes for a beat in burst
    constant dbits              : in  integer;            -- IB data bus width
    constant inj_base_addr      : in  std_logic_vector(31 downto 0);  -- Injector APB base address
    constant inj_conf           : in  std_logic_vector(31 downto 0);  -- Injector configuration for the test
    signal   irq                : in  std_logic;          -- Descriptor completion flag
    signal   wait_descr_compl   : out std_logic           -- Testbench is waiting for descriptor completion flag signal
  );

end package tb_injector_pkg;



package body tb_injector_pkg is

  -----------------------------------------------------------------------------
  -- Function and types
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


  -----------------------------------------------------------------------------
  -- Sequential processes
  -----------------------------------------------------------------------------

  procedure read_address(
    signal   clk              : in  std_ulogic;
    signal   apbo             : in  apb_slave_out;
    signal   apbi             : out apb_slave_in;
    constant address          : in  std_logic_vector(31 downto 0);
    variable rd_data          : out std_logic_vector(31 downto 0)
  ) is
  begin
    apbi.en     <= '0';
    apbi.addr   <= address;
    wait until rising_edge(clk);
    apbi.en     <= '1';
    apbi.wr_en  <= '0';
    wait until rising_edge(clk);
    rd_data     := apbo.rdata;
    apbi.en     <= '0';
    apbi.wr_en  <= '0';
    apbi.addr   <= (others => '0');
    apbi.wdata  <= (others => '0');
  end procedure read_address;

  procedure write_address(
    signal   clk              : in  std_ulogic;
    signal   apbo             : in  apb_slave_out;
    signal   apbi             : out apb_slave_in;
    constant address          : in  std_logic_vector(31 downto 0);
    constant wr_data          : in  std_logic_vector(31 downto 0)
  ) is
  begin
    apbi.en     <= '0';
    apbi.addr   <= address;
    apbi.wdata  <= wr_data;
    wait until rising_edge(clk);
    apbi.en     <= '1';
    apbi.wr_en  <= '1';
    wait until rising_edge(clk);
    apbi.en     <= '0';
    apbi.wr_en  <= '0';
    apbi.addr   <= (others => '0');
    apbi.wdata  <= (others => '0');
  end procedure write_address;


  procedure load_descriptors(
    signal   clk                : in  std_ulogic;
    constant inj_base_addr      : in  std_logic_vector(31 downto 0);
    constant descriptor_bank_tb : in  descriptor_bank_tb;
    signal   apbi               : out apb_slave_in
  ) is 
    variable descriptor         :     descriptor_words;
  begin
    -- Set the signals to write descriptors through the APB serial register
    apbi.addr   <= std_logic_vector(unsigned(inj_base_addr) + X"FC");
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
    signal   clk                : in  std_ulogic;
    signal   rstn               : out std_ulogic;
    signal   ibin               : in  ib_mosi;
    signal   ibout              : out ib_miso;
    signal   apbo               : in  apb_slave_out;
    signal   apbi               : out apb_slave_in;
    constant descr_bnk          : in  descriptor_bank_tb;
    constant MAX_BURST          : in  integer;
    constant dbits              : in  integer;
    constant inj_base_addr      : in  std_logic_vector(31 downto 0);
    constant inj_conf           : in  std_logic_vector(31 downto 0);
    signal   irq                : in  std_logic;
    signal   wait_descr_compl   : out std_logic
  ) is
    variable descr_wrd          :     descriptor_words; -- Descriptor being executed
    variable descr_num          :     integer := 0;     -- Var to select the descriptor being executed from batch
    variable tot_size           :     integer := 0;     -- Remaining bytes to read/write from total transfer
    variable burst_size         :     integer := 0;     -- Size in bytes read/writen on each burst
    variable addr_act           :     std_logic_vector(31 downto 0); -- Source/destination address
  begin

    -- Configure and start injector
    write_address(clk, apbo, apbi, inj_base_addr, inj_conf);

    -- LOOP for every programmed descriptor at the injector program
    for descr_num in descr_bnk'range loop
      descr_wrd := descr_bnk(descr_num);

      -- LOOP for number of repetitions programmed on the descriptor
      for repet_count in 0 to to_integer(unsigned(descr_wrd(0)(12 downto 7))) loop
        tot_size      := to_integer(unsigned(descr_wrd(0)(31 downto 13))) + 1;
        addr_act      := descr_wrd(1)(31 downto 0);

        case descr_wrd(0)(5 downto 1) is
          when OP_READ  | OP_READ_FIX  => -- Read transaction routine

            -- Due to maximum size beats in bursts, big transactions must be sliced in MAX_SIZE_BEAT beats,
            while (tot_size > 0) loop -- or dbits/8 byte beats if it's a fixed address transaction.

              -- Each burst requires a handshake request process. The first one is done on the main loop to allow select transaction mode routine.
              if(ibin.rd_req = '0') then wait until rising_edge(ibin.rd_req); end if;

              -- Grant the burst request.
              ibout.rd_req_grant <= '1';

              -- Size management
              if(tot_size > MAX_BURST) then
                burst_size  := MAX_BURST;
                tot_size    := tot_size - MAX_BURST;
              else
                burst_size  := tot_size;
                tot_size    := 0;
              end if;

              -- Check if injector is reading on the correct address
              assert (ibin.rd_addr = addr_act) report  "Wrong address fetched for read transaction!" & LF & "Expected 0x"         -- VHDL2008
                                        & to_hstring(unsigned(addr_act))     & " address, but injector fetched at 0x"             -- VHDL2008
                                        & to_hstring(unsigned(ibin.rd_addr)) & "." & LF & "This has happened at descriptor "      -- VHDL2008
                                        & integer'image(descr_num) & " with a repetition count of " & integer'image(repet_count)  -- VHDL2008
                                        & "." severity failure;                                                                   -- VHDL2008
              --assert (ibin.rd_addr = addr_act) report  "Wrong address fetched for write transaction!" severity failure;         -- !VHDL2008

              -- Check if injector is requesting correct transfer size
              assert (ibin.rd_size = std_logic_vector(to_unsigned(burst_size - 1, ibin.rd_size'length)))                         -- VHDL2008
                                        report "Wrong size request for read transaction!" & LF & "Expected 0x"                   -- VHDL2008
                                        & to_hstring(to_unsigned(burst_size - 1, 32)) & " size, but injector requested 0x"       -- VHDL2008
                                        & to_hstring(unsigned(ibin.rd_size))      & "." & LF & "This has happened at descriptor "-- VHDL2008
                                        & integer'image(descr_num) & " with a repetition count of " & integer'image(repet_count) -- VHDL2008
                                        & "." severity failure;                                                                  -- VHDL2008
              --assert (ibin.wr_addr = addr_act) report  "Wrong address fetched for write transaction!" severity failure;        -- !VHDL2008

              -- Update action address for next beat
              if(descr_wrd(0)(5 downto 1) = OP_READ) then
                addr_act := std_logic_vector(unsigned(addr_act) + burst_size);
              end if;

              -- Start reading beat
              wait until falling_edge(ibin.rd_req); -- Waiting for request deassertion allows to manage beats
              ibout.rd_req_grant <= '0';

              -- Data transfer loop
              while (burst_size > dbits/8) loop
                burst_size      := burst_size - dbits/8;
                ibout.rd_valid  <= '1';
                wait until rising_edge(clk);
                ibout.rd_valid  <= '0';
                wait until rising_edge(clk); -- This wait can be multiple
              end loop;

              -- Finishing reading a beat
              ibout.rd_valid  <= '1';
              ibout.rd_done   <= '1';
              wait until rising_edge(clk);
              ibout.rd_valid  <= '0';
              ibout.rd_done   <= '0';
              --wait until rising_edge(clk); -- May change to add random wait

            end loop; -- BURST loop
        

          when OP_WRITE | OP_WRITE_FIX => -- Write transaction routine

            -- Due to maximum size beats in bursts, big transactions must be sliced in MAX_SIZE_BEAT beats,
            while (tot_size > 0) loop -- or 4 byte beats if it's a fixed address transaction.

              -- Each burst requires a handshake request process. The first one is done on the main loop to allow select transaction mode routine.
              if(ibin.wr_req = '0') then wait until rising_edge(ibin.wr_req) ; end if;
                
              -- Grant the burst request.
              ibout.wr_req_grant <= '1';

              -- Size management
              if(tot_size > MAX_BURST) then
                burst_size  := MAX_BURST;
                tot_size    := tot_size - MAX_BURST;
              else
                burst_size  := tot_size;
                tot_size    := 0;
              end if;

              -- Check if injector is writing on the correct address
              assert (ibin.wr_addr = addr_act) report  "Wrong address fetched for write transaction!" & LF & "Expected 0x"       -- VHDL2008
                                        & to_hstring(unsigned(addr_act))     & " address, but injector fetched at 0x"            -- VHDL2008
                                        & to_hstring(unsigned(ibin.wr_addr)) & "." & LF & "This has happened at descriptor "     -- VHDL2008
                                        & integer'image(descr_num) & " with a repetition count of " & integer'image(repet_count) -- VHDL2008
                                        & "." severity failure;                                                                  -- VHDL2008
              --assert (ibin.wr_addr = addr_act) report  "Wrong address fetched for write transaction!" severity failure;        -- !VHDL2008

              -- Check if injector is requesting correct transfer size
              assert (ibin.wr_size = std_logic_vector(to_unsigned(burst_size - 1, ibin.wr_size'length)))                         -- VHDL2008
                                        report "Wrong size request for write transaction!" & LF & "Expected 0x"                  -- VHDL2008
                                        & to_hstring(to_unsigned(burst_size - 1, 32)) & " size, but injector requested 0x"       -- VHDL2008
                                        & to_hstring(unsigned(ibin.wr_size))      & "." & LF & "This has happened at descriptor "-- VHDL2008
                                        & integer'image(descr_num) & " with a repetition count of " & integer'image(repet_count) -- VHDL2008
                                        & "." severity failure;                                                                  -- VHDL2008
              --assert (ibin.wr_addr = addr_act) report  "Wrong address fetched for write transaction!" severity failure;        -- !VHDL2008

              -- Update action address for next beat
              if(descr_wrd(0)(5 downto 1) = OP_WRITE) then
                addr_act := std_logic_vector(unsigned(addr_act) + burst_size);
              end if;

              -- Start writing beat
              wait until falling_edge(ibin.wr_req); -- Waiting for request deassertion allows to manage beats
              ibout.wr_req_grant <= '0';

              -- Data transfer loop simulation
              while (burst_size > dbits/8) loop
                burst_size    := burst_size - dbits/8;
                ibout.wr_full <= '0';
                wait until rising_edge(clk);
                ibout.wr_full <= '1';
                wait until rising_edge(clk); -- This wait can be multiple
              end loop;
                
              -- Finishing writing a beat
              ibout.wr_full   <= '0';
              wait until rising_edge(clk);
              ibout.wr_full   <= '1';
              ibout.wr_done   <= '1';
              wait until rising_edge(clk);
              ibout.wr_done   <= '0';
              --wait until rising_edge(clk); -- May change to add random wait
                
            end loop; -- BURST loop


          when others => 
            ibout             <= DEF_INJ_IB;

        end case; -- DESCRIPTOR TYPE STIMULUS SELECTOR

        -- Set IB bus to default state
        ibout <= DEF_INJ_IB;
      end loop; -- LOOP for descriptor's repetitions

      -- Wait until injector reports descriptor completion if programmed
      if( descr_wrd(0)(6) = '1' and irq /= '1' ) then
        wait_descr_compl <= '1';
        wait until rising_edge(irq);
      end if;
      wait_descr_compl <= '0';
        
    end loop; -- LOOP for descriptors of injector program


    -- Check in the injector configuration if the Queue mode is enabled.
    if(inj_conf(2) = '1') then
      -- On queue mode enabled, check if it is requesting transaction for 
      -- the first descriptor in the program, while also reseting the injector.
      case descr_bnk(0)(0)(5 downto 1) is
        when OP_READ  | OP_READ_FIX  =>
          if(ibin.rd_req = '0') then
            for i in 0 to 5 loop
              wait until rising_edge(ibin.rd_req);
              assert i = 5 report "Queue mode was enabled, injector is not requesting READ" severity failure;
            end loop;
          end if;
          if(ibin.rd_addr /= descr_bnk(0)(1)) then
            assert FALSE report "Queue mode was enabled, injector is requesting READ" & 
              " (correctly), but not the correct address." severity failure;
          end if;

        when OP_WRITE | OP_WRITE_FIX =>
        if(ibin.wr_req = '0') then
          for i in 0 to 5 loop
            wait until rising_edge(ibin.wr_req);
            assert i = 5 report "Queue mode was enabled, injector is not requesting WRITE" severity failure;
          end loop;
        end if;
        if(ibin.wr_addr /= descr_bnk(0)(1)) then
          assert FALSE report "Queue mode was enabled, injector is requesting WRITE" & 
            " (correctly), but not the correct address." severity failure;
        end if;

        when others =>
        ibout             <= DEF_INJ_IB;

      end case;

    end if; -- Queue mode check

    -- Hard reset to skip having to manage the any transaction when queue mode is enabled.
    rstn <= '0';
    wait until rising_edge(clk);
    rstn <= '1';
    wait until rising_edge(clk);

  end procedure test_descriptor_batch;


  -----------------------------------------------------------------------------
  -- Component instantiation
  -----------------------------------------------------------------------------

end package body tb_injector_pkg;

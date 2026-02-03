-----------------------------------------------------------------------------
-- Entity:      injector_csr
-- File:        injector_csr.vhd
-- Author:      Francis Fuentes, Oriol Sala
-- Description: Control and Status Register interface for programing Injector.
------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library safety;
use safety.injector_pkg.all;


--------------------------------------------------------------
-- Entity to read and write CSR registers for Injector core --
--------------------------------------------------------------

entity injector_csr is
  generic (
    PC_LEN          : integer                         := 4;   -- Length of PC register
    DEFAULT_PROFILE : std_logic_vector(31 downto 0)   := (others => '0'); -- Default Network profile.
    CSR_READ_INST   : boolean                         := FALSE; -- Instantaneous CSR read
    ASYNC_RST       : boolean                         := TRUE -- Allow asynchronous reset flag
  );
  port (
  -- External I/O
    rstn            : in  std_ulogic;                         -- Reset
    clk             : in  std_ulogic;                         -- Clock
    csri            : in  csr_in;                             -- CSR Subordinate input
    csro            : out csr_out;                            -- CSR Subordinate output
    network_profile : out std_logic_vector(31 downto 0);      -- Network profile to apply during transaction requests
  -- Internal I/O
    -- Signals for CONTROL
    gen_config      : out injector_config;                    -- General injector configuration signals
    -- Signals for FETCH
    desc_word       : out std_logic_vector(31 downto 0);      -- Descriptor word input register from CSR
    desc_word_wen   : out std_logic;                          -- Write enable for descriptor word input
    -- Signals for EXE
    wr_data         : out std_logic_vector(31 downto 0);      -- EXE write descriptor data
    -- Signals from CONTROL
    disable         : in  std_logic;                          -- Turn off injector execution flag
    irq_flag        : in  std_logic;                          -- Interruption flag for CSR output
    -- Signals from EXE
    request_granted : in  std_logic;                          -- Request grant signal
    -- Signals for DEBUG
    debug_pc        : in  std_logic_vector(31 downto 0);      -- PC from each stage combined
    debug_state     : in  pipeline_state_array                -- State from each stage
  );
end entity injector_csr;

architecture rtl of injector_csr is

  -----------------------------------------------------------------------------
  -- Types and reset constants declaration
  -----------------------------------------------------------------------------

  -- Debug bus between stages and CSR interface
  type debug_stages is record
    fetch_pc        : std_logic_vector(PC_LEN - 1 downto 0);
    fetch_status    : std_logic_vector(MAX_STATUS_LEN - 1 downto 0);
    fetch_desc      : desc_words;
    decode_pc       : std_logic_vector(PC_LEN - 1 downto 0);
    decode_status   : std_logic_vector(MAX_STATUS_LEN - 1 downto 0);
    decode_desc     : desc_words;
    exe_pc          : std_logic_vector(PC_LEN - 1 downto 0);
    exe_status      : std_logic_vector(MAX_STATUS_LEN - 1 downto 0);
    exe_desc        : desc_words;
  end record debug_stages;

  type csr_reg is record
    gen_config      : injector_config;              -- Injector general configuration
    network_profile : std_logic_vector(31 downto 0);-- Generic network profile register
    irq_counter     : unsigned(31 downto 0);        -- Interruption counter
    request_counter : unsigned(31 downto 0);        -- Traffic internface request counter
    desc_word       : std_logic_vector(31 downto 0);-- Descriptor word CSR input register
    desc_wen        : std_logic;                    -- Write enable of descriptor word input
    irq             : std_logic;                    -- Interruption flag register
    wr_data         : std_logic_vector(31 downto 0);-- EXE write descriptor data
    debug           : debug_stages;                 -- Debug register bank
  end record csr_reg;

  -- Reset values for CSR registers.
  constant RESET_INJECTOR_CONFIG  : injector_config := (
    enable            => '0',
    hold              => '0',
    reset_sw          => '0',
    queue_mode_en     => '0',
    irq_prog_compl_en => '0',
    irq_err_core_en   => '0',
    irq_err_net_en    => '0',
    freeze_irq_en     => '0'
  );

  constant RESET_DESC_WORDS       : desc_words      := (
    others            => ( others => '0' )
  );

  constant RESET_CSR_DEBUG        : debug_stages    := (
    fetch_pc          => (others => '0'),
    fetch_status      => (others => '0'),
    fetch_desc        => RESET_DESC_WORDS,
    decode_pc         => (others => '0'),
    decode_status     => (others => '0'),
    decode_desc       => RESET_DESC_WORDS,
    exe_pc            => (others => '0'),
    exe_status        => (others => '0'),
    exe_desc          => RESET_DESC_WORDS
  );

  constant RESET_CSR_REGS         : csr_reg         := (
    gen_config        => RESET_INJECTOR_CONFIG,
    irq_counter       => (others => '0'),
    request_counter   => (others => '0'),
    network_profile   => DEFAULT_PROFILE,
    desc_word         => (others => '0'),
    desc_wen          => '0',
    irq               => '0',
    wr_data           => (others => '0'),
    debug             => RESET_CSR_DEBUG
  );


  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------

  -- Registers
  signal csr_regs   : csr_reg;
  signal csr_rd     : std_logic_vector(csro.rdata'range);


begin -- rtl

  -----------------------------------------------------------------------------
  -- Assignments
  -----------------------------------------------------------------------------

  -- I/O assignments
  csro.irq        <= csr_regs.irq;
  network_profile <= csr_regs.network_profile;

  -- Internal output signals from CSR interface module
  gen_config      <= csr_regs.gen_config;
  desc_word       <= csr_regs.desc_word;
  desc_word_wen   <= csr_regs.desc_wen;
  wr_data         <= csr_regs.wr_data;


  --------------------------------
  -- CSR READ CONCURRENT ACCESS --
  --------------------------------

  comb0 : if CSR_READ_INST = TRUE generate
    -- Implement instantaneous concurrent CSR read access
    csr_read : process(csri, csr_regs)
    begin
      -- Default signaling CSR output
      csro.rdata          <= (others => '0');

      if(csri.en = '1' and csri.wr_en = '0') then
        case csri.addr is
          when CSR_OFFSET_CONFIG =>               --0x00 Injector control register
            csro.rdata(0) <= csr_regs.gen_config.enable;
            csro.rdata(1) <= csr_regs.gen_config.hold;
            csro.rdata(2) <= csr_regs.gen_config.reset_sw;
            csro.rdata(3) <= csr_regs.gen_config.queue_mode_en;
            csro.rdata(4) <= csr_regs.gen_config.irq_prog_compl_en;
            csro.rdata(5) <= csr_regs.gen_config.irq_err_core_en;
            csro.rdata(6) <= csr_regs.gen_config.irq_err_net_en;
            csro.rdata(7) <= csr_regs.gen_config.freeze_irq_en;

          when CSR_OFFSET_DEBUG_PC =>             --0x60 Debug: PC from each stage
            csro.rdata    <= (31 downto 20+PC_LEN => '0') & csr_regs.debug.exe_pc    &
                             (19 downto 10+PC_LEN => '0') & csr_regs.debug.decode_pc &
                             ( 9 downto    PC_LEN => '0') & csr_regs.debug.fetch_pc;

          when CSR_OFFSET_CNT_INT =>              --0x80 Interruption counter
            csro.rdata    <= std_logic_vector(csr_regs.irq_counter);

          when CSR_OFFSET_CNT_ACCESS =>           --0x84 Request access counter
            csro.rdata    <= std_logic_vector(csr_regs.request_counter);

          when CSR_WRITE_DATA =>                  --0xF0 Interface write data
            csro.rdata    <= csr_regs.wr_data;

          when CSR_OFFSET_NET_PROFILE =>          --0xF8 Network interface configuration
            csro.rdata    <= csr_regs.network_profile;

          -- CSR_OFFSET_DESC_W_INPUT => WR_ONLY   --0xFC Descriptor word input

          when others =>
            null;
        end case;
      end if;
    end process csr_read;
  end generate comb0;

  comb1 : if CSR_READ_INST = FALSE generate
    -- Implement buffered CSR read access
    csro.rdata  <= csr_rd;
  end generate comb1;



  -----------------------------------------------------------------------------
  -- Sequential process
  -----------------------------------------------------------------------------

  seq0 : process (clk, rstn)
  begin
    if(rstn = '0' and ASYNC_RST) then
      csr_regs            <= RESET_CSR_REGS;
      csr_rd              <= (others => '0');
    elsif rising_edge(clk) then
      if(rstn = '0') then
        csr_regs          <= RESET_CSR_REGS;
        csr_rd            <= (others => '0');
      else

      -------------------------
      -- CSR register update --
      -------------------------

        -- Disable the injector if is signaled to do so.
        if(disable = '1') then
          csr_regs.gen_config.enable  <= '0';
        end if;

        -- Reset the software reset bit after a reset has been propagated through the injector.
        if(csr_regs.gen_config.reset_sw = '1') then
          csr_regs.gen_config.reset_sw  <= '0';
          csr_regs.debug  <= RESET_CSR_DEBUG;
        else
          -- Update debug registers
          csr_regs.debug.fetch_pc       <= (others => '0'); --debug_pc(20+PC_LEN-1 downto 20); TODO: Specific PC for every stage
          csr_regs.debug.decode_pc      <= (others => '0'); --debug_pc(10+PC_LEN-1 downto 10);
          csr_regs.debug.exe_pc         <= (others => '0'); --debug_pc(   PC_LEN-1 downto  0);
          csr_regs.debug.fetch_status   <= debug_state.fetch;
          csr_regs.debug.decode_status  <= debug_state.decode;
          csr_regs.debug.exe_status     <= debug_state.exe;
        end if;

        -- Interruption send register and counter
        csr_regs.irq    <= irq_flag;
        csr_regs.irq_counter <= csr_regs.irq_counter + unsigned(std_logic_vector'('0' & irq_flag));

        -- Access request counter
        if(request_granted = '1') then
          csr_regs.request_counter <= csr_regs.request_counter + 1;
        end if;


      ---------------------
      -- CSR READ ACCESS --
      ---------------------

        -- Default signaling CSR data output
        csr_rd          <= (others => '0');

        if(csri.en = '1' and csri.wr_en = '0') then
          case csri.addr is
            when CSR_OFFSET_CONFIG =>               --0x00 Injector control register
              csr_rd(0) <= csr_regs.gen_config.enable;
              csr_rd(1) <= csr_regs.gen_config.hold;
              csr_rd(2) <= csr_regs.gen_config.reset_sw;
              csr_rd(3) <= csr_regs.gen_config.queue_mode_en;
              csr_rd(4) <= csr_regs.gen_config.irq_prog_compl_en;
              csr_rd(5) <= csr_regs.gen_config.irq_err_core_en;
              csr_rd(6) <= csr_regs.gen_config.irq_err_net_en;
              csr_rd(7) <= csr_regs.gen_config.freeze_irq_en;

            when CSR_OFFSET_DEBUG_PC =>             --0x60 Debug: PC from each stage
              csr_rd    <= (others => '0');
              --csr_rd    <= (31 downto 20+PC_LEN => '0') & csr_regs.debug.exe_pc    & TODO: Specific PC for every stage
              --             (19 downto 10+PC_LEN => '0') & csr_regs.debug.decode_pc &
              --             ( 9 downto    PC_LEN => '0') & csr_regs.debug.fetch_pc;

            when CSR_OFFSET_CNT_INT =>              --0x80 Interruption counter
              csr_rd    <= std_logic_vector(csr_regs.irq_counter);

            when CSR_OFFSET_CNT_ACCESS =>           --0x84 Request access counter
              csr_rd    <= std_logic_vector(csr_regs.request_counter);

            when CSR_WRITE_DATA =>                  --0xF0 Interface write data
              csr_rd    <= csr_regs.wr_data;

            when CSR_OFFSET_NET_PROFILE =>          --0xF8 Network interface configuration
              csr_rd    <= csr_regs.network_profile;

            -- CSR_OFFSET_DESC_W_INPUT => WR_ONLY   --0xFC Descriptor word input

            when others =>
              null;
          end case;
        end if;


      ----------------------
      -- CSR WRITE ACCESS --
      ----------------------

        -- Default signaling for registers
        csr_regs.desc_wen <= '0';

        if(csri.en = '1' and csri.wr_en = '1') then
          case csri.addr is
            when CSR_OFFSET_CONFIG =>           --0x00 Injector control register
              csr_regs.gen_config.enable              <= csri.wdata(0);
              csr_regs.gen_config.hold                <= csri.wdata(1);
              csr_regs.gen_config.reset_sw            <= csri.wdata(2);
              csr_regs.gen_config.queue_mode_en       <= csri.wdata(3);
              csr_regs.gen_config.irq_prog_compl_en   <= csri.wdata(4);
              csr_regs.gen_config.irq_err_core_en     <= csri.wdata(5);
              csr_regs.gen_config.irq_err_net_en      <= csri.wdata(6);
              csr_regs.gen_config.freeze_irq_en       <= csri.wdata(7);

            --CSR_OFFSET_DEBUG_PC => RD_ONLY    --0x60 Debug: PC from each stage
            when CSR_OFFSET_CNT_INT =>          --0x80 Interruption counter
              csr_regs.irq_counter      <= unsigned(csri.wdata);

            when CSR_OFFSET_CNT_ACCESS =>       --0x84 Request access counter
              csr_regs.request_counter  <= unsigned(csri.wdata);

            when CSR_WRITE_DATA =>              --0xF0 Interface write data
              csr_regs.wr_data          <= csri.wdata;

            when CSR_OFFSET_NET_PROFILE =>      --0xF8 Network interface configuration
              csr_regs.network_profile  <= csri.wdata;

            when CSR_OFFSET_DESC_W_INPUT =>     --0xFC Descriptor word input
              csr_regs.desc_word        <= csri.wdata;
              csr_regs.desc_wen         <= '1';

            when others =>
              null;
          end case;
        end if;

      end if;
    end if;
  end process seq0;


end architecture rtl;

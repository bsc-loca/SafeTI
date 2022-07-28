-----------------------------------------------------------------------------   
-- Entity:      injector_apb
-- File:        injector_apb.vhd
-- Author:      Francis Fuentes, Oriol Sala
-- Description: APB register interface for Injector core.
------------------------------------------------------------------------------ 
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library safety;
use safety.injector_pkg.all;


--------------------------------------------------------------
-- Entity to read and write APB registers for Injector core --
--------------------------------------------------------------

entity injector_apb is
  generic (
    PC_LEN          : integer                         := 4;   -- Length of PC register
    ASYNC_RST       : boolean                         := TRUE -- Allow asynchronous reset flag
  );
  port (
  -- External I/O
    rstn            : in  std_ulogic;                         -- Reset
    clk             : in  std_ulogic;                         -- Clock
    apbi            : in  apb_slave_in;                       -- APB slave input
    apbo            : out apb_slave_out;                      -- APB slave output
  -- Internal I/O
    -- Signals for CONTROL
    gen_config      : out injector_config;                    -- General injector configuration signals
    -- Signals for FETCH
    desc_word       : out std_logic_vector(31 downto 0);      -- Descriptor word input register from APB
    desc_word_wen   : out std_ulogic;                         -- Write enable for descriptor word input
    -- Signals from CONTROL
    disable         : in  std_ulogic;                         -- Turn off injector execution flag
    irq_flag        : in  std_ulogic                          -- Interruption flag for APB output
  );
end entity injector_apb;

architecture rtl of injector_apb is
  
  -----------------------------------------------------------------------------
  -- Types and reset constants declaration
  -----------------------------------------------------------------------------

  type apb_debug is record --TODO: FINISH DEBUG REGISTERS
    fetch_pc        : std_logic_vector(PC_LEN - 1 downto 0);
    fetch_status    : std_logic_vector(MAX_STATUS_LEN - 1 downto 0);
    fetch_desc      : desc_words;
    decode_pc       : std_logic_vector(PC_LEN - 1 downto 0);
    decode_status   : std_logic_vector(MAX_STATUS_LEN - 1 downto 0);
    decode_desc     : desc_words;
    exe_pc          : std_logic_vector(PC_LEN - 1 downto 0);
    exe_status      : std_logic_vector(MAX_STATUS_LEN - 1 downto 0);
    exe_desc        : desc_words;
  end record apb_debug;
  
  type apb_reg is record
    gen_config      : injector_config;              -- Injector general configuration
    desc_word       : std_logic_vector(31 downto 0);-- Descriptor word APB input register
    desc_wen        : std_ulogic;                   -- Write enable of descriptor word input
    irq             : std_ulogic;                   -- Interruption flag register
    debug           : apb_debug;                    -- Debug register bank
  end record apb_reg;

  -- Reset values for APB registers.
  constant RESET_INJECTOR_CONFIG  : injector_config  := (
    enable            => '0',
    reset_sw          => '0',
    qmode             => '0',
    irq_prog_compl_en => '0',
    irq_err_core_en   => '0',
    irq_err_net_en    => '0',
    freeze_irq_en     => '0'
  );

  constant RESET_DESC_WORDS       : desc_words      := (
    others => ( others => '0' )
  );

  constant RESET_APB_DEBUG        : apb_debug        := (
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

  constant RESET_APB_REGS         : apb_reg          := (
    gen_config        => RESET_INJECTOR_CONFIG,
    desc_word         => (others => '0'),
    desc_wen          => '0',
    irq               => '0',
    debug             => RESET_APB_DEBUG
  );

  
  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------

  -- Registers
  signal apb_regs   : apb_reg;

  -- Signals
  signal apb_input  : apb_reg;
  

begin -- rtl

  -----------------------------------------------------------------------------
  -- Assignments
  -----------------------------------------------------------------------------

  -- I/O assignments
  apbo.irq        <= apb_regs.irq;

  -- Internal output signals from APB interface module
  gen_config      <= apb_regs.gen_config;
  desc_word       <= apb_regs.desc_word;
  desc_word_wen   <= apb_regs.desc_wen;


  ------------------------
  -- APB address decode --
  ------------------------

  comb0 : process(apbi)
  begin
    -- Default signaling APB output
    apbo.rdata        <= (others => '0');
    -- Default signaling for registers
    apb_regs.desc_wen <= '0';

    ---- Read accesses ----
    if(apbi.sel and apbi.en and not apbi.wr_en) = '1' then
      case apbi.addr(7 downto 2) is
        when "000000" =>                --0x00 Injector control register
          apbo.rdata(0) <= apb_regs.gen_config.enable;
          apbo.rdata(1) <= apb_regs.gen_config.reset_sw;
          apbo.rdata(2) <= apb_regs.gen_config.qmode;
          apbo.rdata(3) <= apb_regs.gen_config.irq_prog_compl_en;
          apbo.rdata(4) <= apb_regs.gen_config.irq_err_core_en;
          apbo.rdata(5) <= apb_regs.gen_config.irq_err_net_en;
          apbo.rdata(6) <= apb_regs.gen_config.freeze_irq_en;

        when "000001" =>                --0x04 Injector status register. -- TODO: ADD ERROR STATE+DEBUG REGS
          --prdata(0)  := r.sts.comp;
          --prdata(1)  := r.sts.err;
          --prdata(2)  := r.sts.ongoing;
          --prdata(3)  := r.sts.kick_pend;
          --prdata(4)  := r.sts.irq_flag;
          --prdata(5)  := r.sts.decode_err;
          --prdata(6)  := r.sts.rd_desc_err;
          --prdata(7)  := r.sts.read_if_rd_data_err;
          --prdata(8)  := r.sts.write_if_wr_data_err;
          --prdata(9)  := r.sts.rd_nxt_ptr_err;
          --prdata(14 downto 10) := sts_in.state;
          --prdata(31 downto 15) := (others => '0');

        when others => 
          null;
      end case;
    end if;

    ---- Write accesses ----
    if(apbi.sel and apbi.en and apbi.wr_en) = '1' then
      case apbi.addr(7 downto 2) is
        when "000000" =>                --0x00 Injector control register
          apb_input.gen_config.enable             <= apbi.wdata(0);
          apb_input.gen_config.reset_sw           <= apbi.wdata(1);
          apb_input.gen_config.qmode              <= apbi.wdata(2);
          apb_regs.gen_config.irq_prog_compl_en   <= apbi.wdata(3);
          apb_regs.gen_config.irq_err_core_en     <= apbi.wdata(4);
          apb_regs.gen_config.irq_err_net_en      <= apbi.wdata(5);
          apb_regs.gen_config.freeze_irq_en       <= apbi.wdata(6);

        when "000001" =>                --0x04 Injector status register. Errors are cleared on write --TODO: ADD ERROR FLAGS
          --v.sts.err                   := r.sts.err and not(apbi.wdata(1));
          --v.sts.irq_flag              := r.sts.irq_flag and not(apbi.wdata(5));
          --v.sts.decode_err            := r.sts.decode_err and not(apbi.wdata(6));
          --v.sts.rd_desc_err           := r.sts.rd_desc_err and not(apbi.wdata(7));        
          --v.sts.read_if_rd_data_err   := r.sts.read_if_rd_data_err and not(apbi.wdata(8));
          --v.sts.write_if_wr_data_err  := r.sts.write_if_wr_data_err and not(apbi.wdata(9));
          --v.sts.rd_nxt_ptr_err        := r.sts.rd_nxt_ptr_err and not(apbi.wdata(10));

        when "111111" =>                --0xFC Descriptor word input
          apb_input.desc_word <= apbi.wdata;
          apb_input.desc_wen  <= '1';
        
        when others =>
          null;
      end case;
    end if;
  end process comb0;


  -----------------------------------------------------------------------------
  -- Sequential process
  -----------------------------------------------------------------------------
  
  seq : process (clk, rstn)
  begin
    if(rstn = '0' and ASYNC_RST) then
      apb_regs            <= RESET_APB_REGS;
    elsif rising_edge(clk) then
      if(rstn = '0') then
        apb_regs          <= RESET_APB_REGS;
      else

      -------------------------
      -- APB register update --
      -------------------------

        -- Update APB registers with the input signals
        apb_regs          <= apb_input;

        -- Disable the injector if is signaled, but APB signaling takes priority.
        apb_regs.gen_config.enable  <= apb_input.gen_config.enable or not(disable);

        -- Reset the software reset bit after a reset has been propagated through the injector.
        if(apb_regs.gen_config.reset_sw = '1') then
          apb_regs.gen_config.reset_sw  <= '0';
          apb_regs.debug            <= RESET_APB_DEBUG;
        end if;

        -- Send IRQ through APB bus if there's an interruption to be sent.
        apb_regs.irq      <= irq_flag;


      end if;
    end if;
  end process seq;

  
end architecture rtl;

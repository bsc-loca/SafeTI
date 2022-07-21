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
    PC_LEN          : integer                           := 4;   -- Length of PC register
    pindex          : integer                           := 0;   -- APB configuartion slave index
    pirq            : integer range 0 to APB_IRQ_NMAX-1 := 0;   -- APB configuartion slave irq
    ASYNC_RST       : boolean                           := TRUE -- Allow asynchronous reset flag
  );
  port (
  -- External I/O
    rstn            : in  std_ulogic;                       -- Reset
    clk             : in  std_ulogic;                       -- Clock
    apbi            : in  apb_slave_in;                     -- APB slave input
    apbo            : out apb_slave_out;                    -- APB slave output
  -- Internal I/O
    -- Signals for CONTROL
    gen_config      : out injector_config;                  -- General injector configuration signals
    -- Signals for FETCH
    desc_word       : out std_logic_vector(31 downto 0);    -- Descriptor word input register from APB
    desc_word_wen   : out std_ulogic;                       -- Write enable for descriptor word input
    -- Signals from CONTROL
    irq_flag        : in  std_ulogic;                       -- Interruption flag for APB output
    err_location    : in  std_logic_vector( 2 downto 0);    -- Interruption error flag + state from FETCH
    err_state       : in  std_logic_vector( 5 downto 0)     -- Interruption error flag + state from DECODE
  );
end entity injector_apb;

------------------------------------------------------------------------------
-- Architecture of injector_apb
------------------------------------------------------------------------------

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

  -- Reset values for registers.
  constant RESET_INJECTOR_CONFIG  : injector_config  := (
    en                => '0',
    rst               => '0',
    qmode             => '0',
    freeze_irq_en     => '0',
    irq_prog_compl_en => '0',
    irq_err_en        => '0'
  );

  constant RESET_APB_DEBUG        : apb_debug        := (
    fetch_pc        => (others => '0'),
    fetch_status    => (others => '0'),
    fetch_desc_w0   => (others => '0'),
    fetch_desc_w1   => (others => '0'),
    fetch_desc_w2   => (others => '0'),
    decode_pc       => (others => '0'),
    decode_status   => (others => '0'),
    decode_desc_w0  => (others => '0'),
    decode_desc_w1  => (others => '0'),
    decode_desc_w2  => (others => '0'),
    exe_pc          => (others => '0'),
    exe_status      => (others => '0'),
    exe_desc_w0     => (others => '0'),
    exe_desc_w1     => (others => '0'),
    exe_desc_w2     => (others => '0')
  );

  constant RESET_APB_REGS         : apb_reg          := (
    gen_config      => RESET_INJECTOR_CONFIG,
    desc_word       => (others => '0'),
    desc_wen        => '0',
    debug           => RESET_APB_DEBUG
  );

  -- Reset values for APB output.
  constant RESET_APB_SLAVE_OUT    : apb_slave_out    := (
    rdata           => (others => '0'),
    irq             => (others => '0'),
    config          => (others => '0'),
    index           => 0
  );

  
  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------

  -- Registers
  signal apb_regs : apb_reg;
  

begin -- rtl

  -----------------------------------------------------------------------------
  -- Assignments
  -----------------------------------------------------------------------------

  -- APB registered or constant output signals
  apbo.index      <= pindex;
  apbo.irq(pirq)  <= apb_regs.irq;

  -- Internal output signals from APB interface module
  gen_config      <= apb_regs.gen_config;
  desc_word       <= apb_regs.desc_word;
  desc_word_wen   <= apb_regs.desc_wen;


  -----------------------------------------------------------------------------
  -- Sequential process
  -----------------------------------------------------------------------------
  
  seq : process (clk, rstn)
  begin
    if(rstn = '0' and ASYNC_RST) then
      apbo                <= RESET_APB_SLAVE_OUT;
      apb_regs            <= RESET_APB_REGS;
    elsif rising_edge(clk) then
      if(rstn = '0') then
        apbo              <= RESET_APB_SLAVE_OUT;
        apb_regs          <= RESET_APB_REGS;
      else
        -- Default signaling APB output
        apbo.rdata        <= (others => '0');
        -- Default signaling for registers
        apb_regs.desc_wen <= '0';


      ------------------------
      -- APB address decode --
      ------------------------

        ---- Read accesses ----
        if(apbi.sel(pindex) and apbi.en and not apbi.wr_en) = '1' then
          case apbi.addr(7 downto 2) is
            when "000000" =>                --0x00 Injector control register
              apbo.rdata(0) <= apb_regs.gen_config.en;
              apbo.rdata(1) <= apb_regs.gen_config.rst;
              apbo.rdata(2) <= apb_regs.gen_config.qmode;
              apbo.rdata(3) <= apb_regs.gen_config.freeze_irq_en;
              apbo.rdata(4) <= apb_regs.gen_config.irq_prog_compl_en;
              apbo.rdata(5) <= apb_regs.gen_config.irq_err_en;
              
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
        if(apbi.sel(pindex) and apbi.en and apbi.wr_en ) = '1' then
          case apbi.addr(7 downto 2) is
            when "000000" =>                --0x00 Injector control register
              apb_regs.gen_config.en      <= apbi.wdata(0);
              apb_regs.gen_config.rst     <= apbi.wdata(1);
              apb_regs.gen_config.qmode   <= apbi.wdata(2);
              apb_regs.gen_config.freeze_irq_en     <= apbi.wdata(3);
              apb_regs.gen_config.irq_prog_compl_en <= apbi.wdata(4);
              apb_regs.gen_config.irq_err_en        <= apbi.wdata(5);

            when "000001" =>                --0x04 Injector status register. Errors are cleared on write --TODO: ADD ERROR FLAGS
              --v.sts.err                   := r.sts.err and not(apbi.wdata(1));
              --v.sts.irq_flag              := r.sts.irq_flag and not(apbi.wdata(5));
              --v.sts.decode_err            := r.sts.decode_err and not(apbi.wdata(6));
              --v.sts.rd_desc_err           := r.sts.rd_desc_err and not(apbi.wdata(7));        
              --v.sts.read_if_rd_data_err   := r.sts.read_if_rd_data_err and not(apbi.wdata(8));
              --v.sts.write_if_wr_data_err  := r.sts.write_if_wr_data_err and not(apbi.wdata(9));
              --v.sts.rd_nxt_ptr_err        := r.sts.rd_nxt_ptr_err and not(apbi.wdata(10));

            when "111111" =>                --0xFC Descriptor word input
              apb_regs.desc_word    <= apbi.wdata;
              apb_regs.desc_wen           <= '1';
              
            when others =>
              null;
          end case;
        end if;

      end if;
    end if;
  end process seq;
  
end architecture rtl;

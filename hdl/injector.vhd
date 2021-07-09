-----------------------------------------------------------------------------   
-- Entity:      injector
-- File:        injector.vhd
-- Author:      Oriol Sala
-- Description: Injector core entity.
------------------------------------------------------------------------------ 
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library grlib;
use grlib.stdlib.all;
use grlib.amba.all;
use grlib.config_types.all;
use grlib.config.all;
use grlib.generic_bm_pkg.log_2;
library bsc;
use bsc.injector_pkg.all;
library techmap;
use techmap.gencomp.all;

-----------------------------------------------------------------------------
-- Injector core
-- This is the core layer which integrates Injector modules like CTRL, READ_IF, WRITE_IF and APB.
-----------------------------------------------------------------------------

entity injector is
  generic (
    tech     : integer range 0 to NTECH     := inferred; -- Target technology
    -- APB configuration  
    pindex   : integer                      := 0;        -- APB configuartion slave index
    paddr    : integer                      := 0;        -- APB configuartion slave address
    pmask    : integer                      := 16#FF8#;  -- APB configuartion slave mask
    pirq     : integer range 0 to NAHBIRQ-1 := 0;        -- APB configuartion slave irq
    -- Bus master configuration
    dbits    : integer range 32 to 128      := 32        -- Data width of BM and FIFO        
    );
  port (
    rstn    : in  std_ulogic;                    -- Reset
    clk     : in  std_ulogic;                    -- Clock
    -- APB interface signals
    apbi    : in  apb_slv_in_type;               -- APB slave input
    apbo    : out apb_slv_out_type;              -- APB slave output
    -- Bus master signals
    bm0_in  : out bm_in_type;                    -- Input to Bus master 0
    bm0_out : in  bm_out_type                    -- Output from Bus master 0

  );
end entity injector;

------------------------------------------------------------------------------
-- Architecture of injector
------------------------------------------------------------------------------

architecture rtl of injector is
  -----------------------------------------------------------------------------
  -- Constant declaration
  -----------------------------------------------------------------------------

  attribute sync_set_reset         : string;
  attribute sync_set_reset of rstn : signal is "true";
  -- Constant for bit - byte manipulation
  constant SHIFT_BIT               : natural := 3;
  constant bm_bytes                : integer := to_integer(shift_right(unsigned(conv_std_logic_vector(dbits, 9)), SHIFT_BIT));
   
  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------  
  -- APB interface signals
  signal ctrl_reg      : injector_ctrl_reg_type;
  signal desc_ptr_reg  : injector_desc_ptr_type;
  signal err_status    : std_ulogic;
  signal err_sts_data  : std_ulogic;
  signal status        : status_out_type;
  signal active        : std_ulogic;
  -- READ_IF
  signal read_if_status    : d_ex_sts_out_type;
  signal read_if_start     : std_ulogic;
  signal read_if_bmo       : bm_out_type;
  signal read_if_bmi       : bm_ctrl_reg_type;
  -- WRITE_IF
  signal write_if_status    : d_ex_sts_out_type;
  signal write_if_start     : std_ulogic;
  signal write_if_bmi       : bm_ctrl_reg_type;
  signal write_if_bmo       : bm_out_type;
  -- DELAY_IF
  signal delay_if_status    : d_ex_sts_out_type;
  signal delay_if_start     : std_ulogic;
  --Control
  signal ctrl_rst      : std_ulogic;
  signal ctrl_bmo      : bm_out_type;
  signal ctrl_bmi      : bm_in_type;
  signal curr_desc     : curr_des_out_type;
  signal curr_desc_ptr : std_logic_vector(31 downto 0);
  signal data_desc     : data_dsc_strct_type;
  signal irq_flag_sts  : std_ulogic;

  -----------------------------------------------------------------------------
  -- Function/procedure declaration
  -----------------------------------------------------------------------------
  
begin  -- rtl

  -----------------
  -- Assignments --
  -----------------
  -----------------------------------------------------------------------------
  -- Glue logic - Signal assignments
  -----------------------------------------------------------------------------
   bm0_in       <= ctrl_bmi;
   ctrl_bmo     <= bm0_out;
  
  -----------------------------------------------------------------------------
  -- Component instantiation
  -----------------------------------------------------------------------------

  -- APB interface
  apb : injector_apb
    generic map (
      pindex   => pindex,
      paddr    => paddr,
      pmask    => pmask,
      pirq     => pirq,
      dbits    => dbits
    )
    port map (
      rstn          => rstn,
      clk           => clk,
      apbi          => apbi,
      apbo          => apbo,
      ctrl_out      => ctrl_reg,
      desc_ptr_out  => desc_ptr_reg,
      active        => active,
      err_status    => err_status,
      irq_flag_sts  => irq_flag_sts,
      curr_desc_in  => curr_desc,
      curr_desc_ptr => curr_desc_ptr,
      sts_in        => status
      );

  -- READ_IF
  read_if : injector_read_if
    generic map (
      dbits      => dbits,
      bm_bytes   => bm_bytes
      )
    port map (
      rstn              => rstn,
      clk               => clk,
      ctrl_rst          => ctrl_rst,
      err_sts_in        => err_sts_data,
      read_if_start     => read_if_start,
      d_des_in          => data_desc,
      status_out        => read_if_status,
      read_if_bmi       => read_if_bmo,
      read_if_bmo       => read_if_bmi
      );  

  -- WRITE_IF
  write_if : injector_write_if
    generic map (
      dbits      => dbits,
      bm_bytes   => bm_bytes
      )
    port map (
      rstn              => rstn,
      clk               => clk,
      ctrl_rst          => ctrl_rst,
      err_sts_in        => err_sts_data,
      write_if_start    => write_if_start,
      d_des_in          => data_desc,
      status_out        => write_if_status,
      write_if_bmi      => write_if_bmo,
      write_if_bmo      => write_if_bmi
      );

  -- DELAY_IF
  delay_if : injector_delay_if
    port map (
      rstn              => rstn,
      clk               => clk,
      ctrl_rst          => ctrl_rst,
      err_sts_in        => err_sts_data,
      delay_if_start    => delay_if_start,
      d_des_in          => data_desc,
      status_out        => delay_if_status
      );


  -- Control module
  ctrl : injector_ctrl
    generic map (
      dbits => dbits
      )  
    port map (
      rstn              => rstn,
      clk               => clk,
      ctrl              => ctrl_reg,
      des_ptr           => desc_ptr_reg,
      active            => active,
      err_status        => err_status,
      curr_desc_out     => curr_desc,
      curr_desc_ptr     => curr_desc_ptr,
      status            => status,
      irq_flag_sts      => irq_flag_sts,
      bm_in             => ctrl_bmo,
      bm_out            => ctrl_bmi,
      read_if_bm_in     => read_if_bmi,
      read_if_bm_out    => read_if_bmo,
      write_if_bm_in    => write_if_bmi,
      write_if_bm_out   => write_if_bmo,
      d_desc_out        => data_desc,
      ctrl_rst          => ctrl_rst,
      err_sts_out       => err_sts_data,
      read_if_start     => read_if_start,
      read_if_sts_in    => read_if_status,
      write_if_sts_in   => write_if_status,
      write_if_start    => write_if_start,
      delay_if_sts_in   => delay_if_status,
      delay_if_start    => delay_if_start
      );  

end architecture rtl;

-----------------------------------------------------------------------------   
-- Entity:      injector_control
-- File:        injector_control.vhd
-- Author:      Francisco Fuentes, Oriol Sala
-- Description: Main control module for the Traffic Injector
------------------------------------------------------------------------------ 
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library safety;
use safety.injector_pkg.all;

---------------------------------------------------------------------------------------------------------
-- Combinational module for controlling the injector pipeline and setting the proper interruption flag --
---------------------------------------------------------------------------------------------------------

entity injector_control is
  port (
    -- Signals from/for APB interface
    apb_config        : in  injector_config;                  -- Injector configuration
    disable           : out std_logic;                        -- Disable injector flag
    irq_send          : out std_logic;                        -- Send interruption flag
    -- Pipeline control signals
    enable_pipeline   : out std_logic_vector(1 to 3);         -- Enable pipeline modules
    rst_sw_pipeline   : out std_logic_vector(1 to 3);         -- Reset pipeline modules
    irq_err_pipeline  : in  std_logic_vector(1 to 3);         -- Interruption from pipeline stages
    irq_err_network   : in  std_logic_vector(0 to 1);         -- Network interruptions (0 = READ, 1 = WRITE)
    -- EXE output signals
    exe_irq_desc_comp : in  std_logic;                        -- Interrupt enable for complete descriptor
    exe_desc_comp     : in  std_logic;                        -- Descriptor completed flag
    exe_program_comp  : in  std_logic                         -- Injector program complete flag
  );
end entity injector_control;


architecture rtl of injector_control is

  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------

  signal interruption : std_logic;


begin
  -----------------
  -- Assignments --
  -----------------

  -- I/O assignments
  irq_send  <= interruption and apb_config.enable;

  -- Interruption signal management
  interruption <= 
    (exe_desc_comp      and exe_irq_desc_comp           ) or
    (exe_program_comp   and apb_config.irq_prog_compl_en) or
    ((irq_err_pipeline(PL_FETCH) or irq_err_pipeline(PL_DECODE) or irq_err_pipeline(PL_EXE)) and apb_config.irq_err_core_en) or
    ((irq_err_network(0) or irq_err_network(1)                                             ) and apb_config.irq_err_net_en );

  -- Disable signal management
  disable <= interruption and apb_config.freeze_irq_en;


end architecture rtl;

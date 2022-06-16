-----------------------------------------------------------------------------   
-- Entity:      injector
-- File:        injector.vhd
-- Author:      Oriol Sala
-- Description: Injector core entity.
------------------------------------------------------------------------------ 
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library safety;
use safety.injector_pkg.all;

-----------------------------------------------------------------------------
-- Injector core
-- This is the core layer which integrates Injector modules like CTRL, READ_IF, WRITE_IF and APB.
-----------------------------------------------------------------------------

entity injector is
  generic (
    -- Injector configuration
    mem_Ndesc     : integer range 1 to  256          :=   16;     -- Maximum number of programmable descriptors [Only power of 2s allowed]
    dbits         : integer range 8 to 1024          :=   32;     -- Data width of BM and FIFO at injector. [Only power of 2s allowed]
    MAX_SIZE_BURST: integer range 8 to 4096          := 1024;     -- Maximum number of bytes allowed at a burst transaction.
    -- APB configuration  
    pindex        : integer                           := 0;       -- APB configuartion slave index
    paddr         : integer                           := 0;       -- APB configuartion slave address
    pmask         : integer                           := 16#FFF#; -- APB configuartion slave mask
    pirq          : integer range 0 to APB_IRQ_NMAX-1 := 0;       -- APB configuartion slave irq
    -- Asynchronous reset configuration
    ASYNC_RST     : boolean                           := FALSE    -- Allow asynchronous reset
    );
  port (
    rstn          : in  std_ulogic;           -- Reset
    clk           : in  std_ulogic;           -- Clock
    -- APB interface signals
    apbi          : in  apb_slave_in_type;    -- APB slave input
    apbo          : out apb_slave_out_type;   -- APB slave output
    -- Bus master signals
    bm0_mosi      : out bm_mosi;              -- Input to Master interface 0
    bm0_miso      : in  bm_miso               -- Output from Master interface 0
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

  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------  
  -- APB interface signals
  signal ctrl_reg           : injector_ctrl_reg_type;
  signal err_status         : std_ulogic;
  signal err_sts_data       : std_ulogic;
  signal status             : status_out_type;
  signal active             : std_ulogic;
  signal desc_mem           : descriptor_memory(mem_Ndesc - 1 downto 0);
  -- READ_IF
  signal read_if_status     : d_ex_sts_out_type;
  signal read_if_start      : std_ulogic;
  signal read_if_bmo        : bm_miso;
  signal read_if_bmi        : bm_mosi;
  -- WRITE_IF
  signal write_if_status    : d_ex_sts_out_type;
  signal write_if_start     : std_ulogic;
  signal write_if_bmi       : bm_mosi;
  signal write_if_bmo       : bm_miso;
  -- DELAY_IF
  signal delay_if_status    : d_ex_sts_out_type;
  signal delay_if_start     : std_ulogic;
  --Control
  signal ctrl_rst           : std_ulogic;
  signal ctrl_bmo           : bm_miso;
  signal ctrl_bmi           : bm_mosi;
  signal curr_desc          : curr_des_out_type;
  signal curr_desc_ptr      : std_logic_vector(31 downto 0);
  signal desc_ctrl          : descriptor_control;
  signal desc_actaddr       : descriptor_actionaddr;
  signal desc_branch        : descriptor_branch;
  signal irq_flag_sts       : std_ulogic;

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
   bm0_mosi     <= ctrl_bmi;
   ctrl_bmo     <= bm0_miso;
  
  -----------------------------------------------------------------------------
  -- Component instantiation
  -----------------------------------------------------------------------------

  -- APB interface
  apb : injector_apb
    generic map (
      pindex          => pindex,
      paddr           => paddr,
      pmask           => pmask,
      pirq            => pirq,
      mem_Ndesc       => mem_Ndesc,
      ASYNC_RST       => ASYNC_RST
    )
    port map (
      rstn            => rstn,
      clk             => clk,
      apbi            => apbi,
      apbo            => apbo,
      ctrl_out        => ctrl_reg,
      active          => active,
      err_status      => err_status,
      irq_flag_sts    => irq_flag_sts,
      curr_desc_in    => curr_desc,
      curr_desc_ptr   => curr_desc_ptr,
      sts_in          => status,
      desc_mem_out    => desc_mem
    );

  -- READ_IF
  read_if : injector_read_if
    generic map (
      dbits           => dbits,
      MAX_SIZE_BURST  => MAX_SIZE_BURST,
      ASYNC_RST       => ASYNC_RST
    )
    port map (
      rstn            => rstn,
      clk             => clk,
      ctrl_rst        => ctrl_rst,
      err_sts_in      => err_sts_data,
      read_if_start   => read_if_start,
      desc_ctrl       => desc_ctrl,
      desc_actaddr    => desc_actaddr,
      status_out      => read_if_status,
      read_if_bmi     => read_if_bmo,
      read_if_bmo     => read_if_bmi
    );  

  -- WRITE_IF
  write_if : injector_write_if
    generic map (
      dbits           => dbits,
      MAX_SIZE_BURST  => MAX_SIZE_BURST,
      ASYNC_RST       => ASYNC_RST
    )
    port map (
      rstn            => rstn,
      clk             => clk,
      ctrl_rst        => ctrl_rst,
      err_sts_in      => err_sts_data,
      write_if_start  => write_if_start,
      desc_ctrl       => desc_ctrl,
      desc_actaddr    => desc_actaddr,
      status_out      => write_if_status,
      write_if_bmi    => write_if_bmo,
      write_if_bmo    => write_if_bmi
    );

  -- DELAY_IF
  delay_if : injector_delay_if
    generic map (
      ASYNC_RST       => ASYNC_RST
    )
    port map (
      rstn            => rstn,
      clk             => clk,
      ctrl_rst        => ctrl_rst,
      err_sts_in      => err_sts_data,
      delay_if_start  => delay_if_start,
      desc_ctrl       => desc_ctrl,
      desc_branch     => desc_branch,
      status_out      => delay_if_status
    );


  -- Control module
  ctrl : injector_ctrl
    generic map (
      dbits           => dbits,
      mem_Ndesc       => mem_Ndesc,
      ASYNC_RST       => ASYNC_RST
    )  
    port map (
      rstn            => rstn,
      clk             => clk,
      ctrl            => ctrl_reg,
      active          => active,
      err_status      => err_status,
      curr_desc_out   => curr_desc,
      curr_desc_ptr   => curr_desc_ptr,
      status          => status,
      irq_flag_sts    => irq_flag_sts,
      bm_in           => ctrl_bmo,
      bm_out          => ctrl_bmi,
      read_if_bm_in   => read_if_bmi,
      read_if_bm_out  => read_if_bmo,
      write_if_bm_in  => write_if_bmi,
      write_if_bm_out => write_if_bmo,
      desc_ctrl_out   => desc_ctrl,
      desc_actaddr_out=> desc_actaddr,
      desc_branch_out => desc_branch,
      ctrl_rst        => ctrl_rst,
      err_sts_out     => err_sts_data,
      read_if_start   => read_if_start,
      read_if_sts_in  => read_if_status,
      write_if_sts_in => write_if_status,
      write_if_start  => write_if_start,
      delay_if_sts_in => delay_if_status,
      delay_if_start  => delay_if_start,
      desc_mem        => desc_mem
    );

end architecture rtl;

-----------------------------------------------------------------------------   
-- Entity:      injector_axi
-- File:        injector_axi.vhd
-- Author:      Francis Fuentes Diaz (BSC-CNS)
-- Description: injector top level entity.
------------------------------------------------------------------------------ 
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library bsc;
use bsc.injector_pkg.apb_slave_in_type;
use bsc.injector_pkg.apb_slave_out_type;
use bsc.injector_pkg.numTech;
use bsc.injector_pkg.typeTech;
use bsc.injector_pkg.APB_IRQ_NMAX;
use bsc.injector_pkg.injector;
use bsc.injector_pkg.dbits;
use bsc.axi4_pkg.all;

-----------------------------------------------------------------------------
-- Top level entity for injector.
-- This is a wrapper which integrates injector core to the
-- AXI4 master - generic bus master bridge
-----------------------------------------------------------------------------

entity injector_axi is
  generic (
    tech          : integer range 0 to numTech          := typeTech; -- Target technology
    -- APB configuration  
    pindex        : integer                             := 0;       -- APB configuartion slave index
    paddr         : integer                             := 0;       -- APB configuartion slave address
    pmask         : integer                             := 16#FFF#; -- APB configuartion slave mask
    pirq          : integer range 0 to APB_IRQ_NMAX - 1 := 1;       -- APB configuartion slave irq
    -- AXI Master configuration
    axi_id        : integer                             := 0        -- AXI master index
    
  );
  port (
    rstn          : in  std_ulogic;         -- Reset
    clk           : in  std_ulogic;         -- Clock
    -- APB interface signals
    apbi          : in  apb_slave_in_type;  -- APB slave input
    apbo          : out apb_slave_out_type; -- APB slave output
    -- AXI interface signals
    axi4mi        : in  axi4_miso;          -- AXI4 master input 
    axi4mo        : out axi4_mosi           -- AXI4 master output
  );
end entity injector_axi;

architecture rtl of injector_axi is

  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------

  -- I/O Injector and AXI interface
  signal bm_in_injector   : bsc.injector_pkg.bm_in_type;
  signal bm_out_injector  : bsc.injector_pkg.bm_out_type;
  signal bm_in_manager    : bsc.axi4_pkg.bm_mosi;
  signal bm_out_manager   : bsc.axi4_pkg.bm_miso;

begin

  -----------------
  -- Assignments --
  -----------------

  bm_in_manager.rd_addr         <= bm_in_injector.rd_addr;
  bm_in_manager.rd_size         <= bm_in_injector.rd_size;
  bm_in_manager.rd_req          <= bm_in_injector.rd_req;
  bm_in_manager.wr_addr         <= bm_in_injector.wr_addr;
  bm_in_manager.wr_size         <= bm_in_injector.wr_size;
  bm_in_manager.wr_req          <= bm_in_injector.wr_req;
  bm_in_manager.wr_data         <= bm_in_injector.wr_data;

  bm_out_injector.rd_data       <= bm_out_manager.rd_data;
  bm_out_injector.rd_req_grant  <= bm_out_manager.rd_req_grant;
  bm_out_injector.rd_valid      <= bm_out_manager.rd_valid;
  bm_out_injector.rd_done       <= bm_out_manager.rd_done;
  bm_out_injector.rd_err        <= bm_out_manager.rd_err;
  bm_out_injector.wr_req_grant  <= bm_out_manager.wr_req_grant;
  bm_out_injector.wr_full       <= bm_out_manager.wr_full;
  bm_out_injector.wr_done       <= bm_out_manager.wr_done;
  bm_out_injector.wr_err        <= bm_out_manager.wr_err;
  

  -----------------------------------------------------------------------------
  -- Component instantiation
  -----------------------------------------------------------------------------

  -- injector core
  core : injector
    generic map (
      pindex          => pindex,
      paddr           => paddr,
      pmask           => pmask,
      pirq            => pirq,
      ASYNC_RST       => FALSE
    )
    port map (
      rstn            => rstn,
      clk             => clk,
      apbi            => apbi,
      apbo            => apbo,
      bm0_in          => bm_in_injector,
      bm0_out         => bm_out_injector
    );

  axi4M : axi4_manager
    generic map (
      dbits           => dbits,
      axi_id          => 0,
      ASYNC_RST       => FALSE
    )
    port map (
      rstn            => rstn,
      clk             => clk,
      axi4mi          => axi4mi,
      axi4mo          => axi4mo,
      bm_in           => bm_in_manager,
      bm_out          => bm_out_manager,
      bm_in_bypass_rd => '0'
    );

end architecture rtl;
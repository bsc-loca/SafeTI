-----------------------------------------------------------------------------   
-- Entity:      injector_ahb
-- File:        injector_ahb.vhd
-- Author:      Oriol Sala
-- Description: injector top level entity.
------------------------------------------------------------------------------ 
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
--library grlib;
--use grlib.config_types.all;
--use grlib.config.all;
--use grlib.stdlib.all;
--use grlib.amba.all;
--use grlib.devices.all;
--use grlib.generic_bm_pkg.all;
library bsc;
use bsc.injector_pkg.all;
--library techmap;
--use techmap.gencomp.all;

-----------------------------------------------------------------------------
-- Top level entity for injector.
-- This is a wrapper which integrates injector core to the
-- AHB master - generic bus master bridge
-----------------------------------------------------------------------------

entity injector_ahb is
  generic (
    tech              : integer range  0 to numTech         := typeTech;  -- Target technology
    -- APB configuration  
    pindex            : integer                             := 0;         -- APB configuartion slave index
    paddr             : integer                             := 0;         -- APB configuartion slave address
    pmask             : integer                             := 16#FF8#;   -- APB configuartion slave mask
    pirq              : integer range  0 to APB_IRQ_NMAX-1  := 0;         -- APB configuartion slave irq
    -- Bus master configuration
    dbits             : integer range 32 to 128             := 32;        -- Data width of BM and FIFO    
    hindex            : integer                             := 0;         -- AHB master index
    MAX_SIZE_BEAT     : integer range 32 to 1024            := 1024;      -- Maximum size of a beat at a burst transaction.
    -- Injector configuration
    ASYNC_RST         : boolean                             := FALSE      -- Allow asynchronous reset flag
    );
  port (
    rstn    : in  std_ulogic;                    -- Reset
    clk     : in  std_ulogic;                    -- Clock
    -- APB interface signals
    apbi    : in  apb_slave_in_type;             -- APB slave input
    apbo    : out apb_slave_out_type;            -- APB slave output
    -- AHB interface signals
    bm_in   : out bm_in_type;                    -- For AHB master 0 input to bus
    bm_out  : in  bm_out_type                    -- For AHB master 0 output to bus
    );
end entity injector_ahb;

------------------------------------------------------------------------------
-- Architecture of grdmac2
------------------------------------------------------------------------------

architecture rtl of injector_ahb is
  -----------------------------------------------------------------------------
  -- Constant declaration
  -----------------------------------------------------------------------------
  attribute sync_set_reset         : string;
  attribute sync_set_reset of rstn : signal is "true";

  -----------------------------------------------------------------------------
  -- Records and types
  -----------------------------------------------------------------------------

  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------

  -----------------------------------------------------------------------------
  -- Function/procedure declaration
  -----------------------------------------------------------------------------
  
begin  -- rtl

  -----------------
  -- Assignments --
  -----------------

  -----------------------------------------------------------------------------
  -- Component instantiation
  -----------------------------------------------------------------------------

  -- injector core
  core : injector
    generic map (
      tech          => tech,
      pindex        => pindex,
      paddr         => paddr,
      pmask         => pmask,
      pirq          => pirq,
      dbits         => dbits,
      MAX_SIZE_BEAT => MAX_SIZE_BEAT
      )
    port map (
      rstn    => rstn,
      clk     => clk,
      apbi    => apbi,
      apbo    => apbo,
      bm0_in  => bm_in,
      bm0_out => bm_out
      );
  
end architecture rtl;




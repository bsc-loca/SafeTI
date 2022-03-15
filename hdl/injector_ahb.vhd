-----------------------------------------------------------------------------   
-- Entity:      injector_ahb
-- File:        injector_ahb.vhd
-- Author:      Oriol Sala
-- Description: injector top level entity.
------------------------------------------------------------------------------ 
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library bsc;
use bsc.injector_pkg.all;

-----------------------------------------------------------------------------
-- Top level entity for injector.
-- This is a wrapper which integrates injector core to the
-- AHB master - generic bus master bridge
-----------------------------------------------------------------------------

entity injector_ahb is
  generic (
    -- SafeTI configuration
    dbits             : integer range 32 to  128            := 32;        -- Data width of BM and FIFO at injector. [Only power of 2s allowed]
    MAX_SIZE_BURST    : integer range 32 to 1024            := 1024;      -- Maximum size of a beat at a burst transaction.
    tech              : integer range  0 to numTech         := typeTech;  -- Target technology
    -- APB configuration  
    pindex            : integer                             := 0;         -- APB configuartion slave index
    paddr             : integer                             := 0;         -- APB configuartion slave address
    pmask             : integer                             := 16#FFF#;   -- APB configuartion slave mask
    pirq              : integer range  0 to APB_IRQ_NMAX-1  := 1;         -- APB configuartion slave irq
    -- Bus master configuration
    hindex            : integer                             := 0;         -- AHB master index
    -- Asynchronous reset configuration
    ASYNC_RST         : boolean                             := FALSE      -- Allow asynchronous reset flag
    );
  port (
    rstn              : in  std_ulogic;           -- Reset
    clk               : in  std_ulogic;           -- Clock
    -- APB interface signals
    apbi              : in  apb_slave_in_type;    -- APB slave input
    apbo              : out apb_slave_out_type;   -- APB slave output
    -- AHB interface signals
    bm_mosi           : out bm_mosi;              -- For AHB master 0 input to bus
    bm_miso           : in  bm_miso               -- For AHB master 0 output to bus
    );
end entity injector_ahb;

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
      dbits         => dbits,
      MAX_SIZE_BURST=> MAX_SIZE_BURST,
      pindex        => pindex,
      paddr         => paddr,
      pmask         => pmask,
      pirq          => pirq,
      ASYNC_RST     => ASYNC_RST
      )
    port map (
      rstn          => rstn,
      clk           => clk,
      apbi          => apbi,
      apbo          => apbo,
      bm0_mosi      => bm_mosi,
      bm0_miso      => bm_miso
      );
  
end architecture rtl;




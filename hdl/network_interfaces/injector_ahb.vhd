-----------------------------------------------------------------------------
-- Entity:      injector_ahb
-- File:        injector_ahb.vhd
-- Author:      Oriol Sala
-- Description: injector top level entity.
------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library safety;
use safety.injector_pkg.all;

-----------------------------------------------------------------------------
-- Top level entity for injector.
-- This is a wrapper which integrates injector core to the
-- AHB master - generic bus master bridge
-----------------------------------------------------------------------------

entity injector_ahb is
  generic (
    -- Injector configuration
    PC_LEN            : integer range 2 to   10       :=    4;-- Set the maximum number of programmable descriptor words to 2^PC_LEN
    CORE_DATA_WIDTH   : integer range 8 to 1024       :=   32;-- Data width of the injector core. [Only power of 2s allowed]
    MAX_SIZE_BURST    : integer range 8 to 4096       := 1024;-- Maximum number of bytes allowed at a burst transaction.
    ASYNC_RST         : boolean                       := TRUE -- Allow asynchronous reset
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
  core : injector_core
    generic map (
      PC_LEN          => PC_LEN,
      CORE_DATA_WIDTH => DATA_WIDTH,
      MAX_SIZE_BURST  => MAX_SIZE_BURST,
      ASYNC_RST       => ASYNC_RST
    )
    port map (
      rstn            => rstn,
      clk             => clk,
      apbi            => apbi,
      apbo            => apbo,
      bm_out          => bm_mosi,
      bm_in           => bm_miso
    );

end architecture rtl;

-----------------------------------------------------------------------------   
-- Entity:      injector_axi4_SELENE
-- File:        injector_axi4_SELENE.vhd
-- Author:      Francis Fuentes
-- Description: injector top level entity for SELENE platform.
------------------------------------------------------------------------------ 

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library grlib;
use grlib.config_types.all;
use grlib.config.all;
use grlib.stdlib.all;
use grlib.amba.all;
use grlib.devices.all;
use grlib.generic_bm_pkg.all;
library bsc;
use bsc.injector_pkg.all;
library techmap;
use techmap.gencomp.all;

entity injector_axi4_SELENE is
  generic (
    tech          : integer range 0 to NTECH      := inferred;-- Target technology
    -- APB configuration  
    pindex        : integer                       := 0;       -- APB configuartion slave index
    paddr         : integer                       := 0;       -- APB configuartion slave address
    pmask         : integer                       := 16#FFF#; -- APB configuartion slave mask
    pirq          : integer range 0 to NAHBIRQ-1  := 1;       -- APB configuartion slave irq
    -- Bus master configuration
    dbits         : integer range 32 to  128      := 32;      -- Data width of BM and FIFO (must be a power of 2)
    -- AXI Master configuration
    axi_id        : integer                       := 0;       -- AXI master index
    MAX_SIZE_BEAT : integer range 64 to 4096      := 1024     -- Maximum size of a beat at a burst transaction.
  );
  port (
    rstn    : in  std_ulogic; -- Reset
    clk     : in  std_ulogic; -- Clock
    -- APB interface signals
    apbi    : in  apb_slv_in_type;  -- APB subordinate input to injector
    apbo    : out apb_slv_out_type; -- APB subordinate output from injector
    -- AXI4 interconnect SELENE bus
    axi4mi  : in  axi_somi_type;    -- AXI4 manager input to injector
    axi4mo  : out axi4_mosi_type    -- AXI4 manager output from injector
  );
end entity injector_axi4_SELENE;


architecture rtl of injector_axi4_SELENE is

  -----------------------------------------------------------------------------
  -- Constant declaration
  -----------------------------------------------------------------------------
  attribute sync_set_reset         : string;
  attribute sync_set_reset of rstn : signal is "true";

  -- Reset configuration
  constant ASYNC_RST : boolean := GRLIB_CONFIG_ARRAY(grlib_async_reset_enable) = 1;

  -----------------------------------------------------------------------------
  -- Records and types
  -----------------------------------------------------------------------------


  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------
  -- AXI master interface (injector)
  signal axi_in     : axi4_in_type;
  signal axi_out    : axi4_out_type;

  -----------------------------------------------------------------------------
  -- Function/procedure declaration
  -----------------------------------------------------------------------------
  
begin  -- rtl

  -----------------
  -- Assignments --
  -----------------
  -- AXI Master input to the injector
  axi_in.aw_ready   <= axi4mi.aw.ready;
  axi_in.w_ready    <= axi4mi.w.ready;
  axi_in.b_id       <= axi4mi.b.id;
  axi_in.b_resp     <= axi4mi.b.resp;
  axi_in.b_valid    <= axi4mi.b.valid;
  axi_in.ar_ready   <= axi4mi.ar.ready;
  axi_in.r_id       <= axi4mi.r.id;
  axi_in.r_data     <= axi4mi.r.data;
  axi_in.r_resp     <= axi4mi.r.resp;
  axi_in.r_last     <= axi4mi.r.last;
  axi_in.r_valid    <= axi4mi.r.valid;
  -- AXI Master output from the injector
  axi4mo.aw.id      <= axi_out.aw_id;
  axi4mo.aw.addr    <= axi_out.aw_addr;
  axi4mo.aw.len     <= axi_out.aw_len;
  axi4mo.aw.size    <= axi_out.aw_size;
  axi4mo.aw.burst   <= axi_out.aw_burst;
  axi4mo.aw.lock    <= axi_out.aw_lock; 
  axi4mo.aw.cache   <= axi_out.aw_cache;
  axi4mo.aw.prot    <= axi_out.aw_prot; 
  axi4mo.aw.valid   <= axi_out.aw_valid;
  axi4mo.aw.qos     <= axi_out.aw_qos;  
  axi4mo.w.data     <= axi_out.w_data;
  axi4mo.w.strb     <= axi_out.w_strb;
  axi4mo.w.last     <= axi_out.w_last;
  axi4mo.w.valid    <= axi_out.w_valid;
  axi4mo.b.ready    <= axi_out.b_ready;
  axi4mo.ar.id      <= axi_out.ar_id;
  axi4mo.ar.addr    <= axi_out.ar_addr;
  axi4mo.ar.len     <= axi_out.ar_len;
  axi4mo.ar.size    <= axi_out.ar_size;
  axi4mo.ar.burst   <= axi_out.ar_burst;
  axi4mo.ar.lock    <= axi_out.ar_lock; 
  axi4mo.ar.cache   <= axi_out.ar_cache;
  axi4mo.ar.prot    <= axi_out.ar_prot; 
  axi4mo.ar.valid   <= axi_out.ar_valid;
  axi4mo.r.ready    <= axi_out.r_ready;  

  -----------------------------------------------------------------------------
  -- Component instantiation
  -----------------------------------------------------------------------------

  -- injector AXI interface
  AXIinterface : injector_axi
    generic map (
      tech          => tech,          -- Target technology
      -- APB configuration  
      pindex        => pindex,        -- APB configuartion slave index
      paddr         => paddr,         -- APB configuartion slave address
      pmask         => pmask,         -- APB configuartion slave mask
      pirq          => pirq,          -- APB configuartion slave irq
      -- AXI4 Master configuration (other parameters must be updated at injector_pkg.vhd)
      axi_id        => axi_id,        -- AXI ID
      -- Injector configuration
      ASYNC_RST     => ASYNC_RST      -- Allow asynchronous reset flag
      )
    port map (
      rstn    => rstn,    -- Reset
      clk     => clk,     -- Clock
      -- APB interface signals
      apbi    => apbi,    -- APB slave input
      apbo    => apbo,    -- APB slave output
      -- AXI interface signals
      axi4mi  => axi_in,  -- AXI4 master input 
      axi4mo  => axi_out  -- AXI4 master output
      );

end architecture rtl;
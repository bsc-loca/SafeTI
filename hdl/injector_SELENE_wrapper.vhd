-----------------------------------------------------------------------------   
-- Entity:      injector_SELENE_wrapper
-- File:        injector_SELENE_wrapper.vhd
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
use bsc.injector_settings.all;
library techmap;
use techmap.gencomp.all;

entity injector_SELENE_wrapper is
    generic (
        tech             : integer range 0 to NTECH     := inferred;  -- Target technology
        -- APB configuration  
        pindex           : integer                      := 0;         -- APB configuartion slave index
        paddr            : integer                      := 0;         -- APB configuartion slave address
        pmask            : integer                      := 16#FF8#;   -- APB configuartion slave mask
        pirq             : integer range 0 to NAHBIRQ-1 := 0;         -- APB configuartion slave irq
        -- Bus master configuration
        dbits            : integer range 32 to 128      := 32;        -- Data width of BM and FIFO    
        max_burst_length : integer range 2 to 256       := 128        -- BM backend burst length in words. Total burst of 'Max_size'bytes, is split in to bursts of 'max_burst_length' bytes by the BMIF
    );
    port (
    rstn    : in  std_ulogic;                    -- Reset
    clk     : in  std_ulogic;                    -- Clock
    -- AXI interconnect SELENE bus
    axi4bi  : in  axi_somi_type;
    axi4bo  : out axi4_mosi_type
    );
end entity injector_SELENE_wrapper;


architecture rtl of injector_SELENE_wrapper is

  -----------------------------------------------------------------------------
  -- Constant declaration
  -----------------------------------------------------------------------------
  attribute sync_set_reset         : string;
  attribute sync_set_reset of rstn : signal is "true";

  -- Reset configuration
  constant ASYNC_RST : boolean := GRLIB_CONFIG_ARRAY(grlib_async_reset_enable) = 1;

  -- Bus master interface burst chop mask
  constant burst_chop_mask : integer := (max_burst_length*(log_2(AXIDW)-1));

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
  -- AXI input to the injector
  axi_in.aw_ready   <= axi4bi.aw.ready;
  axi_in.w_ready    <= axi4bi.w.ready;
  axi_in.b_id       <= axi4bi.b.id;
  axi_in.b_resp     <= axi4bi.b.resp;
  axi_in.b_valid    <= axi4bi.b.valid;
  axi_in.ar_ready   <= axi4bi.ar.ready;
  axi_in.r_id       <= axi4bi.r.id;
  axi_in.r_data     <= axi4bi.r.data;
  axi_in.r_resp     <= axi4bi.r.resp;
  axi_in.r_last     <= axi4bi.r.last;
  axi_in.r_valid    <= axi4bi.r.valid;
  -- AXI output from the injector;
  axi4bo.aw.id      <= axi_out.aw_id;
  axi4bo.aw.addr    <= axi_out.aw_addr;
  axi4bo.aw.len     <= axi_out.aw_len;
  axi4bo.aw.size    <= axi_out.aw_size;
  axi4bo.aw.burst   <= axi_out.aw_burst;
  axi4bo.aw.lock    <= axi_out.aw_lock; 
  axi4bo.aw.cache   <= axi_out.aw_cache;
  axi4bo.aw.prot    <= axi_out.aw_prot; 
  axi4bo.aw.valid   <= axi_out.aw_valid;
  axi4bo.aw.qos     <= axi_out.aw_qos;  
  axi4bo.w.data     <= axi_out.w_data;
  axi4bo.w.strb     <= axi_out.w_strb;
  axi4bo.w.last     <= axi_out.w_last;
  axi4bo.w.valid    <= axi_out.w_valid;
  axi4bo.b.ready    <= axi_out.b_ready;
  axi4bo.ar.id      <= axi_out.ar_id;
  axi4bo.ar.addr    <= axi_out.ar_addr;
  axi4bo.ar.len     <= axi_out.ar_len;
  axi4bo.ar.size    <= axi_out.ar_size;
  axi4bo.ar.burst   <= axi_out.ar_burst;
  axi4bo.ar.lock    <= axi_out.ar_lock; 
  axi4bo.ar.cache   <= axi_out.ar_cache;
  axi4bo.ar.prot    <= axi_out.ar_prot; 
  axi4bo.ar.valid   <= axi_out.ar_valid;
  axi4bo.r.ready    <= axi_out.r_ready;  

  -----------------------------------------------------------------------------
  -- Component instantiation
  -----------------------------------------------------------------------------

  -- injector AXI interface
  AXIinterface : injector_axi
    generic map (
      tech             => tech,               -- Target technology
      -- APB configuration  
      pindex           => pindex,             -- APB configuartion slave index
      paddr            => paddr,              -- APB configuartion slave address
      pmask            => pmask,              -- APB configuartion slave mask
      pirq             => pirq,               -- APB configuartion slave irq
      -- Bus master configuration
      dbits            => dbits,              -- Data width of BM and FIFO    
      max_burst_length => max_burst_length    -- BM backend burst length in words. Total burst of 'Max_size'bytes, is split in to bursts of 'max_burst_length' bytes by the BMIF
      )
    port map (
      rstn     => rstn,
      clk      => clk,
      -- AXI interface signals
      axi4mi   => axi_in, -- AXI4 master input  (to the injector)
      axi4mo   => axi_out -- AXI4 master output (from the injector)
      );

end architecture rtl;
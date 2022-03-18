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
library safety;
use safety.injector_pkg.all;
use safety.axi4_pkg.axi4_mosi;
use safety.axi4_pkg.axi4_miso;
library techmap;
use techmap.gencomp.all;

entity injector_axi4_SELENE is
  generic (
    -- SafeTI configuration
    dbits         : integer range 32 to  128      := 32;      -- Data width of BM and FIFO at injector. [Only power of 2s allowed]
    MAX_SIZE_BURST: integer range 32 to 4096      := 4096;    -- Maximum size of a beat at a burst transaction.
    tech          : integer range 0 to NTECH      := inferred;-- Target technology
    -- APB configuration  
    pindex        : integer                       := 0;       -- APB configuartion slave index
    paddr         : integer                       := 0;       -- APB configuartion slave address
    pmask         : integer                       := 16#FFF#; -- APB configuartion slave mask
    pirq          : integer range 0 to NAHBIRQ-1  := 0;       -- APB configuartion slave irq
    -- AXI Master configuration
    axi_id        : integer                       := 0        -- AXI master index
  );
  port (
    rstn          : in  std_ulogic;       -- Reset
    clk           : in  std_ulogic;       -- Clock
    -- APB interface signals
    apbi          : in  apb_slv_in_type;  -- APB subordinate input to injector
    apbo          : out apb_slv_out_type; -- APB subordinate output from injector
    -- AXI4 interconnect SELENE bus
    axi4mi        : in  axi_somi_type;    -- AXI4 manager input to injector
    axi4mo        : out axi4_mosi_type    -- AXI4 manager output from injector
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

  -- Plug and Play Information (APB slave interface)
  constant REVISION   : integer := 0;
  constant interrupt  : std_logic_vector( 6 downto 0 ) := conv_std_logic_vector(pirq, 7);
  constant pconfig    : apb_config_type := (
    0 => (conv_std_logic_vector(VENDOR_BSC, 8) & conv_std_logic_vector(5, 12) & interrupt(6 downto 5) 
          & conv_std_logic_vector(REVISION, 5) & interrupt(4 downto 0)), 
    1 => (conv_std_logic_vector(paddr, 12) & "0000" & conv_std_logic_vector(pmask, 12) & "0001"));

  -----------------------------------------------------------------------------
  -- Records and types
  -----------------------------------------------------------------------------


  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------
  -- AXI maanager interface (injector)
  signal axi_mi     : axi4_miso;
  signal axi_mo     : axi4_mosi;
  -- APB slave interface (injector)
  signal apbi_inj   : apb_slave_in_type;
  signal apbo_inj   : apb_slave_out_type;

  -----------------------------------------------------------------------------
  -- Function/procedure declaration
  -----------------------------------------------------------------------------
  -- INJECTOR AXI4 top level interface
  component injector_axi is
    generic (
      dbits         : integer range 32 to  128            := 32;
      MAX_SIZE_BURST: integer range 32 to 4096            := 4096;
      tech          : integer range 0 to numTech          := typeTech;
      pindex        : integer                             := 0;
      paddr         : integer                             := 0;
      pmask         : integer                             := 16#FFF#;
      pirq          : integer range 0 to APB_IRQ_NMAX - 1 := 1;
      axi_id        : integer                             := 0;
      ASYNC_RST     : boolean                             := FALSE
    );
    port (
      rstn          : in  std_ulogic;
      clk           : in  std_ulogic;
      apbi          : in  apb_slave_in_type;
      apbo          : out apb_slave_out_type;
      axi4mi        : in  axi4_miso;
      axi4mo        : out axi4_mosi
    );
  end component injector_axi;
  
begin  -- rtl

  -----------------
  -- Assignments --
  -----------------
  -- AXI Master input to the injector
  axi_mi.aw_ready   <= axi4mi.aw.ready;
  axi_mi.w_ready    <= axi4mi.w.ready;
  axi_mi.b_id       <= axi4mi.b.id;
  axi_mi.b_resp     <= axi4mi.b.resp;
  axi_mi.b_valid    <= axi4mi.b.valid;
  axi_mi.ar_ready   <= axi4mi.ar.ready;
  axi_mi.r_id       <= axi4mi.r.id;
  axi_mi.r_data     <= axi4mi.r.data;
  axi_mi.r_resp     <= axi4mi.r.resp;
  axi_mi.r_last     <= axi4mi.r.last;
  axi_mi.r_valid    <= axi4mi.r.valid;
  -- AXI Master output from the injector
  axi4mo.aw.id      <= axi_mo.aw_id;
  axi4mo.aw.addr    <= axi_mo.aw_addr;
  axi4mo.aw.len     <= axi_mo.aw_len;
  axi4mo.aw.size    <= axi_mo.aw_size;
  axi4mo.aw.burst   <= axi_mo.aw_burst;
  axi4mo.aw.lock    <= axi_mo.aw_lock; 
  axi4mo.aw.cache   <= axi_mo.aw_cache;
  axi4mo.aw.prot    <= axi_mo.aw_prot; 
  axi4mo.aw.valid   <= axi_mo.aw_valid;
  axi4mo.aw.qos     <= axi_mo.aw_qos;  
  axi4mo.w.data     <= axi_mo.w_data;
  axi4mo.w.strb     <= axi_mo.w_strb;
  axi4mo.w.last     <= axi_mo.w_last;
  axi4mo.w.valid    <= axi_mo.w_valid;
  axi4mo.b.ready    <= axi_mo.b_ready;
  axi4mo.ar.id      <= axi_mo.ar_id;
  axi4mo.ar.addr    <= axi_mo.ar_addr;
  axi4mo.ar.len     <= axi_mo.ar_len;
  axi4mo.ar.size    <= axi_mo.ar_size;
  axi4mo.ar.burst   <= axi_mo.ar_burst;
  axi4mo.ar.lock    <= axi_mo.ar_lock;
  axi4mo.ar.cache   <= axi_mo.ar_cache;
  axi4mo.ar.prot    <= axi_mo.ar_prot;
  axi4mo.ar.valid   <= axi_mo.ar_valid;
  axi4mo.r.ready    <= axi_mo.r_ready;

  -- APB Slave input to the injector
  apbi_inj.sel      <= apbi.psel;
  apbi_inj.en       <= apbi.penable;
  apbi_inj.addr     <= apbi.paddr;
  apbi_inj.write    <= apbi.pwrite;
  apbi_inj.wdata    <= apbi.pwdata;
  apbi_inj.irq      <= apbi.pirq;
  apbi_inj.ten      <= apbi.testen;
  apbi_inj.trst     <= apbi.testrst;
  apbi_inj.scnen    <= apbi.scanen;
  apbi_inj.touten   <= apbi.testoen;
  apbi_inj.tinen    <= apbi.testin;
  -- APB Slave output from the injector
  apbo.prdata       <= apbo_inj.rdata;
  apbo.pirq         <= apbo_inj.irq;
  apbo.pindex       <= apbo_inj.index;
  apbo.pconfig      <= pconfig;


  -----------------------------------------------------------------------------
  -- Component instantiation
  -----------------------------------------------------------------------------

  -- injector AXI interface
  AXI_injector : injector_axi
    generic map (
      -- SafeTI configuration
      dbits         => dbits,         -- Data width of BM and FIFO at injector. [Only power of 2s allowed]
      MAX_SIZE_BURST=> MAX_SIZE_BURST,-- Maximum size of a beat at a burst transaction.
      tech          => tech,          -- Target technology
      -- APB configuration  
      pindex        => pindex,        -- APB configuartion slave index
      paddr         => paddr,         -- APB configuartion slave address
      pmask         => pmask,         -- APB configuartion slave mask
      pirq          => pirq,          -- APB configuartion slave irq
      -- AXI4 Master configuration (other parameters must be updated at injector_pkg.vhd)
      axi_id        => axi_id,        -- AXI fixed burst ID
      -- Asynchronous reset configuration
      ASYNC_RST     => ASYNC_RST      -- Allow asynchronous reset flag
      )
    port map (
      rstn          => rstn,          -- Reset
      clk           => clk,           -- Clock
      -- APB interface signals
      apbi          => apbi_inj,      -- APB slave input
      apbo          => apbo_inj,      -- APB slave output
      -- AXI interface signals
      axi4mi        => axi_mi,        -- AXI4 master input 
      axi4mo        => axi_mo         -- AXI4 master output
      );

end architecture rtl;
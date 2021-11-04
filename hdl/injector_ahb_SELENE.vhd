-----------------------------------------------------------------------------   
-- Entity:      injector_ahb_SELENE
-- File:        injector_ahb_SELENE.vhd
-- Author:      Oriol Sala
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

-----------------------------------------------------------------------------
-- Top level entity for injector at SELENE platform.
-- This is a wrapper which integrates injector core to the
-- AHB master - generic bus master bridge
-----------------------------------------------------------------------------

entity injector_ahb_SELENE is
  generic (
    tech              : integer range 0 to NTECH      := inferred;  -- Target technology
    -- APB configuration  
    pindex            : integer                       := 0;         -- APB configuartion slave index
    paddr             : integer                       := 0;         -- APB configuartion slave address
    pmask             : integer                       := 16#FF8#;   -- APB configuartion slave mask
    pirq              : integer range 0 to NAHBIRQ-1  := 0;         -- APB configuartion slave irq
    -- Bus master configuration
    dbits             : integer range 32 to 128       := 32;        -- Data width of BM and FIFO    
    hindex            : integer                       := 0;         -- AHB master index 0
    max_burst_length  : integer range 2 to 256        := 128        -- BM backend burst length in words. Total burst of 'Max_size'bytes, is split in to bursts of 'max_burst_length' bytes by the BMIF
    );
  port (
    rstn              : in  std_ulogic;                   -- Reset
    clk               : in  std_ulogic;                   -- Clock
    -- APB interface signals
    apbi              : in  apb_slv_in_type;              -- APB slave input to injector
    apbo              : out apb_slv_out_type;             -- APB slave output from injector
    -- AHB interface signals
    ahbmi             : in  ahb_mst_in_type;              -- AHB master 0 input from bus
    ahbmo             : out ahb_mst_out_type              -- AHB master 0 output to bus
    );
end entity injector_ahb_SELENE;

------------------------------------------------------------------------------
-- Architecture of grdmac2
------------------------------------------------------------------------------

architecture rtl of injector_ahb_SELENE is
  -----------------------------------------------------------------------------
  -- Constant declaration
  -----------------------------------------------------------------------------
  attribute sync_set_reset         : string;
  attribute sync_set_reset of rstn : signal is "true";

  -- Reset configuration

  constant ASYNC_RST : boolean := GRLIB_CONFIG_ARRAY(grlib_async_reset_enable) = 1;

  -- Plug and Play Information (AHB master interface)

  constant REVISION   : integer := 0;
  constant hconfig    : ahb_config_type := ((
    conv_std_logic_vector(VENDOR_BSC, 8) & conv_std_logic_vector(5, 12) &
    "00" & conv_std_logic_vector(REVISION, 5) & "00000"), others => (others => '0'));

  -- Plug and Play Information (APB slave interface)

  constant interrupt  : std_logic_vector( 6 downto 0 ) := conv_std_logic_vector(pirq, 7);
  constant pconfig    : apb_config_type := (
    0 => (conv_std_logic_vector(VENDOR_BSC, 8) & conv_std_logic_vector(5, 12) & interrupt(6 downto 5) 
          & conv_std_logic_vector(REVISION, 5) & interrupt(4 downto 0)), 
    1 => (conv_std_logic_vector(paddr, 12) & "0000" & conv_std_logic_vector(pmask, 12) & "0001"));

  -- Bus master interface burst chop mask
  constant burst_chop_mask : integer := (max_burst_length*(log2(AHBDW)-1));

  -----------------------------------------------------------------------------
  -- Records and types
  -----------------------------------------------------------------------------

  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------
  signal ahb_bmsti  : ahb_bmst_in_type;
  signal ahb_bmsto  : ahb_bmst_out_type;
  signal apbi_inj   : apb_slave_in_type;
  signal apbo_inj   : apb_slave_out_type;
  signal bm_in      : bm_in_type;
  signal bm_out     : bm_out_type;

  -----------------------------------------------------------------------------
  -- Function/procedure declaration
  -----------------------------------------------------------------------------
  
begin  -- rtl

  -----------------
  -- Assignments --
  -----------------
  ahb_bmsti.hgrant  <= ahbmi.hgrant(hindex);
  ahb_bmsti.hready  <= ahbmi.hready;
  ahb_bmsti.hresp   <= ahbmi.hresp;

  ahbmo.hbusreq     <= ahb_bmsto.hbusreq;
  ahbmo.hlock       <= ahb_bmsto.hlock;
  ahbmo.htrans      <= ahb_bmsto.htrans;
  ahbmo.haddr       <= ahb_bmsto.haddr;
  ahbmo.hwrite      <= ahb_bmsto.hwrite;
  ahbmo.hsize       <= ahb_bmsto.hsize;
  ahbmo.hburst      <= ahb_bmsto.hburst;
  ahbmo.hprot       <= ahb_bmsto.hprot;
  ahbmo.hirq        <= (others => '0');
  ahbmo.hconfig     <= hconfig;
  ahbmo.hindex      <= hindex;

  apbo.prdata       <= apbo_inj.rdata;
  apbo.pirq         <= apbo_inj.irq;
  apbo.pindex       <= apbo_inj.index;
  apbo.pconfig      <= pconfig;
  
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


  -----------------------------------------------------------------------------
  -- Component instantiation
  -----------------------------------------------------------------------------

  -- injector_ahb
  ahb : injector_ahb
    generic map(
      tech              => tech,              -- Target technology
      -- APB configuration  
      pindex            => pindex,            -- APB configuartion slave index
      paddr             => paddr,             -- APB configuartion slave address
      pmask             => pmask,             -- APB configuartion slave mask
      pirq              => pirq,              -- APB configuartion slave irq
      -- Bus master configuration
      dbits             => dbits,             -- Data width of BM and FIFO    
      hindex            => hindex,            -- AHB master index 0
      max_burst_length  => max_burst_length,  -- BM backend burst length in words. Total burst of 'Max_size'bytes, is split in to bursts of 'max_burst_length' bytes by the BMIF
      -- Injector configuration
      ASYNC_RST         => ASYNC_RST          -- Allow asynchronous reset flag
      )
    port map(
      rstn              => rstn,              -- Reset
      clk               => clk,               -- Clock
      -- APB interface signals
      apbi              => apbi_inj,          -- APB slave input
      apbo              => apbo_inj,          -- APB slave output
      -- AHB interface signals
      bm_in             => bm_in,             -- For AHB master 0 input
      bm_out            => bm_out             -- For AHB master 0 output
    );


  -- BM0
  bm0 : generic_bm_ahb
    generic map(
      async_reset      => ASYNC_RST,
      bm_dw            => dbits,
      be_dw            => AHBDW,
      be_rd_pipe       => 0,
      max_size         => 1024,
      max_burst_length => max_burst_length,
      burst_chop_mask  => burst_chop_mask,
      bm_info_print    => 1,
      hindex           => hindex
      )        
    port map (
      clk              => clk,
      rstn             => rstn,
      ahbmi            => ahb_bmsti,
      ahbmo            => ahb_bmsto,
      hrdata           => ahbmi.hrdata,
      hwdata           => ahbmo.hwdata,
      bmrd_addr        => bm_in.rd_addr,
      bmrd_size        => bm_in.rd_size,
      bmrd_req         => bm_in.rd_req,
      bmrd_req_granted => bm_out.rd_req_grant,
      bmrd_data        => bm_out.rd_data(127 downto 128-dbits),
      bmrd_valid       => bm_out.rd_valid,
      bmrd_done        => bm_out.rd_done,
      bmrd_error       => bm_out.rd_err,
      bmwr_addr        => bm_in.wr_addr,
      bmwr_size        => bm_in.wr_size,
      bmwr_req         => bm_in.wr_req,
      bmwr_req_granted => bm_out.wr_req_grant,
      bmwr_data        => bm_in.wr_data(127 downto 128-dbits),
      bmwr_full        => bm_out.wr_full,
      bmwr_done        => bm_out.wr_done,
      bmwr_error       => bm_out.wr_err,
      excl_en          => '0',
      excl_nowrite     => '0',
      excl_done        => open,
      excl_err         => open
      );
  
  
end architecture rtl;




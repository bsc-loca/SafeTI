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
library safety;
use safety.injector_pkg.all;
library techmap;
use techmap.gencomp.all;

-----------------------------------------------------------------------------
-- Top level entity for injector at SELENE platform.
-- This is a wrapper which integrates injector core to the
-- AHB master - generic bus master bridge
-----------------------------------------------------------------------------

entity injector_ahb_SELENE is
  generic (
    -- SafeTI configuration
    INJ_MEM_LENGTH    : integer range 2 to   10       :=    4;      -- Set the maximum number of programmable descriptor words to 2^INJ_MEM_LENGTH
    MAX_SIZE_BURST    : integer range 8 to 1024       := 1024;      -- Maximum number of bytes allowed at a burst transaction.
    tech              : integer range 0 to NTECH      := inferred;  -- Target technology
    -- APB configuration
    pindex            : integer                       := 0;         -- APB configuration slave index
    paddr             : integer                       := 0;         -- APB configuration slave address
    pmask             : integer                       := 16#FFF#;   -- APB configuration slave mask
    pirq              : integer range  0 to NAHBIRQ-1 := 0;         -- APB configuration slave irq
    -- AHB configuration
    AHB_DATAW         : integer range 8  to 1024      := 32;        -- Data bus width of AHB. [Only power of 2s allowed]
    hindex            : integer                       :=  0         -- AHB master index 0
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
    conv_std_logic_vector(VENDOR_CONTRIB, 8) & conv_std_logic_vector(16#000#, 12) &
    "00" & conv_std_logic_vector(REVISION, 5) & "00000"), others => (others => '0'));

  -- Plug and Play Information (APB slave interface)
  constant interrupt  : std_logic_vector( 6 downto 0 ) := conv_std_logic_vector(pirq, 7);
  constant pconfig    : apb_config_type := (
    0 => (conv_std_logic_vector(VENDOR_CONTRIB, 8) & conv_std_logic_vector(16#000#, 12) & interrupt(6 downto 5)
          & conv_std_logic_vector(REVISION, 5) & interrupt(4 downto 0)),
    1 => (conv_std_logic_vector(paddr, 12) & "0000" & conv_std_logic_vector(pmask, 12) & "0001"));

  -- Bus master interface burst chop mask
  --constant burst_chop_mask : integer := (max_burst_length*(log2(AHBDW)-1));
  constant max_burst_length : integer := MAX_SIZE_BURST/(AHBDW/8);

  constant CORE_DATA_WIDTH  : integer := AHB_DATAW;


  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------

  -- BM0 AHB interface signals
  signal ahb_bmsti  : ahb_bmst_in_type;
  signal ahb_bmsto  : ahb_bmst_out_type;
  -- SafeTI APB signals
  signal apbi_inj   : apb_slave_in;
  signal apbo_inj   : apb_slave_out;
  -- SafeTI IB/BM signals
  signal ib_mosi    : ib_mosi;
  signal ib_miso    : ib_miso;


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
  apbo.pirq(pirq)   <= apbo_inj.irq;
  apbo.pindex       <= pindex;
  apbo.pconfig      <= pconfig;

  apbi_inj.sel      <= apbi.psel(pindex);
  apbi_inj.en       <= apbi.penable;
  apbi_inj.addr     <= apbi.paddr;
  apbi_inj.wr_en    <= apbi.pwrite;
  apbi_inj.wdata    <= apbi.pwdata;
  apbi_inj.irq      <= apbi.pirq(pirq);
  --apbi_inj.ten      <= apbi.testen;
  --apbi_inj.trst     <= apbi.testrst;
  --apbi_inj.scnen    <= apbi.scanen;
  --apbi_inj.touten   <= apbi.testoen;
  --apbi_inj.tinen    <= apbi.testin;


  -----------------------------------------------------------------------------
  -- Component instantiation
  -----------------------------------------------------------------------------

  -- BM0 AHB interface
  bm0 : generic_bm_ahb
    generic map(
      async_reset      => ASYNC_RST,
      bm_dw            => CORE_DATA_WIDTH,
      be_dw            => AHBDW,
      be_rd_pipe       => 0,
      max_size         => MAX_SIZE_BURST,
      max_burst_length => max_burst_length,
      burst_chop_mask  => MAX_SIZE_BURST,
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
      bmrd_addr        => ib_mosi.rd_addr,
      bmrd_size        => ib_mosi.rd_size(9 downto 0),
      bmrd_req         => ib_mosi.rd_req,
      bmrd_req_granted => ib_miso.rd_req_grant,
      bmrd_data        => ib_miso.rd_data(CORE_DATA_WIDTH - 1 downto 0),
      bmrd_valid       => ib_miso.rd_valid,
      bmrd_done        => ib_miso.rd_done,
      bmrd_error       => ib_miso.rd_err,
      bmwr_addr        => ib_mosi.wr_addr,
      bmwr_size        => ib_mosi.wr_size(9 downto 0),
      bmwr_req         => ib_mosi.wr_req,
      bmwr_req_granted => ib_miso.wr_req_grant,
      bmwr_data        => ib_mosi.wr_data(CORE_DATA_WIDTH - 1 downto 0),
      bmwr_full        => ib_miso.wr_full,
      bmwr_done        => ib_miso.wr_done,
      bmwr_error       => ib_miso.wr_err,
      excl_en          => '0',
      excl_nowrite     => '0',
      excl_done        => open,
      excl_err         => open
    );

  -- Injector core
  core : injector_core
    generic map (
      PC_LEN          => INJ_MEM_LENGTH,
      CORE_DATA_WIDTH => CORE_DATA_WIDTH,
      MAX_SIZE_BURST  => MAX_SIZE_BURST,
      ASYNC_RST       => ASYNC_RST
    )
    port map (
      rstn            => rstn,
      clk             => clk,
      apbi            => apbi_inj,
      apbo            => apbo_inj,
      ib_out          => ib_mosi,
      ib_in           => ib_miso,
      network_profile => open
    );


end architecture rtl;

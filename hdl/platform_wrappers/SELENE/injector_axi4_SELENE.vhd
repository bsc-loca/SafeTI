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
use safety.axi4_pkg.all;
library techmap;
use techmap.gencomp.all;

entity injector_axi4_SELENE is
  generic (
    -- SafeTI configuration
    dbits         : integer range 32 to  128            := 32;      -- Data width of BM and FIFO at injector. [Only power of 2s are allowed]
    MAX_SIZE_BURST: integer range 32 to 4096            := 4096;    -- Maximum size of a beat at a burst transaction.
    tech          : integer range  0 to NTECH           := inferred;-- Target technology
    -- APB configuration  
    pindex        : integer                             := 0;       -- APB configuartion slave index
    paddr         : integer                             := 0;       -- APB configuartion slave address
    pmask         : integer                             := 16#FFF#; -- APB configuartion slave mask
    pirq          : integer range  0 to NAHBIRQ-1       := 0;       -- APB configuartion slave irq
    -- AXI Manager configuration
    ID_R_WIDTH    : integer range  0 to   32            := 4;       -- AXI ID's bus width.
    ID_W_WIDTH    : integer range  0 to   32            := 4;       -- AXI ID's bus width.
    ADDR_WIDTH    : integer range 12 to   64            := 32;      -- AXI address bus width. (Tested only for 32 bits)
    DATA_WIDTH    : integer range  8 to 1024            := 128;     -- AXI data bus width. [Only power of 2s are allowed]
    axi_id        : integer range  0 to 32**2-1         := 0;       -- AXI manager burst index [Must be < ID_X_WIDTH**2-1]
    rd_n_fifo_regs: integer range  2 to  256            := 4;       -- Number of FIFO registers to use at AXI read transactions.  [Only power of 2s are allowed]
    wr_n_fifo_regs: integer range  2 to  256            := 4        -- Number of FIFO registers to use at AXI write transactions. [Only power of 2s are allowed]
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
    0 => (conv_std_logic_vector(VENDOR_CONTRIB, 8) & conv_std_logic_vector(16#000#, 12) & interrupt(6 downto 5) 
          & conv_std_logic_vector(REVISION, 5) & interrupt(4 downto 0)), 
    1 => (conv_std_logic_vector(paddr, 12) & "0000" & conv_std_logic_vector(pmask, 12) & "0001"));

  -----------------------------------------------------------------------------
  -- Records and types
  -----------------------------------------------------------------------------


  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------
  -- AXI Manager interface signals (injector)
  signal axi_mi     : axi4_miso;
  signal axi_mo     : axi4_mosi;
  -- APB Subordinate interface signals (injector)
  signal apbi_inj   : apb_slave_in_type;
  signal apbo_inj   : apb_slave_out_type;
  -- BM interface signals between SafeTI (Manager) and AXI4 Manager interface (Subordinate)
  signal bm_out_injector  : safety.injector_pkg.bm_mosi;  -- Output from injector
  signal bm_in_injector   : safety.injector_pkg.bm_miso;  -- Input to injector
  signal bm_in_manager    : safety.axi4_pkg.bm_mosi;      -- Input to AXI4 Manager interface
  signal bm_out_manager   : safety.axi4_pkg.bm_miso;      -- Output from AXI4 Manager interface
  

  -----------------------------------------------------------------------------
  -- Function/procedure declaration
  -----------------------------------------------------------------------------

  
begin  -- rtl

  -----------------
  -- Assignments --
  -----------------
  -- AXI4 Manager interface input (to the injector)
  axi_mi.aw_ready   <= axi4mi.aw.ready;
  axi_mi.w_ready    <= axi4mi.w.ready;
  axi_mi.b_id       <= (axi_mi.b_id'high   downto ID_W_WIDTH => '0') & axi4mi.b.id;
  axi_mi.b_resp     <= axi4mi.b.resp;
  axi_mi.b_valid    <= axi4mi.b.valid;
  axi_mi.ar_ready   <= axi4mi.ar.ready;
  axi_mi.r_id       <= (axi_mi.r_id'high   downto ID_R_WIDTH => '0') & axi4mi.r.id;
  axi_mi.r_data     <= (axi_mi.r_data'high downto DATA_WIDTH => '0') & axi4mi.r.data;
  axi_mi.r_resp     <= axi4mi.r.resp;
  axi_mi.r_last     <= axi4mi.r.last;
  axi_mi.r_valid    <= axi4mi.r.valid;
  -- AXI Manager interface output (from the injector)
  axi4mo.aw.id      <= axi_mo.aw_id(  axi4mo.aw.id'range  );
  axi4mo.aw.addr    <= axi_mo.aw_addr(axi4mo.aw.addr'range);
  axi4mo.aw.len     <= axi_mo.aw_len;
  axi4mo.aw.size    <= axi_mo.aw_size;
  axi4mo.aw.burst   <= axi_mo.aw_burst;
  axi4mo.aw.lock    <= axi_mo.aw_lock; 
  axi4mo.aw.cache   <= axi_mo.aw_cache;
  axi4mo.aw.prot    <= axi_mo.aw_prot; 
  axi4mo.aw.valid   <= axi_mo.aw_valid;
  axi4mo.aw.qos     <= axi_mo.aw_qos;  
  axi4mo.w.data     <= axi_mo.w_data( axi4mo.w.data'range );
  axi4mo.w.strb     <= axi_mo.w_strb( axi4mo.w.strb'range );
  axi4mo.w.last     <= axi_mo.w_last;
  axi4mo.w.valid    <= axi_mo.w_valid;
  axi4mo.b.ready    <= axi_mo.b_ready;
  axi4mo.ar.id      <= axi_mo.ar_id(  axi4mo.ar.id'range  );
  axi4mo.ar.addr    <= axi_mo.ar_addr(axi4mo.ar.addr'range);
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

  -- BM SafeTI input (MISO)
  bm_in_injector.rd_data      <= bm_out_manager.rd_data(dbits-1 downto 0) & (bm_in_injector.rd_data'high downto dbits => '0');
  bm_in_injector.rd_req_grant <= bm_out_manager.rd_req_grant;
  bm_in_injector.rd_valid     <= bm_out_manager.rd_valid;
  bm_in_injector.rd_done      <= bm_out_manager.rd_done;
  bm_in_injector.rd_err       <= bm_out_manager.rd_err;
  bm_in_injector.wr_req_grant <= bm_out_manager.wr_req_grant;
  bm_in_injector.wr_full      <= bm_out_manager.wr_full;
  bm_in_injector.wr_done      <= bm_out_manager.wr_done;
  bm_in_injector.wr_err       <= bm_out_manager.wr_err;
  -- BM SafeTI output (MOSI)
  bm_in_manager.rd_addr       <= (63 downto bm_out_injector.rd_addr'length => '0') & bm_out_injector.rd_addr;
  bm_in_manager.rd_size       <= bm_out_injector.rd_size;
  bm_in_manager.rd_req        <= bm_out_injector.rd_req;
  bm_in_manager.wr_addr       <= (63 downto bm_out_injector.wr_addr'length => '0') & bm_out_injector.wr_addr;
  bm_in_manager.wr_size       <= bm_out_injector.wr_size;
  bm_in_manager.wr_req        <= bm_out_injector.wr_req;
  bm_in_manager.wr_data       <= (1023 downto dbits => '0') & bm_out_injector.wr_data(bm_in_injector.rd_data'high downto bm_in_injector.rd_data'length-dbits);
  
  bm_in_manager.rd_fixed_addr <= '0';
  bm_in_manager.rd_axi_cache  <= "0011";
  bm_in_manager.rd_axi_prot   <= "001";
  bm_in_manager.wr_fixed_addr <= '0';
  bm_in_manager.wr_axi_cache  <= "0011";
  bm_in_manager.wr_axi_prot   <= "001";



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
      apbi          => apbi_inj,
      apbo          => apbo_inj,
      bm0_mosi      => bm_out_injector,
      bm0_miso      => bm_in_injector
    );

  -- AXI4 Manager interface
  AXI4_M0 : axi4_manager
    generic map (
      ID_R_WIDTH    => ID_R_WIDTH,
      ID_W_WIDTH    => ID_W_WIDTH,
      ADDR_WIDTH    => ADDR_WIDTH,
      DATA_WIDTH    => DATA_WIDTH,
      axi_id        => axi_id,
      dbits         => dbits,
      rd_n_fifo_regs=> rd_n_fifo_regs,
      wr_n_fifo_regs=> wr_n_fifo_regs,
      ASYNC_RST     => ASYNC_RST
    )
    port map (
      rstn            => rstn,
      clk             => clk,
      axi4mi          => axi_mi,
      axi4mo          => axi_mo,
      bm_in           => bm_in_manager,
      bm_out          => bm_out_manager
    );
    

end architecture rtl;
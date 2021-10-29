-----------------------------------------------------------------------------   
-- Entity:      injector_axi
-- File:        injector_axi.vhd
-- Author:      Francis Fuentes, Oriol Sala
-- Description: injector top level entity.
------------------------------------------------------------------------------ 
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library bsc;
use bsc.injector_pkg.all;
use bsc.injector_settings.all;

-----------------------------------------------------------------------------
-- Top level entity for injector.
-- This is a wrapper which integrates injector core to the
-- AXI4 master - generic bus master bridge
-----------------------------------------------------------------------------

entity injector_axi is
  generic (
    tech             : integer range 0 to 67        := 0;         -- Target technology
    -- APB configuration  
    pindex           : integer                      := 0;         -- APB configuartion slave index
    paddr            : integer                      := 0;         -- APB configuartion slave address
    pmask            : integer                      := 16#FF8#;   -- APB configuartion slave mask
    pirq             : integer range 0 to 320       := 0;         -- APB configuartion slave irq
    -- Bus master configuration
    dbits            : integer range 32 to 128      := 32;        -- Data width of BM and FIFO    
    max_burst_length : integer range 2 to 256       := 128        -- BM backend burst length in words. Total burst of 'Max_size'bytes, is split in to bursts of 'max_burst_length' bytes by the BMIF
    );
  port (
    rstn     : in  std_ulogic;                    -- Reset
    clk      : in  std_ulogic;                    -- Clock
    -- APB interface signals
    apbi     : in  apb_slv_in_type;               -- APB slave input
    apbo     : out apb_slv_out_type;              -- APB slave output
    -- AXI interface signals
    axi4mi   : in  axi4_in_type;                  -- AXI4 master input 
    axi4mo   : out axi4_out_type                  -- AXI4 master output
    );
end entity injector_axi;

------------------------------------------------------------------------------
-- Architecture of grdmac2
------------------------------------------------------------------------------

architecture rtl of injector_axi is
  -----------------------------------------------------------------------------
  -- Constant declaration
  -----------------------------------------------------------------------------
  -- Bus master interface burst chop mask
  constant burst_chop_mask : integer := (max_burst_length*(log_2(AXI4_DATA_WIDTH)-1));

  -----------------------------------------------------------------------------
  -- Records and types
  -----------------------------------------------------------------------------

  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------
  signal bm_in      : bm_in_type;
  signal bm_out     : bm_out_type;
  signal axi_in     : axi4_in_type;
  signal axi_out    : axi4_out_type;

  signal 

  -----------------------------------------------------------------------------
  -- Function/procedure declaration
  -----------------------------------------------------------------------------
  
begin  -- rtl

  -----------------
  -- Assignments --
  -----------------
  -- Write address channel
  axi4mo.aw_id      <= ( others => '0');
  axi4mo.aw_addr    <= bm_in.wr_addr;
  axi4mo.aw_len     <= ( others => '0'); -- Burst length ('0' = 1 transaction)
  axi4mo.aw_burst   <= ( others => '0'); -- Burst type
  axi4mo.aw_lock    <= ( others => '0'); -- AXI4 does not support lock trans.
  axi4mo.aw_cache   <= AXI4_CACHE_AW;
  axi4mo.aw_size    <= ; -- Number of bursts
  axi4mo.aw_prot    <= ;
  axi4mo.aw_qos     <= ;
  axi4mo.aw_region  <= ;
  axi4mo.aw_valid   <= ;
  -- Write data channel
  axi4mo.w_data     <= ;
  axi4mo.w_strb     <= ;
  axi4mo.w_last     <= ;
  axi4mo.w_valid    <= ;
  -- Write response channel
  axi4mo.b_ready    <= ;
  -- Read address channel
  axi4mo.ar_id      <= ;
  axi4mo.ar_addr    <= ;
  axi4mo.ar_len     <= ;
  axi4mo.ar_burst   <= ;
  axi4mo.ar_lock    <= ;
  axi4mo.ar_cache   <= ;
  axi4mo.ar_size    <= ;
  axi4mo.ar_prot    <= ;
  axi4mo.ar_qos     <= ;
  axi4mo.ar_region  <= ;
  axi4mo.ar_valid   <= ;
  -- Read data channel
  axi4mo.r_ready    <= ;


  --ahb_bmsti.hgrant <= ahbmi.hgrant(hindex);
  --ahb_bmsti.hready <= ahbmi.hready;
  --ahb_bmsti.hresp  <= ahbmi.hresp;

  --ahbmo.hbusreq    <= ahb_bmsto.hbusreq;
  --ahbmo.hlock      <= ahb_bmsto.hlock;
  --ahbmo.htrans     <= ahb_bmsto.htrans;
  --ahbmo.haddr      <= ahb_bmsto.haddr;
  --ahbmo.hwrite     <= ahb_bmsto.hwrite;
  --ahbmo.hsize      <= ahb_bmsto.hsize;
  --ahbmo.hburst     <= ahb_bmsto.hburst;
  --ahbmo.hprot      <= ahb_bmsto.hprot;
  --ahbmo.hirq       <= (others => '0');
  --ahbmo.hconfig    <= hconfig;
  --ahbmo.hindex     <= hindex;

  -----------------------------------------------------------------------------
  -- Combinational process
  -----------------------------------------------------------------------------

  process (clk, rstn) begin
    if (rstn = '0')

    elsif rising_edge(clk) then

    end if;
  end process; 

  -----------------------------------------------------------------------------
  -- Component instantiation
  -----------------------------------------------------------------------------

  -- injector core
  core : injector
    generic map (
      tech     => tech,
      pindex   => pindex,
      paddr    => paddr,
      pmask    => pmask,
      pirq     => pirq,
      dbits    => dbits
      )
    port map (
      rstn    => rstn,
      clk     => clk,
      apbi    => apbi,
      apbo    => apbo,
      bm0_in  => bm_in,
      bm0_out => bm_out
      );




  -- BM0
  --bm0 : generic_bm_axi
  --  generic map(
  --    async_reset      => ASYNC_RST,
  --    bm_dw            => dbits,
  --    be_dw            => AXIDW,
  --    be_rd_pipe       => 0,
  --    max_size         => 1024,
  --    max_burst_length => max_burst_length,
  --    burst_chop_mask  => burst_chop_mask,
  --    bm_info_print    => 1
  --    )        
  --  port map (
  --    clk              => clk,
  --    rstn             => rstn,
  --    --write address channel
  --    axi_aw_id        => ,
  --    axi_aw_addr      => ,
  --    axi_aw_len       => ,
  --    axi_aw_size      => ,
  --    axi_aw_burst     => ,
  --    axi_aw_lock      => ,
  --    axi_aw_cache     => ,
  --    axi_aw_prot      => ,
  --    axi_aw_valid     => ,
  --    axi_aw_qos       => ,
  --    axi_aw_ready     => ,
  --    --write data channel
  --    axi_w_data       => ,
  --    axi_w_strb       => ,
  --    axi_w_last       => ,
  --    axi_w_valid      => ,
  --    axi_w_ready      => ,
  --    --write response channel
  --    axi_b_ready      => ,
  --    axi_b_id         => ,
  --    axi_b_resp       => ,
  --    axi_b_valid      => ,
  --    --read address channel
  --    axi_ar_id        => ,
  --    axi_ar_addr      => ,
  --    axi_ar_len       => ,
  --    axi_ar_size      => ,
  --    axi_ar_burst     => ,
  --    axi_ar_lock      => ,
  --    axi_ar_cache     => ,
  --    axi_ar_prot      => ,
  --    axi_ar_valid     => ,
  --    axi_ar_qos       => ,
  --    axi_ar_ready     => ,
  --    --read data channel
  --    axi_r_ready      => ,
  --    axi_r_id         => ,
  --    axi_r_data       => ,
  --    axi_r_resp       => ,
  --    axi_r_last       => ,
  --    axi_r_valid      => ,
  --    axi_aw_addr      => ,
  --    --Bus master domain
  --    --Read Channel
  --    bmrd_addr        => bm_in.rd_addr,
  --    bmrd_size        => bm_in.rd_size,
  --    bmrd_req         => bm_in.rd_req,
  --    bmrd_req_granted => bm_out.rd_req_grant,
  --    bmrd_data        => bm_out.rd_data(127 downto 128-dbits),
  --    bmrd_valid       => bm_out.rd_valid,
  --    bmrd_done        => bm_out.rd_done,
  --    bmrd_error       => bm_out.rd_err,
  --    --Write Channel
  --    bmwr_addr        => bm_in.wr_addr,
  --    bmwr_size        => bm_in.wr_size,
  --    bmwr_req         => bm_in.wr_req,
  --    bmwr_req_granted => bm_out.wr_req_grant,
  --    bmwr_data        => bm_in.wr_data(127 downto 128-dbits),
  --    bmwr_full        => bm_out.wr_full,
  --    bmwr_done        => bm_out.wr_done,
  --    bmwr_error       => bm_out.wr_err
  --    );

end architecture rtl;



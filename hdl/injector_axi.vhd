-----------------------------------------------------------------------------   
-- Entity:      injector_axi
-- File:        injector_axi.vhd
-- Author:      Francis Fuentes Diaz (BSC-CNS)
-- Description: injector top level entity.
------------------------------------------------------------------------------ 
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library safety;
use safety.injector_pkg.all;
use safety.axi4_pkg.all;

-----------------------------------------------------------------------------
-- Top level entity for injector.
-- This is a wrapper which integrates injector core to the
-- AXI4 master - generic bus master bridge
-----------------------------------------------------------------------------

entity injector_axi is
  generic (
    -- SafeTI configuration
    dbits         : integer range 32 to  128            := 32;      -- Data width of BM and FIFO at injector. [Only power of 2s allowed]
    MAX_SIZE_BURST: integer range 32 to 4096            := 4096;    -- Maximum size of a beat at a burst transaction.
    tech          : integer range 0 to numTech          := typeTech;-- Target technology
    -- APB configuration  
    pindex        : integer                             := 0;       -- APB configuartion slave index
    paddr         : integer                             := 0;       -- APB configuartion slave address
    pmask         : integer                             := 16#FFF#; -- APB configuartion slave mask
    pirq          : integer range 0 to APB_IRQ_NMAX - 1 := 0;       -- APB configuartion slave irq
    -- AXI Master configuration
    axi_id        : integer                             := 0;       -- AXI fixed burst ID
    -- Asynchronous reset configuration
    ASYNC_RST     : boolean                             := FALSE    -- Allow asynchronous reset flag
  );
  port (
    rstn          : in  std_ulogic;         -- Reset
    clk           : in  std_ulogic;         -- Clock
    -- APB interface signals
    apbi          : in  apb_slave_in_type;  -- APB slave input
    apbo          : out apb_slave_out_type; -- APB slave output
    -- AXI interface signals
    axi4mi        : in  axi4_miso;          -- AXI4 master input 
    axi4mo        : out axi4_mosi           -- AXI4 master output
  );
end entity injector_axi;

architecture rtl of injector_axi is

  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------

  -- I/O Injector and AXI interface
  signal bm_out_injector  : safety.injector_pkg.bm_mosi;  -- Output from injector
  signal bm_in_injector   : safety.injector_pkg.bm_miso;  -- Input to injector
  signal bm_in_manager    : safety.axi4_pkg.bm_mosi;      -- Input to AXI4 Manager interface
  signal bm_out_manager   : safety.axi4_pkg.bm_miso;      -- Output from AXI4 Manager interface
  signal bm_bypass        : std_logic;                    -- Bypass flag for read transactions

begin

  -----------------
  -- Assignments --
  -----------------

  bm_in_manager.rd_addr         <= bm_out_injector.rd_addr;
  bm_in_manager.rd_size         <= bm_out_injector.rd_size;
  bm_in_manager.rd_req          <= bm_out_injector.rd_req;
  bm_bypass                     <= not(bm_out_injector.rd_descr);
  bm_in_manager.wr_addr         <= bm_out_injector.wr_addr;
  bm_in_manager.wr_size         <= bm_out_injector.wr_size;
  bm_in_manager.wr_req          <= bm_out_injector.wr_req;
  bm_in_manager.wr_data         <= bm_out_injector.wr_data & (bm_in_manager.wr_data'high-bm_out_injector.wr_data'length downto 0 => '0');

  bm_in_injector.rd_data        <= bm_out_manager.rd_data(bm_out_manager.rd_data'high downto bm_out_manager.rd_data'high-bm_in_injector.rd_data'high);
  bm_in_injector.rd_req_grant   <= bm_out_manager.rd_req_grant;
  bm_in_injector.rd_valid       <= bm_out_manager.rd_valid;
  bm_in_injector.rd_done        <= bm_out_manager.rd_done;
  bm_in_injector.rd_err         <= bm_out_manager.rd_err;
  bm_in_injector.wr_req_grant   <= bm_out_manager.wr_req_grant;
  bm_in_injector.wr_full        <= bm_out_manager.wr_full;
  bm_in_injector.wr_done        <= bm_out_manager.wr_done;
  bm_in_injector.wr_err         <= bm_out_manager.wr_err;
  

  -----------------------------------------------------------------------------
  -- Component instantiation
  -----------------------------------------------------------------------------

  -- injector core
  core : injector
    generic map (
      dbits           => dbits,
      MAX_SIZE_BURST  => MAX_SIZE_BURST,
      pindex          => pindex,
      paddr           => paddr,
      pmask           => pmask,
      pirq            => pirq,
      ASYNC_RST       => ASYNC_RST
    )
    port map (
      rstn            => rstn,
      clk             => clk,
      apbi            => apbi,
      apbo            => apbo,
      bm0_miso        => bm_in_injector,
      bm0_mosi        => bm_out_injector
    );

  axi4M : axi4_manager
    generic map (
      dbits           => dbits,
      axi_id          => 0,
      rd_n_fifo_regs  => 4,
      wr_n_fifo_regs  => 4,
      ASYNC_RST       => ASYNC_RST,
      Injector_implementation => TRUE
    )
    port map (
      rstn            => rstn,
      clk             => clk,
      axi4mi          => axi4mi,
      axi4mo          => axi4mo,
      bm_in           => bm_in_manager,
      bm_out          => bm_out_manager,
      bm_in_bypass_rd => bm_bypass
    );

end architecture rtl;
-----------------------------------------------------------------------------   
-- Entity:      injector_axi
-- File:        injector_axi.vhd
-- Author:      Francis Fuentes Diaz (BSC-CNS)
-- Description: injector top level entity + AXI4 interface.
------------------------------------------------------------------------------ 
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library safety;
use safety.injector_pkg.all;
use safety.axi4_pkg.all;

-----------------------------------------------------------------------------
-- Top level entity of AXI4 Safe Traffic Injector for testbench.
-- This is a wrapper which integrates the injector core to the
-- AXI4 Full Manager - generic bus master bridge.
-----------------------------------------------------------------------------

entity injector_axi is
  generic (
    -- SafeTI configuration
    PC_LEN          : integer                         := 4;   -- Length of PC register
    MAX_SIZE_BURST: integer range 32 to 4096          := 4096;    -- Maximum number of bytes allowed at a burst transaction.
    -- AXI Manager configuration
    ID_R_WIDTH    : integer range  0 to   32          :=   4;     -- AXI ID's bus width.
    ID_W_WIDTH    : integer range  0 to   32          :=   4;     -- AXI ID's bus width.
    ADDR_WIDTH    : integer range 12 to   64          :=  32;     -- AXI address bus width. (Tested only for 32 bits)
    DATA_WIDTH    : integer range  8 to 1024          := 128;     -- AXI data bus width. [Only power of 2s are allowed]
    axi_id        : integer range  0 to 32**2-1       :=   0;     -- AXI manager burst index [Must be < ID_X_WIDTH**2-1]
    rd_n_fifo_regs: integer range  2 to  256          :=   2;     -- Number of FIFO registers to use at AXI read transactions.  [Only power of 2s are allowed]
    wr_n_fifo_regs: integer range  2 to  256          :=   2;     -- Number of FIFO registers to use at AXI write transactions. [Only power of 2s are allowed]
    -- Asynchronous reset configuration
    ASYNC_RST       : boolean                         := TRUE -- Allow asynchronous reset flag
  );
  port (
    rstn          : in  std_ulogic;         -- Reset
    clk           : in  std_ulogic;         -- Clock
    -- APB interface signals
    apbi          : in  apb_slave_in;       -- APB slave input
    apbo          : out apb_slave_out;      -- APB slave output
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

begin

  -----------------
  -- Assignments --
  -----------------

  bm_in_manager.rd_addr         <= (63 downto bm_out_injector.rd_addr'length => '0') & bm_out_injector.rd_addr;
  bm_in_manager.rd_size         <= bm_out_injector.rd_size;
  bm_in_manager.rd_fixed_addr   <= bm_out_injector.rd_fix_addr;
  bm_in_manager.rd_req          <= bm_out_injector.rd_req;
  bm_in_manager.wr_addr         <= (63 downto bm_out_injector.wr_addr'length => '0') & bm_out_injector.wr_addr;
  bm_in_manager.wr_size         <= bm_out_injector.wr_size;
  bm_in_manager.wr_fixed_addr   <= bm_out_injector.wr_fix_addr;
  bm_in_manager.wr_req          <= bm_out_injector.wr_req;
  bm_in_manager.wr_data         <= (1023 downto DATA_WIDTH => '0') & bm_out_injector.wr_data(DATA_WIDTH - 1 downto 0);

  
  bm_in_manager.rd_axi_cache    <= "0011";
  bm_in_manager.rd_axi_prot     <= "001";
  bm_in_manager.wr_axi_cache    <= "0011";
  bm_in_manager.wr_axi_prot     <= "001";

  bm_in_injector.rd_data        <= bm_out_manager.rd_data(1023 downto DATA_WIDTH) & (DATA_WIDTH - 1 downto 0 => '0');
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

  -- Injector core
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
      bm_out          => bm_out_injector,
      bm_in           => bm_in_injector
    );

  -- AXI4 Manager interface
  axi4M : axi4_manager
    generic map (
      ID_R_WIDTH      => ID_R_WIDTH,
      ID_W_WIDTH      => ID_W_WIDTH,
      ADDR_WIDTH      => ADDR_WIDTH,
      DATA_WIDTH      => DATA_WIDTH,
      axi_id          => axi_id,
      dbits           => DATA_WIDTH,
      rd_n_fifo_regs  => rd_n_fifo_regs,
      wr_n_fifo_regs  => wr_n_fifo_regs,
      ASYNC_RST       => ASYNC_RST
    )
    port map (
      rstn            => rstn,
      clk             => clk,
      axi4mi          => axi4mi,
      axi4mo          => axi4mo,
      bm_in           => bm_in_manager,
      bm_out          => bm_out_manager
    );

end architecture rtl;
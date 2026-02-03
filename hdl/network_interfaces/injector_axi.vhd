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
    PC_LEN            : integer                     :=    8;  -- Length of PC register
    CORE_DATA_WIDTH   : integer range 8 to 1024     :=   32;  -- Data width of the injector core. [Only power of 2s allowed]
    MAX_SIZE_BURST    : integer range 32 to 4096    := 4096;  -- Maximum number of bytes allowed at a burst transaction.
    CSR_READ_INST     : boolean                     := FALSE; -- Instantaneous CSR read
    -- AXI Manager configuration
    ID_R_WIDTH        : integer range  0 to   32    :=   4;   -- AXI ID's bus width.
    ID_W_WIDTH        : integer range  0 to   32    :=   4;   -- AXI ID's bus width.
    ADDR_WIDTH        : integer range 12 to   64    :=  32;   -- AXI address bus width. (Tested only for 32 bits)
    DATA_WIDTH        : integer range  8 to 1024    := 128;   -- AXI data bus width. [Only power of 2s are allowed]
    axi_id            : integer range  0 to 32**2-1 :=   0;   -- AXI manager burst index [Must be < ID_X_WIDTH**2-1]
    rd_n_fifo_regs    : integer range  2 to  256    :=   2;   -- Number of FIFO registers to use at AXI read transactions.  [Only power of 2s are allowed]
    wr_n_fifo_regs    : integer range  2 to  256    :=   2;   -- Number of FIFO registers to use at AXI write transactions. [Only power of 2s are allowed]
    -- Asynchronous reset configuration
    ASYNC_RST         : boolean                     := FALSE  -- Allow asynchronous reset flag
  );
  port (
    rstn                : in  std_ulogic;         -- Reset
    clk                 : in  std_ulogic;         -- Clock
    -- Programming Interface signals
    pb_en               : in  std_logic;
    pb_addr             : in  std_logic_vector( 7 downto 0);
    pb_wr_en            : in  std_logic;
    pb_wdata            : in  std_logic_vector(31 downto 0);
    pb_rdata            : out std_logic_vector(31 downto 0);
    pb_irq              : out std_logic;
    -- AXI interface signals
    axi4i_aw_ready      : in  std_logic;
    axi4i_w_ready       : in  std_logic;
    axi4i_b_id          : in  std_logic_vector(  31 downto 0 );
    axi4i_b_resp        : in  std_logic_vector(   1 downto 0 );
    axi4i_b_valid       : in  std_logic;
    axi4i_ar_ready      : in  std_logic;
    axi4i_r_id          : in  std_logic_vector(  31 downto 0 );
    axi4i_r_data        : in  std_logic_vector(1023 downto 0 );
    axi4i_r_resp        : in  std_logic_vector(   1 downto 0 );
    axi4i_r_last        : in  std_logic;
    axi4i_r_valid       : in  std_logic;
    axi4o_aw_id         : out std_logic_vector(  31 downto 0 );
    axi4o_aw_addr       : out std_logic_vector(  63 downto 0 );
    axi4o_aw_len        : out std_logic_vector(   7 downto 0 );
    axi4o_aw_size       : out std_logic_vector(   2 downto 0 );
    axi4o_aw_burst      : out std_logic_vector(   1 downto 0 );
    axi4o_aw_lock       : out std_logic;
    axi4o_aw_cache      : out std_logic_vector(   3 downto 0 );
    axi4o_aw_prot       : out std_logic_vector(   2 downto 0 );
    axi4o_aw_qos        : out std_logic_vector(   3 downto 0 );
    axi4o_aw_region     : out std_logic_vector(   3 downto 0 );
    axi4o_aw_valid      : out std_logic;
    axi4o_w_data        : out std_logic_vector(1023 downto 0 );
    axi4o_w_strb        : out std_logic_vector( 127 downto 0 );
    axi4o_w_last        : out std_logic;
    axi4o_w_valid       : out std_logic;
    axi4o_b_ready       : out std_logic;
    axi4o_ar_id         : out std_logic_vector(  31 downto 0 );
    axi4o_ar_addr       : out std_logic_vector(  63 downto 0 );
    axi4o_ar_len        : out std_logic_vector(   7 downto 0 );
    axi4o_ar_size       : out std_logic_vector(   2 downto 0 );
    axi4o_ar_burst      : out std_logic_vector(   1 downto 0 );
    axi4o_ar_lock       : out std_logic;
    axi4o_ar_cache      : out std_logic_vector(   3 downto 0 );
    axi4o_ar_prot       : out std_logic_vector(   2 downto 0 );
    axi4o_ar_qos        : out std_logic_vector(   3 downto 0 );
    axi4o_ar_region     : out std_logic_vector(   3 downto 0 );
    axi4o_ar_valid      : out std_logic;
    axi4o_r_ready       : out std_logic
  );
end entity injector_axi;

architecture rtl of injector_axi is

  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------

  -- Exterior and Injector/AXI interface
  signal csri             : safety.injector_pkg.csr_in;   -- SafeTI Program and Debug Input
  signal csro             : safety.injector_pkg.csr_out;  -- SafeTI Program and Debug Output
  signal axi4mi           : safety.axi4_pkg.axi4_miso;    -- AXI4 Manager Input
  signal axi4mo           : safety.axi4_pkg.axi4_mosi;    -- AXI4 Manager Output

  -- Injector and AXI interface
  signal ib_out_injector  : safety.injector_pkg.ib_mosi;  -- Output from injector
  signal ib_in_injector   : safety.injector_pkg.ib_miso;  -- Input to injector
  signal ib_in_manager    : safety.axi4_pkg.ib_mosi;      -- Input to AXI4 Manager interface
  signal ib_out_manager   : safety.axi4_pkg.ib_miso;      -- Output from AXI4 Manager interface

begin

  -----------------
  -- Assignments --
  -----------------

  -- Injector core / Outside
  csri.en             <= pb_en;
  csri.addr           <= pb_addr;
  csri.wr_en          <= pb_wr_en;
  csri.wdata          <= pb_wdata;

  pb_rdata            <= csro.rdata;
  pb_irq              <= csro.irq;


  -- AXI4 Manager interface / Outside
  axi4mi.aw_ready     <= axi4i_aw_ready;
  axi4mi.w_ready      <= axi4i_w_ready;
  axi4mi.b_id         <= axi4i_b_id;
  axi4mi.b_resp       <= axi4i_b_resp;
  axi4mi.b_valid      <= axi4i_b_valid;
  axi4mi.ar_ready     <= axi4i_ar_ready;
  axi4mi.r_id         <= axi4i_r_id;
  axi4mi.r_data       <= axi4i_r_data;
  axi4mi.r_resp       <= axi4i_r_resp;
  axi4mi.r_last       <= axi4i_r_last;
  axi4mi.r_valid      <= axi4i_r_valid;

  axi4o_aw_id         <= axi4mo.aw_id;
  axi4o_aw_addr       <= axi4mo.aw_addr;
  axi4o_aw_len        <= axi4mo.aw_len;
  axi4o_aw_size       <= axi4mo.aw_size;
  axi4o_aw_burst      <= axi4mo.aw_burst;
  axi4o_aw_lock       <= axi4mo.aw_lock;
  axi4o_aw_cache      <= axi4mo.aw_cache;
  axi4o_aw_prot       <= axi4mo.aw_prot;
  axi4o_aw_qos        <= axi4mo.aw_qos;
  axi4o_aw_region     <= axi4mo.aw_region;
  axi4o_aw_valid      <= axi4mo.aw_valid;
  axi4o_w_data        <= axi4mo.w_data;
  axi4o_w_strb        <= axi4mo.w_strb;
  axi4o_w_last        <= axi4mo.w_last;
  axi4o_w_valid       <= axi4mo.w_valid;
  axi4o_b_ready       <= axi4mo.b_ready;
  axi4o_ar_id         <= axi4mo.ar_id;
  axi4o_ar_addr       <= axi4mo.ar_addr;
  axi4o_ar_len        <= axi4mo.ar_len;
  axi4o_ar_size       <= axi4mo.ar_size;
  axi4o_ar_burst      <= axi4mo.ar_burst;
  axi4o_ar_lock       <= axi4mo.ar_lock;
  axi4o_ar_cache      <= axi4mo.ar_cache;
  axi4o_ar_prot       <= axi4mo.ar_prot;
  axi4o_ar_qos        <= axi4mo.ar_qos;
  axi4o_ar_region     <= axi4mo.ar_region;
  axi4o_ar_valid      <= axi4mo.ar_valid;
  axi4o_r_ready       <= axi4mo.r_ready;


  -- Injector core / AXI4 Manager interface
  ib_in_manager.rd_addr         <= (63 downto ib_out_injector.rd_addr'length => '0') & ib_out_injector.rd_addr;
  ib_in_manager.rd_size         <= ib_out_injector.rd_size;
  ib_in_manager.rd_req          <= ib_out_injector.rd_req;
  ib_in_manager.rd_fixed_addr   <= ib_out_injector.rd_fix_addr;
  ib_in_manager.wr_addr         <= (63 downto ib_out_injector.wr_addr'length => '0') & ib_out_injector.wr_addr;
  ib_in_manager.wr_size         <= ib_out_injector.wr_size;
  ib_in_manager.wr_req          <= ib_out_injector.wr_req;
  ib_in_manager.wr_fixed_addr   <= ib_out_injector.wr_fix_addr;
  ib_in_manager.wr_data         <= (ib_in_manager.wr_data'high downto CORE_DATA_WIDTH => '0') & ib_out_injector.wr_data(CORE_DATA_WIDTH - 1 downto 0);

  ib_in_manager.rd_axi_cache    <= "0011";
  ib_in_manager.rd_axi_prot     <= "001";
  ib_in_manager.rd_axi_qos      <= (others => '0');
  ib_in_manager.rd_axi_region   <= (others => '0');
  ib_in_manager.wr_axi_cache    <= "0011";
  ib_in_manager.wr_axi_prot     <= "001";
  ib_in_manager.wr_axi_qos      <= (others => '0');
  ib_in_manager.wr_axi_region   <= (others => '0');

  ib_in_injector.rd_data        <= (ib_in_injector.rd_data'high downto CORE_DATA_WIDTH => '0') & ib_out_manager.rd_data(CORE_DATA_WIDTH - 1 downto 0);
  ib_in_injector.rd_req_grant   <= ib_out_manager.rd_req_grant;
  ib_in_injector.rd_valid       <= ib_out_manager.rd_valid;
  ib_in_injector.rd_done        <= ib_out_manager.rd_done;
  ib_in_injector.rd_err         <= ib_out_manager.rd_err;
  ib_in_injector.wr_req_grant   <= ib_out_manager.wr_req_grant;
  ib_in_injector.wr_full        <= ib_out_manager.wr_full;
  ib_in_injector.wr_done        <= ib_out_manager.wr_done;
  ib_in_injector.wr_err         <= ib_out_manager.wr_err;


  -----------------------------------------------------------------------------
  -- Component instantiation
  -----------------------------------------------------------------------------

  -- Injector core
  core : injector_core
    generic map (
      PC_LEN          => PC_LEN,
      CORE_DATA_WIDTH => CORE_DATA_WIDTH,
      MAX_SIZE_BURST  => MAX_SIZE_BURST,
      CSR_READ_INST   => CSR_READ_INST,
      ASYNC_RST       => ASYNC_RST
    )
    port map (
      rstn            => rstn,
      clk             => clk,
      csri            => csri,
      csro            => csro,
      ib_out          => ib_out_injector,
      ib_in           => ib_in_injector
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
      ib_in           => ib_in_manager,
      ib_out          => ib_out_manager
    );

end architecture rtl;

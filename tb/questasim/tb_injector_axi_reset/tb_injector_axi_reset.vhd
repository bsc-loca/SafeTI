-----------------------------------------------------------------------------   
-- Entity:        tb_injector_axi
-- File:          tb_injector_axi.vhd
-- Author:        Francis Fuentes
-- Description:   Testbench AXI4 manager interface + injector top level entity.
-- Compatibility: This TB requires VHDL2008. However, it is compatible with older
--                compilers by comenting VHDL2008 and uncommenting !VHDL2008 lines.
------------------------------------------------------------------------------ 
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library safety;
use safety.injector_pkg.all;
use safety.tb_injector_pkg.all;
use safety.axi4_pkg.axi4_miso;
use safety.axi4_pkg.axi4_mosi;
use safety.axi4_pkg.axi4_manager;
use std.env.all; -- VHDL2008

-----------------------------------------------------------------------------
-- Top level testbench entity for SafeTI with AXI4 Manager interface.
--
-- This testbench setting is not thought to be used as an automated testing tool, since the tb logic 
-- is not prepared yet to fetch for AXI manager's BM control signals. In addition, it requires a 
-- AXI4 Subordinate module that is not included with this testbench.
-- Some generics may not work for different values from the default ones. 
--
-- This specific testbench tests if the injector is correctly reset (through software bit) in the middle 
-- of an AXI burst without blocking the AXI bus for the transaction type which was being executed during 
-- the reset. Furthermore, this test also allows to check on the injector itself is being reset successfully.
-- 
-----------------------------------------------------------------------------

entity tb_injector_axi_reset is
  generic (
    -- SafeTI configuration
    dbits             : integer range 32 to  128            := 32;        -- Data width of BM and FIFO at injector. [Only power of 2s allowed]
    MAX_SIZE_BURST    : integer range 32 to 4096            := 4096;      -- Maximum size of a beat at a burst transaction.
    -- APB configuration  
    pindex            : integer                             := 6;         -- APB configuartion slave index (default=6)
    paddr             : integer                             := 16#850#;   -- APB configuartion slave address (default=16#850#)
    pmask             : integer                             := 16#FFF#;   -- APB configuartion slave mask (default=16#FFF#)
    pirq              : integer range  0 to APB_IRQ_NMAX-1  := 6;         -- APB configuartion slave irq (default=6)
    -- AXI4 Configuration
    ID_R_WIDTH        : integer range  0 to   32            := 4;         -- AXI ID's bus width.
    ID_W_WIDTH        : integer range  0 to   32            := 4;         -- AXI ID's bus width.
    ADDR_WIDTH        : integer range 12 to   64            := 32;        -- AXI address bus width. (Tested only for 32 bits)
    DATA_WIDTH        : integer range  8 to 1024            := 128;       -- AXI data bus width. [Only power of 2s are allowed]
    axi_id            : integer range  0 to 32**2-1         := 0;         -- AXI manager burst index [Must be < ID_X_WIDTH**2-1]
    rd_n_fifo_regs    : integer range  2 to  256            := 4;         -- Number of FIFO registers to use at AXI read transactions.  [Only power of 2s are allowed]
    wr_n_fifo_regs    : integer range  2 to  256            := 4;         -- Number of FIFO registers to use at AXI write transactions. [Only power of 2s are allowed]
    -- Asynchronous reset configuration
    ASYNC_RST         : boolean                             := FALSE      -- Allow asynchronous reset flag (default=FALSE)
    );

end entity tb_injector_axi_reset;

architecture rtl of tb_injector_axi_reset is
  -----------------------------------------------------------------------------
  -- Constant declaration
  -----------------------------------------------------------------------------

  -- Pointers
  constant apb_inj_addr   : std_logic_vector(31 downto 0) := X"000" & std_logic_vector(to_unsigned(paddr, 12)) & X"00"; -- Location of the injector at APB memory
  constant action_addr    : std_logic_vector(31 downto 0) := X"0000_0003";  -- Write/read address

  -- Injector reset configuration
  constant inj_rst        : std_logic_vector(31 downto 0) := X"0000_00" & "00" & "000010";

  -- Test X configuration: Queue mode enabled, enable interrupt on error, interrupt enabled, (kick disabled), reset, start injector.
  constant inj_configX    : std_logic_vector(31 downto 0) := X"0000_00" & "00" & "111001";
  
  constant descriptorsXwrH : descriptor_bank_tb(0 to 1) := (
    write_descriptor(          524288,  0, WRT, action_addr, '0', '0' ),
    write_descriptor(          524288,  0, WRT, action_addr, '0', '1' )
  );

  constant descriptorsXwrL : descriptor_bank_tb(0 to 1) := (
    write_descriptor(               3,  0, WRT, action_addr, '0', '0' ),
    write_descriptor(               3,  0, WRT, action_addr, '0', '1' )
  );

  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------

  -- Injector I/O + Initial input values
  signal clk    : std_ulogic  := '0';
  signal rstn   : std_ulogic  := '0';

  signal apbi   : apb_slave_in_type := DEF_INJ_APB;
  signal apbo   : apb_slave_out_type;

  signal axi4mi : axi4_miso;
  signal axi4mo : axi4_mosi;

  signal bm_axi_req_rd : std_logic;
  signal bm_axi_req_wr : std_logic;

  -- I/O Injector and AXI interface
  signal bm_out_injector  : safety.injector_pkg.bm_mosi;
  signal bm_in_injector   : safety.injector_pkg.bm_miso;
  signal bm_in_manager    : safety.axi4_pkg.bm_mosi;
  signal bm_out_manager   : safety.axi4_pkg.bm_miso;
  signal bm_out_test      : safety.injector_pkg.bm_miso;

  -- Testbench BM I/O
  signal bm_in  : safety.injector_pkg.bm_mosi;
  signal bm_out : safety.injector_pkg.bm_miso := DEF_INJ_BM;

  -- Selector between Injector BM to AXI manager (TRUE) or testbench (FALSE)
  signal AXI_com  : boolean := FALSE;

  -- APB configuration
  signal apb_sel: std_logic_vector(apbi.sel'range) := std_logic_vector(shift_left(to_unsigned(1, apbi.sel'length), apbi.sel'length-pindex-1));


  -----------------------------------------------------------------------------
  -- Component instantiation
  -----------------------------------------------------------------------------

  -- AXI4 subordinate memory wrapper
  component subordinate_v1_0 is
    generic (
      C_S00_AXI_ID_WIDTH     : integer := 1;
      C_S00_AXI_DATA_WIDTH   : integer := 32;
      C_S00_AXI_ADDR_WIDTH   : integer := 10;
      C_S00_AXI_AWUSER_WIDTH : integer := 0;
      C_S00_AXI_ARUSER_WIDTH : integer := 0;
      C_S00_AXI_WUSER_WIDTH  : integer := 0;
      C_S00_AXI_RUSER_WIDTH  : integer := 0;
      C_S00_AXI_BUSER_WIDTH  : integer := 0
    );
    port (
      s00_AXI_aclk      : in  std_ulogic;
      s00_AXI_aresetn   : in  std_ulogic;
      s00_AXI_awid      : in  std_logic_vector(C_S00_AXI_ID_WIDTH-1 downto 0);
      s00_AXI_awaddr    : in  std_logic_vector(C_S00_AXI_ADDR_WIDTH-1 downto 0);
      s00_AXI_awlen     : in  std_logic_vector(7 downto 0);
      s00_AXI_awsize    : in  std_logic_vector(2 downto 0);
      s00_AXI_awburst   : in  std_logic_vector(1 downto 0);
      s00_AXI_awlock    : in  std_logic;
      s00_AXI_awcache   : in  std_logic_vector(3 downto 0);
      s00_AXI_awprot    : in  std_logic_vector(2 downto 0);
      s00_AXI_awqos     : in  std_logic_vector(3 downto 0);
      s00_AXI_awregion  : in  std_logic_vector(3 downto 0);
      s00_AXI_awuser    : in  std_logic_vector(C_S00_AXI_AWUSER_WIDTH-1 downto 0);
      s00_AXI_awvalid   : in  std_logic;
      s00_AXI_awready   : out std_logic;
      s00_AXI_wdata     : in  std_logic_vector(C_S00_AXI_DATA_WIDTH-1 downto 0);
      s00_AXI_wstrb     : in  std_logic_vector((C_S00_AXI_DATA_WIDTH/8)-1 downto 0);
      s00_AXI_wlast     : in  std_logic;
      s00_AXI_wuser     : in  std_logic_vector(C_S00_AXI_WUSER_WIDTH-1 downto 0);
      s00_AXI_wvalid    : in  std_logic;
      s00_AXI_wready    : out std_logic;
      s00_AXI_bid       : out std_logic_vector(C_S00_AXI_ID_WIDTH-1 downto 0);
      s00_AXI_bresp     : out std_logic_vector(1 downto 0);
      s00_AXI_buser     : out std_logic_vector(C_S00_AXI_BUSER_WIDTH-1 downto 0);
      s00_AXI_bvalid    : out std_logic;
      s00_AXI_bready    : in  std_logic;
      s00_AXI_arid      : in  std_logic_vector(C_S00_AXI_ID_WIDTH-1 downto 0);
      s00_AXI_araddr    : in  std_logic_vector(C_S00_AXI_ADDR_WIDTH-1 downto 0);
      s00_AXI_arlen     : in  std_logic_vector(7 downto 0);
      s00_AXI_arsize    : in  std_logic_vector(2 downto 0);
      s00_AXI_arburst   : in  std_logic_vector(1 downto 0);
      s00_AXI_arlock    : in  std_logic;
      s00_AXI_arcache   : in  std_logic_vector(3 downto 0);
      s00_AXI_arprot    : in  std_logic_vector(2 downto 0);
      s00_AXI_arqos     : in  std_logic_vector(3 downto 0);
      s00_AXI_arregion  : in  std_logic_vector(3 downto 0);
      s00_AXI_aruser    : in  std_logic_vector(C_S00_AXI_ARUSER_WIDTH-1 downto 0);
      s00_AXI_arvalid   : in  std_logic;
      s00_AXI_arready   : out std_logic;
      s00_AXI_rid       : out std_logic_vector(C_S00_AXI_ID_WIDTH-1 downto 0);
      s00_AXI_rdata     : out std_logic_vector(C_S00_AXI_DATA_WIDTH-1 downto 0);
      s00_AXI_rresp     : out std_logic_vector(1 downto 0);
      s00_AXI_rlast     : out std_logic;
      s00_AXI_ruser     : out std_logic_vector(C_S00_AXI_RUSER_WIDTH-1 downto 0);
      s00_AXI_rvalid    : out std_logic;
      s00_AXI_rready    : in  std_logic
    );
  end component subordinate_v1_0;

begin  -- rtl

  -----------------
  -- Assignments --
  -----------------

  -- Clock generation
  clk <= not clk after T/2;

  -- BM interconnect switch used to load descriptors from testbench.
  bm_in_manager.rd_addr       <= (63 downto 32 => '0') & bm_out_injector.rd_addr when AXI_com else (others => '0');
  bm_in_manager.rd_size       <= bm_out_injector.rd_size when AXI_com else (others => '0');
  bm_in_manager.rd_req        <= bm_out_injector.rd_req  when AXI_com else '0';
  bm_in_manager.wr_addr       <= (63 downto 32 => '0') & bm_out_injector.wr_addr when AXI_com else (others => '0');
  bm_in_manager.wr_size       <= bm_out_injector.wr_size when AXI_com else (others => '0');
  bm_in_manager.wr_req        <= bm_out_injector.wr_req  when AXI_com else '0';
  bm_in_manager.wr_data       <= ((1023 downto 128 => '0') & bm_out_injector.wr_data) when AXI_com else (others => '0');

  bm_in_manager.rd_fixed_addr <= '0';
  bm_in_manager.rd_axi_cache  <= "0011";
  bm_in_manager.rd_axi_prot   <= "001";
  bm_in_manager.wr_fixed_addr <= '0';
  bm_in_manager.wr_axi_cache  <= "0011";
  bm_in_manager.wr_axi_prot   <= "001";

  bm_in.rd_addr               <= bm_out_injector.rd_addr when not(AXI_com) else (others => '0');
  bm_in.rd_size               <= bm_out_injector.rd_size when not(AXI_com) else (others => '0');
  bm_in.rd_req                <= bm_out_injector.rd_req  when not(AXI_com) else '0';
  bm_in.wr_addr               <= bm_out_injector.wr_addr when not(AXI_com) else (others => '0');
  bm_in.wr_size               <= bm_out_injector.wr_size when not(AXI_com) else (others => '0');
  bm_in.wr_req                <= bm_out_injector.wr_req  when not(AXI_com) else '0';
  bm_in.wr_data               <= bm_out_injector.wr_data when not(AXI_com) else (others => '0');


  bm_in_injector.rd_data      <= bm_out_manager.rd_data(dbits-1 downto 0) & (127 downto dbits => '0') when AXI_com else bm_out.rd_data;
  bm_in_injector.rd_req_grant <= bm_out_manager.rd_req_grant when AXI_com else bm_out.rd_req_grant;
  bm_in_injector.rd_valid     <= bm_out_manager.rd_valid     when AXI_com else bm_out.rd_valid    ;
  bm_in_injector.rd_done      <= bm_out_manager.rd_done      when AXI_com else bm_out.rd_done     ;
  bm_in_injector.rd_err       <= bm_out_manager.rd_err       when AXI_com else bm_out.rd_err      ;
  bm_in_injector.wr_req_grant <= bm_out_manager.wr_req_grant when AXI_com else bm_out.wr_req_grant;
  bm_in_injector.wr_full      <= bm_out_manager.wr_full      when AXI_com else bm_out.wr_full     ;
  bm_in_injector.wr_done      <= bm_out_manager.wr_done      when AXI_com else bm_out.wr_done     ;
  bm_in_injector.wr_err       <= bm_out_manager.wr_err       when AXI_com else bm_out.wr_err      ;

  bm_out_test.rd_valid        <= bm_out_manager.rd_valid;
  bm_out_test.rd_done         <= bm_out_manager.rd_done;

  
  -----------------------------------------------------------------------------
  -- Sequential process
  -----------------------------------------------------------------------------

  test : process
  begin

    ----------------------------------------
    --               TEST X               --
    ----------------------------------------
    
    apbi.sel  <= apb_sel; -- Set injector at the APB bus to write configuration
    AXI_com   <= FALSE;   -- Change BM connections to testbench, so no AXI communication is established
    wait until rising_edge(clk);
    rstn      <= '1';
    
    -- Load descriptors for test X wr with MAX size
    load_descriptors(clk, apb_inj_addr, descriptorsXwrH, apbi);

    -- Configure injector for test X
    configure_injector(clk, apb_inj_addr, inj_configX, apbo, apbi);
    

    -- Execute some transactions and reset the injector midway
    AXI_com   <= TRUE; -- Connect with AXI interface
    wait for 400 ns;
    configure_injector(clk, apb_inj_addr, inj_rst, apbo, apbi); -- Reset injector
    wait until rising_edge(clk);
    wait for 400 ns;
    
    -- Load new descriptor program
    AXI_com   <= FALSE;   -- Change BM connections to testbench
    configure_injector(clk, apb_inj_addr, inj_configX, apbo, apbi);
    load_descriptors(clk, apb_inj_addr, descriptorsXwrL, apbi);
    AXI_com   <= TRUE; -- Connect with AXI interface
    
    wait for 5000 ns;

    stop;

  end process test;


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
    apbi          => apbi,
    apbo          => apbo,
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
    rd_n_fifo_regs=> 4,
    wr_n_fifo_regs=> 4,
    ASYNC_RST     => ASYNC_RST
  )
  port map (
    rstn            => rstn,
    clk             => clk,
    axi4mi          => axi4mi,
    axi4mo          => axi4mo,
    bm_in           => bm_in_manager,
    bm_out          => bm_out_manager
  );

  -- AXI4 subordinate memory 1024 bytes
  AXI4_S0 : subordinate_v1_0
  generic map (
    C_S00_AXI_ID_WIDTH      => ID_R_WIDTH,
    C_S00_AXI_DATA_WIDTH    => DATA_WIDTH,
    C_S00_AXI_ADDR_WIDTH    => 13, --axi4mo.aw_addr'length,
    C_S00_AXI_AWUSER_WIDTH  => 1,
    C_S00_AXI_ARUSER_WIDTH  => 1,
    C_S00_AXI_WUSER_WIDTH   => 1
  )
  port map (
    s00_AXI_aclk      => clk,
    s00_AXI_aresetn   => rstn,
    s00_AXI_awid      => axi4mo.aw_id(ID_R_WIDTH-1 downto 0),
    s00_AXI_awaddr    => axi4mo.aw_addr(12 downto 0),
    s00_AXI_awlen     => axi4mo.aw_len,
    s00_AXI_awsize    => axi4mo.aw_size,
    s00_AXI_awburst   => axi4mo.aw_burst,
    s00_AXI_awlock    => axi4mo.aw_lock,
    s00_AXI_awcache   => axi4mo.aw_cache,
    s00_AXI_awprot    => axi4mo.aw_prot,
    s00_AXI_awqos     => axi4mo.aw_qos,
    s00_AXI_awregion  => axi4mo.aw_region,
    s00_AXI_awuser    => "0",
    s00_AXI_awvalid   => axi4mo.aw_valid,
    s00_AXI_awready   => axi4mi.aw_ready,
    s00_AXI_wdata     => axi4mo.w_data(DATA_WIDTH-1 downto 0),
    s00_AXI_wstrb     => axi4mo.w_strb(DATA_WIDTH/8-1 downto 0),
    s00_AXI_wlast     => axi4mo.w_last,
    s00_AXI_wuser     => "0",
    s00_AXI_wvalid    => axi4mo.w_valid,
    s00_AXI_wready    => axi4mi.w_ready,
    s00_AXI_bid       => axi4mi.b_id(ID_R_WIDTH-1 downto 0),
    s00_AXI_bresp     => axi4mi.b_resp,
    --s00_AXI_buser     => "0",
    s00_AXI_bvalid    => axi4mi.b_valid,
    s00_AXI_bready    => axi4mo.b_ready,
    s00_AXI_arid      => axi4mo.ar_id(ID_R_WIDTH-1 downto 0),
    s00_AXI_araddr    => axi4mo.ar_addr(12 downto 0),
    s00_AXI_arlen     => axi4mo.ar_len,
    s00_AXI_arsize    => axi4mo.ar_size,
    s00_AXI_arburst   => axi4mo.ar_burst,
    s00_AXI_arlock    => axi4mo.ar_lock,
    s00_AXI_arcache   => axi4mo.ar_cache,
    s00_AXI_arprot    => axi4mo.ar_prot,
    s00_AXI_arqos     => axi4mo.ar_qos,
    s00_AXI_arregion  => axi4mo.ar_region,
    s00_AXI_aruser    => "0",
    s00_AXI_arvalid   => axi4mo.ar_valid,
    s00_AXI_arready   => axi4mi.ar_ready,
    s00_AXI_rid       => axi4mi.r_id(ID_R_WIDTH-1 downto 0),
    s00_AXI_rdata     => axi4mi.r_data(DATA_WIDTH-1 downto 0),
    s00_AXI_rresp     => axi4mi.r_resp,
    s00_AXI_rlast     => axi4mi.r_last,
    --s00_AXI_ruser     => "0",
    s00_AXI_rvalid    => axi4mi.r_valid,
    s00_AXI_rready    => axi4mo.r_ready
  );

  -- Zero not used signals
  axi4mi.b_id(axi4mi.b_id'high downto ID_R_WIDTH)     <= (others => '0');
  axi4mi.r_id(axi4mi.r_id'high downto ID_R_WIDTH)     <= (others => '0');
  axi4mi.r_data(axi4mi.r_data'high downto DATA_WIDTH) <= (others => '0');


end architecture rtl;




-----------------------------------------------------------------------------   
-- Entity:      injector_axi
-- File:        injector_axi.vhd
-- Author:      Francis Fuentes
-- Description: injector top level entity.
------------------------------------------------------------------------------ 
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library bsc;
use bsc.injector_pkg.all;

-----------------------------------------------------------------------------
-- Top level entity for injector.
-- This is a wrapper which integrates injector core to the
-- AXI4 master - generic bus master bridge
--
-- This AXI4 master interface does not allow to have an BM bus wider than the AXI bus
-- (AXI4_DATA_WIDTH must be greater or equal to dbits).
-----------------------------------------------------------------------------

entity injector_axi is
  generic (
    tech          : integer range 0 to numTech := typeTech; -- Target technology
    -- APB configuration  
    pindex        : integer := 0; -- APB configuartion slave index
    paddr         : integer := 0; -- APB configuartion slave address
    pmask         : integer := 16#FFF#; -- APB configuartion slave mask
    pirq          : integer range 0 to APB_IRQ_NMAX - 1 := 1; -- APB configuartion slave irq
    -- Bus master configuration
    dbits         : integer range 32 to  128 := 32; -- Data width of BM and FIFO (must be power of 2)
    axi_id        : integer := 0; -- AXI master index
    MAX_SIZE_BEAT : integer range 32 to 4096 := 1024; -- Maximum size of a beat at a burst transaction.
    -- Injector configuration
    ASYNC_RST     : boolean := FALSE -- Allow asynchronous reset flag
  );
  port (
    rstn    : in std_ulogic; -- Reset
    clk     : in std_ulogic; -- Clock
    -- APB interface signals
    apbi    : in apb_slave_in_type := (others => '0'); -- APB slave input
    apbo    : out apb_slave_out_type; -- APB slave output
    -- AXI interface signals
    axi4mi  : in axi4_in_type      := (others => '0'); -- AXI4 master input 
    axi4mo  : out axi4_out_type -- AXI4 master output
  );
end entity injector_axi;

architecture rtl of injector_axi is
  -----------------------------------------------------------------------------
  -- Constant declaration
  -----------------------------------------------------------------------------

  -- Constants that should be equal to what is written at injector_pkg.vhd
  constant INT_BURST_WIDTH : integer := log_2(MAX_SIZE_BEAT) + 1; -- Register width for maximum byte size at a burst
  constant AXI4_DATA_WIDTH : integer := axi4mo.w_data'length; -- This width value of the AXI data bus must be the same at injector_pkg

  -- AXI constants
  constant size_array : integer(0 to 7) := (1, 2, 4, 8, 16, 32, 64, 128); -- AXI size transfer modes
  constant FIXED      : std_logic_vector(1 downto 0) := "00"; -- AXI burst modes
  constant INC        : std_logic_vector(1 downto 0) := "01";
  constant WRAP       : std_logic_vector(1 downto 0) := "10";

  -----------------------------------------------------------------------------
  -- Records and types
  -----------------------------------------------------------------------------

  -- The operation, when idle, accepts any transaction from the component (write and read), listening to what address and 
  -- size may the operation be. When a request has been detected, this information is processed at the compute state, 
  -- where the burst mode/size/length is set, the end address is computed (if it overflows the 4KB address space, asserts
  -- the two_burst flag so during the operation this is taken into account). The handshake state 

  type transf_state is (idle, compute1, compute2, handshake, operation);

  type transfer_operation is record
    state       : transf_state; -- State of the operation
    grant       : std_logic; -- Grant signals to component
    error       : std_logic; -- Error (4KB boundary overflow)

    axi_mode    : std_logic_vector(1 downto 0); -- AXI parameter: burst mode
    axi_size    : std_logic_vector(2 downto 0); -- AXI parameter: size of each beat in the burst
    axi_len     : std_logic_vector(7 downto 0); -- AXI parameter: number of beats in the burst
    axi_valid   : std_logic;                    -- AXI parameter: valid addr/data/control

    addr        : std_logic_vector(31 downto 0); -- Actual address to transfer
    addr_curr   : std_logic_vector(31 downto 0); -- Starting address
    two_burst   : std_logic; -- Need to slice transfer in two bursts for 4KB boundary

    curr_size   : std_logic_vector(INT_BURST_WIDTH - 1 downto 0); -- Total and then remaining size for first burst
    rem_size    : std_logic_vector(INT_BURST_WIDTH - 1 downto 0); -- Remaining size for second burst (4KB boundary)

    data_full   : std_logic; -- Cache is full, do not receive any more data
    data_bus    : std_logic_vector(AXI4_DATA_WIDTH - 1 downto 0); -- Lanes register
    data_strobe : std_logic_vector(AXI4_DATA_WIDTH/8 - 1 downto 0);
  end record;

  constant RST_TRANSF_OP : transfer_operation := (
    state       => idle,
    grant       => '0',
    error       => '0',
    axi_mode    => (others => '0'),
    axi_size    => (others => '0'),
    axi_len     => (others => '0'),
    axi_valid   => '0',
    addr        => (others => '0'),
    addr_curr   => (others => '0'),
    two_burst   => '0',
    curr_size   => (others => '0'),
    data_full   => '0',
    data_bus    => (others => '0'),
    data_strobe => (others => '0')
  );

  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------

  -- I/O Injector and AXI interface
  signal bm_in : bm_in_type;
  signal bm_out : bm_out_type;
  signal axi_in : axi4_in_type;
  signal axi_out : axi4_out_type;

  -- Signals to manage things
  signal rd_dbits : std_logic_vector(dbits - 1 downto 0); -- This is to addapt the 128bit bm.rd_data bus

  -- Registers for control
  signal wr : transfer_operation;
  signal rd : transfer_operation;

  -----------------------------------------------------------------------------
  -- Function/procedure declaration
  -----------------------------------------------------------------------------

  -- To check: 1) cur_size > size, 2) 4kb_boundary_size > 
  procedure decide_size(
    cur_size  : in  std_logic_vector(12 downto 0);
    addr      : in  std_logic_vector(31 downto 0);
    two_burst : in  std_logic;
    addr_strt : out std_logic_vector(31 downto 0);
    size      : out std_logic_vector( 2 downto 0)
  ) is
  begin

  end procedure decide_size;

  
begin -- rtl

  -----------------
  -- Assignments --
  -----------------
  
  -- Advance eXtensible Interface (interconnect bus)
  -- Write address channel
  axi4mo.aw_id      <= std_logic_vector(to_unsigned(axi_id, axi4mo.aw_id'length));
  axi4mo.aw_addr    <= wr.addr when wr.axi_valid = '1' else (others => '0');
  axi4mo.aw_len     <= (others => '0'); -- Burst length ('0' = 1 transaction)
  axi4mo.aw_burst   <= wr.axi_mode; -- Burst mode
  --axi4mo.aw_lock    <= (others => '0');
  axi4mo.aw_cache   <= 0; --AXI4_CACHE_AW;
  --axi4mo.aw_size    <= std_logic_vector( to_unsigned(log_2(dbits), axi4mo.aw_size'length)); -- Number of bytes to transfer for beat
  --axi4mo.aw_prot    <= ;
  --axi4mo.aw_qos     <= ;
  --axi4mo.aw_region  <= ;
  --axi4mo.aw_valid   <= ;
  ---- Write data channel
  --axi4mo.w_data     <= ;
  --axi4mo.w_strb     <= ;
  --axi4mo.w_last     <= ;
  --axi4mo.w_valid    <= ;
  ---- Write response channel
  --axi4mo.b_ready    <= ;
  ---- Read address channel
  --axi4mo.ar_id      <= ;
  axi4mo.ar_addr    <= rd.addr     when rd.axi_valid = '1' else (others => '0'); -- Address
  axi4mo.ar_len     <= rd.axi_len  when rd.axi_valid = '1' else (others => '0'); -- Number of beats
  axi4mo.ar_burst   <= rd.axi_mode when rd.axi_valid = '1' else (others => '0'); -- Burst mode
  --axi4mo.ar_lock    <= ;
  --axi4mo.ar_cache   <= ;
  axi4mo.ar_size    <= rd.axi_size when rd.axi_valid = '1' else (others => '0'); -- Beat size
  --axi4mo.ar_prot    <= ;
  --axi4mo.ar_qos     <= ;
  --axi4mo.ar_region  <= ;
  axi4mo.ar_valid   <= rd.axi_valid;
  ---- Read data channel
  --axi4mo.r_ready    <= ;
  --axi4mo.r_data     <= ;
  --axi4mo.r_resp     <= ;
  --axi4mo.r_last     <= ;
  --axi4mo.r_valid    <= ;

  -- Bus Master (injector)
  -- Write channel
  bm_in.wr_addr;
  bm_in.wr_size;
  bm_in.wr_req;
  bm_out.wr_req_grant;
  bm_in.wr_data(127 downto 128 - dbits);
  bm_out.wr_full;
  bm_out.wr_done;
  bm_out.wr_err;

  -- Read channel
  bm_in.rd_addr;
  bm_in.rd_size;
  bm_in.rd_req;
  bm_out.rd_req_grant <= rd.grant;
  bm_out.rd_data <= ( 127 downto (128 - dbits) => rd_dbits, others => '0');
  --bm_out.rd_valid <= rd.;
  --bm_out.rd_done <= rd.;
  --bm_out.rd_err <= rd.;


  -----------------------------------------------------------------------------
  -- Sequential process
  -----------------------------------------------------------------------------

  -- READ PROCESS
  read_proc : process (clk, rstn)
    variable addr_temp : std_logic_vector(12 downto 0); -- LSB of ending address for INC mode
  begin
    if (rstn = '0') then
      rd       <= RST_TRANSF_OP;
      rd_dbits <= (others => '0');

    elsif (clk = '1' and clk'event) then
      case rd.state is
        when idle =>
          -- Reset registers
          rd       <= RST_TRANSF_OP;
          rd_dbits <= (others => '0');

          -- Load request information from BM
          if (bm_in.rd_req = '1' and rd.grant = '1') then
            rd.grant      <= '0';           -- Deassert granting requests for BM component
            rd.curr_size  <= bm_in.rd_size; -- Load BM size to transfer (real is +1)
            rd.addr       <= bm_in.rd_addr; -- Load starting address
            rd.state      <= compute1;      -- Change to next state
          else
            rd.grant <= '1'; -- Allow new transaction requests
          end if;

        when compute1 =>
          -- Check if transaction will access two 4KB address boundary
          addr_temp    := add_vector(rd.curr_size, rd.addr(11 downto 0), addr_temp'length);
          rd.two_burst <= addr_temp(12);

          -- If transaction must be split in two bursts, due to 4KB overflow boundary, calculate...
          if(rd.two_burst = '1') then
            -- first burst size (does not require +1)
            rd.curr_size <= sub_vector(x"1000", rd.addr(11 downto 0), rd.curr_size'length);
            -- second burst size (requires +1)
            rd.rem_size  <= sub_vector(addr_temp(11 downto 0), rd.curr_size, rd.rem_size'length);
          end if;

          -- Next compute step
          rd.state <= compute2;

        when compute2 =>
          -- 
              


          rd.axi_size      <= decide_size(rd.curr_size, rd.addr, rd.two_burst);
          --rd.axi_len       <= decide_len(0);
          rd.axi_mode      <= INC;
          rd.axi_valid     <= '1'; -- Request AXI read for first burst

      end case;


    end if;
  end process read_proc;


  -----------------------------------------------------------------------------
  -- Combinational process
  -----------------------------------------------------------------------------


  -----------------------------------------------------------------------------
  -- Component instantiation
  -----------------------------------------------------------------------------

  -- injector core
  core : injector
  generic map(
    tech => tech,
    pindex => pindex,
    paddr => paddr,
    pmask => pmask,
    pirq => pirq,
    dbits => dbits
  )
  port map(
    rstn => rstn,
    clk => clk,
    apbi => apbi,
    apbo => apbo,
    bm0_in => bm_in,
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
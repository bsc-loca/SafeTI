-----------------------------------------------------------------------------   
-- Entity:      injector_exe
-- File:        injector_exe.vhd
-- Author:      Francis Fuentes
-- Description: EXE stage in SafeTI Injector core pipeline.
------------------------------------------------------------------------------ 
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library safety;
use safety.injector_pkg.all;


----------------------------------------------------
-- Entity for EXE stage in Injector core pipeline --
----------------------------------------------------

entity injector_exe is
  generic (
    PC_LEN            : integer range 2 to   10     :=    4;  -- Set the maximum number of programmable descriptor words to 2^^PC_LEN
    CORE_DATA_WIDTH   : integer range 8 to 1024     :=   32;  -- Data width of the injector core. [Only power of 2s allowed]
    MAX_SIZE_BURST    : integer range 8 to 4096     := 1024;  -- Maximum number of bytes allowed at a burst transaction.
    ASYNC_RST         : boolean                     := TRUE   -- Allow asynchronous reset flag
  );
  port (
  -- External I/O
    rstn              : in  std_ulogic;                       -- Reset
    clk               : in  std_ulogic;                       -- Clock
    bm_in             : in  bm_miso;                          -- BM connection with network interface
    bm_out            : out bm_mosi;                          -- BM connection with network interface
  -- Internal I/O
    enable            : in  std_logic;                        -- Enable DECODE stage
    rst_sw            : in  std_logic;                        -- Software reset through APB
      -- Signals from/for DECODE
    decode_ready      : in  std_logic;                        -- Control data ready to be read flag
    exe_ready         : out std_logic;                        -- Control data can be read flag
    decode_pc         : in  unsigned(PC_LEN - 1 downto 0);    -- Descriptor word 0 PC of the operation being executed
    decode_data       : in  bus_decode_exe;                   -- Control signals for operation execution
      -- Control signals
    irq_desc_comp     : out std_logic;                        -- Submodule interruption
    desc_comp         : out std_logic;                        -- Descriptor completion
    program_comp      : out std_logic;                        -- Program completed flag
      -- Debug signals
    pc_ongoing        : out unsigned(PC_LEN - 1 downto 0);    -- PC of descriptor being executed
    state             : out std_logic_vector(MAX_STATUS_LEN - 1 downto 0)
  );
end entity injector_exe;

architecture rtl of injector_exe is

  -----------------------------------------------------------------------------
  -- Component declaration
  -----------------------------------------------------------------------------

  -- DELAY submodule number 0
  component injector_delay is
    generic (
      ASYNC_RST       : boolean                   := TRUE
    );
    port (
      -- External I/O
      rstn            : in  std_ulogic;
      clk             : in  std_ulogic;
      -- Internal I/O
      rst_sw          : in  std_logic;
      start           : in  std_logic;
      busy            : out std_logic;
      desc_data       : in  operation_delay;
      status          : out std_logic_vector(MAX_STATUS_LEN - 1 downto 0)
    );
  end component injector_delay;

  -- READ submodule number 1
  component injector_read is
    generic (
      CORE_DATA_WIDTH : integer range  8 to 1024  :=   32;
      MAX_SIZE_BURST  : integer range  8 to 4096  := 1024;
      ASYNC_RST       : boolean                   := TRUE
    );
    port (
      rstn            : in  std_ulogic;
      clk             : in  std_ulogic;
      rst_sw          : in  std_logic;
      start           : in  std_logic;
      busy            : out std_logic;
      desc_data       : in  operation_rd_wr;
      status          : out std_logic_vector(MAX_STATUS_LEN - 1 downto 0);
      bm_req_grant    : in  std_logic;
      bm_req          : out std_logic;
      bm_valid        : in  std_logic;
      bm_done         : in  std_logic;
      bm_addr         : out std_logic_vector(31 downto 0);
      bm_size         : out std_logic_vector(11 downto 0)
    );
  end component injector_read;

  -- WRITE submodule number 2
  component injector_write is
    generic (
      CORE_DATA_WIDTH : integer range  8 to 1024  :=   32;
      MAX_SIZE_BURST  : integer range  8 to 4096  := 1024;
      ASYNC_RST       : boolean                   := TRUE
    );
    port (
      rstn            : in  std_ulogic;
      clk             : in  std_ulogic;
      rst_sw          : in  std_logic;
      start           : in  std_logic;
      busy            : out std_logic;
      desc_data       : in  operation_rd_wr;
      status          : out std_logic_vector(MAX_STATUS_LEN - 1 downto 0);
      bm_req_grant    : in  std_logic;
      bm_req          : out std_logic;
      bm_full         : in  std_logic;
      bm_done         : in  std_logic;
      bm_addr         : out std_logic_vector(31 downto 0);
      bm_size         : out std_logic_vector(11 downto 0)
    );
   end component injector_write;


-------------------------------------------------------------------------------
-- Labels 
-------------------------------------------------------------------------------

  -- Submodule indexing
  constant SUBM_DELAY : integer :=  0;
  constant SUBM_READ  : integer :=  1;
  constant SUBM_WRITE : integer :=  2;


  -----------------------------------------------------------------------------
  -- Types and reset constants declaration
  -----------------------------------------------------------------------------

  -- State array
  type state_submodule_vector is array (0 to EXE_N_SUBMODULES - 1) of std_logic_vector(MAX_STATUS_LEN - 1 downto 0);

  -- Reset values for submodule_enable type
  constant RESET_SUBMODULE_ENABLE : submodule_enable := (
    delay_en      => '0',
    read_en       => '0',
    write_en      => '0'
  );


  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------

  -- Registers
  signal pc           : unsigned(PC_LEN - 1 downto 0);
  signal active_subm  : submodule_enable;
  signal last_count   : std_logic;
  signal last_descr   : std_logic;
  signal irq_desc_en  : std_logic;

  -- Signals
  signal start        : std_logic;  -- Global start flag
  signal busy         : std_logic;  -- Global busy flag
  signal busy_subm    : std_logic_vector(0 to EXE_N_SUBMODULES - 1);
  signal state_subm   : state_submodule_vector;


begin -- rtl

  -----------------------------------------------------------------------------
  -- Assignments
  -----------------------------------------------------------------------------

  -- I/O signal assignments
  exe_ready     <= enable      and not(busy);
  irq_desc_comp <= irq_desc_en;
  desc_comp     <= last_count  and not(busy);
  program_comp  <= last_count  and not(busy) and last_descr;
  pc_ongoing    <= decode_pc when (start = '1') else pc;


  -- Start flag of any execution
  start     <= '1' when (decode_data.subm_enable /= RESET_SUBMODULE_ENABLE and busy = '0') else '0';

  -- Busy flag of any execution
  busy      <= '1' when (busy_subm /= (busy_subm'range => '0')) else '0';

  -- Multiplex of the state bus
  comb0 : process(start, active_subm, state_subm)
  begin
    if(start = '1') then
      state <= DEBUG_STATE_1st_EXE;
    elsif(active_subm.write_en = '1') then
      state <= state_subm(SUBM_WRITE);
    elsif(active_subm.read_en  = '1') then
      state <= state_subm(SUBM_READ);
    elsif(active_subm.delay_en = '1') then
      state <= state_subm(SUBM_DELAY);
    else
      state <= DEBUG_STATE_IDLE;
    end if;
  end process comb0;
  

  -----------------------------------------------------------------------------
  -- Sequential process
  -----------------------------------------------------------------------------
  
  seq0 : process(clk, rstn)
  begin
    if(rstn = '0' and ASYNC_RST) then
      pc                  <= (others => '0');
      active_subm         <= RESET_SUBMODULE_ENABLE;
      last_count          <= '0';
      last_descr          <= '0';
      irq_desc_en         <= '0';
    elsif rising_edge(clk) then
      if(rstn = '0' or rst_sw = '1') then
        pc                <= (others => '0');
        active_subm       <= RESET_SUBMODULE_ENABLE;
        last_count        <= '0';
        last_descr        <= '0';
        irq_desc_en       <= '0';
      else

        if(enable = '1' and start = '1') then
          pc              <= decode_pc;
          active_subm     <= decode_data.subm_enable;
          last_count      <= decode_data.last_count;
          last_descr      <= decode_data.last_desc;
          irq_desc_en     <= decode_data.irq_desc;
        elsif(busy = '0') then
        -- In case where the execution has ended and no DECODE data is being input, reset the array.
          active_subm     <= RESET_SUBMODULE_ENABLE;
          last_count      <= '0';
        end if;

      end if;
    end if;
  end process seq0;


  -----------------------------------------------------------------------------
  -- Component instantiation
  -----------------------------------------------------------------------------

  -- SUBMODULE: DELAY
  sub_delay : injector_delay 
    generic map (
      ASYNC_RST       => ASYNC_RST
    )
    port map (
      rstn            => rstn,
      clk             => clk,
      rst_sw          => rst_sw,
      start           => decode_data.subm_enable.delay_en,
      busy            => busy_subm(SUBM_DELAY),
      desc_data       => decode_data.delay,
      status          => state_subm(SUBM_DELAY)
  );

  -- SUBMODULE: READ
  sub_rd : injector_read 
    generic map (
      CORE_DATA_WIDTH => CORE_DATA_WIDTH,
      MAX_SIZE_BURST  => MAX_SIZE_BURST,
      ASYNC_RST       => ASYNC_RST
    )
    port map (
      rstn            => rstn,
      clk             => clk,
      rst_sw          => rst_sw,
      start           => decode_data.subm_enable.read_en,
      busy            => busy_subm(SUBM_READ),
      desc_data       => decode_data.rd_wr,
      status          => state_subm(SUBM_READ),
      bm_req_grant    => bm_in.rd_req_grant,
      bm_req          => bm_out.rd_req,
      bm_valid        => bm_in.rd_valid,
      bm_done         => bm_in.rd_done,
      bm_addr         => bm_out.rd_addr,
      bm_size         => bm_out.rd_size
  );

  -- SUBMODULE: WRITE
  sub_wr : injector_write 
    generic map (
      CORE_DATA_WIDTH => CORE_DATA_WIDTH,
      MAX_SIZE_BURST  => MAX_SIZE_BURST,
      ASYNC_RST       => ASYNC_RST
    )
    port map (
      rstn            => rstn,
      clk             => clk,
      rst_sw          => rst_sw,
      start           => decode_data.subm_enable.write_en,
      busy            => busy_subm(SUBM_WRITE),
      desc_data       => decode_data.rd_wr,
      status          => state_subm(SUBM_WRITE),
      bm_req_grant    => bm_in.wr_req_grant,
      bm_req          => bm_out.wr_req,
      bm_full         => bm_in.wr_full,
      bm_done         => bm_in.wr_done,
      bm_addr         => bm_out.wr_addr,
      bm_size         => bm_out.wr_size
  );


end architecture rtl;
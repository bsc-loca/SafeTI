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
--
-- In order to increase performance, the logic of the submodules may allow to initiate the execution 
-- on the same clock cycle that reading the decoded descriptor from the DECODE stage. Thus, it is 
-- important that the submodules assert the busy flag after this time frame.
--
-- Furthermore, using the done flag call the execution of the next descriptor inline. Use this feature 
-- if the submdoule logic supports executing the last execution while executing the next descriptor at 
-- the same time.
--
----------------------------------------------------

entity injector_exe is
  generic (
    PC_LEN            : integer range 2 to   10   :=    4;  -- Set the maximum number of programmable descriptor words to 2^^PC_LEN
    CORE_DATA_WIDTH   : integer range 8 to 1024   :=   32;  -- Data width of the injector core. [Only power of 2s allowed]
    MAX_SIZE_BURST    : integer range 8 to 4096   := 1024;  -- Maximum number of bytes per transaction
    ASYNC_RST         : boolean                   := TRUE   -- Allow asynchronous reset flag
  );
  port (
  -- External I/O
    rstn              : in  std_ulogic;                     -- Reset
    clk               : in  std_ulogic;                     -- Clock
    ib_in             : in  ib_miso;                        -- IB connection with network interface
    ib_out            : out ib_mosi;                        -- IB connection with network interface
  -- Internal I/O
    enable            : in  std_logic;                      -- Enable DECODE stage
    rst_sw            : in  std_logic;                      -- Software reset through APB
      -- Signals from/for DECODE
    decode_ready      : in  std_logic;                      -- Control data ready to be read flag
    exe_ready         : out std_logic;                      -- Control data can be read flag
    decode_pc         : in  unsigned(PC_LEN - 1 downto 0);  -- Descriptor word 0 PC of the operation being executed
    decode_data       : in  bus_decode_exe;                 -- Control signals for operation execution
      -- Control signals
    irq_desc_comp     : out std_logic;                      -- Submodule interruption
    desc_comp         : out std_logic;                      -- Descriptor completion
    program_comp      : out std_logic;                      -- Program completed flag
      -- Debug signals
    pc_ongoing        : out unsigned(PC_LEN - 1 downto 0);  -- PC of descriptor being executed
    error             : out std_logic;                      -- Error flag
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
      ASYNC_RST         : boolean                   := TRUE
    );
    port (
      -- External I/O
      rstn              : in  std_ulogic;
      clk               : in  std_ulogic;
      -- Internal I/O
      rst_sw            : in  std_logic;
      start             : in  std_logic;
      busy              : out std_logic;
      done              : out std_logic;
      desc_data         : in  operation_delay;
      status            : out std_logic_vector(MAX_STATUS_LEN - 1 downto 0)
    );
  end component injector_delay;

  -- READ submodule number 1
  component injector_read is
    generic (
      CORE_DATA_WIDTH   : integer range  8 to 1024  :=   32;
      MAX_SIZE_BURST    : integer range  8 to 4096  := 1024;
      ASYNC_RST         : boolean                   := TRUE
    );
    port (
    -- External I/O
      rstn              : in  std_ulogic;                       -- Reset
      clk               : in  std_ulogic;                       -- Clock
      -- Interface Bus signals
      ib_req_grant      : in  std_logic;                        -- Request granted by network interface
      ib_req            : out std_logic;                        -- Transaction request for the network interface
      ib_valid          : in  std_logic;                        -- Valid data beat from ongoing transaction
      ib_done           : in  std_logic;                        -- Last valid data beat of the last requested transaction
      ib_addr           : out std_logic_vector(31 downto 0);    -- Address where to execute the transaction
      ib_size           : out std_logic_vector(11 downto 0);    -- Encoded number of bytes to transfer (-1 from real transfer size)
      ib_addr_fix       : out std_logic;                        -- Transaction to execute on fixed address.
    -- Internal I/O
      enable            : in  std_logic;                        -- Enable descriptor execution
      rst_sw            : in  std_logic;                        -- Software reset through APB
      start             : in  std_logic;                        -- Start descriptor execution flag
      busy              : out std_logic;                        -- Ongoing descriptor execution flag
      done              : out std_logic;                        -- Completion of the descriptor iteration flag
      desc_data         : in  operation_rd_wr;                  -- Control data to execute descriptor
      error             : out std_logic;                        -- Error flag
      status            : out std_logic_vector(MAX_STATUS_LEN - 1 downto 0) -- Status of the READ transaction
    );
  end component injector_read;

  -- WRITE submodule number 2
  component injector_write is
    generic (
      CORE_DATA_WIDTH   : integer range  8 to 1024  :=   32;
      MAX_SIZE_BURST    : integer range  8 to 4096  := 1024;
      ASYNC_RST         : boolean                   := TRUE
    );
    port (
    -- External I/O
      rstn              : in  std_ulogic;                       -- Reset
      clk               : in  std_ulogic;                       -- Clock
      -- Interface Bus signals
      ib_req_grant      : in  std_logic;                        -- Request granted by network interface
      ib_req            : out std_logic;                        -- Transaction request for the network interface
      ib_full           : in  std_logic;                        -- Valid data beat from ongoing transaction
      ib_done           : in  std_logic;                        -- Done response of the data transfer of the requested transaction
      ib_addr           : out std_logic_vector(31 downto 0);    -- Address where to execute the transaction
      ib_size           : out std_logic_vector(11 downto 0);    -- Encoded number of bytes to transfer (-1 from real transfer size)
      ib_addr_fix       : out std_logic;                        -- Transaction to execute on fixed address.
    -- Internal I/O
      enable            : in  std_logic;                        -- Enable descriptor execution
      rst_sw            : in  std_logic;                        -- Software reset through APB
      start             : in  std_logic;                        -- Start descriptor execution flag
      busy              : out std_logic;                        -- Ongoing descriptor execution flag
      done              : out std_logic;                        -- Completion of the descriptor iteration flag
      desc_data         : in  operation_rd_wr;                  -- Control data to execute descriptor
      error             : out std_logic;                        -- Error flag
      status            : out std_logic_vector(MAX_STATUS_LEN - 1 downto 0) -- Status of the READ transaction
    );
   end component injector_write;


  -----------------------------------------------------------------------------
  -- Types and reset constants declaration
  -----------------------------------------------------------------------------

  -- State array
  type state_submodule_array is record 
    delay_sub         : std_logic_vector(MAX_STATUS_LEN - 1 downto 0);
    read_sub          : std_logic_vector(MAX_STATUS_LEN - 1 downto 0);
    write_sub         : std_logic_vector(MAX_STATUS_LEN - 1 downto 0);
  end record state_submodule_array;

  -- Reset values for submodule_enable type
  constant RESET_SUBMODULE_BIT : submodule_bit := (
    delay_sub         => '0',
    read_sub          => '0',
    write_sub         => '0'
  );


  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------

  -- Registers
  signal pc           : unsigned(PC_LEN - 1 downto 0);
  signal active_subm  : submodule_bit;
  signal last_count   : std_logic;
  signal last_descr   : std_logic;
  signal irq_desc_en  : std_logic;

  -- Signals
  signal rd_decode    : std_logic;      -- Ready to read decoded descriptor signal
  signal start        : std_logic;      -- Global start signal
  signal busy         : std_logic;      -- Global busy signal
  signal done         : std_logic;      -- Global done signal
  signal start_subm   : submodule_bit;  -- Submodules start array signals
  signal busy_subm    : submodule_bit;  -- Submodules busy array signals
  signal done_subm    : submodule_bit;  -- Submodules done array signals
  signal error_subm   : submodule_bit;  -- Submodules error array signals
  signal state_subm   : state_submodule_array;


begin -- rtl

  -----------------------------------------------------------------------------
  -- Assignments
  -----------------------------------------------------------------------------

  -- I/O signal assignments
  exe_ready       <= rd_decode;
  irq_desc_comp   <= irq_desc_en;
  desc_comp       <= last_count  and not(busy);
  program_comp    <= last_count  and not(busy) and last_descr;
  pc_ongoing      <= decode_pc when (start = '1') else pc;
  ib_out.wr_data  <= (ib_out.wr_data'range => '0');
  error           <= '1' when (error_subm /= RESET_SUBMODULE_BIT) else '0';

  -- Read decoded descriptor signal
  rd_decode       <= enable and (not(busy) or done);

  -- Start signal of any execution
  start           <= rd_decode and decode_ready;

  -- Busy signal of any execution
  busy            <= '1' when (busy_subm /= RESET_SUBMODULE_BIT) else '0';

  -- Done signal of any execution
  done            <= '1' when (done_subm /= RESET_SUBMODULE_BIT) else '0';

  -- Set the start array for the submodules.
  start_subm      <= decode_data.subm_enable when (start = '1') else RESET_SUBMODULE_BIT;

  -- The DELAY submodule cannot infeer an error.
  error_subm.delay_sub  <= '0';

  -- Multiplex of the state bus
  comb0 : process(start, active_subm, state_subm)
  begin
    if(start = '1') then
      state <= DEBUG_STATE_1st_EXE;
    elsif(active_subm.write_sub = '1') then
      state <= state_subm.write_sub;
    elsif(active_subm.read_sub  = '1') then
      state <= state_subm.read_sub;
    elsif(active_subm.delay_sub = '1') then
      state <= state_subm.delay_sub;
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
      active_subm         <= RESET_SUBMODULE_BIT;
      last_count          <= '0';
      last_descr          <= '0';
      irq_desc_en         <= '0';
    elsif rising_edge(clk) then
      if(rstn = '0' or rst_sw = '1') then
        pc                <= (others => '0');
        active_subm       <= RESET_SUBMODULE_BIT;
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
          active_subm     <= RESET_SUBMODULE_BIT;
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
      ASYNC_RST         => ASYNC_RST
    )
    port map (
      rstn              => rstn,
      clk               => clk,
      rst_sw            => rst_sw,
      start             => start_subm.delay_sub,
      busy              => busy_subm.delay_sub,
      done              => done_subm.delay_sub,
      desc_data         => decode_data.delay,
      status            => state_subm.delay_sub
  );

  -- SUBMODULE: READ
  sub_rd : injector_read 
    generic map (
      CORE_DATA_WIDTH   => CORE_DATA_WIDTH,
      MAX_SIZE_BURST    => MAX_SIZE_BURST,
      ASYNC_RST         => ASYNC_RST
    )
    port map (
      rstn              => rstn,
      clk               => clk,
      ib_req_grant      => ib_in.rd_req_grant,
      ib_req            => ib_out.rd_req,
      ib_valid          => ib_in.rd_valid,
      ib_done           => ib_in.rd_done,
      ib_addr           => ib_out.rd_addr,
      ib_size           => ib_out.rd_size,
      ib_addr_fix       => ib_out.rd_fix_addr,
      enable            => enable,
      rst_sw            => rst_sw,
      start             => start_subm.read_sub,
      busy              => busy_subm.read_sub,
      done              => done_subm.read_sub,
      desc_data         => decode_data.rd_wr,
      error             => error_subm.read_sub,
      status            => state_subm.read_sub

  );

  -- SUBMODULE: WRITE
  sub_wr : injector_write 
    generic map (
      CORE_DATA_WIDTH   => CORE_DATA_WIDTH,
      MAX_SIZE_BURST    => MAX_SIZE_BURST,
      ASYNC_RST         => ASYNC_RST
    )
    port map (
      rstn              => rstn,
      clk               => clk,
      ib_req_grant      => ib_in.wr_req_grant,
      ib_req            => ib_out.wr_req,
      ib_full           => ib_in.wr_full,
      ib_done           => ib_in.wr_done,
      ib_addr           => ib_out.wr_addr,
      ib_size           => ib_out.wr_size,
      ib_addr_fix       => ib_out.wr_fix_addr,
      enable            => enable,
      rst_sw            => rst_sw,
      start             => start_subm.write_sub,
      busy              => busy_subm.write_sub,
      done              => done_subm.write_sub,
      desc_data         => decode_data.rd_wr,
      error             => error_subm.write_sub,
      status            => state_subm.write_sub
  );


end architecture rtl;
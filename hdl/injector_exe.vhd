------------------------------------------------------------------------------
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
    PC_LEN            : integer                   :=    4;  -- Set the maximum number of programmable descriptor words to 2^^PC_LEN
    CORE_DATA_WIDTH   : integer range 8 to 1024   :=   32;  -- Data width of the injector core. [Only power of 2s allowed]
    MAX_SIZE_BURST    : integer range 8 to 4096   := 1024;  -- Maximum number of bytes per transaction
    DUAL_CHANNEL_INT  : boolean                   := FALSE; -- Design flag for when the Traffic interface supports dual-channel
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
    rst_sw            : in  std_logic;                      -- Software reset through CSR
      -- Signals from/for CSR
    hold_csr          : in std_logic;                       -- Hold operations from CSR interface flag
    wr_data_csr       : in std_logic_vector(31 downto 0);   -- EXE write descriptor data
      -- Signals from/for DECODE
    decode_ready      : in  std_logic;                      -- Control data ready to be read flag
    exe_read          : out std_logic;                      -- Control data can be read flag
    decode_pc         : in  unsigned(PC_LEN - 1 downto 0);  -- Descriptor word 0 PC of the operation being executed
    decode_data       : in  bus_decode_exe;                 -- Control signals for operation execution
      -- Control signals
    irq_desc_comp     : out std_logic;                      -- Submodule interruption
    desc_comp         : out std_logic;                      -- Descriptor completion
    program_comp      : out std_logic;                      -- Program completed flag
      -- Debug signals
    pc_ongoing        : out unsigned(PC_LEN - 1 downto 0);  -- PC of descriptor being executed
    error             : out std_logic;                      -- Error flag
    state             : out DEBUG_STATE
  );
end entity injector_exe;

architecture rtl of injector_exe is

  -----------------------------------------------------------------------------
  -- Component declaration
  -----------------------------------------------------------------------------

  -- DELAY submodule number 1
  component injector_delay is
    generic (
      ASYNC_RST         : boolean := TRUE -- Enable asynchronous HW reset
    );
    port (
      -- Common I/O
      rstn              : in  std_ulogic; -- Active low HW reset
      clk               : in  std_ulogic; -- Clock signal
      -- Internal I/O
      rst_sw            : in  std_logic;  -- Synchronous active high SW reset
      program           : in  std_logic;  -- Program submodule flag
      start             : in  std_logic;  -- Start execution flag
      hold              : in  std_logic;  -- Hold execution flag
      busy              : out std_logic;  -- Busy submodule flag
      full              : out std_logic;  -- Full submodule flag
      desc_data         : in  operation_delay; -- Operation data
      error             : out std_logic;       -- Error flag
      status            : out DEBUG_STATE -- Status of the execution
    );
  end component injector_delay;

  -- REQUEST submodule number 2/3
  component injector_requester is
    generic (
      MAX_SIZE_BURST    : integer range 8 to 4096     := 1024;  -- Maximum number of bytes per transaction
      ASYNC_RST         : boolean                     := TRUE   -- Allow asynchronous reset flag
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
      rst_sw            : in  std_logic;  -- Synchronous active high SW reset
      program           : in  std_logic;  -- Program submodule flag
      start             : in  std_logic;  -- Start execution flag
      hold              : in  std_logic;  -- Hold execution flag
      busy              : out std_logic;  -- Busy submodule flag
      full              : out std_logic;  -- Full submodule flag
      desc_data         : in  operation_rd_wr; -- Operation data
      error             : out std_logic;       -- Error flag
      status            : out DEBUG_STATE -- Status of the transaction
    );
  end component injector_requester;

  -- OBSERVER submodule number 4
  component injector_observer is
    generic (
      ASYNC_RST         : boolean := TRUE -- Enable asynchronous HW reset
    );
    port (
      -- Common I/O
      rstn              : in  std_ulogic; -- Active low HW reset
      clk               : in  std_ulogic; -- Clock signal
      -- Internal I/O
      rst_sw            : in  std_logic;  -- Synchronous active high SW reset
      program           : in  std_logic;  -- Program submodule flag
      start             : in  std_logic;  -- Start execution flag
      hold              : in  std_logic;  -- Hold execution flag
      busy              : out std_logic;  -- Busy submodule flag
      full              : out std_logic;  -- Full submodule flag
      desc_data         : in  bus_decode_exe; -- Operation data
      external_addr     : in  std_logic_vector(31 downto 0); -- Interface address
      status            : out DEBUG_STATE
    );
  end component injector_observer;


  -----------------------------------------------------------------------------
  -- Types and reset constants declaration
  -----------------------------------------------------------------------------

  -- State array
  type submodule_state_array is array (submodule_bit'range) of DEBUG_STATE;

  -- Submodule bit array for execution buffer (activation, execution order buffers)
    -- This array length sets the FIFO length, which has been validated only
    -- for 3 (0 to 2). Larger FIFOs could be implemented to allow many more
    -- descriptors being initilizated in parallel, but that could generate
    -- larger execution bubbles, resulting in unintended delays being applied.
    -- In addition, no execution descriptors (eg OBSERVER_ADDR) require updating
    -- exe_fifo mid-FIFO, which complicates things.
  type submodule_vector is array (0 to 2) of submodule_bit;

  -- FIFO bit array for execution options
  type desc_options is record
    pc                  : unsigned(PC_LEN - 1 downto 0);
    last_count          : std_logic;
    last_descr          : std_logic;
    irq_desc_en         : std_logic;
  end record desc_options;
  type desc_options_vector is array (submodule_vector'range) of desc_options;


  -------------------------------------------------------------------------------
  -- Reset constants
  -------------------------------------------------------------------------------

  constant RESET_DESC_OPTIONS : desc_options := (
    pc              => (others => '0'),
    last_count      => '0',
    last_descr      => '0',
    irq_desc_en     => '0'
  );


  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------

  -- Registers
  signal init_desc      : std_logic;      -- Top FIFO flag for descriptor init
  signal en_fifo        : std_logic_vector(submodule_vector'range);
  signal exe_fifo       : submodule_vector;
  signal options_fifo   : desc_options_vector;

  -- Signals
  signal exe_ready      : std_logic;      -- Ready to program descriptor
  signal fifo_full      : std_logic;      -- Full exe_fifo program reg
  signal fifo_empty     : std_logic;      -- Empty exe_fifo activation reg+done
  signal fifo_push      : std_logic_vector(submodule_vector'range);
  signal start_halt     : std_logic;      -- Special conditions for avoiding early start
  signal program        : std_logic;      -- Global descriptor program signal
  signal start_req      : std_logic;      -- Global start signal request
  signal start          : std_logic;      -- Global start signal
  signal busy           : std_logic;      -- Global busy signal
  signal done           : std_logic;      -- Global done descriptor signal
    -- EXE intermediary arrays
  signal load_subm      : submodule_bit;  -- Submodules program DECODE array
  signal program_subm   : submodule_bit;  -- Submodules program array signals
  signal activation_subm: submodule_bit;  -- Submodules activation signals
  signal active_subm    : submodule_bit;  -- Active submodule execution
  signal active_options : desc_options;   -- Executing descriptor options
    -- To submodules
  signal start_subm     : submodule_bit;  -- Submodules start array signals
  signal hold_subm      : submodule_bit;  -- Submodules hold array signals
    -- From submodules
  signal busy_subm      : submodule_bit;  -- Submodules busy array signals
  signal full_subm      : submodule_bit;  -- Submodules full array signals
  signal error_subm     : submodule_bit;  -- Submodules error array signals
    -- Debug
  signal state_subm     : submodule_state_array;
  signal state_exe      : DEBUG_STATE;


begin -- rtl

  -- The EXE stages applies several FIFO registers as an execution schedule:
  -- 1) Top FIFO level, descriptor initialization:
    -- The top level of the FIFO registers load control data whenever a
    -- descriptor is programmed into the submodules. For such action to occur,
    -- it's required that the respective submodules related to the descriptor
    -- being delivered from DECODE stage are empty (full_subm(i) = 0).
    -- Once a descriptor has been programmed (program = 1), the top FIFO
    -- registers of en_fifo, exe_fifo and options_fifo will contain a 1,
    -- the submodule in question, and common descriptor options respectively,
    -- in addition to setting the init_desc flag to 1.
    -- During this FIFO level or to the next, the submodule must be activated,
    -- and for doing so, it's required that the exe_fifo respective submodules
    -- are not executing a descriptor (aka, busy_subm(i) = 0).
    -- Once activated (start = '1'), there can be 2 situations:
      -- A) The lower FIFO levels are full and there is no carry in the FIFO
      --    (fifo_push(i-1) = 0). In this case, the init_desc flag is set to 0
      --    and no further initialization signals are sent to avoid duplicates.
      --    The top FIFO level will remain full (en_fifo(i) = 1), and no
      --    descriptors will be programmed any further until the FIFO carry.
      -- B) The next FIFO level is empty (en_fifo(i-1) = 0) or there's a FIFO
      --    carry (fifo_push(i-1) = 1), thus, the control data will be moved to
      --    the lower level, giving the option of loading a new descriptor.
      --    In this case, the FIFO top level will set to 0 both en_fifo(i) and
      --    init_desc, unless a new descriptor enters the FIFO (is programmed).
  -- 2) Intermediate FIFO levels, descriptor buffer:
    -- The only need for these intermediate levels is for giving slack to the
    -- submodules initialization so the execution gets prepared for when they're
    -- needed. The control data is propagated in the FIFO downwards, using the
    -- en_fifo(i) flags for signaling that the FIFO slot is full.
    -- When they're empty, they load the control data from upper levels, while
    -- also considering if bottom levels are empty.
  -- 3) Bottom FIFO level, descriptor execution:
    -- Once the control data reaches the bottom of the FIFO, a hold system
    -- releases the execution of the submodules now stored at exe_fifo(0), so
    -- the actual execution of the descriptor is carried out.
    -- The bottom FIFO levels will only load control data from the level above
    -- if the FIFO is empty (en_fifo(0) = 0) or when a descriptor is completed
    -- by setting the busy_subm(i) signals of the submodules in execution
    -- (exe_subm(0)) to 0 (busy = 0).
  --
  -- In summary, descriptors are loaded into the submodules and top of the FIFO,
  -- when said submodules are empty (full_subm(i) = 0) and DECODE stage offers
  -- a descriptor to be programmed.
  -- The control data is held on the top of the FIFO until said submodules are
  -- started at least, and will remain there unless lower levels carry the data.
  -- Bottom FIFO orchestrates the execution of the descriptors, which should
  -- be prepared thanks to the FIFO construction and back-pressure from DECODE.
  --
  -- This design puts some requirements on the submodules, such as (1) one cycle
  -- must be spent for programing, setting full to 1, (2) another clock cycle
  -- for starting, raising busy to 1 until just before the last clock cycle of
  -- the execution, (3) the full flag must set to 0 as early as possible during
  -- the execution for allowing the next decriptor programing, and (4) the busy
  -- flag must be set to 0 by the last clock cycle of the operation in order to
  -- ensure time accuracy on next descriptors.
  --

  -----------------------------------------------------------------------------
  -- Assignments
  -----------------------------------------------------------------------------

  -- I/O signal assignments
  exe_read        <= exe_ready;
  irq_desc_comp   <= done and active_options.irq_desc_en;
  desc_comp       <= done;
  program_comp    <= done and active_options.last_descr;
  pc_ongoing      <= active_options.pc;
  ib_out.rd_hold  <= enable and hold_subm(SUBM_ID_READ);   -- READ and WRITE executions
  ib_out.wr_hold  <= enable and hold_subm(SUBM_ID_WRITE);  -- are held at interface level
  ib_out.wr_data  <= (ib_out.wr_data'high downto wr_data_csr'length => '0') & wr_data_csr;
  error           <= '1' when (error_subm /= RESET_SUBMODULE_BIT) else '0';
  state           <= state_exe;

  -- Programming logic
    -- Read from DECODE when the submodules to be programmed "program_subm" are
    -- not full "not(full_subm)". In affirmative case, keep programing
    -- descriptors until the fifo is full or not busy.
  program         <= exe_ready and decode_ready;
  program_subm    <= load_subm when (exe_ready = '1') else RESET_SUBMODULE_BIT;
  exe_ready       <= (enable and not(fifo_full)) when ( (load_subm and not(full_subm)) = load_subm ) else '0';
  load_subm       <= decode_data.subm_enable;

  -- Activation logic
    -- Submodule activation post-program is achieved by comparing what is
    -- required to be started "activation_subm", and if it can be started
    -- "not(busy_subm)". If a start cannot be propagated, the bus used for
    -- starting the submodules "start_subm" is lowered.
    -- Said start must only happen once per descriptor count, thus, the
    -- "init_desc" register is used as high flag if not start has been sent yet.
  activation_subm <= exe_fifo(exe_fifo'high);
  start_req       <= enable and init_desc and not(start_halt);
  start           <= start_req when ( (activation_subm and not(busy_subm)) = activation_subm ) else '0';
  start_subm      <= activation_subm when (start = '1') else RESET_SUBMODULE_BIT;

    -- Avoid mixing incompatible descriptors (eg, READ and WRITE requests)
    -- executions on single-channel interfaces.
  dual_channel_check : if DUAL_CHANNEL_INT generate
    start_halt    <= '0';
  end generate dual_channel_check;
  single_channel_check : if not(DUAL_CHANNEL_INT) generate
    start_halt    <= ((full_subm(SUBM_ID_READ)  and activation_subm(SUBM_ID_WRITE)) or
                      (full_subm(SUBM_ID_WRITE) and activation_subm(SUBM_ID_READ)));
  end generate single_channel_check;

  -- Hold logic
    -- The active descriptor propagates a hold "hold_subm" to the submodules,
    -- so the executions are prepared but not started until the actual
    -- operation is completed.
  active_subm     <= exe_fifo(0);
  active_options  <= options_fifo(0);
  busy            <= '1' when ( (busy_subm and active_subm) /= (busy_subm'range => '0') ) else '0';
    -- The done descriptor signal includes a mask of the active and busy
    -- submodule buses to ensure full completion of multi-submodules operations.
  done            <= active_options.last_count and en_fifo(0) when ( (active_subm and not(busy_subm)) = active_subm ) else '0';

  -- Set constants to unset signals
  error_subm(SUBM_ID_OBSERVER)    <= '0'; -- OBSERVER cannot produce an error

  -- Multiplex of the state and hold bus
  comb0 : process(active_subm, state_subm, hold_csr)
  begin
    state_exe     <= DEBUG_STATE_IDLE;
    hold_subm     <= (others => '1');
    if(active_subm(SUBM_ID_WRITE) = '1') then
      state_exe   <= state_subm(SUBM_ID_WRITE);
      hold_subm(SUBM_ID_WRITE)    <= hold_csr;
    end if;
    if(active_subm(SUBM_ID_READ)  = '1') then
      state_exe   <= state_subm(SUBM_ID_READ);
      hold_subm(SUBM_ID_READ)     <= hold_csr;
    end if;
    if(active_subm(SUBM_ID_DELAY) = '1') then
      state_exe   <= state_subm(SUBM_ID_DELAY);
      hold_subm(SUBM_ID_DELAY)    <= hold_csr;
    end if;
    if(active_subm(SUBM_ID_OBSERVER) = '1') then
      state_exe   <= state_subm(SUBM_ID_OBSERVER);
      hold_subm(SUBM_ID_OBSERVER) <= hold_csr;
    end if;
  end process comb0;

  -- FIFO combinational carry logic
  fifo_full       <= en_fifo(en_fifo'high) and not(fifo_push(fifo_push'high));
  fifo_empty      <= fifo_push(0);

  comb1 : for i in fifo_push'range generate
  begin
    fifo_carry : process(fifo_push, en_fifo, start, init_desc, busy)
    begin
      case i is
        when fifo_push'high =>
          fifo_push(i)  <= (fifo_push(i-1) or not(en_fifo(i))) and (start or not(init_desc));
        when 0 =>
          fifo_push(i)  <= not(busy);
        when others =>
          fifo_push(i)  <= fifo_push(i-1) or not(en_fifo(i));
      end case;
    end process fifo_carry;
  end generate comb1;


  -----------------------------------------------------------------------------
  -- Sequential process
  -----------------------------------------------------------------------------

  -- Manage DECODE descriptor into FIFOs, as execution order and options
  seq0 : for i in en_fifo'range generate
  begin
    exe_schedule : process(clk, rstn)
    begin
      if(rstn = '0' and ASYNC_RST) then
        en_fifo(i)        <= '0';
        exe_fifo(i)       <= RESET_SUBMODULE_BIT;
        options_fifo(i)   <= RESET_DESC_OPTIONS;
      elsif rising_edge(clk) then
        if(rstn = '0' or rst_sw = '1') then
          en_fifo(i)      <= '0';
          exe_fifo(i)     <= RESET_SUBMODULE_BIT;
          options_fifo(i) <= RESET_DESC_OPTIONS;
        else

          if(enable = '1') then
            case i is
              when en_fifo'high =>
                if(program = '1') then
                  en_fifo(i)      <= '1';
                  exe_fifo(i)     <= decode_data.subm_enable;
                  options_fifo(i).pc            <= decode_pc;
                  options_fifo(i).last_count    <= decode_data.last_count;
                  options_fifo(i).last_descr    <= decode_data.last_desc;
                  options_fifo(i).irq_desc_en   <= decode_data.irq_desc;
                elsif(fifo_push(i-1) = '1') then
                  en_fifo(i)      <= not(start or not(init_desc));
                end if;
              when en_fifo'high - 1 =>
                if(fifo_push(i) = '1') then
                  if((start or not(init_desc)) = '1') then
                    options_fifo(i) <= options_fifo(i+1);
                    en_fifo(i)    <= en_fifo(i+1);
                  else
                    en_fifo(i)    <= '0';
                  end if;
                  exe_fifo(i)     <= exe_fifo(i+1);
                end if;
              when others =>
                if(fifo_push(i) = '1') then
                  en_fifo(i)      <= en_fifo(i+1);
                  -- In case the operation doesn't do execution (OBSERVER_ADDR)
                  exe_fifo(i)     <= exe_fifo(i+1) and busy_subm;
                  options_fifo(i) <= options_fifo(i+1);
                end if;
            end case;
          end if; -- EXE enable = '1'

        end if;
      end if;
    end process exe_schedule;
  end generate seq0;

  -- Initialization flag management for top FIFO level descriptor
  seq1 : process(clk, rstn)
  begin
    if(rstn = '0' and ASYNC_RST) then
      init_desc           <= '0';
    elsif rising_edge(clk) then
      if(rstn = '0' or rst_sw = '1') then
        init_desc         <= '0';
      else

        if(enable = '1') then
        -- Limit start signal to single pulse per descriptor count
          if(program = '1') then
            init_desc     <= '1';
          elsif(start = '1') then
            init_desc     <= '0';
          end if;
        end if; -- EXE enable = '1'

      end if;
    end if;
  end process seq1;


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
      program           => program_subm(SUBM_ID_DELAY),
      start             => start_subm(SUBM_ID_DELAY),
      hold              => hold_subm(SUBM_ID_DELAY),
      busy              => busy_subm(SUBM_ID_DELAY),
      full              => full_subm(SUBM_ID_DELAY),
      desc_data         => decode_data.delay,
      error             => error_subm(SUBM_ID_DELAY),
      status            => state_subm(SUBM_ID_DELAY)
  );

  -- SUBMODULE: REQUESTER (simplified READ/WRITE)
  sub_rd : injector_requester
    generic map (
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
      rst_sw            => rst_sw,
      program           => program_subm(SUBM_ID_READ),
      start             => start_subm(SUBM_ID_READ),
      hold              => hold_subm(SUBM_ID_READ),
      busy              => busy_subm(SUBM_ID_READ),
      full              => full_subm(SUBM_ID_READ),
      desc_data         => decode_data.rd_wr,
      error             => error_subm(SUBM_ID_READ),
      status            => state_subm(SUBM_ID_READ)
  );

  sub_wr : injector_requester
    generic map (
      MAX_SIZE_BURST    => MAX_SIZE_BURST,
      ASYNC_RST         => ASYNC_RST
    )
    port map (
      rstn              => rstn,
      clk               => clk,
      ib_req_grant      => ib_in.wr_req_grant,
      ib_req            => ib_out.wr_req,
      ib_valid          => '0',
      ib_done           => ib_in.wr_done,
      ib_addr           => ib_out.wr_addr,
      ib_size           => ib_out.wr_size,
      ib_addr_fix       => ib_out.wr_fix_addr,
      rst_sw            => rst_sw,
      program           => program_subm(SUBM_ID_WRITE),
      start             => start_subm(SUBM_ID_WRITE),
      hold              => hold_subm(SUBM_ID_WRITE),
      busy              => busy_subm(SUBM_ID_WRITE),
      full              => full_subm(SUBM_ID_WRITE),
      desc_data         => decode_data.rd_wr,
      error             => error_subm(SUBM_ID_WRITE),
      status            => state_subm(SUBM_ID_WRITE)
  );

  -- SUBMODULE: OBSERVER
  sub_obs : injector_observer
    generic map (
      ASYNC_RST         => ASYNC_RST
    )
    port map (
      rstn              => rstn,
      clk               => clk,
      rst_sw            => rst_sw,
      program           => program_subm(SUBM_ID_OBSERVER),
      start             => start_subm(SUBM_ID_OBSERVER),
      hold              => hold_subm(SUBM_ID_OBSERVER),
      busy              => busy_subm(SUBM_ID_OBSERVER),
      full              => full_subm(SUBM_ID_OBSERVER),
      desc_data         => decode_data,
      external_addr     => ib_in.external_addr,
      status            => state_subm(SUBM_ID_OBSERVER)
  );


end architecture rtl;

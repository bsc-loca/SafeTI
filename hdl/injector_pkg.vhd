------------------------------------------------------------------------------
-- Package:     injector_pkg
-- File:        injector_pkg.vhd
-- Author:      Francis Fuentes, Oriol Sala
-- Description: Internal package for Traffic Injector
------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


package injector_pkg is

-------------------------------------------------------------------------------
-- Parametric constants 
-------------------------------------------------------------------------------

  constant MAX_DESC_WORDS   : integer := 2; -- Maximum number of words for descriptors.
  constant MAX_STATUS_LEN   : integer := 5; -- Maximum length of the internal status flag field from any stage.
  constant EXE_N_SUBMODULES : integer := 3; -- Number of task submodules on the EXE stage.


-------------------------------------------------------------------------------
-- Labels 
-------------------------------------------------------------------------------

  -- Descriptor types (action_type)
  constant OP_DELAY       : std_logic_vector(4 downto 0) := "00000";
  constant OP_READ        : std_logic_vector(4 downto 0) := "00001";
  constant OP_WRITE       : std_logic_vector(4 downto 0) := "00010";
  constant OP_BRANCH      : std_logic_vector(4 downto 0) := "00100"; -- TODO: Implement the BRANCH operation.
  constant OP_READ_FIX    : std_logic_vector(4 downto 0) := "00101";
  constant OP_WRITE_FIX   : std_logic_vector(4 downto 0) := "00110";
  constant OP_META        : std_logic_vector(4 downto 0) := "11111"; -- TODO: Use META type to add new descriptor words.

  -- Debug states
  constant DEBUG_STATE_IDLE               : std_logic_vector(MAX_STATUS_LEN - 1 downto 0) := "00000"; -- Common debug states
  constant DEBUG_STATE_1st_EXE            : std_logic_vector(MAX_STATUS_LEN - 1 downto 0) := "00001"; -- EXE debug states
  constant DEBUG_STATE_REQUEST            : std_logic_vector(MAX_STATUS_LEN - 1 downto 0) := "00010"; -- READ and WRITE debug states
  constant DEBUG_STATE_DATA_TRANSFER      : std_logic_vector(MAX_STATUS_LEN - 1 downto 0) := "00011";
  constant DEBUG_STATE_WAIT_DONE          : std_logic_vector(MAX_STATUS_LEN - 1 downto 0) := "00100";
  constant DEBUG_STATE_NO_OPERATION       : std_logic_vector(MAX_STATUS_LEN - 1 downto 0) := "00101"; -- DELAY debug state
  constant DEBUG_STATE_1st_DECODE         : std_logic_vector(MAX_STATUS_LEN - 1 downto 0) := "01001"; -- DECODE debug states
  constant DEBUG_STATE_REPETITION         : std_logic_vector(MAX_STATUS_LEN - 1 downto 0) := "01010";
  -- Error debug states
  constant DEBUG_STATE_UNEXPECTED_DATA    : std_logic_vector(MAX_STATUS_LEN - 1 downto 0) := "10000"; -- READ and WRITE error debug states
  constant DEBUG_STATE_UNEXPECTED_DONE    : std_logic_vector(MAX_STATUS_LEN - 1 downto 0) := "10001";
  constant DEBUG_STATE_UNEXPECTED_START   : std_logic_vector(MAX_STATUS_LEN - 1 downto 0) := "11000"; -- Common error states


-------------------------------------------------------------------------------
-- Common types 
-------------------------------------------------------------------------------

  -- Descriptor words type
  type desc_words is array (0 to MAX_DESC_WORDS - 1) of std_logic_vector(31 downto 0);

  -- Type for common signals on pipeline components (enable, reset, irq)
  type pipeline_common_array is record
    fetch                   : std_logic;
    decode                  : std_logic;
    exe                     : std_logic;
  end record pipeline_common_array;

  -- Common signal arrays of the pipeline stages
  type pipeline_state_array is record
    fetch             : std_logic_vector(MAX_STATUS_LEN - 1 downto 0);
    decode            : std_logic_vector(MAX_STATUS_LEN - 1 downto 0);
    exe               : std_logic_vector(MAX_STATUS_LEN - 1 downto 0);
  end record pipeline_state_array;
  

-------------------------------------------------------------------------------
-- External I/O signaling types 
-------------------------------------------------------------------------------

  -- IB bus types
  type ib_miso is record  --Input to injector_ctrl from bus master interface output
    -- Read channel
    rd_data           : std_logic_vector(127 downto 0);
    rd_req_grant      : std_logic;
    rd_valid          : std_logic;
    rd_done           : std_logic;
    rd_err            : std_logic;
    -- Write channel
    wr_req_grant      : std_logic;
    wr_full           : std_logic;
    wr_done           : std_logic;
    wr_err            : std_logic;
  end record ib_miso;

  type ib_mosi is record  --Output from injector_ctrl to bus master interface input
    -- Read channel
    rd_addr           : std_logic_vector(31 downto 0);
    rd_size           : std_logic_vector(11 downto 0);
    rd_fix_addr       : std_logic;
    rd_req            : std_logic;
    -- Write channel
    wr_addr           : std_logic_vector(31 downto 0);
    wr_size           : std_logic_vector(11 downto 0);
    wr_fix_addr       : std_logic;
    wr_req            : std_logic;
    wr_data           : std_logic_vector(127 downto 0);
  end record ib_mosi;

  -- APB bus types
  type apb_slave_in is record
    sel               : std_logic;
    en                : std_logic;
    addr              : std_logic_vector( 31 downto 0 );
    wr_en             : std_logic;
    wdata             : std_logic_vector( 31 downto 0 );
    irq               : std_logic;
  end record apb_slave_in;

  type apb_slave_out is record -- Lacks config, but it is added on the platform wrapper.
    rdata             : std_logic_vector( 31 downto 0 );
    irq               : std_logic;
  end record apb_slave_out;


-----------------------------------------------------------------------------
-- Injector configuration type
-----------------------------------------------------------------------------

  -- Injector configuration register
  type injector_config is record
    enable            : std_logic;  -- Injector core enable
    reset_sw          : std_logic;  -- Injector core software reset
    queue_mode_en     : std_logic;  -- Queue mode enable
    irq_prog_compl_en : std_logic;  -- Program completion interrupt enable
    irq_err_core_en   : std_logic;  -- Error interrupt enable for core errors
    irq_err_net_en    : std_logic;  -- Error interrupt enable for network errors
    freeze_irq_en     : std_logic;  -- Freeze injector at interruption       
  end record injector_config;


-----------------------------------------------------------------------------
-- DECODE/EXE control data types
-----------------------------------------------------------------------------

  -- Decoded descriptor type into a bit array of the submodules to enable for the operation.
  type submodule_bit is record
    delay_sub         : std_logic;
    read_sub          : std_logic;
    write_sub         : std_logic;
  end record submodule_bit;

  -- Decoded descriptor for READ and WRITE descriptor types
  type operation_rd_wr is record
    size_left         : unsigned(18 downto 0);  -- On DECODE, the descriptor size is increased by 1 obtain the total transfer size left.
    size_burst        : unsigned(18 downto 0);  -- Encoded burst transfer size of the next transaction.
    addr              : unsigned(31 downto 0);  -- Starting transaction address.
    addr_fix          : std_logic;              -- Transaction to be executed on a fixed address flag.
  end record operation_rd_wr;

  -- Decoded descriptor for DELAY descriptor type
  type operation_delay is record
    num_cycles        : unsigned(18 downto 0);
  end record operation_delay;

  -- Decoded descriptor for BRANCH descriptor type
  --type operation_branch is record
  --  not_flag          : std_logic;
  --  flags             : std_logic_vector(15 downto 0);
  --  loop_times        : std_logic_vector( 5 downto 0);
  --  branch_ptr        : std_logic_vector( 8 downto 0);
  --end record operation_branch;

  -- Type used to encapsulate connections from DECODE to EXE stages
  type bus_decode_exe is record
    subm_enable       : submodule_bit;    -- Decoded descriptor type for specific submodule start
    irq_desc          : std_logic;        -- Interruption flag for descriptor completion
    last_desc         : std_logic;        -- Last descriptor flag in injector program
    last_count        : std_logic;        -- Last iteration of the descriptor
    delay             : operation_delay;  -- Control data to execute DELAY operations
    rd_wr             : operation_rd_wr;  -- Control data to execute READ and WRITE operations
  end record bus_decode_exe;


  -------------------------------------------------------------------------------
  -- Subprograms declaration
  -------------------------------------------------------------------------------

  -- Computes the ceil log base two from an integer.
  -- This function is NOT for describing hardware, just to compute bus lengths and pre-synthesis stuff.
  function log_2(max_size : integer) return integer;
  

  -------------------------------------------------------------------------------
  -- Component declaration
  -------------------------------------------------------------------------------

  component injector_core is
    generic (
      -- Injector configuration
      PC_LEN            : integer range 2 to   10       :=    4;  -- Set the maximum number of programmable descriptor words to 2^PC_LEN
      CORE_DATA_WIDTH   : integer range 8 to 1024       :=   32;  -- Data width of the injector core. [Only power of 2s allowed]
      MAX_SIZE_BURST    : integer range 8 to 4096       := 1024;  -- Maximum number of bytes allowed at a burst transaction.
      DEFAULT_PROFILE   : std_logic_vector(31 downto 0) := (others => '0'); -- Default Network profile.
      ASYNC_RST         : boolean                       := TRUE   -- Allow asynchronous reset
    );
    port (
      rstn              : in  std_ulogic;                         -- Reset
      clk               : in  std_ulogic;                         -- Clock
      -- APB interface signals
      apbi              : in  apb_slave_in;                       -- APB slave input
      apbo              : out apb_slave_out;                      -- APB slave output
      -- Bus master signals
      ib_out            : out ib_mosi;                            -- Input to network interface
      ib_in             : in  ib_miso;                            -- Output from network interface
      network_profile   : out std_logic_vector(31 downto 0)       -- Network profile to apply during transaction requests
    );
  end component injector_core;


end package injector_pkg;

package body injector_pkg is

  -------------------------------------------------------------------------------
  -- Functions body
  -------------------------------------------------------------------------------

  -- Function used to compute bus lengths. DO NOT attempt to use it as 
  -- combinational logic, just to compute values pre-synthesis.
  function log_2(max_size : integer) return integer is
    variable res : integer := 0;
  begin
    while (2**res < max_size) and res < 31 loop
       res := res + 1;
    end loop;
    return res;
  end log_2;
    

end package body injector_pkg;

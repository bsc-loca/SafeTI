-----------------------------------------------------------------------------   
-- Entity:      injector_core
-- File:        injector_core.vhd
-- Author:      Francisco Fuentes, Oriol Sala
-- Description: Injector core entity.
------------------------------------------------------------------------------ 
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library safety;
use safety.injector_pkg.all;


--------------------------------------------------------------------------------------------------------------------------------
-- Entity of the injector core, that integrates the CONTROL, APB interface and pipeline realted FETCH, DECODE and EXE modules --
--------------------------------------------------------------------------------------------------------------------------------

entity injector_core is
  generic (
    -- Injector configuration
    PC_LEN            : integer range 2 to   10       :=    4;-- Set the maximum number of programmable descriptor words to 2^PC_LEN
    CORE_DATA_WIDTH   : integer range 8 to 1024       :=   32;-- Data width of the injector core. [Only power of 2s allowed]
    MAX_SIZE_BURST    : integer range 8 to 4096       := 1024;-- Maximum number of bytes allowed at a burst transaction.
    ASYNC_RST         : boolean                       := TRUE -- Allow asynchronous reset
  );
  port (
    rstn              : in  std_ulogic;                       -- Reset
    clk               : in  std_ulogic;                       -- Clock
    -- APB interface signals
    apbi              : in  apb_slave_in;                     -- APB slave input
    apbo              : out apb_slave_out;                    -- APB slave output
    -- Bus master signals
    bm_out            : out bm_mosi;                          -- Input to network interface
    bm_in             : in  bm_miso                           -- Output from network interface
  );
end entity injector_core;

architecture rtl of injector_core is

  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------

  -- Common signals
  signal enable_pipeline    : std_logic_vector(1 to 3); -- Enable reset for pipeline modules
  signal reset_pipeline     : std_logic_vector(1 to 3); -- Software reset for pipeline modules
  signal req_grant          : std_logic_vector(1 to 2); -- Data available for next stage flag
  signal req                : std_logic_vector(2 to 3); -- Stage prepared to read data from prior stage flag
  signal irq                : std_logic_vector(0 to 3); -- Interruption from/for pipeline stages
  signal pc_pipeline        : pipeline_pc_array;        -- PC of each pipeline stage
  signal state_pipeline     : pipeline_state_array;     -- State of each pipeline stage

  -- APB interface signals
  signal inj_config         : injector_config;
  signal prog_mem_wr_data   : std_logic_vector(31 downto 0);
  signal prog_mem_wr_en     : std_logic;

  -- FETCH signals
  signal descriptor         : desc_words;

  -- DECODE signals
  signal decoded_descriptor : bus_decode_exe;

  -- EXE signals
  signal program_end        : std_logic;
  signal descr_complete     : std_logic;
  signal irq_desc_compl_en  : std_logic;

  -- CONTROL signal
  signal ctrl_disable       : std_logic; -- Turn off injector signal

  -- External signals
  signal err_network        : std_logic_vector(0 to 1); -- Network error (0 = READ, 1 = WRITE)
  

  -----------------------------------------------------------------------------
  -- Component declaration
  -----------------------------------------------------------------------------

  -- APB interface
  component injector_apb is
    generic (
      PC_LEN          : integer                         := 4;   -- Length of PC register
      ASYNC_RST       : boolean                         := TRUE -- Allow asynchronous reset flag
    );
    port (
    -- External I/O
      rstn            : in  std_ulogic;                         -- Reset
      clk             : in  std_ulogic;                         -- Clock
      apbi            : in  apb_slave_in;                       -- APB slave input
      apbo            : out apb_slave_out;                      -- APB slave output
    -- Internal I/O
      -- Signals for CONTROL
      gen_config      : out injector_config;                    -- General injector configuration signals
      -- Signals for FETCH
      desc_word       : out std_logic_vector(31 downto 0);      -- Descriptor word input register from APB
      desc_word_wen   : out std_ulogic;                         -- Write enable for descriptor word input
      -- Signals from CONTROL
      disable         : in  std_ulogic;                         -- Turn off injector execution flag
      irq_flag        : in  std_ulogic                          -- Interruption flag for APB output
    );
  end component injector_apb;

  -- FETCH pipeline stage
  component injector_fetch is
    generic (
      PC_LEN          : integer                         := 4;   -- Length of PC register
      ASYNC_RST       : boolean                         := TRUE -- Allow asynchronous reset flag
    );
    port (
      -- External I/O
      rstn            : in  std_ulogic;                         -- Reset
      clk             : in  std_ulogic;                         -- Clock
      -- Internal I/O
      enable          : in  std_logic;                          -- Enable FETCH stage
      rst_sw          : in  std_logic;                          -- Software reset through APB
        -- Signals from APB registers
      desc_word_wr    : in  std_logic_vector(31 downto 0);      -- Descriptor word to be written on the Program Memory
      desc_word_wen   : in  std_logic;                          -- Write enable of a descriptor word write
        -- Signals for/from DECODE
      fetch_ready     : out std_logic;                          -- Descriptor ready to be read flag
      decode_read     : in  std_logic;                          -- Descriptor can be read flag from buffer
      pc              : out unsigned(PC_LEN - 1 downto 0);      -- PC of the word 0 descriptor fetched
      desc            : out desc_words;                         -- Descriptor words
        -- Debug signals
      irq             : out std_logic;                          -- Error interruption
      state           : out std_logic_vector(MAX_STATUS_LEN - 1 downto 0)
    );
  end component injector_fetch;

  -- DECODE pipeline stage
  component injector_decode is
    generic (
      PC_LEN            : integer                       := 4;   -- Length of PC register
      ASYNC_RST         : boolean                       := TRUE -- Allow asynchronous reset flag
    );
    port (
      -- External I/O
      rstn              : in  std_ulogic;                       -- Reset
      clk               : in  std_ulogic;                       -- Clock
      -- Internal I/O
      enable            : in  std_logic;                        -- Enable DECODE stage
      rst_sw            : in  std_logic;                        -- Software reset through APB
        -- Signals from/for FETCH
      fetch_ready       : in  std_logic;                        -- Descriptor ready to be read flag
      decode_read       : out std_logic;                        -- Descriptor can be read flag
      fetch_pc          : in  unsigned(PC_LEN - 1 downto 0);    -- PC of the fetched word 0 descriptor
      desc              : in  desc_words;                       -- Fetched descriptor words
        -- Signals for/from EXE
      decode_ready      : out std_logic;                        -- Decoded descriptor ready to be read
      exe_read          : in  std_logic;                        -- Decoded descriptor can be read
      exe_pc            : out unsigned(PC_LEN - 1 downto 0);    -- PC of the decoded descriptor
      exe_data          : out bus_decode_exe;                   -- Control signals for operation execution
        -- Debug signals
      irq               : out std_logic;                        -- Error interruption
      state             : out std_logic_vector(MAX_STATUS_LEN - 1 downto 0)
    );
  end component injector_decode;

  -- EXE pipeline stage
  component injector_exe is
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
  end component injector_exe;

  -- CONTROL combinational block
  component injector_control is
    port (
      -- Signals from/for APB interface
      apb_config        : in  injector_config;                  -- Injector configuration
      disable           : out std_logic;                        -- Disable injector flag
      irq_send          : out std_logic;                        -- Send interruption flag
      -- Pipeline control signals
      enable_pipeline   : out std_logic_vector(1 to 3);         -- Enable pipeline modules
      rst_sw_pipeline   : out std_logic_vector(1 to 3);         -- Reset pipeline modules
      irq_err_pipeline  : in  std_logic_vector(1 to 3);         -- Interruption from pipeline stages
      irq_err_network   : in  std_logic_vector(0 to 1);         -- Network interruptions (0 = READ, 1 = WRITE)
      -- EXE output signals
      exe_irq_desc_comp : in  std_logic;                        -- Interrupt enable for complete descriptor
      exe_desc_comp     : in  std_logic;                        -- Descriptor completed flag
      exe_program_comp  : in  std_logic                         -- Injector program complete flag
    );
  end component injector_control;

  
begin  -- rtl
  
  -----------------------------------------------------------------------------
  -- Assignments
  -----------------------------------------------------------------------------

  err_network <= bm_in.rd_err & bm_in.wr_err;


  -----------------------------------------------------------------------------
  -- Component instantiation
  -----------------------------------------------------------------------------

  -- APB interface
  apb : injector_apb
    generic map (
      PC_LEN            => PC_LEN,
      ASYNC_RST         => ASYNC_RST
    )
    port map (
    -- External I/O
      rstn              => rstn,
      clk               => clk,
      apbi              => apbi,
      apbo              => apbo,
    -- Internal I/O
      -- Signals for CONTROL
      gen_config        => inj_config,
      -- Signals for FETCH
      desc_word         => prog_mem_wr_data,
      desc_word_wen     => prog_mem_wr_en,
      -- Signals from CONTROL
      disable           => ctrl_disable,
      irq_flag          => irq(PL_APB)
  );

  -- FETCH pipeline stage
  fetch : injector_fetch
    generic map (
      PC_LEN            => PC_LEN,
      ASYNC_RST         => ASYNC_RST
    )
    port map (
    -- External I/O
      rstn              => rstn,
      clk               => clk,
    -- Internal I/O
      enable            => enable_pipeline(PL_FETCH),
      rst_sw            => reset_pipeline(PL_FETCH),
        -- Signals from APB registers
      desc_word_wr      => prog_mem_wr_data,
      desc_word_wen     => prog_mem_wr_en,
        -- Signals for/from DECODE
      fetch_ready       => req_grant(PL_FETCH),
      decode_read       => req(PL_DECODE),
      pc                => pc_pipeline(PL_FETCH),
      desc              => descriptor,
        -- Debug signals
      irq               => irq(PL_FETCH),
      state             => state_pipeline(PL_FETCH)
  );

  -- DECODE pipeline stage
  decode : injector_decode
    generic map (
      PC_LEN            => PC_LEN,
      ASYNC_RST         => ASYNC_RST
    )
    port map (
    -- External I/O
      rstn              => rstn,
      clk               => clk,
    -- Internal I/O
      enable            => enable_pipeline(PL_DECODE),
      rst_sw            => reset_pipeline(PL_DECODE),
        -- Signals from/for FETCH
      fetch_ready       => req_grant(PL_FETCH),
      decode_read       => req(PL_DECODE),
      fetch_pc          => pc_pipeline(PL_FETCH),
      desc              => descriptor,
        -- Signals for/from EXE
      decode_ready      => req_grant(PL_DECODE),
      exe_read          => req(PL_EXE),
      exe_pc            => pc_pipeline(PL_DECODE),
      exe_data          => decoded_descriptor,
        -- Debug signals
      irq               => irq(PL_DECODE),
      state             => state_pipeline(PL_DECODE)
  );

  -- EXE pipeline stage
  exe : injector_exe
    generic map (
      PC_LEN            => PC_LEN,
      CORE_DATA_WIDTH   => CORE_DATA_WIDTH,
      MAX_SIZE_BURST    => MAX_SIZE_BURST,
      ASYNC_RST         => ASYNC_RST
    )
    port map (
    -- External I/O
      rstn              => rstn,
      clk               => clk,
      bm_in             => bm_in,
      bm_out            => bm_out,
    -- Internal I/O
      enable            => enable_pipeline(PL_EXE),
      rst_sw            => reset_pipeline(PL_EXE),
        -- Signals from/for DECODE
      decode_ready      => req_grant(PL_DECODE),
      exe_ready         => req(PL_EXE),
      decode_pc         => pc_pipeline(PL_DECODE),
      decode_data       => decoded_descriptor,
        -- Control signals
      irq_desc_comp     => irq_desc_compl_en,
      desc_comp         => descr_complete,
      program_comp      => program_end,
        -- Debug signals
      pc_ongoing        => pc_pipeline(PL_EXE),
      state             => state_pipeline(PL_EXE)
  );

  -- CONTROL combinational block
  control : injector_control
    port map (
      -- Signals from/for APB interface
      apb_config        => inj_config,
      disable           => ctrl_disable,
      irq_send          => irq(PL_APB),
      -- Pipeline control signals
      enable_pipeline   => enable_pipeline,
      rst_sw_pipeline   => reset_pipeline,
      irq_err_pipeline  => irq(PL_FETCH to PL_EXE),
      irq_err_network   => err_network,
      -- EXE output signals
      exe_irq_desc_comp => irq_desc_compl_en,
      exe_desc_comp     => descr_complete,
      exe_program_comp  => program_end
  );


end architecture rtl;

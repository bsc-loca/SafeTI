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
  -- User defined parameters START --

    -- APB bus generics
    constant APB_SLAVE_NMAX     : integer :=  16;   -- Max number of slaves at APB bus
    constant APB_IRQ_NMAX       : integer :=  32;   -- Max number of interrupts at APB bus
    constant APB_TEST_WIDTH     : integer :=   4;   -- apb_slave_in test in enable (tinen)

  -- User defined parameters END --

-------------------------------------------------------------------------------
-- Parametric constants 
-------------------------------------------------------------------------------

  constant MAX_DESC_LEN     : integer := 2; -- Maximum number of words for descriptors.
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


-------------------------------------------------------------------------------
-- Common types 
-------------------------------------------------------------------------------

  -- Descriptor words type
  type desc_words is array (0 to MAX_DESC_LEN - 1) of std_logic_vector(31 downto 0);
  

-------------------------------------------------------------------------------
-- External I/O signaling types 
-------------------------------------------------------------------------------

  -- BM bus types
  type bm_miso is record  --Input to injector_ctrl from bus master interface output
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
  end record bm_miso;

  type bm_mosi is record  --Output from injector_ctrl to bus master interface input
    -- Read channel
    rd_addr           : std_logic_vector(31 downto 0);
    rd_size           : std_logic_vector(11 downto 0);
    rd_req            : std_logic;
    -- Write channel
    wr_addr           : std_logic_vector(31 downto 0);
    wr_size           : std_logic_vector(11 downto 0);
    wr_req            : std_logic;
    wr_data           : std_logic_vector(127 downto 0);
  end record bm_mosi;

  -- Reset value for Bus Master interface signals
  constant RESET_BM_MISO : bm_miso := (
    -- Read channel
    rd_data           => (others => '0'),
    rd_req_grant      => '0',
    rd_valid          => '0',
    rd_done           => '0',
    rd_err            => '0',
    -- Write channel
    wr_req_grant      => '0',
    wr_full           => '0',
    wr_done           => '0',
    wr_err            => '0'
  );

  constant RESET_BM_MOSI : bm_mosi := (
    -- Read channel
    rd_addr           => (others => '0'),
    rd_size           => (others => '0'),
    rd_req            => '0',
    -- Write channel
    wr_addr           => (others => '0'),
    wr_size           => (others => '0'),
    wr_req            => '0',
    wr_data           => (others => '0')
  );  

  -- APB bus types
  type apb_slave_in is record
    sel               : std_logic_vector ( 0 to APB_SLAVE_NMAX-1 );
    en                : std_logic;
    addr              : std_logic_vector ( 31 downto 0 );
    wr_en             : std_logic;
    wdata             : std_logic_vector ( 31 downto 0 );
    irq               : std_logic_vector ( APB_IRQ_NMAX-1   downto 0 );
    ten               : std_logic;
    trst              : std_logic;
    scnen             : std_logic;
    touten            : std_logic;
    tinen             : std_logic_vector ( APB_TEST_WIDTH-1 downto 0 );
  end record apb_slave_in;

  type apb_slave_out is record -- Lacks config, but it is added on the platform wrapper.
    rdata             : std_logic_vector( 31 downto 0 );
    irq               : std_logic_vector( 31 downto 0 );
    index             : integer range 0 to 15;
  end record apb_slave_out;

-----------------------------------------------------------------------------
-- Injector configuration type
-----------------------------------------------------------------------------

  -- Injector configuration register
  type injector_config is record
    en                : std_ulogic;  -- Injector core enable
    rst               : std_ulogic;  -- Injector core reset
    qmode             : std_ulogic;  -- Queue mode
    freeze_irq_en     : std_ulogic;  -- Freeze injector at interruption
    irq_prog_compl_en : std_ulogic;  -- Program completion interrupt enable
    irq_err_en        : std_ulogic;  -- Error interrupt enable
  end record injector_config;


-----------------------------------------------------------------------------
-- DECODE/EXE control data types
-----------------------------------------------------------------------------

  -- Decoded descriptor type into a bit array of the submodules to enable for the operation.
  type submodule_enable is record
    delay_en          : std_logic;
    read_en           : std_logic;
    write_en          : std_logic;
  end record submodule_enable;

  -- Decoded descriptor for READ and WRITE descriptor types
  type operation_rd_wr is record
    size              : unsigned(19 downto 0);  -- On DECODE, the descriptor size is increased by 1 to set the real size.
    addr              : std_logic_vector(31 downto 0);-- Starting transaction address.
    addr_fix          : std_logic;                    -- Transaction to be executed on a fixed address flag.
  end record operation_rd_wr;

  -- Decoded descriptor for DELAY descriptor type
  type operation_delay is record
    num_cycles        : unsigned(19 downto 0);  -- On DECODE, the descriptor size is increased by 1 to set the real delay.
  end record operation_delay;

  -- Decoded descriptor for BRANCH descriptor type
  type operation_branch is record
    not_flag          : std_logic;
    flags             : std_logic_vector(30 downto 15);
    loop_times        : std_logic_vector(14 downto  9);
    branch_ptr        : std_logic_vector( 8 downto  0);
  end record operation_branch;


  -------------------------------------------------------------------------------
  -- Types required by subprograms and components
  -------------------------------------------------------------------------------

  type array_integer          is array (natural range <>) of integer;

  -------------------------------------------------------------------------------
  -- Subprograms
  -------------------------------------------------------------------------------
  function find_burst_size(fixed_addr       : std_logic;
                           max_bsize        : integer;
                           bm_bytes         : integer;
                           total_size       : std_logic_vector(19 downto 0)
                           ) return std_logic_vector;

  -- Computes the ceil log base two from an integer. This function is NOT for describing hardware, just to compute bus lengths and that.
  function log_2            (max_size         : integer) return integer;

  -- Returns maximum value from an array of integers.
  function max              (A : array_integer) return integer;

  -- Unsigned addition and subtraction functions between std vectors and integers, returning a vector of len lenght
  function add_vector       (A, B : std_logic_vector; len : natural) return std_logic_vector;
  function sub_vector       (A, B : std_logic_vector; len : natural) return std_logic_vector;
  function add_vector       (A : std_logic_vector; B : integer; len : natural) return std_logic_vector;
  function sub_vector       (A : std_logic_vector; B : integer; len : natural) return std_logic_vector;
  function sub_vector       (A : integer; B : std_logic_vector; len : natural) return std_logic_vector;

  -- OR_REDUCE substitude function, it just provides a low delay OR of all the bits from a std_logic_vector
  function or_vector        (vect : std_logic_vector) return std_logic;
  
  -------------------------------------------------------------------------------
  -- Components
  -------------------------------------------------------------------------------

  -- INJECTOR APB interface
  component injector_apb is
    generic (
      pindex            : integer                 := 0;
      paddr             : integer                 := 0;
      pmask             : integer                 := 16#FF8#;
      pirq              : integer                 := 0;
      mem_Ndesc         : integer range 1 to 512  := 16;
      ASYNC_RST         : boolean                 := FALSE
    );
    port (
      rstn              : in  std_ulogic;
      clk               : in  std_ulogic;
      apbi              : in  apb_slave_in;
      apbo              : out apb_slave_out;
      gen_config_out    : out injector_config;
      active            : out std_ulogic;
      err_status        : out std_ulogic;
      irq_flag_sts      : in  std_ulogic;
      curr_desc_in      : in  curr_des_out;
      curr_desc_ptr     : in  std_logic_vector(31 downto 0);
      sts_in            : in  status_out;
      desc_mem_out      : out descriptor_memory
    );
  end component injector_apb;

  -- Control Module
  component injector_ctrl is
    generic (
      dbits             : integer range  8 to 1024  :=   32;
      mem_Ndesc         : integer range  1 to  512  :=   16;
      ASYNC_RST         : boolean                   := FALSE
    );
    port (
      rstn              : in  std_ulogic;
      clk               : in  std_ulogic;
      gen_config        : in  injector_config;
      active            : in  std_ulogic;
      err_status        : in  std_ulogic;
      curr_desc_out     : out curr_des_out;
      curr_desc_ptr     : out std_logic_vector(31 downto 0);
      status            : out status_out;
      irq_flag_sts      : out std_ulogic;
      bm_in             : in  bm_miso;
      bm_out            : out bm_mosi;
      read_if_bm_in     : in  bm_mosi;
      read_if_bm_out    : out bm_miso;
      write_if_bm_in    : in  bm_mosi;
      write_if_bm_out   : out bm_miso;
      desc_ctrl_out     : out descriptor_control;
      desc_actaddr_out  : out descriptor_actionaddr;
      desc_branch_out   : out descriptor_branch;
      ctrl_rst          : out std_ulogic;
      err_sts_out       : out std_ulogic;
      read_if_start     : out std_ulogic;
      read_if_sts_in    : in  d_ex_sts_out;
      write_if_sts_in   : in  d_ex_sts_out;
      write_if_start    : out std_logic;
      delay_if_sts_in   : in  d_ex_sts_out;
      delay_if_start    : out std_logic;
      desc_mem          : in descriptor_memory
    );
  end component injector_ctrl;

  -- WRITE_IF
  component injector_write_if is
    generic (
      dbits             : integer range  8 to 1024  :=   32;
      MAX_SIZE_BURST    : integer range  8 to 4096  := 1024;
      ASYNC_RST         : boolean                   := FALSE
    );
    port (
      rstn              : in  std_ulogic;
      clk               : in  std_ulogic;
      ctrl_rst          : in  std_ulogic;
      err_sts_in        : in  std_ulogic;
      write_if_start    : in  std_ulogic;
      desc_ctrl         : in  descriptor_control;
      desc_actaddr      : in  descriptor_actionaddr;
      status_out        : out d_ex_sts_out;
      write_if_bmi      : in  bm_miso;
      write_if_bmo      : out bm_mosi
    );
   end component injector_write_if;

   -- READ_IF
  component injector_read_if is
    generic (
      dbits             : integer range  8 to 1024  :=   32;
      MAX_SIZE_BURST    : integer range  8 to 4096  := 1024;
      ASYNC_RST         : boolean                   := FALSE
    );
    port (
      rstn              : in  std_ulogic;
      clk               : in  std_ulogic;
      ctrl_rst          : in  std_ulogic;
      err_sts_in        : in  std_ulogic;
      read_if_start     : in  std_ulogic;
      desc_ctrl         : in  descriptor_control;
      desc_actaddr      : in  descriptor_actionaddr;
      status_out        : out d_ex_sts_out;
      read_if_bmi       : in  bm_miso;
      read_if_bmo       : out bm_mosi
    );
  end component injector_read_if;

  -- DELAY_IF
  component injector_delay_if is
    generic (
      ASYNC_RST         : boolean                   := FALSE
    );
    port (
      rstn              : in  std_ulogic;
      clk               : in  std_ulogic;
      ctrl_rst          : in  std_ulogic;
      err_sts_in        : in  std_ulogic;
      delay_if_start    : in  std_ulogic;
      desc_ctrl         : in  descriptor_control;
      desc_branch       : in  descriptor_branch;
      status_out        : out d_ex_sts_out
    );
  end component injector_delay_if;

  -- Injector core
  component injector is
    generic (
      mem_Ndesc       : integer range 1 to  512           :=   16;
      dbits           : integer range 8 to 1024           :=   32;
      MAX_SIZE_BURST  : integer range 8 to 4096           := 1024;
      pindex          : integer                           :=    0;
      paddr           : integer                           :=    0;
      pmask           : integer                           := 16#FFF#;
      pirq            : integer range 0 to APB_IRQ_NMAX-1 :=    0;
      ASYNC_RST       : boolean                           := FALSE
    );
    port (
      rstn            : in  std_ulogic;
      clk             : in  std_ulogic;
      apbi            : in  apb_slave_in;
      apbo            : out apb_slave_out;
      bm0_mosi        : out bm_mosi;
      bm0_miso        : in  bm_miso
    );
  end component injector;

  -- INJECTOR AHB top level interface
  component injector_ahb is
    generic (
      dbits           : integer range 8 to 1024           :=   32;
      MAX_SIZE_BURST  : integer range 8 to 1024           := 1024;
      pindex          : integer                           :=    0;
      paddr           : integer                           :=    0;
      pmask           : integer                           := 16#FFF#;
      pirq            : integer range 0 to APB_IRQ_NMAX-1 :=    0;
      hindex          : integer                           :=    0;
      ASYNC_RST       : boolean                           := FALSE
    );
    port (
      rstn            : in  std_ulogic;
      clk             : in  std_ulogic;
      apbi            : in  apb_slave_in;
      apbo            : out apb_slave_out;
      bm_mosi         : out bm_mosi;
      bm_miso         : in  bm_miso
    );
  end component injector_ahb;
  

  -------------------------------------------------------------------------------
  -- Procedures
  -------------------------------------------------------------------------------


end package injector_pkg;

package body injector_pkg is

  -- Function to determine the burst size based on maximum burst limit
  function find_burst_size(
    fixed_addr          : std_logic;
    max_bsize           : integer;
    bm_bytes            : integer;
    total_size          : std_logic_vector(19 downto 0)
  ) return std_logic_vector is
    variable temp       : integer;
    variable burst_size : std_logic_vector(log_2(max_bsize) downto 0);
    variable total_int  : integer;
  begin
    total_int := to_integer(unsigned(total_size));
    -- Limit the burst burst size by maximum burst length
    if(fixed_addr = '1') then
      if(total_int < bm_bytes) then  -- less than BM data bus bytes
        temp := total_int;
      else
        temp := bm_bytes;
      end if;
    elsif (total_int > max_bsize) then
      temp := max_bsize;
    else
      temp := total_int;
    end if;
    burst_size := std_logic_vector(to_unsigned(temp, burst_size'length)); 

    return burst_size;
  end find_burst_size;

  -- Maximum integer from an array of integers.
  function max(A : array_integer) return integer is
    variable temp : integer := 0;
    variable k : integer := 0;
  begin
    for k in A'range loop
      if(A(k) > temp) then
        temp := A(k);
      end if;
    end loop;
    return temp;
  end max;

  -- Addition function between std_logic_vectors, outputs with length assigned
  function add_vector(
    A, B : std_logic_vector;
    len : natural) 
  return std_logic_vector is
    variable res : std_logic_vector(max((len, A'length, B'length)) - 1 downto 0);
  begin
    res := std_logic_vector(unsigned(A) + resize(unsigned(B), res'length));
    return res(len - 1 downto 0);
  end add_vector;

  -- Addition function between std_logic_vector and integer, outputs with length assigned
  function add_vector(
    A : std_logic_vector;
    B : integer;
    len : natural) 
  return std_logic_vector is
    variable res : std_logic_vector(max((len, A'length)) - 1 downto 0);
  begin
    res := std_logic_vector(resize(unsigned(A), res'length) + to_unsigned(B, res'length));
    return res(len - 1 downto 0);
  end add_vector;

  -- Subtract function between std_logic_vectors, outputs with length assigned
  function sub_vector(
    A, B : std_logic_vector;
    len : natural) 
  return std_logic_vector is
    variable res : std_logic_vector(max((len, A'length, B'length)) - 1 downto 0);
  begin
    res := std_logic_vector(resize(unsigned(A), res'length) - resize(unsigned(B), res'length));
    return res(len - 1 downto 0);
  end sub_vector;

  -- Subtract function between std_logic_vector and integer, outputs with length assigned
  function sub_vector(
    A : std_logic_vector;
    B : integer;
    len : natural) 
  return std_logic_vector is
    variable res : std_logic_vector(max((len, A'length)) - 1 downto 0);
  begin
    res := std_logic_vector(resize(unsigned(A), res'length) - to_unsigned(B, res'length));
    return res(len - 1 downto 0);
  end sub_vector;

  -- Subtract function between integer and std_logic_vector, outputs with length assigned
  function sub_vector(
    A : integer;
    B : std_logic_vector;
    len : natural) 
  return std_logic_vector is
    variable res : std_logic_vector(max((len, B'length)) - 1 downto 0);
  begin
    res := std_logic_vector(to_unsigned(A, res'length) - resize(unsigned(B), res'length));
    return res(len - 1 downto 0);
  end sub_vector;

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

  function or_vector(vect : std_logic_vector) return std_logic is
    variable wool  : std_logic;
  begin
    wool := '0';
    for i in vect'range loop
      wool := wool or vect(i);
    end loop;
    return wool;
  end or_vector;


end package body injector_pkg;

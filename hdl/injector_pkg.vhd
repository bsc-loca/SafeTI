------------------------------------------------------------------------------
-- Package:     injector_pkg
-- File:        injector_pkg.vhd
-- Author:      Oriol Sala
-- Description: Internal package for Traffic Injector
------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


package injector_pkg is
  -- User parameters START --

    -- APB bus generics
    constant APB_SLAVE_NMAX     : integer :=  16;   -- Max number of slaves at APB bus
    constant APB_IRQ_NMAX       : integer :=  32;   -- Max number of interrupts at APB bus
    constant APB_TEST_WIDTH     : integer :=   4;   -- apb_slave_in test in enable (tinen)

    -- AHB bus generics
    constant AHB_MASTER_NMAX    : integer :=  16;   -- Max number of masters at AHB bus
    constant AHB_IRQ_NMAX       : integer :=  32;   -- Max number of interrupts at APB bus
    constant AHB_DATA_WIDTH     : integer :=  32;   -- Data's width at AHB bus
    constant AHB_TEST_WIDTH     : integer :=   4;   -- ahb_master_in testin width

    -- Common generics
    constant numTech            : integer :=  67;   -- Target technology
    constant typeTech           : integer :=   0;

  -- User parameters END --

-------------------------------------------------------------------------------
-- Parametric constants
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Labels
-------------------------------------------------------------------------------

-- Descriptor types (act_type)
constant READ_OP    : std_logic_vector(2 downto 0) := "000";
constant WRITE_OP   : std_logic_vector(2 downto 0) := "001";
constant DELAY_OP   : std_logic_vector(2 downto 0) := "010";
constant META_OP    : std_logic_vector(2 downto 0) := "111";

-------------------------------------------------------------------------------
-- Types and records
-------------------------------------------------------------------------------

  -- BM specific types
  type bm_miso is record  --Input to injector_ctrl from bus master interface output
    -- Read channel
    rd_data       : std_logic_vector(127 downto 0);
    rd_req_grant  : std_logic;
    rd_valid      : std_logic;
    rd_done       : std_logic;
    rd_err        : std_logic;
    -- Write channel
    wr_req_grant  : std_logic;
    wr_full       : std_logic;
    wr_done       : std_logic;
    wr_err        : std_logic;
  end record;

  type bm_mosi is record  --Output from injector_ctrl to bus master interface input
    -- Read channel
    rd_addr       : std_logic_vector(31 downto 0);
    rd_size       : std_logic_vector(11 downto 0);
    rd_req        : std_logic;
    -- Write channel
    wr_addr       : std_logic_vector(31 downto 0);
    wr_size       : std_logic_vector(11 downto 0);
    wr_req        : std_logic;
    wr_data       : std_logic_vector(127 downto 0);
  end record;

  -- Reset value for Bus Master interface signals
  constant BM_MISO_RST : bm_miso := (
    rd_data       => (others => '0'),
    rd_req_grant  => '0',
    rd_valid      => '0',
    rd_done       => '0',
    rd_err        => '0',
    -- Write channel
    wr_req_grant  => '0',
    wr_full       => '0',
    wr_done       => '0',
    wr_err        => '0'
    );

  constant BM_MOSI_RST : bm_mosi := (
    -- Read channel
    rd_addr   => (others => '0'),
    rd_size   => (others => '0'),
    rd_req    => '0',
    -- Write channel
    wr_addr   => (others => '0'),
    wr_size   => (others => '0'),
    wr_req    => '0',
    wr_data   => (others => '0')
    );  

  type apb_slave_in_type is record
    sel             : std_logic_vector ( 0 to APB_SLAVE_NMAX-1 );
    en              : std_logic;
    addr            : std_logic_vector ( 31 downto 0 );
    wr_en           : std_logic;
    wdata           : std_logic_vector ( 31 downto 0 );
    irq             : std_logic_vector ( APB_IRQ_NMAX-1   downto 0 );
    ten             : std_logic;
    trst            : std_logic;
    scnen           : std_logic;
    touten          : std_logic;
    tinen           : std_logic_vector ( APB_TEST_WIDTH-1 downto 0 );
  end record;

  type apb_slave_out_type is record
    rdata           : std_logic_vector( 31 downto 0 );
    irq             : std_logic_vector( 31 downto 0 );
    config          : std_logic_vector( 31 downto 0 );
    index           : integer range 0 to 15;
  end record;
  
  type ahb_master_in_type is record
    grant           : std_logic_vector(  0 to AHB_MASTER_NMAX-1 );
    ready           : std_ulogic;
    resp            : std_logic_vector(  1 downto 0 );
    rdata           : std_logic_vector( AHB_DATA_WIDTH-1 downto 0 );
    irq             : std_logic_vector( AHB_IRQ_NMAX-1   downto 0 );
    testen          : std_ulogic;
    testrst         : std_ulogic;
    scanen          : std_ulogic;
    testoen         : std_ulogic;
    testin          : std_logic_vector( AHB_TEST_WIDTH-1 downto 0 );
    endian          : std_ulogic;
  end record;

  type ahb_master_out_type is record
    busreq          : std_ulogic;
    lock            : std_ulogic;
    trans           : std_logic_vector(  1 downto 0 );
    addr            : std_logic_vector( 31 downto 0 );
    wr_en           : std_ulogic;
    size            : std_logic_vector(  2 downto 0 );
    burst           : std_logic_vector(  2 downto 0 );
    prot            : std_logic_vector(  3 downto 0 );
    wdata           : std_logic_vector( AHB_DATA_WIDTH-1 downto 0 );
    irq             : std_logic_vector( AHB_IRQ_NMAX-1 downto 0 );
    config          : std_logic_vector( 31 downto 0 );
    index           : integer range 0 to AHB_MASTER_NMAX-1;
  end record;

  -- status out type 
  type status_out_type is record
    err             : std_ulogic;       -- General error status. Asserted along with other errors
    decode_desc_err : std_ulogic;       -- Error while decoding descriptor 
    rd_desc_err     : std_ulogic;       -- Error while reading desc from BM IF
    rd_data_err     : std_ulogic;       -- Error while reading data from BM IF
    wr_data_err     : std_ulogic;       -- Error while writing data to BM IF
    state           : std_logic_vector(4 downto 0);  -- Current operating state of Injector 
    ongoing         : std_ulogic;       -- Ongoing Injector operation
    desc_comp       : std_ulogic;       -- Descriptor completed flag
    kick            : std_ulogic;       -- Kick flag
    rd_nxt_ptr_err  : std_ulogic;       -- Error during re-reading des.nxt_ptr field on a kick request
    comp            : std_ulogic;       -- all desc are completed
    count           : std_logic_vector(5 downto 0);       -- Current transaction repetition count value
  end record;

   -- reset value for status out type
  constant STATUS_OUT_RST : status_out_type := (
    err             => '0',
    decode_desc_err => '0',
    rd_desc_err     => '0',
    rd_data_err     => '0',
    wr_data_err     => '0',
    state           => (others => '0'),
    ongoing         => '0',
    desc_comp       => '0',
    kick            => '0',
    rd_nxt_ptr_err  => '0',
    comp            => '0',
    count           => (others => '0')
    );

  -- Data execution status out type 
  type d_ex_sts_out_type is record
    read_if_err   : std_ulogic;                    -- Error while reading data from BM IF
    write_if_err  : std_ulogic;                    -- Error while writing data to BM IF
    delay_if_err  : std_ulogic;                    -- Error while executing delay IF
    state         : std_logic_vector(4 downto 0);  -- Current state of data execution in READ_IF/WRITE_IF  
    operation     : std_ulogic;                    -- Ongoing execution
    comp          : std_ulogic;                    -- read_if operation completed
  end record;

   -- reset value for status out type
  constant D_EX_STS_RST : d_ex_sts_out_type := (
    read_if_err   => '0',
    write_if_err  => '0',
    delay_if_err  => '0',
    state         => (others => '0'),
    operation     => '0',
    comp          => '0'
    );  

  -- Current descriptor out type
  type curr_des_out_type is record
    dbg_ctrl     : std_logic_vector (31 downto 0);  -- Current descriptor ctrl field
    dbg_nxt      : std_logic_vector (31 downto 0);  -- Next_desc pointer of current descriptor
    dbg_dst_addr : std_logic_vector (31 downto 0);  -- dest_addr
    dbg_src_addr : std_logic_vector (31 downto 0);  -- src_addr
    dbg_sts      : std_logic_vector (31 downto 0);  -- status
  end record;

  -- Reset value for read_if/WRITE_IF current descriptor out type
  constant CUR_DES_OUT_RST : curr_des_out_type := (
    dbg_ctrl     => (others => '0'),
    dbg_nxt      => (others => '0'),
    dbg_dst_addr => (others => '0'),
    dbg_src_addr => (others => '0'),
    dbg_sts      => (others => '0')
    );

  -----------------------------------------------------------------------------
  -- Injector descriptor types
  -----------------------------------------------------------------------------

   -- Next descriptor structure
  type nxt_des_type is record
    ptr  : std_logic_vector(31 downto 0);  -- Next descriptor pointer 
    last : std_ulogic;                     -- Last descriptor
  end record;

  -- Reset value for next descriptor structure
  constant NXT_DES_RST : nxt_des_type := (
    ptr  => (others => '0'),
    last => '0'
    );  

  -- Data descriptor control 
  type data_dsc_ctrl_type is record
    en           : std_ulogic;                    -- Enable descriptor 
    desc_type    : std_logic_vector(2 downto 0);  -- Type of descriptor. 0 for data 
    irq_en       : std_ulogic;                    -- Interrupt enable on descriptor completion 
    src_fix_adr  : std_ulogic;                    -- Fetch data from fixed address
    dest_fix_adr : std_ulogic;                    -- Write data to fixed address
    count_size   : std_logic_vector(5 downto 0);  -- Transaction repetition counter size (bits resolution)
    size         : std_logic_vector(19 downto 0); -- Real size of data to be transfered
  end record;

  -- Reset value for data descriptor control
  constant DATA_DSC_CTRL_RST : data_dsc_ctrl_type := (
    en           => '0',
    desc_type    => (others => '0'),
    irq_en       => '0',
    src_fix_adr  => '0',
    dest_fix_adr => '0',
    count_size   => (others => '0'),
    size         => (others => '0')
    );

  -- Data descriptor structure 
  type data_dsc_strct_type is record
    ctrl      : data_dsc_ctrl_type;             -- Data descriptor control
    nxt_des   : nxt_des_type;                   -- Next descriptor pointer
    dest_addr : std_logic_vector(31 downto 0);  -- Address where data is to be written
    src_addr  : std_logic_vector(31 downto 0);  -- Address from where data is to be fetched    
  end record;

  -- Reset value for data descriptor structure
  constant DATA_DSC_STRCT_RST : data_dsc_strct_type := (
    ctrl      => DATA_DSC_CTRL_RST,
    nxt_des   => NXT_DES_RST,
    dest_addr => (others => '0'),
    src_addr  => (others => '0')
    );

  -----------------------------------------------------------------------------
  -- Injector register types
  -----------------------------------------------------------------------------

  -- Injector control register
  type injector_ctrl_reg_type is record
    en      : std_ulogic;  -- Injector core enable
    rst     : std_ulogic;  -- Injector core reset
    kick    : std_ulogic;  -- Kick
    qmode   : std_ulogic;  -- Queue mode   
    irq_en  : std_ulogic;  -- Global Interrupt enable
    irq_err : std_ulogic;  -- Enable interrupt on errors  
  end record;

  -- Reset value for Injector control register
  constant INJECTOR_CTRL_REG_RST : injector_ctrl_reg_type := (
    en      => '0',
    rst     => '0',
    kick    => '0',
    qmode   => '0',
    irq_en  => '0',
    irq_err => '0'
    );

  -- Injector status register : 
  type injector_sts_reg_type is record
    comp                 : std_ulogic;                    -- Completed all descriptors successfully
    err                  : std_ulogic;                    -- Error on channel    
    ongoing              : std_ulogic;                    -- Ongoing transaction operation
    irq_flag             : std_ulogic;                    -- Interrupt flag            
    decode_err           : std_ulogic;                    -- read_if error during desc decoding
    rd_desc_err          : std_ulogic;                    -- Error from read_if BMI during desc reading
    state                : std_logic_vector(4 downto 0);  -- M2b state in which error captured
    read_if_rd_data_err  : std_ulogic;                    -- Error from read_if BMI during data fetching         
    write_if_wr_data_err : std_ulogic;                    -- Error from write_if BMI during data writing
    kick_pend            : std_ulogic;                    -- Pending Kick request flag
    rd_nxt_ptr_err       : std_ulogic;                    -- Error during re-reading des.nxt_ptr field on a kick request
    count                : std_ulogic_vector(5 downto 0); -- Counter value of current transaction repetition
    active               : std_ulogic;                    -- Core enabled after reset
  end record;

  -- Reset value for Injector status register
  constant INJECTOR_STS_REG_RST : injector_sts_reg_type := (
    comp                    => '0',
    err                     => '0',
    ongoing                 => '0',
    irq_flag                => '0',
    decode_err              => '0',
    rd_desc_err             => '0',
    state                   => (others => '0'),
    read_if_rd_data_err     => '0',
    write_if_wr_data_err    => '0',
    kick_pend               => '0',
    rd_nxt_ptr_err          => '0',
    count                   => (others => '0'), 
    active                  => '0'
    );

  -- INJECTOR capability register (read only)
  -- Current descriptor control field for debug. 
  type cur_desc_ctrl_type is record
    ctrl : std_logic_vector(31 downto 0);
  end record;

  -- Reset value for current descriptor control field
  constant CUR_DESC_CTRL_RST : cur_desc_ctrl_type := (
    ctrl => (others => '0')
    );

  -- Current descriptor's nxt_des_ptr field for debug. 
  type cur_nxt_des_type is record
    ptr : std_logic_vector(31 downto 0);
  end record;

  -- Reset value for current descriptor's nxt_des_ptr field 
  constant CUR_NXT_DES_RST : cur_nxt_des_type := (
    ptr => (others => '0')
    );

  -- Current descriptor's d_des:dest_addr
  type cur_des_dst_addr_trype is record
    ptr : std_logic_vector(31 downto 0);
  end record;

  -- Reset value for current descriptor's f_nxt_des_ptr field 
  constant CUR_FNXT_DES_RST : cur_des_dst_addr_trype := (
    ptr => (others => '0')
    );  

  -- Current descriptor's conditional address field for debug. 
  -- c_des:cond_addr, d_des:src_addr
  type cur_des_src_addr is record
    addr : std_logic_vector(31 downto 0);
  end record;

  -- Reset value for current descriptor's address field 
  constant CUR_COND_ADR_RST : cur_des_src_addr := (
    addr => (others => '0')
    );

  -- Current descriptor's status field for debug. 
  type cur_desc_sts_type is record
    sts : std_logic_vector(31 downto 0);
  end record;

  -- Reset value for current descriptor's sts field 
  constant CUR_DESC_STS_RST : cur_desc_sts_type := (
    sts => (others => '0')
    );   

  -- Current descriptor pointer for debug.
  type cur_desc_ptr_type is record
    ptr : std_logic_vector(31 downto 0);
  end record;

  -- Reset value for current descriptor's sts field 
  constant CUR_DESC_PTR_RST : cur_desc_ptr_type := (
    ptr => (others => '0')
    ); 

  -- Injector combined register type
  type injector_reg_type is record
    ctrl     : injector_ctrl_reg_type;   -- Control register
    sts      : injector_sts_reg_type;    -- Status register
  end record;

  -- Reset value for reg_type
  constant INJECTOR_REG_RST : injector_reg_type := (
    ctrl     => INJECTOR_CTRL_REG_RST,
    sts      => INJECTOR_STS_REG_RST
    );

  ----------------

  -- Descriptor control field (common)
  type descriptor_control is record
    size      : std_logic_vector(32 downto 13); -- Internally, the size is increased by 1 to indicate the
    count     : std_logic_vector(12 downto  7); -- real transfer size anywhere else than on ctrl and debug.
    reserved  : std_logic;
    type_spec : std_logic;
    irqe      : std_logic;
    act_type  : std_logic_vector( 3 downto  1);
    last      : std_logic;
  end record;

  type descriptor_actionaddr is record
    addr      : std_logic_vector(31 downto  0);
  end record;

  -- Descriptor word (basic)
  type descriptor_branch is record
    not_flag  : std_logic;
    flags     : std_logic_vector(30 downto 15);
    loop_times: std_logic_vector(14 downto  9);
    branch_ptr: std_logic_vector( 8 downto  0);
  end record;

  -- Reset constant for descriptor's control word
  constant DESCRIPTOR_CTRL_RST : descriptor_control := (
    size      => (others => '0'),
    count     => (others => '0'),
    reserved  => '0',
    type_spec => '0',
    irqe      => '0',
    act_type  => (others => '0'),
    last      => '0'
  );

  -- Reset constant for descriptor's action address word
  constant DESCRIPTOR_ACTADDR_RST : descriptor_actionaddr := (
    addr      => (others => '0')
  );

  -- Reset constant for descriptor's action address word
  constant DESCRIPTOR_BRANCH_RST : descriptor_branch := (
    not_flag  => '0',
    flags     => (others => '0'),
    loop_times=> (others => '0'),
    branch_ptr=> (others => '0')
  );

  -------------------------------------------------------------------------------
  -- Types required by subprograms and components
  -------------------------------------------------------------------------------

  type array_integer          is array (natural range <>) of integer;
  type descriptor_memory      is array (natural range <>) of std_logic_vector(31 downto 0);

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

  --STD_LOGIC_VECTOR serial array (first ctrl, then addr) conversion of/to descriptor
  function desc_ctrl_2_serial     (desc_ctrl : descriptor_control) return std_logic_vector;
  function serial_2_desc_ctrl     (serial : std_logic_vector(31 downto 0)) return descriptor_control;
  function serial_2_desc_actaddr  (serial : std_logic_vector(31 downto 0)) return descriptor_actionaddr;
  function serial_2_desc_branch   (serial : std_logic_vector(31 downto 0)) return descriptor_branch;
  
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
      apbi              : in  apb_slave_in_type;
      apbo              : out apb_slave_out_type;
      ctrl_out          : out injector_ctrl_reg_type;
      active            : out std_ulogic;
      err_status        : out std_ulogic;
      irq_flag_sts      : in  std_ulogic;
      curr_desc_in      : in  curr_des_out_type;
      curr_desc_ptr     : in  std_logic_vector(31 downto 0);
      sts_in            : in  status_out_type;
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
      ctrl              : in  injector_ctrl_reg_type;
      active            : in  std_ulogic;
      err_status        : in  std_ulogic;
      curr_desc_out     : out curr_des_out_type;
      curr_desc_ptr     : out std_logic_vector(31 downto 0);
      status            : out status_out_type;
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
      read_if_sts_in    : in  d_ex_sts_out_type;
      write_if_sts_in   : in  d_ex_sts_out_type;
      write_if_start    : out std_logic;
      delay_if_sts_in   : in  d_ex_sts_out_type;
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
      status_out        : out d_ex_sts_out_type;
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
      status_out        : out d_ex_sts_out_type;
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
      status_out        : out d_ex_sts_out_type
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
      apbi            : in  apb_slave_in_type;
      apbo            : out apb_slave_out_type;
      bm0_mosi        : out bm_mosi;
      bm0_miso        : in  bm_miso
      );
  end component injector;

  -- INJECTOR AHB top level interface
  component injector_ahb is
    generic (
      dbits           : integer range 8 to 1024           :=   32;
      MAX_SIZE_BURST  : integer range 8 to 1024           := 1024;
      tech            : integer range 0 to numTech        := typeTech;
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
      apbi            : in  apb_slave_in_type;
      apbo            : out apb_slave_out_type;
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
    fixed_addr      : std_logic;
    max_bsize       : integer;
    bm_bytes        : integer;
    total_size      : std_logic_vector(19 downto 0)
    )
    return std_logic_vector is
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

  function desc_ctrl_2_serial(desc_ctrl : descriptor_control) return std_logic_vector is
    variable serial :std_Logic_vector(31 downto 0) := (others => '0');
  begin
    serial := desc_ctrl.size(31 downto 13) & desc_ctrl.count & desc_ctrl.reserved & desc_ctrl.type_spec & desc_ctrl.irqe & desc_ctrl.act_type & desc_ctrl.last;
    
    return serial;
  end desc_ctrl_2_serial;

  function serial_2_desc_ctrl(serial : std_logic_vector(31 downto 0)) return descriptor_control is
    variable desc_ctrl : descriptor_control := DESCRIPTOR_CTRL_RST;
  begin
    desc_ctrl.size      := '0' & serial(31 downto 13);
    desc_ctrl.count     := serial(12 downto  7);
    desc_ctrl.reserved  := serial( 6);
    desc_ctrl.type_spec := serial( 5);
    desc_ctrl.irqe      := serial( 4);
    desc_ctrl.act_type  := serial( 3 downto  1);
    desc_ctrl.last      := serial( 0);

    return desc_ctrl;
  end serial_2_desc_ctrl;

  function serial_2_desc_actaddr(serial : std_logic_vector(31 downto 0)) return descriptor_actionaddr is
    variable desc_actaddr : descriptor_actionaddr := DESCRIPTOR_ACTADDR_RST;
  begin
    desc_actaddr.addr   := serial(31 downto  0);

    return desc_actaddr;
  end serial_2_desc_actaddr;

  function serial_2_desc_branch(serial : std_logic_vector(31 downto 0)) return descriptor_branch is
    variable desc_branch : descriptor_branch := DESCRIPTOR_BRANCH_RST;
  begin
    desc_branch.not_flag    := serial(31);
    desc_branch.flags       := serial(30 downto 15);
    desc_branch.loop_times  := serial(14 downto  9);
    desc_branch.branch_ptr  := serial( 8 downto  0);

    return desc_branch;
  end serial_2_desc_branch;


end package body injector_pkg;

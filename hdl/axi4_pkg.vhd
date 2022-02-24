------------------------------------------------------------------------------
-- Package:     axi4_pkg
-- File:        axi4_pkg.vhd
-- Author:      Francis Fuentes (BSC_CNS)
-- Description: Internal package for AXI4 components
------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package axi4_pkg is
  -----------------------------------------------------------------------------
  -- Constant declaration
  -----------------------------------------------------------------------------

  -- User parameters START --

    -- AXI bus generics
    constant ID_R_WIDTH             : integer                   := 4;   -- AXI ID's bus width.
    constant ID_W_WIDTH             : integer                   := 4;   -- AXI ID's bus width.
    constant ADDR_WIDTH             : integer range  1 to   32  := 32;  -- AXI address bus width. (Tested only for 32 bits)
    constant DATA_WIDTH             : integer range  8 to 1024  := 64;  -- AXI data bus width. [Only power of 2s are allowed]

    constant rd_n_fifo_regs : integer range  2 to  128  := 4;   -- Number of buffer registers to use at AXI read transactions. [Only power of 2s are allowed]
    constant wr_n_fifo_regs : integer range  2 to  128  := 4;   -- Number of buffer registers to use at AXI write transactions. [Only power of 2s are allowed]

    -- Special features

      -- The Injector_implementation flag allows, when TRUE, to skip the blottleneck at the BM data bus by discarding read data and sending '0' to write 
      -- with the characteristics of a normal transaction. In addition, it saves the resources spent on the BM transfer side of the write tranfer logic.
      -- The read transaction logic is maintained so the injector can still use the interface to read the descriptors allocated on the AXI memory space.
      -- However, the bottleneck on read transactions can still be bypassed asserting the "bypass_rd_bm" input signal on the Manager interface when 
      -- requesting a transaction.
    constant Injector_implementation: boolean                   := TRUE; -- If don't know what to set it, FALSE is the best option for normal operation.

  -- User parameters END --

  -- Informative specification parameters (DO NOT MODIFY, BUT NOTICE THEM)
    constant Max_Transaction_Bytes  : integer                   := 4096; -- Maximum number of bytes that can be requested per BM transaction.


    constant FIX                    : std_logic_vector(1 downto 0) := "00"; -- AXI burst modes: FIXED
    constant INC                    : std_logic_vector(1 downto 0) := "01"; --                  INCREMENTAL
    constant WRAP                   : std_logic_vector(1 downto 0) := "10"; --                  WRAP

  -----------------------------------------------------------------------------
  -- Records and types
  -----------------------------------------------------------------------------

  -- AXI4 interface bus output
  type axi4_out_type is record
    -- Write address channel
    aw_id           : std_logic_vector( ID_W_WIDTH-1   downto 0 );
    aw_addr         : std_logic_vector( ADDR_WIDTH-1   downto 0 );
    aw_len          : std_logic_vector(  7 downto 0 );
    aw_burst        : std_logic_vector(  1 downto 0 );
    aw_lock         : std_logic;
    aw_cache        : std_logic_vector(  3 downto 0 );
    aw_size         : std_logic_vector(  2 downto 0 );
    aw_prot         : std_logic_vector(  2 downto 0 );
    aw_qos          : std_logic_vector(  3 downto 0 );
    aw_region       : std_logic_vector(  3 downto 0 );
    aw_valid        : std_logic;
    -- Write data channel
    w_data          : std_logic_vector( DATA_WIDTH-1   downto 0 );
    w_strb          : std_logic_vector( DATA_WIDTH/8-1 downto 0 );
    w_last          : std_logic;
    w_valid         : std_logic;
    -- Write response channel
    b_ready         : std_logic;
    -- Read address channel
    ar_id           : std_logic_vector( ID_R_WIDTH-1   downto 0 );
    ar_addr         : std_logic_vector( ADDR_WIDTH-1   downto 0 );
    ar_len          : std_logic_vector(  7 downto 0 );
    ar_size         : std_logic_vector(  2 downto 0 );
    ar_burst        : std_logic_vector(  1 downto 0 );
    ar_lock         : std_logic;
    ar_cache        : std_logic_vector(  3 downto 0 );
    ar_prot         : std_logic_vector(  2 downto 0 );
    ar_qos          : std_logic_vector(  3 downto 0 );
    ar_region       : std_logic_vector(  3 downto 0 );
    ar_valid        : std_logic;
    -- Read data channel
    r_ready         : std_logic;
  end record;

  -- AXI4 interface bus input
  type axi4_in_type is record
    -- Write address channel
    aw_ready        : std_logic;
    -- Write data channel
    w_ready         : std_logic;
    -- Write response channel
    b_id            : std_logic_vector ( ID_W_WIDTH-1  downto 0 );
    b_resp          : std_logic_vector (  1 downto 0 );
    b_valid         : std_logic;
    -- Read address channel
    ar_ready        : std_logic;
    -- Read data channel
    r_id            : std_logic_vector ( ID_R_WIDTH-1  downto 0 );
    r_data          : std_logic_vector ( DATA_WIDTH-1  downto 0 );
    r_resp          : std_logic_vector (  1 downto 0 );
    r_last          : std_logic;
    r_valid         : std_logic;
  end record;

  -- BM specific types
  type bm_out_type is record  --Input to injector_ctrl from bus master interface output
    -- Read channel
    rd_data         : std_logic_vector(127 downto 0);
    rd_req_grant    : std_logic;
    rd_valid        : std_logic;
    rd_done         : std_logic;
    rd_err          : std_logic;
    -- Write channel
    wr_req_grant    : std_logic;
    wr_full         : std_logic;
    wr_done         : std_logic;
    wr_err          : std_logic;
  end record;

  type bm_in_type is record  --Output from injector_ctrl to bus master interface input
    -- Read channel
    rd_addr         : std_logic_vector( ADDR_WIDTH-1   downto 0);
    rd_size         : std_logic_vector(11 downto 0);
    rd_req          : std_logic;
    -- Write channel
    wr_addr         : std_logic_vector( ADDR_WIDTH-1   downto 0);
    wr_size         : std_logic_vector(11 downto 0);
    wr_req          : std_logic;
    wr_data         : std_logic_vector(127 downto 0);
  end record;

  type array_integer          is array (natural range <>) of integer;
  type array_128vector        is array (natural range <>) of std_logic_vector(127 downto 0);

  -------------------------------------------------------------------------------
  -- Subprograms
  -------------------------------------------------------------------------------

  -- Unsigned addition and subtraction functions between std vectors and integers, returning a vector of 'len' lenght.
  function add_vector       (A, B : std_logic_vector; len : natural) return std_logic_vector;
  function add_vector       (A : std_logic_vector; B : integer; len : natural) return std_logic_vector;
  function sub_vector       (A, B : std_logic_vector; len : natural) return std_logic_vector;
  function sub_vector       (A : std_logic_vector; B : integer; len : natural) return std_logic_vector;
  function sub_vector       (A : integer; B : std_logic_vector; len : natural) return std_logic_vector;

  -- Computes the ceil log base two from an integer. This function is NOT for describing hardware, just to compute constants and buses widths.
  function log_2            (max_size : integer) return integer;

  -- OR_REDUCE substitude function. It returns a std_logic of the OR function of all the bits from a std_logic_vector.
  function or_vector        (vect : std_logic_vector) return std_logic;
  
  -- Boolean to std_logic. Just because VHDL is too strong.
  function to_std_logic     (wool : boolean) return std_logic;

  -- Returns maximum value from an array of integers.
  function max              (A : array_integer) return integer;

  -- IF function for when VHDL can not use if (like at constants).
  function sel              (A, B : integer; sel : boolean) return integer;

  -----------------------------------------------------------------------------
  -- Component instantiation
  -----------------------------------------------------------------------------

  component axi4_manager is
    generic (
      dbits           : integer range 32 to  128  := 32;
      axi_id          : integer                   := 0;
      ASYNC_RST       : boolean                   := FALSE
    );
    port (
      rstn            : in  std_ulogic;
      clk             : in  std_ulogic;
      axi4mi          : in  axi4_in_type;
      axi4mo          : out axi4_out_type;
      bm_in           : in  bm_in_type;
      bm_out          : out bm_out_type;
      bm_in_bypass_rd : in  std_logic
    );
  end component axi4_manager;

end package axi4_pkg;



package body axi4_pkg is

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

  -- Addition function between std_logic_vector and integer, returns vector with length assigned
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

  -- OR function of all the bits in a std_logic_vector.
  function or_vector(vect : std_logic_vector) return std_logic is
    variable wool  : std_logic;
  begin
    wool := '0';
    for i in vect'range loop
      wool := wool or vect(i);
    end loop;
    return wool;
  end or_vector;

  -- Boolean to std_logic function.
  function to_std_logic(wool : boolean) return std_logic is
    variable logic : std_logic;
  begin
    if wool then logic := '1'; 
    else         logic := '0';
    end if;
    return logic;
  end to_std_logic;

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

  -- IF function that outputs the first input if the boolean is true, the second if false.
  function sel(A, B : integer; sel : boolean) return integer is
  begin
    if sel then return A;
    else return B;
    end if;
  end sel;


end package body axi4_pkg;
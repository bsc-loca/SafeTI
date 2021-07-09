-----------------------------------------------------------------------------   
-- Entity:      Injector Delay Interface
-- File:        injector_delay_if.vhd
-- Author:      Oriol Sala
-- Description: Transaction delay implementation
------------------------------------------------------------------------------ 
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library grlib;
use grlib.config_types.all;
use grlib.config.all;
use grlib.stdlib.all;
library bsc;
use bsc.injector_pkg.all;


-----------------------------------------------------------------------------------------
-- Entity to perform specified transaction delays
-----------------------------------------------------------------------------------------
--Injector Delay IF performs specified waiting clock cycles passed from a specified Size. Data descriptor
--fields are passed from injector_ctrl.
------------------------------------------------------------------------------------------

entity injector_delay_if is
  port (
    rstn            : in  std_ulogic;           -- Active low reset
    clk             : in  std_ulogic;           -- Clock
    -- Signals to and from injector_ctrl
    ctrl_rst        : in  std_ulogic;           -- Reset signal from APB interface through injector
    err_sts_in      : in  std_ulogic;           -- Core error status from APB status register 
    delay_if_start  : in  std_ulogic;           -- Start control signal
    d_des_in        : in  data_dsc_strct_type;  -- Data descriptor input
    status_out      : out d_ex_sts_out_type     -- Delay status out signals 
    );
end entity injector_delay_if;

------------------------------------------------------------------------------
-- Architecture of injector_delay_if
------------------------------------------------------------------------------

architecture rtl of injector_delay_if is
  attribute sync_set_reset         : string;
  attribute sync_set_reset of rstn : signal is "true";
  -----------------------------------------------------------------------------
  -- Constant declaration
  -----------------------------------------------------------------------------

  -- Reset configuration
  constant ASYNC_RST : boolean := GRLIB_CONFIG_ARRAY(grlib_async_reset_enable) = 1;

  -- Constants for read_if present state
  constant DELAY_IF_IDLE    	: std_logic_vector(4 downto 0) := "01100"; -- 0x0C
  constant DELAY_IF_EXEC    	: std_logic_vector(4 downto 0) := "01101"; -- 0x0D

  -----------------------------------------------------------------------------
  -- Type and record 
  -----------------------------------------------------------------------------

  -- DELAY_IF states --
  -- idle =>
  -- Starting state. Waits for 'delay_if_start_in' signal to proceed.
  -- 
  -- exec_data_desc =>
  -- Wait until specified delay size is achieved. Send completed signal back to control interface 
  --

  type delay_if_state_type is (idle, exec_data_desc);

  --DELAY_IF reg type
  type delay_if_reg_type is record
    delay_if_state      : delay_if_state_type;            -- DELAY_IF states
    sts                 : d_ex_sts_out_type;              -- Status signals 
    tot_size            : std_logic_vector(18 downto 0);  -- Total size of delay cycles
    curr_size           : std_logic_vector(18 downto 0);  -- Remaining delay cycles
    err_state           : std_logic_vector(4 downto 0);   -- Error state
  end record;

  -- Reset value for READ_IF reg type
  constant DELAY_IF_REG_RES : delay_if_reg_type := (
    delay_if_state      => idle,
    sts                 => D_EX_STS_RST,
    tot_size            => (others => '0'),
    curr_size           => (others => '0'),
    err_state           => (others => '0')
    );

  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------
  signal r, rin : delay_if_reg_type;
  
begin
  -----------------------------------------------------------------------------
  -- Assignments
  -----------------------------------------------------------------------------

  -----------------------------------------------------------------------------
  -- Combinational process
  -----------------------------------------------------------------------------
  
  comb : process (r, d_des_in, delay_if_start, err_sts_in)

    variable v             : delay_if_reg_type;
  
  begin

    -- Default values 
    v                := r;

    -- DELAY_IF state machine
    case r.delay_if_state is
      when idle =>
        -- Default values
        v.sts.operation     := '0';
        v.curr_size         := (others => '0');
        v.sts.delay_if_err  := '0';

	-- Operation starts when start signal from control interface arrives and no errors are present
        if delay_if_start = '1' and err_sts_in = '0' then
          v.err_state      := (others => '0');
          v.sts.operation  := '1';
          v.sts.comp       := '0';
          v.tot_size       := d_des_in.ctrl.size;
          if orv(d_des_in.ctrl.size) = '0' then
            v.sts.comp := '1';
          end if;
          v.delay_if_state := exec_data_desc;
        end if;
        ----------     
        
      when exec_data_desc =>
        
        if (to_integer(unsigned(r.curr_size)) < to_integer(unsigned(r.tot_size)) ) then  
          v.curr_size := std_logic_vector(to_unsigned(to_integer(unsigned(r.curr_size)) + 1, 19));
        else                           
          v.sts.comp       := '1';
          v.sts.operation  := '0';
          v.delay_if_state := idle;
        end if;
      ----------

      ----------
      when others =>
        v.delay_if_state := idle;
        ----------         
    end case;  -- DELAY_IF state machine
    ----------------------
    -- Signal update --
    ----------------------
    -- State decoding for status display
    if r.sts.delay_if_err = '1' then
      status_out.state <= r.err_state;
    else
      case r.delay_if_state is
        when exec_data_desc =>
          status_out.state <= DELAY_IF_EXEC;
        when others =>
          status_out.state <= DELAY_IF_IDLE;
      end case;
    end if;

    rin                      <= v;
    status_out.delay_if_err  <= r.sts.delay_if_err;
    status_out.operation     <= r.sts.operation;
    status_out.comp          <= r.sts.comp;
     
  end process comb;

  -----------------------------------------------------------------------------
  -- Sequential Process
  -----------------------------------------------------------------------------

  seq : process (clk, rstn)
  begin
    if (rstn = '0' and ASYNC_RST) then
      r <= DELAY_IF_REG_RES;
    elsif rising_edge(clk) then
      if rstn = '0' or ctrl_rst = '1' then
        r <= DELAY_IF_REG_RES;
      else
        r <= rin;
      end if;
    end if;
  end process seq;
-----------------------------------------------------------------------------  
-- Component instantiation
-----------------------------------------------------------------------------
  
end architecture rtl;

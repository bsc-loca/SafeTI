-----------------------------------------------------------------------------   
-- Entity:      injector_delay
-- File:        injector_delay.vhd
-- Author:      Francisco Fuentes, Oriol Sala
-- Description: Module that implements the execution of DELAY descriptors.
------------------------------------------------------------------------------ 
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library safety;
use safety.injector_pkg.all;

entity injector_delay is
  generic (
    ASYNC_RST         : boolean := TRUE           -- Allow asynchronous reset flag
  );
  port (
    -- Internal I/O
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
end entity injector_delay;

architecture rtl of injector_delay is

  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------

  -- Registers
  signal wait_time    : unsigned(desc_data.num_cycles'range);
  signal done_reg     : std_logic;
  signal status_reg   : std_logic_vector(status'range);

  -- Signals
  signal ongoing      : std_logic;
  signal exe_time     : unsigned(desc_data.num_cycles'range);

  
begin

  -----------------------------------------------------------------------------
  -- Assignments
  -----------------------------------------------------------------------------

  -- I/O signal assignments.
  busy      <= ongoing;
  done      <= (start or done_reg) and not(ongoing); 
  status    <= status_reg;

  -- Decrease wait counter.
  ongoing   <= '1' when (exe_time /= (wait_time'range => '0')) else '0';

  -- Set the execution wait time.
  exe_time  <= desc_data.num_cycles when (start = '1') else wait_time;


  -----------------------------------------------------------------------------
  -- Sequential Process
  -----------------------------------------------------------------------------

  seq0 : process(clk, rstn)
  begin
    if(rstn = '0' and ASYNC_RST) then
      wait_time       <= (others => '0');
      status_reg      <= DEBUG_STATE_IDLE;
    elsif rising_edge(clk) then
      if(rstn = '0' or rst_sw = '1') then
        wait_time     <= (others => '0');
        status_reg    <= DEBUG_STATE_IDLE;
      else

        if(ongoing = '1') then
          done_reg    <= '1';
          wait_time   <= exe_time - 1;
          status_reg  <= DEBUG_STATE_NO_OPERATION;
        else
          done_reg    <= '0';
          status_reg  <= DEBUG_STATE_IDLE;
        end if;

      end if;
    end if;
  end process seq0;
  
end architecture rtl;

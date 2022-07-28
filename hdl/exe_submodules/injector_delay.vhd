-----------------------------------------------------------------------------   
-- Entity:      Injector Delay Submodule
-- File:        injector_delay.vhd
-- Author:      Francisco Fuentes, Oriol Sala
-- Description: Module that implements the execution of DELAY descriptors.
------------------------------------------------------------------------------ 
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library safety;
use safety.injector_pkg.all;

entity injector_delay_if is
  generic (
    ASYNC_RST       : boolean := TRUE           -- Allow asynchronous reset flag
  );
  port (
    -- Internal I/O
    rstn            : in  std_ulogic;
    clk             : in  std_ulogic;
    -- Internal I/O
    rst_sw          : in  std_logic;
    start           : in  std_logic;
    busy            : out std_logic;
    desc_data       : in  operation_delay;
    status          : out std_logic_vector(MAX_STATUS_LEN - 1 downto 0)
  );
end entity injector_delay_if;

architecture rtl of injector_delay_if is

  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------

  -- Registers
  signal wait_time  : unsigned(desc_data.num_cycles'range);
  signal status_int : std_logic_vector(status'range);

  -- Signals
  signal ongoing    : std_logic;

  
begin

  -----------------------------------------------------------------------------
  -- Assignments
  -----------------------------------------------------------------------------

  -- I/O signal assignments.
  busy <= ongoing;

  -- Decrease wait counter.
  ongoing <= '1' when (wait_time /= to_unsigned(0, wait_time'length)) else '0';


  -----------------------------------------------------------------------------
  -- Sequential Process
  -----------------------------------------------------------------------------

  seq0 : process(clk, rstn)
  begin
    if(rstn = '0' and ASYNC_RST) then
      wait_time           <= (others => '0');
      status_int          <= DEBUG_STATE_IDLE;
    elsif rising_edge(clk) then
      if(rstn = '0' or rst_sw = '1') then
        wait_time         <= (others => '0');
        status_int        <= DEBUG_STATE_IDLE;
      else

        if(start = '1') then
          wait_time       <= desc_data.num_cycles;
          status_int      <= DEBUG_STATE_NORMAL;
        elsif(ongoing = '1') then
          wait_time       <= wait_time - 1;
        else
          status_int      <= DEBUG_STATE_IDLE;
        end if;

      end if;
    end if;
  end process seq0;
  
end architecture rtl;

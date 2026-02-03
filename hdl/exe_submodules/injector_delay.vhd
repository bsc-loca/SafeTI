-----------------------------------------------------------------------------
-- Entity:      injector_delay
-- File:        injector_delay.vhd
-- Author:      Francisco Fuentes, Oriol Sala
-- Description: Module that implements the execution of DELAY descriptors.
--              Since single cycle operations may be input (size = 0), this
--              design features a program+start implementation supporting
--              multiple DELAY descriptors with delays of 1 cycle each.
------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library safety;
use safety.injector_pkg.all;

entity injector_delay is
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
    error             : out std_logic;  -- Error submodule flag
    status            : out DEBUG_STATE
  );
end entity injector_delay;

architecture rtl of injector_delay is

  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------

  -- Registers
  signal busy_reg     : std_logic;
  signal full_reg     : std_logic;
  signal hold_buffer  : std_logic;
  signal size_buffer  : std_logic_vector(desc_data.num_cycles'range);
  signal wait_time    : unsigned(desc_data.num_cycles'range) := (others => '0');
  signal status_reg   : DEBUG_STATE;

  -- Signals
  signal busy_sig     : std_logic;
  signal full_sig     : std_logic;
  signal sb_zero      : std_logic;          -- size_buffer = 0
  signal wt_zero_n    : std_logic;          -- wait_time /= 0
  signal minus        : unsigned(0 to 0);   -- wait_time /= 0, but in unsigned


begin

  -----------------------------------------------------------------------------
  -- Assignments
  -----------------------------------------------------------------------------

  -- I/O signal assignments
  busy      <= busy_sig;
  full      <= full_sig;
  error     <= start and busy_sig;
  status    <= DEBUG_STATE_UNEXPECTED_START when ((start and busy_sig) = '1') else
               DEBUG_STATE_HOLD             when ((hold_buffer and hold) = '1') else status_reg;

  -- Middle-operand signals
  sb_zero   <= '1' when (size_buffer = (size_buffer'range => '0')) else '0';
  wt_zero_n <= '1' when (wait_time /= (wait_time'range => '0')) else '0';
  minus     <= "1" when (wt_zero_n = '1') else "0";
  busy_sig  <= wt_zero_n or (hold and (busy_reg or hold_buffer));
  full_sig  <= full_reg when not(start = '1' and hold = '0') else '0';


  -----------------------------------------------------------------------------
  -- Sequential Process
  -----------------------------------------------------------------------------

  seq0 : process(clk, rstn)
  begin
    if(rstn = '0' and ASYNC_RST) then
      full_reg          <= '0';
      busy_reg          <= '0';
      hold_buffer       <= '0';
      size_buffer       <= (others => '0');
      wait_time         <= (others => '0');
      status_reg        <= DEBUG_STATE_IDLE;
    elsif rising_edge(clk) then
      if(rstn = '0' or rst_sw = '1') then
        full_reg        <= '0';
        busy_reg        <= '0';
        hold_buffer     <= '0';
        size_buffer     <= (others => '0');
        wait_time       <= (others => '0');
        status_reg      <= DEBUG_STATE_IDLE;
      else

        -- Operation logic
        if(busy_reg = '1') then
          hold_buffer   <= hold;
          if(hold = '0') then
            busy_reg    <= wt_zero_n;
            wait_time   <= wait_time - minus;
            if(wt_zero_n = '1') then
              status_reg<= DEBUG_STATE_OPERATION;
            else
              status_reg<= DEBUG_STATE_IDLE;
            end if;
          end if;
        end if;

        -- Start logic
        if(full_reg = '1' and start = '1') then
          full_reg      <= '0';
          busy_reg      <= not(sb_zero);
          wait_time     <= unsigned(size_buffer);
          hold_buffer   <= hold;
          if(sb_zero = '0') then
            status_reg  <= DEBUG_STATE_OPERATION;
          else
            status_reg  <= DEBUG_STATE_IDLE;
          end if;
        end if;

        -- Program logic
        if(full_sig = '0' and program = '1') then
          full_reg      <= '1';
          size_buffer   <= desc_data.num_cycles;
          status_reg    <= DEBUG_STATE_PROGRAMMED;
        end if;

      end if;
    end if;
  end process seq0;

end architecture rtl;

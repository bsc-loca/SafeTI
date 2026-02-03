-----------------------------------------------------------------------------
-- Entity:      injector_observer
-- File:        injector_observer.vhd
-- Author:      Francisco Fuentes
-- Description: Module that implements the execution of HOLD descriptors.
------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library safety;
use safety.injector_pkg.all;

entity injector_observer is
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
end entity injector_observer;

architecture rtl of injector_observer is

  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------

  -- Registers
  signal busy_reg     : std_logic;
  signal hold_reg     : std_logic;
  signal addr_reg     : std_logic_vector(desc_data.rd_wr.addr'range);
  signal mask_reg     : std_logic_vector(external_addr'range);
  signal status_reg   : DEBUG_STATE;

  -- Signals
  signal full_sig     : std_logic;
  signal addr_match   : std_logic;


begin

  -----------------------------------------------------------------------------
  -- Assignments
  -----------------------------------------------------------------------------

  -- I/O signal assignments.
  busy        <= full_sig; -- Active when waiting for interface address match
  full        <= full_sig; -- Disable addr update or repeated desc if active
  status      <= status_reg;

  -- Middle-operand signals
  full_sig    <= busy_reg and not(addr_match);
  addr_match  <= '1' when ( (mask_reg and (addr_reg xor external_addr)) = (external_addr'range => '0') ) else '0';


  -----------------------------------------------------------------------------
  -- Sequential Process
  -----------------------------------------------------------------------------

  seq0 : process(clk, rstn)
  begin
    if(rstn = '0' and ASYNC_RST) then
      busy_reg          <= '0';
      hold_reg          <= '0';
      addr_reg          <= (others => '0');
      mask_reg          <= (others => '1');
      status_reg        <= DEBUG_STATE_IDLE;
    elsif rising_edge(clk) then
      if(rstn = '0' or rst_sw = '1') then
        busy_reg        <= '0';
        hold_reg        <= '0';
        addr_reg        <= (others => '0');
        mask_reg        <= (others => '1');
        status_reg      <= DEBUG_STATE_IDLE;
      else

        -- Default state
        busy_reg        <= '0';
        status_reg      <= DEBUG_STATE_IDLE;

        -- Operation logic
        if( (hold_reg = '1' and start = '1') or busy_reg = '1') then
          busy_reg      <= '1';
          hold_reg      <= '0';
          if(hold = '0') then
            busy_reg    <= not(addr_match);
            status_reg  <= DEBUG_STATE_OPERATION;
          else
            status_reg  <= DEBUG_STATE_HOLD;
          end if;
        end if;

        -- Program logic
        if(full_sig = '0' and program = '1') then
          if(desc_data.observer.set_addr = '1') then
            addr_reg    <= std_logic_vector(desc_data.rd_wr.addr);
          end if;
          hold_reg      <= desc_data.observer.set_hold;
          mask_reg      <= (mask_reg'high downto desc_data.observer.set_mask'length => '1') & desc_data.observer.set_mask;
          status_reg    <= DEBUG_STATE_PROGRAMMED;
        end if;

      end if;
    end if;
  end process seq0;

end architecture rtl;

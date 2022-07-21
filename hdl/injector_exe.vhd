-----------------------------------------------------------------------------   
-- Entity:      injector_exe
-- File:        injector_exe.vhd
-- Author:      Francis Fuentes
-- Description: EXE stage in SafeTI Injector core pipeline.
------------------------------------------------------------------------------ 
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library safety;
use safety.injector_pkg.all;


-------------------------------------------------------
-- Entity for EXE stage in Injector core pipeline --
-------------------------------------------------------

entity injector_exe is
generic (
  PC_LEN            : integer                       := 4;   -- Length of PC register
  ASYNC_RST         : boolean                       := TRUE -- Allow asynchronous reset flag
);
port (
  -- External I/O
  rstn              : in  std_ulogic;                       -- Reset
  clk               : in  std_ulogic;                       -- Clock
  fixed_address_op  : out std_logic;                        -- Fixed address operation flag
  -- Internal I/O
  enable            : in  std_logic;                        -- Enable DECODE stage
  rst_sw            : in  std_logic;                        -- Software reset through APB
    -- Signals from/for DECODE
  decode_ready      : in  std_logic;                        -- Control data ready to be read flag
  exe_ready         : out std_logic;                        -- Control data can be read flag
  decode_pc         : in  unsigned(PC_LEN - 1 downto 0);    -- Descriptor word 0 PC of the operation being executed
  decode_act_subm   : in  submodule_enable;                 -- Active submodule
  decode_rd_wr      : in  operation_rd_wr;                  -- Decoded RD and WR operation data
  decode_delay      : in  operation_delay;                  -- Decoded DELAY operation data
    -- Debug signals
  exe_pc            : out unsigned(PC_LEN - 1 downto 0);    -- PC of the decoded descriptor
  irq               : out std_logic;                        -- Interruption due to submodule
  state             : out std_logic_vector(MAX_STATUS_LEN - 1 downto 0)
);
end entity injector_exe;

architecture rtl of injector_exe is

  -----------------------------------------------------------------------------
  -- Types and reset constants declaration
  -----------------------------------------------------------------------------

  constant RESET_SUBMODULE_ENABLE : submodule_enable := (
    delay_en      => '0',
    read_en       => '0',
    write_en      => '0'
  );


  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------

  -- Registers
  signal pc     : unsigned(PC_LEN - 1 downto 0);
  signal busy   : std_logic;

  -- Signals
  signal start  : std_logic;


begin -- rtl

  -----------------------------------------------------------------------------
  -- Assignments
  -----------------------------------------------------------------------------

  -- I/O signal assignments


  -- Start flag of any execution
  start <= '1' when (decode_act_subm /= RESET_SUBMODULE_ENABLE and busy = '0') else '0';
  

  -----------------------------------------------------------------------------
  -- Sequential process
  -----------------------------------------------------------------------------
  
  seq0 : process(clk, rstn)
  begin
    if(rstn = '0' and ASYNC_RST) then
      pc                  <= (others => '0');
    elsif rising_edge(clk) then
      if(rstn = '0' or rst_sw = '1') then
        pc                <= (others => '0');
      else

      end if;
    end if;
  end process seq0;

end architecture rtl;
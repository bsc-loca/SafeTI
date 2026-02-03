-----------------------------------------------------------------------------
-- Entity:      injector_ram
-- File:        injector_ram.vhd (simple_dual_one_clock.vhd)
-- Author:      Francis Fuentes, Xilinx Vivado 2020.2 Template
-- Description: Memory component infering injector_ram FPGA resources.
------------------------------------------------------------------------------
-- Simple Dual-Port Block RAM with One Clock
-- Correct Modelization with a Shared Variable
-- Original Source File: simple_dual_one_clock.vhd

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;

entity injector_ram is
  generic (
    RAM_ADDR_LEN  : integer range 4 to 32         := 10;
    RAM_RESET_VAL : std_logic_vector(31 downto 0) := (others => '0')
  );
  port (
    clk     : in  std_logic;
    rd_en   : in  std_logic;                      -- Read  enable  port
    rd_addr : in  std_logic_vector(31 downto 0);  -- Read  address port
    rd_data : out std_logic_vector(31 downto 0);  -- Read  data    port
    wr_en   : in  std_logic;                      -- Write enable  port
    wr_addr : in  std_logic_vector(31 downto 0);  -- Write address port
    wr_data : in  std_logic_vector(31 downto 0)   -- Write data    port
  );
end injector_ram;

architecture rtl of injector_ram is

    constant ram_len : integer := 2 ** RAM_ADDR_LEN;
    type ram_type is array (0 to ram_len - 1) of std_logic_vector(31 downto 0);
    signal RAM : ram_type := (others => RAM_RESET_VAL);
    attribute ram_style : string;
    attribute ram_style of RAM : signal is "auto"; --"distributed";

  begin


    process(clk)
    begin
      if rising_edge(clk) then

        -- Write logic
        if wr_en = '1' then
          RAM(conv_integer(( wr_addr(RAM_ADDR_LEN-1 downto 0)) )) <= wr_data;
        end if;

      end if;
    end process;

    process(clk)
    begin
      if rising_edge(clk) then

        -- Read logic
        if rd_en = '1' then
          rd_data <= RAM( conv_integer(rd_addr(RAM_ADDR_LEN-1 downto 0)) );
        else
          rd_data <= (others => '0');
        end if;

      end if;
    end process;

end architecture rtl;

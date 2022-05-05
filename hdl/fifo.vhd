library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity fifo is
  generic (
    RAM_LENGTH : integer := 16;     
    BUS_LENGTH : integer := 128;
    ASYNC_RST  : boolean := FALSE
  );
  port(
    clk           : in  std_logic;
    rstn          : in  std_logic;
    write_i       : in  std_logic;
    read_i        : in  std_logic;
    read_rst_i    : in  std_logic;
    full_o        : out std_logic;
    comp_o        : out std_logic;
    wdata_i       : in  std_logic_vector(BUS_LENGTH-1 downto 0);
    rdata_o       : out std_logic_vector(BUS_LENGTH-1 downto 0);
    ctrl_rst      : in  std_logic
  );  
end entity fifo;


architecture rtl of fifo is
  attribute sync_set_reset         : string;
  attribute sync_set_reset of rstn : signal is "true";
  -----------------------------------------------------------------------------
  -- Constant declaration
  -----------------------------------------------------------------------------

  -- Reset configuration
  constant RAM_INDEX : integer := integer(ceil(log2(real(RAM_LENGTH))));
  
  type ram_type is array (RAM_LENGTH-1 downto 0) of std_logic_vector(BUS_LENGTH-1 downto 0);
  signal ram : ram_type;
  signal addr, r_write_addr, r_read_addr : unsigned(RAM_INDEX-1 downto 0);
  signal fifo_full, fifo_comp : std_logic; 
begin

  RAM_PROC: process(clk)
  begin
    if (rstn = '0' and ASYNC_RST) then
      r_write_addr  <= (others => '0');
      r_read_addr   <= (others => '0');
      ram           <= (others => (others => '0'));
      fifo_full     <= '0';
      fifo_comp     <= '0';
    elsif rising_edge(clk) then
      if rstn = '0' or ctrl_rst = '1' then
        r_write_addr  <= (others => '0');
        r_read_addr   <= (others => '0');
        ram           <= (others => (others => '0'));
        fifo_full     <= '0';
        fifo_comp     <= '0';
      else
        -- RAM
        if write_i = '1' then
          ram(to_integer(r_write_addr)) <= wdata_i;
        end if;
        --if read_i = '1' then
          rdata_o <= ram(to_integer(r_read_addr));
        --end if;
        
        -- FIFO
        if write_i = '1' then
          r_write_addr <= r_write_addr + 1;
        end if;
        if read_i = '1' then
        r_read_addr <= r_read_addr + 1;
        end if;

        if read_rst_i = '1' then
          r_read_addr <= (others => '0');
        end if;

        if r_read_addr = (RAM_LENGTH - 1) then
          fifo_comp <= '1';
        else
          fifo_comp <= '0';
        end if;

        if r_write_addr = (RAM_LENGTH -1) then
          fifo_full <= '1';
        else
          fifo_full <= '0';
        end if;

      end if;
    end if;
  end process;
  
  full_o <= fifo_full;
  comp_o <= fifo_comp;
  --addr <= r_write_addr when write_i = '1' else r_read_addr;

end;

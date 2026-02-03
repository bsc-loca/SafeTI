-----------------------------------------------------------------------------
-- Entity:      injector_fetch
-- File:        injector_fetch.vhd
-- Author:      Francis Fuentes
-- Description: FETCH stage in SafeTI Injector core pipeline.
------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library safety;
use safety.injector_pkg.all;


------------------------------------------------------
-- Entity for FETCH stage in Injector core pipeline --
------------------------------------------------------

entity injector_fetch is
  generic (
    PC_LEN          : integer                       := 4;   -- Length of PC register
    ASYNC_RST       : boolean                       := TRUE -- Allow asynchronous reset flag
  );
  port (
  -- External I/O
    rstn            : in  std_ulogic;                       -- Reset
    clk             : in  std_ulogic;                       -- Clock
  -- Internal I/O
    enable          : in  std_logic;                        -- Enable FETCH stage
    rst_sw          : in  std_logic;                        -- Software reset through CSR
      -- Signals from CSR registers
    queue_mode_en   : in  std_logic;                        -- QUEUE mode enabled
    desc_word_wr    : in  std_logic_vector(31 downto 0);    -- Descriptor word to be written on the Program Memory
    desc_word_wen   : in  std_logic;                        -- Write enable of a descriptor word write
      -- Signals for/from DECODE
    fetch_ready     : out std_logic;                        -- Descriptor ready to be read flag
    decode_read     : in  std_logic;                        -- Descriptor can be read flag from buffer
    pc              : out unsigned(PC_LEN - 1 downto 0);    -- PC of the word 0 descriptor fetched
    desc            : out desc_words;                       -- Descriptor words
      -- Debug signals
    irq             : out std_logic;                        -- Error interruption
    state           : out DEBUG_STATE
  );
end entity injector_fetch;

architecture rtl of injector_fetch is

  -----------------------------------------------------------------------------
  -- Component declaration
  -----------------------------------------------------------------------------

  component injector_ram is
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
  end component injector_ram;


  -----------------------------------------------------------------------------
  -- Types and reset constants declaration
  -----------------------------------------------------------------------------

  type program_memory is array (0 to 2**PC_LEN - 1) of std_logic_vector(31 downto 0);

  constant MAX_DESC_WORD_INDEX  : integer := log_2(MAX_DESC_WORDS);

  constant RESET_DESC_WORDS     : desc_words      := (
    others => ( others => '0' )
  );


  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------

  -- Registers
  signal fetch_en_del     : std_logic;      -- FETCH stage input enable delayed
  signal fetch_en_reg     : std_logic;      -- FETCH stage enable register
  signal pc_wr            : unsigned(PC_LEN downto 0);  -- PC for writing. Use PC_LEN as error flag.
  signal pc_rd            : unsigned(PC_LEN downto 0);  -- PC for reading. Use PC_LEN as error flag.
  signal pc_rd_del        : unsigned(PC_LEN downto 0);  -- PC of what has been read. Use PC_LEN bit high as initialization.
  signal desc_buffer      : desc_words;     -- Descriptor words buffer for DECODE stage.
  signal desc_w_counter   : unsigned(MAX_DESC_WORD_INDEX downto 0) := (others => '0');      -- Counter to index desc_buffer words (overflow allowed).
  signal pc_desc          : unsigned(PC_LEN - 1 downto 0);          -- PC of the buffered descriptor (PC of desc_buffer[0]).

  -- Signals
  signal fetch_en         : std_logic;      -- FETCH stage enable
  signal fetch_en_pulse   : std_logic;      -- FETCH stage enable pulse
  signal desc_ready       : std_logic;      -- Descriptor ready to be read flag
  signal desc_sent        : std_logic;      -- Descriptor has been read by DECODE stage
  signal desc_last_sent   : std_logic;      -- Last descriptor of program has been read by DECODE stage
  signal desc_w_stop      : unsigned(MAX_DESC_WORD_INDEX - 1 downto 0) := (others => '0');  -- desc_buffer index to stop depending on type.
  signal err_pc_wr_oom    : std_logic;      -- PC write overflow = out of memory for writing error flag.
  signal err_pc_oom       : std_logic;      -- PC overflow = out of memory error flag.
  signal err_type         : std_logic;      -- Unsupported type decode.
  signal mem_rd           : std_logic;                      -- MEM read  word happens
  signal mem_rd_addr      : std_logic_vector(31 downto 0);  -- MEM read  address
  signal mem_rd_data      : std_logic_vector(31 downto 0);  -- MEM read  word data
  signal mem_wr_en        : std_logic;                      -- MEM write enable
  signal mem_wr_addr      : std_logic_vector(31 downto 0);  -- MEM write address
  signal mem_wr_data      : std_logic_vector(31 downto 0);  -- MEM write word data


begin -- rtl

  -----------------------------------------------------------------------------
  -- Assignments
  -----------------------------------------------------------------------------

  -- I/O signal assignments
  pc            <= pc_desc;
  irq           <= err_pc_wr_oom or err_pc_oom or err_type;
  state         <= err_pc_wr_oom & err_pc_oom & err_type & (MAX_STATUS_LEN - 4 downto 0 => '0');
  desc          <= desc_buffer;
  fetch_ready   <= desc_ready;


  -- Error signal assignments
  err_pc_wr_oom <= pc_wr(PC_LEN) and desc_word_wen; -- Error when tried to write on OoM position
  err_pc_oom    <= pc_rd(PC_LEN) and not(desc_buffer(0)(0)); -- Error when last descriptor of memory is not last

  -- Decode the descriptor type to set the respective desc_w_stop for each type in combinational logic.
  comb0 : process(desc_buffer(0)(5 downto 1))
  begin
    err_type    <= '0';

    case(desc_buffer(0)(5 downto 1)) is
      -- Operations that are encoded by one word
      when OP_DELAY | OP_OBSERVER_HOLD =>
        desc_w_stop <= to_unsigned(0, desc_w_stop'length);

      -- Operations that are encoded by two words
      when OP_READ | OP_WRITE | OP_READ_FIX | OP_WRITE_FIX | OP_READ_SEQ | OP_WRITE_SEQ | OP_OBSERVER_ADDR =>
        desc_w_stop <= to_unsigned(1, desc_w_stop'length);

      --when OP_BRANCH    =>  desc_w_stop <= to_unsigned(0, desc_w_stop'length);
      --when OP_META      =>  desc_w_stop <= to_unsigned(x, desc_w_stop'length); -- TODO: META TO BE IMPLEMENTED
      when others =>
        desc_w_stop <= (others => '0');
        err_type    <= '1';

    end case;
  end process comb0;

  -- Signal high when a descriptor has been transfer to the DECODE stage of the pipeline
  desc_sent       <= desc_ready and decode_read;

  -- Signal high when the descriptor sent is the last in the program.
  desc_last_sent  <= desc_buffer(0)(0) and desc_sent;

  -- Flag descriptor ready for reading when all the words for that type have been read.
  desc_ready      <= '1' when (desc_w_counter > ('0' & desc_w_stop)) else '0';

  -- MEM signaling
  mem_rd          <= fetch_en when ( (desc_sent = '1' or desc_ready = '0') and (pc_rd = pc_rd_del) ) else '0';
  mem_rd_addr     <= (mem_rd_addr'high downto PC_LEN => '0') & std_logic_vector(pc_rd(PC_LEN - 1 downto 0));
  mem_wr_en       <= '1' when (desc_word_wen = '1' and pc_wr(PC_LEN) = '0') else '0';
  mem_wr_addr     <= (mem_wr_addr'high downto PC_LEN => '0') & std_logic_vector(pc_wr(PC_LEN - 1 downto 0));
  mem_wr_data     <= desc_word_wr;

  -- FETCH enable pulse with software reset
  fetch_en        <= '0' when (desc_last_sent = '1' and queue_mode_en = '0') else fetch_en_reg;
  fetch_en_pulse  <= '1' when (enable = '1' and fetch_en_del = '0') else '0';


  -----------------------------------------------------------------------------
  -- Sequential process
  -----------------------------------------------------------------------------

  seq0 : process(clk, rstn)
  begin
    if(rstn = '0' and ASYNC_RST) then
      pc_wr               <= (others => '0');
      pc_rd               <= (others => '0');
      pc_rd_del           <= '1' & (PC_LEN-1 downto 0 => '0');
      pc_desc             <= (others => '0');
      desc_w_counter      <= (others => '0');
      desc_buffer         <= RESET_DESC_WORDS;
    elsif rising_edge(clk) then
      if(rstn = '0' or rst_sw = '1') then
        pc_wr             <= (others => '0');
        pc_rd             <= (others => '0');
        pc_rd_del         <= '1' & (PC_LEN-1 downto 0 => '0');
        pc_desc           <= (others => '0');
        desc_w_counter    <= (others => '0');
        desc_buffer       <= RESET_DESC_WORDS;
      else

      --------------------------------
      -- Program memory write logic --
      --------------------------------

        if(mem_wr_en = '1') then
          pc_wr                                         <= pc_wr + 1;
        end if;


      -------------------------------
      -- Program memory read logic --
      -------------------------------

        -- Fetch new descriptor when enabled, no out of memory error
        if(enable = '1' and err_pc_oom = '0') then
          -- Remember what PC word is being read
          pc_rd_del         <= pc_rd;

          -- Return PC to 0 when sending last descriptor of the injector program.
          if(desc_last_sent = '1') then
            pc_rd           <= (others => '0');
          elsif(mem_rd = '1') then
          -- Otherwise, increase PC for reading MEM
            pc_rd           <= pc_rd + 1;
          end if;

          -- Manage descriptor PC (debug) and rd word counter for the descriptor buffer.
          if(desc_sent = '1') then
          -- Set word counter to 1 if this cycle a word is being read, 0 otherwise.
            desc_w_counter  <= unsigned(std_logic_vector'('0' & mem_rd));
            pc_desc         <= pc_rd(pc_desc'range);
            if(mem_rd = '1') then
              desc_buffer(0) <= mem_rd_data;
            end if;
          elsif(mem_rd = '1') then
          -- Increment word counter if the descriptor read is still incomplete.
            desc_w_counter  <= desc_w_counter + 1;
            desc_buffer(to_integer(desc_w_counter)) <= mem_rd_data;
          end if;

        end if;

      end if;
    end if;
  end process seq0;

  -- To avoid reading from MEM first descriptor after program end it is
  -- required to disable mem_rd in advance. For that, it's necessary
  -- fetch_en_pulse to latch the enable only when reading MEM is required.
  seq1 : process(clk, rstn)
  begin
    if(rstn = '0' and ASYNC_RST) then
      fetch_en_reg    <= '0';
      fetch_en_del    <= '0';
    elsif rising_edge(clk) then
      if(rstn = '0' or rst_sw = '1') then
        fetch_en_reg  <= '0';
        fetch_en_del  <= '0';
      else

        -- Enable signal delayed to generate enable pulse
        if(fetch_en_del = '0') then
          fetch_en_del    <= enable;
        end if;

        -- Enable latch used only when QUEUE mode is disabled
        if(fetch_en_pulse = '1') then
          fetch_en_reg  <= '1';
        elsif(fetch_en_reg = '1' and queue_mode_en = '0') then
          fetch_en_reg  <= not(desc_last_sent);
        end if;

      end if;
    end if;
  end process seq1;


  -----------------------------------------------------------------------------
  -- Component instantiation
  -----------------------------------------------------------------------------

  -- Descriptor memory buffer
  mem : injector_ram
    generic map (
      RAM_ADDR_LEN  => PC_LEN,
      RAM_RESET_VAL => (desc_word_wr'high downto 1 => '0') & '1'
    )
    port map (
      clk     => clk,
      rd_en   => enable,        -- Read  enable  port
      rd_addr => mem_rd_addr,   -- Read  address port
      rd_data => mem_rd_data,   -- Read  data    port
      wr_en   => mem_wr_en,     -- Write enable  port
      wr_addr => mem_wr_addr,   -- Write address port
      wr_data => mem_wr_data    -- Write data    port
  );

end architecture rtl;

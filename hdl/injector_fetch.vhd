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
    PC_LEN          : integer                       := 4;     -- Length of PC register
    ASYNC_RST       : boolean                       := TRUE   -- Allow asynchronous reset flag
  );
  port (
  -- External I/O
    rstn            : in  std_ulogic;                         -- Reset
    clk             : in  std_ulogic;                         -- Clock
  -- Internal I/O
    enable          : in  std_logic;                          -- Enable FETCH stage
    rst_sw          : in  std_logic;                          -- Software reset through APB
      -- Signals from APB registers
    desc_word_wr    : in  std_logic_vector(31 downto 0);      -- Descriptor word to be written on the Program Memory
    desc_word_wen   : in  std_logic;                          -- Write enable of a descriptor word write
      -- Signals for/from DECODE
    fetch_ready     : out std_logic;                          -- Descriptor ready to be read flag
    decode_read     : in  std_logic;                          -- Descriptor can be read flag from buffer
    pc              : out unsigned(PC_LEN - 1 downto 0);      -- PC of the word 0 descriptor fetched
    desc            : out desc_words;                         -- Descriptor words
      -- Debug signals
    irq             : out std_logic;                          -- Error interruption
    state           : out std_logic_vector(MAX_STATUS_LEN - 1 downto 0)
  );
end entity injector_fetch;

architecture rtl of injector_fetch is

  -----------------------------------------------------------------------------
  -- Types and reset constants declaration
  -----------------------------------------------------------------------------

  type program_memory is array (0 to 2**PC_LEN - 1) of std_logic_vector(31 downto 0);

  constant MAX_DESC_WORD_INDEX  : integer := log_2(MAX_DESC_WORDS);

  constant RESET_PROGRAM_MEMORY : program_memory  := (
    others => ( (31 downto 1 => '0') & '1' )  -- Reset descriptor words with asserted last bit.
  );

  constant RESET_DESC_WORDS     : desc_words      := (
    others => ( others => '0' )
  );


  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------

  -- Registers
  signal mem              : program_memory; -- Memory program to be written with injector program and to be fetched from.
  signal desc_buffer      : desc_words;     -- Descriptor words buffer for DECODE stage.
  signal pc_wr            : unsigned(PC_LEN downto 0);  -- PC for writing. Use overflow bit as error flag.
  signal pc_rd            : unsigned(PC_LEN downto 0);  -- PC for reading. Use overflow bit as error flag.
  signal desc_w_counter   : unsigned(MAX_DESC_WORD_INDEX downto 0); -- Counter to index desc_buffer words (overflow allowed).
  signal pc_desc          : unsigned(PC_LEN - 1 downto 0);          -- PC of the loading or loaded buffer descriptor.

  -- Signals
  signal desc_ready       : std_logic;      -- Descriptor ready to be read flag
  signal desc_index       : unsigned(MAX_DESC_WORD_INDEX - 1 downto 0); -- Actual index for desc_buffer words.
  signal desc_w_stop      : unsigned(MAX_DESC_WORD_INDEX - 1 downto 0); -- desc_buffer index to stop depending on type.
  signal desc_word_rd     : std_logic_vector(31 downto 0);              -- Descriptor word fetched from program memory.
  signal err_pc_wr_oom    : std_logic;      -- PC write overflow = out of memory for writing error flag.
  signal err_pc_oom       : std_logic;      -- PC overflow = out of memory error flag.
  signal err_type         : std_logic;      -- Unsupported type decode.
  

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
  err_pc_wr_oom <= pc_wr(PC_LEN);
  err_pc_oom    <= pc_rd(PC_LEN);

  -- Decode the descriptor type to set the respective desc_w_counter for each type in combinational logic.
  comb0 : process(desc_buffer(0)(5 downto 1))
  begin
    err_type    <= '0';
    
    case(desc_buffer(0)(5 downto 1)) is
      -- Operations that are encoded by one word
      when OP_DELAY =>
        desc_w_stop <= to_unsigned(0, desc_w_stop'length);

      -- Operations that are encoded by two words
      when OP_READ | OP_WRITE | OP_READ_FIX | OP_WRITE_FIX =>
        desc_w_stop <= to_unsigned(1, desc_w_stop'length);

      --when OP_BRANCH    =>  desc_w_stop <= to_unsigned(0, desc_w_stop'length);
      --when OP_META      =>  desc_w_stop <= to_unsigned(x, desc_w_stop'length); -- TODO: META TO BE IMPLEMENTED
      when others =>
        desc_w_stop <= (others => '0');
        err_type    <= '1';

    end case;
  end process comb0;

  -- Flag descriptor ready for reading when all the words for that type have been read.
  desc_ready    <= '1' when (desc_w_counter > ('0' & desc_w_stop)) else '0';

  -- Set the index to load the descriptor word into buffer 0 if on this clock cycle the descriptor in the buffer is being read.
  desc_index    <= (others => '0') when (desc_ready = '1' and decode_read = '1') else desc_w_counter(desc_index'range);

  -- Let the descriptor word fetch be combinational controlled by PC.
  desc_word_rd  <= mem(to_integer( pc_rd(PC_LEN - 1 downto 0) ));

  
  -----------------------------------------------------------------------------
  -- Sequential process
  -----------------------------------------------------------------------------
  
  seq0 : process(clk, rstn)
  begin
    if(rstn = '0' and ASYNC_RST) then
      pc_wr               <= (others => '0');
      pc_rd               <= (others => '0');
      pc_desc             <= (others => '0');
      desc_w_counter      <= (others => '0');
      mem                 <= RESET_PROGRAM_MEMORY;
      desc_buffer         <= RESET_DESC_WORDS;
    elsif rising_edge(clk) then
      if(rstn = '0' or rst_sw = '1') then
        pc_wr             <= (others => '0');
        pc_rd             <= (others => '0');
        pc_desc           <= (others => '0');
        desc_w_counter    <= (others => '0');
        mem               <= RESET_PROGRAM_MEMORY;
        desc_buffer       <= RESET_DESC_WORDS;
      else
        
      --------------------------------
      -- Program memory write logic --
      --------------------------------

        if(desc_word_wen = '1' and pc_wr(PC_LEN) = '0') then
          mem(to_integer( pc_wr(PC_LEN - 1 downto 0) )) <= desc_word_wr;
          pc_wr                                         <= pc_wr + 1;
        end if;


      -------------------------------
      -- Program memory read logic --
      -------------------------------

        -- Fetch new descriptor when enabled, no out of memory error and when no descriptor is prepared to be 
        -- read yet or will be read in this clock cycle.
        if(enable = '1' and err_pc_oom = '0') then
          if( desc_ready = '0' or (desc_ready and decode_read) = '1' ) then
            desc_buffer(to_integer(desc_index)) <= desc_word_rd;
          end if;

          -- Return to 0 if the last word of the last descriptor of the injector program.
          if(desc_buffer(0)(0) = '1' and desc_ready = '1') then
            pc_rd                             <= (others => '0');
          elsif(desc_ready = '0') then
            pc_rd                             <= pc_rd + 1;
          end if;

          if((desc_ready and decode_read) = '1') then
          -- Set the index counter to 1 for the word read after having read the descriptor.
            desc_w_counter                    <= to_unsigned(1, desc_w_counter'length);
            pc_desc                           <= pc_rd(PC_LEN - 1 downto 0);
          elsif(desc_ready = '0') then
          -- Or increment it if there's still more words to read from the program memory.
            desc_w_counter                    <= desc_w_counter + 1;
          end if;

        end if;


      end if;
    end if;
  end process seq0;

end architecture rtl;

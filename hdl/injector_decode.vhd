-----------------------------------------------------------------------------
-- Entity:      injector_decode
-- File:        injector_decode.vhd
-- Author:      Francis Fuentes
-- Description: DECODE stage in SafeTI Injector core pipeline.
------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library safety;
use safety.injector_pkg.all;


-------------------------------------------------------
-- Entity for DECODE stage in Injector core pipeline --
-------------------------------------------------------

entity injector_decode is
  generic (
    PC_LEN            : integer                     :=    4;  -- Length of PC register
    CORE_DATA_WIDTH   : integer range 8 to 1024     :=   32;  -- Data width of the injector core. [Only power of 2s allowed]
    MAX_SIZE_BURST    : integer range 8 to 4096     := 1024;  -- Maximum number of bytes per transaction
    ASYNC_RST         : boolean                     := TRUE   -- Allow asynchronous reset flag
  );
  port (
  -- External I/O
    rstn              : in  std_ulogic;                       -- Reset
    clk               : in  std_ulogic;                       -- Clock
  -- Internal I/O
    enable            : in  std_logic;                        -- Enable DECODE stage
    rst_sw            : in  std_logic;                        -- Software reset through APB
    queue_mode_en     : in  std_logic;                        -- Queue mode enable signal
      -- Signals from/for FETCH
    fetch_ready       : in  std_logic;                        -- Descriptor ready to be read flag
    decode_read       : out std_logic;                        -- Descriptor can be read flag
    fetch_pc          : in  unsigned(PC_LEN - 1 downto 0);    -- PC of the fetched word 0 descriptor
    desc              : in  desc_words;                       -- Fetched descriptor words
      -- Signals for/from EXE
    decode_ready      : out std_logic;                        -- Decoded descriptor ready to be read
    exe_read          : in  std_logic;                        -- Decoded descriptor can be read
    exe_pc            : out unsigned(PC_LEN - 1 downto 0);    -- PC of the decoded descriptor
    exe_data          : out bus_decode_exe;                   -- Control signals for operation execution
      -- Debug signals
    irq               : out std_logic;                        -- Error interruption
    state             : out std_logic_vector(MAX_STATUS_LEN - 1 downto 0)
);
end entity injector_decode;

architecture rtl of injector_decode is

  -------------------------------------------------------------------------------
  -- Labels
  -------------------------------------------------------------------------------

    -- Descriptor types (action_type)
    constant OP_DELAY       : std_logic_vector(4 downto 0) := "00000";
    constant OP_READ        : std_logic_vector(4 downto 0) := "00001";
    constant OP_WRITE       : std_logic_vector(4 downto 0) := "00010";
    constant OP_BRANCH      : std_logic_vector(4 downto 0) := "00100"; -- TODO: Implement the BRANCH operation.
    constant OP_READ_FIX    : std_logic_vector(4 downto 0) := "00101";
    constant OP_WRITE_FIX   : std_logic_vector(4 downto 0) := "00110";
    constant OP_READ_SEQ    : std_logic_vector(4 downto 0) := "01001";
    constant OP_WRITE_SEQ   : std_logic_vector(4 downto 0) := "01010";
    constant OP_META        : std_logic_vector(4 downto 0) := "11111"; -- TODO: Use META type to add new descriptor words.


  -----------------------------------------------------------------------------
  -- Types and reset constants declaration
  -----------------------------------------------------------------------------

  -- Descriptor control field (common on all descriptor types on word 0 + PC)
  type descriptor_control is record
    pc              : unsigned(PC_LEN - 1 downto 0);-- PC of the decoded descriptor
    act_subm        : submodule_bit;                -- Active submodule on EXE during iterations
    count           : unsigned(10 downto 0);        -- Iteration count
    size_seq        : unsigned(14 downto 0);        -- Original descriptor size for SEQ operations (+1 bit for overflow)
    irq_en          : std_logic;                    -- Interruption flag at descriptor completion
    last            : std_logic;                    -- Last descriptor of injector program
  end record descriptor_control;

  constant RESET_DESCRIPTOR_CONTROL : descriptor_control := (
    pc              => (others => '0'),
    act_subm        => (others => '0'),
    count           => (others => '0'),
    size_seq        => (others => '0'),
    irq_en          => '0',
    last            => '0'
  );

  constant RESET_OPERATION_RD_WR : operation_rd_wr := (
    size_left       => (others => '0'),
    size_burst      => (others => '0'),
    addr            => (others => '0'),
    addr_fix        => '0'
  );

  -- Reset values for submodule_enable type
  constant RESET_SUBMODULE_BIT : submodule_bit := (
    delay_sub         => '0',
    read_sub          => '0',
    write_sub         => '0'
  );

  -- Set a constant to manage maximum size access allowed on SEQ operations.
  constant MAX_SIZE_BURST_SEQ : integer :=  if_else(16384 < MAX_SIZE_BURST, 16384, MAX_SIZE_BURST);


  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------

  -- Registers
  signal common     : descriptor_control;
  signal disable    : std_logic;      -- Disable EXE's descriptor feed due to last descriptor has been sent.
  signal rd_wr      : operation_rd_wr;
  signal delay      : operation_delay;
  signal state_reg  : std_logic_vector(state'range);

  -- Control signals
  signal desc_read  : std_logic;      -- Descriptor is being read from FETCH signal
  signal desc_rd_en : std_logic;      -- Enable the read of a descriptor from FETCH
  signal desc_ready : std_logic;      -- Decoded descriptor ready to be sent to EXE signal
  signal desc_sent  : std_logic;      -- Sent decoded descriptor to EXE signal

  signal act_subm   : submodule_bit;  -- Combinational select submodule bus
  signal desc_act   : std_logic;      -- Active descriptor to be executed flag
  signal no_rep     : std_logic;      -- High when all iterations but last are complete
  signal err_type   : std_logic;      -- Error flag

  -- Descriptor fields
    -- Basic
  signal desc_last  : std_logic;
  signal desc_type  : std_logic_vector( 4 downto 0);
  signal desc_irq   : std_logic;
  signal desc_count : std_logic_vector( 5 downto 0);
  signal desc_size  : std_logic_vector(rd_wr.size_burst'range);
  signal desc_addr  : std_logic_vector(31 downto 0);
    -- SEQ access operations
  signal desc_seq_size  : std_logic_vector(common.size_seq'high - 1 downto 0);
  signal desc_seq_count : std_logic_vector(common.count'range);

  -- Descriptor type specific signals
  signal size_burst : unsigned(rd_wr.size_burst'range);
  signal desc_fix_addr  : std_logic; -- Used on FIX address operations
  signal desc_seq_addr  : std_logic; -- Used on SEQ address operations


begin -- rtl

  -----------------------------------------------------------------------------
  -- Assignments
  -----------------------------------------------------------------------------

  -- I/O signal assignments
  decode_read           <= desc_rd_en;        -- Prepared to decode new descriptor
  decode_ready          <= desc_ready;        -- Descriptor decoded and prepared to be executed
  exe_pc                <= common.pc;         -- Word 0 PC of the decoded descriptor
  exe_data.subm_enable  <= common.act_subm;   -- Decoded descriptor type to select EXE submodule
  exe_data.irq_desc     <= common.irq_en;     -- Interruption enable at descriptor completion
  exe_data.last_desc    <= common.last;       -- Last descriptor on injector program
  exe_data.last_count   <= no_rep;            -- Last iteration execution of the descriptor
  exe_data.delay        <= delay;             -- Decoded descriptor data to execute DELAY
  exe_data.rd_wr        <= rd_wr;             -- Decoded descriptor data to execute READ or WRITE injection vector
  state                 <= state_reg;         -- DECODE state
  irq                   <= err_type;          -- Interrupt error signal

  -- Descriptor field assignments
  ---- DESCRIPTOR WORD 0 FIELD FORMAT
  ----
  ----[31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 09 08 07 06 05 04 03 02 01 00]-- bits
  ----|                      size                              |      count      | A|  RD/WR type  | B|-- word 0 fields of rd/wr and delay type
  ----|  seq_count   |                      size               |    seq_count    | A|  RD/WR type  | B|-- word 0 fields of rd/wr sequential type
  ----
  ---- A: Interruption enable at descriptor completion          B: Last descriptor of the injector program
  ----
  desc_last       <= desc(0)(0);
  desc_type       <= desc(0)( 5 downto  1);
  desc_irq        <= desc(0)(6);
  desc_count      <= desc(0)(12 downto  7);
  desc_size       <= desc(0)(31 downto 13);
  desc_addr       <= desc(1);

  desc_seq_size   <= desc(0)(26 downto 13);
  desc_seq_count  <= desc(0)(31 downto 27) & desc(0)(12 downto 7);


  -- Descriptor is yet to be executed if any of the active submodule bits are high.
  desc_act        <= '1' when (common.act_subm /= RESET_SUBMODULE_BIT) else '0';

  -- Prepared to read descriptor from FETCH when DECODE is enabled and there's no more iterations.
  desc_rd_en      <= enable and no_rep and not(desc_act xor desc_sent);

  -- Signal high when descriptor is being read from FETCH stage.
  desc_read       <= desc_rd_en and fetch_ready;

  -- Signal high when decoded descriptor is ready to be sent to EXE stage.
  desc_ready      <= desc_act and not(disable);

  -- Signal high when the decoded descriptor is being sent to EXE stage.
  desc_sent       <= desc_ready and exe_read;

  -- No repetitions required signal.
  no_rep          <= '1' when (common.count = (common.count'range => '0')) else '0';


  -- Decode the incoming descriptor type onto the active_submodule bit array.
  comb0 : process(desc_type)
  begin
    -- Default values
    err_type      <= '0';
    act_subm      <= (others => '0');
    desc_fix_addr <= '0';
    desc_seq_addr <= '0';

    case(desc_type) is
      when OP_DELAY =>
        act_subm.delay_sub  <= '1';

      when OP_READ  =>
        act_subm.read_sub   <= '1';

      when OP_WRITE =>
        act_subm.write_sub  <= '1';

      when OP_READ_FIX  =>
        act_subm.read_sub   <= '1';
        desc_fix_addr       <= '1';

      when OP_WRITE_FIX =>
        act_subm.write_sub  <= '1';
        desc_fix_addr       <= '1';

      when OP_READ_SEQ =>
        act_subm.read_sub   <= '1';
        desc_seq_addr       <= '1';

      when OP_WRITE_SEQ =>
        act_subm.write_sub  <= '1';
        desc_seq_addr       <= '1';

      when others =>
        err_type            <= '1';

    end case;
  end process comb0;

  -- Pre-compute the burst size for READ and WRITE operations for the first transaction.
  comb1 : process(desc_size, desc_seq_size, desc_seq_addr)
  begin
    if(desc_seq_addr = '0') then -- READ, WRITE and FIX variants have a MAX_SIZE_BURST size.
      if(unsigned(desc_size) > to_unsigned(MAX_SIZE_BURST - 1, desc_size'length)) then
        size_burst <= to_unsigned(MAX_SIZE_BURST - 1, desc_size'length);
      else
        size_burst <= unsigned(desc_size);
      end if;
    else -- READ_SEQ and WRITE_SEQ variants have a MAX_SIZE_BURST_SEQ size.
      if(unsigned(desc_seq_size) > to_unsigned(MAX_SIZE_BURST_SEQ - 1, desc_seq_size'length)) then
        size_burst <= to_unsigned(MAX_SIZE_BURST_SEQ - 1, desc_size'length);
      else
        size_burst <= (size_burst'high downto desc_seq_size'length => '0') & unsigned(desc_seq_size);
      end if;
    end if;

  end process comb1;


  -----------------------------------------------------------------------------
  -- Sequential process
  -----------------------------------------------------------------------------

  seq0 : process(clk, rstn)
    variable size_seq_t : unsigned(desc_size'range);
  begin
    if(rstn = '0' and ASYNC_RST) then
      common      <= RESET_DESCRIPTOR_CONTROL;
      disable     <= '0';
      rd_wr       <= RESET_OPERATION_RD_WR;
      delay       <= (others => (others => '0'));
      state_reg   <= DEBUG_STATE_IDLE;
    elsif rising_edge(clk) then
      if(rstn = '0' or rst_sw = '1') then
        common    <= RESET_DESCRIPTOR_CONTROL;
        disable   <= '0';
        rd_wr     <= RESET_OPERATION_RD_WR;
        delay     <= (others => (others => '0'));
        state_reg <= DEBUG_STATE_IDLE;
      else

        if(enable = '1') then
        -----------------------------------------
        -- Descriptor iteration counter update --
        -----------------------------------------
          if(desc_sent = '1') then
            if(no_rep = '0') then
              -- Decrease iteration counter after a EXE read if not 0.
              common.count      <= common.count - 1;
              state_reg         <= DEBUG_STATE_REPETITION;
              -- If the execution type is SEQ, increase address for next block.
              if(desc_seq_addr = '1') then
                rd_wr.addr      <= rd_wr.addr + common.size_seq;
              end if;
            else
              -- Disable active decoded descriptor if last iteration is being executed.
              common.act_subm   <= (others => '0');
              if(desc_read = '0') then
                state_reg <= DEBUG_STATE_IDLE;
              end if;
            end if;
          end if; -- descriptor read by EXE

        ------------------------------------
        -- Descriptor decode and register --
        ------------------------------------
          if(desc_read = '1') then

            -- Common descriptor signals.
            common.pc           <= fetch_pc;
            common.act_subm     <= act_subm;
            common.irq_en       <= desc_irq;
            common.last         <= desc_last;

            -- Count repetitions differs between SEQ and non-SEQ operations.
            if(desc_seq_addr = '0') then
              common.count      <= (common.count'high downto desc_count'length => '0') & unsigned(desc_count);
            else
              common.count      <= unsigned(desc_seq_count);
            end if;

            -- Restore default register values on descriptor completion.
            rd_wr               <= RESET_OPERATION_RD_WR;
            delay               <= (others => (others => '0'));


            -- Operation specific signals.
              -- DELAY
            if(act_subm.delay_sub = '1') then
              delay.num_cycles  <= unsigned(desc_size);
            end if;

              -- READ and WRITE
            if(act_subm.read_sub = '1' or act_subm.write_sub = '1') then -- RD and WR share same DECODE registers.
              rd_wr.addr        <= unsigned(desc_addr);
              rd_wr.addr_fix    <= desc_fix_addr;
              if(desc_seq_addr = '1') then -- If the operation is SEQ, take into account the narrow size_seq field.
                common.size_seq <= unsigned('0' & desc_seq_size) + 1; -- +1 for real size, so the addition with address is correct
                size_seq_t      := (size_burst'high downto desc_seq_size'length => '0') & unsigned(desc_seq_size) - size_burst;
                rd_wr.size_left   <= (rd_wr.size_left'high downto desc_seq_size'length => '0') & size_seq_t(desc_seq_size'high downto 0);
                rd_wr.size_burst  <= size_burst;
              else
                rd_wr.size_left   <= unsigned(desc_size) - size_burst;
                rd_wr.size_burst  <= size_burst;
              end if;
            end if;

            -- Disable sending the descriptor being read from FETCH if the last sent
            -- was the last on the program and the queue mode is disabled.
            disable             <= common.last and not(queue_mode_en);

            -- Set the DECODE state for first decode of the descriptor.
            state_reg           <= DEBUG_STATE_1st_DECODE;

          end if; -- descriptor read from FETCH

        else    -- DECODE stage disabled

          -- After an injetor disable due to program completion, reset the disable register.
          disable               <= '0';

        end if;


      end if;
    end if;
  end process seq0;

end architecture rtl;

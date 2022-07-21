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
  PC_LEN            : integer                       := 4;   -- Length of PC register
  ASYNC_RST         : boolean                       := TRUE -- Allow asynchronous reset flag
);
port (
  -- External I/O
  rstn              : in  std_ulogic;                       -- Reset
  clk               : in  std_ulogic;                       -- Clock
  -- Internal I/O
  enable            : in  std_logic;                        -- Enable DECODE stage
  rst_sw            : in  std_logic;                        -- Software reset through APB
    -- Signals from/for FETCH
  fetch_ready       : in  std_logic;                        -- Descriptor ready to be read flag
  decode_read       : out std_logic;                        -- Descriptor can be read flag
  fetch_pc          : in  unsigned(PC_LEN - 1 downto 0);    -- PC of the fetched word 0 descriptor
  desc              : in  desc_words;                       -- Fetched descriptor words
    -- Signals for/from EXE
  decode_ready      : out std_logic;                        -- Decoded descriptor ready to be read
  exe_read          : in  std_logic;                        -- Decoded descriptor can be read
  exe_pc            : out unsigned(PC_LEN - 1 downto 0);    -- PC of the decoded descriptor
  exe_act_subm      : out submodule_enable;                 -- Active submodule
  exe_rd_wr         : out operation_rd_wr;                  -- Decoded RD and WR operation data
  exe_delay         : out operation_delay;                  -- Decoded DELAY operation data
    -- Debug signals
  irq               : out std_logic;                        -- Error interruption
  state             : out std_logic_vector(MAX_STATUS_LEN - 1 downto 0)
);
end entity injector_decode;

architecture rtl of injector_decode is

  -- DESCRIPTOR WORD 0 FIELD FORMAT
  -- 
  --[31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 09 08 07 06 05 04 03 02 01 00]-- bits
  --|                      size                              |      count      | A|  RD/WR type  | B|-- word 0 fields of rd/wr and delay type
  --
  -- A: Interruption enable at descriptor completion          B: Last descriptor of the injector program (PC = 0 after completion)
  --


  -----------------------------------------------------------------------------
  -- Types and reset constants declaration
  -----------------------------------------------------------------------------

  -- Descriptor control field (common on all descriptor types on word 0 + PC)
  type descriptor_control is record
    active        : std_logic;                    -- Decoded descriptor prepared to be executed
    pc            : unsigned(PC_LEN - 1 downto 0);-- PC of the decoded descriptor
    act_subm      : submodule_enable;             -- Active submodule on EXE
    count         : unsigned(5 downto 0);         -- Iteration count
    irq_en        : std_logic;                    -- Interruption flag at descriptor completion
    last          : std_logic;                    -- Last descriptor of injector program
  end record descriptor_control;

  constant RESET_DESCRIPTOR_CONTROL : descriptor_control := (
    active        => '0',
    pc            => (others => '0'),
    act_subm      => (others => '0'),
    count         => (others => '0'),
    irq_en        => '0',
    last          => '0'
  );

  constant RESET_OPERATION_RD_WR : operation_rd_wr := (
    size          => (others => '0'),
    addr          => (others => '0'),
    addr_fix      => '0'
  );


  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------

  -- Registers
  signal common     : descriptor_control;
  signal rd_wr      : operation_rd_wr;
  signal delay      : operation_delay;

  -- Control signals
  signal rd_desc_en : std_logic;  -- Read new descriptor from FETCH
  signal act_subm   : submodule_enable; -- Select submodule signals
  signal no_rep     : std_logic;  -- High when all iterations but last are complete
  signal err_type   : std_logic;  -- Error flag

  -- Data signals
  signal size_incr  : unsigned(rd_wr.size'range);


begin -- rtl

  -----------------------------------------------------------------------------
  -- Assignments
  -----------------------------------------------------------------------------

  -- I/O signal assignments
  decode_read <= rd_desc_en;    -- Prepared to decode new descriptor
  decode_ready<= common.active; -- Descriptor decoded and prepared to be executed
  exe_pc      <= common.pc;     -- Word 0 PC of the decoded descriptor
  exe_act_subm<= act_subm;      -- Decoded descriptor type to select EXE submodule
  exe_rd_wr   <= rd_wr;         -- Decoded descriptor data to execute injection vector
  exe_delay   <= delay;         -- Decoded descriptor data to execute delay
  irq         <= err_type;      -- Interrupt error signal

  -- Prepared to read descriptor from FETCH when DECODE is enabled and there's no more iterations.
  rd_desc_en  <= enable and no_rep;

  -- No repetitions required signal.
  no_rep      <= '1' when common.count = (common.count'range => '0')  else '0';

  -- Decode the descriptor type onto the active_submodule bit array.
  comb0 : process(desc(0)(5 downto 1))
  begin
    err_type  <= '0';
    act_subm  <= (others => '0');
    case(desc(0)(5 downto 1)) is
      when OP_DELAY =>
        act_subm.delay_en <= '1';

      when OP_READ | OP_READ_FIX =>
        act_subm.read_en  <= '1';

      when OP_WRITE | OP_WRITE_FIX =>
        act_subm.write_en <= '1';

      when others =>
        err_type    <= '1';

    end case;
  end process comb0;

  -- Increment descriptor size on decode.
  size_incr  <= unsigned('0' & desc(0)(31 downto 13)) + 1;


  -----------------------------------------------------------------------------
  -- Sequential process
  -----------------------------------------------------------------------------
  
  seq0 : process(clk, rstn)
  begin
    if(rstn = '0' and ASYNC_RST) then
      common              <= RESET_DESCRIPTOR_CONTROL;
      rd_wr               <= RESET_OPERATION_RD_WR;
      delay               <= (others => (others => '0'));
    elsif rising_edge(clk) then
      if(rstn = '0' or rst_sw = '1') then
        common            <= RESET_DESCRIPTOR_CONTROL;
        rd_wr             <= RESET_OPERATION_RD_WR;
        delay             <= (others => (others => '0'));
      else

        ------------------------------------
        -- Descriptor decode and register --
        ------------------------------------
        if(rd_desc_en and fetch_ready = '1') then

          -- Common descriptor signals.
          common.active       <= '1';
          common.pc           <= fetch_pc;
          common.act_subm     <= act_subm;
          common.count        <= unsigned(desc(0)(12 downto 7));
          common.irq_en       <= desc(0)(6);
          common.last         <= desc(0)(0);

          -- Restore default register values on descriptor completion.
          rd_wr               <= RESET_OPERATION_RD_WR;
          delay               <= (others => (others => '0'));

          -- Operation specific signals.
          if(act_subm.delay_en = '1') then
            delay.num_cycles  <= size_incr;
          end if;

          if(act_subm.read_en or act_subm.write_en = '1') then -- RD and WR share same DECODE registers.
            rd_wr.size        <= size_incr;
            rd_wr.addr        <= desc(1);
            rd_wr.addr_fix    <= desc(0)(3);
          end if;
          
        end if; -- descriptor read from FETCH


        -----------------------------------------
        -- Descriptor iteration counter update --
        -----------------------------------------
        if(common.active and exe_read = '1') then
          if(no_rep = '1') then
            common.active       <= '0';
          else
            common.count        <= common.count - 1;
          end if;
        end if; -- descriptor read by EXE


      end if;
    end if;
  end process seq0;

end architecture rtl;
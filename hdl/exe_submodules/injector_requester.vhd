-----------------------------------------------------------------------------
-- Entity:      Injector REQUESTER submodule
-- File:        injector_requester.vhd
-- Author:      Francisco Fuentes
-- Description: Transaction request engine for segmenting READ and WRITE descriptors to the network interface.
------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library safety;
use safety.injector_pkg.all;


---------------------------------------------------
-- Entity for REQUEST submodule of the EXE stage --
---------------------------------------------------

entity injector_requester is
  generic (
    MAX_SIZE_BURST    : integer range 8 to 4096     := 1024;  -- Maximum number of bytes per transaction
    ASYNC_RST         : boolean                     := TRUE   -- Allow asynchronous reset flag
  );
  port (
  -- External I/O
    rstn              : in  std_ulogic;                       -- Reset
    clk               : in  std_ulogic;                       -- Clock
    -- Interface Bus signals
    ib_req_grant      : in  std_logic;                        -- Request granted by network interface
    ib_req            : out std_logic;                        -- Transaction request for the network interface
    ib_valid          : in  std_logic;                        -- Valid data beat from ongoing transaction
    ib_done           : in  std_logic;                        -- Last valid data beat of the last requested transaction
    ib_addr           : out std_logic_vector(31 downto 0);    -- Address where to execute the transaction
    ib_size           : out std_logic_vector(11 downto 0);    -- Encoded number of bytes to transfer (-1 from real transfer size)
    ib_addr_fix       : out std_logic;                        -- Transaction to execute on fixed address.
  -- Internal I/O
    rst_sw            : in  std_logic;  -- Synchronous active high SW reset
    program           : in  std_logic;  -- Program submodule flag
    start             : in  std_logic;  -- Start execution flag
    hold              : in  std_logic;  -- Hold execution flag
    busy              : out std_logic;  -- Busy submodule flag
    full              : out std_logic;  -- Full submodule flag
    desc_data         : in  operation_rd_wr; -- Operation data
    error             : out std_logic;       -- Error flag
    status            : out DEBUG_STATE -- Status of the transaction
  );
end entity injector_requester;

architecture rtl of injector_requester is

  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------

  -- Registers
  signal busy_reg     : std_logic;        -- Preparing/active operation
  signal full_reg     : std_logic;        -- Descriptor buffer full flag
  signal req_reg      : std_logic;        -- Request transaction register
  signal last_reg     : std_logic;        -- Last transfer register
  signal desc_reg     : operation_rd_wr := RESET_OPERATION_RD_WR;  -- Active descriptor or to be executed
  signal status_reg   : DEBUG_STATE;
  signal trans_reg    : std_logic;        -- Data transfer flag

  -- Signals
  signal last_req     : std_logic;        -- Next request is last
  signal error_start  : std_logic;        -- Error start without descriptor
  signal error_data   : std_logic;        -- Error data transfer without request


begin

  -----------------------------------------------------------------------------
  -- Assignments
  -----------------------------------------------------------------------------

  -- I/O assignments
  ib_req          <= req_reg;
  ib_addr         <= std_logic_vector(desc_reg.addr);
  ib_size         <= std_logic_vector(to_unsigned(MAX_SIZE_BURST-1, ib_size'length)) when (last_req = '0') else desc_reg.size(ib_size'range);
  ib_addr_fix     <= desc_reg.addr_fix;
  busy            <= busy_reg and not(last_reg and ib_done);
  full            <= full_reg;
  error           <= error_start or error_data;
  status          <= DEBUG_STATE_UNEXPECTED_START when (error_start = '1')
                else DEBUG_STATE_UNEXPECTED_DATA  when (error_data = '1')
                else DEBUG_STATE_HOLD             when ((hold and trans_reg) = '1')
                else status_reg;

  -- Last request flag asserted when ongoing descriptor size is lower than MAX_SIZE_BURST.
  last_req        <= '1' when (unsigned(desc_reg.size) < to_unsigned(MAX_SIZE_BURST, desc_reg.size'length)) else '0';

  -- Error flags
  error_start     <= start and not(full_reg);
  error_data      <= ib_valid and not(busy_reg);


  -----------------------------------------------------------------------------
  -- Sequential Process
  -----------------------------------------------------------------------------

  seq0 : process(clk, rstn)
  begin
    if(rstn = '0' and ASYNC_RST) then
      busy_reg          <= '0';
      full_reg          <= '0';
      req_reg           <= '0';
      last_reg          <= '0';
      trans_reg         <= '0';
      desc_reg          <= RESET_OPERATION_RD_WR;
      status_reg        <= DEBUG_STATE_IDLE;
    elsif rising_edge(clk) then
      if(rstn = '0') then
        busy_reg        <= '0';
        full_reg        <= '0';
        req_reg         <= '0';
        last_reg        <= '0';
        trans_reg       <= '0';
        desc_reg        <= RESET_OPERATION_RD_WR;
        status_reg      <= DEBUG_STATE_IDLE;
      else

        -- Operation logic
        if( (full_reg = '1' and start = '1') or busy_reg = '1') then
          busy_reg      <= not(last_reg and ib_done) or start;
          req_reg       <= req_reg or start;
          trans_reg     <= trans_reg and not(ib_done);
          if(last_reg = '0') then
            last_reg    <= last_req and start;
          else
            last_reg    <= last_reg and not(ib_done);
          end if;
        end if;

        -- Request logic
        if(req_reg = '1' and ib_req_grant = '1') then
          last_reg      <= last_req;
          if(last_req = '0') then
            desc_reg.size <= std_logic_vector(unsigned(desc_reg.size) - to_unsigned(MAX_SIZE_BURST, desc_reg.size'length));
          else
            full_reg    <= '0';
            req_reg     <= '0';
          end if;
          if(desc_reg.addr_fix = '0') then
            desc_reg.addr <= std_logic_vector(unsigned(desc_reg.addr) + to_unsigned(MAX_SIZE_BURST, desc_reg.addr'length));
          end if;
          trans_reg     <= '1';
        end if;

        -- Program logic
        if(full_reg = '0' and program = '1') then
          full_reg      <= '1';
          desc_reg      <= desc_data;
        end if;

        -- Status buffer (inverse order of normal execution)
        if(last_reg = '1' and ib_done = '1') then
          status_reg    <= DEBUG_STATE_IDLE;
        elsif(trans_reg = '1' and last_reg = '1') then
          status_reg    <= DEBUG_STATE_LAST_TRANSFER;
        elsif(last_reg = '1') then
          status_reg    <= DEBUG_STATE_LAST;
        elsif(trans_reg = '1') then
          status_reg    <= DEBUG_STATE_DATA_TRANSFER;
        elsif(busy_reg = '1') then
          status_reg    <= DEBUG_STATE_OPERATION;
        elsif(full_reg = '0' and program = '1') then
          status_reg    <= DEBUG_STATE_PROGRAMMED;
        end if;

      end if;
    end if;
  end process seq0;


end architecture rtl;

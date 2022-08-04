-----------------------------------------------------------------------------   
-- Entity:      Injector READ submodule
-- File:        injector_read.vhd
-- Author:      Francisco Fuentes, Oriol Sala
-- Description: Read engine to ensure correct read by part of the network interface.
------------------------------------------------------------------------------ 
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library safety;
use safety.injector_pkg.all;


------------------------------------------------
-- Entity for READ submodule of the EXE stage --
------------------------------------------------

entity injector_read is
  generic (
    CORE_DATA_WIDTH   : integer range 8 to 1024     :=   32;  -- Data width of the injector core. [Only power of 2s allowed]
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
    enable            : in  std_logic;                        -- Enable descriptor execution
    rst_sw            : in  std_logic;                        -- Software reset through APB
    start             : in  std_logic;                        -- Start descriptor execution flag
    busy              : out std_logic;                        -- Ongoing descriptor execution flag
    desc_data         : in  operation_rd_wr;                  -- Control data to execute descriptor
    error             : out std_logic;                        -- Error flag
    status            : out std_logic_vector(MAX_STATUS_LEN - 1 downto 0) -- Status of the READ transaction
  );
end entity injector_read;

architecture rtl of injector_read is

  -----------------------------------------------------------------------------
  -- Types and reset constants declaration
  -----------------------------------------------------------------------------

  -- Reset values
  constant RESET_OPERATION_RD_WR : operation_rd_wr := (
    size_left       => (others => '0'),
    size_burst      => (others => '0'),
    addr            => (others => '0'),
    addr_fix        => '0'
  );


  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------

  -- Registers
  signal req_reg          : std_logic;        -- Request transaction register
  signal desc_ongoing     : operation_rd_wr;  -- Register of the ongoing execution of the descriptor.
  signal size_transf_rem  : unsigned(desc_ongoing.size_burst'range); -- Remaining bytes to transfer of the requested transaction.
  signal transfer_on      : std_logic;        -- Ongoing transfer flag of the last granted transaction.
  signal status_reg       : std_logic_vector(MAX_STATUS_LEN - 1 downto 0); -- Status register.

  -- Signals
  signal req              : std_logic;        -- Transaction request.
  signal req_granted      : std_logic;        -- Transaction has been granted flag.
  signal desc_to_exe      : operation_rd_wr;  -- Multiplex bus between DECODE and internal submodule registers.
  signal not_last_transf  : std_logic;        -- Ongoing transfer of a granted transaction. Lowers on last transfer.
  signal error_start      : std_logic;        -- Incorrect timing of the signal start from DECODE.
  signal error_valid      : std_logic;        -- Incorrect timing of the signal ib_valid from interface.
  signal error_done       : std_logic;        -- Incorrect timing of the signal ib_done from interface.

  
begin

  -----------------------------------------------------------------------------
  -- Assignments
  -----------------------------------------------------------------------------

  -- I/O assignments
  ib_req          <= req;
  ib_addr         <= std_logic_vector(desc_to_exe.addr);
  ib_size         <= std_logic_vector(desc_to_exe.size_burst(ib_size'range));
  ib_addr_fix     <= desc_to_exe.addr_fix;
  busy            <= req_reg or transfer_on;
  error           <= error_start or error_valid or error_done;
  status          <= status_reg;


  -- Request combinational conditions. Do not request any transaction if EXE is disabled or if it's transfering.
  req             <= enable and (start or req_reg) and not(transfer_on);

  -- Request grant conditions.
  req_granted     <= ib_req_grant and req;

  -- Multiplex control data to execute descriptor between input and stored on register.
  desc_to_exe     <= desc_ongoing when (req_reg = '1' or transfer_on = '1') else desc_data;

  -- Set the not_last_transf flag to check for when the ib_done flag must be high.
  not_last_transf <= '1' when (size_transf_rem > to_unsigned(CORE_DATA_WIDTH/8, size_transf_rem'length)) else '0';

  -- Error signaling assignment.
    -- The start signal must not be high when a descriptor is being executed.
  error_start     <= start and (transfer_on or req_reg);
    -- The valid signal must not be high when no transfer is expected.
  error_valid     <= ib_valid and not(transfer_on);
    -- The done signal must not be high when valid is not high and it is not the last transfer.
  error_done      <= ib_done and (not(ib_valid) or not(not_last_transf) or not(transfer_on));


  -----------------------------------------------------------------------------
  -- Sequential Process
  -----------------------------------------------------------------------------

  seq0 : process(clk, rstn)
  begin
    if(rstn = '0' and ASYNC_RST) then
      req_reg             <= '0';
      desc_ongoing        <= RESET_OPERATION_RD_WR;
      size_transf_rem     <= (others => '0');
      transfer_on         <= '0';
      status_reg          <= DEBUG_STATE_IDLE;
    elsif rising_edge(clk) then
      if(rstn = '0') then
        req_reg           <= '0';
        desc_ongoing      <= RESET_OPERATION_RD_WR;
        size_transf_rem   <= (others => '0');
        transfer_on       <= '0';
        status_reg        <= DEBUG_STATE_IDLE;
      else

        if(transfer_on = '0') then

        ------------------------------------
        -- Transaction request generation --
        ------------------------------------

          if(req_granted = '1') then
            -- At request granted, update size_left and compute next transfer size.
            if(desc_to_exe.size_left > to_unsigned(MAX_SIZE_BURST, desc_to_exe.size_left'length)) then
              desc_ongoing.size_left  <= desc_to_exe.size_left - to_unsigned(MAX_SIZE_BURST, desc_to_exe.size_left'length);
              desc_ongoing.size_burst <= to_unsigned(MAX_SIZE_BURST - 1, desc_to_exe.size_burst'length);
            else
              desc_ongoing.size_left  <= (others => '0');
              desc_ongoing.size_burst <= desc_to_exe.size_left - 1;
            end if;

            if(desc_to_exe.addr_fix = '0') then 
              desc_ongoing.addr       <= desc_to_exe.addr + to_unsigned(MAX_SIZE_BURST, desc_to_exe.addr'length);
            else
              desc_ongoing.addr       <= desc_to_exe.addr;
            end if;

            desc_ongoing.addr_fix     <= desc_to_exe.addr_fix;

            -- Set the transfer size of the transaction.
            size_transf_rem           <= desc_ongoing.size_burst + 1;
            transfer_on               <= '1';

            -- Set the request register to perform more request depending if there's more data to be transfered.
            if(desc_to_exe.size_left /= (desc_to_exe.size_left'range => '0')) then
              req_reg                 <= '1';
            else
              req_reg                 <= '0';
            end  if;

          elsif(start = '1') then -- No request has been granted, but DECODE called for action.
            desc_ongoing              <= desc_data;
            req_reg                   <= '1';
          end if;


        -------------------------
        -- Transfer management --
        -------------------------

        elsif(ib_valid = '1') then
        -- The data transfer logic is always enabled in order to not block any ongoing transaction on the network.
          if(not_last_transf = '1') then
            if(ib_done = '1') then  -- Incorrect done timing, set transaction has ended anyways.
              size_transf_rem       <= (others => '0');
              transfer_on           <= '0';
            else
              size_transf_rem       <= size_transf_rem - CORE_DATA_WIDTH/8;
            end if;
          else -- Correct done timing.
            size_transf_rem         <= (others => '0');
            transfer_on             <= '0';
          end if;
        end if;


        -- Set the debug state. It shows the present state of the execution, so it must load the state of the next clock cycle.
        if(error_start = '1') then                                    -- Error due to unexpected start signaling from DECODE.
          status_reg                <= DEBUG_STATE_UNEXPECTED_START;
        elsif(error_done = '1') then                                  -- Error due to unexpected ib_done from interface.
          status_reg                <= DEBUG_STATE_UNEXPECTED_DONE;
        elsif(error_valid = '1') then                                 -- Error due to unexpected ib_valid from interface.
          status_reg                <= DEBUG_STATE_UNEXPECTED_DATA;
        elsif( (start = '1' or req = '1') and req_granted = '0') then -- Start descriptor execution, so a request is done but not granted.
          status_reg                <= DEBUG_STATE_REQUEST;
        elsif(req_granted = '1' or not_last_transf = '1') then        -- Request is being granted, data transfer starts.
          status_reg                <= DEBUG_STATE_DATA_TRANSFER;
        elsif(not_last_transf = '0' and ib_done = '0') then           -- All data but last read is completed, waiting for done.
          status_reg                <= DEBUG_STATE_WAIT_DONE;
        elsif(ib_done = '1') then                                     -- Return to IDLE.
          status_reg                <= DEBUG_STATE_IDLE;
        end if;

        
        -- Software reset only affects the injector registers related to the descriptor, not the ongoing transfer.
        if(rst_sw = '1') then
          req_reg           <= '0';
          desc_ongoing      <= RESET_OPERATION_RD_WR;
        end if;

        
      end if;
    end if;
  end process seq0;

  
end architecture rtl;

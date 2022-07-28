-----------------------------------------------------------------------------   
-- Entity:      Injector Read Interface
-- File:        injector_read_if.vhd
-- Author:      Oriol Sala
-- Description: Read engine to read data from memory through a generic bus
-- master interface.
------------------------------------------------------------------------------ 
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library safety;
use safety.injector_pkg.all;


-----------------------------------------------------------------------------------------
-- Entity to perform memory to buffer data transfer
-----------------------------------------------------------------------------------------
--Injector Read IF deals with data fetch from memory. Data descriptor
--fields are passed from injector_ctrl. The FIFO is used as an infinite buffer to transfer
--the read transactions, once it is full, it will automatically restart from index 0.
--Thus, the data coming from the Read transactions are not saved. The Read IF will continue
--until the data size specified in d_desc.ctrl.size field is completely transferred.
--There is no Paused status.
------------------------------------------------------------------------------------------

entity injector_read_if is
  generic (
    dbits           : integer range 8 to 1024 := 32;      -- Data width of BM and FIFO at injector. [Only power of 2s allowed]
    MAX_SIZE_BURST  : integer range 8 to 4096 := 1024;    -- Maximum number of bytes per transaction
    ASYNC_RST       : boolean                 := TRUE     -- Allow asynchronous reset flag
    );
  port (
    rstn            : in  std_ulogic;                     -- Active low reset
    clk             : in  std_ulogic;                     -- Clock
    -- Signals to and from injector_ctrl
    ctrl_rst        : in  std_ulogic;                     -- Reset signal from APB interface through grdmac_ctrl
    err_sts_in      : in  std_ulogic;                     -- Core error status from APB status register 
    read_if_start   : in  std_ulogic;                     -- Start control signal
    desc_ctrl       : in  descriptor_control;             -- Control word of the descriptor to be executed
    desc_actaddr    : in  descriptor_actionaddr;          -- Action address word of the descriptor to be executed
    status_out      : out d_ex_sts_out_type;              -- M2b status out signals 
    -- Generic bus master interface
    read_if_bmi     : in  bm_miso;                        -- BM interface signals to READ_IF,through crontrol module
    read_if_bmo     : out bm_mosi                         -- Signals from READ_IF to BM IF through control module
    );
end entity injector_read_if;

------------------------------------------------------------------------------
-- Architecture of injector_read_if
------------------------------------------------------------------------------

architecture rtl of injector_read_if is
  attribute sync_set_reset         : string;
  attribute sync_set_reset of rstn : signal is "true";

  -----------------------------------------------------------------------------
  -- Constant declaration
  -----------------------------------------------------------------------------

  -- Constants for read_if present state
  constant READ_IF_IDLE       : std_logic_vector(4 downto 0) := "00101"; -- 0x05
  constant READ_IF_EXEC       : std_logic_vector(4 downto 0) := "00110"; -- 0x06
  constant READ_IF_DATA       : std_logic_vector(4 downto 0) := "00111"; -- 0x07

  -----------------------------------------------------------------------------
  -- Type and record 
  -----------------------------------------------------------------------------

  -- READ_IF states --
  -- idle =>
  -- Starting state. Waits for 'read_if_start_in' signal to proceed.
  -- Decides on size of the current burst to be transferred
  --
  -- request =>
  -- Initiates bus master interface burst read transfer from source address.
  --
  -- read_data =>
  -- Reads each data word from bus master interface output and writes to FIFO
  -- unitl the total data is transferred.

  type read_if_state_type is (idle, request, read_data);

  --READ_IF reg type
  type read_if_reg_type is record
    read_if_state     : read_if_state_type;                     -- READ_IF states
    sts               : d_ex_sts_out_type;                      -- M2b status signals
    rd_req            : std_logic;                              -- Request read transaction flag
    tot_size          : std_logic_vector(desc_ctrl.size'length - 1 downto 0); -- Total size of data to be read
    curr_size         : std_logic_vector(log_2(MAX_SIZE_BURST) downto 0);     -- Remaining bytes to be read in the burst
    inc               : std_logic_vector(desc_ctrl.size'length - 1 downto 0); -- For data destination address increment (20 bits)
    bmst_rd_busy      : std_ulogic;                             -- bus master read busy
    err_state         : std_logic_vector(4 downto 0);           -- Error state
  end record;

  -- Reset value for READ_IF reg type
  constant READ_IF_REG_RES : read_if_reg_type := (
    read_if_state     => idle,
    sts               => D_EX_STS_RST,
    rd_req            => '0',
    tot_size          => (others => '0'),
    curr_size         => (others => '0'),
    inc               => (others => '0'),
    bmst_rd_busy      => '0',
    err_state         => (others => '0')
  );

  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------
  signal r, rin : read_if_reg_type;
  
begin
  -----------------------------------------------------------------------------
  -- Assignments
  -----------------------------------------------------------------------------

  -----------------------------------------------------------------------------
  -- Combinational process
  -----------------------------------------------------------------------------
  
  comb : process (read_if_bmi, r, desc_ctrl, desc_actaddr, read_if_start, err_sts_in)
    variable v  : read_if_reg_type;
  begin

    -- Default values 
    v           := r;
    read_if_bmo <= BM_MOSI_RST;

    -- READ_IF state machine
    case r.read_if_state is
      when idle =>
        -- Default values
        v.sts.operation   := '0';
        v.sts.comp        := '0';
        v.curr_size       := (others => '0');
        v.sts.read_if_err := '0';
        v.bmst_rd_busy    := '0';

        -- Operation starts when start signal from control block arrives and no errors are present
        if(read_if_start = '1' and err_sts_in = '0') then
          v.err_state      := (others => '0');
          v.sts.operation  := '1';
          v.tot_size       := desc_ctrl.size;
          v.inc            := (others => '0');
          v.curr_size := find_burst_size(fixed_addr => desc_ctrl.type_spec,
                                         max_bsize  => MAX_SIZE_BURST,
                                         bm_bytes   => dbits/8,
                                         total_size => desc_ctrl.size
                                         );
          v.read_if_state := request;
        end if; -- No Restart (or resume) bit for the moment
      ----------     
        
      when request =>
        -- Set action address, size and request read transaction
        if(desc_ctrl.type_spec = '1') then
          read_if_bmo.rd_addr <= desc_actaddr.addr;
        else
          read_if_bmo.rd_addr <= add_vector(desc_actaddr.addr, r.inc, read_if_bmo.rd_addr'length);
        end if;
        read_if_bmo.rd_size   <= sub_vector(r.curr_size, 1, read_if_bmo.rd_size'length); -- interface understands value 0 as 1 byte
        v.rd_req              := '1';

        if((read_if_bmi.rd_req_grant and r.rd_req) = '1') then
          v.bmst_rd_busy      := '1';
          v.rd_req            := '0';
          v.read_if_state     := read_data;
        end if;
      ----------
        
      when read_data =>
        -- Check if interface is delivering valid data
        if(read_if_bmi.rd_valid = '1') then
          if(read_if_bmi.rd_err = '1') then -- In case of an error during read transaction,
            v.sts.read_if_err := '1';       -- notify it to sts, but continue with transaction.
            v.err_state       := READ_IF_DATA;
          end if;

          if(to_integer(unsigned(r.curr_size)) > dbits/8) then
            v.curr_size       := sub_vector(r.curr_size, dbits/8, v.curr_size'length);
            v.inc             := add_vector(r.inc, dbits/8, v.inc'length);
            v.tot_size        := sub_vector(r.tot_size, dbits/8, v.tot_size'length);
          else
            v.inc             := add_vector(r.inc, r.curr_size, v.inc'length);
            v.tot_size        := sub_vector(r.tot_size, r.curr_size, v.tot_size'length);
            -- At the last read beat of the burst, the rd_done flag must be set to high.
            if(read_if_bmi.rd_done = '1') then
              -- In case there's still more data to be read, request another burst.
              if(v.tot_size /= (v.tot_size'range => '0')) then
                v.curr_size := find_burst_size(fixed_addr => desc_ctrl.type_spec,
                                               max_bsize  => MAX_SIZE_BURST,
                                               bm_bytes   => dbits/8,
                                               total_size => v.tot_size
                                               );
                v.read_if_state := request;
                v.bmst_rd_busy  := '0';
              else -- If the descriptor repetition is finished, return to idle
                v.read_if_state := idle;
                v.sts.operation := '0';
                v.bmst_rd_busy  := '0';
                v.sts.comp      := '1';
              end if;
            else -- If the burst is not done, flag an error and return to idle.
              v.sts.read_if_err := '1';
              v.err_state       := READ_IF_DATA;
            end if;
          end if;
        end if;
      ----------

      when others =>
        v.read_if_state := idle;
      ----------         
    end case;  --READ_IF state machine


    ----------------------
    -- Signal update --
    ----------------------
    
    -- State decoding for status display
    if r.sts.read_if_err = '1' then
      status_out.state <= r.err_state;
    else
      case r.read_if_state is
        when request =>
          status_out.state <= READ_IF_EXEC;
        when read_data =>
          status_out.state <= READ_IF_DATA;
        when others =>
          status_out.state <= READ_IF_IDLE;
      end case;
    end if;

    rin                     <= v;
    status_out.read_if_err  <= r.sts.read_if_err;
    status_out.write_if_err <= r.sts.write_if_err;
    status_out.operation    <= r.sts.operation;
    status_out.comp         <= r.sts.comp;
    read_if_bmo.rd_req      <= r.rd_req;
     
  end process comb;

  -----------------------------------------------------------------------------
  -- Sequential Process
  -----------------------------------------------------------------------------

  seq : process (clk, rstn)
  begin
    if (rstn = '0' and ASYNC_RST) then
      r <= READ_IF_REG_RES;
    elsif rising_edge(clk) then
      if rstn = '0' or ctrl_rst = '1' then
        r <= READ_IF_REG_RES;
      else
        r <= rin;
      end if;
    end if;
  end process seq;
-----------------------------------------------------------------------------  
-- Component instantiation
-----------------------------------------------------------------------------
  
end architecture rtl;
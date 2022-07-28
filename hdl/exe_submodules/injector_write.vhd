-----------------------------------------------------------------------------   
-- Entity:      Injector Write Interface
-- File:        injector_write_if.vhd
-- Author:      Oriol Sala
-- Description: Write engine to generate random data and write it to memory through a generic bus master interface.
------------------------------------------------------------------------------ 
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library safety;
use safety.injector_pkg.all;


-----------------------------------------------------------------------------
-- Entity to perform write data transactions to memory
-----------------------------------------------------------------------------------------
-- WRITE_IF module deals with data write to memory. Data descriptor
-- fields are passed from injector_ctrl module. write_if_start signals
-- is asserted only for enabled descriptors. Once the data from FIFO is
-- transferred to memory, execution stops WRITE_IF operation and goes back to main control
-- state machine in injector_ctrl. This continues until data
-- of size specified in d_des.ctrl.size field, is completely transferred.
------------------------------------------------------------------------------------------

entity injector_write_if is
  generic (
    dbits           : integer range 8 to 1024 := 32;      -- Data width of BM and FIFO at injector. [Only power of 2s allowed]
    MAX_SIZE_BURST  : integer range 8 to 4096 := 1024;    -- Maximum number of bytes per transaction
    ASYNC_RST       : boolean                 := TRUE     -- Allow asynchronous reset flag
    );
  port (
    rstn            : in  std_ulogic;                     -- Active low reset
    clk             : in  std_ulogic;                     -- Clock
    -- Control input from injector_ctrl
    ctrl_rst        : in  std_ulogic;                     -- Reset signal from APB interface through injector_ctrl
    err_sts_in      : in  std_ulogic;                     -- Core error status from APB status register 
    write_if_start  : in  std_ulogic;                     -- Start control signal
    desc_ctrl       : in  descriptor_control;             -- Control word of the descriptor to be executed
    desc_actaddr    : in  descriptor_actionaddr;          -- Action address word of the descriptor to be executed
    status_out      : out d_ex_sts_out_type;              -- Write_if status out signals 
    -- Generic bus master interface
    write_if_bmi    : in  bm_miso;                        -- BM interface signals to write_if,through control module 
    write_if_bmo    : out bm_mosi                         -- Signals from Write_IF to BM_IF through control module
  );
end entity injector_write_if;

------------------------------------------------------------------------------
-- Architecture of injector_write_if
------------------------------------------------------------------------------

architecture rtl of injector_write_if is
  attribute sync_set_reset         : string;
  attribute sync_set_reset of rstn : signal is "true";

  -----------------------------------------------------------------------------
  -- Constant declaration
  -----------------------------------------------------------------------------

  -- Constants for injector_write_if present state
  constant WRITE_IF_IDLE      : std_logic_vector(4 downto 0) := "01000"; -- 0x08
  constant WRITE_IF_REQUEST   : std_logic_vector(4 downto 0) := "01001"; -- 0x09
  constant WRITE_IF_BURST     : std_logic_vector(4 downto 0) := "01010"; -- 0x0A
  constant WRITE_IF_CHECK     : std_logic_vector(4 downto 0) := "01011"; -- 0x0B

  -----------------------------------------------------------------------------
  -- Type and record 
  -----------------------------------------------------------------------------

  -- WRITE_IF states --
  -- idle             : Starting state. Waits for 'write_if_start' signal to proceed
  -- write_request    : Request write transaction and maybe first beat
  -- write_burst      : Continue data write in the burst until done
  -- write_data_check : Check if data burst write was successful.


  type write_if_state_type is (idle, request, write_burst, write_data_check);

  --WRITE_IF reg type
  type write_if_reg_type is record
    write_if_state    : write_if_state_type;                    -- WRITE_IF states
    sts               : d_ex_sts_out_type;                      -- WRITE_IF status signals
    wr_req            : std_logic;                              -- Request write transaction flag
    tot_size          : std_logic_vector(desc_ctrl.size'length - 1 downto 0); -- Total size of data to write 
    curr_size         : std_logic_vector(log_2(MAX_SIZE_BURST) downto 0);     -- Remaining size in the burst, to be written
    inc               : std_logic_vector(desc_ctrl.size'length - 1 downto 0); -- For data destination address increment (20 bits)
    bmst_wr_busy      : std_ulogic;                             -- Ongoing write transaction flag (goes low between repetitions)
    err_state         : std_logic_vector(4 downto 0);           -- Error state
  end record;

  -- Reset value for WRITE_IF reg type
  constant WRITE_IF_REG_RES : write_if_reg_type := (
    write_if_state    => idle,
    sts               => D_EX_STS_RST,
    wr_req            => '0',
    tot_size          => (others => '0'),
    curr_size         => (others => '0'),
    inc               => (others => '0'),
    bmst_wr_busy      => '0',
    err_state         => (others => '0')
  );

  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------
  signal r, rin : write_if_reg_type;
  -----------------------------------------------------------------------------
  
begin
  -----------------------------------------------------------------------------
  -- Assignments
  -----------------------------------------------------------------------------

  -----------------------------------------------------------------------------
  -- Combinational process
  -----------------------------------------------------------------------------
  
  comb : process (write_if_bmi, r, desc_ctrl, desc_actaddr, write_if_start, err_sts_in)
    variable v    : write_if_reg_type;
    
  begin
    -- Default values
    v             := r;
    write_if_bmo  <= BM_MOSI_RST;

    -- WRITE_IF state machine
    case r.write_if_state is
      when idle =>
        -- Default values
        v.sts.operation     := '0';
        v.sts.comp          := '0';
        v.curr_size         := (others => '0');
        v.sts.write_if_err  := '0';
        v.bmst_wr_busy      := '0';

        -- Operation starts when start signal from control block arrives and no errors are present
        if write_if_start  = '1' and err_sts_in = '0' then
          v.err_state       := (others => '0');          
          v.sts.operation   := '1';
          v.tot_size        := desc_ctrl.size;
          v.inc             := (others => '0');
          v.curr_size := find_burst_size( fixed_addr  => desc_ctrl.type_spec,
                                          max_bsize   => MAX_SIZE_BURST,
                                          bm_bytes    => dbits/8,
                                          total_size  => desc_ctrl.size
                                          );
          v.write_if_state := request;
        end if;
      ----------
     
      when request =>  -- Write request and first beat
          -- Set first burst address, size and request write transaction
          if desc_ctrl.type_spec = '1' then
            write_if_bmo.wr_addr <= desc_actaddr.addr;
          else
            write_if_bmo.wr_addr <= add_vector(desc_actaddr.addr, r.inc, write_if_bmo.wr_addr'length);
          end if;
          write_if_bmo.wr_size <= sub_vector(r.curr_size, 1, write_if_bmo.wr_size'length); -- interface understands value 0 as 1 byte
          v.wr_req := '1';

          if( (write_if_bmi.wr_req_grant and r.wr_req) = '1' ) then
            v.bmst_wr_busy    := '1';
            v.wr_req          := '0';
            v.write_if_state  := write_burst;
          end if;
      ----------
        
      when write_burst =>
        if write_if_bmi.wr_full = '0' then
        -- r.curr_size is the remaining data size to be processed after writing second
        -- data or any of the data writes that comes after second data.
        -- Control reaches in write_burst state only if desc_ctrl.size >=
        -- two words with dbits/8 size each.
          if to_integer(unsigned(r.curr_size)) > dbits/8 then
            v.curr_size       := sub_vector(r.curr_size, dbits/8, v.curr_size'length);
            v.inc             := add_vector(r.inc, dbits/8, v.inc'length);
            v.tot_size        := sub_vector(r.tot_size, dbits/8, v.tot_size'length);
          else
            v.curr_size       := (others => '0');  -- No more data pending, after writing 2nd data
            v.inc             := add_vector(r.inc, r.curr_size, v.inc'length);
            v.tot_size        := sub_vector(r.tot_size, r.curr_size, v.tot_size'length);
            v.write_if_state  := write_data_check;
          end if;
        end if;
      ----------      
        
      when write_data_check =>
        -- Evaluate if burst has finished
        if write_if_bmi.wr_done = '1' then
          if write_if_bmi.wr_err = '0' then
            -- Request additional burst if there's still data to be transfered
            if r.tot_size /= (r.tot_size'range => '0') then
              v.curr_size := find_burst_size(fixed_addr => desc_ctrl.type_spec,
                                             max_bsize  => MAX_SIZE_BURST,
                                             bm_bytes   => dbits/8,
                                             total_size => r.tot_size
                                             );
              v.write_if_state  := request;
            else
              v.bmst_wr_busy    := '0';
              v.sts.comp        := '1';
              v.sts.operation   := '0';
              v.write_if_state  := idle;
            end if;
          else
            v.sts.write_if_err  := '1';
            v.err_state         := WRITE_IF_CHECK;
            v.write_if_state    := idle;
          end if;
        end if;
        ----------
          
      when others =>
        v.write_if_state := idle;
        ----------         
    end case;  --WRITE_IF state machine

    
    ----------------------
    -- Signal update --
    ----------------------
   
    write_if_bmo.wr_data <= X"FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF"; --one128(127 downto (128-dbits)) & one128(127-dbits downto 0);
    
    -- State decoding display
    if r.sts.write_if_err = '1' then
      status_out.state <= r.err_state;
    else
      case r.write_if_state is
        when request =>
          status_out.state <= WRITE_IF_REQUEST;
        when write_burst =>
          status_out.state <= WRITE_IF_BURST;
        when write_data_check =>
          status_out.state <= WRITE_IF_CHECK;
        when others =>
          status_out.state <= WRITE_IF_IDLE;
      end case;
    end if;
    rin                     <= v;
    status_out.read_if_err  <= r.sts.read_if_err;
    status_out.write_if_err <= r.sts.write_if_err;
    status_out.operation    <= r.sts.operation;
    status_out.comp         <= r.sts.comp;
    write_if_bmo.wr_req     <= r.wr_req;
  end process comb;

  -----------------------------------------------------------------------------
  -- Sequential Process
  -----------------------------------------------------------------------------

  seq : process (clk, rstn)
  begin
    if (rstn = '0' and ASYNC_RST) then
      r <= WRITE_IF_REG_RES;
    elsif rising_edge(clk) then
      if rstn = '0' or ctrl_rst = '1' then
        r <= WRITE_IF_REG_RES;
      else
        r <= rin;
      end if;
    end if;
  end process seq;
-----------------------------------------------------------------------------  
-- Component instantiation
-----------------------------------------------------------------------------
  
end architecture rtl;
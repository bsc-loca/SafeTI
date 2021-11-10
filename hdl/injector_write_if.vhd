-----------------------------------------------------------------------------   
-- Entity:      Injector Write Interface
-- File:        injector_write_if.vhd
-- Author:      Oriol Sala
-- Description: Write engine to generate random data and write it to memory through a generic bus master interface.
------------------------------------------------------------------------------ 
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_misc.or_reduce; -- OR to a vector
--library grlib;
--use grlib.config_types.all;
--use grlib.config.all;
--use grlib.stdlib.all;
library bsc;
use bsc.injector_pkg.all;


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
    dbits           : integer range 32 to  128  := 32;    -- Bus master front end data
    bm_bytes        : integer range  4 to   16  := 4;     -- bus master data width in bytes
    MAX_SIZE_BEAT   : integer range 32 to 1024  := 1024;  -- Maximum size of a beat at a burst transaction.
    ASYNC_RST       : boolean                   := FALSE  -- Allow asynchronous reset flag
    );
  port (
    rstn            : in  std_ulogic;                     -- Active low reset
    clk             : in  std_ulogic;                     -- Clock
    -- Control input from injector_ctrl
    ctrl_rst        : in  std_ulogic;                     -- Reset signal from APB interface through injector_ctrl
    err_sts_in      : in  std_ulogic;                     -- Core error status from APB status register 
    write_if_start  : in  std_ulogic;                     -- Start control signal
    d_des_in        : in  data_dsc_strct_type;            -- Data descriptor needs to executed
    status_out      : out d_ex_sts_out_type;              -- Write_if status out signals 
    -- Generic bus master interface
    write_if_bmi    : in  bm_out_type;                    -- BM interface signals to write_if,through control module 
    write_if_bmo    : out bm_in_type                      -- Signals from Write_IF to BM_IF through control module
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

  -- Reset configuration
  --constant ASYNC_RST          : boolean := GRLIB_CONFIG_ARRAY(grlib_async_reset_enable) = 1;

  -- Constants for injector_write_if present state
  constant WRITE_IF_IDLE   	  : std_logic_vector(4 downto 0) := "01000"; -- 0x08
  constant WRITE_IF_FIRST_W	  : std_logic_vector(4 downto 0) := "01001"; -- 0x09
  constant WRITE_IF_BURST   	: std_logic_vector(4 downto 0) := "01010"; -- 0x0A
  constant WRITE_IF_CHECK   	: std_logic_vector(4 downto 0) := "01011"; -- 0x0B

  -- Constant for bit - byte manipulation
  constant BURST_BUS_WIDTH    : integer := log_2(MAX_SIZE_BEAT)+1;  -- Maximum BM interface data size
  constant SHIFT_BIT          : natural := 3;                       -- in single burst is 1024 bytes

  -----------------------------------------------------------------------------
  -- Type and record 
  -----------------------------------------------------------------------------

  -- WRITE_IF states --
  -- idle             : Starting state. Waits for 'write_if_start' signal to proceed
  -- exec_data_desc   : Execute data descriptor.
  -- write_init       : Initiate write and latch first data
  -- write_burst      : Continue data write in the burst until done
  -- write_data_check : Check if data burst write was successful.


  type write_if_state_type is (idle, first_word, write_burst, write_data_check);

  --WRITE_IF reg type
  type write_if_reg_type is record
    write_if_state    : write_if_state_type;                    -- WRITE_IF states
    sts               : d_ex_sts_out_type;                      -- WRITE_IF status signals
    tot_size          : std_logic_vector(18 downto 0);          -- Total size of data to write 
    curr_size         : std_logic_vector(BURST_BUS_WIDTH-1 downto 0); -- Remaining size in the burst, to be written
    inc               : std_logic_vector(21 downto 0);          -- For data destination address increment (22 bits)
    bmst_wr_busy      : std_ulogic;                             -- bus master write busy
    err_state         : std_logic_vector(4 downto 0);           -- Error state
  end record;

  -- Reset value for WRITE_IF reg type
  constant WRITE_IF_REG_RES : write_if_reg_type := (
    write_if_state    => idle,
    sts               => D_EX_STS_RST,
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
  
  comb : process (write_if_bmi, r, d_des_in, write_if_start, err_sts_in)
    variable v             : write_if_reg_type;
    variable sz_aftr_write : std_logic_vector(BURST_BUS_WIDTH-2 downto 0);  -- Index for data remaining to be transferred
    variable err           : std_logic_vector(2 downto 0);   -- error variable
    
  begin
    -- Default values
    v                := r;
    sz_aftr_write    := (others => '0');
    err              := (others => '0');
    write_if_bmo     <= BM_CTRL_REG_RST;

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
          v.sts.comp        := '0';
          v.tot_size        := d_des_in.ctrl.size;
          v.inc             := (others => '0');
          if or_reduce(d_des_in.ctrl.size) = '0' then
            v.sts.comp      := '1';
          end if;
          v.curr_size := find_burst_size(src_fixed_addr       => d_des_in.ctrl.src_fix_adr,
                                              dest_fixed_addr => d_des_in.ctrl.dest_fix_adr,
                                              max_bsize       => MAX_SIZE_BEAT,
                                              total_size      => d_des_in.ctrl.size
                                              );
          v.write_if_state := first_word;
        end if;
      ----------
     
      when first_word =>  -- First data passed with write initiation
        if or_reduce(r.curr_size) /= '0' then
	        if d_des_in.ctrl.dest_fix_adr = '1' then
            write_if_bmo.wr_addr <= d_des_in.dest_addr;
          else
            write_if_bmo.wr_addr <= add_vector(d_des_in.dest_addr, r.inc, write_if_bmo.wr_addr'length);
          end if;
          if r.bmst_wr_busy = '0' then
            write_if_bmo.wr_size <= sub_vector(r.curr_size, 1, write_if_bmo.wr_size'length); -- AHB interface understands value 0 as 1 byte
            write_if_bmo.wr_req  <= '1';
            if write_if_bmi.wr_req_grant = '1' then
              v.bmst_wr_busy  := '1';
              if to_integer(unsigned(r.curr_size)) >= bm_bytes then
                sz_aftr_write := sub_vector(r.curr_size, bm_bytes, sz_aftr_write'length);
                v.curr_size   := sub_vector(r.curr_size, bm_bytes, v.curr_size'length);  -- Size pending, after writing first data
                v.inc         := add_vector(r.inc, bm_bytes, v.inc'length);
                v.tot_size    := sub_vector(r.tot_size, bm_bytes, v.tot_size'length);
                if or_reduce(sz_aftr_write) /= '0' then
                  v.write_if_state := write_burst;
                else
                  v.write_if_state := write_data_check;
                end if;
              else
                v.curr_size   := (others => '0');  -- Size pending, after writing first data
                v.inc         := add_vector(r.inc, r.curr_size, v.inc'length);
                v.tot_size    := sub_vector(r.tot_size, r.curr_size, v.tot_size'length);
                v.write_if_state := write_data_check;
              end if;
            end if;
          end if;
  	    else
	        v.sts.comp	      := '1';
	        v.write_if_state  := idle;
	      end if;
      ----------
        
      when write_burst =>
        write_if_bmo.wr_req <= '0';
        if write_if_bmi.wr_full = '0' then
        -- r.curr_size is the remaining data size to be processed after writing second
        -- data or any of the data writes that comes after second data.
        -- Control reaches in write_burst state only if d_des_in.ctrl.size >=
        -- two words with bm_bytes size each.
          if to_integer(unsigned(r.curr_size)) >= bm_bytes then
            sz_aftr_write     := sub_vector(r.curr_size, bm_bytes, sz_aftr_write'length);
	          if or_reduce(sz_aftr_write) = '0' then -- more data to be writen after current data write
              v.write_if_state := write_data_check;
            end if;
            v.curr_size       := sub_vector(r.curr_size, bm_bytes, v.curr_size'length);
            v.inc             := add_vector(r.inc, bm_bytes, v.inc'length);
            v.tot_size        := sub_vector(r.tot_size, bm_bytes, v.tot_size'length);
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
          v.bmst_wr_busy := '0';
          if write_if_bmi.wr_err = '0' then
            if or_reduce(r.tot_size) /= '0' then --Start again if there are remaining bytes
              v.curr_size := find_burst_size(src_fixed_addr   => d_des_in.ctrl.src_fix_adr,
                                             dest_fixed_addr  => d_des_in.ctrl.dest_fix_adr,
                                             max_bsize        => MAX_SIZE_BEAT,
                                             total_size       => r.tot_size
                                             );
              v.write_if_state  := first_word;
            else
              v.bmst_wr_busy    := '0';
              v.sts.comp        := '1';
              v.sts.operation   := '0';
              v.write_if_state  := idle;
            end if;
          else
            v.sts.write_if_err := '1';
            v.err_state        := WRITE_IF_CHECK;
            v.write_if_state   := idle;
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
        when first_word =>
          status_out.state <= WRITE_IF_FIRST_W;
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

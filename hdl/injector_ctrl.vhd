-----------------------------------------------------------------------------   
-- Entity:      injector_ctrl
-- File:        injector_ctrl.vhd
-- Author:      Oriol Sala
-- Description: Main control module for the Traffic Injector
------------------------------------------------------------------------------ 
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_misc.or_reduce;
--library grlib;
--use grlib.config_types.all;
--use grlib.config.all;
--use grlib.stdlib.all;
--use grlib.amba.all;
--use grlib.devices.all;
library bsc;
use bsc.injector_pkg.all;
--library techmap;
--use techmap.gencomp.all;

-----------------------------------------------------------------------------
-- Control module with main state machine for data execution
------------------------------------------------------------------------------
-- If Injector core is enabled and no error exists, a descriptor is read from the
-- first descriptor pointer. Descriptor is then decoded and executed based on
-- the descriptor type. Data descriptor execution is split to READ_IF and WRITE_IF 
-- operation, which is implemented in injector_read_if.vhd and injector_write_if.vhd. 
-- Descriptor write back (always on) is implemented in control module. 
-- Every time a descriptor is completed, execution comes to idle state before
-- proceeding to next descriptor in the queue.
-----------------------------------------------------------------------------

entity injector_ctrl is
  generic (
    dbits             : integer range 32 to 128 := 32;      -- Bus master front end data width    
    fifo_size         : integer range  1 to  16 := 8;       -- FIFO Length (Length multiple of 8) 
    ASYNC_RST         : boolean                 := FALSE    -- Allow asynchronous reset flag
    );
  port (
    rstn              : in  std_ulogic;                     -- Active low reset signal
    clk               : in  std_ulogic;                     -- Clock
    -- ctrl signals from APB interface
    ctrl              : in  injector_ctrl_reg_type;         -- Control signals from APB interface
    des_ptr           : in  injector_desc_ptr_type;         -- First descriptor pointer
    active            : in  std_ulogic;                     -- Core enabled after reset?
    err_status        : in  std_ulogic;                     -- Core error status from APB status register    
    curr_desc_out     : out curr_des_out_type;              -- Current descriptor field out for debug display
    curr_desc_ptr     : out std_logic_vector(31 downto 0);  -- Current descriptor pointer for debug display
    status            : out status_out_type;                -- Status signals
    irq_flag_sts      : out std_ulogic;                     -- IRQ status flag
    --Bus Master signals
    bm_in             : in  bm_out_type;                    -- BM signals from Bus master to control module
    bm_out            : out bm_in_type;                     -- BM signals to BusMaster interface from control module
    -- READ_IF BM signals
    read_if_bm_in     : in  bm_ctrl_reg_type;               -- BM signals from READ_IF through control module  
    read_if_bm_out    : out bm_out_type;                    -- BM signals to READ_IF through control module  
    -- WRITE_IF BM signals
    write_if_bm_in    : in  bm_ctrl_reg_type;               -- BM signals from WRITE_IF through control module
    write_if_bm_out   : out bm_out_type;                    -- BM signals to WRITE_IF through control module
    -- data descriptor out for READ_IF, WRITE_IF and DELAY
    d_desc_out        : out data_dsc_strct_type;            -- Data descriptor passed to READ_IF and WRITE_IF
    ctrl_rst          : out std_ulogic;                     -- Reset signal from APB interface, to READ_IF and WRITE_IF and DELAY
    err_sts_out       : out std_ulogic;                     -- Core APB status reg error bit. Passed to READ_IF and WRITE_IF and DELAY
    -- READ_IF control signals
    read_if_start     : out std_ulogic;                     -- READ_IF start signal
    read_if_sts_in    : in  d_ex_sts_out_type;              -- READ_IF status signals
    -- WRITE_IF control signals
    write_if_sts_in   : in  d_ex_sts_out_type;              -- WRITE_IF status signals
    write_if_start    : out std_logic;                      -- WRITE_IF start signal
    -- DELAY_IF
    delay_if_sts_in   : in  d_ex_sts_out_type;
    delay_if_start    : out std_logic
  );
end entity injector_ctrl;

------------------------------------------------------------------------------
-- Architecture of injector_ctrl
------------------------------------------------------------------------------

architecture rtl of injector_ctrl is
  attribute sync_set_reset         : string;
  attribute sync_set_reset of rstn : signal is "true";
  -----------------------------------------------------------------------------
  -- Constant declaration
  -----------------------------------------------------------------------------
  -- Constants for FSM present state display
  constant IDLE_STATE      	  : std_logic_vector(4 downto 0) := "00000"; -- 0x00
  constant FETCH_DES       	  : std_logic_vector(4 downto 0) := "00001"; -- 0x01
  constant READ_DES        	  : std_logic_vector(4 downto 0) := "00010"; -- 0x02
  constant READ_FIFO_Q		    : std_logic_vector(4 downto 0) := "00011"; -- 0x03
  constant DECODE          	  : std_logic_vector(4 downto 0) := "00100"; -- 0x04
  
  constant READ_IF_IDLE      	: std_logic_vector(4 downto 0) := "00101"; -- 0x05
  constant READ_IF_EXEC    	  : std_logic_vector(4 downto 0) := "00110"; -- 0x06
  constant READ_IF_DATA_READ	: std_logic_vector(4 downto 0) := "00111"; -- 0x07
  
  constant WRITE_IF_IDLE   	  : std_logic_vector(4 downto 0) := "01000"; -- 0x08
  constant WRITE_IF_FIRST_W	  : std_logic_vector(4 downto 0) := "01001"; -- 0x09
  constant WRITE_IF_BURST   	: std_logic_vector(4 downto 0) := "01010"; -- 0x0A
  constant WRITE_IF_CHECK   	: std_logic_vector(4 downto 0) := "01011"; -- 0x0B  
  
  constant DELAY_IF_IDLE    	: std_logic_vector(4 downto 0) := "01100"; -- 0x0C
  constant DELAY_IF_EXEC    	: std_logic_vector(4 downto 0) := "01101"; -- 0x0D 

  -- Other constants
  constant READ       : std_logic_vector(2 downto 0) := "000";
  constant WRITE      : std_logic_vector(2 downto 0) := "001";
  constant DELAY      : std_logic_vector(2 downto 0) := "010";         
  constant TBD_2      : std_logic_vector(2 downto 0) := "011";
  -- and more if needed
  
  constant DESC_BYTES : std_logic_vector(9 downto 0) := "0000010100";  -- 20 bytes to be fetched(5 words)
  constant WB_SZ      : std_logic_vector(9 downto 0) := "0000000011";  -- 3+1 bytes to be written(1 word)

  -- Constant for bit-byte manipulation
  constant SHIFT_BIT  : natural := 3;
  constant sz_bits    : integer := to_integer(shift_left(unsigned(DESC_BYTES), SHIFT_BIT));

  -- Reset configuration
  --constant ASYNC_RST : boolean := GRLIB_CONFIG_ARRAY(grlib_async_reset_enable) = 1;

  ----------------------------------------------------------------------------- 
  -----------------------------------------------------------------------------
  --- Control FSM states ---

  -- IDLE =>
  -- Execution starts from idle state and comes back after completing a FIFO write of the current descriptor 
  -- found in memory. Core processes a descriptor queue only if there are no errors and the core is enabled. 
  -- If the execution is ongoing and not last descriptor nor FIFO is full, core proceeds with the next 
  -- descriptor fetch. Else if the whole queue is completely read from memory, the transaction execution 
  -- starts from the top of the FIFO. If the core is disabled or if there is an error, injector goes 
  -- in to a paused state.
  --
  -- FETCH DESC =>
  -- Initiates 20 byte long burst read through default Bus Master Interface(BM IF) to read a descriptor.
  --
  -- READ_DESC =>
  -- Reads descriptor data from BM IF output signals and stores it in FIFO queue.
  -- 
  -- READ_FIFO =>
  -- Controls Read-enable signals from FIFO and loads the designated descriptor to internal register for decoding.
  -- 
  -- DECODE_DESC =>
  -- Checks if the descriptor is enabled. Disabled descriptors are skipped and
  -- core jumps to read_fifo to proceed with next descriptor in the queue. If the
  -- descriptor is enabled, based on the desc_type field value, it decodes the
  -- type of the descriptor and jumps to respective states: 
  -- When the descriptor type is a Read Descriptor: it sends read_if_start signal to injector_read_if entity.
  -- When the descriptor type is a Write Descriptor: it sends write_if_start signal to injector_write_if entity.
  -- When the descriptor type is a Delay Descriptor: it sends delay_if_start signal to injector_delay_if entity.
  -- 
  -- READ_IF =>
  -- Always monitors for any errors and status from injector_read_if module.
  -- If an error is reported from injector_read_if module, handles the error.
  -- When a read_if_comp status is received, core assumes the completion of the descriptor execution.
  -- 
  -- WRITE_IF =>
  -- Always monitors for any errors and status from injector_write_if module.
  -- If an error is reported from injector_write_if module, handles the error.
  -- When a write_if_comp status is received, core assumes the completion of the descriptor execution.
  -- 
  -- DELAY_IF =>
  -- Always monitors for any errors and status from injector_delay_if module.
  -- If an error is reported from injector_delay_if module, handles the error.
  -- When a delay_if_comp status is received, core assumes the completion of the descriptor execution.  
  
  type ctrl_state_type is (idle, fetch_desc, read_desc, read_fifo, decode_desc, read_if, write_if, delay_if);

  -- injector_ctrl local reg type
  type ctrl_reg_type is record
    state               : ctrl_state_type;                 -- Present state register
    err_state           : std_logic_vector(4 downto 0);    -- FSM state in which error occured
    desc_ptr            : std_logic_vector(31 downto 0);   -- Current descriptor pointer
    i                   : integer range 0 to 7;            -- Register for index increment
    rep_count           : std_logic_vector(6 downto 0);	   -- Register for Repetition Count increment
    rd_desc             : std_logic_vector(159 downto 0);  -- Register for descriptor read from BM (5 Registers * 32 bits)
    read_if_start       : std_ulogic;                      -- READ_IF start signal
    write_if_start      : std_ulogic;                      -- WRITE_IF start signal
    delay_if_start      : std_ulogic;			                 -- DELAY_IF start signal
    desc_skip           : std_ulogic;                      -- descriptor skip flag
    err_flag            : std_ulogic;                      -- Error flag
    dcomp_flg           : std_ulogic;                      -- Descriptor completed flag
    init_error          : std_ulogic;                      -- Error occured before starting current descriptor execution
    bmst_wr_busy        : std_ulogic;                      -- bus master write busy
    bmst_rd_busy        : std_ulogic;                      -- bus master read busy
    bmst_rd_err         : std_ulogic;                      -- bus master read error
    err_status          : std_ulogic;                      -- register to find the falling edge of err_status input signal
    sts                 : status_out_type;                 -- Status register   
    fifo_wen            : std_ulogic;			   -- FIFO write enable signal
    fifo_ren		        : std_ulogic;			   -- FIFO read enable signal
    fifo_finished       : std_ulogic;			   -- FIFO is fully read flag
    fifo_rd_rst         : std_ulogic;			   -- FIFO read address reset signal 
  end record;
  -- Reset value for injector_ctrl local reg type
  constant CTRL_REG_RST : ctrl_reg_type := (
    state        => idle,
    err_state    => (others => '0'),
    desc_ptr     => (others => '0'),
    i            => 0,
    rep_count    => (others => '0'),
    rd_desc      => (others => '0'),
    read_if_start    => '0',
    write_if_start   => '0',
    delay_if_start   => '0',
    desc_skip    => '0',
    err_flag     => '0',
    dcomp_flg    => '0',
    init_error   => '0',
    bmst_wr_busy => '0',
    bmst_rd_busy => '0',
    bmst_rd_err  => '0',
    err_status   => '0',
    sts          => STATUS_OUT_RST,
    fifo_wen     => '0',
    fifo_ren     => '0',
    fifo_finished => '0',
    fifo_rd_rst  => '0'
    );

  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------

  signal r, rin : ctrl_reg_type;
  signal d_des  : data_dsc_strct_type;  -- Data descriptor
  signal bmst   : bm_ctrl_reg_type;     -- Bus master control signals
  
  signal fifo_wen_o     : std_logic;			                -- Write enable (to FIFO)
  signal fifo_ren_o     : std_logic;			                -- Read enable  (to FIFO)
  signal fifo_wdata     : std_logic_vector(159 downto 0);	-- FIFO Write data bus
  signal fifo_rdata     : std_logic_vector(159 downto 0);	-- FIFO Read data bus
  signal fifo_full      : std_logic;			                -- FIFO is full (from FIFO)
  signal fifo_completed : std_logic;			                -- FIFO completely read (from FIFO)
  signal fifo_read_rst  : std_logic;			                -- FIFO Read Address Reset output

  -----------------------------------------------------------------------------
  -- Function/procedure declaration
  -----------------------------------------------------------------------------
  
begin  -- rtl

  -----------------------------------------------------------------------------
  -- Assignments
  -----------------------------------------------------------------------------
  -- Bus master signal assignment switch logic. Based on the current state bus
  -- master signals are driven by READ_IF or WRITE_IF or control unit.

  bm_out.rd_addr <= read_if_bm_in.rd_addr when r.state = read_if else
                    write_if_bm_in.rd_addr when r.state = write_if else
                    bmst.rd_addr;
  bm_out.rd_size <= read_if_bm_in.rd_size when r.state = read_if else
                    write_if_bm_in.rd_size when r.state = write_if else
                    bmst.rd_size;
  bm_out.rd_req <= read_if_bm_in.rd_req when r.state = read_if else
                    write_if_bm_in.rd_req when r.state = write_if else
                    bmst.rd_req;
  bm_out.wr_addr <= read_if_bm_in.wr_addr when r.state = read_if else
                    write_if_bm_in.wr_addr when r.state = write_if else
                    bmst.wr_addr;
  bm_out.wr_size <= read_if_bm_in.wr_size when r.state = read_if else
                    write_if_bm_in.wr_size when r.state = write_if else
                    bmst.wr_size;
  bm_out.wr_req <= read_if_bm_in.wr_req when r.state = read_if else
                    write_if_bm_in.wr_req when r.state = write_if else
                    bmst.wr_req;
  bm_out.wr_data <= read_if_bm_in.wr_data when r.state = read_if else
                    write_if_bm_in.wr_data when r.state = write_if else
                    bmst.wr_data;

  -- Deassert the start signal when the READ_IF, WRITE_IF, DELAY_IF operation has started.
  read_if_start   <= '0' when read_if_sts_in.operation  = '1' else r.read_if_start;
  write_if_start  <= '0' when write_if_sts_in.operation = '1' else r.write_if_start;
  delay_if_start  <= '0' when delay_if_sts_in.operation = '1' else r.delay_if_start;

  -----------------------------------------------------------------------------
  -- Combinational logic
  ----------------------------------------------------------------------------- 
  comb : process (r, ctrl, des_ptr, active, read_if_sts_in, write_if_sts_in, delay_if_sts_in, read_if_bm_in, write_if_bm_in, err_status, bm_in, d_des, bmst) --deleted c_des

    variable v           : ctrl_reg_type; 
    variable remainder   : integer range 0 to 96;          -- Variable for BM read_data handling
    variable bmst_rd_req : std_ulogic;                     -- Bus master read request variable
    variable bmst_wr_req : std_ulogic;                     -- Bus master write request variable
  begin
    --Variable initialization
    v           := r;
    remainder   := 0;
    bmst_rd_req := '0';
    bmst_wr_req := '0';
    bmst        <= BM_CTRL_REG_RST;

    -- TODO: Kick flag. Whenever the ctrl.kick is set, this flag is set. This indicates addition of new
    -- descriptors or resuming Injector operation after a pause
    if ctrl.kick = '1' then
      v.sts.kick := '1';
    end if;

    -- Deassert the start signal when the READ_IF, WRITE_IF or DELAY_IF operation has started.
    if read_if_sts_in.operation = '1' then
      v.read_if_start   := '0';
    elsif write_if_sts_in.operation = '1' then
      v.write_if_start  := '0';
    elsif delay_if_sts_in.operation = '1' then
      v.delay_if_start  := '0';
    end if;

    v.err_status := err_status;
    -- Falling edge of err_status signal
    if (r.err_status = '1' and err_status = '0') then
      v.err_flag := '0';
    end if;

    -- FIFO Wen single clock Signal
    if r.fifo_wen = '1' then
      v.fifo_wen := '0';
    end if;
    
    if r.fifo_rd_rst = '1' then
      v.fifo_rd_rst := '0';
    end if;

    -- Controller state machine 
    case r.state is
      when idle =>
        -- Default values
        v.write_if_start      := '0';
        v.read_if_start       := '0';
	      v.delay_if_start      := '0';
        v.sts.err             := '0';
        v.sts.decode_desc_err := '0';
        v.sts.rd_desc_err     := '0';
        v.sts.rd_data_err     := '0';
        v.sts.wr_data_err     := '0';
        v.sts.rd_nxt_ptr_err  := '0';
        v.bmst_rd_busy        := '0';
        v.bmst_wr_busy        := '0';
        v.bmst_rd_err         := '0';
        v.sts.desc_comp       := '0';

        if (ctrl.en = '1' and err_status = '0' and r.err_flag = '0') then  -- Enabled and no error
          -- clear any latched events
          v.err_flag   := '0';
          v.err_state  := (others => '0');
          if active = '0' then
            -- Initial starting after reset.
            -- Start descriptor read from first descriptor pointer
            v.desc_skip     := '0';
            v.sts.ongoing   := '1';
            v.sts.comp      := '0';
            v.dcomp_flg     := '0';
	          v.fifo_finished := '0';
            v.desc_ptr      := des_ptr.ptr;
            v.rd_desc       := (others => '0');
            v.state         := fetch_desc;
          elsif r.sts.ongoing = '1' or r.sts.kick = '1' or v.sts.kick = '1' then
            if r.init_error = '1' then  -- Taking up the same descriptor to fetch again since previously
                                        -- it encountered an error before reaching decoding state
              v.sts.ongoing := '1';
              v.rd_desc     := (others => '0');
              v.state       := fetch_desc;
            else  -- No initial desc read error
	            if r.fifo_finished = '1' then  -- When fifo is fully Read, update Completed Flag
		          v.sts.ongoing   := '0';
		          v.dcomp_flg     := '0';
		          v.sts.desc_comp := '0';
		            if(ctrl.qmode = '0') then
		    -- Normal Queue execution finished, update flags and end execution
		            v.sts.comp      := '1';
		            else
		    -- Queue Mode, re-enter FIFO and restart execution
		            v.sts.ongoing   := '1';
		            v.fifo_finished := '0';
		            v.state	   := read_fifo;
		            end if;
	            elsif d_des.nxt_des.last = '1' or fifo_full = '1' then -- If descriptor queue is finished or FIFO is full
		            v.state       := read_fifo;
              else  -- not last descriptor. Continue with next descriptor in the queue
                v.desc_ptr      := d_des.nxt_des.ptr;
                v.dcomp_flg     := '0';
                v.sts.ongoing   := '1';
                v.sts.comp      := '0';
                v.rd_desc       := (others => '0');
                v.state         := fetch_desc;
                v.desc_skip     := '0';
              end if;
            end if;
          end if;
        else  -- Error
          v.sts.ongoing := '0';
        end if;
        -----------

      when fetch_desc =>
        -- Read all fields of descriptor. 20 bytes(5 words)
        v.sts.kick   := '0';
        v.init_error := '0';
        -- Initiate descriptor fetch
        if r.bmst_rd_busy = '0' then
          bmst_rd_req   := '1';
          v.bmst_rd_err := '0';
        end if;
        bmst.rd_addr <= r.desc_ptr;
        bmst.rd_size <= sub_vector(DESC_BYTES, 1, bmst.rd_size'length);
        if bmst_rd_req = '1' and bm_in.rd_req_grant = '1' then
          v.state        := read_desc;
          v.bmst_rd_busy := '1';
        end if;
        -----------
        
      when read_desc =>
        remainder := (sz_bits mod dbits);
        if bm_in.rd_valid = '1' then
          -- Check read errors (for each access in the burst)
          if bm_in.rd_err = '1' then
            v.bmst_rd_err := '1';
            -- Can not write back here as we dont know type of descriptor before decoding.               
          elsif r.bmst_rd_err = '0' then
            -- Read descriptor and store in 160 bit register 'rd_desc'. Logic to take care of configurable data width 'dbits'
            if (r.i < (sz_bits/dbits)) then
              v.rd_desc                   := std_logic_vector(shift_left(unsigned(r.rd_desc), dbits));
              v.rd_desc(dbits-1 downto 0) := bm_in.rd_data(127 downto (128-dbits));
            elsif remainder /= 0 then
              case remainder is
                when 32 =>              -- remainder = 32
                  v.rd_desc              := std_logic_vector(shift_left(unsigned(r.rd_desc), 32));
                  v.rd_desc(31 downto 0) := bm_in.rd_data(127 downto 96);
                when others =>          -- remainder = 96
                  v.rd_desc              := std_logic_vector(shift_left(unsigned(r.rd_desc), 96));
                  v.rd_desc(95 downto 0) := bm_in.rd_data(127 downto 32);
              end case;
            end if;  -- all fields of descriptor are read.
            v.i := r.i + 1;
          end if;
          -- Evaluate if the burst access has finished
          if bm_in.rd_done = '1' then
            if v.bmst_rd_err = '0' then
              -- No errors during the complete burst
              v.fifo_wen := '1';
              v.state    := idle;
            else -- Bus master error
              v.sts.err         := '1';
              v.sts.rd_desc_err := '1';
              v.err_flag        := '1';
              v.err_state       := READ_DES;
              v.init_error      := '1';
              v.state           := idle;
            end if;
            -- Clear indexing register and bus master flags
            v.i            := 0;
            v.bmst_rd_busy := '0';
          end if;
        end if;
        -----------

      when read_fifo =>
	      if r.fifo_finished = '0' then
	        if r.fifo_ren = '0' then -- Single clock Read enable signal
	          v.fifo_ren := '1';
	        else
	          v.fifo_ren       := '0'; -- Ready to read from FIFO
	          v.dcomp_flg      := '0';
	          v.sts.desc_comp  := '0';
	          v.rd_desc        := fifo_rdata;
	          v.state          := decode_desc;

	          -- Check if FIFO is fully read
	          if or_reduce(fifo_rdata) = '0' or fifo_completed = '1' then 
		          v.fifo_rd_rst       := '1';
		          v.fifo_finished     := '1';
	          end if;
	        end if;
	      else
	        v.state := idle;
	      end if;
        -----------

      when decode_desc => 
        -- Finding descriptor type based on desc_type field. 0-read, 1-write, 2-Delay, 3-TBD
        case r.rd_desc(131 downto 129) is
          when READ =>
            if r.rd_desc(128) = '1' then  -- enabled descriptor
              v.read_if_start := '1';
              v.state         := read_if;
            else  -- Disabled descriptor. go to next fifo position.
              v.desc_skip     := '1';
              v.state         := read_fifo;
            end if;
            
          when WRITE =>
            if r.rd_desc(128) = '1' then  -- enabled descriptor
              v.write_if_start  := '1';
              v.state           := write_if;
            else  -- Disabled descriptor. go to next fifo positon.
              v.desc_skip       := '1';
              v.state           := read_fifo;
            end if;
          
          when DELAY =>
            if r.rd_desc(128) = '1' then  -- enabled descriptor
              v.delay_if_start  := '1';
              v.state           := delay_if;
            else  -- Disabled descriptor. go to next fifo position.
              v.state           := read_fifo;
            end if;

          when others =>
            -- desc_type field should have a value in the range 0 to 3 
            v.sts.decode_desc_err := '1';
            v.sts.err             := '1';
            v.err_flag            := '1';
            v.err_state           := DECODE;
            v.state 		  := idle;
        end case;  --Decoding completed
        -----------

      when read_if =>
        -- Check whether the transaction was successfull or not
        if (r.read_if_start = '0' and read_if_sts_in.comp = '1') then 
           if ( r.rep_count < r.rd_desc(140 downto 135) and or_reduce(r.rd_desc(140 downto 135)) = '1' ) then -- Check COUNT parameter in Ctrl Desc Register
              v.rep_count     := add_vector(r.rep_count, 1, r.rep_count'length);
              v.state         := decode_desc;
           else          
	            v.sts.desc_comp := '1';
              v.dcomp_flg     := '1';
              v.rep_count     := (others => '0');
              v.state         := read_fifo;
           end if;
        elsif read_if_sts_in.read_if_err = '1' then  -- READ_IF error
          v.sts.err           := '1';
          v.err_flag          := '1';
          v.err_state         := read_if_sts_in.state;
          v.sts.rd_data_err   := '1';
          v.state	            := idle;

        end if;
        -----------

      when write_if =>
        -- Check whether the transaction was successfull or not
        if (r.write_if_start = '0' and write_if_sts_in.comp = '1') then
           -- Check the descriptor repetition count parameter
	      if ( r.rep_count < r.rd_desc(140 downto 135) and or_reduce(r.rd_desc(140 downto 135)) = '1' ) then -- Check COUNT parameter in Ctrl Desc Register
              v.rep_count     := add_vector(r.rep_count, 1, r.rep_count'length);
	            v.state         := decode_desc;
           else
	            v.sts.desc_comp := '1';
              v.dcomp_flg     := '1';
	            v.rep_count     := (others => '0');
              v.state	        := read_fifo;
	   end if;
        elsif write_if_sts_in.write_if_err = '1' then
          v.sts.err           := '1';
          v.err_flag          := '1';
          v.err_state         := write_if_sts_in.state;
          v.sts.wr_data_err   := '1';
          v.state 	          := idle;
        end if;
        -----------

      when delay_if =>
        -- Check whether the transaction was successfull or not
        if (r.delay_if_start = '0' and delay_if_sts_in.comp = '1') then
          if ( r.rep_count < r.rd_desc(140 downto 135) and or_reduce(r.rd_desc(140 downto 135)) = '1' ) then -- Check COUNT parameter in Ctrl Desc Register
            v.rep_count       := add_vector(r.rep_count, 1, r.rep_count'length);
            v.state           := decode_desc;
          else
	          v.sts.desc_comp   := '1';
            v.dcomp_flg       := '1';
            v.rep_count       := (others => '0');
            v.state	          := read_fifo;
	        end if;
        elsif delay_if_sts_in.delay_if_err = '1' then
          v.sts.err           := '1';
          v.err_flag          := '1';
          v.err_state         := delay_if_sts_in.state;
          v.sts.wr_data_err   := '1';
          v.state 	          := idle;
        end if;
 
      when others =>
       v.state := idle;
    end case;

    ----------------------
    -- Signal update    --
    ----------------------

    -- Descriptor control signals
    d_des.ctrl.en           <= r.rd_desc(128);
    d_des.ctrl.desc_type    <= r.rd_desc(131 downto 129);
    d_des.ctrl.irq_en       <= r.rd_desc(132);
    d_des.ctrl.src_fix_adr  <= r.rd_desc(133);
    d_des.ctrl.dest_fix_adr <= r.rd_desc(134);
    d_des.ctrl.count_size   <= r.rd_desc(140 downto 135);
    d_des.ctrl.size         <= r.rd_desc(159 downto 141);    
    -- Next Descriptor Pointer
    d_des.nxt_des.ptr       <= (r.rd_desc(127 downto 97) & "0");
    d_des.nxt_des.last      <= r.rd_desc(96);
    -- Destination base address where data is to be written
    d_des.dest_addr         <= r.rd_desc(95 downto 64);
    -- Source base address from where data is to be fetched
    d_des.src_addr          <= r.rd_desc(63 downto 32);

    -- Demultiplex Bus Master signals and drive READ_IF or WRITE_IF 
    if r.state = read_if then           --READ_IF
       read_if_bm_out <= bm_in;
       write_if_bm_out <= BM_OUT_RST;
    elsif r.state = write_if then       --WRITE_IF
       write_if_bm_out <= bm_in;
       read_if_bm_out <= BM_OUT_RST;
    end if;

    -- state decoding for status display
    if r.err_flag = '1' then
      status.state <= r.err_state;
    else
      case r.state is
        when fetch_desc =>
          status.state <= FETCH_DES;
        when read_desc =>
          status.state <= READ_DES;
        when decode_desc =>
          status.state <= DECODE;
	when read_fifo =>
	  status.state <= READ_FIFO_Q;
        when read_if =>
          status.state <= read_if_sts_in.state;
        when write_if =>
          status.state <= write_if_sts_in.state;
        when delay_if =>
          status.state <= delay_if_sts_in.state;
        when others =>
          status.state <= IDLE_STATE;
      end case;
    end if;
    
    -- Drive IRQ flag
    if (r.sts.err = '1' or err_status = '1') then
      irq_flag_sts <= ctrl.irq_en and ctrl.irq_err;
    elsif r.dcomp_flg = '1' then
      irq_flag_sts <= d_des.ctrl.irq_en and ctrl.irq_en;
    else
      irq_flag_sts <= '0';
    end if;

    rin                     <= v;
    status.err              <= r.sts.err;
    status.decode_desc_err  <= r.sts.decode_desc_err;
    status.rd_desc_err      <= r.sts.rd_desc_err;
    status.rd_data_err      <= r.sts.rd_data_err;
    status.wr_data_err      <= r.sts.wr_data_err;
    status.ongoing          <= r.sts.ongoing;
    status.desc_comp        <= r.sts.desc_comp;
    status.kick             <= r.sts.kick;
    status.rd_nxt_ptr_err   <= r.sts.rd_nxt_ptr_err;
    status.comp             <= r.sts.comp;
    status.count            <= r.rep_count;

    -- Current descriptor fields for debug display
    curr_desc_out.dbg_ctrl     <= r.rd_desc(159 downto 128);
    curr_desc_out.dbg_nxt      <= r.rd_desc(127 downto 96);
    curr_desc_out.dbg_dst_addr <= r.rd_desc(95 downto 64);
    curr_desc_out.dbg_src_addr <= r.rd_desc(63 downto 32);
    curr_desc_out.dbg_sts      <= b"00" & X"0000_000" & r.err_flag & r.dcomp_flg;
    
    d_desc_out    <= d_des;
    ctrl_rst      <= ctrl.rst or err_status;
    curr_desc_ptr <= r.desc_ptr;
    err_sts_out   <= err_status;
    bmst.rd_req   <= bmst_rd_req;
    bmst.wr_req   <= bmst_wr_req;

    fifo_wen_o    <= r.fifo_wen;
    fifo_ren_o    <= r.fifo_ren;
    fifo_wdata    <= r.rd_desc;
    fifo_read_rst <= r.fifo_rd_rst;
    
  end process comb;

  -----------------------------------------------------------------------------
  -- Sequential process
  -----------------------------------------------------------------------------  
  seq : process (clk, rstn)
  begin
    if (rstn = '0' and ASYNC_RST) then
      r <= CTRL_REG_RST;
    elsif rising_edge(clk) then
      if rstn = '0' or ctrl.rst = '1' then
        r <= CTRL_REG_RST;
      else
        r <= rin;
      end if;
    end if;
  end process seq;

  -----------------------------------------------------------------------------
  -- Component instantiation
  -----------------------------------------------------------------------------  

  -- FIFO
  fifo_inst : fifo
    generic map(
        RAM_LENGTH => fifo_size,  -- WRITE_ENTRIES
        BUS_LENGTH => 160         -- BUS_LENGTH
        )
    port map(
        clk        => clk,
        rstn       => rstn,
        write_i    => fifo_wen_o,
        read_i     => fifo_ren_o,
	read_rst_i => fifo_read_rst,
	full_o     => fifo_full,
	comp_o     => fifo_completed,
        wdata_i    => fifo_wdata,
        rdata_o    => fifo_rdata
    );


end architecture rtl;

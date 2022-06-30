-----------------------------------------------------------------------------   
-- Entity:      injector_ctrl
-- File:        injector_ctrl.vhd
-- Author:      Oriol Sala
-- Description: Main control module for the Traffic Injector
------------------------------------------------------------------------------ 
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library safety;
use safety.injector_pkg.all;

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
    dbits             : integer range 8 to 1024 := 32;      -- Data bus width of BM at injector. [Only power of 2s allowed]
    mem_Ndesc         : integer range 1 to  256 := 16;      -- Maximum number of descriptors allowed
    ASYNC_RST         : boolean                 := FALSE    -- Allow asynchronous reset flag
    );
  port (
    rstn              : in  std_ulogic;                     -- Active low reset signal
    clk               : in  std_ulogic;                     -- Clock
    -- ctrl signals from APB interface
    ctrl              : in  injector_ctrl_reg_type;         -- Control signals from APB interface
    active            : in  std_ulogic;                     -- Core enabled after reset?
    err_status        : in  std_ulogic;                     -- Core error status from APB status register    
    curr_desc_out     : out curr_des_out_type;              -- Current descriptor field out for debug display
    curr_desc_ptr     : out std_logic_vector(31 downto 0);  -- Current descriptor pointer for debug display
    status            : out status_out_type;                -- Status signals
    irq_flag_sts      : out std_ulogic;                     -- IRQ status flag
    --Bus Master signals
    bm_in             : in  bm_miso;                        -- BM signals from Bus master to control module
    bm_out            : out bm_mosi;                        -- BM signals to BusMaster interface from control module
    -- READ_IF BM signals
    read_if_bm_in     : in  bm_mosi;                        -- BM signals from READ_IF through control module  
    read_if_bm_out    : out bm_miso;                        -- BM signals to READ_IF through control module  
    -- WRITE_IF BM signals
    write_if_bm_in    : in  bm_mosi;                        -- BM signals from WRITE_IF through control module
    write_if_bm_out   : out bm_miso;                        -- BM signals to WRITE_IF through control module
    -- data descriptor out for READ_IF, WRITE_IF and DELAY
    desc_ctrl_out     : out descriptor_control;             -- Control descriptor word of the ongoing operation
    desc_actaddr_out  : out descriptor_actionaddr;          -- Action address descriptor word of the ongoing operation
    desc_branch_out   : out descriptor_branch;              -- Branch descritpor word of the ongoing operation
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
    delay_if_start    : out std_logic;

    desc_mem          : in  descriptor_memory               -- Internal memory used to store the injector program (descriptors)
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
  constant IDLE_STATE         : std_logic_vector(4 downto 0) := "00000"; -- 0x00
  constant READ_MEM_STATE     : std_logic_vector(4 downto 0) := "00011"; -- 0x03
  constant DECODE_STATE       : std_logic_vector(4 downto 0) := "00100"; -- 0x04
  
  constant READ_IF_IDLE       : std_logic_vector(4 downto 0) := "00101"; -- 0x05
  constant READ_IF_EXEC       : std_logic_vector(4 downto 0) := "00110"; -- 0x06
  constant READ_IF_DATA_READ  : std_logic_vector(4 downto 0) := "00111"; -- 0x07
  
  constant WRITE_IF_IDLE      : std_logic_vector(4 downto 0) := "01000"; -- 0x08
  constant WRITE_IF_FIRST_W   : std_logic_vector(4 downto 0) := "01001"; -- 0x09
  constant WRITE_IF_BURST     : std_logic_vector(4 downto 0) := "01010"; -- 0x0A
  constant WRITE_IF_CHECK     : std_logic_vector(4 downto 0) := "01011"; -- 0x0B  
  
  constant DELAY_IF_IDLE      : std_logic_vector(4 downto 0) := "01100"; -- 0x0C
  constant DELAY_IF_EXEC      : std_logic_vector(4 downto 0) := "01101"; -- 0x0D 

  ----------------------------------------------------------------------------- 
  -----------------------------------------------------------------------------
  --- Control FSM states ---

  -- IDLE =>
  -- Default state when errors occur or the injector is off. When the injector is enabled and no error flags 
  -- are high, the IDLE state will proceed to the READ_MEM state. However, when the injector program has been 
  -- completed the injector program, the execution will return to this IDLE state to check for the queue flag.
  -- If the queue flag is high, the program will be restarted from pointer 0. If instead is low, it will turn 
  -- off the injector and remain on the IDLE state.
  -- 
  -- READ_MEM =>
  -- Pre-loads all possible descriptor words into registers from the program memory (desc_mem).
  -- 
  -- DECODE_DESC =>
  -- Decodes the descriptor words and distributes them on the different modules, jumping to the respective states: 
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
  
  type ctrl_state_type is (idle, read_mem, decode_desc, read_if, write_if, delay_if);

  -- injector_ctrl local reg type
  type ctrl_reg_type is record
    state               : ctrl_state_type;                -- Present state register
    err_state           : std_logic_vector( 4 downto 0);  -- FSM state in which error occured
    desc_ptr            : unsigned(log_2(mem_Ndesc) downto 0);  -- Current descriptor pointer
    i                   : integer range 0 to 7;           -- Register for index increment
    rep_count           : std_logic_vector( 5 downto 0);  -- Register for Repetition Count increment
    desc_word_0         : std_logic_vector(31 downto 0);  -- Register for reading first descriptor word from desc_mem
    desc_word_1         : std_logic_vector(31 downto 0);  -- Register for reading second descriptor word from desc_mem
    desc_ctrl           : descriptor_control;             -- Register for descriptor control word
    desc_actaddr        : descriptor_actionaddr;          -- Register for descriptor action address word
    desc_branch         : descriptor_branch;              -- Register for descriptor branch word
    read_if_start       : std_ulogic;                     -- READ_IF start signal
    write_if_start      : std_ulogic;                     -- WRITE_IF start signal
    delay_if_start      : std_ulogic;                     -- DELAY_IF start signal
    err_flag            : std_ulogic;                     -- Error flag
    dcomp_flg           : std_ulogic;                     -- Descriptor completed flag
    init_error          : std_ulogic;                     -- Error occured before starting current descriptor execution
    bmst_rd_err         : std_ulogic;                     -- bus master read error
    err_status          : std_ulogic;                     -- register to find the falling edge of err_status input signal
    sts                 : status_out_type;                -- Status register   
    mem_finished        : std_ulogic;                     -- Program memory is fully read flag
  end record;

  -- Reset value for injector_ctrl local reg type
  constant CTRL_REG_RST : ctrl_reg_type := (
    state               => idle,
    err_state           => (others => '0'),
    desc_ptr            => (others => '0'),
    i                   => 0,
    rep_count           => (others => '0'),
    desc_word_0         => (others => '0'),
    desc_word_1         => (others => '0'),
    desc_ctrl           => DESCRIPTOR_CTRL_RST,
    desc_actaddr        => DESCRIPTOR_ACTADDR_RST,
    desc_branch         => DESCRIPTOR_BRANCH_RST,
    read_if_start       => '0',
    write_if_start      => '0',
    delay_if_start      => '0',
    err_flag            => '0',
    dcomp_flg           => '0',
    init_error          => '0',
    bmst_rd_err         => '0',
    err_status          => '0',
    sts                 => STATUS_OUT_RST,
    mem_finished        => '0'
    );

  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------

  signal r, rin         : ctrl_reg_type;

  -----------------------------------------------------------------------------
  -- Function/procedure declaration
  -----------------------------------------------------------------------------
  
begin  -- rtl

  -----------------------------------------------------------------------------
  -- Assignments
  -----------------------------------------------------------------------------

  -- Deassert the start signal when the READ_IF, WRITE_IF, DELAY_IF operation has started.
  read_if_start   <= '0' when read_if_sts_in.operation  = '1' else r.read_if_start;
  write_if_start  <= '0' when write_if_sts_in.operation = '1' else r.write_if_start;
  delay_if_start  <= '0' when delay_if_sts_in.operation = '1' else r.delay_if_start;

  -----------------------------------------------------------------------------
  -- Combinational logic
  ----------------------------------------------------------------------------- 
  comb : process (r, desc_mem, ctrl, active, read_if_sts_in, write_if_sts_in, delay_if_sts_in, 
    read_if_bm_in, write_if_bm_in, err_status, bm_in)
    variable v           : ctrl_reg_type; 

  begin
    --Variable initialization
    v           := r;

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
        v.bmst_rd_err         := '0';
        v.sts.desc_comp       := '0';
        v.desc_ctrl           := DESCRIPTOR_CTRL_RST;
        v.desc_actaddr        := DESCRIPTOR_ACTADDR_RST;
        v.desc_branch         := DESCRIPTOR_BRANCH_RST;
        v.desc_word_0         := (others => '0');
        v.desc_word_1         := (others => '0');

        if (ctrl.en = '1' and err_status = '0' and r.err_flag = '0') then  -- Enabled and no error
          if active = '0' then
            -- Initial starting after reset.
            -- Start descriptor read from first descriptor pointer
            v.sts.ongoing   := '1';
            v.state         := read_mem; --fetch_desc;
          elsif r.sts.ongoing = '1' or r.sts.kick = '1' or v.sts.kick = '1' then
            if r.init_error = '1' then  -- Taking up the same descriptor to fetch again since previously
                                        -- it encountered an error before reaching decoding state
              v.sts.ongoing := '1';
              v.state       := idle; --fetch_desc;
            else  -- No initial desc read error
              if r.mem_finished = '1' then  -- When program memory is fully Read, update Completed Flag
                if(ctrl.qmode = '0') then
        -- Normal Queue execution finished, update flags and end execution
                  v.sts.comp    := '1';
                else
        -- Queue Mode, re-enter memory program from pointer 0 and restart execution
                  v.sts.ongoing   := '1';
                  v.state         := read_mem;
                end if;
              end if;
            end if;
          end if;
        else  -- Error
          v.sts.ongoing := '0';
        end if;
        -----------

      when read_mem =>
        v.dcomp_flg     := '0';
        v.sts.desc_comp := '0';
        
        -- Finish operation if the desc_ptr goes above the program memory length or if previous descriptor was last.
        if ( (r.desc_ptr >= to_unsigned(mem_Ndesc - 1, r.desc_ptr'length)) or (r.desc_ctrl.last = '1') ) then
          v.desc_ptr      := (others => '0');
          v.mem_finished  := '1';
          v.state         := idle;
        else -- Increase number of words whenever any new descriptor require more words.
          v.desc_word_0   := desc_mem(to_integer(r.desc_ptr(log_2(mem_Ndesc) - 1 downto 0)));
          v.desc_word_1   := desc_mem(to_integer(r.desc_ptr(log_2(mem_Ndesc) - 1 downto 0) + 1));
          v.state         := decode_desc;
        end if;
        -----------

      when decode_desc =>
        -- Decode common control word, and then split next descriptor words depending on action type,
        -- while also incrementing accordengly the desc_ptr for next descriptor inline
        v.desc_ctrl := serial_2_desc_ctrl(r.desc_word_0);
        case v.desc_ctrl.act_type is
          when READ_OP =>
            v.read_if_start   := '1';
            v.state           := read_if;
            v.desc_actaddr    := serial_2_desc_actaddr(r.desc_word_1);
            
          when WRITE_OP =>
            v.write_if_start  := '1';
            v.state           := write_if;
            v.desc_actaddr    := serial_2_desc_actaddr(r.desc_word_1);
          
          when DELAY_OP =>
            v.delay_if_start  := '1';
            v.state           := delay_if;
            v.desc_branch     := serial_2_desc_branch(r.desc_word_1);

          when others => -- Implement METADATA OPERATIONS (which include many more words on descriptor)
            v.sts.decode_desc_err := '1';
            v.sts.err   := '1';
            v.err_flag  := '1';
            v.err_state := DECODE_STATE;
            v.state     := idle;
        end case;  --Decoding completed
        -----------

      when read_if =>
        -- Check whether the transaction was successfull or not
        v.read_if_start       := '0';
        if( (r.read_if_start = '0') and (read_if_sts_in.comp = '1') ) then 
           if( (r.rep_count < r.desc_ctrl.count) ) then -- Check COUNT parameter in Ctrl Desc Register
              v.rep_count     := add_vector(r.rep_count, 1, r.rep_count'length);
              v.read_if_start := '1';
              v.state         := read_if;
           else          
              v.sts.desc_comp := '1';
              v.dcomp_flg     := '1';
              v.rep_count     := (others => '0');
              v.desc_ptr      := r.desc_ptr + 2; -- READ descriptor type has 2 words
              v.state         := read_mem;
           end if;
        elsif read_if_sts_in.read_if_err = '1' then  -- READ_IF error
          v.sts.err           := '1';
          v.err_flag          := '1';
          v.err_state         := read_if_sts_in.state;
          v.sts.rd_data_err   := '1';
          v.state             := idle;
        end if;
        -----------

      when write_if =>
      v.write_if_start      := '0';
        -- Check whether the transaction was successfull or not
        if (r.write_if_start = '0' and write_if_sts_in.comp = '1') then
          -- Check the descriptor repetition count parameter
          if( (r.rep_count < r.desc_ctrl.count) and (or_vector(r.desc_ctrl.count) = '1') ) then -- Check COUNT parameter in Ctrl Desc Register
            v.rep_count     := add_vector(r.rep_count, 1, r.rep_count'length);
            v.write_if_start:= '1';
            v.state         := write_if;
          else
            v.sts.desc_comp := '1';
            v.dcomp_flg     := '1';
            v.rep_count     := (others => '0');
            v.desc_ptr      := r.desc_ptr + 2; -- WRITE descriptor type has 2 words
            v.state         := read_mem;
          end if;
        elsif write_if_sts_in.write_if_err = '1' then
          v.sts.err         := '1';
          v.err_flag        := '1';
          v.err_state       := write_if_sts_in.state;
          v.sts.wr_data_err := '1';
          v.state           := idle;
        end if;
        -----------

      when delay_if =>
      v.delay_if_start      := '0';
        -- Check whether the transaction was successfull or not
        if (r.delay_if_start = '0' and delay_if_sts_in.comp = '1') then
          if( (r.rep_count < r.desc_ctrl.count) and (or_vector(r.desc_ctrl.count) = '1') ) then -- Check COUNT parameter in Ctrl Desc Register
            v.rep_count     := add_vector(r.rep_count, 1, r.rep_count'length);
            v.delay_if_start:= '1';
            v.state         := delay_if;
          else
            v.sts.desc_comp := '1';
            v.dcomp_flg     := '1';
            v.rep_count     := (others => '0');
            v.desc_ptr      := r.desc_ptr + 2; -- DELAY descriptor type has 2 words
            v.state         := read_mem;
          end if;
        elsif delay_if_sts_in.delay_if_err = '1' then
          v.sts.err         := '1';
          v.err_flag        := '1';
          v.err_state       := delay_if_sts_in.state;
          v.sts.wr_data_err := '1';
          v.state           := idle;
        end if;
 
      when others =>
       v.state := idle;
    end case;

    ----------------------
    -- Signal update    --
    ----------------------

    -- Default outputs
    read_if_bm_out      <= BM_MISO_RST;
    write_if_bm_out     <= BM_MISO_RST;
    bm_out              <= BM_MOSI_RST;

    -- Redirect BM input and output to the especific module in use.
    case r.state is
      when read_if =>   -- READ_IF     
        read_if_bm_out  <= bm_in;
        bm_out          <= read_if_bm_in;
      when write_if =>  -- WRITE_IF
        write_if_bm_out <= bm_in;
        bm_out          <= write_if_bm_in;
      when others =>
        null;
    end case;

    -- state decoding for status display
    if r.err_flag = '1' then
      status.state <= r.err_state;
    else
      case r.state is
        when decode_desc =>
          status.state <= DECODE_STATE;
        when read_mem =>
          status.state <= READ_MEM_STATE;
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
      irq_flag_sts <= r.desc_ctrl.irqe and ctrl.irq_en;
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
    curr_desc_out.dbg_ctrl     <= desc_ctrl_2_serial(r.desc_ctrl);
    curr_desc_out.dbg_nxt      <= (others => '0');
    curr_desc_out.dbg_dst_addr <= r.desc_actaddr.addr;
    curr_desc_out.dbg_src_addr <= r.desc_actaddr.addr;
    curr_desc_out.dbg_sts      <= b"00" & X"0000_000" & r.err_flag & r.dcomp_flg;
    
    desc_ctrl_out <= r.desc_ctrl; -- This way, on ctrl there's the original size, while anywhere else there's the real size.
    desc_ctrl_out.size  <= add_vector(r.desc_ctrl.size, 1, desc_ctrl_out.size'length);
    desc_actaddr_out    <= r.desc_actaddr;
    desc_branch_out     <= r.desc_branch;
    ctrl_rst      <= ctrl.rst or err_status;
    curr_desc_ptr <= (31 downto r.desc_ptr'high + 1 => '0') & std_logic_vector(r.desc_ptr);
    err_sts_out   <= err_status;
    
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


end architecture rtl;

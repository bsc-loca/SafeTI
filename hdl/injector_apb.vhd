-----------------------------------------------------------------------------   
-- Entity:      injector_apb
-- File:        injector_apb.vhd
-- Author:      Oriol Sala
-- Description: APB register interface for INJECTOR.
------------------------------------------------------------------------------ 
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library safety;
use safety.injector_pkg.all;


-----------------------------------------------------------------------------
-- Entity to read and write APB registers for Injector.
-----------------------------------------------------------------------------

entity injector_apb is
  generic (
    pindex          : integer                           := 0;         -- APB configuartion slave index
    paddr           : integer                           := 0;         -- APB configuartion slave address
    pmask           : integer                           := 16#FFF#;   -- APB configuartion slave mask
    pirq            : integer range 0 to APB_IRQ_NMAX-1 := 0;         -- APB configuartion slave irq
    mem_Ndesc       : integer range 1 to 1024           := 16;        -- Maximum number of descriptor words allowed
    ASYNC_RST       : boolean                           := TRUE       -- Allow asynchronous reset flag
    );
  port (
    rstn            : in  std_ulogic;                       -- Reset
    clk             : in  std_ulogic;                       -- Clock
    apbi            : in  apb_slave_in_type;                -- APB slave input
    apbo            : out apb_slave_out_type;               -- APB slave output
    ctrl_out        : out injector_ctrl_reg_type;           -- Control configuration signals
    active          : out std_ulogic;                       -- Injector enabled after reset, status
    err_status      : out std_ulogic;                       -- Core error status in APB status register
    irq_flag_sts    : in  std_ulogic;                       -- IRQ flag     
    curr_desc_in    : in  curr_des_out_type;                -- Current descriptor fields for debug display
    curr_desc_ptr   : in  std_logic_vector(31 downto 0);    -- Current descriptor pointer for debug display
    sts_in          : in  status_out_type;                  -- Status flags from control module
    desc_mem_out    : out descriptor_memory                 -- Internal memory used to store the injector program (descriptors)
    );
end entity injector_apb;

------------------------------------------------------------------------------
-- Architecture of injector_apb
------------------------------------------------------------------------------

architecture rtl of injector_apb is
  attribute sync_set_reset : string;
  attribute sync_set_reset of rstn : signal is "true"; 
  -----------------------------------------------------------------------------
  -- Constant declaration
  -----------------------------------------------------------------------------


  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------

  signal r,                 rin                 : injector_reg_type;
  signal r_desc_mem,        rin_desc_mem        : descriptor_memory(mem_Ndesc - 1 downto 0);  -- Memory program to be written by APB
  signal r_desc_apb_ptr,    rin_desc_apb_ptr    : unsigned(log_2(mem_Ndesc) downto 0);        -- APB pointer to write descriptor words

  -----------------------------------------------------------------------------
  -- Function/procedure declaration
  -----------------------------------------------------------------------------
  
begin -- rtl

  -----------------------------------------------------------------------------
  -- Assignments
  -----------------------------------------------------------------------------
  
  apbo.index    <= pindex;
  desc_mem_out  <= r_desc_mem;
  
  -----------------------------------------------------------------------------
  -- Combinational process
  -----------------------------------------------------------------------------
  
  comb : process (apbi, r, r_desc_mem, r_desc_apb_ptr, sts_in, curr_desc_in, curr_desc_ptr, irq_flag_sts)
    variable v                  : injector_reg_type;
    variable v_desc_mem         : descriptor_memory(r_desc_mem'range);
    variable v_desc_apb_ptr     : unsigned(r_desc_apb_ptr'range);
    variable prdata : std_logic_vector(31 downto 0);

  begin
    -- Initialization
    v             := r;
    prdata        := (others => '0');
    v_desc_mem    := r_desc_mem;
    v_desc_apb_ptr:= r_desc_apb_ptr;

    ----------------------
    -- core status logic
    ----------------------

    -- Core Status update
    v.sts.comp    := sts_in.comp;
    v.sts.ongoing := sts_in.ongoing;

    -- Error updates
    if sts_in.err = '1' then
      v.sts.err := '1';
    end if;
    if sts_in.decode_desc_err = '1' then
      v.sts.decode_err := '1';
    end if;
    if sts_in.rd_desc_err = '1' then
      v.sts.rd_desc_err := '1';
    end if;
    if sts_in.rd_data_err = '1' then
      v.sts.read_if_rd_data_err := '1';
    end if;
    if sts_in.wr_data_err = '1' then
      v.sts.write_if_wr_data_err := '1';
    end if;
    if sts_in.rd_nxt_ptr_err = '1' then
      v.sts.rd_nxt_ptr_err := '1';
    end if;
      
    -- Set the active bit in status register when Injector is enabled after reset
    if r.ctrl.en = '1' then
      v.sts.active := '1';
    end if;

    -- Interrupt flag on descriptor completion or error
    v.sts.irq_flag := irq_flag_sts;

    -- Clear kick bit once kick status signal from injector_ctrl becomes zero.
    v.ctrl.kick     := '0';
    v.sts.kick_pend := sts_in.kick or r.ctrl.kick;

    -- Clear kick bit once restart status signal from injector_ctrl becomes zero.
    --v.ctrl.restart     := '0';
    --v.sts.restart_pend := sts_in.restart or r.ctrl.restart;


    ----------------------  
    -- APB address decode  
    ----------------------
    
    ---- Read accesses ----
    if (apbi.sel(pindex) and apbi.en and not apbi.wr_en) = '1' then
      case apbi.addr(7 downto 2) is
        when "000000" =>                --0x00 Injector control register
          prdata(0) := r.ctrl.en;
          prdata(1) := r.ctrl.rst;
          prdata(2) := r.ctrl.kick;
          prdata(3) := r.ctrl.irq_en;
          prdata(4) := r.ctrl.irq_err;
          prdata(5) := r.ctrl.qmode;
        when "000001" =>                --0x04 Injector status register. 
          prdata(0)  := r.sts.comp;
          prdata(1)  := r.sts.err;
          prdata(2)  := r.sts.ongoing;
          prdata(3)  := r.sts.kick_pend;
          prdata(4)  := r.sts.irq_flag;
          prdata(5)  := r.sts.decode_err;
          prdata(6)  := r.sts.rd_desc_err;
          prdata(7)  := r.sts.read_if_rd_data_err;
          prdata(8)  := r.sts.write_if_wr_data_err;
          prdata(9)  := r.sts.rd_nxt_ptr_err;
          prdata(14 downto 10) := sts_in.state;
          --prdata(24 downto 15) := sts_in.count;
          prdata(31 downto 15) := (others => '0');
        when "000010" =>                 --0x08 Injector First descriptor pointer
          prdata(31 downto 0) := (others => '0');
        when "000011" =>                 --0x0C Injector future capabilities register :TODO
          prdata(31 downto 0) := (others => '0');
        when "000100" =>                 --0x10 Current descriptor control field for debug.
          prdata(31 downto 0) := curr_desc_in.dbg_ctrl;
        when "000101" =>                 --0x14 Current descriptor's nxt_des_ptr field for debug.
          prdata(31 downto 0) := curr_desc_in.dbg_nxt;
        when "000110" =>                 --0x18 Descriptor Destination addres
          prdata(31 downto 0) := curr_desc_in.dbg_dst_addr;
        when "000111" =>                 --0x1C Descriptor Source address
          prdata(31 downto 0) := curr_desc_in.dbg_src_addr;
        when "001000" =>                 --0x20 Descriptor status word
          prdata(31 downto 0) := curr_desc_in.dbg_sts;
        when "001001" =>                 --0x24 Current descriptor pointer field for debug
          prdata(31 downto 0) := curr_desc_ptr;
        when others => 
          null;
      end case;
    end if;
    
    ---- Write accesses ----
    if (apbi.sel(pindex) and apbi.en and apbi.wr_en ) = '1' then
      case apbi.addr(7 downto 2) is
        when "000000" =>                --0x00 Injector control register
          v.ctrl.en      := apbi.wdata(0);
          v.ctrl.rst     := apbi.wdata(1);
          v.ctrl.kick    := apbi.wdata(2);
          v.ctrl.irq_en  := apbi.wdata(3);
          v.ctrl.irq_err := apbi.wdata(4);
          v.ctrl.qmode   := apbi.wdata(5);
        when "000001" =>                --0x04 Injector status register. Errors are cleared on write
          v.sts.err                   := r.sts.err and not(apbi.wdata(1));
          v.sts.irq_flag              := r.sts.irq_flag and not(apbi.wdata(5));
          v.sts.decode_err            := r.sts.decode_err and not(apbi.wdata(6));
          v.sts.rd_desc_err           := r.sts.rd_desc_err and not(apbi.wdata(7));        
          v.sts.read_if_rd_data_err   := r.sts.read_if_rd_data_err and not(apbi.wdata(8));
          v.sts.write_if_wr_data_err  := r.sts.write_if_wr_data_err and not(apbi.wdata(9));
          v.sts.rd_nxt_ptr_err        := r.sts.rd_nxt_ptr_err and not(apbi.wdata(10));
        when "111111" =>                --0xFC Descriptor word input
          if(r_desc_apb_ptr <= to_unsigned(mem_Ndesc - 1, r_desc_apb_ptr'length)) then
            v_desc_mem(to_integer(r_desc_apb_ptr)) := apbi.wdata;
            v_desc_apb_ptr  := r_desc_apb_ptr + 1;  -- Increase pointer for next word
          else -- Internal program memory is full
            -- TODO: ERROR FULL INJECTOR MEMORY
          end if;
        when others =>
          null;
      end case;
    end if;


    ----------------------
    -- Signal update --
    ----------------------

    rin               <= v;
    rin_desc_mem      <= v_desc_mem;
    rin_desc_apb_ptr  <= v_desc_apb_ptr;
    apbo.rdata        <= prdata;
    apbo.irq          <= (others => '0');
    -- IRQ pulse generation
    if sts_in.err = '1' then
      apbo.irq(pirq) <= r.ctrl.irq_en and r.ctrl.irq_err;
    elsif sts_in.desc_comp = '1' then
      if (curr_desc_in.dbg_ctrl(4) or curr_desc_in.dbg_ctrl(3) or curr_desc_in.dbg_ctrl(2) or curr_desc_in.dbg_ctrl(1)) = '0' then
        --apbo.pirq(pirq) <= curr_desc_in.dbg_ctrl(8) and r.ctrl.irq_en;
      else
        apbo.irq(pirq) <= curr_desc_in.dbg_ctrl(4) and r.ctrl.irq_en;
      end if;
    end if;
    --  control signals
    ctrl_out        <= r.ctrl;
    err_status      <= r.sts.err or r.sts.decode_err or r.sts.rd_desc_err or
                       r.sts.read_if_rd_data_err or r.sts.write_if_wr_data_err or
                       r.sts.rd_nxt_ptr_err;
    active          <= r.sts.active;
    
  end process comb;
  
  -----------------------------------------------------------------------------
  -- Sequential process
  -----------------------------------------------------------------------------
  
  seq : process (clk, rstn)
  begin
    if (rstn = '0' and ASYNC_RST) then -- Asynchronous reset
      r               <= INJECTOR_REG_RST; 
      r_desc_mem      <= (others => (others => '0'));
      r_desc_apb_ptr  <= (others => '0');
    elsif rising_edge(clk) then
      if rstn = '0' or r.ctrl.rst = '1' then
        r               <= INJECTOR_REG_RST;
        r_desc_mem      <= (others => (others => '0'));
        r_desc_apb_ptr  <= (others => '0');
      else
        r               <= rin;
        r_desc_mem      <= rin_desc_mem;
        r_desc_apb_ptr  <= rin_desc_apb_ptr;
      end if;
    end if;
  end process seq;
  
  -----------------------------------------------------------------------------
  -- Component instantiation
  -----------------------------------------------------------------------------

end architecture rtl;

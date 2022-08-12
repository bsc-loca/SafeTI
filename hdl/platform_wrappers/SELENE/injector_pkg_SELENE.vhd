------------------------------------------------------------------------------
-- Package:     injector_pkg_selene
-- File:        injector_pkg_selene.vhd
-- Author:      Francis Fuentes
-- Description: Internal package for AHB interface of the injector.
--              Only to be loaded by the platform.
------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library grlib;
use grlib.amba.all;
library techmap;
use techmap.gencomp.all;


package injector_pkg_selene is

  -- AHB interface wrapper for SELENE platform
  component injector_ahb_SELENE is
    generic (
    -- SafeTI configuration
      dbits             : integer range 32 to  128      := 32;        -- Data width of BM and FIFO at injector. [Only power of 2s allowed]
      MAX_SIZE_BURST    : integer range 32 to 1024      := 1024;      -- Maximum byte size of a network/BM transaction. 1024/4096 for AHB/AXI4
      tech              : integer range 0 to NTECH      := inferred;  -- Target technology
    -- APB configuration  
      pindex            : integer                       := 0;
      paddr             : integer                       := 0;
      pmask             : integer                       := 16#FFF#;
      pirq              : integer range 0 to NAHBIRQ-1  := 0;
    -- AHB configuration
      hindex            : integer                       := 0
    );
    port (
      rstn              : in  std_ulogic;
      clk               : in  std_ulogic;
    -- APB interface signals
      apbi              : in  apb_slv_in_type;
      apbo              : out apb_slv_out_type;
    -- AHB interface signals
      ahbmi             : in  ahb_mst_in_type;
      ahbmo             : out ahb_mst_out_type
    );
  end component injector_ahb_SELENE;

  -- AXI interface wrapper for SELENE platform
  component injector_axi4_SELENE is
    generic (
    -- SafeTI configuration
      MAX_SIZE_BURST: integer range 32 to 4096            := 4096;    -- Maximum size of a beat at a burst transaction.
      tech          : integer range  0 to NTECH           := inferred;-- Target technology
      two_injectors : boolean                             := FALSE;   -- Implement two SafeTI modules for continuous transaction requests
    -- APB configuration  
      pindex        : integer                             := 0;       -- APB configuartion slave index
      paddr         : integer                             := 0;       -- APB configuartion slave address
      pmask         : integer                             := 16#FFF#; -- APB configuartion slave mask
      pirq          : integer range  0 to NAHBIRQ-1       := 0;       -- APB configuartion slave irq
    -- AXI Manager configuration
      ID_R_WIDTH    : integer range  0 to   32            := 4;       -- AXI ID's bus width.
      ID_W_WIDTH    : integer range  0 to   32            := 4;       -- AXI ID's bus width.
      ADDR_WIDTH    : integer range 12 to   64            := 32;      -- AXI address bus width. (Tested only for 32 bits)
      DATA_WIDTH    : integer range  8 to 1024            := 128;     -- AXI data bus width. [Only power of 2s are allowed]
      axi_id        : integer range  0 to 32**2-1         := 0;       -- AXI manager burst index [Must be < ID_X_WIDTH**2-1]
      axi_cache     : std_logic_vector(3 downto 0)        := "0000";  -- AXI CACHE signaling profile.
      axi_prot      : std_logic_vector(2 downto 0)        := "000";   -- AXI PROT signaling profile.
      axi_qos       : std_logic_vector(3 downto 0)        := "0000";  -- AXI QOS signaling profile.
      axi_region    : std_logic_vector(3 downto 0)        := "0000";  -- AXI REGION signaling profile.
      rd_n_fifo_regs: integer range  2 to  256            := 4;       -- Number of FIFO registers to use at AXI read transactions.  [Only power of 2s are allowed]
      wr_n_fifo_regs: integer range  2 to  256            := 4        -- Number of FIFO registers to use at AXI write transactions. [Only power of 2s are allowed]
    );
    port (
      rstn          : in  std_ulogic;       -- Reset
      clk           : in  std_ulogic;       -- Clock
    -- APB interface signals
      apbi          : in  apb_slv_in_type;  -- APB subordinate input to injector
      apbo          : out apb_slv_out_type; -- APB subordinate output from injector
    -- AXI4 interconnect SELENE bus
      axi4mi        : in  axi_somi_type;    -- AXI4 manager input to injector
      axi4mo        : out axi4_mosi_type    -- AXI4 manager output from injector
    );
  end component injector_axi4_SELENE;
  
  -- IF function for when VHDL can not use if (like at constants).
  function sel              (A, B : integer; sel : boolean) return integer;

end package injector_pkg_selene;

package body injector_pkg_selene is

  -- IF function that outputs the first input if the boolean is true, the second if false.
  function sel(A, B : integer; sel : boolean) return integer is
  begin
    if sel then return A;
    else return B;
    end if;
  end sel;

end package body injector_pkg_selene;

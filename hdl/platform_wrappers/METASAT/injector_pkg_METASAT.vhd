------------------------------------------------------------------------------
-- Package:     injector_pkg_metasat
-- File:        injector_pkg_metasat.vhd
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


package injector_pkg_metasat is

  -- AXI interface wrapper for METASAT platform
  component injector_axi4_METASAT is
    generic (
    -- SafeTI configuration
      INJ_MEM_LENGTH    : integer range  2 to   10      :=    4;      -- Set the maximum number of programmable descriptor words to 2^INJ_MEM_LENGTH
      MAX_SIZE_BURST    : integer range 32 to 4096      := 4096;      -- Maximum size of a beat at a burst transaction.
      tech              : integer range  0 to NTECH     := inferred;  -- Target technology
    -- APB configuration
      pindex            : integer                       := 0;         -- APB configuration subordinate index
      paddr             : integer                       := 0;         -- APB configuration subordinate address
      pmask             : integer                       := 16#FFF#;   -- APB configuration subordinate mask
      pirq              : integer range  0 to NAHBIRQ-1 := 0;         -- APB configuration subordinate irq
    -- AXI Manager configuration
      ID_R_WIDTH        : integer range  0 to   32      := 4;         -- AXI ID's bus width.
      ID_W_WIDTH        : integer range  0 to   32      := 4;         -- AXI ID's bus width.
      ADDR_WIDTH        : integer range 12 to   64      := 32;        -- AXI address bus width. (Tested only for 32 bits)
      DATA_WIDTH        : integer range  8 to 1024      := 128;       -- AXI data bus width. [Only power of 2s are allowed]
      axi_id            : integer range  0 to 32**2-1   := 0;         -- AXI manager burst index [Must be < ID_X_WIDTH**2-1]
      axi_cache         : std_logic_vector(3 downto 0)  := "0000";    -- AXI CACHE signaling profile.
      axi_prot          : std_logic_vector(2 downto 0)  := "000";     -- AXI PROT signaling profile.
      axi_qos           : std_logic_vector(3 downto 0)  := "0000";    -- AXI QOS signaling profile.
      axi_region        : std_logic_vector(3 downto 0)  := "0000";    -- AXI REGION signaling profile.
      rd_n_fifo_regs    : integer range  2 to  256      := 4;         -- Number of FIFO registers to use at AXI read transactions.  [Only power of 2s are allowed]
      wr_n_fifo_regs    : integer range  2 to  256      := 4          -- Number of FIFO registers to use at AXI write transactions. [Only power of 2s are allowed]
    );
    port (
      rstn              : in  std_ulogic;       -- Reset
      clk               : in  std_ulogic;       -- Clock
    -- APB interface signals
      apbi              : in  apb_slv_in_type;  -- APB subordinate input to injector
      apbo              : out apb_slv_out_type; -- APB subordinate output from injector
    -- AXI4 interconnect bus
      axi4mi            : in  axi_somi_type;    -- AXI4 manager input to injector
      axi4mo            : out axi4_mosi_type;   -- AXI4 manager output from injector
    -- AHB External Subordinate interface signals
      axi4_snoop        : in  axi4_mosi_type    -- AXI4 snooping bus for synchronization
    );
  end component injector_axi4_METASAT;

  -- IF function for when VHDL can not use if (like at constants).
  function sel(A, B : integer; sel : boolean) return integer;

end package injector_pkg_metasat;

package body injector_pkg_metasat is

  -- IF function that outputs the first input if the boolean is true, the second if false.
  function sel(A, B : integer; sel : boolean) return integer is
  begin
    if sel then return A;
    else return B;
    end if;
  end sel;

end package body injector_pkg_metasat;

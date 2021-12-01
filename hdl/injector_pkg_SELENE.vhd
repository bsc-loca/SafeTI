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
      tech              : integer range 0 to NTECH          := inferred;
      -- APB configuration  
      pindex            : integer                           := 0;
      paddr             : integer                           := 0;
      pmask             : integer                           := 16#FFF#;
      pirq              : integer range 0 to NAHBIRQ-1      := 0;
      -- Bus master configuration
      dbits             : integer range 32 to 128           := 32;
      hindex            : integer                           := 0;
      MAX_SIZE_BEAT     : integer range 32 to 1024          := 1024
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

end package injector_pkg_selene;

package body injector_pkg_selene is

end package body injector_pkg_selene;

## Interface libraries, modules and wrappers

This folder contains the VHDL files related to the network interfaces (where the injector generates traffic), such as for AXI4 or AHB communication protocols.

The file nomenclature followed is:
* **injector_ _PROTOCOL_.vhd** is the wrapper file used to integrate both interface and injector core modules. This, however, does not include anything intermediate between the platform and the SafeTI, since that's a task for the platform wrappers found on `hdl/platform_wrappers`.
* **_PROTOCOL_ _manager.vhd** is the interface module that manages requests between the network where the traffic injection is performed and the injector core.
* **_PROTOCOL_ _pkg.vhd** is the library file for that specific protocol, which is required to be able to integrate the interface module on the wrapper file and be able to compile the interface.

Note that the AHB protocol does not have an interface module at the moment, since an AHB interface from the platform is used at that instance.

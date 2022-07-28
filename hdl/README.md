## SafeTI components

This folder contains the VHDL core components of the SafeTI distributed in the following folders as such:

* On this same folder it is listed all the core components of the traffic injector, which are:
 * **injector_core.vhd** is the wrapper file that intragrates all the other core components of the injector.
 * **injector_apb.vhd** is the basic APB completer interface to be able to program and debug the SafeTI. The injector configuration is located on this component.
 * **injector_fetch.vhd**, **injector_decode.vhd** and **injector_exe.vhd** are the modules that integrate the SafeTI pipeline.
 * **injector_control.vhd** is a combinatorial block that manages the configuration and control signals from all the different components to control the program execution.
 * **injector_pkg.vhd** is a library file listing some labels, types and functions that are common on various components of the injector. The intent of this library is to just hold only what is exclusively necessary to keep it tidy. Types and constants that are used only on specific modules are declared on that module if not used anywhere else.

* **exe_submodules** folder contains the submodule components used at the _injector_exe.vhd_ pipeline module.

* **network_interfaces** folder includes the files that bring native support to the communication protocol of the network where the traffic injection is performed.

* **platform_wrappers** folder includes the subfolders which the SafeTI provides native support. This does not include the drivers, since this folder is exclusively for HDL designs.

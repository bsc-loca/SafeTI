## EXE stage submodules

This folder contains the VHDL files related to the submodules from the EXEcution stage of the injector core pipeline. These are the actual modules that perform the programmed task in the injection program or descriptors, communicating with the network interface through the BM buses.

The file nomenclature followed is:
* **injector_ _TYPE_.vhd** is the file containing the submodule with the same name that performs the task TYPE, at least. For example, the _injector_write.vhd_ implements the logic that injects WRITE transactions on the network, but it also can perform WRITE_FIX transactions using the same logic.

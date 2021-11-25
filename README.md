# Traffic Injector 
BSC Traffic injector unit

## Description
This unit acts as an AHB Master IP connected to the main AMBA bus on the SELENE Platform. It acts as a core with limited capabilities, only generating transactions to the bus by reading and writing to the AHB Slave RAM memory and controlled via APB registers.
The injector works along with the multi-core setup instantiated on the platform and other peripherals and monitoring units.
In order to generate traffic to the bus, the module performs a set of AMBA transactions based on data descriptors set at startup into a predefined memory address range.
A high-level block diagram of the module is shown, its internal components and respective configuration and functionalities are described

![High Level Module](docs/Injector_Specs/img/injector_high_level.png)

![Sub-modules signals](docs/Injector_Specs/img/injector_low_level.png)


## Descriptors
Descriptors are used to define, control, and monitor transactions in the Traffic Injector. Descriptor types supported by this module can be classified, as of today, as read and write descriptors. Furthermore, each transaction type has the possibility of starting a **burst transfer** by **not fixing** the Source and Destinations bits in the **Descriptor Control Word**.

![Descriptors](docs/Injector_Specs/img/descriptors.png)

For more informatio regarding the use of the Module, see [Specifications](docs/Injector_Specs.pdf)


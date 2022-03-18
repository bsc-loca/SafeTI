# HW Platform wrapper files

This folder contains all the wrappers used in the platforms that the SafeTI has been integrated.

In the folder of each platform, there's three type of files:

- **injector_pkg_PLATFORM.vhd**: Library file that is used to allow instantiation of the wrapper component.
- **injector_INTERFACE_PLATFORM.vhd**: Wrapper component used to parse the connections between SafeTI and PLATFORM types. The INTERFACE tag indicates the port where the traffic is injected.
- **injector_INTERFACE_PLATFORM.txt**: A list of the components that must be called during synthesis/simulation on the PLATFORM for the specific INTERFACE.

These files are expected to be copied to where the SafeTI repository, **bsc_safeti**, is cloned inside a folder named **safety**, the name of the library all files of this repository must be included to. Furthermore, the **injector_INTERFACE_PLATFORM.txt** can be renamed to what the PLATFORM requires, for example on the SELENE platform the file would be renamed to **vhdlsyn.txt**, leaving a tree as:

```bash
safety
├── bsc_safeti
│   ├── ci
│   ├── docs
│   ├── hdl
│   ├── LICENSE
│   ├── Makefile
│   ├── README.md
│   ├── sw
│   ├── synthesis
│   └── tb
├── injector_ahb_SELENE.vhd
├── injector_pkg_SELENE.vhd
└── vhdlsyn.txt
```

If multiple SafeTI modules with different INTERFACE are required to be implemented, it can be done by copying all required **injector_INTERFACE_PLATFORM.vhd** and merging all **injector_INTERFACE_PLATFORM.txt** files for each INTERFACE used.

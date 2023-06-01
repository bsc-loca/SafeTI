#Simulations results are stored in transcript
#Traces of the simulation are stored in vsim.wlf when Questa is launched with the gui (vsim-launch)

hdl=../../../hdl
hdl_sub=$(hdl)/exe_submodules
hdl_int=$(hdl)/network_interfaces

#Compiles the files and creates the library safety
compile: clean
	vlib safety
	mkdir -p safety
	vmap safety safety
	vcom -work safety $(hdl)/injector_pkg.vhd $(hdl_int)/axi4_pkg.vhd
	vcom -2008 -work safety $(hdl)/../tb/questasim/tb_injector_pkg.vhd
	vcom -2008 $(hdl)/../tb/questasim/tb_injector_axi/tb_injector_axi.vhd 
	vlog $(hdl)/../tb/questasim/tb_injector_axi/noInclude/subordinate_v1_0.v $(hdl)/../tb/questasim/tb_injector_axi/noInclude/subordinate_v1_0_S00_AXI.v 
	vcom $(hdl_int)/injector_axi.vhd $(hdl_int)/axi4_manager.vhd $(hdl)/injector_core.vhd $(hdl)/injector_apb.vhd $(hdl)/injector_fetch.vhd $(hdl)/injector_decode.vhd $(hdl)/injector_exe.vhd $(hdl)/injector_control.vhd $(hdl_sub)/injector_delay.vhd $(hdl_sub)/injector_read.vhd $(hdl_sub)/injector_write.vhd

#Launches the simulation with the graphical interface
vsim-launch: compile
	vsim -voptargs=+acc tb_injector_axi -do "do wave.do" -do "run -all"

#Launches the simulation with the GUI and coverage enabled
vsim-coverage: compile
	vcom -coveropt 3 +cover +acc $(hdl)/axi4_manager.vhd
	vsim -coverage -vopt tb_injector_axi -do "coverage save -onexit coverage.ucbd; run -all;" <<!
	vcover report -details coverage.ucbd

#Launches the simulation in batch mode
vsim: compile
	vsim -voptargs=+acc tb_injector_axi -do "run -all" <<! 
    
#Analyzes the results of the simulation
launch-tb : vsim
	@echo "\n\n\n\n\n"
	@errors=`grep 'Errors: 0' transcript` ; \
	echo "$$errors" ; \
	if [ "$$errors" = "" ]; then \
	echo 'TEST FAILED, SOME ERRORS FOUND:' ; \
	grep Error transcript ; \
	else \
	echo 'TEST PASSED WITHOUT ERRORS' ; \
	fi

#Cleans the temporal files
clean:
	rm -rf safety
	rm -rf work
	rm -f modelsim.ini
	rm -f transcript
	rm -f vsim_stacktrace.vstf  
	rm -f vsim.wlf


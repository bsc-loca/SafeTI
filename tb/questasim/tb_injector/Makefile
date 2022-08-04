#Simulations results are stored in transcript
#Traces of the simulation are stored in vsim.wlf when Questa is launched with the gui (vsim-launch)

hdl=../../../hdl
hdl_sub=$(hdl)/exe_submodules

#Compiles the files and creates the library safety
compile: clean
	vlib safety
	mkdir -p safety
	vmap safety safety
	vcom -2008 -work safety $(hdl)/injector_pkg.vhd $(hdl)/../tb/questasim/tb_injector_pkg.vhd
	vcom -2008 tb_injector.vhd
	vcom $(hdl)/injector_core.vhd $(hdl)/injector_apb.vhd $(hdl)/injector_fetch.vhd $(hdl)/injector_decode.vhd $(hdl)/injector_exe.vhd $(hdl)/injector_control.vhd $(hdl_sub)/injector_delay.vhd $(hdl_sub)/injector_read.vhd $(hdl_sub)/injector_write.vhd

#Launches the simulation with the graphical interface
vsim-launch: compile
	vsim -voptargs=+acc tb_injector -do "do wave.do" -do "run -all"

#Launches the simulation in batch mode
vsim: compile
	vsim -voptargs=+acc tb_injector -do "run -all" <<!

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

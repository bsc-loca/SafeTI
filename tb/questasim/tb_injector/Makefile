#Simulations results are stored in transcript
#Traces of the simulation are stored in vsim.wlf when Questa is launched with the gui (vsim-launch)

hdl=../../../hdl

#Compiles the files and creates the library safety
compile: clean
	vlib bsc
	mkdir -p bsc
	vmap bsc bsc
	vcom -2008 -work bsc $(hdl)/injector_pkg.vhd $(hdl)/../tb/questasim/tb_injector_pkg.vhd
	vcom -2008 tb_injector.vhd
	vcom $(hdl)/injector.vhd $(hdl)/injector_apb.vhd $(hdl)/injector_ctrl.vhd $(hdl)/fifo.vhd $(hdl)/injector_delay_if.vhd $(hdl)/injector_read_if.vhd $(hdl)/injector_write_if.vhd 

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
	rm -rf bsc
	rm -rf work
	rm -f modelsim.ini
	rm -f transcript
	rm -f vsim_stacktrace.vstf  
	rm -f vsim.wlf


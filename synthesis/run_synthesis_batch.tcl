#
# NOTE: typical usage would be "vivado -mode tcl -source run_synthesis_batch.tcl"
#
# STEP#0: define output directory area.
#
set outputDir ./output
file mkdir $outputDir
#
# STEP#1: setup design sources and constraints
#
read_vhdl -library bsc ../hdl/injector_pkg.vhd
read_vhdl ../hdl/injector_ahb.vhd  
read_vhdl ../hdl/injector.vhd
read_vhdl ../hdl/injector_apb.vhd
read_vhdl ../hdl/injector_ctrl.vhd
read_vhdl ../hdl/injector_read_if.vhd
read_vhdl ../hdl/injector_write_if.vhd
read_vhdl ../hdl/injector_delay_if.vhd
read_vhdl ../hdl/fifo.vhd

#read_vhdl -library grlib /home/develop/selene-hardware/grlib/lib/grlib/stdlib/version.vhd
#read_vhdl -library grlib /home/develop/selene-hardware/grlib/lib/grlib/stdlib/config.vhd
#read_vhdl -library grlib /home/develop/selene-hardware/grlib/lib/grlib/stdlib/config_types.vhd
#read_vhdl -library grlib /home/develop/selene-hardware/grlib/lib/grlib/stdlib/stdlib.vhd
#
#read_vhdl -library grlib /home/develop/selene-hardware/grlib/lib/grlib/amba/amba.vhd
#read_vhdl -library grlib /home/develop/selene-hardware/grlib/lib/grlib/amba/devices.vhd
#read_vhdl -library grlib /home/develop/selene-hardware/grlib/lib/grlib/amba/defmst.vhd
#read_vhdl -library grlib /home/develop/selene-hardware/grlib/lib/grlib/amba/apbctrl.vhd
#read_vhdl -library grlib /home/develop/selene-hardware/grlib/lib/grlib/amba/apbctrlx.vhd
#read_vhdl -library grlib /home/develop/selene-hardware/grlib/lib/grlib/amba/apbctrldp.vhd
#read_vhdl -library grlib /home/develop/selene-hardware/grlib/lib/grlib/amba/apbctrlsp.vhd
#read_vhdl -library grlib /home/develop/selene-hardware/grlib/lib/grlib/amba/ahbctrl.vhd
#read_vhdl -library grlib /home/develop/selene-hardware/grlib/lib/grlib/amba/dma2ahb_pkg.vhd
#read_vhdl -library grlib /home/develop/selene-hardware/grlib/lib/grlib/amba/dma2ahb.vhd
#read_vhdl -library grlib /home/develop/selene-hardware/grlib/lib/grlib/amba/ahbmst.vhd
#read_vhdl -library grlib /home/develop/selene-hardware/grlib/lib/grlib/amba/ahblitm2ahbm.vhd
#
#read_vhdl -library grlib /home/develop/selene-hardware/grlib/lib/grlib/generic_bm/generic_bm_pkg.vhd
#
#read_vhdl -library techmap /home/develop/selene-hardware/grlib/lib/techmap/gencomp/gencomp.vhd
#read_vhdl -library techmap /home/develop/selene-hardware/grlib/lib/techmap/gencomp/netcomp.vhd


#
# STEP#2: run synthesis, report utilization and timing estimates, write checkpoint design
#
synth_design -top injector_ahb 
create_clock -name clk -period 10 [get_ports clk]
set_input_delay -clock clk 0 [all_inputs]
set_output_delay -clock clk 0 [all_outputs]
write_checkpoint -force $outputDir/post_synth
report_timing_summary -report_unconstrained -file $outputDir/post_synth_timing_summary.rpt
#report_timing_summary -file $outputDir/post_synth_timing_summary.rpt
report_power -file $outputDir/post_synth_power.rpt
exit

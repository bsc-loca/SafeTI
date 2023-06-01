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
read_vhdl -library safety ../hdl/injector_pkg.vhd
read_vhdl ../hdl/injector_core.vhd
read_vhdl ../hdl/injector_apb.vhd
read_vhdl ../hdl/injector_fetch.vhd
read_vhdl ../hdl/injector_decode.vhd
read_vhdl ../hdl/injector_exe.vhd
read_vhdl ../hdl/injector_control.vhd
read_vhdl ../hdl/exe_submodules/injector_delay.vhd
read_vhdl ../hdl/exe_submodules/injector_read.vhd
read_vhdl ../hdl/exe_submodules/injector_write.vhd
read_vhdl -library safety ../hdl/network_interfaces/axi4_pkg.vhd
read_vhdl ../hdl/network_interfaces/injector_axi.vhd
read_vhdl ../hdl/network_interfaces/axi4_manager.vhd

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
# Delete mode to allow bitstream  -mode out_of_context
#synth_design -top injector_ahb -part xcku040-ffva1156-3-e -mode out_of_context
synth_design -top injector_axi -part xcku040-ffva1156-3-e -mode out_of_context
#synth_design -top axi4_manager -part xcku040-ffva1156-3-e -mode out_of_context
# 10=100MHz 5=200MHz 4=250MHz 3=300MHz 2.5=400MHz 2=500MHz
create_clock -name clk -period 4 [get_ports clk]

#set_input_delay -clock clk 0 [all_inputs]
#set_output_delay -clock clk 0 [all_outputs]
#config_timing_analysis -ignore_io_paths true -enable_input_delay_default_clock true

write_checkpoint -force $outputDir/post_synth.dcp
report_timing_summary -report_unconstrained -file $outputDir/post_synth_timing_summary.rpt
report_power -file $outputDir/post_synth_power.rpt
report_utilization -file $outputDir/post_synth_util.rpt

#opt_design
#place_design
#report_clock_utilization -file $outputDir/clock_util.rpt
# Optionally run optimization if there are timing violations after placement
#if {[get_property SLACK [get_timing_paths -max_paths 1 -nworst 1 -setup]] < 0} {
# puts "Found setup timing violations => running physical optimization"
# phys_opt_design
#}
#write_checkpoint -force $outputDir/post_place.dcp
#report_utilization -file $outputDir/post_place_util.rpt
#report_timing_summary -report_unconstrained -file $outputDir/post_place_timing_summary.rpt

#route_design
#write_checkpoint -force $outputDir/post_route.dcp
#report_route_status -file $outputDir/post_route_status.rpt
#report_timing_summary -file $outputDir/post_route_timing_summary.rpt
#report_power -file $outputDir/post_route_power.rpt
#report_drc -file $outputDir/post_imp_drc.rpt
exit

# Generate AXI4 Full Subordinate memory script for testbench
create_project project_1 ./vivado -part xcku040-fbva676-3-e
set_property target_language VHDL [current_project]
create_peripheral user.org user subordinate 1.0 -dir ./ip_repo
add_peripheral_interface S00_AXI -interface_mode slave -axi_type full [ipx::find_open_core user.org:user:subordinate:1.0]
set_property VALUE 1024 [ipx::get_bus_parameters WIZ_MEMORY_SIZE -of_objects [ipx::get_bus_interfaces S00_AXI -of_objects [ipx::find_open_core user.org:user:subordinate:1.0]]]
generate_peripheral -driver -bfm_example_design -debug_hw_example_design [ipx::find_open_core user.org:user:subordinate:1.0]
write_peripheral [ipx::find_open_core user.org:user:subordinate:1.0]
set_property  ip_repo_paths  {./ip_repo/subordinate_1.0} [current_project]
update_ip_catalog -rebuild

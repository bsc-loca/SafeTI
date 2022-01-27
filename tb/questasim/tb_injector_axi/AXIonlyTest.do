onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /tb_injector_axi/clk
add wave -noupdate /tb_injector_axi/rstn
add wave -noupdate -radix unsigned /tb_injector_axi/core/bm0_in.rd_size
add wave -noupdate -radix binary /tb_injector_axi/axi4mo.ar_size
add wave -noupdate /tb_injector_axi/axi4mo.ar_len
add wave -noupdate /tb_injector_axi/AXI4_M0/rd.axi_strobe
add wave -noupdate /tb_injector_axi/axi4mo.ar_addr
add wave -noupdate /tb_injector_axi/axi4mi.ar_ready
add wave -noupdate /tb_injector_axi/core/bm0_out.rd_done
add wave -noupdate -expand /tb_injector_axi/AXI4_M0/rd.axi_data_bus
add wave -noupdate -childformat {{/tb_injector_axi/AXI4_M0/rd.bm_size -radix unsigned} {/tb_injector_axi/AXI4_M0/rd.rem_size -radix unsigned}} -subitemconfig {/tb_injector_axi/AXI4_M0/rd.bm_size {-height 18 -radix unsigned} /tb_injector_axi/AXI4_M0/rd.rem_size {-height 18 -radix unsigned}} /tb_injector_axi/AXI4_M0/rd
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {109 ns} 0}
quietly wave cursor active 1
configure wave -namecolwidth 150
configure wave -valuecolwidth 130
configure wave -justifyvalue left
configure wave -signalnamewidth 1
configure wave -snapdistance 10
configure wave -datasetprefix 0
configure wave -rowmargin 4
configure wave -childrowmargin 2
configure wave -gridoffset 0
configure wave -gridperiod 1
configure wave -griddelta 40
configure wave -timeline 0
configure wave -timelineunits ns
update
WaveRestoreZoom {53 ns} {232 ns}

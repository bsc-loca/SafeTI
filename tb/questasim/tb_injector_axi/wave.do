onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /tb_injector_axi/clk
add wave -noupdate /tb_injector_axi/rstn
add wave -noupdate /tb_injector_axi/axi4mi
add wave -noupdate -childformat {{/tb_injector_axi/axi4mo.aw_size -radix binary} {/tb_injector_axi/axi4mo.ar_size -radix binary}} -subitemconfig {/tb_injector_axi/axi4mo.aw_size {-height 18 -radix binary} /tb_injector_axi/axi4mo.ar_size {-height 18 -radix binary}} /tb_injector_axi/axi4mo
add wave -noupdate -childformat {{/tb_injector_axi/AXI4_M0/wr.fifo_full -radix binary}} -subitemconfig {/tb_injector_axi/AXI4_M0/wr.fifo_full {-height 18 -radix binary}} /tb_injector_axi/AXI4_M0/wr
add wave -noupdate /tb_injector_axi/AXI4_M0/rd
add wave -noupdate /tb_injector_axi/AXI4_M0/bm_in_bypass_rd
add wave -noupdate /tb_injector_axi/AXI4_M0/bm_in
add wave -noupdate /tb_injector_axi/AXI4_M0/bm_out
add wave -noupdate /tb_injector_axi/core/ctrl/fifo_inst/ram
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {86 ns} 0}
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
WaveRestoreZoom {0 ns} {108938 ns}

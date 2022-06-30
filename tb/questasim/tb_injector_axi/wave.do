onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /tb_injector_axi/clk
add wave -noupdate /tb_injector_axi/rstn
add wave -noupdate -divider {SafeTI APB}
add wave -noupdate /tb_injector_axi/core/apbi.en
add wave -noupdate /tb_injector_axi/core/apbo.irq(6)
add wave -noupdate -divider {AXI BUS}
add wave -noupdate /tb_injector_axi/axi4mi
add wave -noupdate -childformat {{/tb_injector_axi/axi4mo.aw_size -radix binary} {/tb_injector_axi/axi4mo.ar_size -radix binary}} -subitemconfig {/tb_injector_axi/axi4mo.aw_size {-height 18 -radix binary} /tb_injector_axi/axi4mo.ar_size {-height 18 -radix binary}} /tb_injector_axi/axi4mo
add wave -noupdate -divider {AXI Manager interface}
add wave -noupdate /tb_injector_axi/AXI4_M0/rd_main
add wave -noupdate /tb_injector_axi/AXI4_M0/rd_burst
add wave -noupdate /tb_injector_axi/AXI4_M0/wr_main
add wave -noupdate /tb_injector_axi/AXI4_M0/wr_burst
add wave -noupdate -divider {SafeTI injector}
add wave -noupdate /tb_injector_axi/bm_in_injector
add wave -noupdate -childformat {{/tb_injector_axi/bm_out_injector.rd_size -radix unsigned}} -subitemconfig {/tb_injector_axi/bm_out_injector.rd_size {-height 18 -radix unsigned}} /tb_injector_axi/bm_out_injector
add wave -noupdate /tb_injector_axi/core/ctrl/r
add wave -noupdate /tb_injector_axi/core/apb/r_desc_mem
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {1858 ns} 0}
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
WaveRestoreZoom {0 ns} {5250 ns}

onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /tb_injector_axi/clk
add wave -noupdate /tb_injector_axi/rstn
add wave -noupdate -divider {SafeTI configure}
add wave -noupdate /tb_injector_axi/core/apbi.en
add wave -noupdate -divider {AXI BUS}
add wave -noupdate /tb_injector_axi/axi4mi
add wave -noupdate -childformat {{/tb_injector_axi/axi4mo.aw_size -radix binary} {/tb_injector_axi/axi4mo.ar_size -radix binary}} -subitemconfig {/tb_injector_axi/axi4mo.aw_size {-height 18 -radix binary} /tb_injector_axi/axi4mo.ar_size {-height 18 -radix binary}} /tb_injector_axi/axi4mo
add wave -noupdate -divider {AXI Manager interface}
add wave -noupdate -childformat {{/tb_injector_axi/AXI4_M0/wr.fifo_full -radix binary}} -subitemconfig {/tb_injector_axi/AXI4_M0/wr.fifo_full {-height 18 -radix binary}} /tb_injector_axi/AXI4_M0/wr
add wave -noupdate -childformat {{/tb_injector_axi/AXI4_M0/rd.fifo -radix hexadecimal}} -subitemconfig {/tb_injector_axi/AXI4_M0/rd.fifo {-height 18 -radix hexadecimal}} /tb_injector_axi/AXI4_M0/rd
add wave -noupdate /tb_injector_axi/bm_in_manager
add wave -noupdate /tb_injector_axi/bm_out_manager
add wave -noupdate -divider {SafeTI injector}
add wave -noupdate /tb_injector_axi/bm_in_injector
add wave -noupdate -childformat {{/tb_injector_axi/bm_out_injector.rd_size -radix unsigned}} -subitemconfig {/tb_injector_axi/bm_out_injector.rd_size {-height 18 -radix unsigned}} /tb_injector_axi/bm_out_injector
add wave -noupdate /tb_injector_axi/core/apb/desc_bank
add wave -noupdate /tb_injector_axi/core/ctrl/r
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {30 ns} 0}
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
WaveRestoreZoom {0 ns} {98 ns}

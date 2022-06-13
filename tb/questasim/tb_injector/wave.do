onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /tb_injector/rstn
add wave -noupdate /tb_injector/clk
add wave -noupdate /tb_injector/apbi
add wave -noupdate /tb_injector/apbo
add wave -noupdate -childformat {{/tb_injector/bm_mosi.rd_size -radix unsigned} {/tb_injector/bm_mosi.wr_size -radix unsigned}} -subitemconfig {/tb_injector/bm_mosi.rd_size {-height 18 -radix unsigned} /tb_injector/bm_mosi.wr_size {-height 18 -radix unsigned}} /tb_injector/bm_mosi
add wave -noupdate /tb_injector/bm_miso
add wave -noupdate /tb_injector/core/ctrl/r
add wave -noupdate /tb_injector/core/apb/desc_bank
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {49378 ns} 0}
quietly wave cursor active 1
configure wave -namecolwidth 150
configure wave -valuecolwidth 100
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
WaveRestoreZoom {49174 ns} {49582 ns}

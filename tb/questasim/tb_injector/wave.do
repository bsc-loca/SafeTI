onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /tb_injector/rstn
add wave -noupdate /tb_injector/clk
add wave -noupdate /tb_injector/apbi
add wave -noupdate /tb_injector/apbo
add wave -noupdate -childformat {{/tb_injector/bm_mosi.rd_size -radix unsigned} {/tb_injector/bm_mosi.wr_size -radix unsigned}} -subitemconfig {/tb_injector/bm_mosi.rd_size {-height 18 -radix unsigned} /tb_injector/bm_mosi.wr_size {-height 18 -radix unsigned}} /tb_injector/bm_mosi
add wave -noupdate /tb_injector/bm_miso
add wave -noupdate /tb_injector/core/apb/desc_mem_out
add wave -noupdate -childformat {{/tb_injector/core/ctrl/r.rep_count -radix unsigned}} -subitemconfig {/tb_injector/core/ctrl/r.rep_count {-height 18 -radix unsigned}} /tb_injector/core/ctrl/r
add wave -noupdate /tb_injector/core/ctrl/read_if_sts_in
add wave -noupdate /tb_injector/core/ctrl/write_if_sts_in
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {2662 ns} 0}
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
WaveRestoreZoom {0 ns} {66135 ns}

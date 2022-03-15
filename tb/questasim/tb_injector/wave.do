onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /tb_injector/rstn
add wave -noupdate /tb_injector/clk
add wave -noupdate /tb_injector/apbi
add wave -noupdate /tb_injector/apbo
add wave -noupdate -childformat {{/tb_injector/bm_mosi.rd_size -radix unsigned} {/tb_injector/bm_mosi.wr_size -radix unsigned}} -expand -subitemconfig {/tb_injector/bm_mosi.rd_size {-height 18 -radix unsigned} /tb_injector/bm_mosi.wr_size {-height 18 -radix unsigned}} /tb_injector/bm_mosi
add wave -noupdate -expand /tb_injector/bm_miso
add wave -noupdate /tb_injector/core/apb/rin
add wave -noupdate -childformat {{/tb_injector/core/ctrl/status.count -radix unsigned}} -expand -subitemconfig {/tb_injector/core/ctrl/status.count {-height 18 -radix unsigned}} /tb_injector/core/ctrl/status
add wave -noupdate -expand /tb_injector/core/ctrl/fifo_inst/ram
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {466 ns} 0}
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
WaveRestoreZoom {187 ns} {517 ns}

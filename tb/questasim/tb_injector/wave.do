onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /tb_injector/rstn
add wave -noupdate /tb_injector/clk
add wave -noupdate /tb_injector/apbi
add wave -noupdate /tb_injector/apbo
add wave -noupdate /tb_injector/core/apb/pindex
add wave -noupdate /tb_injector/core/apb/rin
add wave -noupdate /tb_injector/bm_in
add wave -noupdate /tb_injector/bm_out
add wave -noupdate /tb_injector/core/ctrl/status
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {14 ns} 0}
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
WaveRestoreZoom {0 ns} {105 ns}

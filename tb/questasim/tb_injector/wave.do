onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate -divider APB
add wave -noupdate /tb_injector/rstn
add wave -noupdate /tb_injector/clk
add wave -noupdate /tb_injector/apbi
add wave -noupdate /tb_injector/apbo
add wave -noupdate /tb_injector/core/apb/apb_regs
add wave -noupdate -divider FETCH
add wave -noupdate /tb_injector/clk
add wave -noupdate /tb_injector/core/apb/apb_regs.gen_config.enable
add wave -noupdate -group Fetch /tb_injector/core/fetch/enable
add wave -noupdate -group Fetch /tb_injector/core/fetch/pc_wr
add wave -noupdate -group Fetch /tb_injector/core/fetch/desc_word_wr
add wave -noupdate -group Fetch /tb_injector/core/fetch/desc_word_wen
add wave -noupdate -group Fetch /tb_injector/core/fetch/mem
add wave -noupdate -group Fetch /tb_injector/core/fetch/pc_rd
add wave -noupdate -group Fetch /tb_injector/core/fetch/desc_w_counter
add wave -noupdate -group Fetch /tb_injector/core/fetch/desc_w_stop
add wave -noupdate -group Fetch /tb_injector/core/fetch/desc_index
add wave -noupdate -group Fetch /tb_injector/core/fetch/desc_word_rd
add wave -noupdate -group Fetch /tb_injector/core/fetch/pc
add wave -noupdate -group Fetch /tb_injector/core/fetch/desc_buffer
add wave -noupdate -group Fetch /tb_injector/core/fetch/fetch_ready
add wave -noupdate -group Fetch /tb_injector/core/fetch/decode_read
add wave -noupdate -group Fetch /tb_injector/core/fetch/irq
add wave -noupdate -group Fetch /tb_injector/core/fetch/state
add wave -noupdate -divider DECODE
add wave -noupdate /tb_injector/clk
add wave -noupdate /tb_injector/core/apb/apb_regs.gen_config.enable
add wave -noupdate -group Decode /tb_injector/core/decode/enable
add wave -noupdate -group Decode /tb_injector/core/decode/fetch_ready
add wave -noupdate -group Decode /tb_injector/core/decode/decode_read
add wave -noupdate -group Decode /tb_injector/core/decode/desc
add wave -noupdate -group Decode /tb_injector/core/decode/act_subm
add wave -noupdate -group Decode /tb_injector/core/decode/no_rep
add wave -noupdate -group Decode -expand /tb_injector/core/decode/common
add wave -noupdate -group Decode -expand /tb_injector/core/decode/rd_wr
add wave -noupdate -group Decode /tb_injector/core/decode/delay
add wave -noupdate -group Decode /tb_injector/core/decode/decode_ready
add wave -noupdate -group Decode /tb_injector/core/decode/exe_read
add wave -noupdate -group Decode /tb_injector/core/decode/irq
add wave -noupdate -group Decode /tb_injector/core/decode/state
add wave -noupdate -divider EXE
add wave -noupdate /tb_injector/clk
add wave -noupdate /tb_injector/core/apb/apb_regs.gen_config.enable
add wave -noupdate -group EXE /tb_injector/core/exe/enable
add wave -noupdate -group EXE /tb_injector/core/exe/decode_ready
add wave -noupdate -group EXE /tb_injector/core/exe/exe_ready
add wave -noupdate -group EXE /tb_injector/core/exe/pc_ongoing
add wave -noupdate -group EXE /tb_injector/core/exe/active_subm
add wave -noupdate -group EXE /tb_injector/core/exe/start
add wave -noupdate -group EXE /tb_injector/core/exe/busy
add wave -noupdate -group EXE /tb_injector/core/exe/error_subm
add wave -noupdate -group EXE /tb_injector/core/exe/state_subm
add wave -noupdate -divider DELAY
add wave -noupdate /tb_injector/clk
add wave -noupdate /tb_injector/core/apb/apb_regs.gen_config.enable
add wave -noupdate -divider READ
add wave -noupdate /tb_injector/clk
add wave -noupdate /tb_injector/core/apb/apb_regs.gen_config.enable
add wave -noupdate -group IB_RD /tb_injector/core/ib_out.rd_addr
add wave -noupdate -group IB_RD /tb_injector/core/ib_out.rd_size
add wave -noupdate -group IB_RD /tb_injector/core/ib_out.rd_fix_addr
add wave -noupdate -group IB_RD /tb_injector/core/ib_out.rd_req
add wave -noupdate -group IB_RD /tb_injector/core/ib_in.rd_data
add wave -noupdate -group IB_RD /tb_injector/core/ib_in.rd_req_grant
add wave -noupdate -group IB_RD /tb_injector/core/ib_in.rd_valid
add wave -noupdate -group IB_RD /tb_injector/core/ib_in.rd_done
add wave -noupdate -group IB_RD /tb_injector/core/ib_in.rd_err
add wave -noupdate -group SUB_READ /tb_injector/core/exe/sub_rd/enable
add wave -noupdate -group SUB_READ /tb_injector/core/exe/sub_rd/start
add wave -noupdate -group SUB_READ /tb_injector/core/exe/sub_rd/busy
add wave -noupdate -group SUB_READ /tb_injector/core/exe/sub_rd/transfer_on
add wave -noupdate -group SUB_READ /tb_injector/core/exe/sub_rd/desc_to_exe
add wave -noupdate -group SUB_READ /tb_injector/core/exe/sub_rd/req_reg
add wave -noupdate -group SUB_READ /tb_injector/core/exe/sub_rd/err_start
add wave -noupdate -group SUB_READ /tb_injector/core/exe/sub_rd/err_valid
add wave -noupdate -group SUB_READ /tb_injector/core/exe/sub_rd/err_done
add wave -noupdate -group SUB_READ /tb_injector/core/exe/sub_rd/status
add wave -noupdate -divider WRITE
add wave -noupdate /tb_injector/clk
add wave -noupdate /tb_injector/core/apb/apb_regs.gen_config.enable
add wave -noupdate -group IB_WR /tb_injector/core/ib_out.wr_addr
add wave -noupdate -group IB_WR /tb_injector/core/ib_out.wr_size
add wave -noupdate -group IB_WR /tb_injector/core/ib_out.wr_fix_addr
add wave -noupdate -group IB_WR /tb_injector/core/ib_out.wr_req
add wave -noupdate -group IB_WR /tb_injector/core/ib_out.wr_data
add wave -noupdate -group IB_WR /tb_injector/core/ib_in.wr_req_grant
add wave -noupdate -group IB_WR /tb_injector/core/ib_in.wr_full
add wave -noupdate -group IB_WR /tb_injector/core/ib_in.wr_done
add wave -noupdate -group IB_WR /tb_injector/core/ib_in.wr_err
add wave -noupdate -group SUB_WRITE /tb_injector/core/exe/sub_wr/enable
add wave -noupdate -group SUB_WRITE /tb_injector/core/exe/sub_wr/start
add wave -noupdate -group SUB_WRITE /tb_injector/core/exe/sub_wr/busy
add wave -noupdate -group SUB_WRITE /tb_injector/core/exe/sub_wr/transfer_on
add wave -noupdate -group SUB_WRITE /tb_injector/core/exe/sub_wr/desc_to_exe
add wave -noupdate -group SUB_WRITE /tb_injector/core/exe/sub_wr/done_wait
add wave -noupdate -group SUB_WRITE /tb_injector/core/exe/sub_wr/req_reg
add wave -noupdate -group SUB_WRITE /tb_injector/core/exe/sub_wr/err_start
add wave -noupdate -group SUB_WRITE /tb_injector/core/exe/sub_wr/err_full
add wave -noupdate -group SUB_WRITE /tb_injector/core/exe/sub_wr/err_done
add wave -noupdate -group SUB_WRITE /tb_injector/core/exe/sub_wr/status
add wave -noupdate -divider CONTROL
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {78 ns} 0}
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
WaveRestoreZoom {0 ns} {96 ns}

add wave -noupdate -childformat {{/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahbmi.hgrant -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahbmi.hrdata -radix hexadecimal}} -expand -subitemconfig {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahbmi.hgrant {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahbmi.hrdata {-height 18 -radix hexadecimal}} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahbmi
add wave -noupdate -childformat {{/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahbmo.haddr -radix hexadecimal}} -expand -subitemconfig {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahbmo.haddr {-height 18 -radix hexadecimal}} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahbmo
add wave -noupdate -childformat {{/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/bm_in.rd_addr -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/bm_in.rd_size -radix unsigned} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/bm_in.wr_addr -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/bm_in.wr_size -radix unsigned} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/bm_in.wr_data -radix hexadecimal}} -expand -subitemconfig {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/bm_in.rd_addr {-radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/bm_in.rd_size {-height 18 -radix unsigned} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/bm_in.wr_addr {-radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/bm_in.wr_size {-height 18 -radix unsigned} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/bm_in.wr_data {-height 18 -radix hexadecimal}} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/bm_in
add wave -noupdate -expand /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/bm_out
add wave -noupdate -childformat {{/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_ctrl -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt -radix hexadecimal -childformat {{/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(31) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(30) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(29) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(28) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(27) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(26) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(25) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(24) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(23) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(22) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(21) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(20) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(19) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(18) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(17) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(16) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(15) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(14) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(13) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(12) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(11) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(10) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(9) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(8) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(7) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(6) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(5) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(4) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(3) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(2) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(1) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(0) -radix hexadecimal}}} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_dst_addr -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_src_addr -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_sts -radix hexadecimal}} -expand -subitemconfig {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_ctrl {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt {-height 18 -radix hexadecimal -childformat {{/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(31) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(30) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(29) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(28) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(27) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(26) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(25) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(24) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(23) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(22) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(21) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(20) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(19) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(18) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(17) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(16) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(15) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(14) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(13) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(12) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(11) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(10) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(9) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(8) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(7) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(6) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(5) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(4) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(3) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(2) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(1) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(0) -radix hexadecimal}}} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(31) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(30) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(29) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(28) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(27) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(26) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(25) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(24) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(23) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(22) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(21) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(20) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(19) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(18) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(17) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(16) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(15) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(14) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(13) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(12) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(11) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(10) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(9) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(8) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(7) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(6) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(5) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(4) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(3) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(2) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(1) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_nxt(0) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_dst_addr {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_src_addr {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc.dbg_sts {-height 18 -radix hexadecimal}} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/curr_desc
add wave -noupdate -radix hexadecimal -childformat {{/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/ctrl/fifo_inst/ram(7) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/ctrl/fifo_inst/ram(6) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/ctrl/fifo_inst/ram(5) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/ctrl/fifo_inst/ram(4) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/ctrl/fifo_inst/ram(3) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/ctrl/fifo_inst/ram(2) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/ctrl/fifo_inst/ram(1) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/ctrl/fifo_inst/ram(0) -radix hexadecimal}} -subitemconfig {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/ctrl/fifo_inst/ram(7) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/ctrl/fifo_inst/ram(6) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/ctrl/fifo_inst/ram(5) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/ctrl/fifo_inst/ram(4) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/ctrl/fifo_inst/ram(3) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/ctrl/fifo_inst/ram(2) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/ctrl/fifo_inst/ram(1) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/ctrl/fifo_inst/ram(0) {-height 18 -radix hexadecimal}} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/ctrl/fifo_inst/ram
add wave -noupdate -expand /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/ctrl/status
add wave -noupdate /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/ctrl/r
add wave -noupdate -radix hexadecimal -childformat {{/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(31) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(30) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(29) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(28) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(27) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(26) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(25) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(24) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(23) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(22) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(21) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(20) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(19) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(18) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(17) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(16) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(15) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(14) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(13) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(12) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(11) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(10) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(9) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(8) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(7) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(6) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(5) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(4) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(3) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(2) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(1) -radix hexadecimal} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(0) -radix hexadecimal}} -subitemconfig {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(31) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(30) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(29) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(28) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(27) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(26) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(25) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(24) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(23) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(22) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(21) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(20) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(19) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(18) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(17) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(16) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(15) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(14) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(13) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(12) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(11) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(10) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(9) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(8) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(7) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(6) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(5) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(4) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(3) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(2) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(1) {-height 18 -radix hexadecimal} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr(0) {-height 18 -radix hexadecimal}} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/apb/curr_desc_ptr
add wave -noupdate -childformat {{/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/read_if/rin.tot_size -radix unsigned} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/read_if/rin.curr_size -radix unsigned}} -subitemconfig {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/read_if/rin.tot_size {-height 18 -radix unsigned} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/read_if/rin.curr_size {-height 18 -radix unsigned}} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/read_if/rin
add wave -noupdate -childformat {{/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/write_if/rin.tot_size -radix unsigned} {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/write_if/rin.curr_size -radix unsigned}} -subitemconfig {/testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/write_if/rin.tot_size {-height 18 -radix unsigned} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/write_if/rin.curr_size {-height 18 -radix unsigned}} /testbench/cpu/cpu/core0/gpp0/INJ_inst/ahb/core/write_if/rin

#$1
TOP=../../..
      
vlib bsc
vmap work $PWD/bsc
vlog +acc=rn +incdir+$TOP/hdl/injector_pkg $TOP/tb/tb_injector_pkg

vlib tb_injector 
vmap work $PWD/tb_injector
vlog +acc=rn +incdir+$TOP/hdl/ $TOP/hdl/injector $TOP/hdl/injector_apb $TOP/hdl/injector_ctrl $TOP/hdl/injector_delay_if $TOP/hdl/injector_read_if $TOP/hdl/injector_write_if tb_injector.sv ./colors.vh
vmake tb_injector/ > Makefile

if [ -z "$1" ]
then
      vsim work.tb_injector # -do "view wave -new" -do "do wave.do" -do "run -all"
else
      vsim work.tb_injector $1 # -do "do save_wave.do"
fi

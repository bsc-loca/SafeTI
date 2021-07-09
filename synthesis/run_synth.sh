rm output/ -r
rm vivado*
vivado -mode tcl -source run_synthesis_batch.tcl

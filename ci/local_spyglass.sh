#!/bin/bash
#Script description: Generates a spyglass project 
    # and retrieves the results. 
#Format parameters
FN="$(basename -- $1)"
N="${FN%%.*}"
EX="${FN#*.}"
FILE_LIST=""
#remove tmp folder with same name if any
rm -rf /tmp/$N
#make destination folder
mkdir /tmp/$N
rm -rf $PWD/../../library/
mkdir $PWD/../../library/

## TODO: work on autogenerating the vhdl projects in a similar way than SV
echo "#!SPYGLASS_PROJECT_FILE" >> /tmp/$N/$N.prj
echo "#!VERSION 3.0" >> /tmp/$N/$N.prj
echo "##Data Import Section" >> /tmp/$N/$N.prj
# Add here your files and submodules
echo "read_file -type vhdl $PWD/../hdl/injector_ahb.vhd" >> /tmp/$N/$N.prj
echo "read_file -type vhdl $PWD/../hdl/injector.vhd" >> /tmp/$N/$N.prj
echo "read_file -type vhdl $PWD/../hdl/injector_apb.vhd" >> /tmp/$N/$N.prj
echo "read_file -type vhdl $PWD/../hdl/injector_ctrl.vhd" >> /tmp/$N/$N.prj
echo "read_file -type vhdl $PWD/../hdl/fifo.vhd" >> /tmp/$N/$N.prj
echo "read_file -type vhdl $PWD/../hdl/injector_delay_if.vhd" >> /tmp/$N/$N.prj
echo "read_file -type vhdl $PWD/../hdl/injector_read_if.vhd" >> /tmp/$N/$N.prj
echo "read_file -type vhdl $PWD/../hdl/injector_write_if.vhd" >> /tmp/$N/$N.prj
# Set library name and path
echo "set_option lib bsc {$PWD/bsc}" >> /tmp/$N/$N.prj
mkdir /tmp/$N/bsc
echo "set_option libhdlfiles bsc  {/tmp/$N/bsc}" >> /tmp/$N/$N.prj 
# BSC library files
cp ../hdl/injector_pkg.vhd  /tmp/$N/bsc
echo "set_option libhdlfiles bsc {/tmp/$N/bsc/injector_pkg.vhd}" >> /tmp/$N/$N.prj
echo "##Common Options Section" >> /tmp/$N/$N.prj
echo "set_option projectwdir ." >> /tmp/$N/$N.prj
echo "set_option designread_enable_synthesis no" >> /tmp/$N/$N.prj
echo "set_option language_mode VHDL #mixed" >> /tmp/$N/$N.prj
echo "set_option designread_disable_flatten no" >> /tmp/$N/$N.prj
echo "set_option active_methodology $SPYGLASS_HOME/GuideWare/latest/block/rtl_handoff" >> /tmp/$N/$N.prj
echo "set_option incdir { ./} " >> /tmp/$N/$N.prj
echo "set_option handlememory" >> /tmp/$N/$N.prj
echo "##Goal Setup Section" >> /tmp/$N/$N.prj

cd /tmp/$N;
export SKIP_PLATFORM_CHECK=TRUE
echo -e "run_goal lint/lint_rtl\nexit -save\n"| spyglass_main -shell -project $N.prj;
cd -
cp -r /tmp/$N/$N ./

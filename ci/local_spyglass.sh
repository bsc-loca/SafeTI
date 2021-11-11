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
echo "#################Actual project file " >> /tmp/$N/$N.prj
echo "#!SPYGLASS_PROJECT_FILE" >> /tmp/$N/$N.prj
echo "#!VERSION 3.0" >> /tmp/$N/$N.prj
echo "##Data Import Section" >> /tmp/$N/$N.prj
#TODO: add here your files and submodules
#echo "read_file -type vhdl $PWD/../../bsc_lightlock/hdl/mytop.vhd" >> /tmp/$N/$N.prj
#echo "read_file -type vhdl $PWD/../../bsc_lightlock/hdl/myotherfiles.vhd" >> /tmp/$N/$N.prj
#TODO: Set library name and path
#echo "set_option lib safety $PWD/../../library" >> /tmp/$N/$N.prj
#echo "set_option libhdlfiles safety { $PWD/../../bsc_lightlock/hdl/lightlock_pkg.vhd $PWD/../../bsc_lightlock/hdl/apb_lightlock.vhd $PWD/../../bsc_lightlock/hdl/staggering/staggering_handler.vhd}" >> /tmp/$N/$N.prj
echo "##Common Options Section" >> /tmp/$N/$N.prj
echo "set_option mthresh 5000000" >> /tmp/$N/$N.prj
echo "set_option language_mode mixed" >> /tmp/$N/$N.prj
echo "set_option projectwdir ." >> /tmp/$N/$N.prj
echo "set_option top apb_lightlock" >> /tmp/$N/$N.prj
echo "set_option active_methodology $SPYGLASS_HOME/GuideWare/latest/block/rtl_handoff" >> /tmp/$N/$N.prj
echo "set_option elab_precompile yes" >> /tmp/$N/$N.prj
echo "set_option hdllibdu yes" >> /tmp/$N/$N.prj
echo "##Goal Setup Section" >> /tmp/$N/$N.prj
echo "current_methodology $SPYGLASS_HOME/GuideWare/latest/block/rtl_handoff" >> /tmp/$N/$N.prj

cd /tmp/$N;
export SKIP_PLATFORM_CHECK=TRUE
echo -e "run_goal lint/lint_rtl\nexit -save\n"| spyglass_main -shell -project $N.prj;
cd -
cp -r /tmp/$N/$N ./

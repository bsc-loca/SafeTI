#!/bin/bash
#						BSC-CNS - Guillem Cabo, Francis Fuentes - NOV2021
# Script used to execute SpyGlass at server. Requirements are...
# 1) Have a file with all the Hardware Design filenames (filepath for library files), where each
#    entry must end with a End Of Line (just press Enter). The script also allows to create and 
#    attach files to libraries by leaving an empty line followed by the library name and the files
#    to (these must be in a folder named as the library). Remember that the first entry is used
#    as top. This example uses "injector_ahb" as a top design and has two libraries (bsc, techmap):
#
#	# You can comment lines, but always make sure the first uncommented line is the top file
#	injector_ahb.hdl
#	injector.hdl
#	injector_ctrl.hdl
#	
#	bsc
#	bsc/injector_pkg.hdl
#	bsc/injector_pkg_SELENE.hdl
#
#	techmap
#	techmap/technology.hdl
#
# 2) The user must have access to the server through ssh without password. This is achieved by
#    creating a pair of keys with ssh-keygen and have the public one shared with the server client.
#
# 3) The user must have a SpyGlass_template.prj somewhere accessible at the server side at configured
#    for the user needs (check Spyglass doc folder at the server for +info). Template example:
#	
#	#!SPYGLASS_PROJECT_FILE
#	#!VERSION 3.0
#	##Data Import Section
#	#read_file -type verilog Vector_Accelerator/rtl/FIFO.sv
#	##Common Options Section
#	
#	#set_option incdir { Vector_Accelerator/rtl/include }
#	set_option projectwdir .
#	set_option designread_enable_synthesis no
#	set_option language_mode VHDL #mixed
#	set_option designread_disable_flatten no
#	#set_option enableSV yes
#	#set_option enableSV09 yes
#	#set_option top inst_multi_lane_wrapper
#	#set_option hdllibdu yes
#	#set_option 87 yes      # hdl
#	set_option active_methodology $SPYGLASS_HOME/GuideWare/latest/block/rtl_handoff
#	set_option incdir { ./} 
#	set_option handlememory
#	
#	##Goal Setup Section
#	current_methodology $SPYGLASS_HOME/GuideWare/latest/block/rtl_handoff
#	
# 4) Edit with your info the start of the script and comment the last line if you don't want to be
#    prompted with the SpyGlass results at the end of the script execution.
#
# 5) To use the script, call it from where the files are (hdl folder) and pass as argument the 
#    filepath of the file which lists all the files to be tested through SpyGlass. GGHF
#

#Edit to fit your preferences
username="ffuentes"		#Clientname to ssh to
sshserver="@epi03.bsc.es"	#Server to ssh to
template="/users/$username/spyglass/spyglass_template.prj" #SpyGlass template location on server


#Format parameters
sshclient="${username}${sshserver}"
FN="0"
i=0

#Cleanup in local machine
rm -rf /tmp/importspy
rm -rf /tmp/optionsspy

#Check user input file with path to components
if [ -z "$1" ]; then set -- "./components.txt"; fi

#copy files and set script
while read filepath
do
if [ -z "$filepath" ]; then i=1	#Check if empty line; proceed with script
elif [ ${filepath:0:1} = "#" ]; then : #Check if it's a comment; skip line if it is
else
filename="$(basename -- $filepath)"
 case $i in
  0)	#Routine to write directory and copy all the HD files w/o library
   echo "read_file {./$filename}" >> /tmp/importspy
   #echo "File added: $filename"
   if [ "$FN" = "0" ]; then #Take first file as top routine
    echo "Top is: $filename"
    FN="$filename"
    N="${FN%%.*}"
    EX="${FN#*.}"
    rm -rf ./$N
    ssh $sshclient << EOF #Cleanup in remote machine
    rm -rf /tmp/${username}Spyglass
    mkdir /tmp/${username}Spyglass
    mkdir /tmp/${username}Spyglass/$N
    exit
EOF
   fi
   scp $filepath $sshclient:/tmp/${username}Spyglass/$N
  ;;
  1)	#Routine to load library name and create directory
   libname="$filename"
   echo "Library added: $libname"
   echo "set_option lib $libname {./$libname}" >> /tmp/importspy
   ssh $sshclient << EOF
   mkdir /tmp/${username}Spyglass/$N/$libname
   exit
EOF
   i=2
  ;;
  *)	#Routine to load library files
   #echo "Library file added: $filename"
   echo "set_option libhdlfiles $libname {./$libname/$filename"} >> /tmp/importspy
   scp $filepath $sshclient:/tmp/${username}Spyglass/$N/$libname
  ;;
 esac
fi
done < $1 #Read "components.txt" file


#set the top for spyglass. must be the first argument of the script.
echo "set_option top $N" >> /tmp/optionsspy
scp /tmp/importspy $sshclient:/tmp/${username}Spyglass
scp /tmp/optionsspy $sshclient:/tmp/${username}Spyglass
ssh $sshclient << EOF
cp $template /tmp/${username}Spyglass/$N/$N.prj;
cd /tmp/${username}Spyglass/$N;
sed -i '/Data Import Section/ r /tmp/${username}Spyglass/importspy' ./$N.prj;
sed -i '/Common Options Section/ r /tmp/${username}Spyglass/optionsspy' ./$N.prj;
export SKIP_PLATFORM_CHECK=TRUE
. /eda/env.sh
#echo -e "exports\n";
echo -e "run_goal lint/lint_rtl\nexit -save\n"| spyglass_main -shell -project $N.prj;
#echo -e "remove\n";
exit
EOF
echo -e "exit"
scp -r $sshclient:/tmp/${username}Spyglass/$N/$N ./
echo -e "copy resuts"
#Comment the line below to debug the transffer and SpyGlass execution
#vim -p ./$N/consolidated_reports/${N}_lint_lint_rtl/*.rpt

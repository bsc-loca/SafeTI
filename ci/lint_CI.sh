RED='\033[7;31m'
GREEN='\033[7;32m'
BLUE='\033[7;36m'
NC='\033[0m' # No Color
TOP='injector_axi'
############
## TOP $TOP.sv
############
# Run Spyglass
printf "Please wait, running Spyglass\n"
./local_spyglass.sh \
../hdl/$TOP.vhd
#../hdl/$TOP.vhd  1> /dev/null
# TODO: add remaining files here
#Capture is there is a problem with the script
if [ $? -ne 0 ]; then
exit 3
fi
# check if there is a result file
echo "$TOP/consolidated_reports/${TOP}_lint_lint_rtl/moresimple.rpt"
test -f $TOP/consolidated_reports/${TOP}_lint_lint_rtl/moresimple.rpt || exit 2
# Check outcome
printf "UNIT - : ${BLUE} $TOP ${BLUE}${NC}\n"
cat $TOP/consolidated_reports/${TOP}_lint_lint_rtl/moresimple.rpt  | grep -i 'error\|Syntax' | GREP_COLORS='mt=01;31'  egrep -i --color=always error\|syntax
if [ $? -ne 0 ]; then
printf "SPYGLASS - Chech for errors: ${GREEN}PASS${GREEN}${NC}\n"
cat $TOP/consolidated_reports/${TOP}lint_lint_rtl/moresimple.rpt  | GREP_COLORS='mt=01;33'  egrep -i --color=always 'warning'
else
printf "SPYGLASS - Chech for errors: ${RED}FAIL${RED}${NC}\n"
exit 1
fi

exit 0

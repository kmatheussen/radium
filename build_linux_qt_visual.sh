#!/bin/bash

set -e 

export VISUAL="-DUSE_QT_VISUAL=1 -DUSE_GTK_VISUAL=0"

export GTK_CFLAGS=""
export GTK_LDFLAGS=""

GREEN='\033[0;32m'
LIGHT_CYAN='\033[0;36m'
YELLOW='\033[0;33m'
NC='\033[0m'
printf "${GREEN}Compiling${LIGHT_CYAN} Radium${NC}...\n"

start=`date +%s`

./build_linux_common.sh $@

end=`date +%s`

runtime=$((end-start))

printf "          ${LIGHT_CYAN}Radium ${GREEN} compiled.${YELLOW} (${runtime}s)${NC}\n"


#echo "Building finished."
#echo
#echo "Warning!"
#echo "The Qt version performs significantly worse than the GTK version."
#echo
#echo "This build script should only be used to test the Qt version."
#echo "Packagers should use the build_linux.sh script instead".
#echo
# (doesn't seem to be true anymore after switching from QPixmap to QImage as paint buffer)

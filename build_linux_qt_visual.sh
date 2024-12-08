#!/usr/bin/env bash

#set -x
set -eEu

source $(dirname "${0}")/bash_setup.sh


export VISUAL="-DUSE_QT_VISUAL=1 -DUSE_GTK_VISUAL=0"

export GTK_CFLAGS=""
export GTK_LDFLAGS=""

printf "${GREEN}Compiling${LIGHT_CYAN} Radium${NC}...\n"

start=`date +%s`

#trap 'handle_failure' ERR

#source owijoiwef.h
echo "AAA"
source build_linux_common.sh $@
echo "BBB"
ret=$?

end=`date +%s`

runtime=$((end-start))

printf "          ${LIGHT_CYAN}Radium ${GREEN} compiled.${YELLOW} (${runtime}s)${NC}\n"

if [ -x "$(command -v kdialog)" ]; then
    kdialog --title "Radium compiled" --passivepopup "Finished compiling radium" 2 2>/dev/null &
fi

exit $ret

#echo "Building finished."
#echo
#echo "Warning!"
#echo "The Qt version performs significantly worse than the GTK version."
#echo
#echo "This build script should only be used to test the Qt version."
#echo "Packagers should use the build_linux.sh script instead".
#echo
# (doesn't seem to be true anymore after switching from QPixmap to QImage as paint buffer)

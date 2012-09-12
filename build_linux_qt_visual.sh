#!/bin/sh

set -e 

export VISUAL="-DUSE_QT_VISUAL=1 -DUSE_GTK_VISUAL=0"

export GTK_CFLAGS=""
export GTK_LDFLAGS=""

./build_linux_common.sh $@

#echo "Building finished."
#echo
#echo "Warning!"
#echo "The Qt version performs significantly worse than the GTK version."
#echo
#echo "This build script should only be used to test the Qt version."
#echo "Packagers should use the build_linux.sh script instead".
#echo
# (doesn't seem to be true anymore after switching from QPixmap to QImage as paint buffer)

#!/bin/sh

set -e 

export IS_LINUX_BINARY="-DIS_LINUX_BINARY"

rm -f Qt_Main.o

export OPTIMIZE="-mtune=generic -msse -O3"
export BUILDTYPE=RELEASE

./build_linux.sh $@


cp README radium64_dist/
cp bin/radium radium64_dist/bin/
cp bin/*.py radium64_dist/bin/
cp bin/*.pyc radium64_dist/bin/
cp bin/config radium64_dist/bin/
cp bin/colors radium64_dist/bin/
cp bin/menues.conf radium64_dist/bin/
cp bin/keybindings.conf radium64_dist/bin/
rm -f radium_1.9.1_linux64.tar.gz && tar cvzf radium_1.9.1_linux64.tar.gz radium64_dist


#echo "Building finished."
#echo
#echo "Warning!"
#echo "The Qt version performs significantly worse than the GTK version."
#echo
#echo "This build script should only be used to test the Qt version."
#echo "Packagers should use the build_linux.sh script instead".
#echo
# (doesn't seem to be true anymore after switching from QPixmap to QImage as paint buffer)

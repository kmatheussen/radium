#!/bin/sh

export PYTHONEXE=`./find_python_path.sh`

# find_moc_and_uic_path.sh has been tested on fedora 11, fedora 17, ubuntu 12, and mint 13.
export MOC="`./find_moc_and_uic_paths.sh moc`"
export UIC="`./find_moc_and_uic_paths.sh uic`"

#export BUILDTYPE=RELEASE
export BUILDTYPE=DEBUG
export OPTIMIZE="-mtune=native -O3"

export CCC=g++
export CC=gcc
export LINKER=g++

export PKG=pkg-config
export PYPATH=`$PYTHONEXE -c "import sys;print sys.prefix+'/include/python'+sys.version[:3]"`

export VISUAL="-DUSE_QT_VISUAL=1 -DUSE_GTK_VISUAL=0"

export QT_CFLAGS="`$PKG --cflags Qt3Support`"
export QT_LDFLAGS="`$PKG --libs Qt3Support`"

export RTMIDI_CFLAGS="-D__UNIX_JACK__ -D__LINUX_ALSA__  -D__RTMIDI_DEBUG__"
export RTMIDI_LDFLAGS="-lpthread -lasound -ljack"

export OS_OPTS=

PYTHONLIBPATH=`$PYTHONEXE -c "import sys;print '-L'+sys.prefix+'/lib'"`
PYTHONLIBNAME=`$PYTHONEXE -c "import sys;print '-lpython'+sys.version[:3]"`
export OS_LDFLAGS="-pthread -lrt -lX11 bin/packages/gc-7.2/.libs/libgc.a $PYTHONLIBPATH $PYTHONLIBNAME"

export OBJ_WIN=""
export OBJ_MACOSX=""

make radium $@

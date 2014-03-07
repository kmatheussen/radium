#!/bin/bash

set -e

export PYTHONEXE=`./find_python_path.sh`

# find_moc_and_uic_path.sh has been tested on fedora 11, fedora 17, ubuntu 12, and mint 13.
export MOC="`./find_moc_and_uic_paths.sh moc`"
export UIC="`./find_moc_and_uic_paths.sh uic`"

# Uncomment next line for debug build.
#export BUILDTYPE=RELEASE

if ! env |grep BUILDTYPE ; then
    echo "Must define BUILDTYPE to DEBUG or RELEASE. For instance: \"BUILDTYPE=RELEASE ./build_linux.sh -j7\""
    exit -1
fi

if ! env |grep OPTIMIZE ; then
    export OPTIMIZE="-O3"
fi

export CPUOPT="$OPTIMIZE" # Some files are so CPU intensive that we need to turn on optimizations even in debug mode, at least when running in valgrind.
#export CPUOPT=

export CCC=g++
export CC=gcc
export LINKER=g++

export PKG=pkg-config
export PYPATH=`$PYTHONEXE -c "import sys;print sys.prefix+'/include/python'+sys.version[:3]"`

export QT_CFLAGS="`$PKG --cflags Qt3Support`"
export QT_LDFLAGS="`$PKG --libs Qt3Support --libs QtOpenGL`"

#export RTMIDI_CFLAGS="-D__UNIX_JACK__ -D__LINUX_ALSA__  -D__RTMIDI_DEBUG__"
export RTMIDI_CFLAGS="-D__LINUX_ALSA__  -D__RTMIDI_DEBUG__"
export RTMIDI_LDFLAGS="-lpthread -lasound -ljack"

#export OS_OPTS="-DTEST_GC"
export OS_OPTS="-Werror=array-bounds -msse -mfpmath=sse -DFOR_LINUX -DWITH_PD"
#export OS_OPTS="-Werror=array-bounds -march=native"

export VST_OPTS="-DUSE_VESTIGE=1"


PYTHONLIBPATH=`$PYTHONEXE -c "import sys;print '-L'+sys.prefix+'/lib'"`
PYTHONLIBNAME=`$PYTHONEXE -c "import sys;print '-lpython'+sys.version[:3]"`
export OS_LDFLAGS="-llrdf -pthread -lrt -lX11 bin/packages/gc-7.2/.libs/libgc.a $PYTHONLIBPATH $PYTHONLIBNAME bin/packages/libgig/src/.libs/libgig.a bin/packages/fluidsynth-1.1.6/src/.libs/libfluidsynth.a `$PKG --libs sndfile` `$PKG --libs samplerate` `$PKG --libs glib-2.0` -Wl,-Bstatic -lbfd -Wl,-Bdynamic -lz -liberty -ldl bin/packages/libpd-master/libs/libpds.a "

#  -lz -libery -lintl

export OBJ_WIN=""
export OBJ_MACOSX=""

make radium $@

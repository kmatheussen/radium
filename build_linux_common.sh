#!/bin/sh

export PYTHONEXE=`./find_python_path.sh`

# find_moc_and_uic_path.sh has been tested on fedora 11, fedora 17, ubuntu 12, and mint 13.
export MOC="`./find_moc_and_uic_paths.sh moc`"
export UIC="`./find_moc_and_uic_paths.sh uic`"

#export BUILDTYPE=RELEASE
if ! env |grep BUILDTYPE ; then
    export BUILDTYPE=DEBUG
fi

if ! env |grep OPTIMIZE ; then
    export OPTIMIZE="-mtune=native -O3"
fi

export CPUOPT="$OPTIMIZE" # Some files are so CPU intensive that we need to turn on optimizations even in debug mode, at least when running in valgrind.

export CCC=g++
export CC=gcc
export LINKER=g++

export PKG=pkg-config
export PYPATH=`$PYTHONEXE -c "import sys;print sys.prefix+'/include/python'+sys.version[:3]"`

export QT_CFLAGS="`$PKG --cflags Qt3Support`"
export QT_LDFLAGS="`$PKG --libs Qt3Support`"

#export RTMIDI_CFLAGS="-D__UNIX_JACK__ -D__LINUX_ALSA__  -D__RTMIDI_DEBUG__"
export RTMIDI_CFLAGS="-D__LINUX_ALSA__  -D__RTMIDI_DEBUG__"
export RTMIDI_LDFLAGS="-lpthread -lasound -ljack"

#export OS_OPTS="-DTEST_GC"
export OS_OPTS="-Werror=array-bounds"

PYTHONLIBPATH=`$PYTHONEXE -c "import sys;print '-L'+sys.prefix+'/lib'"`
PYTHONLIBNAME=`$PYTHONEXE -c "import sys;print '-lpython'+sys.version[:3]"`
export OS_LDFLAGS="-llrdf -pthread -lrt -lX11 bin/packages/gc-7.2/.libs/libgc.a $PYTHONLIBPATH $PYTHONLIBNAME bin/packages/libgig/src/.libs/libgig.a bin/packages/fluidsynth-1.1.6/src/.libs/libfluidsynth.a `$(PKG) --libs sndfile` `$(PKG) --libs samplerate` `$(PKG) --libs glib-2.0`"

export OBJ_WIN=""
export OBJ_MACOSX=""

make radium $@

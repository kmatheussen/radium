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

export CCC="g++ -mfpmath=sse"
#export CC="gcc -mfpmath=sse"
#export CCC="clang++ -stdlib=libc++"
export CC="clang -Wno-gnu-designator"
export LINKER=g++

export PKG=pkg-config
export PYPATH=`$PYTHONEXE -c "import sys;print sys.prefix+'/include/python'+sys.version[:3]"`

export QT_CFLAGS="`$PKG --cflags Qt3Support` -Ibin/packages/qhttpserver-master/src"
export QT_LDFLAGS="`$PKG --libs Qt3Support --libs QtOpenGL --libs QtNetwork`"

VL_PATH="bin/packages/Visualization-Library-master"
export VL_CFLAGS="-DVL_STATIC_LINKING -Wall -I$VL_PATH/src -I$VL_PATH/src/3rdparty/Khronos -I$VL_PATH/src/examples"
export VL_LIBS="$VL_PATH/src/vlQt4/lib/libVLQt4.a  $VL_PATH/src/vlVG/lib/libVLVG.a $VL_PATH/src/vlGraphics/lib/libVLGraphics.a $VL_PATH/src/vlCore/lib/libVLCore.a $VL_PATH/src/vlGraphics/plugins/freetype/lib/libFreeType.a -lGL -lGLU "

# jack midi not working very well since the player is now run inside the jack audio thread.
#export RTMIDI_CFLAGS="-D__UNIX_JACK__ -D__LINUX_ALSA__  -D__RTMIDI_DEBUG__"
export RTMIDI_CFLAGS="-D__LINUX_ALSA__  -D__RTMIDI_DEBUG__"
export RTMIDI_LDFLAGS="-lpthread -lasound -ljack"

#export OS_OPTS="-DTEST_GC"
export OS_OPTS="-Werror=array-bounds -msse2 -DFOR_LINUX -DWITH_PD  -Wno-unused-function"
#export OS_OPTS="-Werror=array-bounds -march=native"
 

export VST_OPTS="-DUSE_VESTIGE=1"


PYTHONLIBPATH=`$PYTHONEXE -c "import sys;print '-L'+sys.prefix+'/lib'"`
PYTHONLIBNAME=`$PYTHONEXE -c "import sys;print '-lpython'+sys.version[:3]"`
export OS_LDFLAGS="-llrdf -pthread -lrt -lX11 bin/packages/gc-7.2/.libs/libgc.a $PYTHONLIBPATH $PYTHONLIBNAME bin/packages/libgig/src/.libs/libgig.a bin/packages/fluidsynth-1.1.6/src/.libs/libfluidsynth.a `$PKG --libs sndfile` `$PKG --libs samplerate` `$PKG --libs glib-2.0` -Wl,-Bstatic -lbfd -Wl,-Bdynamic -lz -liberty -lutil -ldl -ldl bin/packages/libpd-master/libs/libpds.a bin/packages/qhttpserver-master/lib/libqhttpserver.a"

#  -lz -libery -lintl

export OBJ_WIN=""
export OBJ_MACOSX=""

make radium $@

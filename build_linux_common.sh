#!/bin/bash

set -e

export PYTHONEXE=`./find_python_path.sh`

# find_moc_and_uic_path.sh has been tested on fedora 11, fedora 17, ubuntu 12, and mint 13.
export MOC="`./find_moc_and_uic_paths.sh moc`"
export UIC="`./find_moc_and_uic_paths.sh uic`"

# Uncomment next line for debug build.
#export BUILDTYPE=RELEASE

export JACKOPT="-DNO_JACK_METADATA"


if ! env |grep BUILDTYPE ; then
    echo "Must define BUILDTYPE to DEBUG or RELEASE. For instance: \"BUILDTYPE=RELEASE ./build_linux.sh -j7\""
    exit -1
fi

#if ! env |grep OPTIMIZE ; then
export OPTIMIZE="-O3 -mfpmath=sse -msse2 $RADIUM_RELEASE_CFLAGS"
#fi

export CPUOPT="$OPTIMIZE" # Some files are so CPU intensive that we need to turn on optimizations even in debug mode, at least when running in valgrind.
#export CPUOPT=

#export CCC="clang++ -mfpmath=sse -msse2"
export CCC="g++ -mfpmath=sse -msse2"
export CC="gcc -mfpmath=sse -msse2"
#export CC="clang -Wno-gnu-designator -mfpmath=sse -msse2"
export GCC="gcc -mfpmath=sse -msse2"
export LINKER=g++

export TARGET_OS=linux

export PKG=pkg-config
export PYPATH=`$PYTHONEXE -c "import sys;print sys.prefix+'/include/python'+sys.version[:3]"`

# static Qt4:
#RQTDIR=/home/kjetil/qt-everywhere-opensource-src-4.8.6
#export QT_CFLAGS="-DQT_STATIC -DQT3_SUPPORT -I$RQTDIR/include/Qt3Support -I$RQTDIR/include/QtCore -I$RQTDIR/include/QtGui -I$RQTDIR/include/QtNetwork -I$RQTDIR/include/QtSql -I$RQTDIR/include/QtOpenGL -Ibin/packages/qhttpserver-master/src"
#export QT_LDFLAGS="$RQTDIR/lib/libQt3Support.a $RQTDIR/lib/libQtSql.a $RQTDIR/lib/libQtOpenGL.a $RQTDIR/lib/libQtGui.a $RQTDIR/lib/libQtNetwork.a $RQTDIR/lib/libQtCore.a -lSM -lICE -lfreetype -lfontconfig -lXrender -lpng -lglib-2.0 -lgobject-2.0"

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

export OS_LDFLAGS="-llrdf -pthread -lrt -lX11 bin/packages/gc-7.2/.libs/libgc.a $PYTHONLIBPATH $PYTHONLIBNAME bin/packages/libgig/src/.libs/libgig.a bin/packages/fluidsynth-1.1.6/src/.libs/libfluidsynth.a `$PKG --libs dbus-1` `$PKG --libs sndfile` `$PKG --libs samplerate` `$PKG --libs glib-2.0` -Wl,-Bstatic -lbfd -Wl,-Bdynamic -lz -liberty -lutil -ldl -ldl bin/packages/libpd-master/libs/libpds.a pluginhost/Builds/Linux/build/libMyPluginHost.a -lXext"

export OS_DEPS="pluginhost/Builds/Linux/build/libMyPluginHost.a"

#bin/packages/qhttpserver-master/lib/libqhttpserver.a

#  -lz -libery -lintl

export OBJ_WIN=""
export OBJ_MACOSX=""

mkdir -p linux_objs

if ! file mmd2load.o |grep Linux ; then
    rm -f *.o
    cp -p linux_objs/*.o . 2>/dev/null | true
fi

if ! file bin/radium |grep Linux ; then
    rm -f bin/radium
    rm -f bin/crashreporter
    rm -f bin/radium_error_message
fi

make buildtype.opt
make flagopts.opt
make radium $@

cp -p *.o linux_objs/ 2>/dev/null | true



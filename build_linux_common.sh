#!/bin/bash

set -e

export PYTHONEXE=`./find_python_path.sh`

export RADIUM_BIN="bin/radium_linux.bin"

# find_moc_and_uic_path.sh has been tested on fedora 11, fedora 17, ubuntu 12, and mint 13.
#export MOC="`./find_moc_and_uic_paths.sh moc`"
#export UIC="`./find_moc_and_uic_paths.sh uic`"

# Uncomment next line for debug build.
#export BUILDTYPE=RELEASE

export JACKOPT="-DNO_JACK_METADATA"
export JACK_LDFLAGS="-ljack -ldl"

if ! env |grep RADIUM_QT_VERSION ; then
    echo "Must define RADIUM_QT_VERSION to either 4 or 5. For instance: \"BUILDTYPE=RELEASE RADIUM_QT_VERSION=5 ./build_linux.sh -j7\""
    exit -1
fi

if ! env |grep BUILDTYPE ; then
    echo "Must define BUILDTYPE to DEBUG or RELEASE. For instance: \"BUILDTYPE=RELEASE RADIUM_QT_VERSION=5 ./build_linux.sh -j7\""
    exit -1
fi


# Don't include faustdev (by default) in debug builds since it increases linker time and increases startup time when running under gdb.
#
#if [[ $BUILDTYPE == RELEASE ]]
#then
#    if ! env |grep IS_LINUX_BINARY ; then
#        export INCLUDE_FAUSTDEV="jadda"
#    fi
#fi

export INCLUDE_FAUSTDEV="jadda"

# Always compile pddev. Most of it is placed in a dynamic library, so it doesn't contribute to higher link time or startup time.
export INCLUDE_PDDEV="jadda"


#if ! env |grep OPTIMIZE ; then
export OPTIMIZE="-O2 -mfpmath=sse -msse2 $RADIUM_RELEASE_CFLAGS"

# -flto 
#fi

export CPUOPT=
#"$OPTIMIZE" # Some files are so CPU intensive that we need to turn on optimizations even in debug mode, at least when running in valgrind. (Makefile sets CPUOPT to "-Og" in debug mode and "-O3" in release mode)
#export CPUOPT=

# To compile llvm/clang/sanitizers: (Note: sometimes rtti is not needed, but it seems coincidental when it's necessary):
# export GCC_PREFIX=$(dirname `which gcc`)/../
# REQUIRES_RTTI=1 cmake -DLLVM_ENABLE_PROJECTS="clang;compiler-rt" -G "Unix Makefiles" -DCMAKE_BUILD_TYPE=Release -DCMAKE_C_COMPILER=$GCC_PREFIX/bin/gcc -DCMAKE_CXX_COMPILER=$GCC_PREFIX/bin/g++ -DGCC_INSTALL_PREFIX=$GCC_PREFIX -DCMAKE_INSTALL_PREFIX=/home/kjetil/site -DLLVM_ENABLE_RTTI=ON ../llvm 
# REQUIRES_RTTI=1 make REQUIRES_RTTI=1 -j2

if ! env |grep USE_RADIUM_CLANG ; then
    USE_RADIUM_CLANG=0
fi

if [[ $RADIUM_USE_CLANG == 1 ]] ; then
    export CLANG_PREFIX=$(dirname `which clang`)/../
    export CCC="clang++ -mfpmath=sse -msse2"
    export CC="clang -Wno-gnu-designator -mfpmath=sse -msse2 -Wenum-conversion "
    if [[ $BUILDTYPE == RELEASE ]] ; then
        export LINKER="clang++"
    else
        export LINKER="clang++ --rtlib=compiler-rt -lgcc_s"
    fi
else
    export CCC="g++ -mfpmath=sse -msse2"
    export CC="gcc -mfpmath=sse -msse2"
    export LINKER=g++
fi

export GCC="gcc -mfpmath=sse -msse2"
export CLANGCC="clang++ -mfpmath=sse -msse2"

export FPIC="-fPIC"

export TARGET_OS=linux

export PKG=pkg-config
export PYPATH=`$PYTHONEXE -c "import sys;print sys.prefix+'/include/python'+sys.version[:3]"`

# static Qt4:
#RQTDIR=/home/kjetil/qt-everywhere-opensource-src-4.8.6
#export QT_CFLAGS="-DQT_STATIC -DQT3_SUPPORT -I$RQTDIR/include/Qt3Support -I$RQTDIR/include/QtCore -I$RQTDIR/include/QtGui -I$RQTDIR/include/QtNetwork -I$RQTDIR/include/QtSql -I$RQTDIR/include/QtOpenGL -Ibin/packages/qhttpserver-master/src"
#export QT_LDFLAGS="$RQTDIR/lib/libQt3Support.a $RQTDIR/lib/libQtSql.a $RQTDIR/lib/libQtOpenGL.a $RQTDIR/lib/libQtGui.a $RQTDIR/lib/libQtNetwork.a $RQTDIR/lib/libQtCore.a -lSM -lICE -lfreetype -lfontconfig -lXrender -lpng -lglib-2.0 -lgobject-2.0"

QT_QMAKE_BIN_PATH=`./find_moc_and_uic_paths.sh qmake`
QT_INCLUDE_PATH=`$QT_QMAKE_BIN_PATH -query QT_INSTALL_HEADERS`
export QT_UI_CFLAGS="-I $QT_INCLUDE_PATH/QtUiTools" # Doing this instead of using pkg since there are bugs in the dependencies of the pkg file in some versions of Qt. (same with the lib file below)
export QT_UI_LDFLAGS="`$QT_QMAKE_BIN_PATH -query QT_INSTALL_LIBS`/libQt5UiTools.a"


VL_PATH="bin/packages/Visualization-Library-master"
#VL_QTLIB="$VL_PATH/src/vlQt$RADIUM_QT_VERSION/lib/libVLQt$RADIUM_QT_VERSION.a"
export VL_CFLAGS="-DVL_STATIC_LINKING -Wall -I$VL_PATH/src -I$VL_PATH/src/3rdparty/Khronos -I$VL_PATH/src/examples"
export VL_LIBS="$VL_PATH/src/vlVG/lib/libVLVG.a $VL_PATH/src/vlGraphics/lib/libVLGraphics.a $VL_PATH/src/vlCore/lib/libVLCore.a `$PKG --libs freetype2` -lGL -lGLU"
# $VL_QTLIB

#$VL_PATH/src/vlGraphics/plugins/freetype/lib/libFreeType.a 

export GCDIR="bin/packages/gc-7.4.16"

# jack midi not working very well since the player is now run inside the jack audio thread.
#export RTMIDI_CFLAGS="-D__UNIX_JACK__ -D__LINUX_ALSA__  -D__RTMIDI_DEBUG__"
export RTMIDI_CFLAGS="-D__LINUX_ALSA__  -D__RTMIDI_DEBUG__"
export RTMIDI_LDFLAGS="-lpthread -lasound -ljack"

#export OS_OPTS="-DTEST_GC"
export OS_OPTS="-Werror=array-bounds -msse2 -fomit-frame-pointer -DFOR_LINUX `$PKG --cflags Qt5X11Extras`"
#export OS_OPTS="-Werror=array-bounds -march=native"


#to copmile faust:
#VERBOSE=1 CMAKEOPT="-DCMAKE_BUILD_TYPE=Debug -DSELF_CONTAINED_LIBRARY=on -DCMAKE_CXX_COMPILER=`which clang++` -DCMAKE_C_COMPILER=`which clang`" make most
#VERBOSE=1 CMAKEOPT="-DCMAKE_BUILD_TYPE=Release -DSELF_CONTAINED_LIBRARY=on -DCMAKE_CXX_COMPILER=`which g++` -DCMAKE_C_COMPILER=`which gcc`" make most

if [[ -v "${INCLUDE_FAUSTDEV}" ]] ; then
    export OS_OPTS="$OS_OPTS -DWITH_FAUST_DEV"
fi

if env |grep INCLUDE_PDDEV ; then
    export OS_OPTS="$OS_OPTS -DWITH_PD"
fi

PYTHONLIBPATH=`$PYTHONEXE -c "import sys;print '-L'+sys.prefix+'/lib'"`
PYTHONLIBNAME=`$PYTHONEXE -c "import sys;print '-lpython'+sys.version[:3]"`

LLVM_OPTS=`llvm-config --cppflags`

MAYBELLVM=`llvm-config --libdir`/libLLVM-`llvm-config --version`.so
if [ -f $MAYBELLVM ]; then
    LLVMLIBS=-lLLVM-`llvm-config --version`
else
    LLVMLIBS=`llvm-config --libs`
fi
if env |grep INCLUDE_FAUSTDEV_BUT_NOT_LLVM ; then
    if [[ -v "${INCLUDE_FAUSTDEV}" ]] ; then
        LLVMLIBS=
        export OS_OPTS="$OS_OPTS -DWITHOUT_LLVM_IN_FAUST_DEV"
    else
        echo "Error. INCLUDE_FAUSTDEV_BUT_NOT_LLVM defined, but not INCLUDE_FAUSTDEV"
        exit -1
    fi
fi

exit -2

export QSCINTILLA_PATH=`pwd`/bin/packages/QScintilla_gpl-2.10.8

if [[ -v "${INCLUDE_FAUSTDEV}" ]] ; then
    FAUSTLDFLAGS="$QSCINTILLA_PATH/Qt4Qt5/libqscintilla2_qt5.a `pwd`/bin/packages/faust/build/lib/libfaust.a `$PKG --libs uuid` `llvm-config --ldflags` $LLVMLIBS -lcrypto -lncurses"
else    
    FAUSTLDFLAGS=""
fi
# _debug

if env |grep INCLUDE_PDDEV ; then
    PDLDFLAGS="bin/packages/libpd-master/libs/libpds.a"
else    
    PDLDFLAGS=""
fi

if ! env |grep RADIUM_BFD_CFLAGS ; then
    export RADIUM_BFD_CFLAGS=""
fi

if ! env |grep RADIUM_BFD_LDFLAGS ; then
if [[ $RADIUM_USE_CLANG == 1 ]] ; then
    export RADIUM_BFD_LDFLAGS="$CLANG_PREFIX/lib/libbfd.a"
else
    export RADIUM_BFD_LDFLAGS="-Wl,-Bstatic -lbfd -Wl,-Bdynamic"
fi
fi

export OS_JUCE_LDFLAGS="-lasound -pthread -lrt -lX11 -ldl -lXext "
export OS_LDFLAGS="$FAUSTLDFLAGS $PDLDFLAGS pluginhost/Builds/Linux/build/libMyPluginHost.a $OS_JUCE_LDFLAGS -llrdf $GCDIR/.libs/libgc.a  $PYTHONLIBPATH $PYTHONLIBNAME bin/packages/libgig/src/.libs/libgig.a bin/packages/fluidsynth-1.1.6/src/.libs/libfluidsynth.a `$PKG --libs dbus-1` `$PKG --libs sndfile` `$PKG --libs samplerate` `$PKG --libs Qt5X11Extras` `$PKG --libs glib-2.0` -lxcb -lxcb-keysyms $RADIUM_BFD_LDFLAGS -lz -liberty -lutil -lgmp -lmpfr -lmpc"

# 

# 

#-L${HOME}/boost_1_60_0/stage/lib -Wl,-Bstatic -lboost_thread -lboost_system -Wl,-Bdynamic"

export OS_DEPS="pluginhost/Builds/Linux/build/libMyPluginHost.a"

#bin/packages/qhttpserver-master/lib/libqhttpserver.a

#  -lz -libery -lintl

export OBJ_WIN=""
export OBJ_MACOSX=""

mkdir -p linux_objs

#if ! file mmd2load.o |grep Linux ; then
#    rm -f *.o
#    cp -p linux_objs/*.o . 2>/dev/null | true
#fi

if ! file $RADIUM_BIN |grep Linux ; then
    echo "  Non-linux build detected. Deleting binaries."
    rm -f bin/radium
    rm -f $RADIUM_BIN
    rm -f bin/radium_crashreporter
    rm -f bin/radium_error_message
fi

make buildtype.opt --stop
make flagopts.opt --stop
make api/radium_proc.h --stop
make common/keyboard_sub_ids.h --stop

if [[ $1 == "test" ]] ; then
   make test_seqautomation
else
    make radium $@ --stop
fi
   
#make pluginhost/Builds/Linux/build/libMyPluginHost.a

cp -f bin/run_radium_linux.sh bin/radium

#cp -p *.o linux_objs/ 2>/dev/null | true



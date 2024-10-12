#!/usr/bin/env bash

set -e


source configuration.sh


export RADIUM_BIN="/tmp/radium_bin/radium_linux.bin"

mkdir -p /tmp/radium_bin
mkdir -p /tmp/radium_objects



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
    echo "Must define BUILDTYPE to DEBUG, DEBUG_FAST, or RELEASE. For instance: \"BUILDTYPE=RELEASE RADIUM_QT_VERSION=5 ./build_linux.sh -j7\""
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

#export INCLUDE_FAUSTDEV="jadda"
#export INCLUDE_FAUSTDEV_BUT_NOT_LLVM="jadda"

# Always compile pddev. Most of it is placed in a dynamic library, so it doesn't contribute to higher link time or startup time.
#export INCLUDE_PDDEV="jadda"


if ! arch |grep arm ; then
    export CPUOPTS="-msse2 -mfpmath=sse"
fi

#if ! env |grep OPTIMIZE ; then
export OPTIMIZE="-O2 $CPUOPTS $RADIUM_RELEASE_CFLAGS -fomit-frame-pointer "

export OS_DEBUG_BUILD_OPTS="-fno-omit-frame-pointer"


# -flto 
#fi

export CPUOPT=
#"$OPTIMIZE" # Some files are so CPU intensive that we need to turn on optimizations even in debug mode, at least when running in valgrind. (Makefile sets CPUOPT to "-Og" in debug mode and "-O3" in release mode)
#export CPUOPT=

# To compile llvm/clang/sanitizers: (Note: sometimes rtti is not needed, but it seems coincidental when it's necessary):
# export GCC_PREFIX=$(dirname `which gcc`)/../
# REQUIRES_RTTI=1 cmake -DLLVM_ENABLE_PROJECTS="clang;compiler-rt" -G "Unix Makefiles" -DCMAKE_BUILD_TYPE=Release -DCMAKE_C_COMPILER=$GCC_PREFIX/bin/gcc -DCMAKE_CXX_COMPILER=$GCC_PREFIX/bin/g++ -DGCC_INSTALL_PREFIX=$GCC_PREFIX -DCMAKE_INSTALL_PREFIX=/home/kjetil/site -DLLVM_ENABLE_RTTI=ON ../llvm 
# REQUIRES_RTTI=1 make REQUIRES_RTTI=1 -j2

branch=$(git branch | sed -n -e 's/^\* \(.*\)/\1/p')
export T=/tmp/radium_objects_$branch/

if ! env |grep RADIUM_USE_CLANG ; then
    RADIUM_USE_CLANG=0
fi

#RADIUM_USE_CLANG=1
RADIUM_USES_MOLD_OR_LDD=0
RADIUM_USES_MOLD_PRELOAD=0

if [[ $RADIUM_USE_CLANG == 1 ]] ; then
    export CLANG_PREFIX=$(dirname `which clang`)/../
    export CCC="clang++ $CPUOPTS "
    export CC="clang -Wno-gnu-designator $CPUOPTS -Wenum-conversion "

    # ldd
    export LINKER="clang++"
    
    if [[ $BUILDTYPE != RELEASE ]] ; then
        # both mold and ldd
        RADIUM_USES_MOLD_OR_LDD=1

        # mold: (probably faster than ldd if having more than 2 cores)
        #export LINKER="$LINKER -fuse-ld=/home/kjetil/mold/mold"
        #RADIUM_USES_MOLD_PRELOAD=0 # set to 1 to speed up linking if having more than 2 cores.    

        # ldd:
        export LINKER="$LINKER -lsframe -fuse-ld=lld"

        # stuff
        export LINKER="$LINKER -lgcc_s --rtlib=compiler-rt"
    fi
    
else
    export CCC="g++ $CPUOPTS "
    export CC="gcc $CPUOPTS "
    export LINKER="g++"

    # Use the ldd linker instead. It's approx. 10x faster.
    if [[ $BUILDTYPE != RELEASE ]] ; then
        RADIUM_USES_MOLD_OR_LDD=1
        # ldd:
        #export LINKER="clang++ -fuse-ld=lld" #-B/home/kjetil/ldd_bin"
        export LINKER="$LINKER -fuse-ld=gold" #-B/home/kjetil/ldd_bin"
        #export LINKER="clang++ -lgcc_s --rtlib=compiler-rt"
    fi
fi

export GCC="gcc $CPUOPTS"
export GPLUSPLUS="g++ $CPUOPTS"
export CLANGCC="clang++ $CPUOPTS"
export FPIC="-fPIC"

export TARGET_OS=linux

export PYPATH=`$PYTHONEXE -c "import sys;print sys.prefix+'/include/python'+sys.version[:3]"`
export PYOPTS="-I $PYPATH"

# static Qt4:
#RQTDIR=/home/kjetil/qt-everywhere-opensource-src-4.8.6
#export QT_CFLAGS="-DQT_STATIC -DQT3_SUPPORT -I$RQTDIR/include/Qt3Support -I$RQTDIR/include/QtCore -I$RQTDIR/include/QtGui -I$RQTDIR/include/QtNetwork -I$RQTDIR/include/QtSql -I$RQTDIR/include/QtOpenGL -Ibin/packages/qhttpserver-master/src"
#export QT_LDFLAGS="$RQTDIR/lib/libQt3Support.a $RQTDIR/lib/libQtSql.a $RQTDIR/lib/libQtOpenGL.a $RQTDIR/lib/libQtGui.a $RQTDIR/lib/libQtNetwork.a $RQTDIR/lib/libQtCore.a -lSM -lICE -lfreetype -lfontconfig -lXrender -lpng -lglib-2.0 -lgobject-2.0"

QT_QMAKE_BIN_PATH=`./find_moc_and_uic_paths.sh qmake`
QT_INCLUDE_PATH=`$QT_QMAKE_BIN_PATH -query QT_INSTALL_HEADERS`
export QT_UI_CFLAGS="-I $QT_INCLUDE_PATH/QtUiTools" # Doing this instead of using pkg since there are bugs in the dependencies of the pkg file in some versions of Qt. (same with the lib file below)
export QT_UI_LDFLAGS="`$QT_QMAKE_BIN_PATH -query QT_INSTALL_LIBS`/libQt5UiTools.a"


#One of these:
# 1. OLD
VL_PATH="bin/packages/Visualization-Library-master"
export VL_CFLAGS="-DVL_STATIC_LINKING -Wall -I$VL_PATH/src -I$VL_PATH/src/3rdparty/Khronos -I$VL_PATH/src/examples -DNEW_VL=0"
export VL_LIBS="$VL_PATH/src/vlVG/lib/libVLVG.a $VL_PATH/src/vlGraphics/lib/libVLGraphics.a $VL_PATH/src/vlCore/lib/libVLCore.a `$PKG --libs freetype2` -lGL -lGLU -lz"

# Or 2. NEW:
#VL_PATH="bin/packages/VisualizationLibrary"
#export VL_CFLAGS="-DVL_STATIC_LINKING -Wall -I$VL_PATH/src -I$VL_PATH/src/gui -I$VL_PATH/src/external/Khronos -I$VL_PATH/src/examples -DNEW_VL=1"
#export VL_LIBS="$VL_PATH/src/vlMain/lib/libVLMain.a $VL_PATH/src/vlX/lib/libVLX.a $VL_PATH/src/vlVG/lib/libVLVG.a $VL_PATH/src/vlGraphics/lib/libVLGraphics.a $VL_PATH/src/vlCore/lib/libVLCore.a `$PKG --libs freetype2` -lGL -lGLU -lz"

#VL_QTLIB="$VL_PATH/src/vlQt$RADIUM_QT_VERSION/lib/libVLQt$RADIUM_QT_VERSION.a"

# $VL_QTLIB

#$VL_PATH/src/vlGraphics/plugins/freetype/lib/libFreeType.a 

export GCDIR="bin/packages/gc-8.2.4"

# jack midi not working very well since the player is now run inside the jack audio thread.
#export RTMIDI_CFLAGS="-D__UNIX_JACK__ -D__LINUX_ALSA__  -D__RTMIDI_DEBUG__"
export RTMIDI_CFLAGS="-D__LINUX_ALSA__  -D__RTMIDI_DEBUG__"
export RTMIDI_LDFLAGS="-lpthread -lasound -ljack"

#export OS_OPTS="-DTEST_GC"
export OS_OPTS="-Werror=array-bounds $CPUOPTS -DFOR_LINUX `$PKG --cflags Qt5X11Extras` -DRADIUM_USES_MOLD_OR_LDD=$RADIUM_USES_MOLD_OR_LDD" # -Ibin/packages/libxcb-1.13/"


#export OS_OPTS="-Werror=array-bounds -march=native"


#to copmile faust:
#VERBOSE=1 CMAKEOPT="-DCMAKE_BUILD_TYPE=Debug -DSELF_CONTAINED_LIBRARY=on -DCMAKE_CXX_COMPILER=`which clang++` -DCMAKE_C_COMPILER=`which clang`" make most
#VERBOSE=1 CMAKEOPT="-DCMAKE_BUILD_TYPE=Release -DSELF_CONTAINED_LIBRARY=on -DCMAKE_CXX_COMPILER=`which g++` -DCMAKE_C_COMPILER=`which gcc`" make most

if env |grep INCLUDE_FAUSTDEV= ; then
    export OS_OPTS="$OS_OPTS -DWITH_FAUST_DEV"
fi

if env |grep INCLUDE_PDDEV ; then
    export OS_OPTS="$OS_OPTS -DWITH_PD"
fi

PYTHONLIBPATH=`$PYTHONEXE -c "import sys;print '-L'+sys.prefix+'/lib'"`
PYTHONLIBNAME=`$PYTHONEXE -c "import sys;print '-lpython'+sys.version[:3]"`


#export QSCINTILLA_PATH=`pwd`/bin/packages/QScintilla_gpl-2.10.8
export QSCINTILLA_PATH=`pwd`/bin/packages/QScintilla_src-2.14.0/src


if env |grep INCLUDE_FAUSTDEV= ; then
    FAUSTLDFLAGS="`pwd`/bin/packages/faust/build/lib/libfaust.a -lcrypto -lncurses"
    if env |grep INCLUDE_FAUSTDEV_BUT_NOT_LLVM= ; then
        export OS_OPTS="$OS_OPTS -DWITHOUT_LLVM_IN_FAUST_DEV"
    else
        LLVM_OPTS=`$LLVM_PATH/bin/llvm-config --cppflags`
        
        MAYBELLVM=`$LLVM_PATH/bin/llvm-config --libdir`/libLLVM-`$LLVM_PATH/bin/llvm-config --version`.so
        if [ -f $MAYBELLVM ]; then
            LLVMLIBS=-lLLVM-`$LLVM_PATH/bin/llvm-config --version`
        else
            LLVMLIBS=`$LLVM_PATH/bin/llvm-config --libs`
        fi
        
        FAUSTLDFLAGS="$FAUSTLDFLAGS `$PKG --libs uuid` `$LLVM_PATH/bin/llvm-config --ldflags` $LLVMLIBS -ltinfo"
    fi
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
    if [[ $RADIUM_USE_CLANG == 1 ]] && [ -f "$CLANG_PREFIX/lib/libbfd.a" ]; then
        export RADIUM_BFD_LDFLAGS="$CLANG_PREFIX/lib/libbfd.a"
    else
        export RADIUM_BFD_LDFLAGS="-Wl,-Bstatic -lbfd -Wl,-Bdynamic"
    fi
fi

export OS_JUCE_LDFLAGS="-lasound -pthread -lrt -lX11 -ldl -lXext "

#LIBGIG_LDFLAGS="bin/packages/libgig/src/.libs/RIFF.o bin/packages/libgig/src/.libs/SF.o"
FLUIDSYNTH_LDFLAGS="bin/packages/fluidsynth-1.1.6/src/.libs/libfluidsynth.a `$PKG --libs glib-2.0`"
export OS_LDFLAGS="$QSCINTILLA_PATH/libqscintilla2_qt5.a $FAUSTLDFLAGS $PDLDFLAGS pluginhost/Builds/Linux/build/libMyPluginHost.a $OS_JUCE_LDFLAGS -llrdf $GCDIR/.libs/libgc.a $PYTHONLIBPATH $PYTHONLIBNAME `$PKG --libs sndfile` `$PKG --libs samplerate` `$PKG --libs liblo` -lxcb -lxcb-keysyms $FLUIDSYNTH_LDFLAGS $RADIUM_BFD_LDFLAGS -liberty `$PKG --libs Qt5X11Extras`"

# -licui18n -licuuc -licudata -lutil 

# -lgmp -lmpfr -lmpc

# 
#`$PKG --libs dbus-1`

#-Lbin/packages/libxcb-1.13/src/.libs

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

api/s7_types_generator.scm types
make buildtype.opt --stop
make flagopts.opt --stop
make api/radium_proc.h --stop
make common/keyboard_sub_ids.h --stop
make bin/radium_check_recent_libxcb --stop

if [[ $1 == "test" ]] ; then
    make test_seqautomation
else
    if [[ $RADIUM_USES_MOLD_PRELOAD == 1 ]] ; then
       rm -f /tmp/run_preload
       make /tmp/run_preload
    fi
    make radium $@ --stop
fi

if [[ $BUILDTYPE == RELEASE ]] ; then
    strip bin/radium_crashreporter
    strip bin/radium_error_message
    strip bin/radium_progress_window
    strip bin/radium_check_jack_status
    strip bin/radium_check_opengl
    strip bin/radium_plugin_scanner
fi
    
#make pluginhost/Builds/Linux/build/libMyPluginHost.a

cp -f bin/run_radium_linux.sh bin/radium

ln -sf $RADIUM_BIN bin/

echo "...making symlinks into /tmp/radium/_bin/"
ln -sf `pwd`/bin/* /tmp/radium_bin/ || true
echo "...finished"

echo


do_source_sanity_checks() {
    
    echo "IN approx. 1 in 20 builds we're going to some source checks. This might take around 5 seconds or thereabout...."

    if grep static\  */*.h */*.hpp */*/*.hpp */*/*/*.hpp */*/*/*/*.hpp */*/*.h */*/*/*/*.h */*/*.cpp */*.c */*.cpp */*.m */*/*.c */*/*.cpp */*/*/*.c */*/*/*/*.c */*/*/*.cpp 2>&1 | grep "\[" | grep -v "\[\]"|grep -v static\ void |grep -v unused_files |grep -v GTK |grep -v test\/ |grep -v X11\/ |grep -v amiga |grep -v faust-examples|grep -v temp\/ |grep -v "\[NO_STATIC_ARRAY_WARNING\]" |grep -v backup |grep -v mingw |grep -v Dropbox |grep -v bin/packages |grep -v python-midi |grep -v "No such file or directory" ; then
	echo
	echo "ERROR in line(s) above. Static arrays may decrease GC performance notably.";
	echo
	exit -1
    fi
    
    if [ -d .git ] ; then
	if git grep -n R_ASSERT|grep \=|grep -v \=\=|grep -v \!\=|grep -v \>\=|grep -v \<\= |grep -v build_linux_common.sh ; then
            echo
            echo "ERROR in line(s) above. An R_ASSERT line is possibly wrongly used.";
            echo
            exit -1
	fi            
	
	if git grep -n assert\(|grep -v START_JUCE_APPLICATION |grep -v assert\(\) |grep \=|grep -v \=\=|grep -v \!\=|grep -v \>\=|grep -v \<\= |grep -v build_linux_common.sh ; then
            echo
            echo "ERROR in line(s) above. An R_ASSERT line is possibly wrongly used.";
            echo
            exit -1
	fi            
	
	if git grep -e if\( --or -e if\ \( *|grep \=|grep -v \=\=|grep -v \!\=|grep -v \>\=|grep -v \<\=|grep -v pluginhost|grep -v bin/scheme|grep -v rtmidi|grep -v python|grep -v amiga|grep -v unused_files|grep -v weakjack|grep -v radium_wrap_1.c|grep -v keybindings.conf |grep -v bin/help ; then
            echo
            echo "ERROR in line(s) above. A single '=' can not be placed on the same line as an if.";
            echo
            exit -1
	fi            
    fi
}

if [[ $BUILDTYPE == RELEASE ]] ; then
    do_source_sanity_checks
elif [ $(($RANDOM % 20)) -eq 0 ]; then
    do_source_sanity_checks
fi

echo "Build finished."


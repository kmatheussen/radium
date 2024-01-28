#!/bin/bash

set -e
set -x

#export INCLUDE_FAUSTDEV_BUT_NOT_LLVM="jadda"
export RADIUM_QT_VERSION=5
#export RADIUM_USE_CLANG=1

unset CFLAGS
unset CFLAGS
unset CPPFLAGS
unset LDFLAGS
unset CXXFLAGS



export COMMON_CFLAGS="-mtune=generic -fPIC -fno-strict-aliasing -Wno-misleading-indentation "

if uname -s |grep Darwin ; then
    if [[ -z "${MACOSX_DEPLOYMENT_TARGET}" ]]; then
	echo "MACOSX_DEPLOYMENT_TARGET not set"
	exit -1
    fi
    export COMMON_CFLAGS="$COMMON_CFLAGS -mmacosx-version-min=${MACOSX_DEPLOYMENT_TARGET}"
fi
   
if ! arch |grep arm ; then
    export COMMON_CFLAGS="$COMMON_CFLAGS -msse2 -mfpmath=sse "
fi

if [[ -z "${RADIUM_USE_CLANG}" ]]; then
    export COMMON_CFLAGS="$COMMON_CFLAGS -fmax-errors=5 "
fi

export CFLAGS="$COMMON_CFLAGS "
export CPPFLAGS="$QT_CPPFLAGS $COMMON_CFLAGS  "
export CXXFLAGS="$QT_CPPFLAGS $COMMON_CFLAGS -I/home/kjetil/site_clang10/include "
export LDFLAGS="$QT_LDFLAGS"

DASCC=gcc
DASCXX=g++


if [[ $RADIUM_USE_CLANG == 1 ]]
then
    DASCC=clang
    DASCXX=clang++
fi

if ! env |grep RADIUM_QT_VERSION ; then
    echo "Must define RADIUM_QT_VERSION to either 4 or 5. For instance: \"RADIUM_QT_VERSION=5 make packages\""
    exit -1
fi



#rm -f /tmp/radium
#ln -s `pwd` /tmp/radium
PWD=`pwd`
PREFIX=`dirname $PWD/$0`
#echo $PREFIX
#exit

#http://www.python.org
#tar xvzf Python-2.2.2.tgz
#cd Python-2.2.2
#./configure --prefix=/tmp/radium --with-threads
#make
#make install
#cd ..



#tar xvf setxkbmap_56346c72127303a445a273217f7633c2afb29cfc.tar
#cd setxkbmap
#make clean
#./configure --prefix=/usr
#make
#cd ..


build_faust() {
    rm -fr faust
    tar xvzf faust-2.70.3.tar.gz
    mv faust-2.70.3 faust
    cd faust
    rm -fr libraries
    tar xvzf ../faustlibraries_2024_01_05.tar.gz
    mv faustlibraries libraries

    patch -p0 <../faust_most.cmake.patch
    patch -p1 <../faust_cmakelist.patch
    ### this patch is needed to build on artix
    #patch -p1 <../faust_make.llvm.static.patch
    if env |grep INCLUDE_FAUSTDEV_BUT_NOT_LLVM ; then
        sed -i 's/LLVM_BACKEND   \tCOMPILER STATIC/LLVM_BACKEND OFF/' build/backends/most.cmake
        if grep LLVM_BACKEND build/backends/most.cmake | grep COMPILER ; then
            echo "sed failed"
            exit -1
        fi
        if grep LLVM_BACKEND build/backends/most.cmake | grep STATIC ; then
            echo "sed failed"
            exit -1
        fi
    fi

    patch -p0 < ../faust3.patch

    sed -i '1s/^/#include \<cstdint> /' architecture/faust/dsp/dsp.h
    
    #cp compiler/generator/libfaust-signal.h architecture/faust/dsp/
    #cp compiler/generator/libfaust-box.h architecture/faust/dsp/

    # release build
    VERBOSE=1 CFLAGS="$CFLAGS" CXXFLAGS="$CXXFLAGS" CMAKEOPT="-DCMAKE_BUILD_TYPE=Release -DSELF_CONTAINED_LIBRARY=on -DCMAKE_CXX_COMPILER=`which $DASCXX` -DCMAKE_C_COMPILER=`which $DASCC` " make most

    # debug build
    #VERBOSE=1 CFLAGS="$CFLAGS" CXXFLAGS="$CXXFLAGS" CMAKEOPT="-DCMAKE_BUILD_TYPE=Debug -DSELF_CONTAINED_LIBRARY=on -DCMAKE_CXX_COMPILER=`which $DASCXX` -DCMAKE_C_COMPILER=`which $DASCC` " make most
    
    cd ..
}

build_Visualization-Library() {

    rm -fr Visualization-Library-master
    tar xvzf Visualization-Library-master.tar.gz 
    cd Visualization-Library-master/
    patch -p1 <../visualization.patch
    sed -i 's/add_subdirectory("freetype")//' src/vlGraphics/plugins/CMakeLists.txt
    #sed -i s/"VL_ACTOR_USER_DATA 0"/"VL_ACTOR_USER_DATA 1"/ src/vlCore/config.hpp
    export MYFLAGS="-std=gnu++11 $CPPFLAGS -fPIC -g  -Wno-c++11-narrowing -Wno-deprecated-declarations -Wno-implicit-function-declaration `pkg-config --cflags freetype2` " #  -D_GLIBCXX_USE_CXX11_ABI=0
    MYFLAGS="-std=gnu++11 $CPPFLAGS -fPIC -g -Wno-c++11-narrowing -Wno-deprecated-declarations -Wno-implicit-function-declaration `pkg-config --cflags freetype2` " #  -D_GLIBCXX_USE_CXX11_ABI=0
    echo 'set(CMAKE_CXX_FLAGS "$MYFLAGS")' >>CMakeLists.txt
    # previously used build type: RelWithDebInfo. Unfortunately, this one enable _DEBUG and various runtime checks.

    #CFLAGS="$CPPFLAGS -fPIC -g" CPPFLAGS="$MYFLAGS" CC="clang" CXX="clang++ $MYFLAGS" cmake -DCMAKE_CXX_FLAGS="$MYFLAGS" CMAKE_CXX_COMPILER="clang++ $MYFLAGS" -DCMAKE_BUILD_TYPE=Release -DCMAKE_POSITION_INDEPENDENT_CODE=ON SUPPORT=ON -DVL_DYNAMIC_LINKING=OFF -DVL_IO_2D_PNG=OFF -DVL_IO_2D_TIFF=OFF -DVL_IO_2D_JPG=OFF -DVL_IO_2D_TGA=OFF -DVL_IO_2D_BMP=OFF .
    
    CFLAGS="$CPPFLAGS -fPIC -g" CPPFLAGS="$MYFLAGS" CC="$DASCC" CXX="$DASCXX $MYFLAGS" cmake -DCMAKE_CXX_FLAGS="$MYFLAGS" CMAKE_CXX_COMPILER="$DASCXX $MYFLAGS" -DCMAKE_BUILD_TYPE=Release -DCMAKE_POSITION_INDEPENDENT_CODE=ON SUPPORT=ON -DVL_DYNAMIC_LINKING=OFF -DVL_IO_2D_PNG=OFF -DVL_IO_2D_TIFF=OFF -DVL_IO_2D_JPG=OFF -DVL_IO_2D_TGA=OFF -DVL_IO_2D_BMP=OFF -DVL_IO_FREETYPE=OFF .
    
    VERBOSE=1 make VLCore -j8
    VERBOSE=1 make VLVG/fast -j8
    VERBOSE=1 make VLGraphics/fast -j8
    cd ..
}

build_libpds() {

    rm -fr libpd-master
    tar xvzf libpd-master.tar.gz
    cd libpd-master/
    sed -i '/define CFLAGS/ s|")| -I/usr/include/tirpc ")|' make.scm
    sed -i 's/k_cext$//' make.scm
    sed -i 's/oscx //' make.scm
    sed -i 's/gcc -O3/gcc -fcommon -O3/' make.scm

    GLIBCVERSION=`ldd --version | head -n 1 | awk '{print $NF}'`
    if [[ $GLIBCVERSION > 2.34 ]] ; then
        sed -i 's/#define fsqrt/\/\/#define fsqrt/g' pure-data/extra/fiddle~/fiddle~.c
    fi

    make clean
    make -j`nproc`
    cd ..
}


build_qhttpserver() {

    rm -fr qhttpserver-master
    tar xvzf qhttpserver-master.tar.gz
    cd qhttpserver-master/
    echo "CONFIG += staticlib" >> src/src.pro
    `../../../find_moc_and_uic_paths.sh qmake`
    make -j8 # necessary to create the moc files.
    cd ..
}


#rm -fr sndlib
#tar xvzf sndlib.tar.gz
#cd sndlib
#patch -p0 <../sndlib.patch
#cd ..


#http://www.hpl.hp.com/personal/Hans_Boehm/gc/
build_gc() {
    GC_VERSION=8.2.4
    LIBATOMIC_VERSION=7.8.0
    rm -fr gc-$GC_VERSION libatomic_ops-$LIBATOMIC_VERSION
    tar xvzf gc-$GC_VERSION.tar.gz
    tar xvzf libatomic_ops-$LIBATOMIC_VERSION.tar.gz
    cd gc-$GC_VERSION
    ln -s ../libatomic_ops-$LIBATOMIC_VERSION libatomic_ops
    #echo 'void RADIUM_ensure_bin_packages_gc_is_used(void){ABORT("GC not configured properly");}' >>malloc.c
    echo 'void RADIUM_ensure_bin_packages_gc_is_used(void){}' >>malloc.c
    echo '#if defined(GC_ASSERTIONS) || !defined(NO_DEBUGGING)' >>malloc.c
    echo "#error "nope"" >>malloc.c
    echo "#endif" >>malloc.c
    #patch -p1 <../gcdiff.patch
    CFLAGS="-mtune=generic -g -O2" ./configure --enable-static --disable-shared --disable-gc-debug --disable-gc-assertions
    CFLAGS="-mtune=generic -g -O2" make -j8
    cd ..
}

build_fluidsynth() {

    rm -fr fluidsynth-1.1.6
    tar xvzf fluidsynth-1.1.6.tar.gz
    cd fluidsynth-1.1.6
    make clean
    CFLAGS="-fPIC -fno-strict-aliasing -O3 -DDEFAULT_SOUNDFONT=\\\"\\\"" CPPFLAGS="-fPIC -fno-strict-aliasing -O3" CXXFLAGS="-fPIC -fno-strict-aliasing -O3" ./configure --enable-static --disable-aufile-support --disable-pulse-support --disable-alsa-support --disable-libsndfile-support --disable-portaudio-support --disable-oss-support --disable-midishare --disable-jack-support --disable-coreaudio --disable-coremidi --disable-dart --disable-lash --disable-ladcca --disable-aufile-support --disable-dbus-support --without-readline
    # --enable-debug
    make -j8
    cd ..
}

build_libgig () {

    rm -fr libgig
    tar xvjf libgig-4.2.0.tar.bz2
    mv libgig-4.2.0 libgig
    cd libgig
    make clean
    CFLAGS="-O3 -fno-strict-aliasing" CPPFLAGS="-O3 -fno-strict-aliasing" CXXFLAGS="-O3 -fno-strict-aliasing" CC=$DASCC CXX=$DASCXX ./configure
    CFLAGS="$CFLAGS" CPPFLAGS="$CPPFLAGS" CPPFLAGS="$CXXFLAGS" make -j8
    cd ..
}


build_qscintilla() {
    # QScintilla
    rm -fr QScintilla_gpl-2.10.8 QScintilla_src-2.14.0
    tar xvzf QScintilla_src-2.14.0.tar.gz 
    cd QScintilla_src-2.14.0/src
    echo "CONFIG += staticlib" >> qscintilla.pro
    `../../../../find_moc_and_uic_paths.sh qmake`
    patch -p0 <../../qscintilla.patch
    make -j8
    cd ../..
}


#if [[ $RADIUM_QT_VERSION == 5 ]]
#then
#    rm -fr qtstyleplugins-src-5.0.0/
#    tar xvzf qtstyleplugins-src-5.0.0.tar.gz
#    cd qtstyleplugins-src-5.0.0/
#    `../../../find_moc_and_uic_paths.sh qmake`
#    CFLAGS="$CFLAGS" CPPFLAGS="$CPPFLAGS" CPPFLAGS="$CXXFLAGS" make -j`nproc`
#    cd ../
#fi

build_xcb() {
    
    if [[ $RADIUM_QT_VERSION == 5 && $RADIUM_BUILD_LIBXCB != 0 ]]
    then

        rm -fr xcb-proto-1.13/
        tar xvjf xcb-proto-1.13.tar.bz2
        cd xcb-proto-1.13/
        mkdir install
        ./configure --prefix=`pwd`/install PYTHON=`which python2`
        make -j8
        make install
        cd ..
        
        rm -fr libxcb-1.13
        tar xvjf libxcb-1.13.tar.bz2 
        cd libxcb-1.13
        #patch -p1 <../libxcb-1.12.patch
        export PKG_CONFIG_PATH=`pwd`/../xcb-proto-1.13/install/lib/pkgconfig:$PKG_CONFIG_PATH
        CFLAGS="$CFLAGS" CPPFLAGS="$CPPFLAGS" CPPFLAGS="$CXXFLAGS" ./configure PYTHON=`which python2`
        CFLAGS="$CFLAGS" CPPFLAGS="$CPPFLAGS" CPPFLAGS="$CXXFLAGS" make -j`nproc`
        cd ..
    
    fi
}


build_Visualization-Library

build_faust
build_qhttpserver
build_gc
build_fluidsynth
build_qscintilla # Note: Linking fails on Mac. Just ignore it.

if uname -s |grep Linux ; then
    build_libpds
    build_xcb
    echo "finished compiling libpds and xcb" # need this line to avoid script failing if the two lines above are commented out.
fi


touch deletemetorebuild

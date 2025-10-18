#!/usr/bin/env bash

set -eEu
set -x

export PYTHONEXE_NOT_AVAILABLE_YET=1

pushd ../../
source configuration.sh
popd

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
   
if ! arch |grep -e arm -e aarch64 ; then
    export COMMON_CFLAGS="$COMMON_CFLAGS -msse2 -mfpmath=sse "
fi

if [[ $RADIUM_USE_CLANG == 0 ]] ; then
    export COMMON_CFLAGS="$COMMON_CFLAGS -fmax-errors=5 "
fi

set_var QT_CPPFLAGS ""
set_var QT_LDFLAGS ""

export CFLAGS="$COMMON_CFLAGS "
export CPPFLAGS="$QT_CPPFLAGS $COMMON_CFLAGS "
export CXXFLAGS="$QT_CPPFLAGS $COMMON_CFLAGS "
export LDFLAGS="$QT_LDFLAGS"

DASCC=gcc
DASCXX=g++


if ! is_0 $RADIUM_USE_CLANG ; then
    DASCC=clang
    DASCXX=clang++
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
    tar xvzf faust-2.81.2.tar.gz
    mv faust-2.81.2 faust
    cd faust
    rm -fr libraries
    tar xvzf ../faustlibraries_2024_01_05.tar.gz
    mv faustlibraries libraries

    ### this line is needed to build on artix
    #export LIBNCURSES_PATH=$(shell find /usr -name libncursesw_g.a)

	cp ../faust_targets.cmake build/targets/most.cmake
	
	if is_0 $FAUST_USES_LLVM ; then
		cp ../faust_radium_nonllvm.cmake build/backends/most.cmake
	else
		cp ../faust_radium_llvm.cmake build/backends/most.cmake
		export ORGTEMPPATH=$PATH
		export PATH=$(dirname $LLVM_CONFIG_BIN):$PATH
		#echo "PATH: $PATH"
    fi

	# Use all CPUs when building faust.
	JOBS=$(nproc)
	#sed -i.backup "s/(BUILDLOCATION)$/(BUILDLOCATION) -j${JOBS}/" Makefile
	#exit -1
	
    # release build
    BUILDOPT="--config Release -j${JOBS}" VERBOSE=1 CFLAGS="$CFLAGS" CXXFLAGS="$CXXFLAGS" CMAKEOPT="-DCMAKE_BUILD_TYPE=Release -DSELF_CONTAINED_LIBRARY=on -DCMAKE_CXX_COMPILER=`which $DASCXX` -DCMAKE_C_COMPILER=`which $DASCC` " make most

	if ! is_0 $FAUST_USES_LLVM ; then
		export PATH=$ORGTEMPPATH
		unset ORGTEMPPATH
    fi
    
    
    # debug build
    #VERBOSE=1 CFLAGS="$CFLAGS" CXXFLAGS="$CXXFLAGS" CMAKEOPT="-DCMAKE_BUILD_TYPE=Debug -DSELF_CONTAINED_LIBRARY=on -DCMAKE_CXX_COMPILER=`which $DASCXX` -DCMAKE_C_COMPILER=`which $DASCC` " make most
    
    cd ..
}

build_Visualization-Library() {

    rm -fr Visualization-Library-master
    tar xvzf Visualization-Library-master.tar.gz 
    cd Visualization-Library-master/
    patch -p1 <../visualization.patch
    sed -i.backup 's/add_subdirectory("freetype")//' src/vlGraphics/plugins/CMakeLists.txt
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

    NOWARNS="-Wno-return-mismatch -Wno-implicit-function-declaration -Wno-int-conversion -Wno-implicit-int"
    
    rm -fr libpd-master
    tar xvzf libpd-master.tar.gz
    cd libpd-master/
    sed -i.backup "/define CFLAGS/ s|\")| -I/usr/include/tirpc $NOWARNS\")|" make.scm
    sed -i.backup 's/k_cext$//' make.scm
    sed -i.backup 's/oscx //' make.scm
    sed -i.backup "s/gcc -O3/gcc -fcommon -O3 $NOWARNS/" make.scm

    GLIBCVERSION=`ldd --version | head -n 1 | awk '{print $NF}'`
    if [[ $GLIBCVERSION > 2.34 ]] ; then
	sed -i.backup 's/#define fsqrt sqrt/#include <math.h>\nstatic float fsqrt(float val){return sqrt(val);}/g' pure-data/extra/fiddle~/fiddle~.c
	sed -i.backup 's/fsqrt/myfsqrt/g' pure-data/extra/fiddle~/fiddle~.c
    fi
    
    make clean
    CPPFLAGS="$CFLAGS $NOWARNS" make -j`nproc`
    cd ..
}


build_qhttpserver() {

    rm -fr qhttpserver-master
    tar xvzf qhttpserver-master.tar.gz
    cd qhttpserver-master/
    echo "CONFIG += staticlib" >> src/src.pro
    $QMAKE
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
    GC_VERSION=8.2.8
    LIBATOMIC_VERSION=7.8.0
    rm -fr gc-$GC_VERSION libatomic_ops-$LIBATOMIC_VERSION
    tar xvzf gc-$GC_VERSION.tar.gz
    #mv bdwgc-$GC_VERSION gc-$GC_VERSION
    tar xvzf libatomic_ops-$LIBATOMIC_VERSION.tar.gz
    cd gc-$GC_VERSION
    ln -s ../libatomic_ops-$LIBATOMIC_VERSION libatomic_ops
    #echo 'void RADIUM_ensure_bin_packages_gc_is_used(void){ABORT("GC not configured properly");}' >>malloc.c
    echo 'void RADIUM_ensure_bin_packages_gc_is_used(void){}' >>malloc.c
    echo '#if defined(GC_ASSERTIONS) || !defined(NO_DEBUGGING)' >>malloc.c
    echo "#error "nope"" >>malloc.c
    echo "#endif" >>malloc.c
    #patch -p1 <../gcdiff.patch
    #./autogen.sh
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
    $QMAKE
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
    
    if ! is_0 ${RADIUM_BUILD_LIBXCB:-1} ; then
	
        rm -fr xcb-proto-1.13/
        tar xvjf xcb-proto-1.13.tar.bz2
        cd xcb-proto-1.13/
        mkdir install
        ./configure --prefix=`pwd`/install PYTHON=$PYTHONEXE
        make -j8
        make install
        cd ..
        
        rm -fr libxcb-1.13
        tar xvjf libxcb-1.13.tar.bz2 
        cd libxcb-1.13
        #patch -p1 <../libxcb-1.12.patch
        export XCBPROTO_LIBS=`pwd`/../xcb-proto-1.13/install/lib # don't append to PKG_CONFIG_PATH because of set -u (or use default args)
        export XCBPROTO_CFLAGS="$CFLAGS"
        CFLAGS="$CFLAGS" CPPFLAGS="$CPPFLAGS" CPPFLAGS="$CXXFLAGS" ./configure PYTHON=$PYTHONEXE
        CFLAGS="$CFLAGS" CPPFLAGS="$CPPFLAGS" CPPFLAGS="$CXXFLAGS" make -j`nproc`
        cd ..
    
    fi
}

source ./build_python27.sh

build_Visualization-Library

build_faust
build_qhttpserver
build_gc
build_fluidsynth
build_python27
build_qscintilla # Note: Linking fails on Mac. Just ignore it.

if uname -s |grep Linux ; then
    if ! arch |grep -e arm -e aarch64 ; then
        build_libpds
    fi
    build_xcb
    echo "finished compiling libpds and xcb" # need this line to avoid script failing if the two lines above are commented out.
fi


touch deletemetorebuild

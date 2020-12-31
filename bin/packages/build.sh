#!/bin/bash

set -e
set -x

unset CFLAGS
unset CFLAGS
unset CPPFLAGS
unset LDFLAGS
unset CXXFLAGS

export CFLAGS="-mtune=generic -msse2 -mfpmath=sse -Wno-misleading-indentation -fPIC -fno-strict-aliasing"
export CPPFLAGS="-mtune=generic -msse2 -mfpmath=sse -fPIC -fno-strict-aliasing"
export CXXFLAGS="-mtune=generic -msse2 -mfpmath=sse -fPIC -fno-strict-aliasing -fmax-errors=5 -I/home/kjetil/site_clang10/include "

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


if [ $# -ne 2 ] ; then
    echo "Usage: PYTHONBIN use_pygtk(yes/no) QTDIR"
    exit
fi

if test $2 != "no" ; then
    if test $2 != "yes" ; then
        echo "argument two (if using pygtk1) must be either yes or no"
        exit
    fi
fi


if test $2 = "yes" ; then

    #http://www.daa.com.au/~james/software/libglade/
    tar xvzf libglade-0.17.tar.gz
    cd libglade-0.17
    ./configure --prefix=$PREFIX
    make -j`nproc`
    make install
    cd ..


    #http://www.daa.com.au/~james/software/pygtk/
    tar xvzf pygtk-0.6.11.tar.gz
    cd pygtk-0.6.11
    export PYTHON=$1
    ./configure --prefix=$PREFIX --with-libglade-config=$PREFIX/bin/libglade-config
    make -j`nproc`
    make install
    sed -i s/" as"/" as2"/ $PREFIX/lib/python2.6/site-packages/gtk-1.2/gtk.py
    cd ..
fi


#tar xvf setxkbmap_56346c72127303a445a273217f7633c2afb29cfc.tar
#cd setxkbmap
#make clean
#./configure --prefix=/usr
#make
#cd ..


build_faust() {
    rm -fr faust
    tar xvzf faust_2020-12-27.tar.gz 
    mv faust-master-dev faust
    cd faust
    tar xvzf ../faustlibraries_2020-12-27.tar.gz 
    mv faustlibraries-master libraries
    
    patch -p0 <../faust.patch
    if env |grep INCLUDE_FAUSTDEV_BUT_NOT_LLVM ; then
        patch -p0 <../faust_nollvm.patch
    fi
    VERBOSE=1 CFLAGS="$CFLAGS" CXXFLAGS="$CXXFLAGS" CMAKEOPT="-DCMAKE_BUILD_TYPE=Release -DSELF_CONTAINED_LIBRARY=on -DCMAKE_CXX_COMPILER=`which $DASCXX` -DCMAKE_C_COMPILER=`which $DASCC`" make most
    cd ..
}

build_Visualization-Library() {

    rm -fr Visualization-Library-master
    tar xvzf Visualization-Library-master.tar.gz 
    cd Visualization-Library-master/
    patch -p1 <../visualization.patch
    #sed -i s/"VL_ACTOR_USER_DATA 0"/"VL_ACTOR_USER_DATA 1"/ src/vlCore/config.hpp
    export MYFLAGS="-std=gnu++11 $CPPFLAGS -fPIC -g  -Wno-c++11-narrowing" #  -D_GLIBCXX_USE_CXX11_ABI=0
    MYFLAGS="-std=gnu++11 $CPPFLAGS -fPIC -g -Wno-c++11-narrowing" #  -D_GLIBCXX_USE_CXX11_ABI=0
    echo 'set(CMAKE_CXX_FLAGS "$MYFLAGS")' >>CMakeLists.txt
    # previously used build type: RelWithDebInfo. Unfortunately, this one enable _DEBUG and various runtime checks.

    #CFLAGS="$CPPFLAGS -fPIC -g" CPPFLAGS="$MYFLAGS" CC="clang" CXX="clang++ $MYFLAGS" cmake -DCMAKE_CXX_FLAGS="$MYFLAGS" CMAKE_CXX_COMPILER="clang++ $MYFLAGS" -DCMAKE_BUILD_TYPE=Release -DCMAKE_POSITION_INDEPENDENT_CODE=ON SUPPORT=ON -DVL_DYNAMIC_LINKING=OFF -DVL_IO_2D_PNG=OFF -DVL_IO_2D_TIFF=OFF -DVL_IO_2D_JPG=OFF -DVL_IO_2D_TGA=OFF -DVL_IO_2D_BMP=OFF .
    
    CFLAGS="$CPPFLAGS -fPIC -g" CPPFLAGS="$MYFLAGS" CC="$DASCC" CXX="$DASCXX $MYFLAGS" cmake -DCMAKE_CXX_FLAGS="$MYFLAGS" CMAKE_CXX_COMPILER="$DASCXX $MYFLAGS" -DCMAKE_BUILD_TYPE=Release -DCMAKE_POSITION_INDEPENDENT_CODE=ON SUPPORT=ON -DVL_DYNAMIC_LINKING=OFF -DVL_IO_2D_PNG=OFF -DVL_IO_2D_TIFF=OFF -DVL_IO_2D_JPG=OFF -DVL_IO_2D_TGA=OFF -DVL_IO_2D_BMP=OFF .
    
    VERBOSE=1 make -j`nproc`
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
    make -j`nproc` # necessary to create the moc files.
    cd ..
}


#rm -fr sndlib
#tar xvzf sndlib.tar.gz
#cd sndlib
#patch -p0 <../sndlib.patch
#cd ..


#http://www.hpl.hp.com/personal/Hans_Boehm/gc/
build_gc() {
    rm -fr gc-7.4.16 libatomic_ops-7.4.14
    tar xvzf gc-7.4.16.tar.gz
    tar xvzf libatomic_ops-7.4.14.tar.gz
    cd gc-7.4.16
    ln -s ../libatomic_ops-7.4.14 libatomic_ops
    #echo 'void RADIUM_ensure_bin_packages_gc_is_used(void){ABORT("GC not configured properly");}' >>malloc.c
    echo 'void RADIUM_ensure_bin_packages_gc_is_used(void){}' >>malloc.c
    echo '#if defined(GC_ASSERTIONS) || !defined(NO_DEBUGGING)' >>malloc.c
    echo "#error "nope"" >>malloc.c
    echo "#endif" >>malloc.c
    #patch -p1 <../gcdiff.patch
    CFLAGS="-mtune=generic -msse2 -mfpmath=sse -g -O2" ./configure --enable-static --disable-shared --disable-gc-debug --disable-gc-assertions
    CFLAGS="-mtune=generic -msse2 -mfpmath=sse -g -O2" make -j`nproc`
    cd ..
}

build_fluidsynth() {

    rm -fr fluidsynth-1.1.6
    tar xvzf fluidsynth-1.1.6.tar.gz
    cd fluidsynth-1.1.6
    make clean
    CFLAGS="-fPIC -fno-strict-aliasing -O3 -DDEFAULT_SOUNDFONT=\\\"\\\"" CPPFLAGS="-fPIC -fno-strict-aliasing -O3" CXXFLAGS="-fPIC -fno-strict-aliasing -O3" ./configure --enable-static --disable-aufile-support --disable-pulse-support --disable-alsa-support --disable-libsndfile-support --disable-portaudio-support --disable-oss-support --disable-midishare --disable-jack-support --disable-coreaudio --disable-coremidi --disable-dart --disable-lash --disable-ladcca --disable-aufile-support --disable-dbus-support --without-readline
    # --enable-debug
    make -j`nproc`
    cd ..
}

build_libgig () {

    rm -fr libgig
    tar xvzf libgig.tar.gz
    cd libgig
    make clean
    CFLAGS="-O3 -fno-strict-aliasing" CPPFLAGS="-O3 -fno-strict-aliasing" CXXFLAGS="-O3 -fno-strict-aliasing" CC=$DASCC CXX=$DASCXX ./configure
    CFLAGS="$CFLAGS" CPPFLAGS="$CPPFLAGS" CPPFLAGS="$CXXFLAGS" make -j`nproc`
    cd ..
}


build_qscintilla() {
    # QScintilla
    rm -fr QScintilla_gpl-2.10.8
    tar xvzf QScintilla_gpl-2.10.8.tar.gz
    cd QScintilla_gpl-2.10.8/Qt4Qt5/
    echo "CONFIG += staticlib" >> qscintilla.pro
    `../../../../find_moc_and_uic_paths.sh qmake`
    patch -p0 <../../qscintilla.patch
    make -j`nproc`
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
        make -j`proc`
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


build_faust
build_Visualization-Library
build_libpds
build_qhttpserver
build_gc
build_fluidsynth
build_libgig
build_qscintilla
build_xcb

touch deletemetorebuild

#!/bin/bash

set -e
set -x

unset CFLAGS
unset CFLAGS
unset CPPFLAGS
unset LDFLAGS
unset CXXFLAGS

export CFLAGS="-mtune=generic -msse2 -mfpmath=sse"
export CPPFLAGS="-mtune=generic -msse2 -mfpmath=sse"
export CXXFLAGS="-mtune=generic -msse2 -mfpmath=sse"


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
    make
    make install
    cd ..


    #http://www.daa.com.au/~james/software/pygtk/
    tar xvzf pygtk-0.6.11.tar.gz
    cd pygtk-0.6.11
    export PYTHON=$1
    ./configure --prefix=$PREFIX --with-libglade-config=$PREFIX/bin/libglade-config
    make
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


rm -fr Visualization-Library-master
tar xvzf Visualization-Library-master.tar.gz 
cd Visualization-Library-master/
export MYFLAGS="-std=gnu++11 $CPPFLAGS -fPIC"
MYFLAGS="-std=gnu++11 $CPPFLAGS -fPIC"
echo 'set(CMAKE_CXX_FLAGS "$MYFLAGS")' >>CMakeLists.txt
if [[ $RADIUM_QT_VERSION == 4 ]]
then
    CFLAGS="$MYFLAGS" CPPFLAGS="$MYFLAGS" CXX="g++ $MYFLAGS" cmake -DCMAKE_CXX_FLAGS="$MYFLAGS" CMAKE_CXX_COMPILER="g++ $MYFLAGS" -DCMAKE_BUILD_TYPE=RelWithDebInfo -DCMAKE_POSITION_INDEPENDENT_CODE=ON -DVL_GUI_QT4_SUPPORT=ON -DVL_DYNAMIC_LINKING=OFF -DVL_IO_2D_PNG=OFF -DVL_IO_2D_TIFF=OFF -DVL_IO_2D_JPG=OFF -DVL_IO_2D_TGA=OFF -DVL_IO_2D_BMP=OFF .
else
    CFLAGS="$MYFLAGS" CPPFLAGS="$MYFLAGS" CXX="g++ $MYFLAGS" cmake -DCMAKE_CXX_FLAGS="$MYFLAGS" CMAKE_CXX_COMPILER="g++ $MYFLAGS" -DCMAKE_BUILD_TYPE=RelWithDebInfo -DCMAKE_POSITION_INDEPENDENT_CODE=ON -DVL_GUI_QT5_SUPPORT=ON -DVL_DYNAMIC_LINKING=OFF -DVL_IO_2D_PNG=OFF -DVL_IO_2D_TIFF=OFF -DVL_IO_2D_JPG=OFF -DVL_IO_2D_TGA=OFF -DVL_IO_2D_BMP=OFF .
fi
VERBOSE=1 make -j `nproc`
cd ..


rm -fr libpd-master
tar xvzf libpd-master.tar.gz
cd libpd-master/
make clean
make -j7
cd ..

rm -fr qhttpserver-master
tar xvzf qhttpserver-master.tar.gz
cd qhttpserver-master/
echo "CONFIG += staticlib" >> src/src.pro
`../../../find_moc_and_uic_paths.sh qmake`
make
cd ..


rm -fr s7
tar xvzf s7.tar.gz


#http://www.hpl.hp.com/personal/Hans_Boehm/gc/
rm -fr gc-7.4.4 libatomic_ops-7.4.4
tar xvzf gc-7.4.4.tar.gz
tar xvzf libatomic_ops-7.4.4.tar.gz
cd gc-7.4.4
ln -s ../libatomic_ops-7.4.4 libatomic_ops
#echo 'void RADIUM_ensure_bin_packages_gc_is_used(void){ABORT("GC not configured properly");}' >>malloc.c
echo 'void RADIUM_ensure_bin_packages_gc_is_used(void){}' >>malloc.c
echo '#if defined(GC_ASSERTIONS) || !defined(NO_DEBUGGING)' >>malloc.c
echo "#error "nope"" >>malloc.c
echo "#endif" >>malloc.c
CFLAGS="-mtune=generic -msse2 -mfpmath=sse -g -O2" ./configure --enable-static --disable-shared --disable-gc-debug --disable-gc-assertions
CFLAGS="-mtune=generic -msse2 -mfpmath=sse -g -O2" make -j3
cd ..

rm -fr fluidsynth-1.1.6
tar xvzf fluidsynth-1.1.6.tar.gz
cd fluidsynth-1.1.6
make clean
CFLAGS="-O3" CPPFLAGS="-O3" CXXFLAGS="-O3" ./configure --enable-static --disable-aufile-support --disable-pulse-support --disable-alsa-support --disable-libsndfile-support --disable-portaudio-support --disable-oss-support --disable-midishare --disable-jack-support --disable-coreaudio --disable-coremidi --disable-dart --disable-lash --disable-ladcca --disable-aufile-support --disable-dbus-support --without-readline
# --enable-debug
make -j3
cd ..

rm -fr libgig
tar xvzf libgig.tar.gz
cd libgig
make clean
CFLAGS="-O3" CPPFLAGS="-O3" CXXFLAGS="-O3" ./configure
make -j3
cd ..


# faust, debug
export CFLAGS="-mtune=generic -msse2 -mfpmath=sse -O0 -fsanitize=address -g"
export CPPFLAGS="-mtune=generic -msse2 -mfpmath=sse -O0 -fsanitize=address -g"
export CXXFLAGS="-mtune=generic -msse2 -mfpmath=sse -O0 -fsanitize=address -g"
export LDFLAGS="-fsanitize=address"
cd faust2
make -j `nproc`
mv compiler/libfaust.a libfaust_debug.a
make clean
cd ..

#faust, release
export CFLAGS="-mtune=generic -msse2 -mfpmath=sse -O2 -g"
export CPPFLAGS="-mtune=generic -msse2 -mfpmath=sse -O2 -g"
export CXXFLAGS="-mtune=generic -msse2 -mfpmath=sse -O2 -g"
export LDFLAGS=""
cd faust2
make -j `nproc`
mv libfaust_debug.a compiler/
cd ..

# QScintilla
rm -fr QScintilla_gpl-2.9.2
tar xvzf QScintilla_gpl-2.9.2.tar.gz
cd QScintilla_gpl-2.9.2/Qt4Qt5/
echo "CONFIG += staticlib" >> qscintilla.pro
`../../../../find_moc_and_uic_paths.sh qmake`
patch -p0 <../../qscintilla.patch
make -j `nproc`
cd ../..


touch deletemetorebuild

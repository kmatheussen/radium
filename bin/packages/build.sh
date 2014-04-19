#!/bin/bash

set -e

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


tar xvf setxkbmap_56346c72127303a445a273217f7633c2afb29cfc.tar
cd setxkbmap
make clean
./configure --prefix=/usr
make
cd ..


rm -fr Visualization-Library-master
tar xvzf Visualization-Library-master.tar.gz 
cd Visualization-Library-master/
cmake -DVL_GUI_QT4_SUPPORT=ON -DVL_DYNAMIC_LINKING=OFF -DVL_IO_2D_PNG=OFF -DVL_IO_2D_TIFF=OFF -DVL_IO_2D_JPG=OFF -DVL_IO_2D_TGA=OFF -DVL_IO_2D_BMP=OFF .
make -j8
cd ..


tar xvzf libpd-master.tar.gz
cd libpd-master/
make clean
make -j7
cd ..


# gc.tar.gz is currently gc-7.2d, with ABORT made into a dummy operation.

#http://www.hpl.hp.com/personal/Hans_Boehm/gc/
tar xvzf gc.tar.gz
cd gc-7.2
echo "void RADIUM_ensure_bin_packages_gc_is_used(void){}" >>malloc.c
CFLAGS=-fPIC ./configure --prefix=$PREFIX
CFLAGS=-fPIC make -j3
cd ..

tar xvjf xmessage-1.0.3.tar.bz2
cd xmessage-1.0.3
./configure --prefix=$PREFIX
make -j3
cd ..

tar xvzf fluidsynth-1.1.6.tar.gz
cd fluidsynth-1.1.6
make clean
CFLAGS="-O3" CPPFLAGS="-O3" ./configure --enable-static --disable-aufile-support --disable-pulse-support --disable-alsa-support --disable-libsndfile-support --disable-portaudio-support --disable-oss-support --disable-midishare --disable-jack-support --disable-coreaudio --disable-coremidi --disable-dart --disable-lash --disable-ladcca --disable-aufile-support --disable-dbus-support --without-readline
# --enable-debug
make -j3
cd ..

tar xvzf libgig.tar.gz
cd libgig
make clean
CFLAGS="-O3" CPPFLAGS="-O3" ./configure
make -j3
cd ..

touch deletemetorebuild




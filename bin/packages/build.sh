#!/bin/sh

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

if [ $# -ne 3 ] ; then
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

#http://www.riverbankcomputing.co.uk/software/pyqt/download3
if [ ! -f pyqtisbuilt ] ; then
    tar xvzf PyQt-x11-gpl-3.18.1.tar.gz
    cd PyQt-x11-gpl-3.18.1/
    python configure.py -q $3 -d $PREFIX
    make -j3
    #make install
    touch pyqtisbuilt
    cd ..
fi

#http://dickey.his.com/xterm/xterm.htlm
tar xvzf xterm.tar.gz
cd xterm-281
./configure --prefix=$PREFIX --x-includes=/usr/X11R6/include --x-libraries=/usr/X11R6/lib
make -j3
strip xterm
make install
cd ..


#http://www.hpl.hp.com/personal/Hans_Boehm/gc/
tar xvzf gc.tar.gz
cd gc-7.2
CFLAGS=-fPIC ./configure --prefix=$PREFIX
CFLAGS=-fPIC make -j3
cd ..

tar xvjf xmessage-1.0.3.tar.bz2
cd xmessage-1.0.3
./configure --prefix=$PREFIX
make -j3
cd ..

#http://code.google.com/p/py-setproctitle/
tar xvzf  dvarrazzo-py-setproctitle-version-1.1.6-0-gc35a1bf.tar.gz
cd dvarrazzo-py-setproctitle-c35a1bf
make
cd ..

ln -sf $1 $PREFIX/bin/python

touch deletemetorebuild




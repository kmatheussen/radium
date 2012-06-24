#!/bin/sh

set -e

rm -f /tmp/radium
ln -s `pwd` /tmp/radium


#http://www.python.org
#tar xvzf Python-2.2.2.tgz
#cd Python-2.2.2
#./configure --prefix=/tmp/radium --with-threads
#make
#make install
#cd ..

if [ $# -ne 2 ] ; then
    echo "Usage: python_bin use_pygtk"
    exit
fi

if test $2 != "no" ; then
    if test $2 != "yes" ; then
        echo "argument two (if using pygtk1) must be either yes or no"
        exit
    fi
fi

#http://www.daa.com.au/~james/software/libglade/
if test $2 = "yes" ; then
    tar xvzf libglade-0.17.tar.gz
    cd libglade-0.17
    ./configure --prefix=/tmp/radium
    make
    make install
    cd ..


#http://www.daa.com.au/~james/software/pygtk/
    tar xvzf pygtk-0.6.11.tar.gz
    cd pygtk-0.6.11
    export PYTHON=$1
    ./configure --prefix=/tmp/radium --with-libglade-config=/tmp/radium/bin/libglade-config
    make
    make install
    sed -i s/" as"/" as2"/ /tmp/radium/lib/python2.6/site-packages/gtk-1.2/gtk.py
    cd ..
fi


#http://dickey.his.com/xterm/xterm.htlm
tar xvzf xterm.tar.gz
cd xterm-179
./configure --prefix=/tmp/radium --x-includes=/usr/X11R6/include --x-libraries=/usr/X11R6/lib
make
strip xterm
make install
cd ..


#http://www.hpl.hp.com/personal/Hans_Boehm/gc/
tar xvzf gc.tar.gz
cd gc-7.2
./configure --prefix=/tmp/radium
make
cd ..

ln -sf $1 /tmp/radium/bin/python

touch deletemetorebuild




#!/bin/sh

export PYTHONEXE=python #mingw/bin/python.exe
export MOC=moc-qt4
export UIC=uic-qt4

export CCC=i686-pc-mingw32-g++
export CC=i686-pc-mingw32-gcc
export LINKER=i686-pc-mingw32-g++

export PKG=mingw32-pkg-config

export PYPATH=`pwd`/mingw/include/python2.7/

export QT_CFLAGS="-DQT_SHARED -DQT3_SUPPORT -I/usr/i686-pc-mingw32/sys-root/mingw/include/Qt3Support -I/usr/i686-pc-mingw32/sys-root/mingw/include/QtCore -I/usr/i686-pc-mingw32/sys-root/mingw/include/QtGui -I/usr/i686-pc-mingw32/sys-root/mingw/include/QtNetwork -I/usr/i686-pc-mingw32/sys-root/mingw/include/QtSql"
export QT_LDFLAGS="-L/usr/i686-pc-mingw32/sys-root/mingw/lib/ -lQt3Support4 -lQtGui4 -lQtNetwork4 -lQtSql4 -lQtCore4"

export OS_OPTS=-DFOR_WINDOWS
export OS_LDFLAGS="-lpthread `pwd`/mingw/gc-7.2/.libs/libgc.a `pwd`/mingw/lib/python2.7/libpython2.7.dll"

# To build gc for mingw32:
#
# cd bin/packages/gc-7.2
# mingw32-configure
# mingw32-make
# (that's it)

mingw32-make radium $@

cp bin/radium mingw/radium/radium.exe


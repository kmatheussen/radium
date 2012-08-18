#!/bin/sh

export PYTHONEXE=python #mingw/bin/python.exe
export MOC=moc-qt4
export UIC=uic-qt4

#export CCC=i686-pc-mingw32-g++
export CCC=i686-w64-mingw32-g++
export CC=i686-w64-mingw32-gcc
export LINKER=i686-w64-mingw32-g++

export BUILDTYPE=DEBUG

export PKG="wine `pwd`/temp/bin/pkg-config.exe"

export PYPATH=`pwd`/mingw/include/python2.7/

export GTK_CFLAGS="-mms-bitfields -I/home/kjetil/radium-qt4/temp/include/gtk-2.0 -I/home/kjetil/radium-qt4/temp/lib/gtk-2.0/include -I/home/kjetil/radium-qt4/temp/include/atk-1.0 -I/home/kjetil/radium-qt4/temp/include/cairo -I/home/kjetil/radium-qt4/temp/include/gdk-pixbuf-2.0 -I/home/kjetil/radium-qt4/temp/include/pango-1.0 -I/home/kjetil/radium-qt4/temp/include/glib-2.0 -I/home/kjetil/radium-qt4/temp/lib/glib-2.0/include -I/home/kjetil/radium-qt4/temp/include -I/home/kjetil/radium-qt4/temp/include/freetype2 -I/home/kjetil/radium-qt4/temp/include/libpng14"
export GTK_LDFLAGS="-L/home/kjetil/radium-qt4/temp/lib -lgtk-win32-2.0 -lgdk-win32-2.0 -latk-1.0 -lgio-2.0 -lpangowin32-1.0 -lgdi32 -lpangocairo-1.0 -lgdk_pixbuf-2.0 -lpango-1.0 -lcairo -lgobject-2.0 -lgmodule-2.0 -lgthread-2.0 -lglib-2.0 -lintl"

export QT_CFLAGS="-DQT_SHARED -DQT3_SUPPORT -I/usr/i686-w64-mingw32/sys-root/mingw/include/Qt3Support -I/usr/i686-w64-mingw32/sys-root/mingw/include/QtCore -I/usr/i686-w64-mingw32/sys-root/mingw/include/QtGui -I/usr/i686-w64-mingw32/sys-root/mingw/include/QtNetwork -I/usr/i686-w64-mingw32/sys-root/mingw/include/QtSql"
export QT_LDFLAGS="-L/usr/i686-w64-mingw32/sys-root/mingw/lib/ -lQt3Support4 -lQtGui4 -lQtNetwork4 -lQtSql4 -lQtCore4"

export OS_OPTS=-DFOR_WINDOWS
export OS_LDFLAGS="-lpthread `pwd`/mingw/gc-7.2/.libs/libgc.a `pwd`/mingw/lib/python2.7/libpython2.7.dll"

# To build gc for mingw32:
#
# cd bin/packages/gc-7.2
# mingw32-configure --enable-threads=single
# echo "void RADIUM_ensure_bin_packages_gc_is_used(void){}" >>malloc.c
# mingw32-make
# (that's it)

mingw32-make radium $@

cp bin/radium mingw/radium/radium.exe
cp bin/radium temp/bin/radium.exe


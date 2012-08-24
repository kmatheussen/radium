#!/bin/sh

export PYTHONEXE=python #In Mingw, this is just the python we use to autogenerate files and so forth during the build process, it is not used to find header or link files
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

export QT_CFLAGS="`mingw32-pkg-config  --cflags Qt3Support`"
export QT_LDFLAGS="`mingw32-pkg-config --libs Qt3Support`"

# MEMORY_DEBUG is defined since bdw-gc doesn't work properly under wine. (It does in real windows though.)
export OS_OPTS="-DFOR_WINDOWS -DMEMORY_DEBUG"
export OS_LDFLAGS="`pwd`/mingw/gc-7.2d/.libs/libgc.a `pwd`/mingw/lib/python2.7/libpython2.7.dll -lpthread"

#export RTMIDI_CFLAGS="-D__WINDOWS_MM__ -D__WINDOWS_KS__ -D__RTMIDI_DEBUG__"
export RTMIDI_CFLAGS="-D__WINDOWS_MM__ -D__RTMIDI_DEBUG__"
export RTMIDI_LDFLAGS="-lwinmm /usr/i686-w64-mingw32/sys-root/mingw/lib/libksuser.a -lsetupapi"

# To build gc for mingw32:
#
# cd bin/packages/gc-7.2
# mingw32-configure
# echo "void RADIUM_ensure_bin_packages_gc_is_used(void){}" >>malloc.c
# mingw32-make
# (that's it)

mingw32-make radium $@

cp bin/radium mingw/radium/radium.exe
cp bin/radium temp/bin/radium.exe


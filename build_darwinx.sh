#!/bin/sh

export PYTHONEXE=python #In Mingw, this is just the python we use to autogenerate files and so forth during the build process, it is not used to find header or link files
export MOC=moc-qt4
export UIC=uic-qt4

export CCC=darwinx-g++
export CC=darwinx-gcc
export LINKER=darwinx-g++

export BUILDTYPE=DEBUG
#export BUILDTYPE=RELEASE

export PYPATH=/home/kjetil/radium-qt4/darwinx/Versions/Current/include/python2.7

export GTK_CFLAGS="`darwinx-pkg-config --cflags gtk+-2.0`"
export GTK_LDFLAGS="`darwinx-pkg-config --static --libs gtk+-2.0`"

export QT_CFLAGS="`darwinx-pkg-config --cflags Qt3Support`"
export QT_LDFLAGS="`darwinx-pkg-config --libs Qt3Support`"

# MEMORY_DEBUG is defined since bdw-gc doesn't work properly under wine. (It does in real windows though.)
export OS_OPTS="-DFOR_MACOSX -DMEMORY_DEBUG"
export OS_LDFLAGS="`pwd`/darwinx/gc-7.2/.libs/libgc.a `pwd`/darwinx/Versions/2.7/lib/python2.7/config/libpython2.7.a -lpthread"

#export RTMIDI_CFLAGS="-D__WINDOWS_MM__ -D__WINDOWS_KS__ -D__RTMIDI_DEBUG__"
#export RTMIDI_CFLAGS="-D__WINDOWS_MM__ -D__RTMIDI_DEBUG__"
#export RTMIDI_LDFLAGS="-lwinmm /usr/i686-w64-mingw32/sys-root/mingw/lib/libksuser.a -lsetupapi"
export RTMIDI_CFLAGS="-D__DUMMY__ -D__RTMIDI_DEBUG__"
export RTMIDI_LDFLAGS=""

if [ $BUILDTYPE = "RELEASE" ] ; then
    export OS_LDFLAGS="$OS_LDFLAGS -mwindows"
fi

# To build gc for mingw32:
#
# cd bin/packages/gc-7.2
# mingw32-configure
# echo "void RADIUM_ensure_bin_packages_gc_is_used(void){}" >>malloc.c
# mingw32-make
# (that's it)

darwinx-make radium $@

cp bin/radium /home/kjetil/Dropbox/radium.app/Contents/MacOS/


#cp bin/radium mingw/radium/radium.exe
#cp bin/radium temp/bin/radium.exe


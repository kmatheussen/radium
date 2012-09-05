#!/bin/sh

export PYTHONEXE=python #In Mingw, this is just the python we use to autogenerate files and so forth during the build process, it is not used to find header or link files
export MOC=moc-qt4
export UIC=uic-qt4

export OPTIMIZE="-mtune=generic -O3"

#export CCC=i686-pc-mingw32-g++
export CCC=i686-w64-mingw32-g++
export CC=i686-w64-mingw32-gcc
export LINKER=i686-w64-mingw32-g++

#export BUILDTYPE=DEBUG
export BUILDTYPE=RELEASE

export PKG="wine `pwd`/temp/bin/pkg-config.exe"

export PYPATH=`pwd`/mingw/include/python2.7/

export QT_CFLAGS="`mingw32-pkg-config  --cflags Qt3Support`"
export QT_LDFLAGS="-L/usr/i686-w64-mingw32/sys-root/mingw/lib -lQt3Support4 -lQtGui4 -lQtCore4"

# MEMORY_DEBUG is defined since bdw-gc doesn't work properly under wine. (It does in real windows though.)
#export OS_OPTS="-DFOR_WINDOWS -DMEMORY_DEBUG"
export OS_OPTS="-DFOR_WINDOWS"
export OS_LDFLAGS="`pwd`/mingw/gc-7.2d/.libs/libgc.a `pwd`/mingw/lib/python2.7/libpython2.7.dll -lpthread"

#export RTMIDI_CFLAGS="-D__WINDOWS_MM__ -D__WINDOWS_KS__ -D__RTMIDI_DEBUG__"
export RTMIDI_CFLAGS="-D__WINDOWS_MM__ -D__RTMIDI_DEBUG__"
export RTMIDI_LDFLAGS="-lwinmm /usr/i686-w64-mingw32/sys-root/mingw/lib/libksuser.a -lsetupapi"

if [ $BUILDTYPE = "RELEASE" ] ; then
    export OS_LDFLAGS="$OS_LDFLAGS -mwindows"
fi

export OBJ_X11=""
export OBJ_MACOSX=""

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
cp bin/radium Dropbox/windows_dist/bin/radium.exe

if echo $OS_OPTS |grep MEMORY_DEBUG 2>/dev/null ; then
    echo
    echo "WARNING! MEMORY_DEBUG is turned on!"
    echo
fi

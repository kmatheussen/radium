#!/bin/bash

set -e

# NOTE! In case linking fails with error messages like this:
#
#   QtGui/qs60style.h:157: undefined reference to `QS60Style::staticMetaObject'
#
# and so forth, removing the line "#include <QtGUI>" is likely to help.
# Plus that lines like this: "#include <QtGui/qwidget.h>" must be replaced with "#include <qwidget.h>" lines.
#


export PYTHONEXE=python #In Mingw, this is just the python we use to autogenerate files and so forth during the build process, it is not used to find header or link files
export MOC=moc-qt4
export UIC=uic-qt4

export OPTIMIZE="-mtune=generic -O3"
#export OPTIMIZE="-mtune=native -O3"
export CPUOPT=$OPTIMIZE # Some files are so CPU intensive that we need to turn on optimizations even in debug mode, at least when running in valgrind.


#export CCC=i686-pc-mingw32-g++
export CCC=i686-w64-mingw32-g++
export CC=i686-w64-mingw32-gcc
export LINKER=i686-w64-mingw32-g++

#export BUILDTYPE=DEBUG
if ! env |grep BUILDTYPE ; then
    echo "Must define BUILDTYPE. (DEBUG/RELEASE)"
    exit -1
fi

#export PKG="wine `pwd`/temp/bin/pkg-config.exe"
export PKG=mingw32-pkg-config

export PYPATH=`pwd`/mingw/include/python2.7/

export JACKOPT="-I/home/kjetil/.wine/drive_c/Program\ Files\ \(x86\)/Jack/includes"

export QT_CFLAGS="`mingw32-pkg-config --cflags Qt3Support`"
export QT_LDFLAGS="`mingw32-pkg-config --libs Qt3Support`"
#"-L/usr/i686-w64-mingw32/sys-root/mingw/lib -lQt3Support4 -lQtGui4 -lQtCore4"

export OS_OPTS="$OS_OPTS -DFOR_WINDOWS -I`pwd`/mingw/include/ -DUSE_VESTIGE=0 -I/home/kjetil/Dropbox/radium_build/"

# MEMORY_DEBUG is defined since bdw-gc doesn't work properly under wine. (It does in real windows though.)
#export OS_OPTS="$OS_OPTS -DMEMORY_DEBUG"

export OS_LDFLAGS="`pwd`/mingw/gc-7.2d/.libs/libgc.a `pwd`/mingw/fluidsynth-1.1.6/src/.libs/libfluidsynth-1.dll `pwd`/mingw/libgig/src/.libs/libgig.a `pwd`/mingw/lib/python2.7/libpython2.7.dll -lpthread /home/kjetil/.wine/drive_c/Program\ Files\ \(x86\)/Jack/lib/libjack.lib -lole32 mingw/lib/WbemUuid.Lib `$PKG --libs lrdf` `$PKG --libs sndfile` `$PKG --libs samplerate` `$PKG --libs glib-2.0` -Lmingw/lib/ /home/kjetil/radium-qt4/mingw/binutils-2.23.1/bfd/libbfd.a /home/kjetil/radium-qt4/mingw/binutils-2.23.1/libiberty/libiberty.a -lz -limagehlp"

# -msse -msse2 -msse3 -mfpmath=sse -ffast-math

#export RTMIDI_CFLAGS="-D__WINDOWS_MM__ -D__WINDOWS_KS__ -D__RTMIDI_DEBUG__"
export RTMIDI_CFLAGS="-D__WINDOWS_MM__ -D__RTMIDI_DEBUG__"
#export RTMIDI_LDFLAGS="-lwinmm /usr/i686-w64-mingw32/sys-root/mingw/lib/libksuser.a -lsetupapi"
export RTMIDI_LDFLAGS="-lwinmm -lsetupapi"

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

cp bin/radium Dropbox/windows_dist/bin/radium.bin.exe
cp bin/crashreporter Dropbox/windows_dist/bin/crashreporter.exe

cp bin/*.py Dropbox/windows_dist/bin/
cp bin/*.pyc Dropbox/windows_dist/bin/
cp bin/colors Dropbox/windows_dist/bin/
cp bin/menues.conf Dropbox/windows_dist/bin/
cp bin/protos.conf Dropbox/windows_dist/bin/
cp bin/keybindings.conf Dropbox/windows_dist/bin/
cp bin/keybindings.cPickle Dropbox/windows_dist/bin/
cp bin/new_song.rad Dropbox/windows_dist/bin/
cp -a bin/sounds Dropbox/windows_dist/bin/
cp bin/radium_256x256x32.png Dropbox/windows_dist/bin/
cp -a bin/fonts Dropbox/windows_dist/bin/

cp bin/config Dropbox/windows_dist/bin/

# fix fonts. Qt uses different text formats for QFont::toString/QFont::fromString on Windows and Linux/OSX. (The windows system stores everything, and is better, BTW.)
sed -i 's/Lato/Lato Black/' Dropbox/windows_dist/bin/config

cd icons && ./create.sh && cd ..
$CC windows/launcher.c icons/windows_icon.o -mwindows -Wall -Wl,--subsystem,windows -o Dropbox/windows_dist/radium.exe 

#if [ $BUILDTYPE == "RELEASE" ] ; then
#    i686-w64-mingw32-strip Dropbox/windows_dist/bin/radium.bin.exe
#fi

if echo $OS_OPTS |grep MEMORY_DEBUG 2>/dev/null ; then
    echo
    echo "WARNING! MEMORY_DEBUG is turned on!"
    echo
fi


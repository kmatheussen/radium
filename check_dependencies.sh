#!/bin/bash

PYTHONEXE=$1
MOC=$2
UIC=$3


set -e

echo
echo "Checking dependencies: "
which sed
which $PYTHONEXE
#which guile-1.8

if [ $4 == "test_packages" ] ; then
    echo "testing packages"
fi

if [ $4 == "test_build" ] ; then
    echo "testing build"
fi

if $1 -c "import sys ; sys.exit(sys.version[:1] == \"2\")" ; then
    echo "Only Python 2 is supported:"
    $1 --version
    echo
    exit 5
fi

if [ $4 == "test_build" ] ; then

    if ! which $MOC ; then
        echo "Can not find moc. Make sure QTDIR and/or MOC is set correctly in the Makefile".
        echo
        exit 5
    fi

    if ! which $UIC ; then
        echo "Can not find uic. Make sure QTDIR and/or UIC set correctly in the Makefile".
        echo
        exit 5
    fi

    if $MOC -v 2>&1 |grep Qt\ 3 ; then
        echo $MOC "is for QT3. Need moc for QT4. Make sure MOC is set correctly in the Makefile."
        echo
        exit 5
    fi

    if $UIC -v 2>&1 |grep Qt\ 3 ; then
        echo $UIC "is for QT3. Need uic for QT4. Make sure UIC is set correctly in the Makefile."
        echo
        exit 5
    fi
fi

if grep -e "\ \*" api/protos.conf ; then
    echo "The above line in api/protos.conf is wrongly formatted. Must use \"<type>*\", not \"<type> *\""
    echo
    exit 5
fi


if [ `uname` == "Linux" ] ; then
 
    echo "#include <X11/Xaw/Scrollbar.h>" >temp$$.c
    echo "main(){return 0;}" >>temp$$.c
    echo >>temp$$.c
    if ! gcc temp$$.c -lXaw ; then
	echo "Might be missing libXaw-devel"
	echo
	rm temp$$.c
	exit 5
    fi
    rm temp$$.c
fi


echo "#include <bfd.h>"  >temp$$.c
echo "#include <dlfcn.h>" >>temp$$.c
echo "#include <libiberty.h>" >>temp$$.c
echo "main(){return 0;}" >>temp$$.c
echo >>temp$$.c
if ! gcc temp$$.c -lbfd -ldl ; then
    echo "Couldn't find -lbfd or -ldl, or header files for bfd, dlfcn or libiberty."
    echo "On Fedora, binutils-devel, libtool-ltdl or libtool might be missing."
    echo "On Debian, libc6-dev or binutils-dev might be missing."
    exit 5
fi
rm temp$$.c

if ! pkg-config --cflags sndfile >/dev/null 2>devnull ; then
    echo "libsndfile not found"
    exit 5
fi

if ! pkg-config --cflags samplerate >/dev/null 2>devnull ; then
    echo "libsamplerate not found"
    exit 5
fi

if [ `uname` == "Linux" ] ; then
    if ! pkg-config --cflags lrdf >/dev/null 2>devnull ; then
	echo "liblrdf not found"
	exit 5
    fi
fi

if ! pkg-config --cflags glib-2.0 >/dev/null 2>devnull ; then
    echo "glib-2.0 not found"
    exit 5
fi

if ! pkg-config --cflags Qt3Support >/dev/null 2>devnull ; then
    echo "Qt4 not found"
    exit 5
fi


if [ $4 == "test_build" ] ; then
    if [ `uname` == "Linux" ] ; then
        if [ ! -f bin/packages/deletemetorebuild ] ; then
	    echo
	    echo "Packages not build. First run 'make packages'"
	    echo
	    exit 5
        fi
    fi
fi

echo "All seems good"
echo


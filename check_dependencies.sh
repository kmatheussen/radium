#!/usr/bin/env bash

PYTHONEXE=$1
MOC=$2
UIC=$3

set -eEu
#set -x

source configuration.sh


mywhich() {
    if which $1 ; then
        echo $1 found
    else
        echo $1 not found
        exit -1
    fi
}

echo
echo "Checking dependencies: "
mywhich sed
mywhich $PYTHONEXE
#which guile-1.8
mywhich guile

if [[ $4 == "test_packages" ]] ; then
    echo "testing packages"
fi

if [[ $4 == "test_build" ]] ; then
    echo "testing build"
fi

if $1 -c "import sys ; sys.exit(sys.version[:1] == \"2\")" ; then
    echo "Only Python 2 is supported:"
    $1 --version
    echo
    exit 5
fi

if [[ $4 == "test_build" ]] ; then

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

    if $MOC -v 2>&1 |grep "Qt 3" ; then
        echo $MOC "is for QT3. Need moc for QT$RADIUM_QT_VERSION. Make sure MOC is set correctly in the Makefile."
        echo
        exit 5
    fi

    if $UIC -v 2>&1 |grep "Qt 3" ; then
        echo $UIC "is for QT3. Need uic for QT$RADIUM_QT_VERSION. Make sure UIC is set correctly in the Makefile."
        echo
        exit 5
    fi
fi

input="api/protos.conf"
while IFS= read -r line
do
    #echo $line
    if [[ "$line" == \#* ]] ; then
        true
    else       
        if echo "$line" | grep -e " \*" ; then
            echo "This line in api/protos.conf is wrongly formatted. Must use \"<type>*\", not \"<type> *\""
            echo
            exit 5
        fi
    fi
done < "$input"


if grep "int seqblockid" api/protos.conf ; then
    echo "The above line(s) in api/protos.conf is/are wrong. seqblock id is 64 bit"
    echo
    exit 5
fi

if grep "int seqblock_id" api/protos.conf ; then
    echo "The above line(s) in api/protos.conf is/are wrong. seqblock id is 64 bit"
    echo
    exit 5
fi

if grep "int instrument_id" api/protos.conf ; then
    echo "The above line(s) in api/protos.conf is/are wrong. instrument id is 64 bit"
    echo
    exit 5
fi

if grep "int guinum" api/protos.conf ; then
    echo "The above line(s) in api/protos.conf is/are wrong. guinum is 64 bit"
    echo
    exit 5
fi

if grep "int id" api/protos.conf ; then
    echo "The above line(s) in api/protos.conf is/are wrong. id has so far been 64 bit"
    echo
    exit 5
fi


#if [ `uname` == "Linux" ] ; then
# 
#    echo "#include <X11/Xaw/Scrollbar.h>" >temp$$.c
#    echo "int main(){return 0;}" >>temp$$.c
#    echo >>temp$$.c
#    if ! gcc temp$$.c -lXaw ; then
#	echo "Might be missing libXaw-devel"
#	echo
#	rm temp$$.c
#	exit 5
#    fi
#    rm temp$$.c
#fi


if [ `uname` == "Linux" ] ; then
    echo "#define PACKAGE 1" >temp$$.c
    echo "#define PACKAGE_VERSION 1" >>temp$$.c
    echo "#include <bfd.h>"  >>temp$$.c
    echo "#include <dlfcn.h>" >>temp$$.c
#    echo "#include <libiberty.h>" >>temp$$.c
    echo "int main(){return 0;}" >>temp$$.c
    echo >>temp$$.c
    if ! gcc temp$$.c -lbfd -liberty -ldl; then
	echo "Couldn't find -lbfd, -ldl, -liberty, or header files for bfd or dlfcn."
	echo "On Fedora, binutils-devel, libtool-ltdl or libtool might be missing."
	echo "On Debian, libc6-dev or binutils-dev might be missing."
	exit 5
    fi
    rm temp$$.c
fi

if $QMAKE --version|grep "5\.0\." ; then
    echo "Qt is too old. Need at least 5.14"
    exit 5
fi

if $QMAKE --version|grep "5\.1\." ; then
    echo "Qt is too old. Need at least 5.14"
    exit 5
fi

if $QMAKE --version|grep "5\.2\." ; then
    echo "Qt is too old. Need at least 5.14"
    exit 5
fi

if $QMAKE --version|grep "5\.3\." ; then
    echo "Qt is too old. Need at least 5.14"
    exit 5
fi

if $QMAKE --version|grep "5\.4\." ; then
    echo "Qt is too old. Need at least 5.14"
    exit 5
fi

if $QMAKE --version|grep "5\.5\." ; then
    echo "Qt is too old. Need at least 5.14"
    exit 5
fi

if $QMAKE --version|grep "5\.6\." ; then
    echo "Qt is too old. Need at least 5.14"
    exit 5
fi

if $QMAKE --version|grep "5\.7\." ; then
    echo "Qt is too old. Need at least 5.14"
    exit 5
fi

if $QMAKE --version|grep "5\.8\." ; then
    echo "Qt is too old. Need at least 5.14"
    exit 5
fi

if $QMAKE --version|grep "5\.9\." ; then
    echo "Qt is too old. Need at least 5.14"
    exit 5
fi

if $QMAKE --version|grep "5\.10\." ; then
    echo "Qt is too old. Need at least 5.14"
    exit 5
fi

if $QMAKE --version|grep "5\.11\." ; then
    echo "Qt is too old. Need at least 5.14"
    exit 5
fi

if $QMAKE --version|grep "5\.12\." ; then
    echo "Qt is too old. Need at least 5.14"
    exit 5
fi

if $QMAKE --version|grep "5\.13\." ; then
    echo "Qt is too old. Need at least 5.14"
    exit 5
fi

if $QMAKE --version|grep "5\.5\." ; then
    QT_QPA_PLATFORM_PLUGIN_PATH=`$QMAKE -query QT_INSTALL_PLUGINS`
    if grep -r kf5deprecatedwidgets.so $QT_QPA_PLATFORM_PLUGIN_PATH ; then
        echo
        echo "The file $QT_QPA_PLATFORM_PLUGIN_PATH/*/kf5deprecatedwidgets.so in Qt 5.5 makes Radium (and other programs) misbehave. You should delete this file or use a different version of Qt."
        echo
        exit 5
    fi
fi

if ! pkg-config --cflags sndfile >/dev/null 2>/dev/null ; then
    echo "libsndfile not found"
    exit 5
fi

if ! pkg-config --cflags samplerate >/dev/null 2>/dev/null ; then
    echo "libsamplerate not found"
    exit 5
fi

if [ `uname` == "Linux" ] ; then
    if ! pkg-config --cflags lrdf >/dev/null 2>/dev/null ; then
	echo "liblrdf not found"
	exit 5
    fi
fi

if ! pkg-config --cflags glib-2.0 >/dev/null 2>/dev/null ; then
    echo "glib-2.0 not found"
    exit 5
fi

if [[ $4 == "test_build" ]] ; then
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


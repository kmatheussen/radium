#!/bin/bash

PYTHONEXE=$1
MOC=$2
UIC=$3

set -e
#set -x

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

    if $MOC -v 2>&1 |grep Qt\ 3 ; then
        echo $MOC "is for QT3. Need moc for QT$RADIUM_QT_VERSION. Make sure MOC is set correctly in the Makefile."
        echo
        exit 5
    fi

    if $UIC -v 2>&1 |grep Qt\ 3 ; then
        echo $UIC "is for QT3. Need uic for QT$RADIUM_QT_VERSION. Make sure UIC is set correctly in the Makefile."
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
    echo "int main(){return 0;}" >>temp$$.c
    echo >>temp$$.c
    if ! gcc temp$$.c -lXaw ; then
	echo "Might be missing libXaw-devel"
	echo
	rm temp$$.c
	exit 5
    fi
    rm temp$$.c
fi


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

if $(./find_moc_and_uic_paths.sh qmake) --version|grep "5\.5\." ; then
    QT_QPA_PLATFORM_PLUGIN_PATH=`$(./find_moc_and_uic_paths.sh qmake) -query QT_INSTALL_PLUGINS`
    if grep -r kf5deprecatedwidgets.so $QT_QPA_PLATFORM_PLUGIN_PATH ; then
        echo
        echo "The file $QT_QPA_PLATFORM_PLUGIN_PATH/*/kf5deprecatedwidgets.so in Qt 5.5 makes Radium (and other programs) misbehave. You should delete this file or use a different version of Qt."
        echo
        exit 5
    fi
fi

if $(./find_moc_and_uic_paths.sh qmake) --version|grep "5\.0\." ; then
    echo "Qt is too old. Need at least 5.9"
    exit 5
fi

if $(./find_moc_and_uic_paths.sh qmake) --version|grep "5\.1\." ; then
    echo "Qt is too old. Need at least 5.9"
    exit 5
fi

if $(./find_moc_and_uic_paths.sh qmake) --version|grep "5\.2\." ; then
    echo "Qt is too old. Need at least 5.9"
    exit 5
fi

if $(./find_moc_and_uic_paths.sh qmake) --version|grep "5\.3\." ; then
    echo "Qt is too old. Need at least 5.9"
    exit 5
fi

if $(./find_moc_and_uic_paths.sh qmake) --version|grep "5\.4\." ; then
    echo "Qt is too old. Need at least 5.9"
    exit 5
fi

if $(./find_moc_and_uic_paths.sh qmake) --version|grep "5\.5\." ; then
    echo "Qt is too old. Need at least 5.9"
    exit 5
fi

if $(./find_moc_and_uic_paths.sh qmake) --version|grep "5\.6\." ; then
    echo "Qt is too old. Need at least 5.9"
    exit 5
fi

if $(./find_moc_and_uic_paths.sh qmake) --version|grep "5\.7\." ; then
    echo "Qt is too old. Need at least 5.9"
    exit 5
fi

if $(./find_moc_and_uic_paths.sh qmake) --version|grep "5\.8\." ; then
    echo "Qt is too old. Need at least 5.9"
    exit 5
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

if [[ ! -f ~/SDKs/vstsdk2.4/pluginterfaces/vst2.x/aeffect.h && ! -f ~/SDKs/"VST3 SDK"/pluginterfaces/vst2.x/aeffect.h && ! -f ~/SDKs/VST_SDK/VST2_SDK/pluginterfaces/vst2.x/aeffect.h ]] ; then
    echo 'Steinberg VST headers not found. (Missing "~/SDKs/vstsdk2.4/pluginterfaces/vst2.x/aeffect.h or "~/SDKs/VST3 SDK/pluginterfaces/vst2.x/aeffect.h" or "~/SDKs/VST_SDK/VST2_SDK/pluginterfaces/vst2.x/aeffect.h")'
    echo 'You should find these files in the "VST Audio Plug-Ins SDK" from http://www.steinberg.net/en/company/developers.html . Unless Steinberg have changed the directory structure again, it should enough to unzip the file into your ~/SDKs directory.'
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


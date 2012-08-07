#!/bin/sh

PYTHONEXE=$1
MOC=$2
UIC=$3

set -e

echo
echo "Checking dependencies: "
which sed
which $PYTHONEXE


if $1 -c "import sys ; sys.exit(sys.version[:1] == \"2\")" ; then
    echo "Only Python 2 is supported:"
    $1 --version
    echo
    exit -1
fi

if ! which $MOC ; then
    echo "Can not find moc. Make sure QTDIR and/or MOC is set correctly in the Makefile".
    echo
    exit -1
fi

if ! which $UIC ; then
    echo "Can not find uic. Make sure QTDIR and/or UIC set correctly in the Makefile".
    echo
    exit -1
fi

if $MOC -v 2>&1 |grep Qt\ 4 ; then
    echo $MOC "is for QT4. Need moc for QT3. Make sure QTDIR and/or MOC is set correctly in the Makefile."
    echo
    exit -1
fi

if $UIC -v 2>&1 |grep Qt\ 4 ; then
    echo $UIC "is for QT4. Need uic for QT3. Make sure QTDIR and/or UIC is set correctly in the Makefile."
    echo
    exit -1
fi


if grep -e "\ \*" api/protos.conf ; then
    echo "The above line in api/protos.conf is wrongly formatted. Must use \"<type>*\", not \"<type> *\""
    echo
    exit -1
fi



echo "#include <X11/Xaw/Scrollbar.h>" >temp$$.c
echo "main(){return 0;}" >>temp$$.c
echo >>temp$$.c
if ! gcc temp$$.c -lXaw ; then
    echo "Might be missing libXaw-devel"
    echo
    rm temp$$.c
    exit -1
fi
rm temp$$.c


if ! xterm -e echo ; then
    echo
    echo "xterm not found in path. xterm must be installed."
    echo
    exit -1
fi

if [ ! -f bin/packages/deletemetorebuild ] ; then
    echo
    echo "Packages not build. First run 'make packages'"
    echo
    exit -1
fi

echo "All seems good"
echo


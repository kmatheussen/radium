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
    exit -1
fi

if ! which $MOC ; then
    echo "Can not find moc. Make sure QTDIR is set correctly in the Makefile".
    exit -1
fi

if ! which $UIC ; then
    echo "Can not find uic. Make sure QTDIR is set correctly in the Makefile".
    exit -1
fi

if $MOC -v 2>&1 |grep Qt\ 4 ; then
    echo $MOC "is for QT4. Need moc for QT3. Make sure QTDIR is set correctly in the Makefile."
    exit -1
fi

if $UIC -v 2>&1 |grep Qt\ 4 ; then
    echo $UIC "is for QT4. Need uic for QT3. Make sure QTDIR is set correctly in the Makefile."
    exit -1
fi


if grep -e "\ \*" api/protos.conf ; then
    echo "The above line in api/protos.conf is wrongly formatted. Must use \"<type>*\", not \"<type> *\""
    exit -1
fi



echo "#include <X11/Xaw/Scrollbar.h>" >temp$$.c
echo "main(){return 0;}" >>temp$$.c
echo >>temp$$.c
if ! gcc temp$$.c -lXaw ; then
    echo "Might be missing libXaw-devel"
    exit -1
fi


echo "#include <ncurses/curses.h>" >temp$$.c
echo "#include <curses.h>" >>temp$$.c
echo "#include <ncurses/term.h>" >>temp$$.c
echo "main(){return 0;}" >>temp$$.c
echo >>temp$$.c
if ! gcc temp$$.c ; then
    echo "Might be missing curses-devel"
    exit -1
fi


echo "All seems good"
echo


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

which $MOC
which $UIC


if $MOC -v 2>&1 |grep Qt\ 4 ; then
    echo $MOC "is for QT4. Need moc for QT3"
    exit -1
fi

if $UIC -v 2>&1 |grep Qt\ 4 ; then
    echo $UIC "is for QT4. Need uic for QT3"
    exit -1
fi

grep -e "char\ \*" api/protos.conf
echo "All seems good (but everything has probably not been tested)"
echo


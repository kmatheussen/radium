#!/bin/sh

PYTHONEXE=$1
MOC=$2
UIC=$3

set -e

echo "Checking install dependencies: "

if grep ^OPTIMIZE Makefile |grep -v -e "-O3" ; then
    echo "The OPTIMIZE variable in the Makefile does not contain -O3"
    exit -1
fi

if grep ^BUILDTYPE\= Makefile |grep -v -e ^BUILDTYPE\=DEBUG ; then
    echo "BUILDTYPE=RELEASE is not supported"
    exit -1
fi

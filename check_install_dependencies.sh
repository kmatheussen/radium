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

if grep ^export\ BUILDTYPE\= build_linux.sh |grep -v -e RELEASE ; then
    echo
    echo "Missing BUILDTYPE=RELEASE."
    exit -1
fi

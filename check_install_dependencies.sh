#!/bin/bash

PYTHONEXE=$1
MOC=$2
UIC=$3

set -e

echo "Checking install dependencies: "

if grep ^export\ OPTIMIZE build_linux.sh |grep -v -e "-O2" ; then
    echo "The OPTIMIZE variable in build_linux.sh does not contain -O2"
    exit -1
fi

if grep ^export\ BUILDTYPE\= build_linux.sh |grep -v -e RELEASE ; then
    echo
    echo "Missing BUILDTYPE=RELEASE  in build_linux.sh."
    exit -1
fi

#if grep ^export\ RTMIDI_CFLAGS\=  build_linux_common.sh |grep -v D__UNIX_JACK_  ; then
#    echo
#    echo "Missing RTMIDI_CFLAGS=-D__UNIX_JACK__ in build_linux_common.sh"
#    exit -1
#fi


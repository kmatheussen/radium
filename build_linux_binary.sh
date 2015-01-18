#!/bin/bash

set -e 

export IS_LINUX_BINARY="-DIS_LINUX_BINARY"

rm -f Qt_Main.o

export OPTIMIZE="-mtune=generic -msse -O3"
export BUILDTYPE=RELEASE

./build_linux.sh $@


#!/bin/bash

# This is the command I use to run when developing. -Kjetil

#VL_DATA_PATH=/home/kjetil/Visualization-Library/data BUILDTYPE=DEBUG ./build_linux.sh -j7 &&

THIS_DIR=$(dirname $(readlink -f $0))
XCB_LIB_DIR=$THIS_DIR/bin/packages/libxcb-1.12/src/.libs

if ! file $XCB_LIB_DIR ; then
    echo "Unable to find directory $XCB_LIB_DIR"
    exit -1
fi

export LD_LIBRARY_PATH=$XCB_LIB_DIR${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}

#export LD_PRELOAD=$HOME/jack_interposer/jack_interposer.so 

BUILDTYPE=DEBUG ./build_linux.sh -j `expr $(nproc) - 1` && ./run_gdb.sh $@

# QT_FATAL_WARNINGS=1 causes lots of crashes in qt5


#BUILDTYPE=DEBUG ./build_linux.sh -j7 && G_DEBUG=fatal-criticals QT_FATAL_WARNINGS=1 gdb bin/radium


#VL_DATA_PATH=/home/kjetil/Visualization-Library/data

# doesn't work:
#rbreak ^__ubsan_handle_
#d 2

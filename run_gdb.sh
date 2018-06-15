#!/bin/bash

THIS_DIR=$(dirname $(readlink -f $0))
XCB_LIB_DIR=$THIS_DIR/bin/packages/libxcb-1.13/src/.libs

if ! file $XCB_LIB_DIR ; then
    echo "Unable to find directory $XCB_LIB_DIR"
    exit -1
fi

export LD_LIBRARY_PATH=$XCB_LIB_DIR${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}

LD_LIBRARY_PATH=$LD_LIBRARY_PATH G_DEBUG=fatal-criticals gdb --args bin/radium_linux.bin $@; killall -9 radium_progress_window ; killall -9 radium_crashreporter

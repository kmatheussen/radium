#!/bin/bash

# This is the command I use to run when developing. -Kjetil

#VL_DATA_PATH=/home/kjetil/Visualization-Library/data BUILDTYPE=DEBUG ./build_linux.sh -j7 &&
export LSAN_OPTIONS=suppressions=`pwd`/SanitizerSuppr.txt
export ASAN_OPTIONS="detect_leaks=0,allocator_may_return_null=1,abort_on_error=1,new_delete_type_mismatch=0,alloc_dealloc_mismatch=0,max_malloc_fill_size=1048576" # new_delete_type_mismatch=0 because of qt5. alloc_dealloc_mismatch because of various vst plugins. detect_stack_use_after_return=1 sounds nice, but is very slow.
export UBSAN_OPTIONS="print_stacktrace=1:abort_on_error=1"
#suppressions=`pwd`/SanitizerSuppr.txt:
export TSAN_OPTIONS="history_size=7,second_deadlock_stack=1,suppressions=SanitizerSuppr.txt"

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

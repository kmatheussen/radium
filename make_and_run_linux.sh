#!/bin/bash

# This is the command I use to run when developing. -Kjetil

#VL_DATA_PATH=/home/kjetil/Visualization-Library/data BUILDTYPE=DEBUG ./build_linux.sh -j7 &&
export LSAN_OPTIONS=suppressions=SanitizerSuppr.txt
export ASAN_OPTIONS="detect_leaks=0,allocator_may_return_null=1,suppressions=SanitizerSuppr.txt,abort_on_error=1,new_delete_type_mismatch=0" # new_delete_type_mismatch=0 because of qt5.

export TSAN_OPTIONS="history_size=7,second_deadlock_stack=1,suppressions=SanitizerSuppr.txt"
BUILDTYPE=DEBUG ./build_linux.sh -j7 && G_DEBUG=fatal-criticals QT_FATAL_WARNINGS=1 gdb bin/radium
#VL_DATA_PATH=/home/kjetil/Visualization-Library/data

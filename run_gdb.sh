#!/bin/bash

if [ -z "$RADIUM_NSM_EXECUTABLE_NAME" ] ; then
        export RADIUM_NSM_EXECUTABLE_NAME=$(basename -- "$0")
fi

export LSAN_OPTIONS=suppressions=`pwd`/SanitizerSuppr.txt
export ASAN_OPTIONS="detect_leaks=0,allocator_may_return_null=1,abort_on_error=1,new_delete_type_mismatch=0,alloc_dealloc_mismatch=0,max_malloc_fill_size=1048576,detect_odr_violation=2" # new_delete_type_mismatch=0 because of qt5. alloc_dealloc_mismatch because of various vst plugins. detect_stack_use_after_return=1 sounds nice, but is very slow.
export UBSAN_OPTIONS="print_stacktrace=1:abort_on_error=1"
#suppressions=`pwd`/SanitizerSuppr.txt:
export TSAN_OPTIONS="history_size=7,second_deadlock_stack=1,suppressions=SanitizerSuppr.txt"


THIS_DIR=$(dirname $(readlink -f $0))
XCB_LIB_DIR=$THIS_DIR/bin/packages/libxcb-1.13/src/.libs

if ! file $XCB_LIB_DIR ; then
    echo "Unable to find directory $XCB_LIB_DIR"
    exit -1
fi

# To avoid buggy qt plugins from crashing radium (very common).
unset QT_QPA_PLATFORMTHEME
unset QT_QPA_PLATFORM
unset QT_PLUGIN_PATH

# To avoid freezing X
export USE_SAFE_POPUP="1"

export LD_LIBRARY_PATH=$XCB_LIB_DIR${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}
export G_DEBUG="fatal-warnings,gc-friendly"

#ulimit -s 655360

LD_LIBRARY_PATH=$LD_LIBRARY_PATH G_DEBUG="fatal-warnings,gc-friendly" gdb --args bin/radium_linux.bin $@; killall -9 radium_progress_window ; killall -9 radium_crashreporter

# without gdb:
#LD_LIBRARY_PATH=$LD_LIBRARY_PATH G_DEBUG="fatal-warnings,gc-friendly" bin/radium_linux.bin $@; killall -9 radium_progress_window ; killall -9 radium_crashreporter


# to test crashreporter, comment out the above line, and uncomment the next line:
#LD_LIBRARY_PATH=$LD_LIBRARY_PATH G_DEBUG="fatal-warnings,gc-friendly" bin/radium_linux.bin $@

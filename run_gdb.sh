#!/usr/bin/env bash

if [ -z "$RADIUM_NSM_EXECUTABLE_NAME" ] ; then
        export RADIUM_NSM_EXECUTABLE_NAME=$(basename -- "$0")
fi

export LSAN_OPTIONS=suppressions=`pwd`/SanitizerSuppr.txt
export ASAN_OPTIONS="detect_leaks=0,abort_on_error=1,max_malloc_fill_size=1048576,detect_odr_violation=2,detect_container_overflow=0,suppressions=SanitizerSupprAddr.txt"
#
# earlier we also had these ASAN_OPTIONS:
#
# allocator_may_return_null=1,  # Don't know why it was added...
# new_delete_type_mismatch=0,   # Because of Qt, but seems to have been fixed now.
# alloc_dealloc_mismatch=0,     # Because of various Vst plugins.

export UBSAN_OPTIONS="print_stacktrace=1:abort_on_error=1"
#suppressions=`pwd`/SanitizerSuppr.txt:
export TSAN_OPTIONS="history_size=7,second_deadlock_stack=1,print_stacktrace=1:abort_on_error=1,suppressions=SanitizerSuppr.txt"


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

#DEBUGGER="gdb --args"
DEBUGGER="lldb --"
#DEBUGGER=

unameOut="$(uname -s)"
case "${unameOut}" in
    Linux*)     EXECUTABLE="bin/radium_linux.bin";;
    Darwin*)    EXECUTABLE="/tmp/radium_bin/radium_macos.bin";;
    *)          EXECUTABLE="where_is_radium_for_\"${unameOut}\"?_(change_these_lines_in_run_gdb.sh_to_fix_this)";;
esac



LD_LIBRARY_PATH=$LD_LIBRARY_PATH G_DEBUG="fatal-warnings,gc-friendly" USE=libedit/readline $DEBUGGER $EXECUTABLE $@; killall -9 radium_progress_window ; killall -9 radium_crashreporter

# without gdb:
#LD_LIBRARY_PATH=$LD_LIBRARY_PATH G_DEBUG="fatal-warnings,gc-friendly" bin/radium_linux.bin $@; killall -9 radium_progress_window ; killall -9 radium_crashreporter


# to test crashreporter, comment out the above line, and uncomment the next line:
#LD_LIBRARY_PATH=$LD_LIBRARY_PATH G_DEBUG="fatal-warnings,gc-friendly" bin/radium_linux.bin $@

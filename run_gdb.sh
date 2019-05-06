#!/bin/bash

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

LD_LIBRARY_PATH=$LD_LIBRARY_PATH G_DEBUG="fatal-warnings,gc-friendly" gdb --args bin/radium_linux.bin $@; killall -9 radium_progress_window ; killall -9 radium_crashreporter
# to test crashreporter, comment out the above line, and uncomment the next line:
#LD_LIBRARY_PATH=$LD_LIBRARY_PATH G_DEBUG="fatal-warnings,gc-friendly" bin/radium_linux.bin $@

#!/bin/sh

set -e

if [ -z "$RADIUM_NSM_EXECUTABLE_NAME" ] ; then
        export RADIUM_NSM_EXECUTABLE_NAME=$(basename -- "$0")
fi

THIS_DIR="$(dirname "$(readlink -f "$0")")"

if ! $THIS_DIR/radium_check_recent_libxcb ; then

    XCB_LIB_DIR="$THIS_DIR"/packages/libxcb-1.13/src/.libs
    
    if ! file "$XCB_LIB_DIR" ; then
        echo "Unable to find directory $XCB_LIB_DIR"
        exit -1
    fi
    
    export LD_LIBRARY_PATH="$XCB_LIB_DIR"${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}
    
fi

# To avoid buggy qt plugins crashing radium (very common).
unset QT_QPA_PLATFORMTHEME
unset QT_QPA_PLATFORM
unset QT_PLUGIN_PATH

QT_QPA_PLATFORM="xcb" LD_LIBRARY_PATH="$LD_LIBRARY_PATH" "$THIS_DIR"/radium_linux.bin "$@"

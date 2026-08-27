#!/usr/bin/env bash

set -e

if [ -z "$RADIUM_NSM_EXECUTABLE_NAME" ] ; then
        export RADIUM_NSM_EXECUTABLE_NAME=$(basename -- "$0")
fi

THIS_DIR="$(dirname "$(readlink -f "$0")")"

# Show an error message to the user. A small Xlib-only program is used when
# possible, so that the message is shown even if Qt can't start (for instance
# when the user double-clicks the AppImage icon and a system library such as
# libxcb-cursor is missing). The message is also printed to stdout, so it's
# visible when radium is started from a terminal.
show_error_message() {
    printf '%s\n' "$@"
    if [[ -x "$THIS_DIR"/radium_show_message ]] ; then
        "$THIS_DIR"/radium_show_message "$@" 2>/dev/null || true
    fi
}

# Returns the name of the Debian/Ubuntu package providing library $1.
package_name_for_library() {
    case "$1" in
        libxcb-cursor.so.*)      printf '%s' "libxcb-cursor0" ;;
        libxcb-icccm.so.*)       printf '%s' "libxcb-icccm4" ;;
        libxcb-image.so.*)       printf '%s' "libxcb-image0" ;;
        libxcb-keysyms.so.*)     printf '%s' "libxcb-keysyms1" ;;
        libxcb-render-util.so.*) printf '%s' "libxcb-render-util0" ;;
        libxcb-util.so.*)        printf '%s' "libxcb-util1" ;;
        libxkbcommon-x11.so.*)   printf '%s' "libxkbcommon-x11-0" ;;
        libjack.so.*)            printf '%s' "libjack-jackd2-0" ;;
        *)                       printf '%s' "" ;;
    esac
}

# Sets MISSING_LIBRARIES to the list of libraries that $1 can't find.
find_missing_libraries() {
    local ldd_output
    local ldd_line

    ldd_output="$(QT_QPA_PLATFORM="xcb" LD_LIBRARY_PATH="$LD_LIBRARY_PATH" ldd -r "$1" 2>/dev/null || true)"

    MISSING_LIBRARIES=""

    while IFS= read -r ldd_line ; do
        case "$ldd_line" in
            *"=> not found"*)
                MISSING_LIBRARIES="$MISSING_LIBRARIES $(printf '%s\n' "$ldd_line" | sed 's/^[[:space:]]*\([^[:space:]]*\).*/\1/')"
                ;;
        esac
    done <<< "$ldd_output"
}

if ! $THIS_DIR/radium_check_recent_libxcb ; then

    XCB_LIB_DIR="$THIS_DIR"/packages/libxcb-1.13/src/.libs
    
    if ! file "$XCB_LIB_DIR" ; then
        show_error_message \
            "Radium can't start." \
            "libxcb is missing or too old, and the bundled version couldn't be found here:" \
            "$XCB_LIB_DIR"
        exit -1
    fi
    
    export LD_LIBRARY_PATH="$XCB_LIB_DIR"${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}
    
fi

export LD_LIBRARY_PATH="$THIS_DIR/packages/python27_install/lib:$THIS_DIR/packages/faust/build/lib"${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}

# To avoid buggy qt plugins crashing radium (very common).
unset QT_QPA_PLATFORMTHEME
unset QT_QPA_PLATFORM
unset QT_PLUGIN_PATH

files_to_check=("$THIS_DIR"/radium_linux.bin)

if [[ -n "$QT_QPA_PLATFORM_PLUGIN_PATH" ]] ; then
    files_to_check+=("$QT_QPA_PLATFORM_PLUGIN_PATH"/platforms/libqxcb.so)
fi

for file_to_check in "${files_to_check[@]}" ; do

    if [[ ! -f "$file_to_check" ]] ; then
        continue
    fi

    find_missing_libraries "$file_to_check"

    if [[ -n "$MISSING_LIBRARIES" ]] ; then

        packages=""

        for library in $MISSING_LIBRARIES ; do
            package="$(package_name_for_library "$library")"
            if [[ -n "$package" ]] ; then
                packages="$packages $package"
            fi
        done

        if [[ -n "$packages" ]] ; then
            show_error_message \
                "Radium can't start." \
                "The following system libraries are missing:" \
                "${MISSING_LIBRARIES# }" \
                "" \
                "On Ubuntu or Debian, install them with this command:" \
                "sudo apt install$packages" \
                "" \
                "If you use another distribution, install the packages providing these libraries."
        else
            show_error_message \
                "Radium can't start." \
                "The following system libraries are missing:" \
                "${MISSING_LIBRARIES# }" \
                "" \
                "Please install the missing libraries and try again."
        fi

        exit -1
    fi
done

if [[ -n "$QT_QPA_PLATFORM_PLUGIN_PATH" ]] ; then
	if QT_QPA_PLATFORM="xcb" LD_LIBRARY_PATH="$LD_LIBRARY_PATH" ldd -r "$QT_QPA_PLATFORM_PLUGIN_PATH"/platforms/libqxcb.so 2>/dev/null |grep undefined ; then
        show_error_message \
            "Radium can't start." \
            "Undefined symbols in $QT_QPA_PLATFORM_PLUGIN_PATH/platforms/libqxcb.so." \
            "Please contact info@radium.dog and include the following information:" \
            "$(QT_QPA_PLATFORM="xcb" LD_LIBRARY_PATH="$LD_LIBRARY_PATH" ldd -r "$QT_QPA_PLATFORM_PLUGIN_PATH"/platforms/libqxcb.so 2>/dev/null |grep undefined || true)"
		exit -1
	fi
fi


QT_QPA_PLATFORM="xcb" LD_LIBRARY_PATH="$LD_LIBRARY_PATH" exec "$THIS_DIR"/radium_linux.bin "$@"

#!/usr/bin/env bash

set -eEu
#set -x

THIS_DIR="$(dirname "$(readlink -f "$0")")"


PREFIX=$1

if ! [[ "$PREFIX" = /* ]]; then
    echo "$PREFIX is not an absolute path"
    exit -2
fi

#if [ ! -d "$PREFIX" ]; then
#    echo "Directory $PREFIX does not exist"
#    exit -1
#fi

mkdir -p "$PREFIX"

TARGET="$PREFIX/radium"

if [ -d "$TARGET" ]; then
    echo "Directory $TARGET already exist. Please uninstall program first"
    exit -1
fi


echo $TARGET

mkdir -p "$TARGET"

cd "$THIS_DIR/bin"

can_copy() {
    if [[ "$1" = *"packages"* ]]; then
        return 1 # in bash, 1 is false and 0 is true.
    elif [[ "$1" = *.rad ]]; then
        return 1
    elif [[ "$1" = *.bak ]]; then
        return 1
    elif [[ "$1" = *_audio ]]; then
        return 1
    elif [[ "$1" = *.wav ]]; then
        return 1
    elif [[ "$1" = *.radium_peaks ]]; then
        return 1
    elif [[ "$1" = *.rec ]]; then
        return 1
    elif [[ "$1" = *.mrec ]]; then
        return 1
    elif [[ "$1" = *.orig ]]; then
        return 1
    elif [[ "$1" = *.rej ]]; then
        return 1
    elif [[ "$1" = *~ ]]; then
        return 1
    elif [[ "$1" = *#*# ]]; then
        return 1
    elif [[ "$1" = .#* ]]; then
        return 1
    elif [[ "$1" = .DS_Store ]]; then
        return 1
    elif [[ "$1" = deletemetorebuild ]]; then
        return 1
    else
        return 0
    fi
}

GENERATED_FILES="radium|radium_linux.bin|radium.bin.exe|radium_check_jack_status|radium_check_jack_status.exe|radium_check_recent_libxcb|radium_crashreporter|radium_crashreporter.exe|radium_error_message|radium_error_message.exe|radium_plugin_scanner|radium_plugin_scanner.exe|radium_progress_window|radium_progress_window.exe|radium_show_message|keybindingsparser.pyc|keysubids.pyc|protoconfparser.pyc|color.frag.qsb|color.vert.qsb|texture_fragment.qsb|texture_vertex.qsb|llvm_math.ll|protos.conf"

in_allowlist() {
    local f
    for f in ${GENERATED_FILES//|/ } ; do
        if [[ "$1" = "$f" ]]; then
            return 0
        fi
    done
    return 1
}

# Copy files which are in the git repository.
TMP_FILELIST=/tmp/radium_install_filelist.$$
if ! git ls-files -z > "$TMP_FILELIST"; then
    echo "Unable to list files in the git repository. install.sh must be run from a git checkout of radium."
    exit 1
fi

while IFS= read -r -d '' a; do
    top="${a%%/*}"
    if [[ "$top" = scheme ]] || can_copy "$top"; then
        if test -e "$a" || test -L "$a"; then
            mkdir -p "$TARGET/$(dirname "$a")"
            cp -a "$a" "$TARGET/$a"
        fi
    fi
done < "$TMP_FILELIST"

rm -f "$TMP_FILELIST"

# Copy known generated files.
for a in ${GENERATED_FILES//|/ } ; do
    if test -e "$a"; then
        cp -a "$a" "$TARGET/"
    fi
done

if test -f /tmp/radium_bin/radium_linux.bin; then
    rm -f "$TARGET/radium_linux.bin"
    cp -f /tmp/radium_bin/radium_linux.bin "$TARGET/"
fi

mkdir -p "$TARGET/packages"

# s7
cp -a packages/s7 "$TARGET/packages/"
rm -f "$TARGET"/packages/s7/*.o

# Remove files only needed when building s7. Only the .scm files (and s7 webserver files)
# are used at runtime.
rm -fr "$TARGET/packages/s7/s7webserver_org"
#rm -fr "$TARGET/packages/s7/tools"
#rm -f "$TARGET/packages/s7/s7test.scm"
#rm -f "$TARGET/packages/s7/lint.scm"
#rm -f "$TARGET/packages/s7/snd-lint.scm"
rm -f "$TARGET/packages/s7/s7.c"
#rm -f "$TARGET/packages/s7/s7.h"
#rm -f "$TARGET/packages/s7/s7.html"
rm -f "$TARGET/packages/s7"/*.c
#rm -f "$TARGET/packages/s7"/*.h
# rm -fr "$TARGET/packages/s7/sndlib"

# faust
mkdir -p "$TARGET/packages/faust"
mkdir -p "$TARGET/packages/faust/build"
cp -a packages/faust/build/lib "$TARGET/packages/faust/build/"
cp -a packages/faust/examples "$TARGET/packages/faust/"
mkdir -p "$TARGET/packages/faust/architecture/faust/gui"
cp -a packages/faust/architecture/faust/gui/Styles "$TARGET/packages/faust/architecture/faust/gui/"
cp -a packages/faust/libraries "$TARGET/packages/faust/"
rm -fr "$TARGET/packages/faust/libraries/.git"

rm -fr "$TARGET/python-midi/src/sequencer_osx"



# pure data
cp -a packages/libpd-master "$TARGET/packages/"
cd "$TARGET/packages/libpd-master"
make clean
rm -f libpds.o
cd "$THIS_DIR/bin"

echo "A1"
# ladspa
if [ ! -d "$TARGET/ladspa" ] && [ ! -L "$TARGET/ladspa" ] 
then
    echo "A2"
    mkdir $TARGET/ladspa
fi

# libxcb
if uname -s |grep Linux ; then
    if [ -v  RADIUM_INSTALL_LIBXCB ] && [[ $RADIUM_INSTALL_LIBXCB != 0 ]]
    then
	cp -a packages/libxcb-1.13 "$TARGET/packages/"
	cd "$TARGET/packages/libxcb-1.13/src"
	rm -f ./*.o
	cd "$THIS_DIR/bin"
    fi
fi


# python27
cp -a packages/python27_install  "$TARGET/packages/"

# Slim down the embedded python installation. Radium only uses a small part of the
# standard library, so remove bytecode, the test suite, and unneeded modules.
find "$TARGET/packages/python27_install" \( -name '*.pyc' -o -name '*.pyo' \) -delete
rm -fr "$TARGET/packages/python27_install/include"
PY27LIB="$TARGET/packages/python27_install/lib/python2.7"
for d in test config plat-mac distutils idlelib lib-tk lib2to3 ensurepip pydoc_data email unittest bsddb multiprocessing compiler wsgiref curses hotshot sqlite3; do
    rm -fr "$PY27LIB/$d"
done
for f in _testcapi.so _sqlite3_failed.so _tkinter.so readline.so _bsddb.so _curses.so _curses_panel.so _hotshot.so audioop.so; do
    rm -f "$PY27LIB/lib-dynload/$f"
done

# Remove files not needed at runtime.
rm -fr "$TARGET/graphics/macosx_iconset"
rm -f "$TARGET/graphics/Radium.icns"
rm -f "$TARGET/graphics/radium_logo_colorized.ico"
rm -fr "$TARGET/help/old"

# Remove accidentally included files (patch artifacts, editor backups, etc.).
# Note: Only remove regular files, not directories, since pure data objects are commonly
# named with a trailing "~", and they are often stored in directories with that name as well.
find "$TARGET" -type f \( -name '*.orig' -o -name '*.rej' -o -name '*~' -o -name '*.bak' \
                        -o -name '#*#' -o -name '.#*' -o -name '.DS_Store' \) -delete

# Collect files in bin/ which were not copied because they are neither in the git repository
# nor known generated files.
excluded_files=""
while IFS= read -r -d '' a; do
    a="${a#./}"
    top="${a%%/*}"
    if [[ "$top" = scheme ]] || can_copy "$top"; then
        if ! in_allowlist "$a" && ! git ls-files --error-unmatch "$a" >/dev/null 2>&1; then
            excluded_files+="$a"$'\n'
        fi
    fi
done < <(find . -path ./packages -prune -o \( -type f -o -type l \) -print0)

if [[ -n "$excluded_files" ]]; then
    while [[ "$excluded_files" == *$'\n' ]]; do
        excluded_files="${excluded_files%$'\n'}"
    done
    echo "Files not included because they are not in the whitelist (git repository + known generated files):"
    RED="$(tput setaf 1 2>/dev/null || printf '\033[31m')"
    RESET="$(tput sgr0 2>/dev/null || printf '\033[0m')"
    while IFS= read -r line; do
        printf '%s%s%s\n' "$RED" "$line" "$RESET"
    done <<< "$excluded_files"
fi

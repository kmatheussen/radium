#!/bin/bash

set -e
set -x

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
    else
        return 0
    fi
}

for a in * ; do
    if can_copy "$a"; then
        cp -a "$a" "$TARGET/"
    fi
done

mkdir -p "$TARGET/packages"

# s7
cp -a packages/s7 "$TARGET/packages/"
rm -f "$TARGET/packages/s7/*.o"
# rm -fr "$TARGET/packages/s7/sndlib"

# faust
mkdir -p "$TARGET/packages/faust2"
cp -a packages/faust2/examples "$TARGET/packages/faust2/"
cp -a packages/faust2/architecture "$TARGET/packages/faust2/"
rm -fr "$TARGET/packages/faust2/architecture/webaudio"
rm -fr "$TARGET/packages/faust2/architecture/osclib"

# pure data
cp -a packages/libpd-master "$TARGET/packages/"
cd "$TARGET/packages/libpd-master"
make clean
rm -f libpds.o
cd "$THIS_DIR/bin"

# libxcb
cp -a packages/libxcb-1.13 "$TARGET/packages/"
cd "$TARGET/packages/libxcb-1.13/src"
rm -f *.o
cd "$THIS_DIR/bin"

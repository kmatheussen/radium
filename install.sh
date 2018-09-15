#!/bin/bash

set -e
set -x

PREFIX=$1

#if [ ! -d "$PREFIX" ]; then
#    echo "Directory $PREFIX does not exist"
#    exit -1
#fi

mkdir -p "$PREFIX"

TARGET=$1/radium

if [ -d "$TARGET" ]; then
    echo "Directory $TARGET already exist. Please uninstall program first"
    exit -1
fi


echo $TARGET

mkdir -p "$TARGET"

cd bin
for a in * ; do
    if ! [[ "$a" = "packages" ]]; then
        cp -a "$a" "$TARGET/"
    fi
done

mkdir -p "$TARGET/packages"

# s7
cp -a packages/s7 "$TARGET/packages/"
rm -f "$TARGET/packages/s7/*.o"
rm -fr "$TARGET/packages/s7/sndlib"

# faust
mkdir -p "$TARGET/packages/faust2"
cp -a packages/faust2/examples "$TARGET/packages/faust2/"
cp -a packages/faust2/architecture "$TARGET/packages/faust2/"

# pure data
cp -a packages/libpd-master "$TARGET/packages/"
cd "$TARGET/packages/libpd-master"
make clean
rm -f libpds.o



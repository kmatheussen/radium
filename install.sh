#!/bin/bash

set -e

PREFIX=$1

TARGET=$1/radium

echo $TARGET

mkdir -p $TARGET

cd bin
for a in * ; do
    if ! [[ "$a" = "packages" ]]; then
        cp -a "$a" "$TARGET/"
    fi
done

mkdir -p "$TARGET/packages"

cp -a packages/s7 "$TARGET/packages/"
rm -f "$TARGET/packages/s7/*.o"
rm -fr "$TARGET/packages/s7/sndlib"

mkdir -p "$TARGET/packages/faust2"
cp -a packages/faust2/examples "$TARGET/packages/faust2/"
cp -a packages/faust2/architecture "$TARGET/packages/faust2/"

cp -a packages/libpd-master "$TARGET/packages/"
cd "$TARGET/packages/libpd-master"
make clean
rm -f libpds.o



#!/bin/sh

# Based on a comment from Akos Cz at http://stackoverflow.com/questions/4677044/how-to-use-dylib-in-mac-os-x-c
DYLIBS=`darwinx-otool -L $1 | grep "Qt" | awk -F' ' '{ print $1 }'`
DYLIBS="$DYLIBS `darwinx-otool -L $1 | grep "png" | awk -F' ' '{ print $1 }'`"
#for dylib in $DYLIBS; do cp $dylib bin/; done;
for dylib in $DYLIBS; do darwinx-install_name_tool -change $dylib \@executable_path/`basename $dylib` $1; done;

darwinx-otool -L $1

echo "compilation finished"

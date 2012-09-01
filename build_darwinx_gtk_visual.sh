#!/bin/sh

echo "Warning, embedding gtk widgets in qt on macosx didn't work very well,"
echo "plus that it's so insanely complicated to distribute executables which use pango fonts (look at gimp and ardour for instance),"
echo "that gtk for macosx has been abandoned."
echo
echo "This file is not maintained."

export VISUAL="-DUSE_QT_VISUAL=1 -DUSE_GTK_VISUAL=0"

export GTK_CFLAGS="`darwinx-pkg-config --cflags gtk+-2.0`"
export GTK_LDFLAGS="`darwinx-pkg-config --static --libs gtk+-2.0`"

export OS_LDFLAGS="cocoa_embed.o $OS_LDFLAGS"

$CC -c -Wall $GTK_CFLAGS macosx/cocoa_embed.m

./build_darwinx_common.sh $@



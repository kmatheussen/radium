#!/bin/sh

echo "Warning, embedding gtk widgets in qt on macosx didn't work very well,"
echo "plus that it's so insanely complicated to distribute executable binaries which uses pango fonts (look at gimp and ardour for instance),"
echo "that gtk for macosx has been disbanded."
echo
echo "This file is not maintained."

export OS_LDFLAGS="cocoa_embed.o $OS_LDFLAGS"

$CC -c -Wall $GTK_CFLAGS macosx/cocoa_embed.m

./build_darwinx_common.sh $@



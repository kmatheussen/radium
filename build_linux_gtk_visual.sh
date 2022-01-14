#!/usr/bin/env bash

export VISUAL="-DUSE_QT_VISUAL=0 -DUSE_GTK_VISUAL=1"

export GTK_CFLAGS="`pkg-config --cflags gtk+-2.0`"
export GTK_LDFLAGS="`pkg-config --libs gtk+-2.0`"

./build_linux_common.sh $@

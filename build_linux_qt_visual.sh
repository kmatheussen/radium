#!/bin/sh

export VISUAL="-DUSE_QT_VISUAL=1 -DUSE_GTK_VISUAL=0"

export GTK_CFLAGS=""
export GTK_LDFLAGS=""

./build_linux_common.sh $@

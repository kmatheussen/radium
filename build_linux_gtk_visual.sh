#!/bin/sh

export VISUAL="-DUSE_QT_VISUAL=0 -DUSE_GTK_VISUAL=1"

./build_linux_common.sh $@

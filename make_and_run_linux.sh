#!/bin/bash

# This is the command I use to run when developing. -Kjetil

#VL_DATA_PATH=/home/kjetil/Visualization-Library/data BUILDTYPE=DEBUG ./build_linux.sh -j7 &&
BUILDTYPE=DEBUG VL_DATA_PATH=/home/kjetil/Visualization-Library/data ./build_linux.sh -j7 && G_DEBUG=fatal-criticals QT_FATAL_WARNINGS=1 gdb bin/radium

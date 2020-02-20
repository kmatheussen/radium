#!/bin/bash

mkdir -p /tmp/radium_objects

for var in "$@"; do
    if [ -z ${var##*.c} ]; then
        sourcefile=$var
        base=${var##*/}
        base=${base%.*}
        #echo "YES1: $var"
        break
    fi
    if [ -z ${var##*.cpp} ]; then
        sourcefile=$var
        base=${var##*/}
        base=${base%.*}
        #echo "YES2: $var"
        break
    fi
done

#echo "base:" $base
#echo "command:" $@ -o /tmp/$base.o

GREEN='\033[0;32m'
LIGHT_CYAN='\033[0;36m'
NC='\033[0m'
printf "${GREEN}Compiling${LIGHT_CYAN} $sourcefile${NC}\n"

"$@" -o /tmp/radium_objects/$base.o

#!/usr/bin/env bash

set -eo pipefail

mkdir -p $T

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
    if [ -z ${var##*.m} ]; then
        sourcefile=$var
        base=${var##*/}
        base=${base%.*}
        #echo "YES2: $var"
        break
    fi
    if [ -z ${var##*.mm} ]; then
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
YELLOW='\033[0;33m'
NC='\033[0m'
printf "${GREEN}Compiling${LIGHT_CYAN} $sourcefile${NC}...\n"

start=`date +%s`
"$@" -o $T/$base.o
end=`date +%s`

runtime=$((end-start))

printf "          ${LIGHT_CYAN}$sourcefile ${GREEN} compiled.${YELLOW} (${runtime}s)${NC}\n"

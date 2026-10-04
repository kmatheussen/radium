#!/usr/bin/env bash

set -eEu

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
cd "$SCRIPT_DIR"

source configuration.sh

STD_CPP=-std=gnu++2a

if [[ $RADIUM_USE_CLANG == 1 ]] ; then
    export CC=clang
    export CCC=clang++
else
    export CC=gcc
    export CCC=g++
fi

# (Pretend the precompiled headers are up to date. The tests don't use them, and the environment needed to build them is only set up by the build scripts.)
make \
     -o Qt/Qt_precompiled.hpp.gch \
     -o Qt/Qt_precompiled.hpp.d \
     -o audio/Faust_plugins_precompiled.hpp.gch \
     -o audio/Faust_plugins_precompiled.hpp.d \
     test "$@"

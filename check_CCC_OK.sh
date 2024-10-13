#!/usr/bin/env bash

source configuration.sh

if [[ $RADIUM_USE_CLANG == 1 ]] ; then
    export FULL_CCC_PATH_NOW=`which clang++`
else
    export FULL_CCC_PATH_NOW=`which g++`
fi

if [[ "$FULL_CCC_PATH" != "$FULL_CCC_PATH_NOW" ]] ; then
    echo
    echo
    echo "Something is wrong. The path to g++ or clang++ is not what it's supposed to be:"
    echo "Supposed: \"${FULL_CCC_PATH}\""
    echo "Actual: \"${FULL_CCC_PATH_NOW}\""
    echo
    
    rm -f touch CCC_OK.touch
    exit -1
fi

touch CCC_OK.touch

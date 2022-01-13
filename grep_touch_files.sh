#!/usr/bin/env bash

set -e

git grep "$1" | while read -r line ; do
    echo `awk -F: '{print $1}'`
done

git grep "$1" | while read -r line ; do
    #cut -d: -f1 $line
    touch `awk -F: '{print $1}'`
done

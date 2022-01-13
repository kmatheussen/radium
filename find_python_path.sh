#!/usr/bin/env bash

if which python2 >/dev/null 2>/dev/null ; then
    which python2
elif which python >/dev/null 2>/dev/null ; then
    which python
else
    echo "<<<python_not_found>>>"
fi

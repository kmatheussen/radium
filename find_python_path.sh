#!/usr/bin/env bash

#source $(dirname "${0}")/bash_setup.sh

echo $(readlink -f $(dirname "${0}"))/bin/packages/python27_install/bin/python
exit 0

if which python2 >/dev/null 2>/dev/null ; then
    which python2
elif which python >/dev/null 2>/dev/null ; then
    which python
else
    echo "<<<python_not_found>>>"
fi

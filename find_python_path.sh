#!/usr/bin/env bash

#source $(dirname "${0}")/bash_setup.sh

# Note: Run-scripts have not been updated to use
# other pythons than the one included with Radium,
# so setting this value to something else might
# lead to unexpected behaviors.
#
echo $(readlink -f $(dirname "${0}"))/bin/packages/python27_install/bin/python
exit 0

if which python2 >/dev/null 2>/dev/null ; then
    which python2
elif which python >/dev/null 2>/dev/null ; then
    which python
else
    echo "<<<python_not_found>>>"
fi

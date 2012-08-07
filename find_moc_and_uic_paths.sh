#!/bin/bash

if [ -f /etc/fedora-release ] ; then
    if uname -a |grep x86_64 >/dev/null ; then
        PATH=/usr/lib64/qt-3.3/bin/
    else
        PATH=/usr/lib/qt-3.3/bin/
    fi

elif grep -i ubuntu /etc/lsb-release >/dev/null ; then
    PATH=/usr/share/qt3/bin/

elif grep -i debian /etc/lsb-release >/dev/null ; then
    PATH=/usr/share/qt3/bin/

elif grep -i arch /etc/lsb-release >/dev/null ; then
    PATH=/opt/qt/bin/

else
    PATH=

fi

echo $PATH$1



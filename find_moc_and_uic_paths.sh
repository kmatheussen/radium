#!/bin/bash

if [ /etc/fedora-release ] ; then
    if uname -a |grep x86_64 >/dev/null ; then
        PATH=/usr/lib64/qt-3.3/bin/
    else
        PATH=/usr/lib/qt-3.3/bin/
    fi
elif [ /etc/ubuntu-release ] ; then
    PATH=/usr/share/qt3/bin/
elif [ /etc/debian-release ] ; then
    PATH=/usr/share/qt3/bin/
elif [ /etc/arch-release ] ; then
    PATH=/opt/qt/bin/
else
    PATH=
fi

echo $PATH$1



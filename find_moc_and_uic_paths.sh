#!/bin/bash

if which $1 >/dev/null 2>/dev/null ; then
    if $1 -v 2>&1 | grep Qt\ 4 >/dev/null ; then
        echo $1
        exit 0
    elif $1 -v 2>&1 | grep version\ 4 >/dev/null ; then
        echo $1
        exit 0
    fi
fi


if [ -f /etc/fedora-release ] ; then
    if uname -a |grep x86_64 >/dev/null ; then
        echo /usr/bin/$1-qt4
    else
        echo /usr/bin/$1-qt4
    fi

elif grep -i ubuntu /etc/lsb-release >/dev/null 2>/dev/null ; then
    echo $1-qt4

elif grep -i debian /etc/lsb-release >/dev/null 2>/dev/null ; then
    echo $1-qt4

elif grep -i mint /etc/lsb-release >/dev/null 2>/dev/null ; then
    echo $1-qt4

elif grep -i arch /etc/lsb-release >/dev/null 2>/dev/null ; then
    echo $1

else
    echo $1
fi

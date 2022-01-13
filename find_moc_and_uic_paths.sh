#!/usr/bin/env bash

if which $1 >/dev/null 2>/dev/null ; then
    if $1 -v 2>&1 | grep Qt\ $RADIUM_QT_VERSION >/dev/null ; then
        echo `which $1`
        exit 0
    elif $1 -v 2>&1 | grep version\ $RADIUM_QT_VERSION >/dev/null ; then
        echo `which $1`
        exit 0
    elif $1 -v 2>&1 | grep qt$RADIUM_QT_VERSION >/dev/null ; then
        echo `which $1`
        exit 0
    elif $1 -v 2>&1 | grep $1\ $RADIUM_QT_VERSION >/dev/null ; then
        echo `which $1`
        exit 0
    fi
fi

if which $1-qt$RADIUM_QT_VERSION >/dev/null 2>/dev/null ; then
    echo `which $1-qt$RADIUM_QT_VERSION`
    exit 0
fi

if [ -f /etc/fedora-release ] ; then
    if uname -a |grep x86_64 >/dev/null ; then
        echo /usr/bin/$1-qt$RADIUM_QT_VERSION
    else
        echo /usr/bin/$1-qt$RADIUM_QT_VERSION
    fi

elif grep -i ubuntu /etc/lsb-release >/dev/null 2>/dev/null ; then
    echo $1-qt$RADIUM_QT_VERSION

elif grep -i debian /etc/lsb-release >/dev/null 2>/dev/null ; then
    echo $1-qt$RADIUM_QT_VERSION

elif grep -i mint /etc/lsb-release >/dev/null 2>/dev/null ; then
    echo $1-qt$RADIUM_QT_VERSION

elif grep -i arch /etc/lsb-release >/dev/null 2>/dev/null ; then
    echo $1-qt$RADIUM_QT_VERSION

else
    echo $1-qt$RADIUM_QT_VERSION
fi

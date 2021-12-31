#!/bin/bash

# X often freezes if a program crashes while a popup menu is open. (at least when running under gdb)


echo $$ >/tmp/$1

sleep $2

if ps -Af|grep $3|grep radium_linux.bin ; then

    if [ -f /tmp/$1 ] ; then
        echo "killing it: $1 $2 $3"
        kill -9 $3
    fi

fi

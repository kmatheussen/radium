#!/bin/bash

echo $$ >/tmp/$1

sleep $2

if [ -f /tmp/$1 ] ; then
    echo "killing it: $1 $2 $3"
    kill -9 $3
fi


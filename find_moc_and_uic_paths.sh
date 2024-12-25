#!/usr/bin/env bash

set -ueE
#set -x

source $(dirname "${0}")/configuration.sh

case "$1" in
    qmake)
        echo $QMAKE
        exit 0
        ;;

    moc)
        echo $MOC
        exit 0
        ;;  
    uic)
        echo $UIC
        exit 0
        ;;  
esac

echo "Argument must be qmake, moc, or uic. Found $1"
exit -1

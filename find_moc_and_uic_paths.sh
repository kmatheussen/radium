#!/bin/bash


#distro finding script copied from http://legroom.net/2010/05/05/generic-method-determine-linux-or-unix-distribution-name

# Determine OS platform
UNAME=$(uname | tr "[:upper:]" "[:lower:]")
# If Linux, try to determine specific distribution

if [ "$UNAME" == "linux" ] ; then
    # If available, use LSB to identify distribution
    if [ -f /etc/lsb-release -o -d /etc/lsb-release.d ]; then
        export DISTRO=$(lsb_release -i | cut -d: -f2 | sed s/'^\t'//)
    # Otherwise, use release info file
    else
        export DISTRO=$(ls -d /etc/[A-Za-z]*[_-][rv]e[lr]* | grep -v "lsb" | cut -d'/' -f3 | cut -d'-' -f1 | cut -d'_' -f1)
    fi
fi
# For everything else (or if above failed), just use generic identifier
[ "$DISTRO" == "" ] && export DISTRO=$UNAME
unset UNAME

#echo $DISTRO

if [ $DISTRO = Ubuntu ] ; then
    PATH=/usr/share/qt3/bin/
elif [ $DISTRO = Debian ] ; then
    PATH=/usr/share/qt3/bin/
elif [ $DISTRO = Fedora ] ; then
    PATH=/usr/lib/qt-3.3/bin/
elif [ $DISTRO = Arch ] ; then
    PATH=/opt/qt/bin/
fi

echo $PATH$1



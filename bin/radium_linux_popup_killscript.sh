#!/bin/bash

# X often freezes if a program crashes while a popup menu is open. (at least when running under gdb)


echo $$ >/tmp/$1

sleep $2
#sleep 2

if ps -Af|grep $3|grep radium_linux.bin ; then

    if [ -f /tmp/$1 ] ; then
		statusfile=$(mktemp)
		
		xterm -e 'dialog --timeout 10 --title "Kill Radium?" --backtitle "A menu has been open for $2 seconds. Locked?" --yesno "Press Yes to kill radium. (Will kill radium in 10 seconds if no response)." 7 60 ; echo $? > '$statusfile

		response=$(cat $statusfile)
		rm $statusfile

		echo "response: $response"
		
		case $response in
			1)
				echo "Pressed no"
				exit
		esac

        echo "killing it: $1 $2 $3"
        kill -9 $3
    fi

fi

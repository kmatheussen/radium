#!/usr/bin/env bash

set -eEu
#set -x

list=""
counter=0

push_back() {
    list="$list STARTGAKK $1 ENDGAKK "
    : $((counter++))
}

contains() {
    if echo "$list" | grep " STARTGAKK $1 ENDGAKK " >/dev/null; then
	#echo "contains -$1- in -$list-"
	#echo "----"
	true
    else
	#echo "NOT contains $1 in -$list-"
	#echo "----"
	false
    fi
}

add_include() {

    #echo "1: -$1-"
    
    while read -r line ; do
	local fullpath=$(echo "$line" | awk -F: '{print $1}')
	
	if ! contains $fullpath ; then
	    
	    local filename=$(basename $fullpath)
	
	    push_back $fullpath

	    #echo $filename
	    echo "$fullpath (contains $1)"
	    
	    touch $fullpath

	    add_include $filename
	fi
    done < <(git grep -P "include(?s)(.*)(\"|\>|\/)$1(\"|\>)")
}

echo

for a in $@ ; do
    add_include $a
done

echo
echo "=== Finished. Touched $counter files ==="


#git grep "$1" | while read -r line ; do
##    #cut -d: -f1 $line
#    touch `awk -F: '{print $1}'`
#done

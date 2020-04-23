#!/bin/bash

endswith(){
    case $2 in *"$1") true;; *) false;; esac;
}

cat frame_template1.html >$1_framed.html

cat $1.html | while IFS= read line; do
    if [[ $line != \<html* ]] && [[ $line != \<\/html* ]] && [[ $line != \<body* ]] && [[ $line != \<\/body* ]]; then
        echo "$line" >>$1_framed.html
    fi
done

cat frame_template2.html | while IFS= read line; do
    
    if [[ $line == *maybeactive* ]]  && [[ $line == *\"$1_framed.html\"* ]] ; then
        echo "$line" | sed s/maybeactive/active/ >>$1_framed.html

    elif [[ $line == *maybeul* ]] ; then

        name=`echo $line | sed '/.*\"\(.*\)\".*/ s//\1/g'`

        if endswith $name $1_framed ; then
            echo "<ul>" >>$1_framed.html
        else
            echo "<ul hidden>" >>$1_framed.html
        fi
        
    else
        echo "$line" >>$1_framed.html
    fi
    
done


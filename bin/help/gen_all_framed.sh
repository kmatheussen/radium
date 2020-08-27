#!/bin/bash

for filename in *html; do
    if [[ $filename != *framed.html ]] && [[ $filename != *template* ]] && [[ $filename != *include.html ]] ; then
        echo "Generating ${filename%.*}_framed.html"
        ./gen_framed.sh "${filename%.*}"
    fi
done


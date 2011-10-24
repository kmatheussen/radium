#!/bin/sh

sh link_tmp_radium.sh

cd bin
#/usr/bin/givertcap
python start.py keybindings.conf $@


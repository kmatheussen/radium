#! /usr/bin/env python

#Written by Kjetil Matheussen: k.s.matheussen@notam02.no


import sys
import os
import urllib2
import readline

executable_path = os.path.split(os.path.abspath(os.path.realpath(sys.argv[0])))[0]

# TODO: Use bin/packages/s7/s7webserver instead, and delete the local s7webserver directory.
sys.path += [os.path.join(executable_path,os.path.pardir,"s7webserver")]

import s7webserver_repl

portnum = None
if len(sys.argv)>1:
    arg = sys.argv[1]
    if os.path.isfile(arg):
        with open(arg) as f:
            portnum = f.read().strip()
    else:
        portnum = arg
else:
    port_file = os.environ.get("RADIUM_CONTROL_PORT_FILE")
    if port_file and os.path.isfile(port_file):
        with open(port_file) as f:
            portnum = f.read().strip()

if not portnum:
    portnum = "5080"

s7webserver_repl.start("radium>", "http://localhost:"+portnum)

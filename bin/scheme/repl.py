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

portnum = "5080"
if len(sys.argv)>1:
    portnum = sys.argv[1]
    
s7webserver_repl.start("radium>", "http://localhost:"+portnum)

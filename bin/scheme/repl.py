#! /usr/bin/env python

#Written by Kjetil Matheussen: k.s.matheussen@notam02.no


import sys
import os
import urllib2
import readline

executable_path = os.path.split(os.path.abspath(os.path.realpath(sys.argv[0])))[0]
 
sys.path += [os.path.join(executable_path,os.path.pardir,"s7webserver")]

import s7webserver_repl

s7webserver_repl.start("radium>", "http://localhost:5080")


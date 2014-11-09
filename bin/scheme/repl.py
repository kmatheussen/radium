#! /usr/bin/env python

#Written by Kjetil Matheussen: k.s.matheussen@notam02.no


import sys
import os
import urllib2
import readline


sys.path += [os.path.join(os.path.dirname(sys.argv[0]),os.path.pardir,"s7webserver")]

import s7webserver_repl

s7webserver_repl.start("radium>", "http://localhost:5080")


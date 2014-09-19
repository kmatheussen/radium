#! /usr/bin/env python

import sys
import urllib2
import readline

headers = {"Content-type": "text/plain", "Accept": "text/plain"}


def post(data):
    request = urllib2.Request("http://localhost:5080/user/gakkgakk", data, headers)
    response = urllib2.urlopen(request)

    data = response.read(1)
    while data:
        sys.stdout.write( '%s' % data )
        sys.stdout.flush()
        data = response.read(1)
    response.close()
    print


while True:
    try:
        line = raw_input("radium> ")
        post(line+"\n")
    except EOFError:
        break


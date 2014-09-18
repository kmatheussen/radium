#! /usr/bin/env python

import sys
import httplib,urllib2
import readline

headers = {"Content-type": "text/plain", "Accept": "text/plain"}
conn = httplib.HTTPConnection("localhost:5080")


def post(data):
    request = urllib2.Request("http://localhost:5080/user/gakkgakk", data, headers)
    response = urllib2.urlopen(request)

    data = response.read(1)
    while data:
        sys.stdout.write( '%s' % data )
        sys.stdout.flush()
        data = response.read(1)
    print


while True:
    try:
        line = raw_input("radium> ")
        post(line)
    except EOFError:
        break

conn.close()

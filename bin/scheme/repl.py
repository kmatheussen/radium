#! /usr/bin/env python

import sys
import urllib2
import readline

headers = {"Content-type": "text/plain", "Accept": "text/plain"}


def post(data):
    request = urllib2.Request("http://localhost:5080/user/gakkgakk", data, headers)
    response = urllib2.urlopen(request)

    all_data = ""
    
    data = response.read(1)
    while data:
        all_data = all_data + data
        sys.stdout.write( '%s' % data )
        sys.stdout.flush()
        data = response.read(1)
    response.close()
    print

    return all_data

line = raw_input("radium> ")
while True:
    try:
        result = post(line)
        if result==" ":
            line = raw_input("")
        else:
            line = raw_input("radium> ")
    except EOFError:
        break


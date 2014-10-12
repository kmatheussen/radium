#! /usr/bin/env python

#Written by Kjetil Matheussen: k.s.matheussen@notam02.no


import sys
import urllib2
import readline

headers = {"Content-type": "text/plain", "Accept": "text/plain"}

if len(sys.argv)>1:
    if (sys.argv[1].startswith("-")):
        print "Usage: s7repl <url>"
        print "       Default value for <url> is http://localhost:5080"
        sys.exit(0)
    url = sys.argv[1]
else:
    url = "http://localhost:5080/user/asdf"
    
def post(data):
    request = urllib2.Request(url, data, headers)
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


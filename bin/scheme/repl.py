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
    try:
        response = urllib2.urlopen(request)
    except urllib2.URLError:
        print "<Unable to contact Radium>"
        return

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

def get_input(prompt):
    try:
        return raw_input(prompt)
    except EOFError:
        sys.exit(0)

line = get_input("radium> ")
while True:
    result = post(line)
    if result==" ":
        line = get_input("")
    else:
        line = get_input("radium> ")
    

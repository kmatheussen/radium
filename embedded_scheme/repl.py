#! /usr/bin/env python

import sys
import httplib        
import readline

headers = {"Content-type": "text/plain", "Accept": "text/plain"}
conn = httplib.HTTPConnection("localhost:5080")

def post(data):
    conn.request("POST", "/user/gakkgakk", data, headers)
    response = conn.getresponse()

    print response.status, response.reason

    data = response.read()
    print(data) 

while 1:
    line = raw_input("radium> ")
    post(line)

conn.close()

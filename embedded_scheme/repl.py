#! /usr/bin/env python

import sys
import httplib        

headers = {"Content-type": "text/plain", "Accept": "text/plain"}
conn = httplib.HTTPConnection("localhost:5080")

def post(data):
    conn.request("POST", "/user/gakkgakk", data, headers)
    response = conn.getresponse()

    print response.status, response.reason

    data = response.read()
    print(data) 

line = sys.stdin.readline()
while line:
    post(line)
    line = sys.stdin.readline()

conn.close()

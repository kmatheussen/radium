#! /usr/bin/env python

# Written by Kjetil Matheussen: k.s.matheussen@notam02.no


import sys
import urllib2
import readline

headers = {"Content-type": "text/plain", "Accept": "text/plain"}

def post(url, data):
    request = urllib2.Request(url, data, headers)
    try:
        response = urllib2.urlopen(request)
    except urllib2.URLError:
        print("<Unable to contact Radium>")
        return

    all_data = ""

    data = response.read(1)
    while data:
        all_data = all_data + data
        sys.stdout.write( '%s' % data )
        sys.stdout.flush()
        data = response.read(1)
    response.close()

    return all_data


def get_input(prompt):
    try:
        return raw_input(prompt)
    except EOFError:
        sys.exit(0)


def start(prompt, url):
    line = get_input(prompt+" ")
    while True:
        result = post(url, line)
        if result=="":
            line = get_input("")
        else:
            print
            line = get_input(prompt+" ")


if __name__ == "__main__":

    prompt = "s7> "
    url = "http://localhost:5080"

    if len(sys.argv)>1:
        if (sys.argv[1].startswith("-")):
            print("Usage: s7repl <prompt> <url>")
            print("       Default value for <prompt> is \"s7> \"")
            print("       Default value for <url> is http://localhost:5080")
            sys.exit(0)
        prompt = sys.argv[1]

    if len(sys.argv)>2:
        url = sys.argv[2]

    start(prompt, url)

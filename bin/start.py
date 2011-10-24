#/* Copyright 2001 Kjetil S. Matheussen
#
#This program is free software; you can redistribute it and/or
#modify it under the terms of the GNU General Public License
#as published by the Free Software Foundation; either version 2
#of the License, or (at your option) any later version.
#
#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.
#
#You should have received a copy of the GNU General Public License
#along with this program; if not, write to the Free Software
#Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. */




import radium,keybindingsparser,sys,traceback

from common import *

ra=radium

class KeyHandler:

    def __init__(self):
        self.keyslist=[]
        self.handlers=[]

    def addHandle(self,keys,handle):

        for lokke in range(len(self.keyslist)):
            if self.keyslist[lokke]==keys:
                return false

        self.keyslist.append(keys)
        self.handlers.append(handle)
        return true
            
# keys is a constant!
    def exe(self,windownum,keys):
        for lokke in range(len(self.keyslist)):
            if self.keyslist[lokke]==keys:
                try:
                    eval(self.handlers[lokke])
                except:
                    traceback.print_exc(file=sys.stdout)
                break
        return


def getKeyHandler(num):
    return KeyHandler()

keyhandles=map(getKeyHandler,range(len(keybindingsparser.keysub)))

# key and keys are constants!
def gotKey(windownum,key,keys):
#    print "key: %d, keys: %s" % (key,keys)
#    key=keys.pop(0)
    keyhandles[key].exe(windownum,keys);    


try:
    infilehandle=open(sys.argv[1],'r')
except:
    print "Cant open %s" % sys.argv[1]
    sys.exit(1)

try:
    outfilehandle=open("eventreceiverparser_generated.py",'w+')
except:
    print "Cant open file for writing"
    sys.exit(2)

outfilehandle.write("#Do not edit. This file is automaticly generated from keybindings.config.\n")


print "Parsing keybindings.conf..."
#profile.run("KeyConfer(infilehandle,outfilehandle)","fooprof")
if keybindingsparser.start(keyhandles,infilehandle,outfilehandle,)==false:
    sys.exit(5)

try:
    outfilehandle.close()
except:
    print "Could not close file. Out of disk space?"
    sys.exit(3)


import eventreceiverparser_generated

#import os
#os.system("/usr/bin/givertcap")

if len(sys.argv)>2:
    ra.init_radium(sys.argv[2],gotKey)
else:
    ra.init_radium("",gotKey)

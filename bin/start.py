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

import sys,os
print sys

print sys.argv[0]
print "program path:",sys.g_program_path
print "sys.path:",sys.path

sys.path = [os.path.join(os.path.expanduser("~"), ".radium")] + sys.path
sys.path = [os.path.abspath(os.path.dirname(sys.argv[0]))] + sys.path

#import platform

#if platform.system() != "Linux" and platform.system() != "mingw":
#    sys.path = [sys.path[0], os.path.join(sys.path[0],"python2.7")]
#    sys.path = sys.path + [os.path.join(sys.path[1],"lib-dynload")]

print "sys.path now:",sys.path
#import platform
#print "platform:",platform.system()

import traceback


import radium,keybindingsparser
#import keybindingsparser


true=1
false=0

def makeemptylist(len):
    ret=[]
    for lokke in range(len):
        ret.append([])
    return ret


ra=radium


print "Parsing keybindings.conf..."
#profile.run("KeyConfer(infilehandle,outfilehandle)","fooprof")

try:
    keybindingsparser.init(sys.argv[1], sys.argv[2])
except:
    print sys.exc_info()
    #radium.addMessage("Couldn't create keybindings dict. ("+str(sys.exc_info())+")")
    message = traceback.format_exc()
    #radium.addMessage("Loading "+filename+" failed.") # If this is a valid module file, please send it to k.s.matheussen@notam02.no ("+str(e)+")")
    #        for m in message.split("\n"):
    radium.addMessage("Couldn't parse keybindings file.\n\nBacktrace:"+message)
    sys.exit(6)

if keybindingsparser.parse_and_show_errors()==False:
    sys.exit(5)

# Hack to fix generated_keybinding_code to load during startup. This is not necessary when calling reloadKeybindings(), so I don't know why this is. Maybe some file cache thing.
# Didn't work (I thought it worked once, but maybe not). Instead we call reloadKeybindings() in Qt_Main.cpp after startup..
#if keybindingsparser.parse_and_show_errors()==False:
#    sys.exit(50)


import generated_keybinding_code as keybinding



#import os
#os.system("/usr/bin/givertcap")


pid = os.getpid()

# Seems like Ctrl+C is handled properly without this code now. This block is also disabled now since it sometimes caused the main process hang if program crashed, and you had to run "killall -9 radium" or similar.
if False: #hasattr(os,'fork') and os.fork()==0:
    import signal,time
    def signal_handler(signalnum, frame):
        print "You pressed Ctrl+C. Sending SIGINT signal to radium."
        os.kill(pid,signal.SIGINT)
        sys.exit(0)
    signal.signal(signal.SIGINT, signal_handler)
    while True:
        #print os.getppid(),pid,"hmm"
        #sys.stdout.flush()
        if os.getppid() != pid:
            print "Seems like parent process died. Exiting Ctrl+C process"
            sys.exit(0)
        time.sleep(1)
    #signal.pause()


if len(sys.argv)>3:
    ra.init_radium(sys.argv[3],keybindingsparser.gotKey)
else:
    ra.init_radium("",keybindingsparser.gotKey)
    

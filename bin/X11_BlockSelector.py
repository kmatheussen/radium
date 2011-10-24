#/* Copyright 2003 Kjetil S. Matheussen
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


import sys,os,socket,string,time,tempfile
import atexit

conn=0


#def BS_resizewindow(void);


def WaitForData():
    while 1:
        time.sleep(0.02)
        data=conn.recv(1024)
        if data:
            return



def BS_StartBlockSelector():
    global conn

    port=50010

    HOST = ''                 # Symbolic name meaning the local host
    s=None

    while s is None:
        try:
            port+=1
            s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            s.bind((HOST, port))
            s.listen(1)
        except:
            if s is not None:
                s.close()
                s=None


    os.system("python X11_BlockSelector.py "+str(port)+" &")

    conn, addr = s.accept()

    atexit.register(BS_EndBlockSelector)
    

def BS_EndBlockSelector():
    conn.send("exit")
    conn.close()

# The list of blocks have changed. Redraw it.
def BS_UpdateBlockList(*items):
    filename=tempfile.mktemp("-radiumBlockselectortempfile");
    file=open(filename,'w')
    file.writelines(map(lambda x:x+"\n", items))
    file.close()
    conn.send('UpdateBlockList '+filename)
    WaitForData()


# The playlist has changed. Redraw it.
def BS_UpdatePlayList(*items):
    filename=tempfile.mktemp("-radiumBlockselectortempfile");
    file=open(filename,'w')
    file.writelines(map(lambda x:str(x)+"\n", items))
    file.close()
    conn.send('UpdatePlayList '+filename)
    WaitForData()


# Current block is now 'blocknum'
def BS_SelectBlock(blocknum):
    conn.send('SelectBlock '+str(blocknum))
    WaitForData()


# Current playlist pos is now 'pos'
def BS_SelectPlaylistPos(pos):
    conn.send('SelectPlaylistPos '+str(pos))
    WaitForData()


def BS_ToFront():
    conn.send('ToFront')
    WaitForData()


def BS_SetX11Window(windownum):
    conn.send('SetX11Window '+str(windownum))
    WaitForData()
    
def BS_StopXSend():
    conn.send("StopXSend")
    WaitForData()
    
if __name__=="__main__":
    from Tkinter import *
    import threading,Queue
    import keybindingsparser

    x11window=""
    doxsend=0

    socketqueue=Queue.Queue(10)

    class SocketThread(threading.Thread):
        def run(n):
            HOST = ''    # The remote host
            port = int(sys.argv[1])
            s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            if s is None:
                print "Could not make socket"

            s.connect((HOST, port))

            while 1:
                data = s.recv(1024)
                if len(data)==0:
                    print "Seems like Radium has died. Ending Blockselector process."
                    socketqueue.put("exit")
                    break
                else:
                    socketqueue.put(data)
                    if data=="exit":
                        break
                    else:
                        s.send('confirm')
            s.close()


    class App:
        def __init__(self, master):

            self.frame=Frame(master)

            blocklistscrollbar = Scrollbar(self.frame)
            blocklistscrollbar.pack(side=LEFT, fill=Y)

            self.blocklist = Listbox(self.frame,exportselection="no",yscrollcommand=blocklistscrollbar.set,selectmode=SINGLE)
            self.blocklist.pack(side=LEFT,fill=BOTH,expand=1)
            self.selectBlock(0)

            blocklistscrollbar.config(command=self.blocklist.yview)

            buttonframe=Frame(self.frame)
            self.remove = Button(buttonframe, text="<-", command=self.remove)
            self.remove.pack(expand=1,fill=BOTH)
            self.add = Button(buttonframe, text="->", fg="red", command=self.add)
            self.add.pack(expand=1,fill=BOTH)
            buttonframe.pack(side=LEFT,fill=Y)

            playlistscrollbar = Scrollbar(self.frame)
            playlistscrollbar.pack(side=LEFT, fill=Y)

            self.playlist = Listbox(self.frame,exportselection="no",yscrollcommand=playlistscrollbar.set,selectmode=SINGLE)
            self.playlist.pack(side=LEFT,fill=BOTH,expand=1)
            self.selectPlaylistPos(0)

            playlistscrollbar.config(command=self.playlist.yview)

            self.keysupdown={}

            self.frame.bind_all("<Key>",lambda x:self.callback(x))
            self.frame.bind_all("<KeyRelease>",lambda x:self.releasecallback(x))
            
            self.frame.pack(fill=BOTH,expand=1)


        def getKeySwitch(self):
            keyswitches={"Control_L":1<<1,
                         "Control_R":1<<9,
                         "Caps_Lock":1<<3,
                         "Shift_L":1<<4,
                         "Shift_R":1<<5,
                         "Alt_L":1<<6,
                         "Alt_R":1<<7,
                         "??":1<<8
                         }
            keyswitch=0
            for i in keyswitches.keys():
                if self.keysupdown.has_key(i):
                    if self.keysupdown[i]==1:
                        keyswitch+=keyswitches[i]
            #print "Keyswitch: "
            #print keyswitch
            return keyswitch

        def getKeyNum(self,keysym):
            if keysym=="Up": keysym="UPARROW"
            elif keysym=="Down": keysym="DOWNARROW"
            elif keysym=="Left": keysym="LEFTARROW"
            elif keysym=="Right": keysym="RIGHTARROW"
            else:
                keysym=string.upper(keysym)
            print keysym
            return keybindingsparser.tuple_has_key(keybindingsparser.keysub,keysym)
            
        def callback(self,event):
            self.keysupdown[event.keysym]=1
            keynum=self.getKeyNum(event.keysym)
            if keynum>0 and doxsend==1:
                os.system("./X11_XSendEvent "+x11window+" 2 "+str(keynum)+" "+str(self.getKeySwitch()))

        def releasecallback(self,event):
            self.keysupdown[event.keysym]=0
            keynum=self.getKeyNum(event.keysym)
            if keynum>0 and doxsend==1:
                os.system("./X11_XSendEvent "+x11window+" 3 "+str(keynum)+" "+str(self.getKeySwitch()))

        def updateBlockList(self,filename):
            file=open(filename,'r')
            self.dasitems=map(lambda x:string.rstrip(x),file.readlines())
            file.close()
            os.system("rm "+filename)

#            if len(self.blocklist.curselection())>0:
#                cursel=int(self.blocklist.curselection()[0])
#            else:
#                cursel=0
                
            self.blocklist.delete(0,self.blocklist.size())

            lokke=0
            for item in self.dasitems:
                self.blocklist.insert(END,str(lokke)+". "+item)
                lokke+=1

#            self.selectBlock(cursel)
            
        def updatePlayList(self,filename):
            file=open(filename,'r')
            self.dasitems=map(lambda x:string.rstrip(x),file.readlines())
            file.close()
            os.system("rm "+filename)

            #cursel=int(self.playlist.curselection()[0])
            
            self.playlist.delete(0,self.playlist.size())

            lokke=0
            for item in self.dasitems:
                self.playlist.insert(END,str(lokke)+". "+self.blocklist.get(int(item)))
                lokke+=1
            self.playlist.insert(END,"   ")

            #self.selectPlaylistPos(cursel)
            

        def selectBlock(self,pos):
            if len(self.blocklist.curselection())>0:
                self.blocklist.selection_clear(self.blocklist.curselection()[0])
            self.blocklist.selection_set(pos)

        def selectPlaylistPos(self,pos):
            self.playlist.selection_set(pos,pos)

        def renumberPlaylist(self):
            for i in range(self.playlist.size()-1):
                text=string.split(self.playlist.get(i)," ",1)
                self.playlist.delete(i)
                self.playlist.insert(i,str(i)+". "+text[1])

        def remove(self):
            if doxsend==1:
                cursel=int(self.playlist.curselection()[0])
                if cursel+1<self.playlist.size():
                    self.playlist.delete(cursel)
                    os.system("./X11_XSendEvent "+x11window+" 1 "+str(cursel))
                    if self.playlist.size()==1:
                        self.playlist.insert(0,"0. "+self.blocklist.get(0))
                    if self.playlist.size()<cursel:
                        cursel=self.playlist.size()-1
                    self.renumberPlaylist()
                    self.selectPlaylistPos(cursel)


        def add(self):
            if doxsend==1:
                cursel=int(self.playlist.curselection()[0])
                self.playlist.selection_clear(cursel)
                self.playlist.insert(cursel,str(int(self.blocklist.curselection()[0]))+". "+self.blocklist.get(self.blocklist.curselection()))
                os.system("./X11_XSendEvent "+x11window+" 0 "+str(cursel)+" "+str(int(self.blocklist.curselection()[0])))
                self.renumberPlaylist()
                if cursel==self.playlist.size()-2:
                    self.selectPlaylistPos(cursel+1)
                else:
                    self.selectPlaylistPos(cursel)


    socketthread=SocketThread()
    socketthread.start()
    root = Tk()
    app = App(root)
    root.title("Playlist window")
    
    doend=0
    while doend==0:
        time.sleep(0.02)
        root.update()
        while not socketqueue.empty():
            command=socketqueue.get_nowait()
            splitcommand=string.split(command," ")
            if command=="exit":
                doend=1
            elif splitcommand[0]=='UpdateBlockList':
                app.updateBlockList(splitcommand[1])
            elif splitcommand[0]=='UpdatePlayList':
                app.updatePlayList(splitcommand[1])
            elif splitcommand[0]=='SelectBlock':
                app.selectBlock(int(splitcommand[1]))
            elif splitcommand[0]=="SelectPlaylistPos":
                app.selectPlaylistPos(int(splitcommand[1]))
            elif splitcommand[0]=="ToFront":
                print "ToFront"
                root.lift()
                root.focus()
            elif splitcommand[0]=="SetX11Window":
                x11window=splitcommand[1]
                doxsend=1
            elif splitcommand[0]=="StopXSend":
                doxsend=0
            else:
                app.playlist.insert(END,command)

    app.frame.quit()

#        pass
#        print "testing"
#    root.mainloop()






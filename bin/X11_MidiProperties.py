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



import sys,os,socket,string,time,cPickle
import atexit

conn=0

def WaitForData():
    while 1:
        time.sleep(0.02)
        data=conn.recv(1024)
        if data:
            return




def UpdateEmptyPatch():
    emptylist=[["No Patch"],0,
               ["No Port"],"No Port",
               0,0,0,0,0,0,0,0,0,
               [0,0,0,0,0,0,0,0],
               ["","","","","","","",""],
               [0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0]]
        
    conn.send("UpdateEmptyPatch "+cPickle.dumps(emptylist))
    WaitForData()
              
def UpdatePatchTest():
    UpdatePatch(
        ["en","to","tre","fire"],2,
        ["No Port"],"No Port",
        2,4,3,50,0,-20,1,50,90,
        [0,1,0,1,0,1,0,1],
        ["null","en","to","tre","fire","fem","seks","sju"],
        [0,1,2,3,4,5,6,7],
        [10,11,12,13,14,15,16,17]
        )


# UpdatePatch <- items format:
#
# char *patchnames[]
# int curr_patch
# char *portnames[]
# char *port
# int channel
# int MSB
# int LSB
# int preset
# int pan on/off
# int pan
# int vol on/off
# int vol
# int vel
# int cc on/off[8]
# char *ccnames[8]
# int ccnumber[8]
# int ccvals[8]

def UpdatePatch(*items):
    conn.send("UpdatePatch "+cPickle.dumps(items))
    WaitForData()
    
def test():
    #    conn.send("gakk gakk")
    conn.send(cPickle.dumps(["en","to","tre",4]))
    WaitForData()


def MIDI_ToFront():
    conn.send("ToFront")
    WaitForData()
    
def MIDI_EndMidiProperties():
    conn.send("exit")
    conn.close()

def MIDI_SetX11Window(windownum):
    print "Setting X11Window"
    conn.send("SetX11Window "+str(windownum))
    print "Waiting"
    WaitForData()
    print "Got it"
    
def MIDI_StopXSend():
    conn.send("StopXSend")
    WaitForData()

def MIDI_StartMidiProperties():    
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


    #os.system("/tmp/radium/bin/python X11_MidiProperties.py "+str(port)+" &")
    os.system("python X11_MidiProperties.py "+str(port)+" &")

    conn, addr = s.accept()

    atexit.register(MIDI_EndMidiProperties)

    UpdateEmptyPatch()


x11window=""
doxsend=0

if __name__=="__main__":

    import os.path

    print os.path.abspath(os.path.dirname(sys.argv[0]))
    
    sys.path.insert(0,'/tmp/radium/lib/python'+sys.version[:3]+'/site-packages/gtk-1.2')
    #sys.path=sys.path[:-1]
    #sys.path.append('/tmp/radium/lib/python2.2/site-packages/gtk-1.2')
    #sys.path.insert=[append(os.path.abspath(os.path.dirname(sys.argv[0]))+"/../lib/")

    import gtk
    import libglade
    import keybindingsparser

    keysupdown={}

    widgs={}


    def getWidget(name):
        if widgs.has_key(name):
            return widgs[name]
        else:
            widg=window.get_widget(name)
            widgs[name]=widg
            return widg

    def UpdateEmptyPatchDo(items):
        global doxsend
        doxsend_bu=doxsend
        doxsend=0
        UpdatePatchDo(items)
        getWidget("port").set_sensitive(0)
        getWidget("channel").set_sensitive(0)
        getWidget("msb").set_sensitive(0)
        getWidget("lsb").set_sensitive(0)
        getWidget("preset").set_sensitive(0)
        getWidget("vol_onoff").set_sensitive(0)
        getWidget("vol").set_sensitive(0)
        getWidget("pan_onoff").set_sensitive(0)
        getWidget("pan").set_sensitive(0)
        getWidget("reset_all_controllers").set_sensitive(0)
        getWidget("local_keyboard_on").set_sensitive(0)
        getWidget("local_keyboard_off").set_sensitive(0)
        getWidget("all_notes_off").set_sensitive(0)
        getWidget("all_sounds_off").set_sensitive(0)
        for lokke in range(8):
            getWidget("cconoff"+str(lokke)).set_sensitive(0)
        doxsend=doxsend_bu
        
    def UpdateNotEmptyPatchDo(items):
        global doxsend
        doxsend_bu=doxsend
        doxsend=0
        UpdatePatchDo(items)
        getWidget("port").set_sensitive(1)
        getWidget("channel").set_sensitive(1)
        getWidget("msb").set_sensitive(1)
        getWidget("lsb").set_sensitive(1)
        getWidget("preset").set_sensitive(1)
        getWidget("vol_onoff").set_sensitive(1)
        #getWidget("vol").set_sensitive(1)
        getWidget("pan_onoff").set_sensitive(1)
        #getWidget("pan").set_sensitive(1)
        getWidget("reset_all_controllers").set_sensitive(1)
        getWidget("local_keyboard_on").set_sensitive(1)
        getWidget("local_keyboard_off").set_sensitive(1)
        getWidget("all_notes_off").set_sensitive(1)
        getWidget("all_sounds_off").set_sensitive(1)
        for lokke in range(8):
            getWidget("cconoff"+str(lokke)).set_sensitive(1)
        doxsend=doxsend_bu
        
    def UpdatePatchDo(items):
        menu=gtk.GtkMenu()
        for item in items[0]:
            menu_item=gtk.GtkMenuItem(item)
            menu_item.connect("activate",Patch_Changed)
            menu_item.show()
            menu.append(menu_item)
        menu.set_active(items[1])
        getWidget("whichpatch").set_menu(menu)

        getWidget("patch_name").set_text(items[0][items[1]])
        
        menu=gtk.GtkMenu()
        for item in items[2]:
            menu_item=gtk.GtkMenuItem(item)
            menu_item.connect("activate",Port_Changed)
            menu_item.show()
            menu.append(menu_item)
        menu.set_active(0)
        getWidget("whichport").set_menu(menu)

        getWidget("port").set_text(items[3])
        
        getWidget("channel").set_text(str(items[4]))
        getWidget("msb").set_text(str(items[5]))
        getWidget("lsb").set_text(str(items[6]))

        getWidget("preset").get_adjustment().set_value(items[7])

        getWidget("pan_onoff").set_active(items[8])
        panwidg=getWidget("pan")
        panwidg.set_sensitive(items[8])
        panwidg.get_adjustment().set_value(items[9])

        getWidget("vol_onoff").set_active(items[10])
        volwidg=getWidget("vol")
        volwidg.set_sensitive(items[10])
        volwidg.get_adjustment().set_value(items[11])

        getWidget("vel").get_adjustment().set_value(items[12])

        ccnams=range(8)
        ccvals=range(8)
        for lokke in range(8):
            ccnams[lokke]=getWidget("ccnam"+str(lokke))
            ccvals[lokke]=getWidget("ccval"+str(lokke))
            getWidget("cconoff"+str(lokke)).set_active(items[13][lokke])
            ccnams[lokke].set_sensitive(items[13][lokke])
            ccvals[lokke].set_sensitive(items[13][lokke])
            ccnams[lokke].set_text(str(items[15][lokke])+". "+items[14][lokke])
            ccvals[lokke].get_adjustment().set_value(items[16][lokke])
        
        
#        for item in getWidget("whichpatch").get_menu().children():
#            item.connect("activate", Patch_Changed)

        
    def Destroy(widg):
        gtk.mainquit()

    def SendMidiEvent(arg0,arg1=0,arg2=0):
        if doxsend==1:
            os.system("./X11_XSendEvent "+x11window+" 4 "+str(arg0)+" "+str(arg1)+" "+str(arg2))

    def SendMidiToAllChannels(cmd,data,val):
        for i in range(16):
            SendMidiEvent(cmd+i,data,val)

    def Patch_Name(widg):
        print "Patch_Name"

    def Patch_Changed(item):
        menu = getWidget("whichpatch").get_menu()
        items = menu.children()
        selected_item = menu.get_active()
        index = menu.children().index(item)
        SendMidiEvent(8,index)

    def Port_Changed(item):
        menu = getWidget("whichport").get_menu()
        items = menu.children()
        selected_item = menu.get_active()
        index = menu.children().index(item)
        SendMidiEvent(23,index)
        #print index
        
    def Patch_New(widg):
        SendMidiEvent(7)
        
    def Reset_All_Controllers(widg):
        SendMidiToAllChannels(0xb0,121,0)
            
        
    def Local_Keyboard_On(widg):
        SendMidiToAllChannels(0xb0,122,127)
            
    def Local_Keyboard_Off(widg):
        SendMidiToAllChannels(0xb0,122,0)
        
    def All_Notes_Off(widg):
        SendMidiToAllChannels(0xb0,123,0)
        
    def All_Sounds_Off(widg):
        SendMidiToAllChannels(0xb0,120,0)
            
    def Set_Midi_Input(widg):
        SendMidiEvent(5)
        
    def Use_0x90_For_Note_Off(widg):
        SendMidiEvent(6,widg.active)
            
    def Port(widg):
        if widg.get_text()=="":
            SendMidiEvent(11)
        else:
            #Write filename
            SendMidiEvent(10)

    def Channel(widg):
        SendMidiEvent(12,int(widg.get_text()))
        
    def Msb(widg):
        try:
            val=int(widg.get_text())
            SendMidiEvent(13,val)
        except:
            pass
        
    def Lsb(widg):
        try:
            SendMidiEvent(14,int(widg.get_text()))
        except:
            pass
        
    def Preset(widg,value):
        SendMidiEvent(15,widg.get_adjustment().value)
        
    def Pan_OnOff(widg):
        SendMidiEvent(16,widg.active)
        
    def Pan(widg,value):
        SendMidiEvent(17,widg.get_adjustment().value)
                      
    def Vol_OnOff(widg):
        SendMidiEvent(18,widg.active)
        
    def Vol(widg,value):
        SendMidiEvent(19,widg.get_adjustment().value)
        
    def Vel(widg,value):
        SendMidiEvent(20,widg.get_adjustment().value)

    def CC_OnOff(widg):
        num=str(widg.get_name()[-1])
        SendMidiEvent(21,num,widg.active)

    def CC_Val(widg,value):
        num=str(widg.get_name()[-1])
        SendMidiEvent(22,num,widg.get_adjustment().value)

    def CC_Nam(widg):
        num=str(widg.get_name()[-1])
        print num
        
    def getKeySwitch():
        keyswitches={65507:1<<1,
                     65508:1<<9,
                     65509:1<<3,
                     65505:1<<4,
                     65506:1<<5,
                     65513:1<<6,
                     65406:1<<7,
                     0:1<<8
                     }
        keyswitch=0
        for i in keyswitches.keys():
            if keysupdown.has_key(i):
                if keysupdown[i]==1:
                    keyswitch+=keyswitches[i]
        #print "Keyswitch: "+str(keyswitch)
        #print keyswitch
        return keyswitch

    def getKeyNum(keyval,keystring):
        if len(keystring)>0:
            keystring=string.upper(keystring)
        else:
            if keyval==65361: keystring="LEFTARROW"
            elif keyval==65362: keystring="UPARROW"
            elif keyval==65363: keystring="RIGHTARROW"
            elif keyval==65364: keystring="DOWNARROW"
            else:
                return -1
        return keybindingsparser.tuple_has_key(keybindingsparser.keysub,keystring)

    def Key_Release(widg,event):
        keysupdown[event.keyval]=0
        #print "rel "+str(event.keyval)
        keynum=getKeyNum(event.keyval,event.string)
        if keynum>0 and doxsend==1:
            os.system("./X11_XSendEvent "+x11window+" 3 "+str(keynum)+" "+str(getKeySwitch()))

    def Key_Press(widg,event):
        keysupdown[event.keyval]=1
        #print "press "+str(event.keyval)+" -"+event.string+"-"
        keynum=getKeyNum(event.keyval,event.string)
        if keynum>0 and doxsend==1:
            os.system("./X11_XSendEvent "+x11window+" 2 "+str(keynum)+" "+str(getKeySwitch()))

        #print "press"
        #print dir(event)
        #print event.state
        #print event.keyval
        #print event.type
        #print "-"+event.string+"-"

#          guint state;
#  guint keyval;
#  gint length;
#  gchar *string;

        
    handle_dict={
        "destroy":Destroy,
        "patch_name":Patch_Name,
        "patch_new":Patch_New,
        "reset_all_controllers":Reset_All_Controllers,
        "local_keyboard_on":Local_Keyboard_On,
        "local_keyboard_off":Local_Keyboard_Off,
        "all_notes_off":All_Notes_Off,
        "all_sounds_off":All_Sounds_Off,
        "set_midi_input":Set_Midi_Input,
        "use_0x90_for_note_off":Use_0x90_For_Note_Off,
        "port":Port,
        "channel":Channel,
        "msb":Msb,
        "lsb":Lsb,
        "preset":Preset,
        "pan_onoff":Pan_OnOff,
        "pan":Pan,
        "vol_onoff":Vol_OnOff,
        "vol":Vol,
        "vel":Vel,
        "cconoff":CC_OnOff,
        "ccval":CC_Val,
        "ccnam":CC_Nam,
        "key_release":Key_Release,
        "key_press":Key_Press
        }


    def OpenWindow():
        global window
        window=libglade.GladeXML("X11_MidiProperties.glade","MidiProperties")
        window.signal_autoconnect(handle_dict)
        getWidget("MidiProperties").add_events(gtk.GDK.KEY_RELEASE_MASK)
    
        for item in getWidget("whichpatch").get_menu().children():
            item.connect("activate", Patch_Changed)
        for item in getWidget("whichport").get_menu().children():
            item.connect("activate", Port_Changed)

 
    def ConnectToRadium():
        global s
        HOST = ''    # The remote host
        port = int(sys.argv[1])
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        if s is None:
            print "Could not make socket"
        s.connect((HOST, port))

    def callback(*items):
        global x11window,doxsend
        
        data=s.recv(1024)
        if len(data)==0:
            print "Seems like Radium has died. Ending MidiProperties process."
            gtk.mainquit()
        else:
            datas=string.split(data," ",1)
            if datas[0]=="exit":
                gtk.mainquit()            
            elif datas[0]=="ToFront":
                #print "ToFront"
                #print dir(getWidget("MidiProperties").get_window())
                getWidget("MidiProperties").get_window()._raise()
                s.send('confirm')
            elif datas[0]=="UpdatePatch":
                UpdateNotEmptyPatchDo(cPickle.loads(datas[1]))
                s.send('confirm')
            elif datas[0]=="UpdateEmptyPatch":
                UpdateEmptyPatchDo(cPickle.loads(datas[1]))
                s.send('confirm')
            elif datas[0]=="SetX11Window":
                x11window=datas[1]
                doxsend=1
                s.send('confirm')
            elif datas[0]=="StopXSend":
                doxsend=0
                s.send('confirm')
            else:
                print "Unknown message "+datas[0]+" For X11_MidiProperties.py"
                s.send('confirm')
                

    OpenWindow()
    ConnectToRadium()
    gtk.input_add(s,gtk.GDK.INPUT_READ,callback)

    import X11_Help
    
    gtk.mainloop()
    s.close()



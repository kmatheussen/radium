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


#!/usr/bin/python


#Ehm, sorry for the totally unreadable "code". The definition of the
#keybindings.conf file syntax started out to be very simple, but after a while..





"""
Example Syntax:

CTRL_LEFT F5: RAO_SetKeyOctave 24
F3: RAO_SetKeyOctave 12


F5:
CTRL_L CTRL_R SHIFT_L SHIFT_R ALT_L ALT_R META_L META_R MOUSE_L MOUSE_R

  x       x                      x                   x                      RAO_SetKeyOctave 24

F5: RAO_SetKeyOctave 24
CL CR SL SR AL AR ML MR ML MR MM
a  a     a           x  x


CL CR SR MR ML F5: RAO_SetKeyOctave 24

CL CR SR MR ML F5 F6: RAO_SetKeyOctave 24

CL CR SR MR ML F5 F6: *
RAO_SetKeyOctave(24)
*



 


"""

import sys,string,radium,protoconfparser
from types import *


true=1
false=0

def makeemptylist(len):
    ret=[]
    for lokke in range(len):
        ret.append([])
    return ret


#import profile


#/* Keyboard Sub IDs: (picked from common/nsmtracker_events.h (must be the same)) */

from keysubids import *




def tuple_has_key(tuple,string):
    for lokke in range(len(tuple)):
        if tuple[lokke]==string:
            return lokke
    return -1


def key_is_qualifier(key):
    #print "key_is_qualifier? "+str(key)+" : "+keysub[key]
    return key!=0 and key<tuple_has_key(keysub,"FIRST_NON_QUALIFIER")


def isSpace(char):
    if char==" " or char=="\t": return true
    return false


class LineParser:
    def __init__(self,line,defines):
        self.parts=[]
        insidestring=false
        partstart=0

        line=string.lstrip(line)

        lokke=0
        while lokke<len(line):
            if line[lokke]==":":
                if lokke+1<len(line) and line[lokke+1]!=" ":
                    line=line[:lokke+1]+" "+line[lokke+1:]
                if line[lokke-1]!=" ":
                    line=line[:lokke]+" "+line[lokke:]
                break
            lokke+=1


        for lokke in range(len(line)):
            dasline=line[lokke]
            if dasline=="\t":
                dasline=" "
            if dasline=="\"":
                if lokke>0:
                    if insidestring==false:
                        insidestring=true
                        partstart=lokke
                    else:
                        insidestring=false
                else:
                    insidestring=true
                    partstart=lokke

            else:
                if lokke>0 and not insidestring:
                    dasprevline=line[lokke-1]
                    if dasprevline=="\t":
                        dasprevline=" "
                    if dasprevline==" " and dasline!=" ":
                       partstart=lokke
                    if dasprevline!=" " and dasline==" ":
                       self.parts.append(line[partstart:lokke])
        if not isSpace(line[lokke]):
            self.parts.append(line[partstart:lokke+1])

        lokke=0
        while lokke<len(self.parts):
            if defines.has_key(self.parts[lokke]):
                defdef=defines[self.parts[lokke]]
                if defdef[0]==0:
                    defadd=LineParser(defdef[2],defines).getParts()
                    self.parts=self.parts[:lokke]+defadd+self.parts[lokke+1:]
                else:
                    defparts=defdef[2][:]
                    defargparts=defdef[1][1]
                    for lokke2 in range(len(defargparts)):
                        for lokke3 in range(len(defargparts[lokke2])):
                            defparts[defargparts[lokke2][lokke3]]=self.parts[lokke+lokke2+1]
                    defstring=""
                    for lokke2 in range(len(defparts)):
                        defstring+=defparts[lokke2]+" "
                    defadd=LineParser(defstring,defines).getParts()
                    self.parts=self.parts[:lokke]+defadd+self.parts[lokke+1+len(defparts):]
            lokke+=1
        return

    def getParts(self):
        return self.parts

    def getLine(self):
        line=""
        for lokke in range(len(self.parts)):
            line+=self.parts[lokke]+" "
        return line

class Parser:
    def __init__(self, filehandle1, filehandle2 = None):
        self.linetype="NORMAL"
        self.filehandle1=filehandle1
        self.filehandle2=filehandle2
        self.linenum=0
        self.keys=[]
        self.defines={}
        self.defines_var={}
        self.nextline=""
        self.parts=[]
        self.parser=0
        self.outlinenum=0
        self.mouseEditorKey = tuple_has_key(keysub,"MOUSE_EDITOR")
        self.mouseMixerKey = tuple_has_key(keysub,"MOUSE_MIXER")
        self.mouseMixerStripsKey = tuple_has_key(keysub,"MOUSE_MIXERSTRIPS")
        self.mouseSequencerKey = tuple_has_key(keysub,"MOUSE_SEQUENCER")
        
    def readLine(self):
        self.linenum+=1
#        print "----------------line: %d " % self.linenum
        return self.filehandle1.readline()
    
    def readNextLine(self):
        if self.nextline!="":
            self.currline=self.nextline
            self.nextline=""
        else:
            self.currline=self.readLine()

        if self.currline=='':
            self.filehandle1.close()
            
            if self.filehandle1 == self.filehandle2:
                return False

            if self.filehandle2 is None:
                return False

            self.filehandle1 = self.filehandle2
            self.linenum = 0
            return self.readNextLine()

        self.currline=string.rstrip(self.currline)

        while self.currline=="" or self.currline=="\n" or self.currline[0:1]=="#":
            return self.readNextLine()

        if self.currline[0:4]=="?out":
            self.outlinenum+=1
            print "  --->%d. %s" % (self.outlinenum,self.currline[4:])
            return self.readNextLine()

        if len(self.currline)>0 and self.currline[len(self.currline)-1]=="\n":
            self.currline=self.currline[:-1]

#        self.currline+="\n"
#        if len(self.currline)>1 and self.currline[len(self.currline)-1]!="\n":


        if self.currline[:7]=="?define":
            if self.currline[:8]=="?define ":
                parts=string.split(self.currline)
                lokke=0;hits=0
                while hits<2:
                    if self.currline[lokke]==" ":
                        while self.currline[lokke]==" ":
                            lokke+=1
                        hits+=1
                    lokke+=1
                self.defines[parts[1]]=[0,[],self.currline[lokke-1:]]
            else:
                lokke=8
                while self.currline[lokke]!=")":
                    lokke+=1
                args=string.split(self.currline[8:lokke],",")

                self.currline=self.currline[lokke+2:]
                parts=string.split(self.currline)
                key=parts.pop(0)
                argplaces=[]
                for lokke2 in range(len(args)):
                    argplaces.append([])
                    for lokke in range(len(parts)):
                        if parts[lokke]==args[lokke2]:
                            argplaces[lokke2].append(lokke)

                self.defines[key]=[len(args),[args,argplaces],parts]
            return self.readNextLine()

        if self.currline[len(self.currline)-1]=="\n":
            self.currline=self.currline[:-1]

        self.currlineorg=self.currline[:]
        self.parser=LineParser(self.currline,self.defines)
        self.currline=self.parser.getLine()

        for lokke in range(len(self.currline)):
            if self.currline[lokke:lokke+2]=="?n":
                self.nextline=self.currline[lokke+3:]
                self.currline=self.currline[:lokke]
                if self.currline!="":
                    self.parser=LineParser(self.currline,self.defines)
                else:
                    return self.readNextLine()                    
                break

        return true
            
    def nextLine(self):
        if not self.readNextLine():
            return "ENDFILE"
        
        if self.currline[0:1]=='*':
            if self.linetype=="GOINGTOINSERTCODE":
                self.linetype="INSERTCODELAST"
            else:
                if self.linetype=="INSERTCODE":
                    self.linetype="INSERTCODELAST"
                else:
                    if self.linetype=="INSERTCLEANCODE":
                        self.linetype="INSERTCLEANCODELAST"
                    else:
                        self.linetype="GOINGTOINSERTCLEANCODE"
                
        else:
            if self.linetype=="GOINGTOINSERTCODE":
                self.linetype="INSERTCODE"
            if self.linetype=="GOINGTOINSERTCLEANCODE":
                self.linetype="INSERTCLEANCODE"
            if self.linetype=="INSERTCODELAST" or self.linetype=="INSERTCLEANCODELAST":
                self.linetype="NORMAL"
            if self.linetype=="NORMAL" or self.linetype=="SKIP":
                if self.currline[-2:]=="* ":
                    self.linetype="GOINGTOINSERTCODE"

                parts=self.parser.getParts()

                keys=[]
                self.command=[]
                for lokke in range(len(parts)):
                    if parts[lokke]==":":
                        keys=parts[:lokke]
                        self.command=parts[lokke+1:]
                        break
                lokke=0
                while lokke<len(keys):
                    key=tuple_has_key(keysub,keys[lokke])
                    if key==-1:
                        key=tuple_has_key(qualsub,keys[lokke])
                        if(key!=-1):
                            key+=tuple_has_key(keysub,"CTRL_L")
                    if key==-1:
                        message = "Unknown key \""+keys[lokke] +"\" in line %d in keyconfig file." % self.linenum
                        print message
                        radium.showMessage2(message)
                        del keys[lokke]
                        self.linetype="ERROR"
                        return "OK"
                    else:
                        keys[lokke]=key
                        lokke+=1

                keys.sort()
                new_keys = []
                self.qualifiers = []
                
                for key in keys:
                    if key_is_qualifier(key):
                        self.qualifiers.append(key)
                    else:
                        new_keys.append(key)

                if len(keys)>len(self.qualifiers): # ????????
                    self.keys=new_keys
                        
                if self.linetype!="GOINGTOINSERTCODE":
                    if len(self.command)==0:
                        self.linetype="SKIP"
                    else:
                        self.linetype="NORMAL"

                #print "------------------------------------------>"
                #print "command: %s" % self.command
                #print "self.qualifiers: %s" % str(map(lambda k:keysub[k], self.qualifiers))
                #print "self.keys: %s " % str(map(lambda k:keysub[k], self.keys))
                #print "<------------------------------------------"

        return "OK"
    

    def getLineType(self):
        return self.linetype

    def getNumElements(self):
        self.part=string.split(self.currline)
        return len(self.part)

    def getCurrLine(self):
        return self.currlineorg

    def getKeys(self):
        return self.keys[:]

    def mouseInQualifiers(self):
        if self.mouseEditorKey in self.qualifiers:
            return True
        elif self.mouseMixerKey in self.qualifiers:
            return True
        elif self.mouseMixerStripsKey in self.qualifiers:
            return True
        elif self.mouseSequencerKey in self.qualifiers:
            return True
        else:
            return False;
        
    def getQualifiers(self):
        #print "qualifiers:", self.qualifiers
        return self.qualifiers[:]

    def getCommands(self):
        return self.command

    def getCurrLineNum(self):
        return self.linenum
        

def putCode(keyhandles,parser,codestring,added_qualifiers):
    keys=parser.getKeys()+parser.getQualifiers()+added_qualifiers
    firstkey=keys.pop(0)
    #print "adding \"%s\", line: %d, firstkey: %d, keys: %s" % (codestring,parser.getCurrLineNum(),firstkey,keys)

    if keyhandles[firstkey].addHandle(keys,compile(codestring,'<string>','single'))==False:
        # Note. This doesn't happen anymore. Redefining a keybinding is allowed.
        message = "Keybindings for command \"%s\" in line %d is already used" % (codestring , parser.getCurrLineNum())
        print message
        radium.showMessage2(message)
        return False
    else:
        print "%s compiled." % codestring

    return True


def printsak(file,keyhandles,parser,codestring,ccommand):
    if 1:
        keys=parser.getKeys()+parser.getQualifiers()
        firstkey=keys.pop(0)
        print "Putting code for '"+codestring+"', with key "+keysub[firstkey]+", is it c command?",ccommand
        if len(keys)>0:
            print " And qualifiers: "+keysub[keys[0]]
    print codestring
    print parser.getKeys()
    print parser.getQualifiers()
    print


def addIt(keyhandles, parser, reader, command, commandname, ercommands, ccommand, firstkey, keys, added_qualifiers):

    if ccommand==False:
        radium.ER_keyAdd(firstkey,"",keys+added_qualifiers,[]) # First remove old c command for the same key binding, if previously added.
        if putCode(keyhandles,parser,command, added_qualifiers)==False:
            return False
    else:
         # Optimization. It always works to call putCode instead. (that's what originally happened).
        success,intercommands2=reader.getUnfoldedCall(commandname,ercommands)
        if not success:
            message = "Error at line %d: \"%s\"" % (parser.getCurrLineNum(),parser.getCurrLine())+"\n"+command+"\n"+str(intercommands2[0])
            print message
            radium.showMessage2(message)
            return False
        else:
            retstring=radium.ER_keyAdd(firstkey,commandname,keys+added_qualifiers,intercommands2);
            if retstring!="OK":
                message = "Error at line %d: \"%s\"" % (parser.getCurrLineNum(),parser.getCurrLine())+"\n"+str(intercommands2[0])
                print message
                radium.showMessage2(message)
                return False

def start(keyhandles,filehandle,filehandle2,outfilehandle):
    keybindingsdict={} # Note: Latest E-radium version has just removed everything related to keybindingsdict from this function. Could be unnecessary.
    
    parser=Parser(filehandle,filehandle2)
    defnum=0
    reader=protoconfparser.Read()
    
    while parser.nextLine()!="ENDFILE":

        if outfilehandle:
            if parser.getLineType()=="GOINGTOINSERTCODE":
                outfilehandle.write("def keycodedef%d():\n" % defnum)
            if parser.getLineType()=="INSERTCODE":
                outfilehandle.write(parser.getCurrLine()+"\n")
            if parser.getLineType()=="INSERTCODELAST":
                outfilehandle.write("\treturn\n")
                if putCode(keyhandles,parser,"eventreceiverparser_generated.keycodedef%d()" % defnum, [])==false:
                    return False
                defnum+=1
            if parser.getLineType()=="INSERTCLEANCODE":
                outfilehandle.write(parser.getCurrLine()+"\n")

        if parser.getLineType()=="ERROR":
            return False

        if parser.getLineType()=="NORMAL":
            commands=parser.getCommands()


            lokke=2
            while lokke<len(commands):
                if commands[lokke][0]=="+":
                    add=int(commands.pop(lokke)[1:])
                    commands[lokke-1]="%d" % (int(commands[lokke-1])+add)
                elif commands[lokke][0]=="-":
                    add=int(commands.pop(lokke)[1:])
                    commands[lokke-1]="%d" % (int(commands[lokke-1])-add)
                elif commands[lokke][0]=="*":
                    add=int(commands.pop(lokke)[1:])
                    commands[lokke-1]="%d" % (int(commands[lokke-1])*add)
                elif commands[lokke][0]=="/":
                    add=int(commands.pop(lokke)[1:])
                    commands[lokke-1]="%d" % (int(commands[lokke-1])/add)
                lokke+=1

            #print "commands", commands
            ercommands=commands[:]
                      
            intercommands=range(len(ercommands))
            dascommand=ercommands.pop(0)
            command=commands.pop(0)
            command+="("
            while len(commands)>1:
                command+=commands.pop(0)+","
            if len(commands)>0:
                command+=commands.pop(0)
            command+=")"
            keys=parser.getKeys()+parser.getQualifiers() 
            firstkey=keys.pop(0)
            ccommand=False
            if dascommand[:3]=="ra.":
                ccommand=True
                for lokke in range(len(ercommands)):
                    if ercommands[lokke][0]!="\"":
                        intercommands[lokke]=ercommands[lokke]
                    else:
                        ccommand=False
                        break

                commandname = dascommand[3:] # Cut ".ra" from beginning of function name
            else:
                commandname = dascommand
                
            # Check that all arguments are integers. If not we can't go through C. (probably no point going through C anymore though, but it made a significant difference when using a 50MHz amiga).
            if ccommand:
                for arg in reader.protos.getProto(commandname).args:
                    if arg.type_string != "int":
                        ccommand=False
                        break

            keybindingsdict[command]=[map(lambda x:keysub[x],parser.getKeys()),map(lambda x:keysub[x],parser.getQualifiers())]
            #printsak(0,keyhandles,parser,command,ccommand)
            
            if not parser.mouseInQualifiers():
                addIt(keyhandles, parser, reader, command, commandname, ercommands, ccommand, firstkey, keys, [parser.mouseEditorKey])
                addIt(keyhandles, parser, reader, command, commandname, ercommands, ccommand, firstkey, keys, [parser.mouseMixerKey])
                addIt(keyhandles, parser, reader, command, commandname, ercommands, ccommand, firstkey, keys, [parser.mouseMixerStripsKey])
                addIt(keyhandles, parser, reader, command, commandname, ercommands, ccommand, firstkey, keys, [parser.mouseSequencerKey])
            else:
                addIt(keyhandles, parser, reader, command, commandname, ercommands, ccommand, firstkey, keys, [])


    try:
        radium._keybindingsdict = keybindingsdict
    except:
        print sys.exc_info()
        radium.showMessage2("Couldn't create keybindings dict. ("+str(sys.exc_info())+")")

    #sys.exit(1)
    
    #print "BBBBBB",radium._keybindingsdict
    return True


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

import sys

import string,sys,os,cPickle

import radium

commands = {}
def get_command(menutext):
  if menutext in commands:
    return commands[menutext]
  else:
    return ""
    
class LineParser:
  def __init__(self,filename):
    file=open("keybindings.cPickle","r")
    keybindingsdict=cPickle.load(file)
    file.close()

    file=open(filename,'r')

    self.lines=map(lambda x:
                   self.constructmenuitemstring(string.split(x,"|"),keybindingsdict),
                   filter(lambda x:
                          len(x)>0 and x[0]!="#",
                          map(lambda x:
                              string.rstrip(x),file.readlines())))

    file.close()
    self.currline=0


  def constructmenuitemstring(self,items,keybindingsdict):
    def emptystring(num):
      if num<=0:
        return " "
      return "  "+emptystring(num-2)
    
    if len(items)==1:
      return items[0]
    key=keybindingsdict[string.lstrip(string.rstrip(items[1]))]
    qualifier=""
    code2read={"CTRL_L":"Left Ctrl",
               "CTRL_R":"Right Ctrl",
               "CAPS":"Caps Lock",
               "SHIFT_L":"Left Shift",
               "SHIFT_R":"Right Shift",
               "ALT_L":"Left Alt",
               "ALT_R":"Alt Gr",
               "EXTRA_L":"Left Meta",
               "EXTRA_R":"Right Meta"}
    
    for item in key[1]:
      qualifier+=code2read[item]+" + "
    ret = string.rstrip(items[0])+emptystring(40-((len(items[0]))*3/2))+qualifier+key[0][0]
    commands[string.lstrip(ret)] = string.lstrip(items[1])
    return ret
  
  def numTabs(self,line):
    if line[0]!='\t':
      return 0
    else:
      return 1+self.numTabs(line[1:])
    
  def nextLine(self,currlevel):
    if self.currline==len(self.lines):
      return -1,-1
    numtabs=self.numTabs(self.lines[self.currline])
    if currlevel>numtabs:
      return -2,-2
    if currlevel<numtabs:
      return -3,-3
    self.currline+=1
    return numtabs,string.lstrip(self.lines[self.currline-1])


class Menu:
  def __init__(self,lineparser,level):
    self.level=level
    self.items=[]
    while 1:
      numtabs,line=lineparser.nextLine(level)
      if numtabs==self.level:
        self.items.append(line)
      elif numtabs==-1:
        break
      elif numtabs==-2:
        break
      else:
        self.items.append(Menu(lineparser,self.level+1))

  def printit(self):
    for item in self.items:
      if isinstance(item,Menu):
        item.printit()
      else:
        space = ""
        for i in range(self.level):
          space = space + "  "
        print space+str(self.level)+". "+item+". Command: '"+get_command(item)+"'"

  def createRadiumMenues(self):
    def rec(items):
      if items==[]:
        return
      if len(items)>1 and isinstance(items[1],Menu):
        radium.addMenuMenu(items[0])
        items[1].createRadiumMenues()
        radium.goPreviousMenuLevel()
        rec(items[2:])
      else:
        radium.addMenuItem(items[0],get_command(items[0]))
        rec(items[1:])
    rec(self.items)
    
  def createGtkMenues(self,menu):
    global prevmenuitem
    for item in self.items:
      if isinstance(item,Menu):
        menu2=gtk.GtkMenu()
        menu2.show()
        prevmenuitem.set_submenu(menu2)
        item.createMenues(menu2)
      else:
        menu_item=gtk.GtkMenuItem(item)
        menu_item.show()
        prevmenuitem=menu_item
        menu.append(menu_item)
        #print str(self.level)+". "+item

    
menu=Menu(LineParser("menues.conf"),0)

if __name__=="__main__":
  menu.printit()
else:
  menu.createRadiumMenues()
  

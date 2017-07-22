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
import platform

import string,sys,os


if __name__!="__main__":
  import radium as ra
else:
  class Mock:
    def getConfPath(self, key):
      return os.path.join(os.getenv("HOME"),".radium",key)
    def hasConfPath(self, key):
      return True
  ra = Mock()
  sys.g_program_path = ""



commands = {}
def get_command(menutext):
  if menutext in commands:
    return commands[menutext]
  else:
    return ""

def parse_user_keys():
  import codecs

  if not ra.hasConfPath("keys"):
    return
    
  user_key_file = ra.getConfPath("keys")
  
  try:
    f = open(user_key_file,'r')
    filecontent = f.read()
    f.close()
  except:
    filecontent = codecs.open(user_key_file, "r", "latin-1" ).read()

  for line in filecontent.split('\n'):
    line = line.split('#')[0].strip()
    if len(line) == 0:
      continue
    elif '=' not in line:
      print '"Error: Malformed line in "'+user_key_file+'": '+"'"+line+"'"
      sys.exit(-1)
    else:
      key, value = line.split("=")
      code2read[key.strip()] = value.strip()


g_meta_name = "Meta" if platform.system()=="Linux" else ("Cmd" if platform.system()=="Darwin" else "Win")

code2read={"CTRL_L":"Left Ctrl",
           "CTRL_R":"Right Ctrl",
           "CTRL":"Ctrl",
           "CAPS":"Caps Lock",
           "SHIFT_L":"Left Shift",
           "SHIFT_R":"Right Shift",
           "SHIFT":"Shift",
           "ALT_L":"Left Alt",
           "ALT_R":"Alt Gr",
           "EXTRA_L": "Left "+g_meta_name,
           "EXTRA_R":"Right "+g_meta_name}


           #import platform
           #if platform.system() != "Linux" and platform.system() != "mingw":
           #  code2read["BACKSPACE"] = "Delete"


def get_key_name(code):    
  if code in code2read:
    return code2read[code]
  else:
    return code


class LineParser:
  def __init__(self,filename):
    if not '_keybindingsdict' in dir(ra):
      radium.showMessage2("Error. Unable to generate menues.");
      
    try:
      keybindingsdict = ra._keybindingsdict
    except:
      print sys.exc_info()
      radium.showMessage2("Unable to generate menues. ("+str(sys.exc_info())+")")

    #print "AAAAAAAAAAAA",keybindingsdict
    
    file=open(filename,'r')

    self.lines=map(lambda x: self.constructmenuitemstring(string.split(x,"|"),keybindingsdict),
                   filter(lambda x: len(x)>0 and x[0]!="#",
                          map(lambda x: string.rstrip(x),
                              file.readlines()
                              )
                        )
                   )

    file.close()
    self.currline=0


  def constructmenuitemstring(self,items,keybindingsdict):
    def emptystring(num):
      if num<=0:
        return ""
      return " "+emptystring(num-1)
    
    if len(items)==1:
      return items[0]

    keykey=string.lstrip(string.rstrip(items[1]))
    if keykey not in keybindingsdict:
      ret = string.rstrip(items[0])
      commands[string.lstrip(ret)] = string.lstrip(items[1])
      return ret
    
    key=keybindingsdict[keykey]
    qualifier=""
    
    for item in key[1]:
      qualifier+=get_key_name(item)+" + "

      #print "items[0]",items[0],len(items[0]),qualifier
    stripped0 = string.rstrip(items[0])
    ret = stripped0 + emptystring(41-len(stripped0)) + qualifier + get_key_name(key[0][0])
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
    line = string.lstrip(self.lines[self.currline-1])
    return numtabs,line


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
        ra.addMenuMenu(items[0], get_command(items[0]))
        items[1].createRadiumMenues()
        ra.goPreviousMenuLevel()
        rec(items[2:])
      elif items[0].startswith("--"):
        ra.addMenuSeparator()
        rec(items[1:])
      else:
        ra.addMenuItem(items[0],get_command(items[0]))
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


if __name__=="__main__":
  parse_user_keys()
  menu=Menu(LineParser("menues.conf"), 0)
  menu.printit()
else:
  parse_user_keys()
  menu=Menu(LineParser(ra.getMenuesConfPath()), 0)
  menu.createRadiumMenues()

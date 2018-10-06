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


import sys,os,socket,time
import atexit


conn=0
s=0

def WaitForData():
    while 1:
        time.sleep(0.02)
        data=conn.recv(1024)
        if data:
            return

def GFX_ColorDialog(filename,*items):
    file=open(filename,'w')
    file.writelines(map(lambda x:str(x)+"\n",items))
    file.close()
    conn.send("ColorDialog "+filename)
    WaitForData()
    
def GFX_MenuDialog(filename,*items):
    file=open(filename,'w')
    file.writelines(map(lambda x:x+"\n", items))
    file.close()
    conn.send("MenuDialog "+filename)
    WaitForData()
  
def GFX_OpenFileRequester(filename):
  conn.send("OpenFileRequester "+filename)
  WaitForData()

def GFX_SaveFileRequester(filename):
  conn.send("SaveFileRequester "+filename)
  WaitForData()

def GFX_SelectEditFont(filename):
    conn.send("SelectFont "+filename)
    WaitForData()
    
def GFX_EndQtstuff():
  conn.send("exit")
  s.close()
  
def GFX_StartQtstuff():
    global conn,s

    port=50001

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

    os.system("./X11_Qtstuff.py "+str(port)+" &")

    conn, addr = s.accept()
    
    atexit.register(GFX_EndQtstuff)

  
if __name__=="__main__":    
  import sys
  from PyQt4 import QtGui, QtCore
  import string

  class MenuWidget( QtGui.QListWidget ):
      def __init__( self, filename, *args ):
          apply( QtGui.QListWidget.__init__, (self,) + args)

          self.filename = filename
          
          file=open(self.filename,'r')
          self.dasitems=map(lambda x:string.rstrip(x),file.readlines())
          file.close()

          reversed = self.dasitems[:]
          reversed.reverse()
          for item in reversed:
              self.insertItem(0,item)
              
          self.currentItemChanged.connect(self.listBoxItemSelected)
          fm = QtGui.QFontMetrics(self.font())
          self.setMinimumSize(200,(fm.height()+0)*(len(self.dasitems)+2))
          
      def listBoxItemSelected( self, current, prev):
          if prev:
              index = self.currentRow()
              print "Writing to -"+self.filename+"-"
              file=open(self.filename,'w')
              file.write(self.dasitems[index])
              file.close()
              a.quit()

  # Not tested with qt4
  def OpenColorWidget(filename):
      file=open(filename,'r')
      dasitems=map(lambda x:string.rstrip(x),file.readlines())
      file.close()

      lokke=0
      for i in range(8):
          rgb=qt.qRgb(int(dasitems[lokke]),int(dasitems[lokke+1]),int(dasitems[lokke+2]))
          qt.QColorDialog.setCustomColor(i,rgb)
          lokke+=3      

      qt.QColorDialog.getColor()

      file=open(filename,'w')
      lokke=0
      for i in range(8):
          def wl3(number):
              file.write(str(number)+" ")
              #if number<100: file.write(" ")
              #if number<10: file.write(" ")
          wl3(qt.qRed(qt.QColorDialog.customColor(i)))
          wl3(qt.qGreen(qt.QColorDialog.customColor(i)))
          wl3(qt.qBlue(qt.QColorDialog.customColor(i)))
      file.close()
      
  def OpenMenuWidget(filename):
      w = MenuWidget(filename)
      #a.setMainWidget( w )
      w.setWindowFlags(QtCore.Qt.WindowStaysOnTopHint)
      w.show()
      w.activateWindow()
      w.raise_()
      a.exec_()

  def GetFileName(type,filename):
    if type=="open":
      fn=QtGui.QFileDialog.getOpenFileName() #None, QtCore.QString(), QtCore.QString(), QtGui.QMainWindow() )
    else:
      fn=QtGui.QFileDialog.getSaveFileName() #None, QtCore.QString(), QtCore.QString(), QtGui.QMainWindow() )
      
    file=open(filename,'w')
    if not fn.isNull():
        s = str(fn)
        print "filename",filename
        print "fn",s
        print string.rstrip(s)
        file.write(string.rstrip(s))
        file.close()

  # Not tested with qt4
  def GetFont(filename):
      font=qt.QFontDialog.getFont()[0]
      file=open(filename,'w')
      print font.rawName().ascii()
      file.write(font.rawName().ascii())
      file.close()

  a = QtGui.QApplication( sys.argv )

  HOST = ''    # The remote host
  port = int(sys.argv[1])
  s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
  if s is None:
    print "Could not make socket"
    sys.exit(3)
      
  s.connect((HOST, port))

  while 1:
    data = s.recv(1024)
    if len(data)==0:
        print "Seems like Radium has died. Ending Qtstuff process."
        break
    else:
        datas=string.split(data," ")
        if data=="exit":
            break
        elif datas[0]=="OpenFileRequester":
            GetFileName("open",datas[1])
            s.send('confirm')
        elif datas[0]=="SaveFileRequester":
            GetFileName("save",datas[1])
            s.send('confirm')
        elif datas[0]=="MenuDialog":
            OpenMenuWidget(datas[1])
            s.send('confirm')
        elif datas[0]=="ColorDialog":
            OpenColorWidget(datas[1])
            s.send('confirm')
        elif datas[0]=="SelectFont":
            GetFont(datas[1]);
            s.send('confirm')
        else:
            print "X11_Qtstuff.py: Unknown message \""+data+"\"."
    
  s.close()


 
#else:
#  os.system("python X11_FileDialog.py")
#  pass



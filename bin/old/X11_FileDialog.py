
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

def GFX_OpenFileRequester(filename):
  conn.send("open "+filename)
  WaitForData()

def GFX_SaveFileRequester(filename):
  conn.send("save "+filename)
  WaitForData()


def GFX_EndFileRequester():
  conn.send("exit")
  #s.close()
  
def GFX_StartFileRequester():
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

    os.system("python X11_FileDialog.py "+str(port)+" &")

    conn, addr = s.accept()
    
    atexit.register(GFX_EndFileRequester)

  
if __name__=="__main__":    

  import qt,string

  def GetFileName(type,filename):
    if type=="open":
      fn=qt.QFileDialog.getOpenFileName( qt.QString.null, qt.QString.null, qt.QMainWindow() )
    else:
      fn=qt.QFileDialog.getSaveFileName( qt.QString.null, qt.QString.null, qt.QMainWindow() )
      
    file=open(filename,'w')
    if not fn.isNull():
      print filename
      print fn.ascii()
      print string.rstrip(fn.ascii())
      file.write(string.rstrip(fn.ascii()))
    file.close()


  a = qt.QApplication( sys.argv )

  HOST = ''    # The remote host
  port = int(sys.argv[1])
  s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
  if s is None:
    print "Could not make socket"
    sys.exit(3)
      
  s.connect((HOST, port))

  while 1:
    data = s.recv(1024)
    if data=="exit":
      break
    else:
      GetFileName(string.split(data," ")[0],string.split(data," ")[1])
      s.send('confirm')

  s.close()


 
#else:
#  os.system("python X11_FileDialog.py")
#  pass



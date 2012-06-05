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


import sys,os,socket,string,subprocess


conn=0

def GFX_OpenReqType(width,height,title):
    global conn,xterm_process
    
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
                
    xterm_process = subprocess.Popen(["/tmp/radium/bin/xterm",
                                      "-geometry",str(width)+"x"+str(height)+"+100+100",
                                      "-title","\""+title+"\"",
                                      "-e","python","X11_ReqType.py",str(port)])
                     
#    print "Open Port "+str(port)
        
    conn, addr = s.accept()
#    print 'Connected by', addr
    return [s,conn]

def GFX_CloseReqType(reqtype):
    reqtype=conn
    reqtype.send('exit')
    reqtype.close()
    xterm_process.wait()

#def GFX_ReadString(reqtype):
def GFX_ReadString(filename):
    reqtype=conn
    reqtype.send('getstring')
    while 1:
        data=reqtype.recv(1024)
        if data:
            break
    file=open(filename,'w')
    data=string.rstrip(data)
    file.write(data)
    file.close()
#    print data
    
def GFX_WriteString(reqtype,string):
    reqtype=conn
    reqtype.send("write "+string)
    while 1:
        data=reqtype.recv(1024)
        if data:
            return


if __name__=="__main__":    
    HOST = ''    # The remote host
    port = int(sys.argv[1])
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    if s is None:
        print "Could not make socket"

    s.connect((HOST, port))

    while 1:
        data = s.recv(1024)
        if data=="exit":
            break
        elif data[:6]=="write ":
            sys.stdout.write(data[6:])
            s.send("confirm")
        else:
            reading=sys.stdin.readline()
            s.send(reading)

    s.close()

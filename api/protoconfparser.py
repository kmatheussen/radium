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




import sys,string
from common import *
        
class Radium_hs:
    def __init__(self):
        self.hs=[]
    def add(self,line):
        self.hs.append(line)
    def get(self):
        return self.hs
    def write(self,oh):
        for lokke in range(len(self.hs)):
            oh.write(self.hs[lokke][2:]+"\n")

class Radium_is:
    def __init__(self):
        self.iss=[]
    def add(self,line):
        self.iss.append(line)
    def get(self):
        return self.iss
    def write(self,oh):
        for lokke in range(len(self.iss)):
            oh.write(self.iss[lokke][2:]+"\n")
                     
class Argument:
    def __init__(self,arg):
        parts=string.split(arg)
        self.default=""
        if len(parts)>2 and parts[len(parts)-2]=="?":
            self.default=parts[len(parts)-1]
            parts=parts[:-2]

        if len(parts)==1:
            self.qualifiers=["void"]
        else:
            self.qualifiers=parts[:-1]

        self.varname=parts[len(parts)-1]

    def write(self,oh,dodefault):
        for lokke in range(len(self.qualifiers)):
            oh.write(self.qualifiers[lokke]+" ")
        oh.write(" "+self.varname)
        if dodefault and self.default!="":
            oh.write("="+self.default)



class Proto:
    def __init__(self,line):
        parts=string.split(line,"|")
        self.args=[]
        self.proc=Argument(string.strip(parts.pop(0)))
        self.arglen=len(parts)
        self.defaults=false
        
        for lokke in range(self.arglen):
            self.args.append(Argument(string.strip(parts[lokke])))

        self.reqarglen=0
        for lokke in range(self.arglen):
            if self.args[lokke].default!="":
                self.defaults=true
                break
            self.reqarglen+=1
                           
    def write(self,oh,dodefault):
        self.proc.write(oh,false)
        oh.write("(");
        if self.args!=[]:
            for lokke in range(self.arglen):
                self.args[lokke].write(oh,dodefault)
                if lokke<self.arglen-1:
                    oh.write(",");
        else:
            oh.write("void")
        oh.write(");\n");

    def write_python_wrap_proc(self,oh):
        oh.write("static PyObject *_wrap_"+self.proc.varname)
        if self.defaults==true:
            oh.write("(PyObject *self,PyObject *args,PyObject *keywds){\n")
        else:
            oh.write("(PyObject *self,PyObject *args){\n")

        oh.write("PyObject *resultobj;\n")
        for lokke in range(self.arglen):
            arg=self.args[lokke]
            for qualifier in arg.qualifiers:
                if sys.platform=="amiga" and qualifier=="float":
                    oh.write("double ")
                else:
                    oh.write(qualifier+" ")
                oh.write("arg%d" % lokke)
            if arg.default!="":
                oh.write("="+arg.default)
            oh.write(";\n")
        if not (len(self.proc.qualifiers)==1 and self.proc.qualifiers[len(self.proc.qualifiers)-1]=="void"):
            for lokke in range(len(self.proc.qualifiers)):
                oh.write(self.proc.qualifiers[lokke]+" ")
            oh.write("result;\n")

        if self.defaults==true:
            oh.write("static char *kwlist[]={")
            for lokke in range(self.arglen):
                oh.write("\""+self.args[lokke].varname+"\",")
            oh.write("NULL};\n")

            oh.write("if(!PyArg_ParseTupleAndKeywords(args,keywds,(char *)\"")
        else:
            oh.write("if(!PyArg_ParseTuple(args,(char *)\"")


        defaultused=false
        for lokke in range(self.arglen):
            arg=self.args[lokke]
            if defaultused==false and arg.default!="":
                oh.write("|")
                defaultused=true
            qualifier=arg.qualifiers[len(arg.qualifiers)-1]
            if qualifier=="int":
                t="i"
            elif qualifier=="float":
                t="f"
            elif qualifier=="PyObject*":
                t="O"
            elif qualifier=="char*":
                t="s"
            oh.write(t)

        oh.write(":"+self.proc.varname+"\"")

        if self.defaults:
            oh.write(",kwlist")

        for lokke in range(self.arglen):
            oh.write(",&arg%d" % lokke)
        oh.write(")) return NULL;\n")

        if not (len(self.proc.qualifiers)==1 and self.proc.qualifiers[len(self.proc.qualifiers)-1]=="void"):
            oh.write("result=")
        oh.write(self.proc.varname+"(")

        for lokke in range(self.arglen):
            oh.write("arg%d" % lokke)
            if lokke<self.arglen-1:
                oh.write(",")
        oh.write(");\n")

        if len(self.proc.qualifiers)==1 and self.proc.qualifiers[len(self.proc.qualifiers)-1]=="void":
            oh.write("Py_INCREF(Py_None);\n")
            oh.write("resultobj=Py_None;\n")
        else:
            oh.write("resultobj=")
            qualifier=self.proc.qualifiers[len(self.proc.qualifiers)-1]
            if qualifier=="PyObject*":
                oh.write("result;\n")
            else:
                if qualifier=="int":
                    t="PyInt_FromLong((long)"
                elif qualifier=="float":
                    t="PyFloat_FromDouble("
                elif qualifier=="char*":
                    t="PyString_FromString("
                oh.write(t+"result);\n")

        oh.write("return resultobj;\n")
        oh.write("}\n\n")
            
    def write_python_wrap_methodstruct(self,oh):
        oh.write("{(char*)\""+self.proc.varname+"\",")
        if self.defaults:
            oh.write("(PyCFunction)")
        oh.write("_wrap_"+self.proc.varname+",")
        if self.defaults:
            oh.write("METH_KEYWORDS|")
        oh.write("METH_VARARGS},\n")

        
    def getUnfoldedCall(self,arguments):
        arglen=len(arguments)
        if arglen>self.arglen:
            return false,["To many arguments"]
        if arglen<self.reqarglen:
            return false,["To few arguments"]

        args=[]
        arglen1=0
        hasgotdefault=false
        for lokke in range(arglen):
            arg=string.split(arguments[lokke],"=")
            if len(arg)==1:
                if hasgotdefault==true:
                    return false,["Non-keyword argument following keyword"]
                arglen1+=1
            if len(arg)==2:
                hasgotdefault=true

            args.append(arg)

        
        ret=makeemptylist(self.arglen)

        for lokke in range(arglen1):
            ret[lokke]=int(args.pop(0)[0])

        for lokke in range(arglen1,self.arglen):
            foundit=false
            for lokke2 in range(len(args)):
                if self.args[lokke].varname==args[lokke2][0]:
                    ret[lokke]=int(args.pop(lokke2)[1])
                    foundit=true;
                    break
            if not foundit:
                ret[lokke]=int(self.args[lokke].default)
                    
        if len(args)>0:
            return false,["Unknown argument %s " % args[0][0]]

        return true,ret


class Protos:
    def __init__(self):
        self.protos=[]
    def add(self,line):
        self.protos.append(Proto(line))
    def writeI(self,oh):
        for lokke in range(len(self.protos)):
            self.protos[lokke].write(oh,true)
    def writeH(self,oh):
        for lokke in range(len(self.protos)):
            self.protos[lokke].write(oh,false)
    def writeC(self,oh):
        for lokke in range(len(self.protos)):
            oh.write("\t{\"%s\",%s},\n" % (self.protos[lokke].proc.varname,self.protos[lokke].proc.varname))
    def write_python_wrap_proc(self,oh):
        for lokke in range(len(self.protos)):
            self.protos[lokke].write_python_wrap_proc(oh)
        oh.write("static PyMethodDef radiumMethods[]={\n")
        for lokke in range(len(self.protos)):
            self.protos[lokke].write_python_wrap_methodstruct(oh)
        oh.write("{NULL,NULL}\n")
        oh.write("};\n\n")
        
    def getUnfoldedCall(self,command,arguments):
        for lokke in range(len(self.protos)):
            if self.protos[lokke].proc.varname==command:
                return self.protos[lokke].getUnfoldedCall(arguments)
                break
        return false,["Command not found"]
    
class Read:
    def __init__(self):
        self.fh=open("protos.conf","r")
        self.linenum=0
        self.protos=Protos()
        self.iss=Radium_is()
        self.hs=Radium_hs()

        notend=true
        while notend:
            notend=self.readNextLine()

    def readLine(self):
        self.linenum+=1
        return self.fh.readline()

    def readNextLine(self):
        line=self.readLine()

        if line=="":
            self.fh.close()
            return false

        line=string.rstrip(line)

        while line=="" or line=="\n" or line[0:1]=="#":
            return self.readNextLine()

        if len(line)>0 and line[len(line)-1]=="\n":
            line=line[:-1]

        if len(line)>1:
            if line[:2]=="?S":
                self.iss.add(line)
            else:
                if line[:2]=="?H":
                    self.hs.add(line)
                else:
                    self.protos.add(line)
        else:
            self.protos.add(line)

        return true

    def makeRadium_i(self):
        oh=open("radium.i","w")
        oh.write("/*This file is automaticly generated from protos.conf.*/\n");
        self.iss.write(oh)
        self.protos.writeI(oh)
        oh.close()

    def makeRadium_h(self):
        oh=open("radium_proc.h","w")
        oh.write("/*This file is automaticly generated from protos.conf.*/\n");
        self.hs.write(oh)
        self.protos.writeH(oh)
        oh.close()

    def makeRadium_wrap_c(self):
        oh=sys.stdout
        self.protos.write_python_wrap_proc(oh)
                
    def makeWrapfunclist_c(self):
        oh=open("wrapfunclist.c","w");
        oh.write("/* This file is automaticly generated from protos.conf. */\n")
        oh.write("\n")
        oh.write("#include \"Python.h\"\n\n")
        oh.write("#include \"../common/nsmtracker.h\"\n")
        oh.write("#include \"../common/nsmtracker_events.h\"\n")
        oh.write("struct WrapFuncList wrapfunclist[]={\n")
        self.protos.writeC(oh)
        oh.write("\t{NULL,NULL}\n")
        oh.write("};\n\n")
        oh.close()


    def getUnfoldedCall(self,command,arguments):        
        return self.protos.getUnfoldedCall(command,arguments)
        
if __name__=="__main__":
    re=Read()
    if sys.argv[1]=="radium.i":
        re.makeRadium_i()
    if sys.argv[1]=="radium_proc.h":
        re.makeRadium_h()
    if sys.argv[1]=="wrapfunclist.c":
        re.makeWrapfunclist_c()
    if sys.argv[1]=="radium_wrap.c":
        re.makeRadium_wrap_c()



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




import sys,string,os
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

        self.type_string = self.qualifiers[len(self.qualifiers)-1]
        self.full_type_string = reduce(lambda x,y: x+" "+y, self.qualifiers)


    def get_s7_make_type_function(self):
        if self.type_string=="int":
            return "s7_make_integer"
        elif self.type_string=="float":
            return "s7_make_real"
        elif self.type_string=="char*":
            return "s7_make_string"
        elif self.type_string=="const_char*":
            return "s7_make_string"
        elif self.type_string=="bool":
            return "s7_make_boolean"
        elif self.type_string=="Place":
            return "s7extra_make_place"
        elif self.type_string=="func_t*":
            raise "Returning func is not supported"
        else:
            sys.stderr.write("Unknown type '"+type_string+"'")
            raise "Unknown type '"+type_string+"'"

    def get_s7_get_type_function(self):
        if self.type_string=="int":
            return "s7_integer("
        elif self.type_string=="float":
            return "s7_number_to_real(radiums7_sc, "
        elif self.type_string=="char*":
            return "(char*)s7_string("
        elif self.type_string=="const_char*":
            return "(const_char*)s7_string("
        elif self.type_string=="bool":
            return "s7_boolean(radiums7_sc, "
        elif self.type_string=="Place":
            return "s7extra_place(radiums7_sc, "
        elif self.type_string=="func_t*":
            return "s7extra_func(radiums7_sc, "
        else:
            sys.stderr.write("Unknown type '"+type_string+"'")
            raise "Unknown type '"+type_string+"'"

    def get_s7_variable_check_function(self):
        if self.type_string=="int":
            return "s7_is_integer"
        elif self.type_string=="float":
            return "s7_is_number"
        elif self.type_string=="char*":
            return "s7_is_string"
        elif self.type_string=="const_char*":
            return "s7_is_string"
        elif self.type_string=="bool":
            return "s7_is_boolean"
        elif self.type_string=="func_t*":
            return "s7_is_procedure"
        elif self.type_string=="Place":
            return "s7extra_is_place"
        else:
            sys.stderr.write("Unknown type '"+self.type_string+"'\n")
            sys.stderr.write("varname:"+self.varname+"\n")
            raise "Unknown type '"+self.type_string+"'"

    # keyDownPlay -> r-key-down-play
    # keyDownBPM -> r-key-down-bpm
    # KeyDownP   -> r-key-down-p
    # playLPBAi  -> play-lpb-ai
    def get_scheme_varname(self):
        def loop(current, name, previous_was_capitol):
            if name=="":
                return [current]

            elif name[0].isupper() and not name[1].isupper() and not name[1]==' ':
                return [current] + loop(name[0]+name[1], name[2:], False)

            elif name[0].isupper() and not previous_was_capitol:
                return [current] + loop(name[0], name[1:], True)
            
            elif name[0].isupper() and previous_was_capitol:
                return loop(current+name[0], name[1:], True)
            
            else:
                return loop(current+name[0], name[1:], False)

            
        #print(self.varname)

        if self.varname=="numLPBs":
            return "ra:num-lpbs"
        if self.varname=="numBPMs":
            return "ra:num-bpms"
        
        result = ""
        for element in loop(string.capitalize(self.varname[0]),self.varname[1:]+" ", True):
            processed = string.strip(string.lower(element))
            if processed != "":
                if result=="":
                    result = "ra:" + processed
                else:
                    result = result + "-" + processed
                    
        return result

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

        self.uses_place = False
        
        if self.proc.type_string=="Place":
            self.uses_place = True

        for arg in self.args:
            if arg.type_string=="Place":
                self.uses_place = True
        
        self.uses_func = False
         
        if self.proc.type_string=="func_t*":
            self.uses_func = True

        for arg in self.args:
            if arg.type_string=="func_t*":
                self.uses_func = True

    def write(self,oh,dodefault):
        #if self.uses_place:
        #    return
        
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
        if self.uses_place:
            return
        if self.uses_func:
            return
        
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
            elif qualifier=="const_char*":
                t="s"
            elif qualifier=="bool":
                t="b"
            else:
                sys.stderr.write("Unknown type '"+qualifier+"'")
                raise "Unknown type '"+qualifier+"'"
            
            oh.write(t)

        oh.write(":"+self.proc.varname+"\"")

        if self.defaults:
            oh.write(",kwlist")

        for lokke in range(self.arglen):
            oh.write(",&arg%d" % lokke)
        oh.write(")) return NULL;\n")

        if ("menu" not in self.proc.varname) and ("Menu" not in self.proc.varname):
            oh.write("EVENTLOG_add_event(\"" + self.proc.varname + " [py]\");\n")

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
                elif qualifier=="const_char*":
                    t="PyString_FromString("
                elif qualifier=="bool":
                    t="PyBool_FromLong((long)"
                oh.write(t+"result);\n")

        oh.write("return resultobj;\n")
        oh.write("}\n\n")
            
    def write_python_wrap_methodstruct(self,oh):
        if self.uses_place:
            return
        if self.uses_func:
            return
        
        oh.write("{(char*)\""+self.proc.varname+"\",")
        if self.defaults:
            oh.write("(PyCFunction)")
        oh.write("_wrap_"+self.proc.varname+",")
        if self.defaults:
            oh.write("METH_KEYWORDS|")
        oh.write("METH_VARARGS},\n")

    '''
static s7_pointer radium_s7_add3(s7_scheme *sc, s7_pointer org_args)
{
  s7_pointer args = org_args;
  s7_pointer arg1_s7,arg2_s7,arg3_s7;
  int arg1, arg2, arg3;
  
  if (!is_pair(args))
    return s7_wrong_number_of_args_error(sc, "add3: wrong number of args: ~A", org_args);
  arg1_s7 = s7_car(args);
  if (!s7_is_integer(arg1_s7))
    return s7_wrong_type_arg_error(sc, "add3", 1, arg1_s7, "an integer");
  arg1 = s7_integer(arg1_s7)
  args = s7_cdr(args);

  if (!is_pair(args))
    return s7_wrong_number_of_args_error(sc, "add3: wrong number of args: ~A", org_args);
  arg2_s7 = s7_car(args);
  if (!s7_is_integer(arg2_s7))
    return s7_wrong_type_arg_error(sc, "add3", 2, arg2_s7, "an integer");
  arg2 = s7_integer(arg2_s7);
  args = s7_cdr(args);

  if (!is_pair(args))
    return s7_wrong_number_of_args_error(sc, "add3: wrong number of args: ~A", org_args);
  arg3 = s7_car(args);
  if (!s7_is_integer(arg3_s7))
    return s7_wrong_type_arg_error(sc, "add3", 3, arg3_s7, "an integer");
  arg3 = s7_integer(arg3_s7);
  args = s7_cdr(args);

  if (!s7_is_null(args))
    return s7_wrong_number_of_args_error(sc, "add3: wrong number of args: ~A", org_args);

  return s7_make_integer(sc, add3(arg1, arg2, arg3));
}
'''

    '''
static s7_pointer radium_s7_add2_secondargumenthasdefaultvalue9(s7_scheme *sc, s7_pointer org_args)
{
  s7_pointer args = org_args;
  s7_pointer arg1_s7,arg2_s7;
  int arg1, arg2;
  
  if (!is_pair(args))
    return (s7_wrong_number_of_args_error(sc, "add2_secondargumenthasdefaultvalue9: wrong number of args: ~A", org_args));
  arg1_s7 = s7_car(args);
  if (!s7_is_integer(arg1_s7))
    s7_wrong_type_arg_error(sc, "add2_secondargumenthasdefaultvalue9", 1, arg1_s7, "an integer");
  arg1 = s7_integer(arg1_s7);
  args = s7_cdr(args);

  if (s7_is_null(args)) {
    arg2 = 9;
    return s7_make_integer(sc, add2_secondargumenthasdefaultvalue9(arg1, arg2));
  }

  if (!is_pair(args))
    return s7_wrong_number_of_args_error(sc, "add2_secondargumenthasdefaultvalue9: wrong number of args: ~A", org_args);
  arg2_s7 = s7_car(args);
  if (!s7_is_integer(arg2_s7))
    return s7_wrong_type_arg_error(sc, "add2_secondargumenthasdefaultvalue9", 2, arg2_s7, "an integer");
  arg2 = s7_integer(arg2_s7);
  args = s7_cdr(args);

  if (!s7_is_null(args))
    return s7_wrong_number_of_args_error(sc, "add2_secondargumenthasdefaultvalue9: wrong number of args: ~A", org_args);  

  return s7_make_integer(sc, add2_secondargumenthasdefaultvalue9(arg1, arg2));
}

static s7_pointer radium_s7_add2_d8_d9(s7_scheme *sc, s7_pointer org_args) // default value for arg1 is 8, default value for arg2 is 9.
{
  s7_pointer args = org_args;
  int arg1; s7_pointer arg1_s7; int arg2; s7_pointer arg2_s7;

  if (s7_is_null(args)) {
    arg1 = 8;
    arg2 = 9;
    return s7_make_integer(sc, add2_secondargumenthasdefaultvalue9(arg1, arg2));
  }
  
  if (!is_pair(args))
    return (s7_wrong_number_of_args_error(sc, "add2_secondargumenthasdefaultvalue9: wrong number of args: ~A", org_args));
  arg1_s7 = s7_car(args);
  if (!s7_is_integer(arg1_s7))
    s7_wrong_type_arg_error(sc, "add2_secondargumenthasdefaultvalue9", 1, arg1_s7, "an integer");
  arg1 = s7_integer(arg1_s7);
  args = s7_cdr(args);

  if (s7_is_null(args)) {
    arg2 = 9;
    return s7_make_integer(sc, add2_secondargumenthasdefaultvalue9(arg1, arg2));
  }

  if (!is_pair(args))
    return s7_wrong_number_of_args_error(sc, "add2_secondargumenthasdefaultvalue9: wrong number of args: ~A", org_args);
  arg2_s7 = s7_car(args);
  if (!s7_is_integer(arg2_s7))
    return s7_wrong_type_arg_error(sc, "add2_secondargumenthasdefaultvalue9", 2, arg2_s7, "an integer");
  arg2 = s7_integer(arg2_s7);
  args = s7_cdr(args);

  if (!s7_is_null(args))
    return s7_wrong_number_of_args_error(sc, "add2_secondargumenthasdefaultvalue9: wrong number of args: ~A", org_args);  

  return s7_make_integer(sc, add2_secondargumenthasdefaultvalue9(arg1, arg2));
}
'''

    # int arg1; s7_pointer arg1_s7; int arg2; s7_pointer arg2_s7;
    def write_s7_args(self,oh):
        for arg in self.args:
            oh.write("  "+arg.full_type_string + " " + arg.varname + "; s7_pointer " + arg.varname+"_s7;\n")

    # arg2 = 9 ; arg3 = 10, ...
    def write_s7_defaults(self, oh, args):
        for arg in args:
            oh.write("    "+arg.varname+" = "+arg.default+";\n")

    def get_arg_list(self, args, separator = ", "):
        if len(args)==0:
            return ""
        elif len(args)==1:
            return args[0].varname
        else:
            return args[0].varname + separator + self.get_arg_list(args[1:])

    # return s7_make_integer(radiums7_sc, add2_secondargumenthasdefaultvalue9(arg1, arg2));
    def write_s7_call_c_function(self,oh):
        if false: #"set" in self.proc.varname:
            oh.write("  EVENTLOG_add_event(\"" + self.proc.varname + " [sc]\"); ")
        else:
            oh.write("  ");
        callstring = self.proc.varname+"("+self.get_arg_list(self.args)+")"
        if self.proc.type_string=="void":
            oh.write(callstring+"; return s7_undefined(radiums7_sc);\n")
        else:
            conversion_function = self.proc.get_s7_make_type_function()
            oh.write("return "+conversion_function+"(radiums7_sc, "+callstring+");\n")

    def write_s7_func(self,oh):
        if "PyObject*" in map(lambda arg: arg.type_string, self.args):
            return

        oh.write("static s7_pointer radium_s7_"+self.proc.varname+"(s7_scheme *radiums7_sc, s7_pointer radiums7_args){\n")
        oh.write("  s7_pointer org_radiums7_args = radiums7_args;\n")
        self.write_s7_args(oh) # int arg1; s7_pointer arg1_s7; int arg2; s7_pointer arg2_s7;

        oh.write("\n")

        for n in range(len(self.args)):
            arg = self.args[n]

            if arg.default != "":
                 oh.write("  if (s7_is_null(radiums7_sc, radiums7_args)) {\n")
                 self.write_s7_defaults(oh, self.args[n:]) # arg2 = 9 ; arg3 = 10, ...
                 oh.write("  ") ; self.write_s7_call_c_function(oh) # return s7_make_integer(radiums7_sc, add2_secondargumenthasdefaultvalue9(arg1, arg2));
                 oh.write("  }\n")

            oh.write("  if (!s7_is_pair(radiums7_args))\n")
            oh.write('    return (s7_wrong_number_of_args_error(radiums7_sc, "'+self.proc.varname+': wrong number of args: ~A", org_radiums7_args));\n')
            oh.write('\n')
            oh.write("  "+arg.varname+"_s7 = s7_car(radiums7_args);\n")
            oh.write("  if (!"+arg.get_s7_variable_check_function()+"("+arg.varname+"_s7))\n")
            oh.write('    return s7_wrong_type_arg_error(radiums7_sc, "'+arg.varname+'", '+str(n)+', '+arg.varname+'_s7, "'+arg.type_string+'");\n')
            oh.write('\n')
            oh.write("  "+arg.varname+" = "+arg.get_s7_get_type_function()+arg.varname+"_s7);\n")
            oh.write("  radiums7_args = s7_cdr(radiums7_args);\n")
            oh.write("\n")

        oh.write("  if (!s7_is_null(radiums7_sc, radiums7_args))\n")
        oh.write('    return s7_wrong_number_of_args_error(radiums7_sc, "'+self.proc.varname+': wrong number of args: ~A", org_radiums7_args);\n')
        oh.write("\n")
        self.write_s7_call_c_function(oh) # return s7_make_integer(radiums7_sc, add2_secondargumenthasdefaultvalue9(arg1, arg2));
        oh.write("}\n")
        oh.write("\n")
        
    def write_s7_define(self,oh):
        if "PyObject*" in map(lambda arg: arg.type_string, self.args):
            return

        scheme_funcname   = self.proc.get_scheme_varname()
        c_funcname        = "radium_s7_"+self.proc.varname
        num_required_args = len(filter(lambda x: x.default == "", self.args))
        num_optional_args = len(self.args) - num_required_args
        has_rest_arg      = "false"
        description       = "("+scheme_funcname+" "+self.get_arg_list(self.args," ")+")"

        oh.write('  s7_define_function(s7, "'+scheme_funcname+'", '+c_funcname+", "+str(num_required_args)+", "+str(num_optional_args)+", "+has_rest_arg+', "'+description+'");\n')
        
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
    def write_s7_funcs(self,oh):
        for proto in self.protos:
            proto.write_s7_func(oh)
    def write_s7_defines(self,oh):
        for proto in self.protos:
            proto.write_s7_define(oh)
            
    def getUnfoldedCall(self,command,arguments):
        for lokke in range(len(self.protos)):
            if self.protos[lokke].proc.varname==command:
                return self.protos[lokke].getUnfoldedCall(arguments)
                break
        return false,["Command not found"]
    
class Read:
    def __init__(self):
        if not hasattr(sys,'g_program_path'):
            sys.g_program_path = "" # protoconfparser.py is also used during build
        self.fh=open(os.path.join(sys.g_program_path,"protos.conf"),"r")
        self.linenum=0
        self.protos=Protos()
        self.iss=Radium_is()
        self.hs=Radium_hs()
        
        self.isComment = False
        
        notend=true
        while notend:
            notend=self.readNextLine()

    def readLine(self):
        self.linenum+=1
        return self.fh.readline()

    def readNextLine(self):
        line = self.readLine()

        if line=="":
            self.fh.close()
            return False

        line = line.rstrip().split("#")[0].strip()

        if line=="" or line=="\n":
            return self.readNextLine()

        if line[len(line)-1]=="\n":
            line=line[:-1]

        if len(line)>1:
            if line[:2]=="?S":
                self.iss.add(line)
            elif line=="'''" or line=='"""':
                if self.isComment:
                    self.isComment = False
                else:
                    self.isComment = True
            elif self.isComment:
                return True
            else:
                if line[:2]=="?H":
                    self.hs.add(line)
                else:
                    self.protos.add(line)
        else:
            self.protos.add(line)

        return True

    def makeRadium_i(self):
        oh=open("radium.i","w")
        oh.write("/*This file is automaticly generated from protos.conf.*/\n");
        self.iss.write(oh)
        self.protos.writeI(oh)
        oh.close()

    def makeRadium_h(self):
        oh=open("radium_proc.h","w")
        oh.write("/*This file is automaticly generated from protos.conf.*/\n");
        oh.write("#define const_char const char\n")
        oh.write("#include \"../common/placement_type.h\"\n")
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

    def makeRadium_s7_wrap_c(self):
        oh=sys.stdout
        oh.write("#include \"Python.h\"\n\n")
        oh.write("#include \"s7.h\"\n\n")
        oh.write("#include \"../common/placement_type.h\"\n\n")
        oh.write("#include \"../embedded_scheme/s7extra_proc.h\"\n")
        oh.write("#include \"radium_proc.h\"\n\n")
        oh.write("#include \"../crashreporter/crashreporter_proc.h\"\n\n")
        self.protos.write_s7_funcs(oh)
        oh.write("void init_radium_s7(s7_scheme *s7){\n")
        self.protos.write_s7_defines(oh)
        oh.write("}\n")
        
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
    if sys.argv[1]=="radium_s7_wrap.c":
        re.makeRadium_s7_wrap_c()




from string import find

try:
	ih=open("radium_wrap.c",'r')
except:
	print "Error. Cant open \"radium_wrap.c\""
	sys.exit(1)

while 1:
	line=ih.readline()
	if line=="":
		print "Error. Could not find \"static PyMethodDef radiumMethods[] = {\" in \"radium_wrap.c\"."
		sys.exit(3)

	if line=="static PyMethodDef radiumMethods[] = {\n":
		break


namelist=[]

while 1:

	line=ih.readline()

	if line=="\n":
		continue

	if line[:2]=="};":
		break;

	firstplace=find(line,"\"")
	if firstplace>0:
		namelist.append(line[firstplace+1:find(line,"\"",firstplace+1)])

ih.close()

if namelist==[]:
	print "Error. Could not find any functions in \"radium_wrap.c\""
	sys.exit(4)

namelist.sort()

try:
	oh=open("wrapfunclist.c",'w+')
except:
	print "Error. Cant open \"wrapfunclist.c\" for writing"
	sys.exit(2)

oh.write("/* Do not edit. This file is automaticly generated. */\n")
oh.write("\n")
oh.write("#include \"../common/nsmtracker.h\"\n")
oh.write("#include \"../common/nsmtracker_events.h\"\n")
oh.write("#include \"python.h\"\n\n")


oh.write("struct WrapFuncList wrapfunclist[]={\n")

for lokke in range(len(namelist)):
	oh.write("\t{\"%s\",%s},\n" % (namelist[lokke],namelist[lokke]))

oh.write("\t{NULL,NULL}\n")
oh.write("};\n\n")

oh.close()



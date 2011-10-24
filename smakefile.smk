
#----------------DEFININTIONS----------------------

# If you don`t have "rad:", or "rad:" is too small. Just change
# "T" to a clean directory with enough space (but not "ram:").
# This is the only place "rad:" is defined.

T=rad:
PD=dev:python20_source

PLAYERPRI=40

FROMMAKEFILE=thesmakefile.smk
TOMAKEFILE=$(T)smakefile.smk
MAKEFILE=smakefile.smk

CC=sc
LINKER=slink

AD=Amiga/
CD=common/
API=api/

MAX_NUM_UNDOS=800

#-----------------COMPILING-----------------------------

all: $(TOMAKEFILE)
	smake -f $(TOMAKEFILE)

clean:
	copy radium:keybindings.conf .

	echo >m.o
   delete $(CD)*.o $(CD)new/*.o $(AD)plug-ins/*.o $(API)*.o \
          *.o radium0.* $(AD)instrprop/*.o $(AD)*.o \
          mmd2loader/*.o

	echo >m.info
   delete $(CD)*.info $(CD)new/*.info $(AD)plug-ins/*.info $(API)*.info \
          *.info $(AD)instrprop/*.info $(AD)*.info \
          mmd2loader/*.info

	echo >m.lnk
   delete $(CD)*.lnk $(CD)new/*.lnk $(AD)plug-ins/*.lnk $(API)*.lnk \
          *.lnk $(AD)instrprop/*.lnk $(AD)*.lnk \
          mmd2loader/*.lnk

	echo >m.dis
   delete $(CD)*.dis $(CD)new/*.dis $(AD)plug-ins/*.dis $(API)*.dis \
          *.dis $(AD)instrprop/*.dis $(AD)*.dis \
          mmd2loader/*.dis

	echo >bin/m.pyc
	delete bin/*.pyc $(API)*.pyc bin/radium bin/radium.info bin/type \
	       bin/eventreceiver_generated.py $(API)radium_wrap.c \
	       $(API)wrapfunclist.c $(API)radium.i $(API)radium_proc.h \
	       bin/protoconfparser.py bin/protos.conf bin/common.py bin/cat


newfile:
	delete $(T)radplace.m
	smake all

$(TOMAKEFILE): $(FROMMAKEFILE) $(MAKEFILE)
	echo "\#Do not edit. This file is automaticly generated." >$(TOMAKEFILE)
	echo "T=$(T)" >>$(TOMAKEFILE)
	echo "MAX_NUM_UNDOS=$(MAX_NUM_UNDOS)" >>$(TOMAKEFILE)
	echo "PD=$(PD)" >>$(TOMAKEFILE)
	echo "PLAYERPRI=$(PLAYERPRI)" >>$(TOMAKEFILE)
	echo "MAKEFILE=$(TOMAKEFILE)" >>$(TOMAKEFILE)
	echo "CC=$(CC)" >>$(TOMAKEFILE)
	echo "LINKER=$(LINKER)" >>$(TOMAKEFILE)
	echo "AD=$(AD)" >>$(TOMAKEFILE)
	echo "CD=$(CD)" >>$(TOMAKEFILE)
	echo "API=$(API)" >>$(TOMAKEFILE)
	type $(FROMMAKEFILE) >>$(TOMAKEFILE)


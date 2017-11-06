/* Copyright 2003 Kjetil S. Matheussen

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. */



#include "nsmtracker.h"
#include "../common/disk.h"
#include "midi_i_plugin.h"
#include "midi_i_plugin_proc.h"

#include "OS_midi_proc.h"

#include "disk_midi_i_plugin_proc.h"


void SaveMIDIPatchData(void *pd){
	struct PatchData *patchdata=(struct PatchData *)pd;
DC_start("PATCHDATA");

        DC_SSS("clustername",patchdata->midi_port->name);
	DC_SSI("channel",patchdata->channel);
	DC_SSI("preset",patchdata->preset);
	DC_SSI("LSB",patchdata->LSB);
	DC_SSI("MSB",patchdata->MSB);

        DC_SSB("volumeonoff",patchdata->volumeonoff);
        DC_SSI("volume",patchdata->volume);
        DC_SSB("panonoff",patchdata->panonoff);
        DC_SSI("pan",patchdata->pan);

        DC_SaveS("controlchange");
        int num;
        for(num=0;num<8;num++){
          DC_SaveCleanString(patchdata->ccnames[num]);DC_SaveCleanString("\n");
          DC_SaveB(patchdata->ccsonoff[num]);
          DC_SaveI(patchdata->cc[num]);
          DC_SaveI(patchdata->ccvalues[num]);
        }

DC_end();
}

void LoadMIDIPatchData(struct PatchData *patchdata){
	static char **objs=NULL;
	static char *vars[10]={
		"clustername",
		"channel",
		"preset",
		"LSB",
		"MSB",
                "volumeonoff",
                "volume",
                "panonoff",
                "pan",
                "controlchange"
	};
	char *clustername = NULL;

	GENERAL_LOAD(0,10)

var0:
	clustername=DC_LoadS();
        patchdata->midi_port = MIDIgetPort(NULL,NULL,clustername,false);
#if 0
	if(patchdata->mymidilink==NULL){
		fprintf(stderr,"Can't find midicluster '%s', using standard midi outlink instead\n",clustername);
		patchdata->mymidilink=MIDI_getMyMidiLink(NULL,NULL,NULL);
		if(patchdata->mymidilink==NULL){
			fprintf(stderr,"Couldn't open standard midi outlink. Loading Failed.\n");
			dc.success=false;
		}
	}
#endif
	goto start;
var1:
	patchdata->channel=DC_LoadI();
        if(patchdata->channel > 15)
          patchdata->channel = 15; // Fix for bug in previous version.
	goto start;
var2:
	patchdata->preset=DC_LoadI();
	goto start;
var3:
	patchdata->LSB=DC_LoadI();
	goto start;
var4:
	patchdata->MSB=DC_LoadI();
	goto start;

var5:
        patchdata->volumeonoff=DC_LoadB();
        goto start;
var6:
        patchdata->volume=DC_LoadI();
        goto start;
var7:
        patchdata->panonoff=DC_LoadB();
        goto start;
var8:
        patchdata->pan=DC_LoadI();
        goto start;
var9:
        {
          int num;
          for(num=0;num<8;num++){
            patchdata->ccnames[num]=DC_LoadS();
            patchdata->ccsonoff[num]=DC_LoadB();
            patchdata->cc[num]=DC_LoadI();
            patchdata->ccvalues[num]=DC_LoadI();
          }
        }
        goto start;
var10:
var11:
var12:
var13:
var14:
var15:
var16:
var17:
var18:
var19:
 var20:
 var21:
        
obj0:
obj1:
obj2:
obj3:
obj4:
obj5:
obj6:

error:
end:
	return;
}



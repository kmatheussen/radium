


#include "nsmtracker.h"
#include <midi/camd.h>
#include "../disk.h"
#include "camd_getMidiLink_proc.h"
#include "camd_i_plugin.h"

#include "disk_camd_i_plugin_proc.h"


void SavePatchData(void *pd){
	struct PatchData *patchdata=(struct PatchData *)pd;
DC_start("PATCHDATA");

	DC_SSS("clustername",patchdata->mymidilink->name);
	DC_SSI("channel",patchdata->channel);
	DC_SSI("preset",patchdata->preset);
	DC_SSI("LSB",patchdata->LSB);
	DC_SSI("MSB",patchdata->MSB);

DC_end();
}

void *LoadPatchData(void){
	char **objs=NULL;
	char *vars[5]={
		"clustername",
		"channel",
		"preset",
		"LSB",
		"MSB"
	};
	char *clustername;
	struct PatchData *patchdata=DC_alloc(sizeof(struct PatchData));

	GENERAL_LOAD(0,5)

var0:
	clustername=DC_LoadS();
	patchdata->mymidilink=CAMD_getMyMidiLink(clustername);
	if(patchdata->mymidilink==NULL){
		fprintf(stderr,"Can't find midicluster '%s', using out.0 instead\n",clustername);
		patchdata->mymidilink=CAMD_getMyMidiLink("out.0");
		if(patchdata->mymidilink==NULL){
			fprintf(stderr,"Couldn't find out.0. Loading Failed.\n");
			dc.success=false;
		}
	}
	goto start;
var1:
	patchdata->channel=DC_LoadI();
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
var6:
var7:
var8:
var9:
var10:
var11:
var12:
var13:
var14:
var15:
var16:
var17:
var18:

obj0:
obj1:
obj2:
obj3:
obj4:
obj5:
obj6:

error:
end:
	return patchdata;
}



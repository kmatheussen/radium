
#include "nsmtracker.h"
#include "disk.h"

#if 0
#ifdef _AMIGA
#include "plug-ins/disk_camd_i_plugin_proc.h"
#include "plug-ins/camd_i_plugin_proc.h"
#endif
#endif


#include "../midi/dummy/OS_midi_spesific.h"

#include "../midi/midi_i_plugin.h"
#include "../midi/midi_i_plugin_proc.h"
#include "../midi/disk_midi_i_plugin_proc.h"




#include "disk_patches_proc.h"



void SavePatch(struct Patch *patch){
if(patch==NULL) return;
DC_start("PATCH");

	DC_SaveL(patch->l.num);

	DC_SSS("name",patch->name);
	DC_SSI("minvel",patch->minvel);
	DC_SSI("maxvel",patch->maxvel);
	DC_SSI("standardvel",patch->standardvel);

#ifdef _AMIGA
	SavePatchData(patch->patchdata);
#endif

DC_end();
SavePatch(NextPatch(patch));
}


struct Patch *LoadPatch(void){
	static char *objs[1]={
		"PATCHDATA"
	};
	static char *vars[4]={
		"name",
		"minvel",
		"maxvel",
		"standardvel"
	};
	struct Patch *patch=DC_alloc(sizeof(struct Patch));
	patch->l.num=DC_LoadN();

#if 0
	patch->playnote=CAMDplaynote;
	patch->stopnote=CAMDstopnote;
	patch->changevelocity=CAMDchangevelocity;
	patch->closePatch=CAMDclosePatch;
	patch->changeTrackPan=CAMDchangeTrackPan;
#else
        MIDI_InitPatch(patch);
#endif

	GENERAL_LOAD(1,4)


var0:
	patch->name=DC_LoadS();
	goto start;
var1:
	patch->minvel=DC_LoadI();
	goto start;
var2:
	patch->maxvel=DC_LoadI();
	goto start;
var3:
	patch->standardvel=DC_LoadI();
	goto start;

obj0:
	patch->patchdata=LoadPatchData();
	goto start;


var4:
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

obj1:
obj2:
obj3:
obj4:
obj5:
obj6:

error:
end:
	return patch;

}



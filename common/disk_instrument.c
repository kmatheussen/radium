
#include "nsmtracker.h"
#include "disk.h"
#include "disk_patches_proc.h"

#include "disk_instrument_proc.h"



void SaveInstrument(struct Instruments *instrument){
DC_start("INSTRUMENT");

	DC_SSS("instrumentname",instrument->instrumentname);

	SavePatch(instrument->patches);

DC_end();
}

struct Instruments *LoadInstrument(void){
	static char *objs[1]={
		"PATCH"
	};
	static char *vars[1]={
		"instrumentname"
	};
	struct Instruments *instrument=DC_alloc(sizeof(struct Instruments));

	GENERAL_LOAD(1,1)


var0:
	instrument->instrumentname=DC_LoadS();
	goto start;

obj0:
	DC_ListAdd1(&instrument->patches,LoadPatch());
	goto start;

var1:
var2:
var3:
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
	return instrument;
}


#include "../midi/midi_i_plugin_proc.h"
#include "../midi/midi_fx_proc.h"
#include "../midi/disk_midi_fx_proc.h"
#include "../midi/midi_playfromstart_proc.h"
#include "../midi/OS_midigfx_proc.h"


void DLoadInstrument(struct Instruments *instrument){
  MIDIinitInstrumentPlugIn(instrument);
}

void DLoadInstrumentGUI(struct Instruments *instrument){
  struct Patch *patch = instrument->patches;
  while(patch!=NULL){
    instrument->PP_Update(instrument, patch);
    patch = NextPatch(patch);
  }
}


#include "nsmtracker.h"
#include "instruments_proc.h"
#include "vector_proc.h"
#include "patch_proc.h"

#include "disk.h"
#include "disk_patches_proc.h"

#include "disk_instrument_proc.h"



void SaveInstrument(struct Instruments *instrument){
DC_start("INSTRUMENT");

	DC_SSS("instrumentname",instrument->instrumentname);

	SavePatches(&instrument->patches);

DC_end();
}

struct Instruments *LoadInstrument(void){
	static char *objs[2]={
                "PATCH",
                "PATCH_V2"
	};
	static char *vars[1]={
		"instrumentname"
	};
        char *instrument_name = NULL;
	struct Instruments *instrument=NULL;

	GENERAL_LOAD(2,1)


var0:
        instrument_name = DC_LoadS();
        if(!strcmp("MIDI instrument",instrument_name))
          instrument = get_MIDI_instrument();
        if(!strcmp("Audio instrument",instrument_name))
          instrument = get_audio_instrument();        

	goto start;

obj0:
        if(instrument==NULL) {
          RError("Instrument==NULL in disk_instrument.c. instrument_name: \"%s\", strcmp returns: %d, get_audio_instrument() returns: %p",instrument_name,strcmp("Audio instrument",instrument_name),get_audio_instrument());
          instrument = get_MIDI_instrument();
        }
          
        PATCH_add_to_instrument(LoadPatchV1());
	goto start;
        
obj1:
        if(instrument==NULL) {
          RError("Instrument==NULL in disk_instrument.c. instrument_name: \"%s\", strcmp returns: %d, get_audio_instrument() returns: %p",instrument_name,strcmp("Audio instrument",instrument_name),get_audio_instrument());
          instrument = get_MIDI_instrument();
        }
          
        PATCH_add_to_instrument(LoadPatchV2());
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
var19:
 var20:
 var21:
        
obj2:
obj3:
obj4:
obj5:
obj6:

error:
end:
	return instrument;
}


#include "../midi/midi_instrument_proc.h"
#include "../midi/midi_fx_proc.h"
#include "../midi/disk_midi_fx_proc.h"
#include "../midi/midi_playfromstart_proc.h"
#include "../midi/OS_midigfx_proc.h"

void DLoadInstrument(struct Instruments *instrument){
  if(instrument==get_MIDI_instrument()) {
    MIDI_initInstrumentPlugIn(instrument);
  }
}

void DLoadInstrumentGUI(struct Instruments *instrument){
  VECTOR_FOR_EACH(struct Patch *patch, &instrument->patches)
    instrument->PP_Update(instrument, patch, true);
  END_VECTOR_FOR_EACH;
}

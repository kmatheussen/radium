
#include "nsmtracker.h"
#include "instruments_proc.h"
#include "vector_proc.h"
#include "patch_proc.h"
#include "visual_proc.h"

#include "disk.h"
#include "disk_patches_proc.h"

#include "disk_instrument_proc.h"



void SaveInstrument(struct Instruments *instrument){
DC_start("INSTRUMENT");

 int num_visible_instruments = 0;

 VECTOR_FOR_EACH(struct Patch *patch, &instrument->patches){
   if (patch->is_visible)
     num_visible_instruments++;
  }END_VECTOR_FOR_EACH;
 
 DC_SSS("instrumentname",instrument->instrumentname);
 DC_SSI("num_patches",num_visible_instruments);
 
 SavePatches(&instrument->patches);

DC_end();
}

struct Instruments *LoadInstrument(void){
	const char *objs[2]={
                "PATCH",
                "PATCH_V2"
	};
	const char *vars[2]={
          "instrumentname",
          "num_patches"
	};
        const char *instrument_name = NULL;
	struct Instruments *instrument=NULL;

        int curr_patch_num = 0;
        int num_patches = 0;
        
	GENERAL_LOAD(2,2)


var0:
        instrument_name = DC_LoadS();
        if(!strcmp("MIDI instrument",instrument_name))
          instrument = get_MIDI_instrument();
        if(!strcmp("Audio instrument",instrument_name))
          instrument = get_audio_instrument();        

	goto start;

var1:
        num_patches = DC_LoadI();
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

        curr_patch_num++;
        
        if (num_patches > 0)
          GFX_ShowProgressMessage(talloc_format("Loading instrument data %d / %d", curr_patch_num, num_patches), false);
    
        PATCH_add_to_instrument(LoadPatchV2());
	goto start;

        
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
  // Pretty sure this is not necessary.
  VECTOR_FOR_EACH(struct Patch *patch, &instrument->patches)
    instrument->PP_Update(instrument, patch, true);
  END_VECTOR_FOR_EACH;
}

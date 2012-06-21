
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

int MIDIgetStandardVelocity(struct Tracks *track);
int MIDIgetMaxVelocity(struct Tracks *track);
void MIDICloseInstrument(struct Instruments *instrument);
void MIDISelectTrackInstrument(struct Tracks *track,struct Instruments *instrument);
void MIDIStopPlaying(struct Instruments *instrument);
int MIDIgetPatch(
	struct Tracker_Windows *window,
	ReqType reqtype,
	struct Tracks *track,
	struct Patch *patch
); 
extern int MIDIgetFX(struct Tracker_Windows *window,struct Tracks *track,struct FX *fx);
extern void MIDIPP_Update(struct Instruments *instrument,struct Patch *patch);
extern void *MIDI_CopyInstrumentData(struct Tracks *track);
extern void *MIDILoadFX(struct FX *fx,struct Tracks *track);


#include "../midi/midi_playfromstart_proc.h"
#include "../midi/OS_midigfx_proc.h"


void DLoadInstrument(struct Instruments *instrument){
	instrument->instrumentname="MIDI instrument";
	instrument->getStandardVelocity= &MIDIgetStandardVelocity;
	instrument->getMaxVelocity= &MIDIgetMaxVelocity;
	instrument->getFX= &MIDIgetFX;
	instrument->getPatch= &MIDIgetPatch;
	instrument->CloseInstrument=MIDICloseInstrument;
	instrument->SelectTrackInstrument=MIDISelectTrackInstrument;
	instrument->StopPlaying=MIDIStopPlaying;
	//	instrument->PP_Update=MIDI_PP_Update;
	instrument->PP_Update=MIDIGFX_PP_Update;
	instrument->PlayFromStartHook=MIDIPlayFromStartHook;
	instrument->CopyInstrumentData=MIDI_CopyInstrumentData;
	instrument->LoadFX=MIDILoadFX;
}



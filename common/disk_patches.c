
#include "nsmtracker.h"
#include "disk.h"
#include "visual_proc.h"

#include "patch_proc.h"
#include "instruments_proc.h"

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

#include "../audio/audio_instrument_proc.h"



#include "disk_patches_proc.h"

extern struct Root *root;


static void SavePatchVoice(struct PatchVoice *voice, int voicenum){
  DC_start("VOICE");
  DC_SaveI(voicenum);
  {
    DC_SSB("is_on",voice->is_on);
    DC_SSI("transpose",voice->transpose);
    DC_SSF("volume",voice->volume);
    DC_SSF("start",voice->start);
    DC_SSF("length",voice->length);
    DC_SSI("time_format",voice->time_format);
  }
  DC_end();
}

static void LoadPatchVoice(struct PatchVoice *voice){
  static char *objs[0] = {};
  static char *vars[6] = {"is_on","transpose","volume","start","length","time_format"};

  GENERAL_LOAD(0,6);

 var0:
  voice->is_on = DC_LoadB();
  goto start;
 var1:
  voice->transpose = DC_LoadI();
  goto start;
 var2:
  voice->volume = DC_LoadF();
  goto start;
 var3:
  voice->start = DC_LoadF();
  goto start;
 var4:
  voice->length = DC_LoadF();
  goto start;
 var5:
  voice->time_format = DC_LoadI();
  goto start;
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

static void SavePatchVoices(struct Patch *patch){
  DC_start("VOICES");

  DC_SSI("max_voices",NUM_PATCH_VOICES);

  int i;
  for(i=0;i<NUM_PATCH_VOICES;i++){
    struct PatchVoice *voice=&patch->voices[i];
    SavePatchVoice(voice, i);
  }

  DC_end();
}

static void LoadPatchVoices(struct Patch *patch){
  static char *objs[1] = {"VOICE"};
  static char *vars[1] = {"max_voices"};

  int voicenum;

  GENERAL_LOAD(1,1);

 var0:
  if(DC_LoadI()!=NUM_PATCH_VOICES)
    RError("wrong number of voices in file");
  goto start;

 obj0:
  voicenum = DC_LoadI();
  //printf("----------Loading voice %d\n",voicenum);
  LoadPatchVoice(&patch->voices[voicenum]);
  //printf("----------Finished Loading voice %d\n",voicenum);
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

// This is messy, but MIDI patchdata is loaded/saved here.
// Audio patchdata is loaded/saved later. (don't know where yet when writing this comment)


void SavePatches(vector_t *v){
  int i;

  for(i=0;i<v->num_elements;i++){
    struct Patch *patch=v->elements[i];

    DC_start("PATCH");
    {
      DC_SaveL(patch->id);
      
      DC_SSS("name",patch->name);
      //DC_SSI("minvel",patch->minvel);
      //DC_SSI("maxvel",patch->maxvel);
      //DC_SSI("standardvel",patch->standardvel);

      DC_SSB("forward_events",patch->forward_events);
      DC_SSB("name_is_edited", patch->name_is_edited);
      
      if(patch->instrument==get_MIDI_instrument())
        SaveMIDIPatchData(patch->patchdata);
      
      SavePatchVoices(patch);
    }
    DC_end();
  }
}


struct Patch *LoadPatch(void){
        bool is_MIDI_instrument = false;

	static char *objs[2]={
          "PATCHDATA",
          "VOICES"
	};
	static char *vars[6]={
		"name",
		"minvel",
		"maxvel",
		"standardvel",
                "forward_events",
                "name_is_edited"
	};

	struct Patch *patch=DC_alloc(sizeof(struct Patch));
	patch->id=DC_LoadN();
        patch->is_usable = true;
        patch->forward_events = true; // default value
        patch->name_is_edited = true; // compatibility value when loading older songs
        
        PATCH_init_voices(patch);

	GENERAL_LOAD(2,6)


var0:
	patch->name=DC_LoadS();
	goto start;
var1:
	//patch->minvel=
        DC_LoadI(); // not used anymore
	goto start;
var2:
	//patch->maxvel=
        DC_LoadI(); // not used anymore
	goto start;
var3:
	//patch->standardvel=DC_LoadI();
        DC_LoadI();  // Not used anymore.
	goto start;

var4:
        patch->forward_events = DC_LoadB();
        goto start;

var5:
        patch->name_is_edited = DC_LoadB();
        goto start;

obj0:
        is_MIDI_instrument = true;
        MIDI_InitPatch(patch, NULL);
        //printf("---Load MIDI Patch Data: %p\n",patch->patchdata);
	LoadMIDIPatchData(patch->patchdata);
        //printf("---Finished Load MIDI Patch Data\n");
	goto start;

obj1:
        //printf("---Load Patch Voices\n");
        LoadPatchVoices(patch);
        //printf("---Finihsed Load Patch Voices\n");
        goto start;

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
        
obj2:
obj3:
obj4:
obj5:
obj6:

error:
end:

        if(is_MIDI_instrument==false)
          AUDIO_InitPatch(patch,NULL);


        if(patch->colornum==0)
          patch->colornum = GFX_MakeRandomCustomColor(-1);

	return patch;
}



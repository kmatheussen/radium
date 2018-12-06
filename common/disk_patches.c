
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



/*****************************************************************
                         NOTE!

  This is legacy code, for loading old songs.
  Saving and loading is now taken care of in common/patch.cpp.

*****************************************************************/



#include "../midi/dummy/OS_midi_spesific.h"

#include "../midi/midi_instrument.h"
#include "../midi/midi_instrument_proc.h"
#include "../midi/disk_midi_instrument_proc.h"

#include "../audio/audio_instrument_proc.h"



#include "disk_patches_proc.h"

extern struct Root *root;


#if defined(RELEASE)
//#  define SAVE_NEW_FORMAT 0 // Going to test the new format a few releases first.
#  define SAVE_NEW_FORMAT 1
#else
#  define SAVE_NEW_FORMAT 1
#endif


#if !SAVE_NEW_FORMAT
#error "should not be necessary anymore"
static void SavePatchVoice(struct PatchVoice *voice, int voicenum){
  DC_start("VOICE");
  DC_SaveI(voicenum);
  {
    DC_SSB("is_on",voice->is_on);
    DC_SSF("transpose",voice->transpose);
    DC_SSF("volume",voice->volume);
    DC_SSF("start",voice->start);
    DC_SSF("length",voice->length);
    DC_SSF("pan", voice->pan);
    DC_SSI("time_format",voice->time_format);
    DC_SSI("chance", voice->chance);
    DC_SSB("only_set_new_transpose_when_note_on", voice->only_set_new_transpose_when_note_on);
    DC_SSB("only_set_new_volume_when_note_on", voice->only_set_new_volume_when_note_on);
    DC_SSB("only_set_new_pan_when_note_on", voice->only_set_new_pan_when_note_on);
  }
  DC_end();
}
#endif

static void LoadPatchVoice(struct PatchVoice *voice){
  static char *objs[0] = {};
  static char *vars[11] = {"is_on","transpose","volume","start","length","pan","time_format","chance", "only_set_new_transpose_when_note_on", "only_set_new_volume_when_note_on", "only_set_new_pan_when_note_on"};

  voice->chance = 256; // For older songs
  
  GENERAL_LOAD(0,11);

 var0:
  voice->is_on = DC_LoadB();
  goto start;
 var1:
  voice->transpose = DC_LoadF();
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
  voice->pan = DC_LoadF();
  goto start;
 var6:
  voice->time_format = DC_LoadI();
  goto start;
 var7:
  voice->chance = DC_LoadI();
  goto start;
var8:
  voice->only_set_new_transpose_when_note_on = DC_LoadB();
  goto start;
var9:
  voice->only_set_new_volume_when_note_on = DC_LoadB();
  goto start;
var10:
  voice->only_set_new_pan_when_note_on = DC_LoadB();
  goto start;
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

#if !SAVE_NEW_FORMAT
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
#endif

static void LoadPatchVoices(struct Patch *patch){
  static char *objs[1] = {"VOICE"};
  static char *vars[1] = {"max_voices"};

  int voicenum;

  GENERAL_LOAD(1,1);

 var0:
  {
    int i = DC_LoadI();    
    if(i!=6 && i!=NUM_PATCH_VOICES)
      RError("wrong number of voices in file: %d",i);
  }
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
 var21:
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

void SavePatches(vector_t *v){
  int i;

  for(i=0;i<v->num_elements;i++){
    struct Patch *patch=v->elements[i];

#if SAVE_NEW_FORMAT
    
    DC_start("PATCH_V2");
    {
      HASH_save(PATCH_get_state(patch), dc.file);
    }
    DC_end();

#else
    
    DC_start("PATCH");
    {
      DC_SaveL(patch->id);
      
      DC_SSS("name",patch->name);
      //DC_SSI("minvel",patch->minvel);
      //DC_SSI("maxvel",patch->maxvel);
      //DC_SSI("standardvel",patch->standardvel);

      DC_SSB("forward_events",patch->forward_events);
      DC_SSB("name_is_edited", patch->name_is_edited);

      DC_SSS("color", GFX_get_colorname_from_color(patch->color));
      DC_SSS("comment", patch->comment==NULL ? "" : patch->comment);

      DC_SSB("wide_mixer_strip", patch->wide_mixer_strip);

      DC_SSS("uuid", patch->uuid);
      
      if(patch->instrument==get_MIDI_instrument())
        SaveMIDIPatchData(patch->patchdata);
      
      SavePatchVoices(patch);

    }
    DC_end();
    
#endif
    
  }
}


struct Patch *LoadPatchV2(void){
  struct Patch *patch = PATCH_create_from_state(HASH_load(dc.file));
  DC_fgets();
    
  return patch;
}

struct Patch *LoadPatchV1(void){
        bool is_MIDI_instrument = false;

	static char *objs[2]={
          "PATCHDATA",
          "VOICES"
	};
	static char *vars[10]={
		"name",
		"minvel",
		"maxvel",
		"standardvel",
                "forward_events",
                "name_is_edited",
                "color",
                "comment",
                "wide_mixer_strip",
                "uuid"
	};

	struct Patch *patch=PATCH_alloc();
	patch->id=DC_LoadN();
        
        if (disk_load_version < 0.67 && patch->id==0) // These songs only contained MIDI instruments, and the ID started at 0. id=0 is the main patch.
          patch->id = 500;
        
        patch->is_usable = true;
        patch->forward_events = true; // default value
        patch->name_is_edited = true; // compatibility value when loading older songs
        patch->color = GFX_mix_colors(GFX_MakeRandomColor(), GFX_get_color(HIGH_EDITOR_BACKGROUND_COLOR_NUM), 0.52f);

        PATCH_init_voices(patch);

	GENERAL_LOAD(2,10)


var0:
        PATCH_set_name(patch, DC_LoadS());
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

var6:
        patch->color = GFX_get_color_from_colorname(DC_LoadS());
        goto start;

var7:
        patch->comment = DC_LoadS();
        goto start;

var8:
        patch->wide_mixer_strip = DC_LoadB();
        goto start;
        
var9:
        patch->uuid = DC_LoadS();
        goto start;
        
obj0:
        is_MIDI_instrument = true;
        MIDI_InitPatch(patch);
        //printf("---Load MIDI Patch Data: %p\n",patch->patchdata);
	LoadMIDIPatchData(patch->patchdata);
        //printf("---Finished Load MIDI Patch Data\n");
	goto start;

obj1:
        //printf("---Load Patch Voices\n");
        LoadPatchVoices(patch);
        //printf("---Finihsed Load Patch Voices\n");
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
obj2:
obj3:
obj4:
obj5:
obj6:

error:
end:

        if(is_MIDI_instrument==false)
          //PATCH_create_audio("Sample Player", "Sample Player", name, NULL);
          AUDIO_set_patch_attributes(patch,NULL);

	return patch;
}



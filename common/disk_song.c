/* Copyright 2000 Kjetil S. Matheussen

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
#include "disk.h"
#include "disk_windows_proc.h"
#include "disk_block_proc.h"
#include "disk_playlist_proc.h"
#include "disk_instrument_proc.h"
#include "OS_Bs_edit_proc.h"
#include "instruments_proc.h"
#include "vector_proc.h"
#include "hashmap_proc.h"
#include "fxlines_proc.h"
#include "patch_proc.h"
#include "visual_proc.h"
#include "scheduler_proc.h"
#include "song_proc.h"
#include "seqtrack_proc.h"
#include "player_pause_proc.h"

#include "../mixergui/QM_MixerWidget.h"
#include "../Qt/Qt_instruments_proc.h"
#include "../Qt/Qt_comment_dialog_proc.h"
#include "../audio/audio_instrument_proc.h"
#include "../audio/Presets_proc.h"
#include "../midi/midi_i_input_proc.h"

#include "../api/api_seqtracks_proc.h"
#include "../api/api_proc.h"


#include "disk_song_proc.h"


static hash_t *g_sequencer_state = NULL;

void SaveSong(struct Song *song){
DC_start("SONG");

	DC_SSN("num_blocks",song->num_blocks);
	DC_SSI("length", 0); // Not used anymore
	DC_SSS("songname",song->songname);
	//DC_SSN("maxtracks",song->maxtracks);

        DC_SSB("linear_accelerando", song->linear_accelerando);
        DC_SSB("linear_ritardando", song->linear_ritardando);
        
        DC_SSB("plugins_should_receive_swing_tempo", song->plugins_should_receive_swing_tempo);
        DC_SSB("editor_should_swing_along", song->editor_should_swing_along);

        DC_SSB("mixer_comments_visible", song->mixer_comments_visible);
        DC_SSB("mute_editor_automation_when_track_is_muted", song->mute_editor_automation_when_track_is_muted);
        DC_SSB("use_sequencer_timing", song->use_sequencer_tempos_and_signatures);
                
        DC_start("COMMENT");{
          HASH_save(COMMENT_get_state(), dc.file);
        }DC_end();

	SaveWindow(song->tracker_windows);
	SaveBlock(song->blocks, true);
        
	//SavePlayList(song->playlist,song->length);

        DC_start("SEQUENCER");{
          HASH_save(SEQUENCER_get_state(), dc.file);
        }DC_end();
        
        // Patchdata for audio patches are saved here, not in disk_patches.
        DC_start("MIXERWIDGET");{
          HASH_save(MW_get_state(NULL, true), dc.file);
          //HASH_save(create_instrument_widget_order_state(),dc.file);
        }DC_end();

        SaveInstrument(get_MIDI_instrument());
	SaveInstrument(get_audio_instrument());

DC_end();
}

struct Song *LoadSong(void){
	static char *objs[7]={
		"TRACKER_WINDOW",
		"BLOCK",
		"PLAYLIST",
		"INSTRUMENT",
                "MIXERWIDGET",
                "SEQUENCER",
                "COMMENT"
	};
	static char *vars[11]={
		"num_blocks",
		"length",
		"songname",
		"maxtracks", // Not used anymore
                "linear_accelerando",
                "linear_ritardando",
                "plugins_should_receive_swing_tempo",
                "editor_should_swing_along",
                "mixer_comments_visible",
                "mute_editor_automation_when_track_is_muted",
                "use_sequencer_timing"
	};
	struct Song *song=SONG_create();

        song->mute_editor_automation_when_track_is_muted = false; // Compatibility with older songs.

        MIDI_SetThroughPatch(NULL);
          
        MW_cleanup(true);
          
        VECTOR_FOR_EACH(struct Patch *patch, &get_MIDI_instrument()->patches){
          InstrumentWidget_delete(patch);
        }END_VECTOR_FOR_EACH;

        while(get_MIDI_instrument()->patches.num_elements > 0)
          PATCH_remove_from_instrument(get_MIDI_instrument()->patches.elements[0]);

        // Commented out since MW_cleanup() above calls PATCH_remove_from_instrument for all audio patches.
        //
        //while(get_audio_instrument()->patches.num_elements > 0)
        //  PATCH_remove_from_instrument(get_audio_instrument()->patches.elements[0]);

        R_ASSERT(get_MIDI_instrument()->patches.num_elements==0);
        R_ASSERT(get_audio_instrument()->patches.num_elements==0);

        PATCH_reset();

        COMMENT_reset();
        
        GENERAL_LOAD(7,11)

obj0:
	DC_ListAdd1(&song->tracker_windows,LoadWindow());
	goto start;
obj1:
	DC_ListAdd1(&song->blocks,LoadBlock());
	goto start;
obj2:
	LoadPlayList(); // Used in older songs instead of sequencer.
	goto start;
obj3:
        LoadInstrument();
	goto start;

obj4:
        {
          hash_t *mixer_state = HASH_load(dc.file);
          DC_fgets();
          
          song->mixerwidget_state = mixer_state;
          goto start;
        }
obj5:
        {
          g_sequencer_state = HASH_load(dc.file);          
          DC_fgets();

          goto start;
        }
        
obj6:
        COMMENT_set_state(HASH_load(dc.file));
        DC_fgets();
        goto start;

var0:
	song->num_blocks=DC_LoadI();
        g_editor_blocks_generation++;
	goto start;
var1:
	//song->length=DC_LoadI();
        DC_LoadI();
	goto start;
var2:
	song->songname=DC_LoadS();
	goto start;
var3:
	//song->maxtracks=DC_LoadN();
        DC_LoadN(); // not used anymore
	goto start;

var4:
        song->linear_accelerando = DC_LoadB();
        goto start;
        
var5:
        song->linear_ritardando = DC_LoadB();
        goto start;
        
var6:
        song->plugins_should_receive_swing_tempo = DC_LoadB();
        goto start;
        
var7:
        song->editor_should_swing_along = DC_LoadB();
        goto start;

var8:
        song->mixer_comments_visible = DC_LoadB();
        goto start;
        
var9:
        song->mute_editor_automation_when_track_is_muted = DC_LoadB();
        goto start;

var10:
        song->use_sequencer_tempos_and_signatures = DC_LoadB();
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
        
error:
end:

	return song;
}

void DLoadSong(struct Root *newroot,struct Song *song){
  R_ASSERT(newroot->song==song);
  
        SONGPROPERTIES_update(song);

        DLoadInstrument(get_MIDI_instrument());
        DLoadInstrument(get_audio_instrument());

	DLoadBlocks(newroot,song->blocks,true);

        if (disk_load_version<0.875)
          DLoadPlayList(newroot,song);
        
	DLoadWindows(newroot,song->tracker_windows);

        GFX_ShowProgressMessage("Creating instruments");

        /*
        if(song->mixerwidget_state==NULL){
          disk_t *disk = DISK_open_for_reading(STRING_create("/home/kjetil/radium/bin/sounds/clean_mixer.hash"));
          song->mixerwidget_state = HASH_load(disk);
          DISK_close_and_delete(disk);
        }
        */
        
        if(song->mixerwidget_state!=NULL)
          MW_create_full_from_state(song->mixerwidget_state, true); // In addition, all audio plugins are created here and put into the patch->patchdata field.
        else{
          MW_create_plain(); // older song.
          gui_resetAllMixerStrips();
        }

        DLoadAudioInstrument(); // Sets correct effect_num for fx, since mapping between fx name and effect_num was not available when loading fx. (The MIDI instrument doesn't map between name and number since the MIDI standard is not going to change, and therefore it's safe to use the numbers directly.)

        if (disk_load_version>=0.875){
          GFX_ShowProgressMessage("Setting up sequencer");
          SEQUENCER_create_from_state(g_sequencer_state, song);
          g_sequencer_state = NULL;
          GFX_ShowProgressMessage("Creating instruments");
        }

        DLoadInstrumentGUI(get_MIDI_instrument());
        DLoadInstrumentGUI(get_audio_instrument());

        //if(song->instrument_widget_order_state!=NULL)
        //  recreate_instrument_widget_order_from_state(song->instrument_widget_order_state);
        song->instrument_widget_order_state=NULL; // release memory

        if (song->mixerwidget_state==NULL){
          
          //PRESET_load(STRING_create("/home/kjetil/radium/bin/clean_mixer.mrec_internal"), NULL, false, 0, 0);
          for(int busnum=0;busnum<NUM_BUSES;busnum++) {            
            createAudioInstrument(talloc_strdup("Bus"), talloc_format("Bus %d", busnum+1), talloc_format("Aux %d Bus", busnum+1), 0, 0);
          }
          PATCH_create_main_pipe();
          
        } else {

          song->mixerwidget_state=NULL; // release memory.

        }
        
        BS_UpdateBlockList();
        BS_UpdatePlayList();
          
        // Audio plugins are created after creating trackreallines.
        //TRACKREALLINES_update_peak_tracks(song->tracker_windows);

        song->tracker_windows->must_redraw = true;
}

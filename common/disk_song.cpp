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
#include "windows_proc.h"
#include "patch_proc.h"
#include "visual_proc.h"
#include "scheduler_proc.h"
#include "song_proc.h"
#include "sequencer_proc.h"
#include "player_pause_proc.h"

#include "../mixergui/QM_MixerWidget.h"
#include "../Qt/Qt_instruments_proc.h"
#include "../Qt/Qt_comment_dialog_proc.h"
#include "../audio/audio_instrument_proc.h"
#include "../audio/Presets_proc.h"
#include "../audio/Seqtrack_plugin_proc.h"
#include "../midi/midi_i_input_proc.h"

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
        DC_SSB("use_swinging_beats_in_sequencer", song->use_swinging_beats_in_sequencer);
        DC_SSB("display_swinging_beats_in_seqblocks_in_sequencer", song->display_swinging_beats_in_seqblocks_in_sequencer);
        DC_SSB("editor_should_swing_along", song->editor_should_swing_along);
        DC_SSI("glissando_behavior", song->glissando_behavior);

        DC_SSB("mixer_comments_visible", song->mixer_comments_visible);
        DC_SSB("include_pan_and_dry_in_wet_signal", song->include_pan_and_dry_in_wet_signal);
        DC_SSB("mute_system_buses_when_bypassed", song->mute_system_buses_when_bypassed);
        DC_SSB("mute_editor_automation_when_track_is_muted", song->mute_editor_automation_when_track_is_muted);
        //DC_SSB("use_sequencer_timing", song->use_sequencer_tempos_and_signatures); // saved in sequencer state instead.

        DC_SSB("mute_plugin_MIDI_when_muted", song->RT_mute_plugin_MIDI_when_muted);
        DC_SSB("send_plugin_MIDI_through_when_bypassed", song->RT_send_plugin_MIDI_through_when_bypassed);
        DC_SSB("implicitly_mute_plugin_MIDI", song->RT_implicitly_mute_plugin_MIDI);

        DC_SSI("default_num_bus_channels", song->default_num_bus_channels);
        
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

bool g_is_loading_mixer = false;

struct Song *LoadSong(void){
	const char *objs[7]={
		"TRACKER_WINDOW",
		"BLOCK",
		"PLAYLIST",
		"INSTRUMENT",
                "MIXERWIDGET",
                "SEQUENCER",
                "COMMENT"
	};
	const char *vars[19]={
		"num_blocks",
		"length",
		"songname",
		"maxtracks", // Not used anymore
                "linear_accelerando",
                "linear_ritardando",
                "plugins_should_receive_swing_tempo",
                "use_swinging_beats_in_sequencer",
                "display_swinging_beats_in_seqblocks_in_sequencer",
                "editor_should_swing_along",
                "mixer_comments_visible",
                "mute_editor_automation_when_track_is_muted",
                "include_pan_and_dry_in_wet_signal",
                "mute_system_buses_when_bypassed",
                "mute_plugin_MIDI_when_muted",
                "send_plugin_MIDI_through_when_bypassed",
                "implicitly_mute_plugin_MIDI",
                "default_num_bus_channels",
                "glissando_behavior"
                //"use_sequencer_timing"
	};
	struct Song *song=SONG_create();

        song->mute_editor_automation_when_track_is_muted = false; // Compatibility with older songs.
        song->include_pan_and_dry_in_wet_signal = false; // Compatibility with older songs.
        song->mute_system_buses_when_bypassed = false; // Compatibility with older songs.
        song->use_swinging_beats_in_sequencer = true;
        song->display_swinging_beats_in_seqblocks_in_sequencer = true;
        song->glissando_behavior = NEVER_DO_GLISSANDO; // compatibility with older songs
        song->RT_mute_plugin_MIDI_when_muted = false; // compatibility with older songs
        song->RT_send_plugin_MIDI_through_when_bypassed = false; // compatibility with older songs
        song->RT_implicitly_mute_plugin_MIDI = false; // compatibility with older songs
        
        MIDI_SetThroughPatch(NULL);
          
        MW_cleanup(true);
          
        VECTOR_FOR_EACH(struct Patch *, patch, &get_MIDI_instrument()->patches){
          InstrumentWidget_delete(patch);
        }END_VECTOR_FOR_EACH;

        while(get_MIDI_instrument()->patches.num_elements > 0)
          PATCH_remove_from_instrument((struct Patch*)get_MIDI_instrument()->patches.elements[0]);

        // Commented out since MW_cleanup() above calls PATCH_remove_from_instrument for all audio patches.
        //
        //while(get_audio_instrument()->patches.num_elements > 0)
        //  PATCH_remove_from_instrument(get_audio_instrument()->patches.elements[0]);

        R_ASSERT(get_MIDI_instrument()->patches.num_elements==0);
        R_ASSERT(get_audio_instrument()->patches.num_elements==0);

        PATCH_reset();

        COMMENT_reset();

        GENERAL_LOAD(7,19)

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
        GFX_ShowProgressMessage("Loading instruments", true);
        LoadInstrument();
	goto start;

obj4:
        {
          g_is_loading_mixer = true;
          GFX_ShowProgressMessage("Loading mixer", true);
          hash_t *mixer_state = HASH_load(dc.file);
          DC_fgets();
          g_is_loading_mixer = false;
          song->mixerwidget_state = mixer_state;
          goto start;
        }
obj5:
        {
          GFX_ShowProgressMessage("Loading sequencer", true);
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
        song->use_swinging_beats_in_sequencer = DC_LoadB();
        goto start;
        
var8:
        song->display_swinging_beats_in_seqblocks_in_sequencer = DC_LoadB();
        goto start;
        
var9:
        song->editor_should_swing_along = DC_LoadB();
        goto start;

var10:
        song->mixer_comments_visible = DC_LoadB();
        goto start;
        
var11:
        song->mute_editor_automation_when_track_is_muted = DC_LoadB();
        goto start;

var12:
        song->include_pan_and_dry_in_wet_signal = DC_LoadB();
        goto start;

var13:
        song->mute_system_buses_when_bypassed = DC_LoadB();
        goto start;

var14:
        song->RT_mute_plugin_MIDI_when_muted = DC_LoadB();
        goto start;

var15:
        song->RT_send_plugin_MIDI_through_when_bypassed = DC_LoadB();
        goto start;

var16:
        song->RT_implicitly_mute_plugin_MIDI = DC_LoadB();
        goto start;        
        
var17:
        song->default_num_bus_channels = DC_LoadI();
        BUS_set_num_channels(song->default_num_bus_channels);
        goto start;
var18:
        song->glissando_behavior = DC_LoadI();
        goto start;
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

	DLoadWindows(newroot,song->tracker_windows);


        calculateNewWindowWidthAndHeight(song->tracker_windows);
 

        GFX_ShowProgressMessage("Creating instruments", true);

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

        if (disk_load_version>=0.875){
          GFX_ShowProgressMessage("Setting up sequencer", true);
          SEQUENCER_create_from_state(g_sequencer_state, song);
          g_sequencer_state = NULL;
          GFX_ShowProgressMessage("Creating instruments", true);
        }

        DLoadAudioInstrument(newroot, song); // Sets correct effect_num for fx, since mapping between fx name and effect_num was not available when loading fx. (The MIDI instrument doesn't map between name and number since the MIDI standard is not going to change, and therefore it's safe to use the numbers directly.)
        
        DLoadInstrumentGUI(get_MIDI_instrument());
        DLoadInstrumentGUI(get_audio_instrument());

        //if(song->instrument_widget_order_state!=NULL)
        //  recreate_instrument_widget_order_from_state(song->instrument_widget_order_state);
        song->instrument_widget_order_state=NULL; // release memory

        if (song->mixerwidget_state==NULL){
          
          //PRESET_load(STRING_create("/home/kjetil/radium/bin/clean_mixer.mrec_internal"), NULL, false, 0, 0);
          for(int busnum=0;busnum<NUM_BUSES;busnum++) {            
            createAudioInstrument("Bus", talloc_format("Bus %d", busnum+1), talloc_format("Aux %d Bus", busnum+1), 0, 0, false, true);
          }
          PATCH_create_main_pipe();
          
        } else {

          song->mixerwidget_state=NULL; // release memory.

        }
        
        BS_UpdateBlockList();
        BS_UpdatePlayList();
          
        // Audio plugins are created after creating trackreallines.
        //TRACKREALLINES_update_peak_tracks(song->tracker_windows);

        if (disk_load_version<0.875)
          DLoadPlayList(newroot,song);

        SEQUENCER_ensure_bus_tracks_exist();

        song->tracker_windows->must_redraw = true;
}

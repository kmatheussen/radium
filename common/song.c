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


#include <stdlib.h>

#include "nsmtracker.h"
#include "playerclass.h"
#include "windows_proc.h"
#include "blocks_proc.h"
#include "instruments_proc.h"
#include "vector_proc.h"
#include "sequencer_proc.h"
#include "song_tempo_automation_proc.h"

#include "song_proc.h"



extern struct Root *root;
extern PlayerClass *pc;

void ClearSong(void){
	CloseAllTrackerWindows();
}

bool NewSong(void){

	root->tempo=128;
	root->lpb=4;
        root->signature = make_static_ratio(4,4);

        init_player_state();
        
	AppendBlock();

        SONG_init();
	//BL_init();


	if(OpenTrackerWindow(20,20,100,100)==-1) return false;

	return true;
}

struct Song *SONG_create(void){
  struct Song *song=talloc(sizeof(struct Song));
  song->editor_should_swing_along = true;
  song->mute_editor_automation_when_track_is_muted = true;
  song->num_channels_in_main_pipe = 2;
  
  song->show_bars_and_beats_sequencer_lane = true;
  song->show_markers_sequencer_lane = true;

  song->RT_mute_plugin_MIDI_when_muted = true;
  song->RT_send_plugin_MIDI_through_when_bypassed = true;
  song->RT_implicitly_mute_plugin_MIDI = true;

  // Set all mixer config variables to true to be compatible with older songs. New songs only have a few of these set to true by default.
  // These variables are saved in MW_get_ab_state() and not in SaveSong().
  song->includeAudioConnectionsInMixerConfig = true;
  song->includeEventConnectionsInMixerConfig = true;
  song->includeVolumeInMixerConfig = true;
  song->includePanningInMixerConfig = true;
  song->includeMuteSoloBypassInMixerConfig = true;
  song->includeSystemEffectsInMixerConfig = true;
  song->includeInstrumentEffectsInMixerConfig = true;
  song->includeInstrumentStatesInMixerConfig = true;
  song->includeMixerStripsConfigurationInMixerConfig = true;
  song->includeRememberCurrentInstrumentInMixerConfig = true;
  song->includeModulatorConnectionsInMixerConfig = true;
  song->includeSystemVolumeInMixerConfig = true;
  
  song->max_num_parallel_editor_seqblocks = 1;
  
  reset_recording_config(&song->default_recording_config);

  VECTOR_push_back(&song->seqtracks, SEQTRACK_create(NULL, 0, -1, false, false));

  SEQUENCER_init(song);

  return song;
}


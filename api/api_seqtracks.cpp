/* Copyright 2016 Kjetil S. Matheussen

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

#define __STDC_FORMAT_MACROS 1

#include "../common/includepython.h"

#include <inttypes.h>

#include <QSet>
#include <QUuid>


#include "../common/nsmtracker.h"
#include "../common/seqtrack_proc.h"
#include "../common/seqtrack_automation_proc.h"
#include "../common/seqblock_automation_proc.h"
#include "../common/song_tempo_automation_proc.h"
#include "../common/sequencer_timing_proc.h"
#include "../common/time_proc.h"
#include "../common/undo_sequencer_proc.h"
#include "../common/undo_song_tempo_automation_proc.h"
#include "../common/undo_blocks_proc.h"
#include "../common/visual_proc.h"
#include "../common/OS_Bs_edit_proc.h"
#include "../common/settings_proc.h"
#include "../common/visual_proc.h"
#include "../common/player_pause_proc.h"

#include "../embedded_scheme/s7extra_proc.h"

#include "../audio/Mixer_proc.h"
#include "../audio/SoundPlugin.h"
#include "../audio/SampleReader_proc.h"
#include "../audio/Envelope.hpp"

#include "api_common_proc.h"

#include "api_seqtracks_proc.h"
#include "radium_proc.h"

extern struct TEvent tevent;



// sequencer

float getSequencerX1(void){
  return SEQUENCER_get_x1();
}

float getSequencerX2(void){
  return SEQUENCER_get_x2();
}

float getSequencerY1(void){
  return SEQUENCER_get_y1();
}

float getSequencerY2(void){
  return SEQUENCER_get_y2();
}

float getSeqtracksX1(void){
  return SEQTRACKS_get_x1();
}

float getSeqtracksX2(void){
  return SEQTRACKS_get_x2();
}

float getSeqtracksY1(void){
  return SEQTRACKS_get_y1();
}

float getSeqtracksY2(void){
  return SEQTRACKS_get_y2();
}

float getSequencerLeftPartX1(void){
  return SEQUENCER_get_left_part_x1();
}

float getSequencerLeftPartX2(void){
  return SEQUENCER_get_left_part_x2();
}

float getSequencerLeftPartY1(void){
  return SEQUENCER_get_left_part_y1();
}

float getSequencerLeftPartY2(void){
  return SEQUENCER_get_left_part_y2();
}

bool getSequencerRightPartEmpty(void){
  return SEQUENCER_right_part_is_empty();
}

void setSequencerRightPartEmpty(bool is_empty){
  SEQUENCER_set_right_part_is_empty(is_empty);
}

float getSequencerRightPartX1(void){
  return SEQUENCER_get_right_part_x1();
}

float getSequencerRightPartX2(void){
  return SEQUENCER_get_right_part_x2();
}

float getSequencerRightPartY1(void){
  return SEQUENCER_get_right_part_y1();
}

float getSequencerRightPartY2(void){
  return SEQUENCER_get_right_part_y2();
}

void undoSeqblockFades(int seqblocknum, int seqtracknum){
  struct SeqBlock *seqblock = getSeqblockFromNum(seqblocknum, seqtracknum);
  if (seqblock==NULL)
    return;

  ADD_UNDO(SeqblockFades(seqtracknum, seqblocknum));
}

void undoSeqblockAutomation(int automationnum, int seqblocknum, int seqtracknum){
  ADD_UNDO(SeqblockAutomation(automationnum, seqblocknum, seqtracknum));
}

void undoSeqtrackAutomations(void){
  ADD_UNDO(SeqtrackAutomations());
}

void undoSeqblock(int seqblocknum, int seqtracknum){
  ADD_UNDO(Seqblock(seqtracknum, seqblocknum));
}

#define UNDO_SEQBLOCK_2(seqblockid) undoSeqblock(getSeqblockSeqblockNum(seqblockid), getSeqblockSeqtrackNum(seqblockid))

void undoSequencer(void){
  ADD_UNDO(Sequencer());
}

// sequencer

int64_t getSequencerSongLengthInFrames(void){
  return (SONG_get_length() + SEQUENCER_EXTRA_SONG_LENGTH) * MIXER_get_sample_rate();
}

int64_t getSequencerVisibleStartTime(void){
  return SEQUENCER_get_visible_start_time();
}

int64_t getSequencerVisibleEndTime(void){
  return SEQUENCER_get_visible_end_time();
}

void setSequencerVisibleStartTime(int64_t value){
  if (value < 0 || value >= SEQUENCER_get_visible_end_time()){
    // Comment out call to handleError since it could happen while playing. (and not very important)
    //handleError("setSequencerVisibleStartTime: Value must be 0 or higher and lower than visible end time. End time: %f. Value: %f\n", SEQUENCER_get_visible_end_time(), (double)value);
    return;
  }
  //printf("                   Set: %f\n", value/48000.0);
  SEQUENCER_set_visible_start_time(value);
}

void setSequencerVisibleEndTime(int64_t value){
  if (value <= SEQUENCER_get_visible_start_time()){
    // Comment out call to handleError since it could happen while playing. (and not very important)
    //handleError("setSequencerVisibleEndTime: Value must be higher than visible start time. Start time: %f. Value: %f\n", SEQUENCER_get_visible_start_time(), (double)value);
    return;
  }
  SEQUENCER_set_visible_end_time(value);
}

void setSequencerSelectionRectangle(float x1, float y1, float x2, float y2){
  SEQUENCER_set_selection_rectangle(x1, y1, x2, y2);
}

void unsetSequencerSelectionRectangle(void){
  SEQUENCER_unset_selection_rectangle();
}




float getSeqnavX1(void){
  return SEQNAV_get_x1();
}

float getSeqnavX2(void){
  return SEQNAV_get_x2();
}

float getSeqnavY1(void){
  return SEQNAV_get_y1();
}

float getSeqnavY2(void){
  return SEQNAV_get_y2();
}


float getSeqnavLeftSizeHandleX1(void){
  return SEQNAV_get_left_handle_x();
}

float getSeqnavLeftSizeHandleX2(void){
  return SEQNAV_get_left_handle_x() + SEQNAV_SIZE_HANDLE_WIDTH;
}

float getSeqnavLeftSizeHandleY1(void){
  return getSeqnavY1();
}

float getSeqnavLeftSizeHandleY2(void){
  return getSeqnavY2();
}

float getSeqnavRightSizeHandleX1(void){
  return SEQNAV_get_right_handle_x() - SEQNAV_SIZE_HANDLE_WIDTH;
}

float getSeqnavRightSizeHandleX2(void){
  return SEQNAV_get_right_handle_x();
}

float getSeqnavRightSizeHandleY1(void){
  return getSeqnavY1();
}

float getSeqnavRightSizeHandleY2(void){
  return getSeqnavY2();
}

void appendSeqtrack(bool for_audiofiles){
  ADD_UNDO(Sequencer());
  SEQUENCER_append_seqtrack(NULL, for_audiofiles);

  setCurrSeqtrack(root->song->seqtracks.num_elements - 1);
}

void insertSeqtrack(bool for_audiofiles, int pos){
  if (pos==-1)
    pos = ATOMIC_GET(root->song->curr_seqtracknum);
  
  if (pos < 0 || pos > root->song->seqtracks.num_elements){
    handleError("Position #%d not legal", pos);
    return;
  }

  ADD_UNDO(Sequencer());
  SEQUENCER_insert_seqtrack(NULL, pos, for_audiofiles);

  setCurrSeqtrack(pos);
}

void deleteSeqtrack(int seqtracknum){
  if (seqtracknum==-1)
    seqtracknum = ATOMIC_GET(root->song->curr_seqtracknum);
  
  if (seqtracknum < 0 || seqtracknum >= root->song->seqtracks.num_elements){
    handleError("Sequencer track #%d does not exist", seqtracknum);
    return;
  }

  if (root->song->seqtracks.num_elements==1){
    handleError("Must have at least one sequencer track");
    return;
  }    

  UNDO_OPEN_REC();{ // SEQUENCER_delete_seqtrack might delete a seqtrack plugin as well.
    ADD_UNDO(Sequencer());
    SEQUENCER_delete_seqtrack(seqtracknum);
  }UNDO_CLOSE();
}

void API_curr_seqtrack_has_changed(void){
  S7EXTRA_GET_FUNC(func, "FROM_C-call-me-when-curr-seqtrack-has-changed");
  
  S7CALL(void_int, func, ATOMIC_GET(root->song->curr_seqtracknum));
}

void setCurrSeqtrack(int seqtracknum){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return;

  int old = ATOMIC_GET(root->song->curr_seqtracknum);
  
  //if (true || seqtracknum != ATOMIC_GET(root->song->curr_seqtracknum)){ // We always go in here since the function is sometimes called just for the update() calls.
  
  if (seqtracknum != old){

    if (old >= 0 && old < root->song->seqtracks.num_elements){
      SEQTRACK_update_with_borders(getSeqtrackFromNum(old));
    }
      
    ATOMIC_SET(root->song->curr_seqtracknum, seqtracknum);

    SEQTRACK_update_with_borders(seqtrack);
    SEQUENCER_update(SEQUPDATE_HEADERS|SEQUPDATE_PLAYLIST|SEQUPDATE_BLOCKLIST);

    struct Patch *patch = seqtrack->patch;
    if(patch!=NULL)
      patch->instrument->PP_Update(patch->instrument, patch, false);

    if(root->song != NULL && root->song->tracker_windows!=NULL)
      root->song->tracker_windows->must_redraw_editor=true;
    else{
      R_ASSERT_NON_RELEASE(false);
    }

    API_curr_seqtrack_has_changed();
  }

}

int getCurrSeqtrack(void){
  return ATOMIC_GET(root->song->curr_seqtracknum);
}

int getNumSeqtracks(void){
  return root->song->seqtracks.num_elements;
}

int getTopmostVisibleSeqtrack(void){
  return root->song->topmost_visible_seqtrack;
}

void setTopmostVisibleSeqtrack(int new_topmost){
  if (new_topmost < 0 || new_topmost >= root->song->seqtracks.num_elements){
    handleError("setTopmostVisibleSeqtrack: Illegal seqtracknum: %d", new_topmost);
    return;
  }
        
  if(new_topmost==getTopmostVisibleSeqtrack())
    return;

  new_topmost = R_MIN(SEQUENCER_get_lowest_reasonable_topmost_seqtracknum(), new_topmost);
                                                       
  if(new_topmost==getTopmostVisibleSeqtrack())
    return;
  
  root->song->topmost_visible_seqtrack = new_topmost;
  SEQUENCER_update(SEQUPDATE_TRACKCOORDINATES);
}

void swapSeqtracks(int seqtracknum1, int seqtracknum2){
  struct SeqTrack *seqtrack1 = getSeqtrackFromNum(seqtracknum1);
  if (seqtrack1==NULL)
    return;
  
  struct SeqTrack *seqtrack2 = getSeqtrackFromNum(seqtracknum2);
  if (seqtrack2==NULL)
    return;

  ADD_UNDO(Sequencer());

  { 
    radium::PlayerPause pause(is_playing_song());
    radium::PlayerLock lock;
    root->song->seqtracks.elements[seqtracknum1] = seqtrack2;
    root->song->seqtracks.elements[seqtracknum2] = seqtrack1;
    if(seqtracknum1==0)
      if (root->song->use_sequencer_tempos_and_signatures==false && seqtrack2->for_audiofiles)
        root->song->use_sequencer_tempos_and_signatures = true;
  }

  SEQUENCER_update(SEQUPDATE_EVERYTHING);
}

int64_t getSeqtrackInstrument(int seqtracknum){
  struct SeqTrack *seqtrack = getAudioSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return -1;

  if (seqtrack->patch==NULL)
    return -1;

  struct SoundPlugin *plugin = (struct SoundPlugin*)seqtrack->patch->patchdata;  
  if (plugin==NULL)
    return -1;

  return seqtrack->patch->id;
}

bool seqtrackForAudiofiles(int seqtracknum){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return false;

  return seqtrack->for_audiofiles;
}

void setSeqtrackName(const_char *name, int seqtracknum){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return;

  if (seqtrack->patch != NULL)
    setInstrumentName(name, seqtrack->patch->id);
  else
    seqtrack->name = talloc_strdup(name);
}

const_char *getSeqtrackName(int seqtracknum){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return "";

  if (seqtrack->patch != NULL)
    return seqtrack->patch->name;
  else
    return seqtrack->name == NULL ? "" : seqtrack->name;
}

void undoSeqtrackNoteGain(int seqtracknum){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return;
  
  ADD_UNDO(EditorSeqtrackVolume(seqtracknum));
}

void setSeqtrackNoteGain(float gain, int seqtracknum){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return;

  {
    radium::PlayerLock lock;
    seqtrack->note_gain = gain;
    seqtrack->note_gain_has_changed_this_block = true;
  }
}

float getSeqtrackNoteGain(int seqtracknum){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return 1.0;

  return safe_float_read(&seqtrack->note_gain);
}

void setEditorSeqtrackMuted(bool muteit, int seqtracknum){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return;

  bool old_value = seqtrack->note_gain_muted < 0.1 ? true : false;
  
  if (muteit==old_value)
    return;

  ADD_UNDO(EditorSeqtrackVolume(seqtracknum));
  
  {
    radium::PlayerLock lock(is_playing_song());
    seqtrack->note_gain_muted = muteit ? 0.0 : 1.0;
    seqtrack->note_gain_has_changed_this_block = true;
  }
}

bool getEditorSeqtrackMuted(int seqtracknum){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return false;

  return seqtrack->note_gain_muted < 0.1 ? true : false;
}


// Recording
//////////////////////////////////////////


// seqtrack is recording

void setSeqtrackIsRecording(int seqtracknum, bool is_recording){
  struct SeqTrack *seqtrack = getAudioSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return;

  if (is_recording && get_num_recording_soundfile_channels(get_seqtrack_recording_config(seqtrack))==0){
    handleError("setSeqtrackIsRecording: Can not record since no channels are selected in the matrix");
    return;
  }

  SEQTRACK_set_recording(seqtrack, is_recording);
}

bool seqtrackIsRecording(int seqtracknum){
  struct SeqTrack *seqtrack = getAudioSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return false;

  return seqtrack->is_recording;
}


static void maybe_restart_recording(struct SeqTrack *seqtrack){
  if (seqtrack->is_recording==false)
    return;

  if (is_playing_song()==true)
    return;

  SEQTRACK_set_recording(seqtrack, false);
  SEQTRACK_set_recording(seqtrack, true);
}


// seqtrack using custom recording config

void setSeqtrackUseCustomRecordingConfig(int seqtracknum, bool use_custom){
  struct SeqTrack *seqtrack = getAudioSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return;

  if (seqtrack->use_custom_recording_config == use_custom)
    return;

  if (use_custom==true)
    seqtrack->custom_recording_config = root->song->default_recording_config;
  
  seqtrack->use_custom_recording_config = use_custom;

maybe_restart_recording(seqtrack);  
}

bool getSeqtrackUseCustomRecordingConfig(int seqtracknum){
  struct SeqTrack *seqtrack = getAudioSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return false;

  return seqtrack->use_custom_recording_config;
}


// seqtrack recording from system input

void setSeqtrackRecordFromSystemInput(int seqtracknum, bool record_from_system_input){
  struct SeqTrack *seqtrack = getAudioSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return;

  get_seqtrack_recording_config(seqtrack)->record_from_system_input = record_from_system_input;

  maybe_restart_recording(seqtrack);
}

bool getSeqtrackRecordFromSystemInput(int seqtracknum){
  struct SeqTrack *seqtrack = getAudioSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return false;

  return get_seqtrack_recording_config(seqtrack)->record_from_system_input;
}


// seqtrack recording matrix

void setSeqtrackRecordingMatrix(int seqtracknum, int input_channel, int soundfile_channel, bool enabled){
  struct SeqTrack *seqtrack = getAudioSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return;

  if (input_channel < 0 || input_channel >= NUM_CHANNELS_RECORDING_MATRIX){
    handleError("setSeqtrackRecordingMatrix: input_channel must be between 0 and %d, found %d", NUM_CHANNELS_RECORDING_MATRIX, input_channel);
    return;
  }
  
  if (soundfile_channel < 0 || soundfile_channel >= NUM_CHANNELS_RECORDING_MATRIX){
    handleError("setSeqtrackRecordingMatrix: soundfile_channel must be between 0 and %d, found %d", NUM_CHANNELS_RECORDING_MATRIX, soundfile_channel);
    return;
  }

  auto *config = get_seqtrack_recording_config(seqtrack);
  config->matrix[input_channel][soundfile_channel] = enabled;

  maybe_restart_recording(seqtrack);
}

bool getSeqtrackRecordingMatrix(int seqtracknum, int input_channel, int soundfile_channel){
  struct SeqTrack *seqtrack = getAudioSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return false;

  if (input_channel < 0 || input_channel >= NUM_CHANNELS_RECORDING_MATRIX){
    handleError("setSeqtrackRecordingMatrix: input_channel must be between 0 and %d, found %d", NUM_CHANNELS_RECORDING_MATRIX, input_channel);
    return false;
  }
  
  if (soundfile_channel < 0 || soundfile_channel >= NUM_CHANNELS_RECORDING_MATRIX){
    handleError("setSeqtrackRecordingMatrix: soundfile_channel must be between 0 and %d, found %d", NUM_CHANNELS_RECORDING_MATRIX, soundfile_channel);
    return false;
  }
  
  return get_seqtrack_recording_config(seqtrack)->matrix[input_channel][soundfile_channel];
}


// reset

void resetSeqtrackRecordingOptions(int seqtracknum){
 struct SeqTrack *seqtrack = getAudioSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return;

  reset_recording_config(get_seqtrack_recording_config(seqtrack));

  maybe_restart_recording(seqtrack);
}



// Sequencer track automation
//////////////////////////////////////////

static int add_seq_automation(int64_t time1, float value1, int64_t time2, float value2, int effect_num, int64_t instrument_id, int seqtracknum, int *nodenum1, int *nodenum2){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return -1;

  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return -1;

  if (effect_num < 0){
    handleError("Unknown effect number %d", effect_num);
    return -1;
  }
  
  struct SoundPlugin *plugin = (struct SoundPlugin*)patch->patchdata;
  R_ASSERT_RETURN_IF_FALSE2(plugin!=NULL, -1);

  if (effect_num >= plugin->type->num_effects + NUM_SYSTEM_EFFECTS){
    handleError("Unknown effect number %d. (Number of effects for %s: %d)", effect_num, patch->name, plugin->type->num_effects + NUM_SYSTEM_EFFECTS);
    return -1;
  }

  if (time1<0 || time2 <= time1){
    handleError("addSeqAutomation: Illegal time values. time1: %d. time2: %d", (int)time1, (int)time2);
    return -1;
  }
  
  ADD_UNDO(SeqtrackAutomations());

  return SEQTRACK_AUTOMATION_add_automation(seqtrack->seqtrackautomation, patch, effect_num, time1, value1, LOGTYPE_LINEAR, time2, value2, nodenum1, nodenum2);
}

int addSeqAutomation(int64_t time1, float value1, int64_t time2, float value2, int effect_num, int64_t instrument_id, int seqtracknum){
  return add_seq_automation(time1, value1, time2, value2, effect_num, instrument_id, seqtracknum, NULL, NULL);
}
  
dyn_t addSeqAutomation2(int64_t time1, float value1, int64_t time2, float value2, int effect_num, int64_t instrument_id, int seqtracknum){
  int nodenum1=-1, nodenum2=-1;
  int automationnum = add_seq_automation(time1, value1, time2, value2, effect_num, instrument_id, seqtracknum, &nodenum1, &nodenum2);
  
  hash_t *hash = HASH_create(3);
  HASH_put_int(hash, ":nodenum1", nodenum1);
  HASH_put_int(hash, ":nodenum2", nodenum2);
  HASH_put_int(hash, ":automationnum", automationnum);
  
  return DYN_create_hash(hash);
}

void replaceAllSeqAutomation(int64_t old_instrument, int64_t new_instrument){
  struct Patch *old_patch = getAudioPatchFromNum(old_instrument);
  if(old_patch==NULL)
    return;

  struct Patch *new_patch = getAudioPatchFromNum(new_instrument);
  if(new_patch==NULL)
    return;

  ADD_UNDO(SeqtrackAutomations());

  SEQTRACK_AUTOMATION_replace_all_automations(old_patch, new_patch);
}

int getNumSeqtrackAutomations(int seqtracknum){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return 0;

  return SEQTRACK_AUTOMATION_get_num_automations(seqtrack->seqtrackautomation);
}

#define VALIDATE_AUTOMATIONNUM(ret)                                     \
  if (automationnum < 0 || automationnum >= SEQTRACK_AUTOMATION_get_num_automations(seqtrack->seqtrackautomation)){ \
    handleError("There is no automation #%d in sequencer track #%d", automationnum, seqtracknum); \
    return ret;                                                         \
  }


#define VALIDATE_NODENUM(ret)                                           \
  if (nodenum < 0 || nodenum >= SEQTRACK_AUTOMATION_get_num_nodes(seqtrack->seqtrackautomation, automationnum)){ \
    handleError("There is no node #%d in automation #%d in sequencer track #%d", nodenum, automationnum, seqtracknum); \
    return ret;                                                          \
  }

#define VALIDATE_TIME(time,ret)                                 \
  if (time < 0){                                                \
    handleError("Time can not be negative: %d", (int)time);     \
    return ret;                                                 \
  }

#define VALIDATE_TIME2(time,ret)                                 \
  if (time < -1){                                                \
    handleError("Time can not be less than -1: %d", (int)time);     \
    return ret;                                                 \
  }

bool getSeqAutomationEnabled(int automationnum, int seqtracknum){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return -1;

  VALIDATE_AUTOMATIONNUM(-1);

  return SEQTRACK_AUTOMATION_is_enabled(seqtrack->seqtrackautomation, automationnum);
}

void setSeqAutomationEnabled(int automationnum, int seqtracknum, bool is_enabled){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return;

  VALIDATE_AUTOMATIONNUM();

  ADD_UNDO(SeqtrackAutomations());
    
  SEQTRACK_AUTOMATION_set_enabled(seqtrack->seqtrackautomation, automationnum, is_enabled);
}

int64_t getSeqAutomationInstrumentId(int automationnum, int seqtracknum){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return -1;

  VALIDATE_AUTOMATIONNUM(-1);

  struct Patch *patch = SEQTRACK_AUTOMATION_get_patch(seqtrack->seqtrackautomation, automationnum);
  if (patch==NULL)
    return 0;

  return patch->id;
}

int getSeqAutomationEffectNum(int automationnum, int seqtracknum){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return -1;

  VALIDATE_AUTOMATIONNUM(-1);

  return SEQTRACK_AUTOMATION_get_effect_num(seqtrack->seqtrackautomation, automationnum);
}

float getSeqAutomationValue(int nodenum, int automationnum, int seqtracknum){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return -1;

  VALIDATE_AUTOMATIONNUM(-1);
  VALIDATE_NODENUM(-1);

  return SEQTRACK_AUTOMATION_get_value(seqtrack->seqtrackautomation, automationnum, nodenum);
}

int64_t getSeqAutomationTime(int nodenum, int automationnum, int seqtracknum){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return -1;

  VALIDATE_AUTOMATIONNUM(-1);
  VALIDATE_NODENUM(-1);

  return SEQTRACK_AUTOMATION_get_seqtime(seqtrack->seqtrackautomation, automationnum, nodenum);
}

int getSeqAutomationLogtype(int nodenum, int automationnum, int seqtracknum){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return -1;

  VALIDATE_AUTOMATIONNUM(-1);
  VALIDATE_NODENUM(-1);

  return SEQTRACK_AUTOMATION_get_logtype(seqtrack->seqtrackautomation, automationnum, nodenum);
}

int getNumSeqtrackAutomationNodes(int automationnum, int seqtracknum){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return -1;

  VALIDATE_AUTOMATIONNUM(-1);

  return SEQTRACK_AUTOMATION_get_num_nodes(seqtrack->seqtrackautomation, automationnum);
}

int addSeqAutomationNode(int64_t time, float value, int logtype, int automationnum, int seqtracknum){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return -1;

  VALIDATE_AUTOMATIONNUM(-1);
  VALIDATE_TIME(time, -1)
    
  ADD_UNDO(SeqtrackAutomations());

  return SEQTRACK_AUTOMATION_add_node(seqtrack->seqtrackautomation, automationnum, time, value, logtype);
}

void deleteSeqAutomationNode(int nodenum, int automationnum, int seqtracknum){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return;

  VALIDATE_AUTOMATIONNUM();
  VALIDATE_NODENUM();

  ADD_UNDO(SeqtrackAutomations());

  SEQTRACK_AUTOMATION_delete_node(seqtrack->seqtrackautomation, automationnum, nodenum);
}

void setCurrSeqAutomationNode(int nodenum, int automationnum, int seqtracknum){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return;
  
  VALIDATE_AUTOMATIONNUM();
  VALIDATE_NODENUM();

  //printf("setCurrSeqAutomationNode\n");
  
  SEQTRACK_AUTOMATION_set_curr_node(seqtrack->seqtrackautomation, automationnum, nodenum);
}

void cancelCurrSeqAutomationNode(int automationnum, int seqtracknum){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return;

  VALIDATE_AUTOMATIONNUM();

  //printf("cancelCurrSeqAutomationNode\n");
  
  SEQTRACK_AUTOMATION_cancel_curr_node(seqtrack->seqtrackautomation, automationnum);
}

void setCurrSeqAutomation(int automationnum, int seqtracknum){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return;

  VALIDATE_AUTOMATIONNUM();

  //printf("setCurrSeqAutomation\n");
  SEQTRACK_AUTOMATION_set_curr_automation(seqtrack, automationnum);
}

void cancelCurrSeqAutomation(void){
  //printf("   cancelCurrSeqAutomation called\n");
  //printf("cancelCurrSeqAutomation\n");
  SEQTRACK_AUTOMATION_cancel_curr_automation();
}

int getCurrSeqAutomationSeqtrack(void){
  int ret = 0;

  ALL_SEQTRACKS_FOR_EACH(){

    if (SEQTRACK_AUTOMATION_get_curr_automation(seqtrack->seqtrackautomation) != -1)
      return ret;

    ret++;

  }END_ALL_SEQTRACKS_FOR_EACH;

  return -1;

}

int getCurrSeqAutomation(void){
  ALL_SEQTRACKS_FOR_EACH(){

    int maybe = SEQTRACK_AUTOMATION_get_curr_automation(seqtrack->seqtrackautomation);

    if (maybe != -1)
      return maybe;

  }END_ALL_SEQTRACKS_FOR_EACH;

  return -1;
}

void setSeqAutomationNode(int64_t time, float value, int logtype, int nodenum, int automationnum, int seqtracknum){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return;

  VALIDATE_AUTOMATIONNUM();
  VALIDATE_NODENUM();
  VALIDATE_TIME(time,)
    
  SEQTRACK_AUTOMATION_set(seqtrack, automationnum, nodenum, time, R_BOUNDARIES(0, value, 1), logtype);
}

float getSeqAutomationNodeX(int nodenum, int automationnum, int seqtracknum){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return 0;

  VALIDATE_AUTOMATIONNUM(0);
  VALIDATE_NODENUM(0);

  return SEQTRACK_AUTOMATION_get_node_x(seqtrack->seqtrackautomation, seqtrack, automationnum, nodenum);
}

float getSeqAutomationNodeY(int nodenum, int automationnum, int seqtracknum){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return 0;

  VALIDATE_AUTOMATIONNUM(0);
  VALIDATE_NODENUM(0);

  return SEQTRACK_AUTOMATION_get_node_y(seqtrack->seqtrackautomation, seqtracknum, automationnum, nodenum);
}



// seqblock gain and so forth
/////////////////////////////

static void maybe_make_seqblock_undo(int64_t seqblockid){
  static int64_t last_seqblockid = -1;
  static double last_undo_time = -1;
  static int last_undo_num = -1;

  double curr_time = TIME_get_ms();
  int curr_undo_num = Undo_num_undos();

  if (seqblockid != last_seqblockid)
    goto do_undo;
  
  if (curr_time > last_undo_time+1000)
    goto do_undo;

  if (curr_undo_num != last_undo_num)
    goto do_undo;

  return;
  
 do_undo:  
  UNDO_SEQBLOCK_2(seqblockid);
  last_undo_num = curr_undo_num + 1;
  last_undo_time = curr_time;
  last_seqblockid = seqblockid;
}

void setSeqblockGain(float new_gain, int64_t seqblockid){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromIdA(seqblockid, &seqtrack);
  if (seqblock==NULL)
    return;

  maybe_make_seqblock_undo(seqblockid);
  SEQBLOCK_set_gain(seqtrack, seqblock, new_gain);
}

float getSeqblockGain(int64_t seqblockid){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromIdA(seqblockid, &seqtrack);
  if (seqblock==NULL)
    return 1.0;

  return SEQBLOCK_get_gain(seqtrack, seqblock);
}

float getMaxSeqblockSampleGain(int64_t seqblockid){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getAudioSeqblockFromIdA(seqblockid, &seqtrack);
  if (seqblock==NULL)
    return 1.0;

  return SEQBLOCK_get_max_sample_gain(seqtrack, seqblock);
}

void setSeqblockGrainStrictNoJitter(bool new_gf, int64_t seqblockid){ //int64_t seqblockid){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getAudioSeqblockFromIdA(seqblockid, &seqtrack);
  if (seqblock==NULL)
    return;

  struct SoundPlugin *plugin = (struct SoundPlugin*)seqtrack->patch->patchdata;  
  R_ASSERT_RETURN_IF_FALSE(plugin!=NULL);

  maybe_make_seqblock_undo(seqblockid);
  SEQTRACKPLUGIN_set_grain_strict_no_jitter(plugin, seqblock->sample_id, new_gf);
}

bool getSeqblockGrainStrictNoJitter(int64_t seqblockid){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getAudioSeqblockFromIdA(seqblockid, &seqtrack);
  if (seqblock==NULL)
    return false;

  struct SoundPlugin *plugin = (struct SoundPlugin*)seqtrack->patch->patchdata;  
  R_ASSERT_RETURN_IF_FALSE2(plugin!=NULL, false);

  return SEQTRACKPLUGIN_get_grain_strict_no_jitter(plugin, seqblock->sample_id);
}

void setSeqblockGrainOverlap(double new_gf, int64_t seqblockid){ //int64_t seqblockid){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getAudioSeqblockFromIdA(seqblockid, &seqtrack);
  if (seqblock==NULL)
    return;

  struct SoundPlugin *plugin = (struct SoundPlugin*)seqtrack->patch->patchdata;  
  R_ASSERT_RETURN_IF_FALSE(plugin!=NULL);

  maybe_make_seqblock_undo(seqblockid);
  SEQTRACKPLUGIN_set_grain_overlap(plugin, seqblock->sample_id, new_gf);
}

double getSeqblockGrainOverlap(int64_t seqblockid){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getAudioSeqblockFromIdA(seqblockid, &seqtrack);
  if (seqblock==NULL)
    return 2.0;

  struct SoundPlugin *plugin = (struct SoundPlugin*)seqtrack->patch->patchdata;  
  R_ASSERT_RETURN_IF_FALSE2(plugin!=NULL, 50);

  return SEQTRACKPLUGIN_get_grain_overlap(plugin, seqblock->sample_id);
}

void setSeqblockGrainLength(double new_gf, int64_t seqblockid){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getAudioSeqblockFromIdA(seqblockid, &seqtrack);
  if (seqblock==NULL)
    return;

  struct SoundPlugin *plugin = (struct SoundPlugin*)seqtrack->patch->patchdata;  
  R_ASSERT_RETURN_IF_FALSE(plugin!=NULL);

  maybe_make_seqblock_undo(seqblockid);
  SEQTRACKPLUGIN_set_grain_length(plugin, seqblock->sample_id, new_gf);
}

double getSeqblockGrainLength(int64_t seqblockid){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getAudioSeqblockFromIdA(seqblockid, &seqtrack);
  if (seqblock==NULL)
    return 50.0;

  struct SoundPlugin *plugin = (struct SoundPlugin*)seqtrack->patch->patchdata;  
  R_ASSERT_RETURN_IF_FALSE2(plugin!=NULL, 50);

  return SEQTRACKPLUGIN_get_grain_length(plugin, seqblock->sample_id);
}

void setSeqblockGrainJitter(double new_jitter, int64_t seqblockid){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getAudioSeqblockFromIdA(seqblockid, &seqtrack);
  if (seqblock==NULL)
    return;

  struct SoundPlugin *plugin = (struct SoundPlugin*)seqtrack->patch->patchdata;  
  R_ASSERT_RETURN_IF_FALSE(plugin!=NULL);

  maybe_make_seqblock_undo(seqblockid);
  SEQTRACKPLUGIN_set_grain_jitter(plugin, seqblock->sample_id, new_jitter);
}

double getSeqblockGrainJitter(int64_t seqblockid){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getAudioSeqblockFromIdA(seqblockid, &seqtrack);
  if (seqblock==NULL)
    return 50.0;

  struct SoundPlugin *plugin = (struct SoundPlugin*)seqtrack->patch->patchdata;  
  R_ASSERT_RETURN_IF_FALSE2(plugin!=NULL, 50);

  return SEQTRACKPLUGIN_get_grain_jitter(plugin, seqblock->sample_id);
}

void setSeqblockGrainRamp(double new_ramp, int64_t seqblockid){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getAudioSeqblockFromIdA(seqblockid, &seqtrack);
  if (seqblock==NULL)
    return;

  struct SoundPlugin *plugin = (struct SoundPlugin*)seqtrack->patch->patchdata;  
  R_ASSERT_RETURN_IF_FALSE(plugin!=NULL);

  maybe_make_seqblock_undo(seqblockid);
  SEQTRACKPLUGIN_set_grain_ramp(plugin, seqblock->sample_id, new_ramp);
}

double getSeqblockGrainRamp(int64_t seqblockid){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getAudioSeqblockFromIdA(seqblockid, &seqtrack);
  if (seqblock==NULL)
    return 0.33;

  struct SoundPlugin *plugin = (struct SoundPlugin*)seqtrack->patch->patchdata;  
  R_ASSERT_RETURN_IF_FALSE2(plugin!=NULL, 0.33);

  return SEQTRACKPLUGIN_get_grain_ramp(plugin, seqblock->sample_id);
}

int getSeqblockResamplerType(int64_t seqblockid){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getAudioSeqblockFromIdA(seqblockid, &seqtrack);
  if (seqblock==NULL)
    return RESAMPLER_SINC1;

  struct SoundPlugin *plugin = (struct SoundPlugin*)seqtrack->patch->patchdata;  
  R_ASSERT_RETURN_IF_FALSE2(plugin!=NULL, RESAMPLER_SINC1);

  return SEQTRACKPLUGIN_get_resampler_type(plugin, seqblock->sample_id);
}




// Sequencer block volume envelope
//////////////////////////////////

#define VALIDATE_SEQBLOCK_AUTOMATIONNUM(ret)                                     \
  if (automationnum < 0 || automationnum >= NUM_SATS){                  \
    handleError("There is no automationnum #%d in seqblock #%d in seqtrack #%d", automationnum, seqblocknum, seqtracknum); \
    return ret;                                                         \
  }                                                                     \
  if (seqblock->block!=NULL && automationnum >= NUM_EDITOR_BLOCK_SATS) {                     \
    handleError("Automation \"%s\" in sequencer block #%d in sequencer track #%d is not supported since it is not an audiofile seqblock", sat_to_string((enum Seqblock_Automation_Type)automationnum), seqblocknum, seqtracknum); \
    return ret;                                                         \
  }

#define VALIDATE_SEQBLOCK_AUTOMATIONNUM2(ret)                                     \
  if (automationnum < 0 || automationnum >= NUM_SATS){                  \
    handleError("There is no automationnum #%d in seqblock with id #%d", automationnum, (int)seqblockid); \
    return ret;                                                         \
  }                                                                     \
  if (seqblock->block!=NULL && automationnum >= NUM_EDITOR_BLOCK_SATS) {                     \
    handleError("Automation \"%s\" in seqblock with id #%d is not supported since it is not an audiofile seqblock", sat_to_string((enum Seqblock_Automation_Type)automationnum), (int)seqblockid); \
    return ret;                                                         \
  }

#define VALIDATE_ENV_NODENUM(ret)                                           \
  if (nodenum < 0 || nodenum >= SEQBLOCK_AUTOMATION_get_num_nodes(seqblock->automations[automationnum])){ \
    handleError("There is no node #%d for %s in sequencer block #%d in sequencer track #%d", nodenum, sat_to_string((enum Seqblock_Automation_Type)automationnum), seqblocknum, seqtracknum); \
    return ret;                                                          \
  }

#define VALIDATE_ENV_TIME(time,ret)                                 \
  if (time < 0){                                                \
    handleError("Time can not be negative: %d", (int)time);     \
    return ret;                                                 \
  }

#define VALIDAT_ENV_TIME2(time,ret)                                 \
  if (time < -1){                                                \
    handleError("Time can not be less than -1: %d", (int)time);     \
    return ret;                                                 \
  }

int getNumSeqblockAutomations(int seqblocknum, int seqtracknum){
 struct SeqBlock *seqblock = getSeqblockFromNum(seqblocknum, seqtracknum);
  if (seqblock==NULL)
    return 0;

  return SEQBLOCK_num_automations(seqblock);
}

const_char* getSeqblockAutomationName(int automationnum){
  if (automationnum < 0 || automationnum >= NUM_SATS){
    handleError("There is no automationnum #%d", automationnum);
    return "illegal";
  }

  return sat_to_string((enum Seqblock_Automation_Type)automationnum);
}

bool getSeqblockAutomationEnabled(int automationnum, int64_t seqblockid){
  struct SeqBlock *seqblock = getSeqblockFromId(seqblockid);
  if (seqblock==NULL)
    return false;

  VALIDATE_SEQBLOCK_AUTOMATIONNUM2(false);

  return RT_seqblock_automation_is_enabled(seqblock->automations[automationnum]);
  //return seqblock->envelope_enabled;
}
void setSeqblockAutomationEnabled(bool is_enabled, int automationnum, int64_t seqblockid){
  struct SeqBlock *seqblock = getSeqblockFromId(seqblockid);
  if (seqblock==NULL)
    return;

  VALIDATE_SEQBLOCK_AUTOMATIONNUM2();

  if (RT_seqblock_automation_is_enabled(seqblock->automations[automationnum])==is_enabled)
    return;

  UNDO_SEQBLOCK_2(seqblockid);

  SEQBLOCK_AUTOMATION_set_enabled(seqblock->automations[automationnum], is_enabled);
}

double getSeqblockAutomationMinValue(int automationnum, int seqblocknum, int seqtracknum){
  struct SeqBlock *seqblock = getSeqblockFromNum(seqblocknum, seqtracknum);;
  if (seqblock==NULL)
    return 0;

  VALIDATE_SEQBLOCK_AUTOMATIONNUM(0);

  return SEQBLOCK_AUTOMATION_get_min_value(seqblock->automations[automationnum]);
}

double getSeqblockAutomationMaxValue(int automationnum, int seqblocknum, int seqtracknum){
  struct SeqBlock *seqblock = getSeqblockFromNum(seqblocknum, seqtracknum);;
  if (seqblock==NULL)
    return 1;

  VALIDATE_SEQBLOCK_AUTOMATIONNUM(1);

  return SEQBLOCK_AUTOMATION_get_max_value(seqblock->automations[automationnum]);
}

double getSeqblockAutomationDefaultValue(int automationnum, int seqblocknum, int seqtracknum){
  struct SeqBlock *seqblock = getSeqblockFromNum(seqblocknum, seqtracknum);;
  if (seqblock==NULL)
    return -1;

  VALIDATE_SEQBLOCK_AUTOMATIONNUM(0);

  return SEQBLOCK_AUTOMATION_get_default_value(seqblock->automations[automationnum]);
}

const_char* getSeqblockAutomationDisplayString(double value, int automationnum, int seqblocknum, int seqtracknum){
  struct SeqBlock *seqblock = getSeqblockFromNum(seqblocknum, seqtracknum);
  if (seqblock==NULL)
    return "unknown";

  VALIDATE_SEQBLOCK_AUTOMATIONNUM("unknown");

  return SEQBLOCK_AUTOMATION_get_display_string(seqblock->automations[automationnum], value);
}

float getSeqblockAutomationValue(int nodenum, int automationnum, int seqblocknum, int seqtracknum){
  struct SeqBlock *seqblock = getSeqblockFromNum(seqblocknum, seqtracknum);;
  if (seqblock==NULL)
    return -1;

  VALIDATE_SEQBLOCK_AUTOMATIONNUM(-1);
  VALIDATE_ENV_NODENUM(-1);

  return SEQBLOCK_AUTOMATION_get_value(seqblock->automations[automationnum], nodenum);
}

float getSeqblockAutomationValueForTime(int64_t time, int automationnum, int seqblocknum, int seqtracknum){
  struct SeqBlock *seqblock = getSeqblockFromNum(seqblocknum, seqtracknum);;
  if (seqblock==NULL)
    return -1;

  VALIDATE_SEQBLOCK_AUTOMATIONNUM(-1);
  VALIDATE_ENV_TIME(time, -1)

  return SEQBLOCK_AUTOMATION_get_value_for_time(seqblock->automations[automationnum], time);
}

int64_t getSeqblockAutomationTime(int nodenum, int automationnum, int seqblocknum, int seqtracknum){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack, false);
  if (seqblock==NULL)
    return -1;

  VALIDATE_SEQBLOCK_AUTOMATIONNUM(-1);
  VALIDATE_ENV_NODENUM(-1);

  return SEQBLOCK_AUTOMATION_get_seqtime(seqblock->automations[automationnum], nodenum);
}

int getSeqblockAutomationLogtype(int nodenum, int automationnum, int seqblocknum, int seqtracknum){
  struct SeqBlock *seqblock = getSeqblockFromNum(seqblocknum, seqtracknum);;
  if (seqblock==NULL)
    return -1;

  VALIDATE_SEQBLOCK_AUTOMATIONNUM(-1);
  VALIDATE_ENV_NODENUM(-1);

  return SEQBLOCK_AUTOMATION_get_logtype(seqblock->automations[automationnum], nodenum);
}

int getNumSeqblockAutomationNodes(int automationnum, int seqblocknum, int seqtracknum){
  struct SeqBlock *seqblock = getSeqblockFromNum(seqblocknum, seqtracknum);;
  if (seqblock==NULL)
    return -1;

  VALIDATE_SEQBLOCK_AUTOMATIONNUM(0);

  return SEQBLOCK_AUTOMATION_get_num_nodes(seqblock->automations[automationnum]);
}

int addSeqblockAutomationNode(int64_t time, float db, int logtype, int automationnum, int seqblocknum, int seqtracknum){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack, false);
  if (seqblock==NULL)
    return -1;

  VALIDATE_SEQBLOCK_AUTOMATIONNUM(-1);
  VALIDATE_ENV_TIME(time, -1)
    
  undoSeqblockAutomation(automationnum, seqblocknum, seqtracknum);

  return SEQBLOCK_AUTOMATION_add_node(seqblock->automations[automationnum], time, db, logtype);
}

void deleteSeqblockAutomationNode(int nodenum, int automationnum, int seqblocknum, int seqtracknum){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack, false);
  if (seqblock==NULL)
    return;

  VALIDATE_SEQBLOCK_AUTOMATIONNUM();

  if (nodenum==0 || nodenum==SEQBLOCK_AUTOMATION_get_num_nodes(seqblock->automations[automationnum])-1){
    undoSeqblockAutomation(automationnum, seqblocknum, seqtracknum);
    
    SEQBLOCK_AUTOMATION_set(seqblock->automations[automationnum],
                            nodenum,
                            SEQBLOCK_AUTOMATION_get_seqtime(seqblock->automations[automationnum], nodenum),
                            SEQBLOCK_AUTOMATION_get_default_value(seqblock->automations[automationnum]),
                            SEQBLOCK_AUTOMATION_get_logtype(seqblock->automations[automationnum], nodenum)
                            );
    return;
  }


  VALIDATE_ENV_NODENUM();

  undoSeqblockAutomation(automationnum, seqblocknum, seqtracknum);

  SEQBLOCK_AUTOMATION_delete_node(seqblock->automations[automationnum], nodenum);
}

void setCurrSeqblockAutomationNode(int nodenum, int automationnum, int seqblocknum, int seqtracknum){
  struct SeqBlock *seqblock = getSeqblockFromNum(seqblocknum, seqtracknum);;
  if (seqblock==NULL)
    return;

  VALIDATE_SEQBLOCK_AUTOMATIONNUM();
  VALIDATE_ENV_NODENUM();

  SEQBLOCK_AUTOMATION_set_curr_node(seqblock->automations[automationnum], nodenum);
}

void cancelCurrSeqblockAutomationNode(int automationnum, int seqblocknum, int seqtracknum){
  struct SeqBlock *seqblock = getSeqblockFromNum(seqblocknum, seqtracknum);;
  if (seqblock==NULL)
    return;

  VALIDATE_SEQBLOCK_AUTOMATIONNUM();

  SEQBLOCK_AUTOMATION_cancel_curr_node(seqblock->automations[automationnum]);
}

void setCurrSeqblockAutomation(int automationnum, int seqblocknum, int seqtracknum){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack, false);
  if (seqblock==NULL)
    return;

  VALIDATE_SEQBLOCK_AUTOMATIONNUM();

  SEQBLOCK_AUTOMATION_set_curr_automation(seqtrack, seqblock, seqblock->automations[automationnum]);
}

void cancelCurrSeqblockAutomation(void){
  //printf("   cancelCurrSeqblockAutomation called\n");
  SEQBLOCK_AUTOMATION_cancel_curr_automation();
}

void setSeqblockAutomationNode(int64_t time, float db, int logtype, int nodenum, int automationnum, int seqblocknum, int seqtracknum){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack, false);
  if (seqblock==NULL)
    return;

  VALIDATE_SEQBLOCK_AUTOMATIONNUM();
  VALIDATE_ENV_NODENUM();
  VALIDATE_ENV_TIME(time,)
    
  SEQBLOCK_AUTOMATION_set(seqblock->automations[automationnum], nodenum, time, db, logtype);
}

float getSeqblockAutomationNodeX(int nodenum, int automationnum, int seqblocknum, int seqtracknum){
  struct SeqBlock *seqblock = getSeqblockFromNum(seqblocknum, seqtracknum);;
  if (seqblock==NULL)
    return 0;

  VALIDATE_SEQBLOCK_AUTOMATIONNUM(0);
  VALIDATE_ENV_NODENUM(0);

  return SEQBLOCK_AUTOMATION_get_node_x(seqblock->automations[automationnum], nodenum);
}

float getSeqblockAutomationNodeY(int nodenum, int automationnum, int seqblocknum, int seqtracknum){
  struct SeqBlock *seqblock = getSeqblockFromNum(seqblocknum, seqtracknum);;
  if (seqblock==NULL)
    return 0;

  VALIDATE_SEQBLOCK_AUTOMATIONNUM(0);
  VALIDATE_ENV_NODENUM(0);

  return SEQBLOCK_AUTOMATION_get_node_y(seqblock->automations[automationnum], seqtracknum, nodenum);
}


// sequencer tempo automation
//////////////////////////////////////////

void undoSeqtempo(void){
  ADD_UNDO(SongTempoAutomation());
}

float getSeqtempoAreaX1(void){
  return SEQTEMPO_get_x1();
}
float getSeqtempoAreaY1(void){
  return SEQTEMPO_get_y1();
}
float getSeqtempoAreaX2(void){
  return SEQTEMPO_get_x2();
}
float getSeqtempoAreaY2(void){
  return SEQTEMPO_get_y2();
}
float getSeqtemponodeX(int nodenum){
  if (nodenum < 0 || nodenum >= TEMPOAUTOMATION_get_num_nodes()){
    handleError("There is no tempo node #%d", nodenum);
    return 0.0;
  }
  return TEMPOAUTOMATION_get_node_x(nodenum);
}
float getSeqtemponodeY(int nodenum){
  if (nodenum < 0 || nodenum >= TEMPOAUTOMATION_get_num_nodes()){
    handleError("There is no tempo node #%d", nodenum);
    return 0.0;
  }
  return TEMPOAUTOMATION_get_node_y(nodenum);
}
void setSeqtempoVisible(bool visible){
  SEQTEMPO_set_visible(visible);
}
bool seqtempoVisible(void){
  return SEQTEMPO_is_visible();
}

double getSeqtempoValue(int nodenum){
  if (nodenum < 0 || nodenum >= TEMPOAUTOMATION_get_num_nodes()){
    handleError("There is no tempo node #%d", nodenum);
    return 0.0;
  }
  return TEMPOAUTOMATION_get_value(nodenum);
}
double getSeqtempoAbstime(int nodenum){
  if (nodenum < 0 || nodenum >= TEMPOAUTOMATION_get_num_nodes()){
    handleError("There is no tempo node #%d", nodenum);
    return 0.0;
  }
  return TEMPOAUTOMATION_get_abstime(nodenum);
}
int getSeqtempoLogtype(int nodenum){
  if (nodenum < 0 || nodenum >= TEMPOAUTOMATION_get_num_nodes()){
    handleError("There is no tempo node #%d", nodenum);
    return 0;
  }
  return TEMPOAUTOMATION_get_logtype(nodenum);
}
int getNumSeqtemponodes(void){
  return TEMPOAUTOMATION_get_num_nodes();
}
int addSeqtemponode(double abstime, double value, int logtype){
  VALIDATE_TIME(abstime, -1);
  undoSeqtempo();
  int ret = TEMPOAUTOMATION_add_node(abstime, value, logtype);
  if (ret==-1)
    UNDO_CANCEL_LAST_UNDO();
  return ret;
}
void deleteSeqtemponode(int nodenum){
  if (nodenum < 0 || nodenum >= TEMPOAUTOMATION_get_num_nodes()){
    handleError("There is no tempo node #%d", nodenum);
    return;
  }
  return TEMPOAUTOMATION_delete_node(nodenum);
}
void setCurrSeqtemponode(int nodenum){
  if (nodenum < -1 || nodenum >= TEMPOAUTOMATION_get_num_nodes()){
    handleError("There is no tempo node #%d", nodenum);
    return;
  }
  TEMPOAUTOMATION_set_curr_node(nodenum);
}
void setSeqtemponode(double abstime, double value, int logtype, int nodenum){
  VALIDATE_TIME(abstime,);
  if (nodenum < 0 || nodenum >= TEMPOAUTOMATION_get_num_nodes()){
    handleError("There is no tempo node #%d", nodenum);
    return;
  }
  return TEMPOAUTOMATION_set(nodenum, abstime, value, logtype);
}
void setSeqtempoLength(double end_time, bool do_shrink){
  VALIDATE_TIME(end_time,)
  return TEMPOAUTOMATION_set_length(end_time, do_shrink);
}
double getSeqtempoLength(void){
  return TEMPOAUTOMATION_get_length();
}
int64_t getSeqtempoAbsabstime(double abstime){
  VALIDATE_TIME(abstime,-1);
  return TEMPOAUTOMATION_get_absabstime(abstime);
}

double getSeqtempoMaxTempo(void){
  return TEMPOAUTOMATION_get_max_tempo();
}
void setSeqtempoMaxTempo(double max_tempo){
  TEMPOAUTOMATION_set_max_tempo(max_tempo);
}


// sequencer timeline, looping, and punch in/out
//

float getSeqtimelineAreaX1(void){
  return SEQTIMELINE_get_x1();
}
float getSeqtimelineAreaY1(void){
  return SEQTIMELINE_get_y1();
}
float getSeqtimelineAreaX2(void){
  return SEQTIMELINE_get_x2();
}
float getSeqtimelineAreaY2(void){
  return SEQTIMELINE_get_y2();
}


// loop
void setSeqlooping(bool do_loop){
  SEQUENCER_set_looping(do_loop);
}

bool isSeqlooping(void){
  return SEQUENCER_is_looping();
}

void setSeqloopingStart(int64_t start){
  SEQUENCER_set_loop_start(start);
}

int64_t getSeqloopingStart(void){
  return SEQUENCER_get_loop_start();
}

void setSeqloopingEnd(int64_t end){
  SEQUENCER_set_loop_end(end);
}

int64_t getSeqloopingEnd(void){
  return SEQUENCER_get_loop_end();
}

// punch in/out
void setSeqpunching(bool do_loop){
  SEQUENCER_set_punching(do_loop);
}

bool isSeqpunching(void){
  return SEQUENCER_is_punching();
}

void setSeqpunchingStart(int64_t start){
  SEQUENCER_set_punch_start(start);
}

int64_t getSeqpunchingStart(void){
  return SEQUENCER_get_punch_start();
}

void setSeqpunchingEnd(int64_t end){
  SEQUENCER_set_punch_end(end);
}

int64_t getSeqpunchingEnd(void){
  return SEQUENCER_get_punch_end();
}



// seqtracks
//

float getSeqtrackX1(int seqtracknum){
  //printf("getSeqtrackX1. num_elements: %d. seqtracknum: %d\n",root->song->seqtracks.num_elements,seqtracknum);
  if (seqtracknum < 0 || seqtracknum >= root->song->seqtracks.num_elements){
    handleError("Sequencer track #%d does not exist", seqtracknum);
    return 0;
  }
  return SEQTRACK_get_x1(seqtracknum);
}

float getSeqtrackX2(int seqtracknum){
  if (seqtracknum < 0 || seqtracknum >= root->song->seqtracks.num_elements){
    handleError("Sequencer track #%d does not exist", seqtracknum);
    return 0;
  }
  return SEQTRACK_get_x2(seqtracknum);
}

float getSeqtrackY1(int seqtracknum){
  if (seqtracknum < 0 || seqtracknum >= root->song->seqtracks.num_elements){
    handleError("Sequencer track #%d does not exist", seqtracknum);
    return 0;
  }
  return SEQTRACK_get_y1(seqtracknum);
}

float getSeqtrackY2(int seqtracknum){
  if (seqtracknum < 0 || seqtracknum >= root->song->seqtracks.num_elements){
    handleError("Sequencer track #%d does not exist", seqtracknum);
    return 0;
  }
  return SEQTRACK_get_y2(seqtracknum);
}

int getSeqtrackFromY(int y){
  for(int seqtracknum=0;seqtracknum<getNumSeqtracks();seqtracknum++){
    float y1 = getSeqtrackY1(seqtracknum);
    float y2 = getSeqtrackY2(seqtracknum);
    //printf("y1: %f / %f,%d / %f\n", y1, tevent.x, y, y2);
    if (y>=y1 && y <= y2)
      return seqtracknum;
  }

  return -1;
}

int getSeqtrackMinHeightType(int seqtracknum){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return 0;

  return (int)seqtrack->min_height_type;
}

void setSeqtrackMinHeightType(int seqtracknum, int new_type){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return;

  if (new_type < SHT_CUSTOM || new_type >= NUM_SHTs){
    handleError("setSeqtrackMinHeightType: Illegal type %d", new_type);
    return;
  }
  
  if (seqtrack->min_height_type==new_type)
    return;

  if(new_type==SHT_CUSTOM)
    seqtrack->custom_min_height = double(SEQTRACK_get_y2(seqtracknum) - SEQTRACK_get_y1(seqtracknum)) / (double)root->song->tracker_windows->systemfontheight;
  
  seqtrack->min_height_type = (enum SeqtrackHeightType)new_type;
  
  SEQUENCER_update(SEQUPDATE_TRACKCOORDINATES);
}

int getSeqtrackMaxHeightType(int seqtracknum){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return 0;

  return (int)seqtrack->max_height_type;
}

void setSeqtrackMaxHeightType(int seqtracknum, int new_type){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return;

  if (new_type < SHT_CUSTOM || new_type >= NUM_SHTs){
    handleError("setSeqtrackMaxHeightType: Illegal type %d", new_type);
    return;
  }
  
  if (seqtrack->max_height_type==new_type)
    return;

  if(new_type==SHT_CUSTOM)
    seqtrack->custom_max_height = double(SEQTRACK_get_y2(seqtracknum) - SEQTRACK_get_y1(seqtracknum)) / (double)root->song->tracker_windows->systemfontheight;
  
  seqtrack->max_height_type = (enum SeqtrackHeightType)new_type;
  
  SEQUENCER_update(SEQUPDATE_TRACKCOORDINATES);
}



static bool g_smooth_sequencer_scrolling_enabled = false;

bool smoothSequencerScrollingEnabled(void){
  static bool has_inited = false;

  if (has_inited==false){
    g_smooth_sequencer_scrolling_enabled = SETTINGS_read_bool("smooth_sequencer_scrolling_enabled", g_smooth_sequencer_scrolling_enabled);
    has_inited = true;
  }

  return g_smooth_sequencer_scrolling_enabled;
}

void setSmoothSequencerScrollingEnabled(bool doit){
  if (doit != g_smooth_sequencer_scrolling_enabled){
    g_smooth_sequencer_scrolling_enabled = doit;
    SETTINGS_write_bool("smooth_sequencer_scrolling_enabled", doit);
    PREFERENCES_update(); // ??
  }
}


static bool g_autoscroll_sequencer_to_make_playcursor_visible = false;

bool autoscrollSequencerToMakePlaycursorVisible(void){
  static bool has_inited = false;

  if (has_inited==false){
    g_autoscroll_sequencer_to_make_playcursor_visible = SETTINGS_read_bool("autoscroll_sequencer_to_make_playcursor_visible", g_autoscroll_sequencer_to_make_playcursor_visible);
    has_inited = true;
  }

  return g_autoscroll_sequencer_to_make_playcursor_visible;
}

void setAutoscrollSequencerToMakePlaycursorVisible(bool doit){
  if (doit != g_autoscroll_sequencer_to_make_playcursor_visible){
    g_autoscroll_sequencer_to_make_playcursor_visible = doit;
    SETTINGS_write_bool("autoscroll_sequencer_to_make_playcursor_visible", doit);
    PREFERENCES_update(); // ??
  }
}



static DEFINE_ATOMIC(bool, g_use_jack_transport) = false;

bool useJackTransport(void){
  static bool has_inited = false;

  if (has_inited==false){
    ATOMIC_SET(g_use_jack_transport, SETTINGS_read_bool("use_jack_transport", ATOMIC_GET(g_use_jack_transport)));
    has_inited = true;
  }

  return ATOMIC_GET(g_use_jack_transport);
}

void setUseJackTransport(bool doit){
  ATOMIC_SET(g_use_jack_transport, doit);
  SETTINGS_write_bool("use_jack_transport", doit);
  SEQUENCER_update(SEQUPDATE_EVERYTHING); // not sure why
}


static bool g_is_jack_timebase_master = true;

bool isJackTimebaseMaster(void){
  static bool has_inited = false;

  if (has_inited==false){
    g_is_jack_timebase_master = SETTINGS_read_bool("is_jack_timebase_master", g_is_jack_timebase_master);
    has_inited = true;
  }

  return g_is_jack_timebase_master;
}

void setIsJackTimebaseMaster(bool doit){
  g_is_jack_timebase_master = doit;
  SETTINGS_write_bool("is_jack_timebase_master", doit);
  MIXER_set_jack_timebase_master(doit);
}

static double g_default_audiofile_fadeout = 10.0;

double getDefaultAudiofileFadeout(void){
  static bool has_inited = false;

  if (has_inited==false){
    g_default_audiofile_fadeout = SETTINGS_read_double("default_audiofile_fadeout", g_default_audiofile_fadeout);
    has_inited = true;
  }

  return g_default_audiofile_fadeout;
}

void setDefaultAudiofileFadeout(double default_audiofile_fadeout){
  g_default_audiofile_fadeout = default_audiofile_fadeout;
  SETTINGS_write_double("default_audiofile_fadeout", default_audiofile_fadeout);
}


static bool g_do_auto_crossfades = true;

bool doAutoCrossfades(void){
  static bool has_inited = false;

  if (has_inited==false){
    g_do_auto_crossfades = SETTINGS_read_bool("do_auto_crossfades", g_do_auto_crossfades);
    has_inited = true;
  }

  return g_do_auto_crossfades;
}

void setDoAutoCrossfades(double default_do_auto_crossfades){
  g_do_auto_crossfades = default_do_auto_crossfades;
  SETTINGS_write_bool("do_auto_crossfades", g_do_auto_crossfades);
}



void setSequencerGridType(const_char *grid_type){
  bool is_error=false;
  enum GridType what = !strcmp(grid_type, "current") ? SEQUENCER_get_grid_type() : string_to_grid_type(grid_type, &is_error);
    
  if(is_error){
    handleError("Sequencer grid type must be either \"no\", \"line\", \"beat\", \"bar\", or \"current\". (\"%s\")", grid_type);
    return;
  }
  
  SEQUENCER_set_grid_type(what);
}

const_char *getSequencerGridType(void){
  enum GridType what = SEQUENCER_get_grid_type();

  return grid_type_to_string(what);
}

int64_t getSeqGriddedTime(int64_t pos, const_char* type){
  //R_ASSERT_NON_RELEASE(seqtracknum==0);

  bool is_error=false;
  enum GridType what = !strcmp(type, "current") ? SEQUENCER_get_grid_type() : string_to_grid_type(type, &is_error);
    
  if(is_error){
    handleError("Sequencer grid type must be either \"no\", \"line\", \"beat\", \"bar\", or \"current\". (\"%s\")", type);
    return pos;
  }
    
  return SEQUENCER_find_closest_grid_start(pos, what);
}

int64_t findClosestSeqtrackBarStart(int seqtracknum, int64_t pos){
  if (seqtracknum < 0 || seqtracknum >= root->song->seqtracks.num_elements){
    handleError("Sequencer track #%d does not exist", seqtracknum);
    return 0;
  }
  
  return SEQUENCER_find_closest_bar_start(pos);
}

int64_t findClosestSeqtrackBeatStart(int seqtracknum, int64_t pos){
  if (seqtracknum < 0 || seqtracknum >= root->song->seqtracks.num_elements){
    handleError("Sequencer track #%d does not exist", seqtracknum);
    return 0;
  }
  
  return SEQUENCER_find_closest_beat_start(pos);
}

int64_t findClosestSeqtrackLineStart(int seqtracknum, int64_t pos){
  if (seqtracknum < 0 || seqtracknum >= root->song->seqtracks.num_elements){
    handleError("Sequencer track #%d does not exist", seqtracknum);
    return 0;
  }
  
  return SEQUENCER_find_closest_line_start(pos);
}


static const char *g_block_grid = NULL;

const_char *getSeqBlockGridType(void){
  if (g_block_grid==NULL)
    g_block_grid = SETTINGS_read_string("seq_block_grid_type", "bar");

  return g_block_grid;
}

void setSeqBlockGridType(const_char *type){
  if (strcmp(type, "no") && strcmp(type, "line") && strcmp(type, "beat") && strcmp(type, "bar")){
    handleError("Sequencer grid type must be either \"no\", \"line\", \"beat\", or \"bar\". (\"%s\")", type);
    return;
  }

  g_block_grid = talloc_strdup(type);
  SETTINGS_write_string("seq_block_grid_type", g_block_grid);
}

static const char *g_automation_grid = NULL;

const_char *getSeqAutomationGridType(void){
  if (g_automation_grid==NULL)
    g_automation_grid = SETTINGS_read_string("seq_automation_grid_type", "beat");

  return g_automation_grid;
}

void setSeqAutomationGridType(const_char *type){
  if (strcmp(type, "no") && strcmp(type, "line") && strcmp(type, "beat") && strcmp(type, "bar")){
    handleError("Sequencer grid type must be either \"no\", \"line\", \"beat\", or \"bar\". (\"%s\")", type);
    return;
  }

  g_automation_grid = talloc_strdup(type);
  SETTINGS_write_string("seq_automation_grid_type", g_automation_grid);
}

static const char *g_tempo_grid = NULL;

const_char *getSeqTempoGridType(void){
  if (g_tempo_grid==NULL)
    g_tempo_grid = SETTINGS_read_string("seq_tempo_grid_type", "beat");

  return g_tempo_grid;
}

void setSeqTempoGridType(const_char *type){
  if (strcmp(type, "no") && strcmp(type, "line") && strcmp(type, "beat") && strcmp(type, "bar")){
    handleError("Sequencer grid type must be either \"no\", \"line\", \"beat\", or \"bar\". (\"%s\")", type);
    return;
  }

  g_tempo_grid = talloc_strdup(type);
  SETTINGS_write_string("seq_tempo_grid_type", g_tempo_grid);
}


static const char *g_loop_grid = NULL;

const_char *getSeqLoopGridType(void){
  if (g_loop_grid==NULL)
    g_loop_grid = SETTINGS_read_string("seq_loop_grid_type", "beat");

  return g_loop_grid;
}

void setSeqLoopGridType(const_char *type){
  if (strcmp(type, "no") && strcmp(type, "line") && strcmp(type, "beat") && strcmp(type, "bar")){
    handleError("Sequencer grid type must be either \"no\", \"line\", \"beat\", or \"bar\". (\"%s\")", type);
    return;
  }

  g_loop_grid = talloc_strdup(type);
  SETTINGS_write_string("seq_loop_grid_type", g_loop_grid);
}



void insertSilenceToSeqtrack(int seqtracknum, int64_t pos, int64_t duration){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return;

  ADD_UNDO(Sequencer());

  SEQTRACK_insert_silence(seqtrack, pos, duration);
}


void playlistInsert(void){
  PLAYLIST_insert();
}

void playlistRemove(void){
  PLAYLIST_remove();
}

static void get_seqblock_start_and_end_seqtime(const struct SeqTrack *seqtrack,
                                               const struct SeqBlock *seqblock,
                                               const struct Blocks *block,
                                               int64_t start_abstime, int64_t end_abstime,
                                               int64_t *start_seqtime, int64_t *end_seqtime)
{
  *start_seqtime = start_abstime;
  *end_seqtime = end_abstime;
  return;
  
  if(start_abstime==-1){
    if(seqblock==NULL){
      *start_seqtime = 0;
      *end_seqtime = 30000;
      R_ASSERT(false);
      return;
    }
  }
  
  *start_seqtime = start_abstime;

  if (end_abstime == -1){
    *end_seqtime = -1;
    return;
  }

  double reltempo = 1.0;
  if (block != NULL)
    reltempo = ATOMIC_DOUBLE_GET(block->reltempo);

  if (block==NULL || reltempo==1.0) {
    
    *end_seqtime = end_abstime;
    
  } else { 
    double blocklen = getBlockSTimeLength(block);      
    int64_t startseqtime = (*start_seqtime)==-1 ? seqblock->t.time : (*start_seqtime);
    double stretch;
    
    if (start_abstime==-1) {
      stretch = seqblock->t.stretch;
    } else {
      int64_t nonstretched_abs_duration = blocklen / reltempo;      
      int64_t stretched_abs_duration = end_abstime-start_abstime;
      stretch = (double)stretched_abs_duration / (double)nonstretched_abs_duration;
    }
    
    *end_seqtime = startseqtime + blocklen * stretch;
  }
}

int createSeqblock(int seqtracknum, int blocknum, int64_t pos, int64_t endpos){
  if (seqtracknum==-1)
    seqtracknum = getCurrSeqtrack();

  if (blocknum==-1)
    blocknum = currentBlock(-1);

  if (pos==-1)
    pos = getSongPos();
  
  VALIDATE_TIME(pos, -1);
  
  struct SeqTrack *seqtrack = getBlockSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return -1;

  struct Blocks *block = getBlockFromNum(blocknum);
  if (block==NULL)
    return -1;

  ADD_UNDO(Sequencer());

  int64_t start_seqtime;
  int64_t end_seqtime;

  get_seqblock_start_and_end_seqtime(seqtrack, NULL, block, pos, endpos, &start_seqtime, &end_seqtime);
  
  return SEQTRACK_insert_block(seqtrack, block, start_seqtime, end_seqtime);
}

int createSampleSeqblock(int seqtracknum, const_char* w_filename, int64_t pos, int64_t endpos){
  VALIDATE_TIME(pos, -1);
  
  struct SeqTrack *seqtrack = getAudioSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return -1;

  ADD_UNDO(Sequencer());

  {
    radium::ScopedIgnoreUndo ignore_undo;
    return SEQTRACK_insert_sample(seqtrack, seqtracknum, w_path_to_path(w_filename), pos, endpos);
  }
}

extern QStringList get_sample_name_filters(void);
const_char* getAudiofilePostfixes(void){
  return talloc_strdup(get_sample_name_filters().join(" ").toUtf8().constData());
}  

bool addAudiofile(const_char* w_filename){
  bool ret = SAMPLEREADER_add_audiofile(w_path_to_path(w_filename));
  BS_UpdateBlockList();
  return ret;
}

int64_t getAudioFilesGeneration(void){
  return g_sample_reader_filenames_generation;
}

dyn_t getAudioFiles(void){
  dynvec_t ret = {};
  vector_t filenames = SAMPLEREADER_get_all_filenames();

  VECTOR_FOR_EACH(wchar_t *, filename, &filenames){
    DYNVEC_push_back(ret, DYN_create_string(path_to_w_path(filename)));
  }END_VECTOR_FOR_EACH;

  return DYN_create_array(ret);
}


int createGfxGfxSeqblock(dyn_t state){
  if (state.type != HASH_TYPE){
    handleError("createGfxGfxSeqblockNew: Expected hash table as first argument, found %s\n", DYN_type_name(state));
    return -1;
  }
  
  const hash_t *hash = state.hash;
  
  if (HASH_has_key(hash, ":seqtracknum")==false){
    handleError("createGfxGfxSeqblockNew: Could not find \"seqtracknum\" key in state");
    return -1;
  }

  int seqtracknum = HASH_get_int32(hash, ":seqtracknum");
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return -1;

  return SEQTRACK_insert_gfx_gfx_block(seqtrack, seqtracknum, hash, THROW_API_EXCEPTION);
}

int createSeqblockFromState(dyn_t state){
  if (state.type != HASH_TYPE){
    handleError("createSeqblockFromState: Expected hash table as first argument, found %s\n", DYN_type_name(state));
    return -1;
  }

  hash_t *hash = state.hash;

  if (HASH_has_key(hash, ":seqtracknum")==false){
    handleError("createSeqblockFromState: Could not find \"seqtracknum\" key in state");
    return -1;
  }

  return SEQBLOCK_insert_seqblock_from_state(hash, THROW_API_EXCEPTION);
}

dyn_t getSeqblockState(int seqblocknum, int seqtracknum){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack, false);
  if (seqblock==NULL)
    return DYN_create_bool(false);
  
  hash_t *state = SEQBLOCK_get_state(seqtrack, seqblock, true);
  R_ASSERT_RETURN_IF_FALSE2(state!=NULL, DYN_create_bool(false));

  HASH_put_int(state, ":seqtracknum", seqtracknum);
  
  return DYN_create_hash(state);
}

dyn_t getSeqblocksState(int seqtracknum){
  const struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL){
    handleError("getSeqblocksState: No sequencer track %d", seqtracknum);
    return g_empty_dynvec;
  }

  return SEQTRACK_get_seqblocks_state(seqtrack);
}

void createGfxSeqblocksFromState(dyn_t seqblocks_state, int seqtracknum){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL){
    handleError("createGfxSeqblocksFromState: No sequencer track %d", seqtracknum);
    return;
  }

  if(seqblocks_state.type!=ARRAY_TYPE){
    handleError("createGfxSeqblocksFromState: Expected first argument to be an array. found %s\n", DYN_type_name(seqblocks_state));
    return;
  }

#if 0 //!defined(RELEASE)
  if (seqtrack->gfx_seqblocks!=NULL)
    abort();
#endif
    
  //printf("\n\n============= Creating gfxseqblocks from state\n");
  SEQTRACK_create_gfx_seqblocks_from_state(seqblocks_state, seqtrack, seqtracknum, THROW_API_EXCEPTION);
}

void cancelGfxSeqblocks(int seqtracknum){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL){
    handleError("cancelGfxSeqblocks: No sequencer track %d", seqtracknum);
    return;
  }

  if (seqtrack->gfx_seqblocks==NULL){
    //handleError("cancelGfxSeqblocks: No gfx seqtracks in sequencer track %d\n", seqtracknum); // Might happen for good reasons. Also, calling cancelGfxSeqblocks is only done inside an error catcher, and throwing exception inside an exception handler just leads to confusion.
    return;
  }

  SEQTRACK_cancel_gfx_seqblocks(seqtrack);

  R_ASSERT(seqtrack->gfx_seqblocks==NULL);
}

void applyGfxSeqblocks(int seqtracknum){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL){
    handleError("applyGfxSeqblocks: No sequencer track %d", seqtracknum);
    return;
  }

  if (seqtrack->gfx_seqblocks==NULL){
    //handleError("cancelGfxSeqblocks: No gfx seqtracks in sequencer track %d\n", seqtracknum); // No big point showing error message.
    return;
  }

  SEQTRACK_apply_gfx_seqblocks(seqtrack, seqtracknum, true);
  
  R_ASSERT_NON_RELEASE(seqtrack->gfx_seqblocks==NULL);
}


// seqblocks

int64_t g_curr_seqblock_id = -1;

void setCurrSeqblock(int64_t seqblockid){
  if (seqblockid==g_curr_seqblock_id)
    return;

  GET_VARS_FROM_SEQBLOCK_ID(seqblockid, true,);

  cancelCurrSeqblock(); // Update GFX of old seqblock.

  setCurrSeqtrack(seqtracknum);
    
  SEQBLOCK_update_with_borders(seqtrack, seqblock);
  
  g_curr_seqblock_id = seqblockid;

  if(seqblock->block != NULL){
    S7EXTRA_GET_FUNC(func, "FROM_C-update-seqblock-track-on-off-configuration");
    
    S7CALL(void_int_int, func, seqtracknum, seqblocknum);
  }
}

int64_t getCurrSeqblockId(void){
  return g_curr_seqblock_id;
}

void cancelCurrSeqblock(void){
  if (g_curr_seqblock_id==-1)
    return;
  
  int seqblocknum, seqtracknum;
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getGfxSeqblockFromIdB(g_curr_seqblock_id, &seqtrack, seqblocknum, seqtracknum, false);

  g_curr_seqblock_id = -1;

  if(seqblock!=NULL)
    SEQBLOCK_update_with_borders(seqtrack, seqblock);
  else
    SEQUENCER_update(SEQUPDATE_TIME); // A scheme error will be thrown if this happens.
}

/*
int getCurrSeqblock(void){
  if (g_curr_seqblock_id==-1)
    return -1;
  
  int seqblocknum, seqtracknum;
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getGfxSeqblockFromIdB(g_curr_seqblock_id, &seqtrack, seqblocknum, seqtracknum, false);
  if (seqblock==NULL)
    return -1;

  return seqblocknum;
}
*/

/*
int getCurrSeqtrack(void){
  if (g_curr_seqblock_id==-1)
    return -1;
  
  int seqblocknum, seqtracknum;
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getGfxSeqblockFromIdB(g_curr_seqblock_id, &seqtrack, seqblocknum, seqtracknum, false);
  if (seqblock==NULL)
    return -1;

  return seqtracknum;
}
*/



void setCurrSeqblockUnderMouse(int64_t seqblockid){
  if (seqblockid==g_curr_seqblock_id_under_mouse)
    return;
  
  GET_VARS_FROM_SEQBLOCK_ID(seqblockid, true,);

  cancelCurrSeqblockUnderMouse(); // Update GFX of old seqblock.

  //g_curr_seqblock_id_under_mouse = seqblock;    
  g_curr_seqblock_id_under_mouse = seqblockid;
  
  /*
  static int s_prev_curr_seqtracknum_under_mouse = -1;

  struct SeqTrack *prev_seqtrack_under_mouse = NULL;
  
  if (s_prev_curr_seqtracknum_under_mouse != seqtracknum
      && s_prev_curr_seqtracknum_under_mouse >= 0
      && s_prev_curr_seqtracknum_under_mouse < root->song->seqtracks.num_elements)
    prev_seqtrack_under_mouse = getSeqtrackFromNum(s_prev_curr_seqtracknum_under_mouse);
  
  s_prev_curr_seqtracknum_under_mouse = seqtracknum;
  */

  //printf("   CURR seqblocknum: %d\n", seqblocknum);
  
  SEQBLOCK_update(seqtrack, seqblock);
  /*
  SEQTRACK_update(seqtrack);
  if (prev_seqtrack_under_mouse != NULL)
    SEQTRACK_update(prev_seqtrack_under_mouse);
  */
}

int64_t getCurrSeqblockIdUnderMouse(void){
  return g_curr_seqblock_id_under_mouse;
}
  
int getCurrSeqblockUnderMouse(void){
  if (g_curr_seqblock_id_under_mouse==-1)
    return -1;
  
  int seqblocknum, seqtracknum;
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getGfxSeqblockFromIdB(g_curr_seqblock_id_under_mouse, &seqtrack, seqblocknum, seqtracknum, false);
  if (seqblock==NULL)
    return -1;

  return seqblocknum;
}

int getCurrSeqtrackUnderMouse(void){
  if (g_curr_seqblock_id_under_mouse==-1)
    return -1;
  
  int seqblocknum, seqtracknum;
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getGfxSeqblockFromIdB(g_curr_seqblock_id_under_mouse, &seqtrack, seqblocknum, seqtracknum, false);
  if (seqblock==NULL)
    return -1;

  return seqtracknum;
}

void cancelCurrSeqblockUnderMouse(void){
  if (g_curr_seqblock_id_under_mouse==-1)
    return;
  
  int seqblocknum, seqtracknum;
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getGfxSeqblockFromIdB(g_curr_seqblock_id_under_mouse, &seqtrack, seqblocknum, seqtracknum, false);

  if(seqblock!=NULL)
    SEQBLOCK_update(seqtrack, seqblock);
  else
    SEQUENCER_update(SEQUPDATE_TIME); // A scheme error will be thrown if this happens.

  //g_curr_seqblock_under_mouse = NULL;
  g_curr_seqblock_id_under_mouse = -1;

}

bool seqblockIsAlive(int64_t seqblockid){
  int seqblocknum, seqtracknum;
  struct SeqTrack *seqtrack;
  return getSeqblockFromIdB(seqblockid, &seqtrack, seqblocknum, seqtracknum, false)!=NULL;
}

dyn_t getBlockUsageInSequencer(void){
  int num_blocks = root->song->num_blocks;
  int ret[num_blocks];
  memset(ret, 0, sizeof(int)*num_blocks);

  VECTOR_FOR_EACH(struct SeqTrack *, seqtrack, &root->song->seqtracks){
    VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &seqtrack->seqblocks){
      if (seqblock->block != NULL)
        ret[seqblock->block->l.num]++;
    }END_VECTOR_FOR_EACH;
  }END_VECTOR_FOR_EACH;

  dynvec_t dynvec = {};

  for(int i=0;i<num_blocks;i++)
    DYNVEC_push_back(&dynvec, DYN_create_int(ret[i]));

  return DYN_create_array(dynvec);
}

void setSeqblockName(const_char* new_name, int seqblocknum, int seqtracknum){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack, false);
  if (seqblock==NULL)
    return;

  if (seqblock->block==NULL) {
    
    undoSeqblock(seqblocknum, seqtracknum);
    seqblock->name = STRING_create(new_name);
    
  } else {

    ADD_UNDO(Block_CurrPos(root->song->tracker_windows));
    seqblock->block->name = talloc_strdup(new_name);

  }

  SEQBLOCK_update(seqtrack, seqblock);
  SEQUENCER_update(SEQUPDATE_BLOCKLIST | SEQUPDATE_PLAYLIST);
}
  
const_char* getSeqblockName(int seqblocknum, int seqtracknum){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack, false);
  if (seqblock==NULL)
    return "";

  if (seqblock->name != NULL)
    return STRING_get_chars(seqblock->name);
  
  else if (seqblock->block==NULL)
    return STRING_get_chars(get_seqblock_sample_name(seqtrack, seqblock, false));
  
  else
    return talloc_strdup(seqblock->block->name);
}

int getNumSeqblocks(int seqtracknum){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return 0;
  else
    return gfx_seqblocks2(seqtrack, false)->num_elements;
}

int getNumGfxGfxSeqblocks(int seqtracknum){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return 0;
  else
    return seqtrack->gfx_gfx_seqblocks.num_elements;
}

int64_t getSeqblockStartTime(int seqblocknum, int seqtracknum, bool use_gfx_if_possible){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack, use_gfx_if_possible);
  if (seqblock==NULL)
    return 0;

  //SEQTRACK_update_all_seqblock_start_and_end_times(seqtrack);
    
  return seqblock->t.time;
}

int64_t getSeqblockEndTime(int seqblocknum, int seqtracknum, bool use_gfx_if_possible){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack, use_gfx_if_possible);
  if (seqblock==NULL)
    return 0;

  //SEQTRACK_update_all_seqblock_start_and_end_times(seqtrack);

  return seqblock->t.time2;
}

int64_t getSeqblockDefaultDuration(int seqblocknum, int seqtracknum){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack, false);
  if (seqblock==NULL)
    return 0;

  return seqblock->t.default_duration;
}

// seqblock area
float getSeqblockX1(int seqblocknum, int seqtracknum){
  if (getGfxSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_x1(seqblocknum, seqtracknum);
}

float getSeqblockY1(int seqblocknum, int seqtracknum){
  if (getGfxSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_y1(seqblocknum, seqtracknum);
}

float getSeqblockX2(int seqblocknum, int seqtracknum){
  if (getGfxSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_x2(seqblocknum, seqtracknum);
}

float getSeqblockY2(int seqblocknum, int seqtracknum){
  if (getGfxSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_y2(seqblocknum, seqtracknum);
}


float getSeqblockHeaderY2(int seqblocknum, int seqtracknum){
  return getSeqblockLeftFadeY1(seqblocknum, seqtracknum);
}

// seqblock left fade area

float getSeqblockLeftFadeX1(int seqblocknum, int seqtracknum){
  if (getGfxSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_left_fade_x1(seqblocknum, seqtracknum);
}

float getSeqblockLeftFadeY1(int seqblocknum, int seqtracknum){
  if (getGfxSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_left_fade_y1(seqblocknum, seqtracknum);
}

float getSeqblockLeftFadeX2(int seqblocknum, int seqtracknum){
  if (getGfxSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_left_fade_x2(seqblocknum, seqtracknum);
}

float getSeqblockLeftFadeY2(int seqblocknum, int seqtracknum){
  if (getGfxSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_left_fade_y2(seqblocknum, seqtracknum);
}

// seqblock right fade area

float getSeqblockRightFadeX1(int seqblocknum, int seqtracknum){
  if (getGfxSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_right_fade_x1(seqblocknum, seqtracknum);
}

float getSeqblockRightFadeY1(int seqblocknum, int seqtracknum){
  if (getGfxSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_right_fade_y1(seqblocknum, seqtracknum);
}

float getSeqblockRightFadeX2(int seqblocknum, int seqtracknum){
  if (getGfxSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_right_fade_x2(seqblocknum, seqtracknum);
}

float getSeqblockRightFadeY2(int seqblocknum, int seqtracknum){
  if (getGfxSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_right_fade_y2(seqblocknum, seqtracknum);
}

// seqblock left interior area

float getSeqblockLeftInteriorX1(int seqblocknum, int seqtracknum){
  if (getGfxSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_left_interior_x1(seqblocknum, seqtracknum);
}

float getSeqblockLeftInteriorY1(int seqblocknum, int seqtracknum){
  if (getGfxSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_left_interior_y1(seqblocknum, seqtracknum);
}

float getSeqblockLeftInteriorX2(int seqblocknum, int seqtracknum){
  if (getGfxSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_left_interior_x2(seqblocknum, seqtracknum);
}

float getSeqblockLeftInteriorY2(int seqblocknum, int seqtracknum){
  if (getGfxSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_left_interior_y2(seqblocknum, seqtracknum);
}

// seqblock right interior area

float getSeqblockRightInteriorX1(int seqblocknum, int seqtracknum){
  if (getGfxSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_right_interior_x1(seqblocknum, seqtracknum);
}

float getSeqblockRightInteriorY1(int seqblocknum, int seqtracknum){
  if (getGfxSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_right_interior_y1(seqblocknum, seqtracknum);
}

float getSeqblockRightInteriorX2(int seqblocknum, int seqtracknum){
  if (getGfxSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_right_interior_x2(seqblocknum, seqtracknum);
}

float getSeqblockRightInteriorY2(int seqblocknum, int seqtracknum){
  if (getGfxSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_right_interior_y2(seqblocknum, seqtracknum);
}

// seqblock left speed area

float getSeqblockLeftSpeedX1(int seqblocknum, int seqtracknum){
  if (getGfxSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_left_speed_x1(seqblocknum, seqtracknum);
}

float getSeqblockLeftSpeedY1(int seqblocknum, int seqtracknum){
  if (getGfxSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_left_speed_y1(seqblocknum, seqtracknum);
}

float getSeqblockLeftSpeedX2(int seqblocknum, int seqtracknum){
  if (getGfxSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_left_speed_x2(seqblocknum, seqtracknum);
}

float getSeqblockLeftSpeedY2(int seqblocknum, int seqtracknum){
  if (getGfxSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_left_speed_y2(seqblocknum, seqtracknum);
}

// seqblock right speed area

float getSeqblockRightSpeedX1(int seqblocknum, int seqtracknum){
  if (getGfxSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_right_speed_x1(seqblocknum, seqtracknum);
}

float getSeqblockRightSpeedY1(int seqblocknum, int seqtracknum){
  if (getGfxSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_right_speed_y1(seqblocknum, seqtracknum);
}

float getSeqblockRightSpeedX2(int seqblocknum, int seqtracknum){
  if (getGfxSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_right_speed_x2(seqblocknum, seqtracknum);
}

float getSeqblockRightSpeedY2(int seqblocknum, int seqtracknum){
  if (getGfxSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_right_speed_y2(seqblocknum, seqtracknum);
}

// seqblock left stretch area

float getSeqblockLeftStretchX1(int seqblocknum, int seqtracknum){
  if (getGfxSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_left_stretch_x1(seqblocknum, seqtracknum);
}

float getSeqblockLeftStretchY1(int seqblocknum, int seqtracknum){
  if (getGfxSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_left_stretch_y1(seqblocknum, seqtracknum);
}

float getSeqblockLeftStretchX2(int seqblocknum, int seqtracknum){
  if (getGfxSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_left_stretch_x2(seqblocknum, seqtracknum);
}

float getSeqblockLeftStretchY2(int seqblocknum, int seqtracknum){
  if (getGfxSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_left_stretch_y2(seqblocknum, seqtracknum);
}

// seqblock right stretch area

float getSeqblockRightStretchX1(int seqblocknum, int seqtracknum){
  if (getGfxSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_right_stretch_x1(seqblocknum, seqtracknum);
}

float getSeqblockRightStretchY1(int seqblocknum, int seqtracknum){
  if (getGfxSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_right_stretch_y1(seqblocknum, seqtracknum);
}

float getSeqblockRightStretchX2(int seqblocknum, int seqtracknum){
  if (getGfxSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_right_stretch_x2(seqblocknum, seqtracknum);
}

float getSeqblockRightStretchY2(int seqblocknum, int seqtracknum){
  if (getGfxSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_right_stretch_y2(seqblocknum, seqtracknum);
}


// seqblock select box

void setSeqblockSelectedBox(int which_one, int seqblocknum, int seqtracknum){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getGfxSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack);
  if (seqblock==NULL)
    return;

  enum SeqblockBoxSelected old = seqblock->selected_box;
  enum SeqblockBoxSelected new_ = (enum SeqblockBoxSelected)which_one;

  if (old != new_){
    seqblock->selected_box = new_;
    SEQBLOCK_update(seqtrack, seqblock);
  }
  
}

int getSeqblockSelectedBox(void){
  if (g_curr_seqblock_id_under_mouse == -1)
    return 0;
  else{
    GET_VARS_FROM_SEQBLOCK_ID(g_curr_seqblock_id_under_mouse, true, 0);
    return seqblock->selected_box;
  }
}
  

// seqblock fade in/out

double getSeqblockFadeIn(int seqblocknum, int seqtracknum){
  struct SeqBlock *seqblock = getSeqblockFromNum(seqblocknum, seqtracknum);
  if (seqblock==NULL)
    return 0.0;

  return seqblock->fadein;
}

double getSeqblockFadeOut(int seqblocknum, int seqtracknum){
  struct SeqBlock *seqblock = getSeqblockFromNum(seqblocknum, seqtracknum);
  if (seqblock==NULL)
    return 0.0;

  return seqblock->fadeout;
}

void setSeqblockFadeIn(double fadein, int seqblocknum, int seqtracknum){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack, false);
  if (seqblock==NULL)
    return;

  if(fadein < 0 || fadein > 1){
    handleError("setSeqblockFadeIn: Illegal fade value: %f", fadein);
    return;
  }

  if (fadein != seqblock->fadein){
    radium::PlayerLock lock(is_playing_song());
    seqblock->fadein = fadein;
  }

  SEQTRACK_update(seqtrack); // seqblock_update creates a small artifact
}

void setSeqblockFadeOut(double fadeout, int seqblocknum, int seqtracknum){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack, false);
  if (seqblock==NULL)
    return;

  if(fadeout < 0 || fadeout > 1){
    handleError("setSeqblockFadeOut: Illegal fade value: %f", fadeout);
    return;
  }

  if (fadeout != seqblock->fadeout){
    radium::PlayerLock lock(is_playing_song());
    seqblock->fadeout = fadeout;
  }

  SEQTRACK_update(seqtrack); // seqblock_update creates a small artifact
}


dyn_t getFadeShapes(void){
  dynvec_t dynvec = {};

  for(int i = 0 ; i<NUM_FADE_SHAPES ; i++)
    if (i != FADE_CUSTOM)
      DYNVEC_push_back(&dynvec, DYN_create_string_from_chars(fade_shape_to_string((enum FadeShape)i)));

  return DYN_create_array(dynvec);
}

const_char* getFadeShapeIconFilename(const_char* shape, bool is_fadein){
  enum FadeShape fade_shape;

  if (string_to_fade_shape2(shape, &fade_shape)==false){
    handleError("getFadeShapeIconFilename: Unknown shape \"%s\"", shape);
    return "";
  }
  
  radium::Envelope env(fade_shape, 1.0, is_fadein);
  return talloc_strdup(env.get_icon_filename().toUtf8().toBase64().constData());
}

const_char *getSeqblockFadeShape(bool is_fadein, int seqblocknum, int seqtracknum){
  struct SeqBlock *seqblock = getSeqblockFromNum(seqblocknum, seqtracknum);;
  if (seqblock==NULL)
    return "";

  if (is_fadein)
    return fade_shape_to_string(seqblock->fade_in_envelope->_shape);
  else
    return fade_shape_to_string(seqblock->fade_out_envelope->_shape);
}

bool setSeqblockFadeShape(const_char *shape, bool is_fadein, int seqblocknum, int seqtracknum){
  struct SeqBlock *seqblock = getSeqblockFromNum(seqblocknum, seqtracknum);;
  if (seqblock==NULL)
    return false;

  if (is_fadein)
    return SEQBLOCK_set_fade_in_shape(seqblock, string_to_fade_shape(shape));
  else
    return SEQBLOCK_set_fade_out_shape(seqblock, string_to_fade_shape(shape));
}




// move seqblock / set stretch

double getSeqblockStretch(int64_t seqblockid,  bool use_gfx_if_possible){
  GET_VARS_FROM_SEQBLOCK_ID(seqblockid, use_gfx_if_possible, 1.0)
    
  if(seqblock_is_stretched(seqblock))    
    return seqblock->t.stretch;
  else
    return 1.0;
}

double getSeqblockSpeed(int64_t seqblockid, bool use_gfx_if_possible){
  GET_VARS_FROM_SEQBLOCK_ID(seqblockid, use_gfx_if_possible, 1.0)
    
  if (seqblock_is_speeded(seqblock))
    return seqblock->t.speed;
  else
    return 1.0;
}

double getSeqblockStretchSpeed(int64_t seqblockid, bool use_gfx_if_possible){
  GET_VARS_FROM_SEQBLOCK_ID(seqblockid, use_gfx_if_possible, 1.0)

  return seqblock->t.stretch * seqblock->t.speed;
}

double getSeqblockResampleRatio(int64_t seqblockid, bool may_use_gfx){
  GET_VARS_FROM_SEQBLOCK_ID(seqblockid, may_use_gfx, 1.0)
    
  if (seqblock->block != NULL){
    handleError("getSeqblockResampleRatio: Seqblock %d in Seqtrack %d holds a block and not a sample", seqblocknum, seqtracknum);
    return 1.0;
  }

  struct SoundPlugin *plugin = (struct SoundPlugin*)seqtrack->patch->patchdata;  
  R_ASSERT_RETURN_IF_FALSE2(plugin!=NULL, 1.0);

  return SEQTRACKPLUGIN_get_resampler_ratio(plugin, seqblock->sample_id);
}

int64_t getSeqblockId(int seqblocknum, int seqtracknum){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack, false);
  if (seqblock==NULL)
    return 0;

  return seqblock->id;
}

int getSeqblockSeqtrackNum(int64_t seqblockid){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromIdA(seqblockid, &seqtrack);
  if (seqblock==NULL)
    return -1;

  return get_seqtracknum(seqtrack);
}

int getSeqblockSeqblockNum(int64_t seqblockid){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromIdA(seqblockid, &seqtrack);
  if (seqblock==NULL)
    return -1;

  return get_seqblocknum(seqtrack, seqblock);
}


static radium::ProtectedS7FuncVector g_seqblock_deleted_callbacks(true);

void addSeqblockDeletedCallback(func_t *callback){
  if (g_seqblock_deleted_callbacks.push_back(callback)==false)
    handleError("addSeqblockDeletedCallback: Callback %p already added\n", callback);
}
                                
static bool removeSeqblockDeletedCallback2(func_t *callback){
  int num_removed = g_seqblock_deleted_callbacks.removeAll(callback);
  R_ASSERT_NON_RELEASE(num_removed==0 || num_removed==1);
  
  return num_removed > 0;
}

void removeSeqblockDeletedCallback(func_t *callback){
  printf("   removeSeqblockDeletedCallback. Removing %p\n", callback);
  
  if (!removeSeqblockDeletedCallback2(callback))
    handleError("removeSeqblockDeletedCallback: Could not find deleted callback %p\n", callback);
}

DEFINE_ATOMIC(bool, g_has_seqblock_marked_as_available) = false;

static QSet<int64_t> g_seqblocks_marked_as_available;

void API_seqblock_has_been_deleted(int64_t seqblockid){
  QVector<func_t*> to_remove;

  if(g_seqblocks_marked_as_available.contains(seqblockid)){
    return;
  }
  
  if(g_curr_seqblock_id==seqblockid)
    g_curr_seqblock_id = -1;

  if(g_curr_seqblock_id_under_mouse==seqblockid)
    g_curr_seqblock_id_under_mouse = -1;
  
  g_seqblock_deleted_callbacks.safe_for_all(true, [&to_remove, seqblockid](func_t *callback){

      if (S7CALL(bool_int, callback, seqblockid)==false)
        to_remove.push_back(callback);

      return true;
      
    });

  for(auto *callback : to_remove){
    printf("   API_seqblock_has_been_deleted: Calling removeSeqblockDeletedCallback for %p\n", callback);
    removeSeqblockDeletedCallback2(callback);
  }
}

void API_all_seqblocks_will_be_deleted(void){
  VECTOR_FOR_EACH(struct SeqTrack *, seqtrack, &root->song->seqtracks){
    VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &seqtrack->seqblocks){
      API_seqblock_has_been_deleted(seqblock->id);
    }END_VECTOR_FOR_EACH;
  }END_VECTOR_FOR_EACH;
}
  
void markSeqblockAvailable(int64_t seqblockid){
  g_seqblocks_marked_as_available << seqblockid;
  ATOMIC_SET(g_has_seqblock_marked_as_available, true);
}

void unmarkSeqblockAvailable(int64_t seqblockid){
  if(g_seqblocks_marked_as_available.remove(seqblockid)==false)
    handleError("unmarkSeqblockAvailable: %d not marked as deletable", (int)seqblockid);

  if(g_seqblocks_marked_as_available.isEmpty())
    ATOMIC_SET(g_has_seqblock_marked_as_available, false);
}

static void remove_unused_seqblocks_from_seqblocks_z_order(struct SeqTrack *seqtrack){

  QSet<int64_t> used;
  VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &seqtrack->seqblocks){
    used << seqblock->id;
  }END_VECTOR_FOR_EACH;

  dynvec_t &order = seqtrack->seqblocks_z_order;

  int dec = 0;

  for(int i=0;i<order.num_elements;i++){
    const dyn_t &dyn = order.elements[i];
    if (false==used.contains(dyn.int_number))
      dec++;
    else if (dec>0){
      R_ASSERT_RETURN_IF_FALSE(i-dec >= 0);
      order.elements[i-dec] = dyn;
    }
  }

  order.num_elements -= dec;
}

dyn_t getSeqblocknumZOrder(int seqtracknum){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return g_empty_dynvec;

  // Create a hash table to avoid O(n^2) when adding the ordered seqblocks.
  QHash<int64_t, int> seqblocks_hash;
  VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &seqtrack->seqblocks){
    seqblocks_hash[seqblock->id] = iterator666;
  }END_VECTOR_FOR_EACH;
  
  QVector<struct SeqBlock*> seqblocks = SEQTRACK_get_seqblocks_in_z_order(seqtrack, false);

  dynvec_t ret = {};
  for(const struct SeqBlock *seqblock : seqblocks)
    DYNVEC_push_back(&ret, DYN_create_int(seqblocks_hash[seqblock->id]));

  return DYN_create_array(ret);
}

dyn_t getSeqblockZOrder(int seqtracknum){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return g_empty_dynvec;

  // clean up
  remove_unused_seqblocks_from_seqblocks_z_order(seqtrack);

  QVector<struct SeqBlock*> seqblocks = SEQTRACK_get_seqblocks_in_z_order(seqtrack, false);

  dynvec_t ret = {};
  for(const struct SeqBlock *seqblock : seqblocks)
    DYNVEC_push_back(&ret, DYN_create_int(seqblock->id));

  return DYN_create_array(ret);
}

void setSeqblockZOrder(dyn_t zorder, int seqtracknum){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return;

  if (zorder.type != ARRAY_TYPE){
    handleError("setSequencerZOrder: Expected array as first argument, found %s\n", DYN_type_name(zorder));
    return;
  }

  // clean up
  remove_unused_seqblocks_from_seqblocks_z_order(seqtrack);

  seqtrack->seqblocks_z_order = *zorder.array;

  SEQTRACK_update(seqtrack);
}



/*
void moveSeqblockGfxGfx(int seqblocknum, int64_t abstime, int seqtracknum, int new_seqtracknum){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getGfxGfxSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack);
  if (seqblock==NULL)
    return;

  VALIDATE_TIME(abstime,);
  
  if (new_seqtracknum==-1)
    new_seqtracknum = seqtracknum;

  struct SeqTrack *new_seqtrack = getSeqtrackFromNum(new_seqtracknum);
  if (new_seqtrack==NULL)
    return;
  
  //ATOMIC_SET(root->song->curr_seqtracknum, new_seqtracknum);
  
  //printf("Trying to move seqblocknum %d/%d to %d\n",seqtracknum,seqblocknum,(int)abstime);
  SEQTRACK_move_gfx_gfx_seqblock(seqtrack, seqblock, abstime);
}
*/

/*
Too inconvenient. Use apply_gfx_seqblocks instead of these two functions. (These things are calculated in bin/scheme/mouse.scm instead. It's faster to programme complicated things like this without having to recompile and start the program again when changing the code.)

static bool set_interior_start(int64_t interior_start, int seqblocknum, int seqtracknum, bool is_gfx){  
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack);
  if (seqblock==NULL)
    return false;
  
  const SeqBlockTimings &timing = is_gfx ? seqblock->gfx : seqblock->t;
    
  int64_t interior_end = timing.interior_end;
  if (interior_start < 0 || interior_start >= interior_end){
    handleError("Illegal interior_start value: %d. Must be between 0 (inclusive) and %d.", (int)interior_start, (int)interior_end);
    return false;
  }
  
  return SEQBLOCK_set_interior_start(seqtrack, seqblock, interior_start, is_gfx);  
}

bool setSeqblockInteriorStart(int64_t interior_start, int seqblocknum, int seqtracknum){
  return set_interior_start(interior_start, seqblocknum, seqtracknum, false);
}

bool setSeqblockInteriorStartGfx(int64_t interior_start, int seqblocknum, int seqtracknum){
  return set_interior_start(interior_start, seqblocknum, seqtracknum, true);
}

bool set_interior_end(int64_t interior_end, int seqblocknum, int seqtracknum, bool is_gfx){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack);
  if (seqblock==NULL)
    return false;

  const SeqBlockTimings &timing = is_gfx ? seqblock->gfx : seqblock->t;

  int64_t interior_start = timing.interior_start;
  int64_t default_duration = timing.default_duration;
  if (interior_end <= interior_start || interior_end > default_duration){
    handleError("Illegal interior_start value: %d. Must be between %d and %d (inclusive).", (int)interior_end, (int)interior_start, (int)default_duration);
    return false;
  }
  
  return SEQBLOCK_set_interior_end(seqtrack, seqblock, interior_end, is_gfx);
}

bool setSeqblockInteriorEnd(int64_t interior_end, int seqblocknum, int seqtracknum){
  return set_interior_end(interior_end, seqblocknum, seqtracknum, false);
}

bool setSeqblockInteriorEndGfx(int64_t interior_end, int seqblocknum, int seqtracknum){
  return set_interior_end(interior_end, seqblocknum, seqtracknum, true);
}
*/

int64_t getSeqblockInteriorStart(int seqblocknum, int seqtracknum, bool use_gfx_if_possible){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack, use_gfx_if_possible);
  if (seqblock==NULL)
    return 0;

  return seqblock->t.interior_start;
}

int64_t getSeqblockInteriorEnd(int seqblocknum, int seqtracknum, bool use_gfx_if_possible){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack, use_gfx_if_possible);
  if (seqblock==NULL)
    return 0;

  return seqblock->t.interior_end;
}

void deleteSeqblock(int64_t seqblockid, bool notify_listeners){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromIdA(seqblockid, &seqtrack);
  if (seqblock==NULL)
    return;

  ADD_UNDO(Sequencer());
  
  SEQTRACK_delete_seqblock(seqtrack, seqblock, notify_listeners);

  BS_UpdatePlayList();
}

void deleteGfxGfxSeqblock(int seqblocknum, int seqtracknum){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getGfxGfxSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack);
  if (seqblock==NULL)
    return;

  SEQTRACK_delete_gfx_gfx_seqblock(seqtrack, seqblock);

  SEQTRACK_update(seqtrack);
}

int getSeqblockBlocknum(int seqblocknum, int seqtracknum){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack, false);
  if (seqblock==NULL)
    return 0;

  if (seqblock->block==NULL){
    handleError("getSeqblockBlocknum: Seqblock %d in Seqtrack %d holds a sample and not a block", seqblocknum, seqtracknum);
    return -1;
  }

  return seqblock->block->l.num;
}

const_char* getSeqblockSample(int seqblocknum, int seqtracknum){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack, false);
  if (seqblock==NULL)
    return "";
  if (seqblock->block!=NULL){
    handleError("getSeqblockSampleId: Seqblock %d in seqtrack %d holds a block and not a sample", seqblocknum, seqtracknum);
    return "";
  }

  const wchar_t *samplename = get_seqblock_sample_name(seqtrack, seqblock, true);
  return path_to_w_path(samplename); // <- to base64 (s7 doesn't support wide char).
}

int64_t getSampleLength(const_char* w_filename){
  const wchar_t *path = w_path_to_path(w_filename);
  int64_t length = SAMPLEREADER_get_sample_duration(path);
  if (length < 0)
    handleError("Sample \"%S\" not found", path);

  return length;
}

/*
void splitSeqblock(int64_t pos, int seqblocknum, int seqtracknum){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack);
  if (seqblock==NULL)
    return;
  if (seqblock->block!=NULL){
    handleError("splitSeqblock: Splitting editor block is not supported yet");
    return;
  }

  evalScheme(talloc_format("(FROM_C-cut-all-selected-seqblocks %d " "%" PRId64 " %" PRId64 ")", pos, seqblocknum, seqtracknum));
}
*/

/*
void selectSeqblock(int seqblocknum, int seqtracknum){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack);
  if (seqblock==NULL)
    return;

  root->song->curr_seqtracknum = seqtracknum;

  selectBlock(seqblock->block->l.num, -1);
}
*/

int getNumSelectedSeqblocks(void){
  int ret = 0;
  VECTOR_FOR_EACH(struct SeqTrack *, seqtrack, &root->song->seqtracks){
    VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &seqtrack->seqblocks){
      if (seqblock->is_selected)
        ret++;
    }END_VECTOR_FOR_EACH;
  }END_VECTOR_FOR_EACH;

  return ret;
}

void selectSeqblock(bool is_selected, int seqblocknum, int seqtracknum){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack, false);
  if (seqblock==NULL)
    return;

  if (  seqblock->is_selected != is_selected){
    seqblock->is_selected = is_selected;
    SEQUENCER_update(SEQUPDATE_TIME);
  }
}

bool isSeqblockSelected(int seqblocknum, int seqtracknum){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack, false);
  if (seqblock==NULL)
    return false;

  return seqblock->is_selected;
}

void unsetAllSelectedSeqblocks(void){
  bool do_update=false;
  VECTOR_FOR_EACH(struct SeqTrack *, seqtrack, &root->song->seqtracks){
    VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &seqtrack->seqblocks){
      if(seqblock->is_selected==true){
        seqblock->is_selected=false;
        do_update=true;
      }
    }END_VECTOR_FOR_EACH;
  }END_VECTOR_FOR_EACH;

  if(do_update)
    SEQUENCER_update(SEQUPDATE_TIME);
}

bool isSeqblockTrackEnabled(int tracknum, int seqblocknum, int seqtracknum){
  if (tracknum < 0 || tracknum >= MAX_DISABLED_SEQBLOCK_TRACKS){
    handleError("isSeqblockTrackEnabled: Illegal tracknum: %d", tracknum);
    return false;
  }
    
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack, false);
  if (seqblock==NULL)
    return false;

  if (seqblock->block==NULL){
    handleError("isSeqblockTrackEnabled: Seqblock %d in Seqtrack %d is not a block seqblock", seqblocknum, seqtracknum);
    return false;
  }

  return !seqblock->track_is_disabled[tracknum];
}

  
void setSeqblockTrackEnabled(bool is_enabled, int tracknum, int seqblocknum, int seqtracknum){
  if (tracknum < 0 || tracknum >= MAX_DISABLED_SEQBLOCK_TRACKS){
    handleError("setSeqblockTrackEnabled: Illegal tracknum: %d", tracknum);
    return;
  }
    
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack, false);
  if (seqblock==NULL)
    return;

  if (seqblock->block==NULL){
    handleError("getSeqblockBlocknum: Seqblock %d in Seqtrack %d is not a block seqblock", seqblocknum, seqtracknum);
    return;
  }

  if (seqblock->track_is_disabled[tracknum] == is_enabled){
    PC_Pause();{
      seqblock->track_is_disabled[tracknum] = !is_enabled;
    }PC_StopPause(NULL);
    SEQUENCER_update(SEQUPDATE_TIME);
  }
}

void copyEditorTrackOnOffToSeqblock(int seqblocknum, int seqtracknum){
  S7CALL2(void_int_int, "FROM_C-copy-editor-track-on/off-to-seqblock", seqblocknum, seqtracknum);
}

bool seqblockHoldsBlock(int seqblocknum, int seqtracknum, bool use_gfx_if_possible){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack, use_gfx_if_possible);
  if (seqblock==NULL)
    return false;
  return seqblock->block != NULL;
}

bool seqblockHoldsSample(int seqblocknum, int seqtracknum, bool use_gfx_if_possible){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack, use_gfx_if_possible);
  if (seqblock==NULL)
    return false;
  return seqblock->block==NULL;
}

void cutSelectedSeqblocks(void){
  evalScheme("(FROM_C-cut-all-selected-seqblocks)");      
}

void pasteSeqblocks(int seqtracknum, int64_t abstime){
  //printf(" pasteSeqblocks. seqtracknum: %d, abstime: %f\n", seqtracknum, (double)abstime);

  //abort();

  if (seqtracknum==-1)
    seqtracknum = getSeqtrackFromY(tevent.y);

  //printf("seqtracknum: %d\n", seqtracknum);

  if (seqtracknum==-1)
    return;

  //printf("abstime: %d\n", (int)abstime);

  if (abstime < 0){
    abstime = scale_int64(tevent.x, getSequencerX1(), getSequencerX2(), getSequencerVisibleStartTime(), getSequencerVisibleEndTime());
  }
  if (abstime < 0)
    return;

  evalScheme(talloc_format("(FROM_C-paste-sequencer-blocks %d " "%" PRId64 ")", seqtracknum, abstime));
}


void copySelectedSeqblocks(void){
  evalScheme("(FROM_C-copy-all-selected-seqblocks)");
}


void deleteSelectedSeqblocks(void){
  evalScheme("(FROM_C-delete-all-selected-seqblocks)");
}



/***************** Tempos (BPM) ****************/


static void addTempo(dynvec_t &ret, int64_t time, double bpm, int logtype){
  hash_t *element = HASH_create(2);

  HASH_put_float(element, ":bpm", bpm);
  HASH_put_int(element, ":logtype", logtype);
  HASH_put_int(element, ":time", time);
  HASH_put_qstring(element, ":uuid", QString::number(time)); // Quick hack to make the uuid field be a unique identifier. These are just for block editor timing, which can't be edited in the timeline anyway. Seqeuncer timing entries have real uuid values.

  dyn_t element2 = DYN_create_hash(element);

  DYNVEC_push_back(ret, element2);
}



//extern void das_printit(struct Blocks *block);

static dyn_t get_editor_tempos(int64_t start_seqtime, int64_t end_seqtime, bool include_all_tempos_needed_to_paint_graph){

  dynvec_t ret = {};

  //das_printit(getWindowFromNum(-1)->wblock->block);
  //return DYN_create_array(ret);

  double prev_bpm = -1;
  int prev_logtype = -1;

  int num_after = 0;

  SEQUENCER_iterate_time_seqblocks
    (start_seqtime,end_seqtime,include_all_tempos_needed_to_paint_graph,
     [&](const struct SeqTrack *seqtrack,const struct SeqBlock *seqblock, const struct Blocks *block, const struct SeqBlock *next_seqblock){


      int64_t start_blockseqtime = seqblock->t.time;

      /*
        int64_t end_blockseqtime = seqblock->t.time2;
        
        int64_t next_blockstarttime = next_seqblock==NULL ? -1 : next_seqblock->t.time;
      */


      int curr_lpb = root->lpb;
      const struct LPBs *next_lpb = block->lpbs;
      
      const struct STimes *times = block->times_without_global_swings;
      const struct STimeChange *change=times[0].tchanges;
      
      for(;;){
        
        const double place1 = change->y1;
        const double time1 = change->t1;
        const double time2 = change->t2;

        const double tempo1 = change->x1 / seqblock->t.stretch;
        const double tempo2 = change->x2 / seqblock->t.stretch;
        const int logtype = has_same_tempo(tempo1, tempo2) ? LOGTYPE_HOLD : LOGTYPE_LINEAR;

        while(next_lpb!=NULL && p_getDouble(next_lpb->l.p) <= place1){
          curr_lpb = next_lpb->lpb;
          next_lpb = NextConstLPB(next_lpb);
        }

        const int64_t seqtime = start_blockseqtime + blocktime_to_seqtime(seqblock, time1);

        //printf("Line: %f. bpm1: %f. bpm2: %f (%f). LPB: %d. seqtime: %d. end: %d\n", change->y1, change->x1, change->x2, change->y2, curr_lpb, (int)seqtime, (int)end_seqtime);

        if (seqtime >= end_seqtime){
          if (include_all_tempos_needed_to_paint_graph==false || num_after==1)
            return radium::IterateSeqblocksCallbackReturn::ISCR_BREAK;
          num_after++;
        }

        double bpm = tempo1/(double)curr_lpb;

        if (include_all_tempos_needed_to_paint_graph==true || seqtime >= start_seqtime)
          if(!has_same_tempo(prev_bpm, bpm) || logtype==LOGTYPE_LINEAR) {
            if (seqtime < start_seqtime)
              DYNVEC_light_clean(ret);
            addTempo(ret, seqtime, bpm, logtype);
          }

        prev_bpm = bpm;
        prev_logtype = logtype;

        if(change->y2 >= block->num_lines) {

          // Add BPM at seqblock end if necessary.
          if (logtype==LOGTYPE_LINEAR) {
            while(next_lpb!=NULL) {
              curr_lpb = next_lpb->lpb;
              next_lpb = NextConstLPB(next_lpb);
            }
            
            const double bpm = tempo2/(double)curr_lpb;

            const int64_t seqtime = start_blockseqtime + blocktime_to_seqtime(seqblock, time2);
            R_ASSERT_NON_RELEASE(fabs(seqtime-seqblock->t.time2) < 10);

            const int logtype = LOGTYPE_HOLD;

            addTempo(ret, seqtime, bpm, logtype);

            prev_bpm = bpm;
            prev_logtype = logtype;
          }

          break;
        }

        change = change + 1; // All changes in a block are allocated sequentially.        
      }

      
      return radium::IterateSeqblocksCallbackReturn::ISCR_CONTINUE;
    });

  if(ret.num_elements==0 && start_seqtime<=0)
    addTempo(ret, 0, root->tempo, LOGTYPE_LINEAR);
  
  return DYN_create_array(ret);
}

dyn_t getSequencerTempos(int64_t start_seqtime, int64_t end_seqtime, bool include_all_tempos_needed_to_paint_graph){
  if (root->song->use_sequencer_tempos_and_signatures)
    return SEQUENCER_TEMPO_get_state();
  else
    return get_editor_tempos(start_seqtime, end_seqtime, include_all_tempos_needed_to_paint_graph);
}

dyn_t getAllSequencerTempos(void){
  if (root->song->use_sequencer_tempos_and_signatures)
    return SEQUENCER_TEMPO_get_state();
  else
    return get_editor_tempos(0, SONG_get_length(), true);
}

void setSequencerTempos(dyn_t tempos){
  if (!root->song->use_sequencer_tempos_and_signatures){
    handleError("setSequencerTempos not supported in sequencer timing mode");
    return;
  }

  SEQUENCER_TEMPO_create_from_state(tempos, -1);
}


/***************** Signatures ****************/

static void add_signature(dynvec_t &ret, int64_t time, const StaticRatio &ratio){
  hash_t *element = HASH_create(2);

  HASH_put_int(element, ":numerator", ratio.numerator);
  HASH_put_int(element, ":denominator", ratio.denominator);
  HASH_put_int(element, ":time", time);
  HASH_put_qstring(element, ":uuid", QString::number(time)); // Quick hack to make the uuid field be a unique identifier. These are just for block editor timing, which can't be edited in the timeline anyway. Seqeuncer timing entries have real uuid values.

  dyn_t element2 = DYN_create_hash(element);

  DYNVEC_push_back(ret, element2);
}

static dyn_t get_editor_signatures(int64_t start_seqtime, int64_t end_seqtime){

  dynvec_t ret = {};

  bool has_created_first = false;

  //das_printit(getWindowFromNum(-1)->wblock->block);
  //return DYN_create_array(ret);

  SEQUENCER_iterate_time_seqblocks
    (start_seqtime,end_seqtime,false,
     [&](const struct SeqTrack *seqtrack,const struct SeqBlock *seqblock, const struct Blocks *block, const struct SeqBlock *next_seqblock){


      int64_t start_blockseqtime = seqblock->t.time;

      /*
        int64_t end_blockseqtime = seqblock->t.time2;
        
        int64_t next_blockstarttime = next_seqblock==NULL ? -1 : next_seqblock->t.time;
      */

      const struct Signatures *signature = block->signatures;
      while(signature != NULL){

        int64_t blocktime = Place2STime(block, &signature->l.p);
        int64_t seqtime = start_blockseqtime + blocktime_to_seqtime(seqblock, blocktime);

        if (has_created_first==false){

          if (seqtime > 0 && start_seqtime<=0)
            add_signature(ret, 0, root->signature);

          has_created_first = true;
        }

        add_signature(ret, seqtime, signature->signature);

        signature = NextConstSignature(signature);
      }
      
      return radium::IterateSeqblocksCallbackReturn::ISCR_CONTINUE;
    });
  
  if(ret.num_elements==0 && start_seqtime<=0)
    add_signature(ret, 0, root->signature);

  return DYN_create_array(ret);
}

dyn_t getSequencerSignatures(int64_t start_seqtime, int64_t end_seqtime){
  if (root->song->use_sequencer_tempos_and_signatures)
    return SEQUENCER_SIGNATURE_get_state();
  else
    return get_editor_signatures(start_seqtime, end_seqtime);
}

dyn_t getAllSequencerSignatures(void){
  if (root->song->use_sequencer_tempos_and_signatures)
    return SEQUENCER_SIGNATURE_get_state();
  else
    return get_editor_signatures(0, SONG_get_length());
}

void setSequencerSignatures(dyn_t signatures){
  if (!root->song->use_sequencer_tempos_and_signatures){
    handleError("setSequencerSignatures not supported in sequencer timing mode");
    return;
  }

  SEQUENCER_SIGNATURE_create_from_state(signatures, -1);
}


/***************** Timing ****************/
dyn_t getSequencerTiming(void){
  return DYN_create_hash(SEQUENCER_TIMING_get_state());
}

void setSequencerTiming(dyn_t state){
  if (state.type!=HASH_TYPE){
    handleError("Illegal value for setSequencerTiming. Expected hash table");
    return;
  }
  return SEQUENCER_TIMING_create_from_state(state.hash, -1);
}


/***************** Markers ****************/

dyn_t getAllSequencerMarkers(void){
  return SEQUENCER_MARKER_get_state();
}

void setSequencerMarkers(dyn_t markers){
  //ADD_UNDO(Sequencer());
  SEQUENCER_MARKER_create_from_state(markers, -1);
}

static bool g_paint_vertical_markers_in_sequencer = false;

bool doPaintVerticalMarkersInSequencer(void){
  return g_paint_vertical_markers_in_sequencer;
}

void setPaintVerticalMarkersInSequencer(bool doit){
  g_paint_vertical_markers_in_sequencer = doit;
  SEQUENCER_update(SEQUPDATE_TIME);
}


/***************** set/get using sequencer timing ****************/

bool isUsingSequencerTiming(void){
  return root->song->use_sequencer_tempos_and_signatures;
}

void setUsingSequencerTiming(bool use_sequencer){
  if (use_sequencer==root->song->use_sequencer_tempos_and_signatures)
    return;

  ADD_UNDO(Sequencer());

  {
    radium::PlayerLock lock(is_playing_song());
    root->song->use_sequencer_tempos_and_signatures = use_sequencer;
  }
}


/***************** iterate time ****************/

void iterateSequencerTime(int64_t start_time, int64_t end_time, const_char* grid_type, func_t* callback){
  bool is_error=false;
  enum GridType what = !strcmp(grid_type, "current") ? SEQUENCER_get_grid_type() : string_to_grid_type(grid_type, &is_error);
    
  if(is_error){
    handleError("Sequencer grid type must be either \"no\", \"line\", \"beat\", \"bar\", or \"current\". (\"%s\")", grid_type);
    return;
  }

  double start_ms_time = TIME_get_ms();
  
  int num_calls_to_handleError = g_num_calls_to_handleError;

  SEQUENCER_iterate_time(start_time, end_time, what,
                         [callback, num_calls_to_handleError, start_ms_time](int64_t seqtime, int barnum, int beatnum, int linenum){

                           if ((TIME_get_ms() - start_ms_time) > 5000){
                             handleError("   Error: Used more than 5 seconds to iterate timing.");
                             return false;
                           }
                           
                           bool ret = S7CALL(bool_int_int_int_int, callback, seqtime, barnum, beatnum, linenum);

                           if (g_num_calls_to_handleError != num_calls_to_handleError)
                             return false;
                           
                           return ret;
                         });
}



/***************** sequencer lane show/hide ****************/

bool showBarsAndBeatsSequencerLane(void){
  return root->song->show_bars_and_beats_sequencer_lane;
}
void setShowBarsAndBeatsSequencerLane(bool doit){
  if (!doit && !showTimeSequencerLane()){
    handleError("Either the time lane or the bars+beats lane must be visible");
    return;
  }
  
  root->song->show_bars_and_beats_sequencer_lane = doit;
  SEQUENCER_update(SEQUPDATE_EVERYTHING);
}
bool showTimeSequencerLane(void){
  return root->song->show_time_sequencer_lane;
}
void setShowTimeSequencerLane(bool doit){
  if (!doit && !showBarsAndBeatsSequencerLane()){
    handleError("Either the time lane or the bars+beats lane must be visible");
    return;
  }
  
  root->song->show_time_sequencer_lane = doit;
  SEQUENCER_update(SEQUPDATE_EVERYTHING);
}
bool showTemposSequencerLane(void){
  return root->song->show_tempos_sequencer_lane;
}
void setShowTemposSequencerLane(bool doit){
  root->song->show_tempos_sequencer_lane = doit;
  SEQUENCER_update(SEQUPDATE_EVERYTHING);
}
bool showSignaturesSequencerLane(void){
  return root->song->show_signatures_sequencer_lane;
}
void setShowSignaturesSequencerLane(bool doit){
  root->song->show_signatures_sequencer_lane = doit;
  SEQUENCER_update(SEQUPDATE_EVERYTHING);
}
bool showMarkersSequencerLane(void){
  return root->song->show_markers_sequencer_lane;
}
void setShowMarkersSequencerLane(bool doit){
  root->song->show_markers_sequencer_lane = doit;
  SEQUENCER_update(SEQUPDATE_EVERYTHING);
}




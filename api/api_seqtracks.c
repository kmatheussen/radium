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
#include <inttypes.h>


#include "../common/includepython.h"

#include "../common/nsmtracker.h"
#include "../common/seqtrack_proc.h"
#include "../common/seqtrack_automation_proc.h"
#include "../common/song_tempo_automation_proc.h"
#include "../common/time_proc.h"
#include "../common/undo_sequencer_proc.h"
#include "../common/undo_song_tempo_automation_proc.h"
#include "../common/visual_proc.h"
#include "../common/OS_Bs_edit_proc.h"
#include "../common/settings_proc.h"
#include "../common/visual_proc.h"
#include "../common/player_pause_proc.h"

#include "../audio/Mixer_proc.h"
#include "../audio/SoundPlugin.h"

#include "api_common_proc.h"

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

void undoSequencerAutomation(void){
  ADD_UNDO(SeqAutomations());
}

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
    handleError("setSequencerVisibleStartTime: Value must be 0 or higher and lower than visible end time. End time: %f. Value: %f\n", SEQUENCER_get_visible_end_time(), (double)value);
    return;
  }
  //printf("                   Set: %f\n", value/48000.0);
  SEQUENCER_set_visible_start_time(value);
}

void setSequencerVisibleEndTime(int64_t value){
  if (value <= SEQUENCER_get_visible_start_time()){
    handleError("setSequencerVisibleEndTime: Value must be higher than visible start time. Start time: %f. Value: %f\n", SEQUENCER_get_visible_start_time(), (double)value);
    return;
  }
  SEQUENCER_set_visible_end_time(value);
}

void setSequencerGridType(int grid_type){
  if (grid_type < FIRST_LEGAL_GRID || grid_type > LAST_LEGAL_GRID){
    handleError("setSequencerGridType: Illegal grid type %d", grid_type);
    return;
  }
  SEQUENCER_set_grid_type(grid_type);
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

void appendSeqtrack(void){
  undoSequencer();
  SEQUENCER_append_seqtrack(NULL);

  ATOMIC_SET(root->song->curr_seqtracknum, root->song->seqtracks.num_elements -1);
  BS_UpdatePlayList();
}

void insertSeqtrack(int pos){
  if (pos==-1)
    pos = ATOMIC_GET(root->song->curr_seqtracknum);
  
  if (pos < 0 || pos > root->song->seqtracks.num_elements){
    handleError("Position #%d not legal", pos);
    return;
  }

  undoSequencer();
  SEQUENCER_insert_seqtrack(NULL, pos);

  ATOMIC_SET(root->song->curr_seqtracknum, pos);
  BS_UpdatePlayList();
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
  
  undoSequencer();
  SEQUENCER_delete_seqtrack(seqtracknum);
}

void selectSeqtrack(int seqtracknum){
  if (seqtracknum < 0 || seqtracknum >= root->song->seqtracks.num_elements){
    handleError("Sequencer track #%d does not exist", seqtracknum);
    return;
  }

  ATOMIC_SET(root->song->curr_seqtracknum, seqtracknum);
  BS_UpdatePlayList();
  SEQUENCER_update();
}

int getCurrSeqtrack(void){
  return ATOMIC_GET(root->song->curr_seqtracknum);
}

int getNumSeqtracks(void){
  return root->song->seqtracks.num_elements;
}




// Sequencer track automation
//////////////////////////////////////////

int addSeqAutomation(int64_t time1, float value1, int64_t time2, float value2, int effect_num, int64_t instrument_id, int seqtracknum){
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
  
  undoSequencerAutomation();

  int64_t seqtime1 = get_seqtime_from_abstime(seqtrack, NULL, time1);
  int64_t seqtime2 = get_seqtime_from_abstime(seqtrack, NULL, time2);

  return SEQTRACK_AUTOMATION_add_automation(seqtrack->seqtrackautomation, patch, effect_num, seqtime1, value1, LOGTYPE_LINEAR, seqtime2, value2);
}

void replaceAllSeqAutomation(int64_t old_instrument, int64_t new_instrument){
  struct Patch *old_patch = getAudioPatchFromNum(old_instrument);
  if(old_patch==NULL)
    return;

  struct Patch *new_patch = getAudioPatchFromNum(new_instrument);
  if(new_patch==NULL)
    return;

  undoSequencerAutomation();

  SEQTRACK_AUTOMATION_replace_all_automations(old_patch, new_patch);
}

int getNumSeqAutomations(int seqtracknum){
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

  int64_t seqtime = SEQTRACK_AUTOMATION_get_seqtime(seqtrack->seqtrackautomation, automationnum, nodenum);

  return get_abstime_from_seqtime(seqtrack, NULL, seqtime);
}

int getSeqAutomationLogtype(int nodenum, int automationnum, int seqtracknum){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return -1;

  VALIDATE_AUTOMATIONNUM(-1);
  VALIDATE_NODENUM(-1);

  return SEQTRACK_AUTOMATION_get_logtype(seqtrack->seqtrackautomation, automationnum, nodenum);
}

int getNumSeqAutomationNodes(int automationnum, int seqtracknum){
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
    
  undoSequencerAutomation();

  int64_t seqtime = get_seqtime_from_abstime(seqtrack, NULL, time);
  return SEQTRACK_AUTOMATION_add_node(seqtrack->seqtrackautomation, automationnum, seqtime, value, logtype);
}

void deleteSeqAutomationNode(int nodenum, int automationnum, int seqtracknum){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return;

  VALIDATE_AUTOMATIONNUM();
  VALIDATE_NODENUM();

  undoSequencerAutomation();

  SEQTRACK_AUTOMATION_delete_node(seqtrack->seqtrackautomation, automationnum, nodenum);
}

void setCurrSeqAutomationNode(int nodenum, int automationnum, int seqtracknum){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return;

  VALIDATE_AUTOMATIONNUM();
  VALIDATE_NODENUM();

  SEQTRACK_AUTOMATION_set_curr_node(seqtrack->seqtrackautomation, automationnum, nodenum);
}

void cancelCurrSeqAutomationNode(int automationnum, int seqtracknum){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return;

  VALIDATE_AUTOMATIONNUM();

  SEQTRACK_AUTOMATION_cancel_curr_node(seqtrack->seqtrackautomation, automationnum);
}

void setCurrSeqAutomation(int automationnum, int seqtracknum){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return;

  VALIDATE_AUTOMATIONNUM();

  SEQTRACK_AUTOMATION_set_curr_automation(seqtrack, automationnum);
}

void cancelCurrSeqAutomation(void){
  //printf("   cancelCurrSeqAutomation called\n");
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
    
  int64_t seqtime = get_seqtime_from_abstime(seqtrack, NULL, time);
  SEQTRACK_AUTOMATION_set(seqtrack, automationnum, nodenum, seqtime, R_BOUNDARIES(0, value, 1), logtype);
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


// sequencer timeline and looping
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

static bool g_show_bars_in_timeline = false;

bool showBarsInTimeline(void){
  static bool has_inited = false;

  if (has_inited==false){
    g_show_bars_in_timeline = SETTINGS_read_bool("show_bars_in_timeline", g_show_bars_in_timeline);
    has_inited = true;
  }

  return g_show_bars_in_timeline;
}

void setShowBarsInTimeline(bool doit){
  g_show_bars_in_timeline = doit;
  SETTINGS_write_bool("show_bars_in_timeline", doit);
  SEQUENCER_update();
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
  SEQUENCER_update();
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


int64_t getSeqGriddedTime(int64_t pos, int seqtracknum, const_char* type){
  if (!strcmp(type, "no"))
    return pos;
  
  else if (!strcmp(type, "line"))
    return findClosestSeqtrackLineStart(seqtracknum, pos);

  else if (!strcmp(type, "beat"))
    return findClosestSeqtrackBeatStart(seqtracknum, pos);

  else if (!strcmp(type, "bar"))
    return findClosestSeqtrackBarStart(seqtracknum, pos);

  handleError("Sequencer grid type must be either \"no\", \"line\", \"beat\", or \"bar\". (\"%s\")", type);
  return pos;
}

int64_t findClosestSeqtrackBarStart(int seqtracknum, int64_t pos){
  if (seqtracknum < 0 || seqtracknum >= root->song->seqtracks.num_elements){
    handleError("Sequencer track #%d does not exist", seqtracknum);
    return 0;
  }
  
  return SEQUENCER_find_closest_bar_start(seqtracknum, pos);
}

int64_t findClosestSeqtrackBeatStart(int seqtracknum, int64_t pos){
  if (seqtracknum < 0 || seqtracknum >= root->song->seqtracks.num_elements){
    handleError("Sequencer track #%d does not exist", seqtracknum);
    return 0;
  }
  
  return SEQUENCER_find_closest_beat_start(seqtracknum, pos);
}

int64_t findClosestSeqtrackLineStart(int seqtracknum, int64_t pos){
  if (seqtracknum < 0 || seqtracknum >= root->song->seqtracks.num_elements){
    handleError("Sequencer track #%d does not exist", seqtracknum);
    return 0;
  }
  
  return SEQUENCER_find_closest_line_start(seqtracknum, pos);
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



static void get_seqblock_start_and_end_seqtime(const struct SeqTrack *seqtrack,
                                               const struct SeqBlock *seqblock,
                                               const struct Blocks *block,
                                               int64_t start_abstime, int64_t end_abstime,
                                               int64_t *start_seqtime, int64_t *end_seqtime)
{
  if(start_abstime==-1){
    if(seqblock==NULL){
      *start_seqtime = 0;
      *end_seqtime = 30000;
      R_ASSERT(false);
      return;
    }
  }
  
  *start_seqtime = start_abstime==-1 ? -1 : get_seqtime_from_abstime(seqtrack, seqblock, start_abstime);

  if (end_abstime == -1){
    *end_seqtime = -1;
    return;
  }

  double reltempo = ATOMIC_DOUBLE_GET(block->reltempo);
  
  if (reltempo==1.0) {
    *end_seqtime = get_seqtime_from_abstime(seqtrack, seqblock, end_abstime);
  } else { 
    double blocklen = getBlockSTimeLength(block);      
    int64_t startseqtime = (*start_seqtime)==-1 ? seqblock->time : (*start_seqtime);
    double stretch;
    
    if (start_abstime==-1) {
      stretch = seqblock->stretch;
    } else {
      int64_t nonstretched_abs_duration = blocklen / reltempo;      
      int64_t stretched_abs_duration = end_abstime-start_abstime;
      stretch = (double)stretched_abs_duration / (double)nonstretched_abs_duration;
    }
    
    *end_seqtime = startseqtime + blocklen * stretch;
  }
}

int createSeqblock(int seqtracknum, int blocknum, int64_t pos, int64_t endpos){
  VALIDATE_TIME(pos, -1);
  
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
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

int createGfxGfxSeqblock(int seqtracknum, int blocknum, int64_t pos, int64_t endpos){
  VALIDATE_TIME(pos, -1);
  
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return -1;

  struct Blocks *block = getBlockFromNum(blocknum);
  if (block==NULL)
    return -1;

  int64_t start_seqtime;
  int64_t end_seqtime;

  get_seqblock_start_and_end_seqtime(seqtrack, NULL, block, pos, endpos, &start_seqtime, &end_seqtime);
  
  return SEQTRACK_insert_gfx_gfx_block(seqtrack, block, start_seqtime, end_seqtime);
}

// seqblocks

void setCurrSeqblock(int seqblocknum, int seqtracknum){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack);
  if (seqblock==NULL)
    return;
  g_curr_seqblock = seqblock;
  SEQUENCER_update();
}

void cancelCurrSeqblock(void){
  g_curr_seqblock = NULL;
  SEQUENCER_update();
}

int getNumSeqblocks(int seqtracknum){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return 0;
  else
    return seqtrack->seqblocks.num_elements;
}

int getNumGfxGfxSeqblocks(int seqtracknum){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return 0;
  else
    return seqtrack->gfx_gfx_seqblocks.num_elements;
}

int64_t getSeqblockStartTime(int seqblocknum, int seqtracknum){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack);
  if (seqblock==NULL)
    return 0;

  SEQTRACK_update_all_seqblock_start_and_end_times(seqtrack);
    
  return seqblock->start_time * MIXER_get_sample_rate(); //seqblock->time;
}

int64_t getSeqblockEndTime(int seqblocknum, int seqtracknum){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack);
  if (seqblock==NULL)
    return 0;

  SEQTRACK_update_all_seqblock_start_and_end_times(seqtrack);

  return seqblock->end_time * MIXER_get_sample_rate();
}

int64_t getSeqblockGfxStartTime(int seqblocknum, int seqtracknum){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack);
  if (seqblock==NULL)
    return 0;

  SEQTRACK_update_all_seqblock_gfx_start_and_end_times(seqtrack);
    
  return seqblock->start_time * MIXER_get_sample_rate(); //seqblock->time;
}

int64_t getSeqblockGfxEndTime(int seqblocknum, int seqtracknum){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack);
  if (seqblock==NULL)
    return 0;

  SEQTRACK_update_all_seqblock_gfx_start_and_end_times(seqtrack);

  return seqblock->end_time * MIXER_get_sample_rate();
}

// seqblock area
float getSeqblockX1(int seqblocknum, int seqtracknum){
  if (getSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_x1(seqblocknum, seqtracknum);
}

float getSeqblockY1(int seqblocknum, int seqtracknum){
  if (getSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_y1(seqblocknum, seqtracknum);
}

float getSeqblockX2(int seqblocknum, int seqtracknum){
  if (getSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_x2(seqblocknum, seqtracknum);
}

float getSeqblockY2(int seqblocknum, int seqtracknum){
  if (getSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_y2(seqblocknum, seqtracknum);
}


// seqblock left stretch area

float getSeqblockLeftStretchX1(int seqblocknum, int seqtracknum){
  if (getSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_left_stretch_x1(seqblocknum, seqtracknum);
}

float getSeqblockLeftStretchY1(int seqblocknum, int seqtracknum){
  if (getSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_left_stretch_y1(seqblocknum, seqtracknum);
}

float getSeqblockLeftStretchX2(int seqblocknum, int seqtracknum){
  if (getSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_left_stretch_x2(seqblocknum, seqtracknum);
}

float getSeqblockLeftStretchY2(int seqblocknum, int seqtracknum){
  if (getSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_left_stretch_y2(seqblocknum, seqtracknum);
}

// seqblock right stretch area

float getSeqblockRightStretchX1(int seqblocknum, int seqtracknum){
  if (getSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_right_stretch_x1(seqblocknum, seqtracknum);
}

float getSeqblockRightStretchY1(int seqblocknum, int seqtracknum){
  if (getSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_right_stretch_y1(seqblocknum, seqtracknum);
}

float getSeqblockRightStretchX2(int seqblocknum, int seqtracknum){
  if (getSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_right_stretch_x2(seqblocknum, seqtracknum);
}

float getSeqblockRightStretchY2(int seqblocknum, int seqtracknum){
  if (getSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_right_stretch_y2(seqblocknum, seqtracknum);
}

// move seqblock / set stretch

static void positionSeqblock2(int64_t start_abstime, int64_t end_abstime, int seqblocknum, int seqtracknum, bool is_gfx){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack);
  if (seqblock==NULL)
    return;

  VALIDATE_TIME2(start_abstime,);
  VALIDATE_TIME2(end_abstime,);

  int64_t start_seqtime;
  int64_t end_seqtime;

  get_seqblock_start_and_end_seqtime(seqtrack, seqblock, seqblock->block, start_abstime, end_abstime, &start_seqtime, &end_seqtime);
  
  //printf("Trying to move seqblocknum %d/%d to %d\n",seqtracknum,seqblocknum,(int)abstime);
  SEQTRACK_set_seqblock_start_and_stop(seqtrack, seqblock, start_seqtime, end_seqtime, is_gfx);
}

void positionSeqblock(int64_t start_abstime, int64_t end_abstime, int seqblocknum, int seqtracknum){
  positionSeqblock2(start_abstime, end_abstime, seqblocknum, seqtracknum, false);
}

void positionSeqblockGfx(int64_t start_abstime, int64_t end_abstime, int seqblocknum, int seqtracknum){
  positionSeqblock2(start_abstime, end_abstime, seqblocknum, seqtracknum, true);
}

void moveSeqblock(int64_t abstime, int seqblocknum, int seqtracknum, int new_seqtracknum){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack);
  if (seqblock==NULL)
    return;

  VALIDATE_TIME(abstime,);
  
  if (new_seqtracknum==-1)
    new_seqtracknum = seqtracknum;

  struct SeqTrack *new_seqtrack = getSeqtrackFromNum(new_seqtracknum);
  if (new_seqtrack==NULL)
    return;
  
  ATOMIC_SET(root->song->curr_seqtracknum, new_seqtracknum);
  
  //printf("Trying to move seqblocknum %d/%d to %d\n",seqtracknum,seqblocknum,(int)abstime);
  SEQTRACK_move_seqblock(seqtrack, seqblock, abstime);
}

void moveSeqblockGfx(int64_t abstime, int seqblocknum, int seqtracknum, int new_seqtracknum){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack);
  if (seqblock==NULL)
    return;

  VALIDATE_TIME(abstime,);
  
  if (new_seqtracknum==-1)
    new_seqtracknum = seqtracknum;

  struct SeqTrack *new_seqtrack = getSeqtrackFromNum(new_seqtracknum);
  if (new_seqtrack==NULL)
    return;
  
  ATOMIC_SET(root->song->curr_seqtracknum, new_seqtracknum);
  
  //printf("Trying to move seqblocknum %d/%d to %d\n",seqtracknum,seqblocknum,(int)abstime);
  SEQTRACK_move_gfx_seqblock(seqtrack, seqblock, abstime);
}


double getSeqblockStretch(int seqblocknum, int seqtracknum){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack);
  if (seqblock==NULL)
    return 1;

  return seqblock->stretch;
}

double getSeqblockStretchGfx(int seqblocknum, int seqtracknum){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack);
  if (seqblock==NULL)
    return 1;

  return seqblock->gfx_stretch;
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

void deleteSeqblock(int seqblocknum, int seqtracknum){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack);
  if (seqblock==NULL)
    return;

  undoSequencer();
  
  SEQTRACK_delete_seqblock(seqtrack, seqblock);

  ATOMIC_SET(root->song->curr_seqtracknum, R_MAX(seqtracknum -1, 0));
  BS_UpdatePlayList();
}

void deleteGfxGfxSeqblock(int seqblocknum, int seqtracknum){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getGfxGfxSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack);
  if (seqblock==NULL)
    return;

  SEQTRACK_delete_gfx_gfx_seqblock(seqtrack, seqblock);

  SEQUENCER_update();
}

int getSeqblockBlocknum(int seqblocknum, int seqtracknum){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack);
  if (seqblock==NULL)
    return 0;

  return seqblock->block->l.num;
}

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
  VECTOR_FOR_EACH(struct SeqTrack *seqtrack, &root->song->seqtracks){
    VECTOR_FOR_EACH(struct SeqBlock *seqblock, &seqtrack->seqblocks){
      if (seqblock->is_selected)
        ret++;
    }END_VECTOR_FOR_EACH;
  }END_VECTOR_FOR_EACH;

  return ret;
}

void selectSeqblock(bool is_selected, int seqblocknum, int seqtracknum){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack);
  if (seqblock==NULL)
    return;

  if (  seqblock->is_selected != is_selected){
    seqblock->is_selected = is_selected;
    SEQUENCER_update();
  }
}

bool isSeqblockSelected(int seqblocknum, int seqtracknum){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack);
  if (seqblock==NULL)
    return false;

  return seqblock->is_selected;
}

bool isSeqblockTrackEnabled(int tracknum, int seqblocknum, int seqtracknum){
  if (tracknum < 0 || tracknum >= MAX_DISABLED_SEQBLOCK_TRACKS){
    handleError("setSeqblockTrackEnabled: Illegal tracknum: %d", tracknum);
    return false;
  }
    
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack);
  if (seqblock==NULL)
    return false;

  return !seqblock->track_is_disabled[tracknum];
}

  
void setSeqblockTrackEnabled(bool is_enabled, int tracknum, int seqblocknum, int seqtracknum){
  if (tracknum < 0 || tracknum >= MAX_DISABLED_SEQBLOCK_TRACKS){
    handleError("setSeqblockTrackEnabled: Illegal tracknum: %d", tracknum);
    return;
  }
    
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack);
  if (seqblock==NULL)
    return;

  if (seqblock->track_is_disabled[tracknum] == is_enabled){
    PC_Pause();{
      seqblock->track_is_disabled[tracknum] = !is_enabled;
    }PC_StopPause(NULL);
    SEQUENCER_update();
  }
}

void cutSelectedSeqblocks(void){
  evalScheme("(FROM_C-cut-all-selected-seqblocks)");      
}

void pasteSeqblocks(int seqtracknum, int64_t abstime){
  //printf(" pasteSeqblocks. seqtracknum: %d, abstime: %f\n", seqtracknum, (double)abstime);

  //abort();

  if (seqtracknum==-1)
    seqtracknum = getSeqtrackFromY(tevent.y);

  printf("seqtracknum: %d\n", seqtracknum);

  if (seqtracknum==-1)
    return;

  printf("abstime: %d\n", (int)abstime);

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



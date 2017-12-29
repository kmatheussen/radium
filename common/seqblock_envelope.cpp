/* Copyright 2017 Kjetil S. Matheussen

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
#include "hashmap_proc.h"
#include "patch_proc.h"
#include "seqtrack_proc.h"
#include "instruments_proc.h"
#include "Vector.hpp"
#include "../Qt/Qt_mix_colors.h"
#include "../Qt/Qt_colors_proc.h"
#include "SeqAutomation.hpp"
#include "../audio/SoundPlugin.h"
#include "../audio/SoundPlugin_proc.h"
#include "../audio/SoundProducer_proc.h"

#include "song_tempo_automation_proc.h"

#include <QPainter>

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wshorten-64-to-32"
#include <QVector> // Shortening warning in the QVector header. Temporarily turned off by the surrounding pragmas.
#pragma clang diagnostic pop


#include "seqblock_envelope_proc.h"


// g_curr_seqblock_envelope_seqtrack is set to NULL by ~SeqblockEnvelope or SEQBLOCK_ENVELOPE_cancel_curr_automation.
// Note that it not possible for a seqtrack to be valid, and its seqtrack->seqblockenvelope value to be invalid, or
// vice versa, so it's fine using the SeqblockEnvelope destructor for setting this value to NULL when the seqtrack is deleted.
static struct SeqTrack *g_curr_seqblock_envelope_seqtrack = NULL;
static struct SeqBlock *g_curr_seqblock_envelope_seqblock = NULL;

static void assert_no_curr_seqtrack(void){
  if (g_curr_seqblock_envelope_seqtrack != NULL){
    R_ASSERT_NON_RELEASE(false);
    g_curr_seqblock_envelope_seqtrack = NULL;
  }

  if (g_curr_seqblock_envelope_seqblock != NULL){
    R_ASSERT_NON_RELEASE(false);
    g_curr_seqblock_envelope_seqblock = NULL;
  }
}

static void set_no_curr_seqtrack(void){
  g_curr_seqblock_envelope_seqtrack = NULL;
  g_curr_seqblock_envelope_seqblock = NULL;
}


namespace{

struct AutomationNode{
  double time; // seqtime format
  double value;
  int logtype;
};

static AutomationNode create_node(double seqtime, double value, int logtype){
  AutomationNode node = {
    .time = seqtime,
    .value = value,
    .logtype = logtype
  };
  return node;
}

static hash_t *get_node_state(const AutomationNode &node){
  hash_t *state = HASH_create(5);
  
  HASH_put_float(state, "seqtime", node.time);
  HASH_put_float(state, "value", node.value);
  HASH_put_int(state, "logtype", node.logtype);

  return state;
}

static AutomationNode create_node_from_state(hash_t *state){
  return create_node(HASH_get_float(state, "seqtime"),
                     HASH_get_float(state, "value"),
                     HASH_get_int32(state, "logtype"));
}


struct Automation{
  radium::SeqAutomation<AutomationNode> automation;
  double last_value = -1.0;

  bool islegalnodenum(int nodenum){
    return nodenum>=0 && (nodenum<=automation.size()-1);
  }

  hash_t *get_state(void) const {
    return automation.get_state(get_node_state);
  }

  Automation()
  {
    SEQBLOCK_ENVELOPE_cancel_curr_automation();
  }

  Automation(const hash_t *state){
    automation.create_from_state(state, create_node_from_state);
  }

  ~Automation(){
    assert_no_curr_seqtrack();
  }
};

}

struct SeqblockEnvelope {
  
private:
  
  //struct SeqTrack *_seqtrack; // Not used, but can be practical when debugging.

#if !defined(RELEASE)
  int magic = 918342; // Check that we are freeing the correct data.
#endif

  
public:

  struct SeqTrack *_seqtrack;
  struct SeqBlock *_seqblock;

  Automation _automation;
  
  SeqblockEnvelope(struct SeqTrack *seqtrack, struct SeqBlock *seqblock, const hash_t *state = NULL)
    : _seqtrack(seqtrack)
    , _seqblock(seqblock)
  {
    SEQBLOCK_ENVELOPE_cancel_curr_automation();
          
    if (state != NULL) {

      _automation = Automation(state);
    
    } else {

      add_automation(0.0, 1.0, LOGTYPE_LINEAR,
                     1.0, 0.0);

      _automation.automation.add_node(create_node(0.2, 0.1, LOGTYPE_LINEAR));
      _automation.automation.add_node(create_node(0.7, 1.0, LOGTYPE_LINEAR));
    }
  }

  ~SeqblockEnvelope(){
#if !defined(RELEASE)
    if (magic != 918342)
      abort();
#endif

    if (g_curr_seqblock_envelope_seqblock!=NULL && g_curr_seqblock_envelope_seqblock->envelope==this){
      set_no_curr_seqtrack(); // It's a little bit unclear what data is available and not right now. We can think through the situation, and probably conclude that it's fine to call SEQBLOCK_ENVELOPE_cancel_curr_automation() no matter what, but this is also fine, probably clearer, and much simpler since we don't have to worry whether it's safe to call SEQBLOCK_ENVELOPE_cancel_curr_automation.
    } else {
      SEQBLOCK_ENVELOPE_cancel_curr_automation();
    }

    assert_no_curr_seqtrack();
  }
  
  hash_t *get_state(void) const {
    return _automation.get_state();
  }

  int add_automation(double seqtime1, double value1, int logtype, double seqtime2, double value2){
    _automation.automation.add_node(create_node(seqtime1, value1, logtype));

    int ret = _automation.automation.add_node(create_node(seqtime2, value2, logtype));
    
    SEQTRACK_update(_seqtrack);
    
    return ret;
  }
};
 

struct SeqblockEnvelope *SEQBLOCK_ENVELOPE_create(struct SeqTrack *seqtrack, struct SeqBlock *seqblock, const hash_t *automation_state){
  return new SeqblockEnvelope(seqtrack, seqblock, automation_state);
}

void SEQBLOCK_ENVELOPE_free(struct SeqblockEnvelope *seqblockenvelope){
  delete seqblockenvelope;
}

int SEQBLOCK_ENVELOPE_add_automation(struct SeqblockEnvelope *seqblockenvelope, double seqtime1, double value1, int logtype, double seqtime2, double value2){
  return seqblockenvelope->add_automation(seqtime1, value1, logtype, seqtime2, value2);
}

int SEQBLOCK_ENVELOPE_get_num_automations(struct SeqblockEnvelope *seqblockenvelope){
  return seqblockenvelope->_automation.automation.size();
}

double SEQBLOCK_ENVELOPE_get_value(struct SeqblockEnvelope *seqblockenvelope, int nodenum){

  struct Automation &automation = seqblockenvelope->_automation;
  R_ASSERT_RETURN_IF_FALSE2(automation.islegalnodenum(nodenum), 0.5);

  return automation.automation.at(nodenum).value;
}

double SEQBLOCK_ENVELOPE_get_seqtime(struct SeqblockEnvelope *seqblockenvelope, int nodenum){

  struct Automation &automation = seqblockenvelope->_automation;
  R_ASSERT_RETURN_IF_FALSE2(automation.islegalnodenum(nodenum), 0.5);

  return automation.automation.at(nodenum).time + get_seqblock_noninterior_start(seqblockenvelope->seqblock);
}

int SEQBLOCK_ENVELOPE_get_logtype(struct SeqblockEnvelope *seqblockenvelope, int nodenum){

  struct Automation &automation = seqblockenvelope->_automation;
  R_ASSERT_RETURN_IF_FALSE2(automation.islegalnodenum(nodenum), 0);

  return automation.automation.at(nodenum).logtype;
}

int SEQBLOCK_ENVELOPE_get_num_nodes(struct SeqblockEnvelope *seqblockenvelope){

  struct Automation &automation = seqblockenvelope->_automation;
  return automation.automation.size();
}
  

int SEQBLOCK_ENVELOPE_add_node(struct SeqblockEnvelope *seqblockenvelope, double seqtime, double value, int logtype){
  if (seqtime < 0)
    seqtime = 0;

  struct Automation &automation = seqblockenvelope->_automation;

  value = R_BOUNDARIES(0.0, value, 1.0);

  double time = R_BOUNDARIES(0,
                             seqtime - get_seqblock_noninterior_start(seqblock),
                             SEQBLOCK_get_seq_duration(seqblock)
                             );

  int ret = automation.automation.add_node(create_node(time, value, logtype));
  
  SEQTRACK_update(seqblockenvelope->_seqtrack);
  
  return ret;
}
                              
void SEQBLOCK_ENVELOPE_delete_node(struct SeqblockEnvelope *seqblockenvelope, int nodenum){

  struct Automation &automation = seqblockenvelope->_automation;
  R_ASSERT_RETURN_IF_FALSE(automation.islegalnodenum(nodenum));

  if (automation.automation.size()<=2){
    R_ASSERT_NON_RELEASE(false);
    return;
  }

  automation.automation.delete_node(nodenum);

  SEQTRACK_update(seqblockenvelope->_seqtrack);
}

void SEQBLOCK_ENVELOPE_set_curr_node(struct SeqblockEnvelope *seqblockenvelope, int nodenum){

  struct Automation &automation = seqblockenvelope->_automation;
  R_ASSERT_RETURN_IF_FALSE(automation.islegalnodenum(nodenum));

  if (automation.automation.get_curr_nodenum() != nodenum){
    automation.automation.set_curr_nodenum(nodenum);
    SEQTRACK_update(seqblockenvelope->_seqtrack);
  }
}

void SEQBLOCK_ENVELOPE_cancel_curr_node(struct SeqblockEnvelope *seqblockenvelope){

  struct Automation &automation = seqblockenvelope->_automation;

  automation.automation.set_curr_nodenum(-1);
  SEQTRACK_update(seqblockenvelope->_seqtrack);
}

// Legal to call even if there is already a current automation.
void SEQBLOCK_ENVELOPE_set_curr_automation(struct SeqTrack *seqtrack, struct SeqBlock *seqblock){
  SEQBLOCK_ENVELOPE_cancel_curr_automation();
  
  struct SeqblockEnvelope *seqblockenvelope = seqblock->envelope;
  
  struct Automation &automation = seqblockenvelope->_automation;
  if (automation.automation.do_paint_nodes()){
    
    R_ASSERT_NON_RELEASE(seqtrack==g_curr_seqblock_envelope_seqtrack);
    R_ASSERT_NON_RELEASE(seqblock==g_curr_seqblock_envelope_seqblock);
    
  } else {

    automation.automation.set_do_paint_nodes(true);
    SEQTRACK_update(seqtrack);
    
  }
  
  g_curr_seqblock_envelope_seqtrack=seqtrack;
  g_curr_seqblock_envelope_seqblock=seqblock;
}

// May be called if it there is no current automation.
void SEQBLOCK_ENVELOPE_cancel_curr_automation(void){
  
  struct SeqTrack *seqtrack = g_curr_seqblock_envelope_seqtrack;
  
  if (seqtrack==NULL){

    
    
  } else {

    struct SeqBlock *seqblock = g_curr_seqblock_envelope_seqblock;
    R_ASSERT_RETURN_IF_FALSE(seqblock!=NULL);

    SeqblockEnvelope *seqblockenvelope = seqblock->envelope;
    R_ASSERT_RETURN_IF_FALSE(seqblockenvelope!=NULL);
    
    Automation &automation = seqblockenvelope->_automation;
    
    if (automation.automation.do_paint_nodes()){
      automation.automation.set_do_paint_nodes(false);
      SEQTRACK_update(seqtrack);
    }else{
      R_ASSERT_NON_RELEASE(false);
    }
  }

  set_no_curr_seqtrack();
}


void SEQBLOCK_ENVELOPE_set(struct SeqTrack *seqtrack, struct SeqBlock *seqblock, int nodenum, double seqtime, double value, int logtype){
  struct SeqblockEnvelope *seqblockenvelope = seqblock->envelope;

  struct Automation &automation = seqblockenvelope->_automation;
  R_ASSERT_RETURN_IF_FALSE(automation.islegalnodenum(nodenum));

  int size = automation.automation.size();

  const AutomationNode *prev = nodenum==0 ? NULL : &automation.automation.at(nodenum-1);
  AutomationNode node = automation.automation.at(nodenum);
  const AutomationNode *next = nodenum==size-1 ? NULL : &automation.automation.at(nodenum+1);

  double mintime = prev==NULL ? 0 : prev->time; //next==NULL ? R_MAX(R_MAX(node.time, seqtime), SONG_get_length()) : prev->time;
  double maxtime = next==NULL ? SEQBLOCK_get_seq_duration(seqblock) : next->time;

  //printf("       mintime: %f, seqtime: %f, maxtime: %f\n",mintime,seqtime,maxtime);
  double time = R_BOUNDARIES(mintime,
                             seqtime - get_seqblock_noninterior_start(seqblock),
                             maxtime);
  
  value = R_BOUNDARIES(0.0, value, 1.0);

  node.time = time;
  node.value = value;
  node.logtype = logtype;

  automation.automation.replace_node(nodenum, node);

  SEQTRACK_update(seqtrack);
}
  

static void RT_handle_seqblock_volume_automation(linked_note_t *linked_notes, struct Patch *patch, struct SoundPlugin *plugin, const int play_id){
  R_ASSERT_NON_RELEASE(is_playing_song());

  for(linked_note_t *linked_note = linked_notes ; linked_note!=NULL ; linked_note=linked_note->next){

    struct Notes *editor_note = linked_note->editor_note;

    if (editor_note!=NULL && editor_note->has_sent_seqblock_volume_automation_this_block==false && linked_note->play_id==play_id){

#if !defined(RELEASE)
      if (editor_note->has_automatically_sent_seqblock_volume_automation_this_block==true)
        abort();
      editor_note->has_automatically_sent_seqblock_volume_automation_this_block = true;
#endif

      editor_note->has_sent_seqblock_volume_automation_this_block = true; // not really necessary.

      if (linked_note->seqtrack == root->song->block_seqtrack){
        R_ASSERT_NON_RELEASE(false); // Left-over from earlier. We are only supposed to be here if playing song.
        continue;
      }

      const note_t &note = linked_note->note;
      const struct SeqBlock *seqblock = note.seqblock;

      if (seqblock==NULL) {

        R_ASSERT(false);

      } else {

        float volume_automation_value =  seqblock->envelope_volume;

        if (volume_automation_value >= 0){

          RT_PATCH_change_velocity(linked_note->seqtrack,
                                   patch,
                                   create_note_t(seqblock,
                                                 editor_note,
                                                 note.id,
                                                 note.pitch,
                                                 note.velocity * volume_automation_value,
                                                 0,
                                                 note.midi_channel,
                                                 0,
                                                 0
                                           ),
                                   0
                                   );
        }
      }
    }
  }
}

// Called after scheduler and before audio.
static void RT_handle_seqblock_volume_automation(void){
  R_ASSERT_NON_RELEASE(is_playing_song());

  const int play_id = ATOMIC_GET_RELAXED(pc->play_id);

  // MIDI patches
  {
    struct Instruments *midi_instrument = get_MIDI_instrument();
    
    VECTOR_FOR_EACH(struct Patch *, patch, &midi_instrument->patches){
      RT_handle_seqblock_volume_automation(patch->playing_notes, patch, NULL, play_id);
    }END_VECTOR_FOR_EACH;
  }

  // Audio patches
  {
    struct Instruments *audio_instrument = get_audio_instrument();
    
    VECTOR_FOR_EACH(struct Patch *, patch, &audio_instrument->patches){
      RT_handle_seqblock_volume_automation(patch->playing_notes, patch, NULL, play_id);
    }END_VECTOR_FOR_EACH;
  }

}

static void RT_clear_all_volume_automation_block_statuses(vector_t *patches){
  R_ASSERT_NON_RELEASE(is_playing_song());

  struct Instruments *audio_instrument = get_audio_instrument();

  VECTOR_FOR_EACH(struct Patch *, patch, &audio_instrument->patches){
    for(linked_note_t *linked_note = patch->playing_notes ; linked_note!=NULL ; linked_note=linked_note->next){
      struct Notes *note = linked_note->editor_note;
      if (note != NULL && note->has_sent_seqblock_volume_automation_this_block==true){
        note->has_sent_seqblock_volume_automation_this_block = false;
#if !defined(RELEASE)
        note->has_automatically_sent_seqblock_volume_automation_this_block = false;
#endif
      }
    }
  }END_VECTOR_FOR_EACH;
}

static void RT_clear_all_volume_automation_block_statuses(void){
  R_ASSERT_NON_RELEASE(is_playing_song());

  RT_clear_all_volume_automation_block_statuses(&get_MIDI_instrument()->patches);
  RT_clear_all_volume_automation_block_statuses(&get_audio_instrument()->patches);
}

static void RT_set_seqblock_volume_automation_values(struct SeqTrack *seqtrack){
  R_ASSERT_NON_RELEASE(is_playing_song());

  const int64_t start_time = seqtrack->start_time;

  VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &seqtrack->seqblocks){

    if (seqtrack->end_time >= seqblock->t.time && seqtrack->start_time <= seqblock->t.time2){

      struct SeqblockEnvelope *seqblockenvelope = seqblock->envelope;

      double s1 = get_seqblock_noninterior_start(seqblock);
      double s2 = get_seqblock_noninterior_end(seqtrack, seqblock);

      double pos = scale_double(start_time,
                                s1, s2,
                                0, 1);

      double new_volume;
      seqblockenvelope->_automation.automation.RT_get_value(pos, new_volume);
      seqblockenvelope->_automation.automation.print();
      printf("new_volume: %f. Pos: %f\n\n", new_volume, pos);
      
      if (new_volume != seqblock->last_envelope_volume){        
        seqblock->last_envelope_volume = new_volume;
        seqblock->envelope_volume = new_volume;
      } else {
        seqblock->envelope_volume = -1;
      }
      
    }

  }END_VECTOR_FOR_EACH;
}


// Called from player.c
*void RT_SEQBLOCK_ENVELOPE_called_before_scheduler(void){
  if (is_playing_song()==false)
    return;
  
  RT_clear_all_volume_automation_block_statuses();
}


// Called from seqtrack.cpp
void RT_SEQBLOCK_ENVELOPE_called_before_editor(struct SeqTrack *seqtrack){
  if (is_playing_song()==false)
    return;

  RT_set_seqblock_volume_automation_values(seqtrack);
}

// Called from player.c
void RT_SEQBLOCK_ENVELOPE_called_after_scheduler_and_before_audio(void){
  if (is_playing_song()==false)
    return;

  RT_handle_seqblock_volume_automation();
}

// Called from player.c
void RT_SEQBLOCK_ENVELOPE_called_when_player_stopped(void){
  ALL_SEQTRACKS_FOR_EACH(){

    //fprintf(stderr, "seqiterator: %d, num_seqtracks: %d\n", seqiterator666, root->song->seqtracks.num_elements);

    VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &seqtrack->seqblocks){

      struct SeqblockEnvelope *seqblockenvelope = seqblock->envelope;
      
      seqblockenvelope->_automation.last_value = -1.0;
      seqblockenvelope->_automation.last_value = -1.0;

      seqblock->last_envelope_volume = -1;
      seqblock->envelope_volume = -1;

    }END_VECTOR_FOR_EACH;

  }END_ALL_SEQTRACKS_FOR_EACH;
}



hash_t *SEQBLOCK_ENVELOPE_get_state(struct SeqblockEnvelope *seqblockenvelope){
  return seqblockenvelope->get_state();
}


static float get_node_x2(const struct SeqblockEnvelope *seqblockenvelope, const AutomationNode &node, double start_time, double end_time, float x1, float x2){
  //int64_t abstime = get_abstime_from_seqtime(seqtrack, NULL, node.time);

  //printf("      GET_NODE_X2 returned %f - %f. start_time: %f, end_time: %f\n",abstime/48000.0, scale(abstime, start_time, end_time, x1, x2), start_time/48000.0, end_time/48000.0);
  return scale(node.time, 0, 1, x1, x2);
}

static float get_node_x(const AutomationNode &node, double start_time, double end_time, float x1, float x2, void *data){
  return get_node_x2((const struct SeqblockEnvelope*)data, node, start_time, end_time, x1, x2);
}

float SEQBLOCK_ENVELOPE_get_node_x(struct SeqblockEnvelope *seqblockenvelope, int nodenum){

  struct Automation &automation = seqblockenvelope->_automation;
  R_ASSERT_RETURN_IF_FALSE2(automation.islegalnodenum(nodenum), 0);

  double start_time = SEQUENCER_get_visible_start_time();
  double end_time = SEQUENCER_get_visible_end_time();

  float x1 = SEQBLOCK_get_x1(seqblockenvelope->_seqtrack, seqblockenvelope->_seqblock);
  float x2 = SEQBLOCK_get_x2(seqblockenvelope->_seqtrack, seqblockenvelope->_seqblock);
  
  const AutomationNode &node = automation.automation.at(nodenum);

  return get_node_x2(seqblockenvelope, node, start_time, end_time, x1, x2);
}

static float get_node_y(const AutomationNode &node, float y1, float y2){
  return scale(node.value, 0, 1, y2, y1);
}

float SEQBLOCK_ENVELOPE_get_node_y(struct SeqblockEnvelope *seqblockenvelope, int seqtracknum, int nodenum){
  float y1 = SEQTRACK_get_y1(seqtracknum) + SEQBLOCK_get_header_height();
  float y2 = SEQTRACK_get_y2(seqtracknum);
  
  struct Automation &automation = seqblockenvelope->_automation;
  R_ASSERT_RETURN_IF_FALSE2(automation.islegalnodenum(nodenum), 0);

  return get_node_y(automation.automation.at(nodenum), y1, y2);
}

void SEQBLOCK_ENVELOPE_paint(QPainter *p, const struct SeqBlock *seqblock, float x1, float y1, float x2, float y2, bool paint_nodes){

  struct SeqblockEnvelope *seqblockenvelope = seqblock->envelope;

  struct Automation &automation = seqblockenvelope->_automation;
  
  //automation.automation.set_do_paint_nodes(paint_nodes);

  automation.automation.paint(p, x1, y1, x2, y2, 0, 1, QColor("white"), get_node_y, NULL, seqblockenvelope); //get_node_y, get_node_x, seqblockenvelope);
}

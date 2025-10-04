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

#define DO_DEBUG 0

#include <memory>

#include "nsmtracker.h"
#include "hashmap_proc.h"
#include "patch_proc.h"
#include "sequencer_proc.h"
#include "player_pause_proc.h"
#include "instruments_proc.h"
#include "Vector.hpp"
//#include "Array.hpp"
#include "../Qt/Qt_mix_colors.h"
#include "../Qt/Qt_colors_proc.h"
#include "SeqAutomation.hpp"
#include "../audio/SoundPlugin.h"
#include "../audio/SoundPlugin_proc.h"
#include "../audio/SoundProducer_proc.h"
#include "../audio/Envelope.hpp"
#include "../audio/Juce_plugins_proc.h"

#include "../api/api_proc.h"

#include "song_tempo_automation_proc.h"

#include <QPainter>

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wshorten-64-to-32"
#include <QVector> // Shortening warning in the QVector header. Temporarily turned off by the surrounding pragmas.
#pragma clang diagnostic pop

#include "seqblock_automation_proc.h"

#define MAX_STRETCH 5

#if 0 // Very inefficient since Envelope::get_y is not a const method. ('_last_i' will be set back to 1 very often)

radium::Envelope g_linear_fade_in = radium::Envelope(FADE_LINEAR, 1.0, true);
radium::Envelope g_fast_fade_in = radium::Envelope(FADE_FAST, 1.0, true);
radium::Envelope g_slow_fade_in = radium::Envelope(FADE_SLOW, 1.0, true);
radium::Envelope g_constant_power_fade_in = radium::Envelope(FADE_CONSTANT_POWER, 1.0, true);
radium::Envelope g_symmetric_fade_in = radium::Envelope(FADE_SYMMETRIC, 1.0, true);

radium::Envelope g_linear_fade_out = radium::Envelope(FADE_LINEAR, 1.0, false);
radium::Envelope g_fast_fade_out = radium::Envelope(FADE_FAST, 1.0, false);
radium::Envelope g_slow_fade_out = radium::Envelope(FADE_SLOW, 1.0, false);
radium::Envelope g_constant_power_fade_out = radium::Envelope(FADE_CONSTANT_POWER, 1.0, false);
radium::Envelope g_symmetric_fade_out = radium::Envelope(FADE_SYMMETRIC, 1.0, false);

#endif




// g_curr_seqblock_automation_seqtrack is set to NULL by ~SeqblockEnvelope or SEQBLOCK_AUTOMATION_cancel_curr_automation.
// Note that it not possible for a seqtrack to be valid, and its seqtrack->seqblockenvelope gain to be invalid, or
// vice versa, so it's fine using the SeqblockEnvelope destructor for setting this value to NULL when the seqtrack is deleted.
static struct SeqTrack *g_curr_seqblock_automation_seqtrack = NULL;
static struct SeqBlock *g_curr_seqblock_automation_seqblock = NULL;

static void assert_no_curr_seqtrack(void){
  if (g_curr_seqblock_automation_seqtrack != NULL){
    R_ASSERT_NON_RELEASE(false);
    g_curr_seqblock_automation_seqtrack = NULL;
  }

  if (g_curr_seqblock_automation_seqblock != NULL){
    R_ASSERT_NON_RELEASE(false);
    g_curr_seqblock_automation_seqblock = NULL;
  }
}

static void set_no_curr_seqtrack(void){
  g_curr_seqblock_automation_seqtrack = NULL;
  g_curr_seqblock_automation_seqblock = NULL;
}


namespace{

static radium::AutomationNode create_node(double seqtime, double db, int logtype){
  radium::AutomationNode node = {
    .time = seqtime,
    .value = db,
    .logtype = logtype
  };
  return node;
}

static hash_t *get_node_state(const radium::AutomationNode &node, void *data){
  hash_t *state = HASH_create(5);
  
  HASH_put_float(state, ":seqtime", node.time);
  HASH_put_float(state, (const char*)data, node.value);
  HASH_put_int(state, ":logtype", node.logtype);

  return state;
}

}


/*
static radium::AutomationNode create_node_from_state(hash_t *state, double state_samplerate){
  double time = HASH_get_float(state, ":seqtime");
  return create_node(state_samplerate < 0 ? time : time*(double)pc->pfreq/state_samplerate,
                     HASH_get_float(state, ":db"),
                     HASH_get_int32(state, ":logtype"));
}
*/



struct SeqblockAutomation : public radium::NodeFromStateProvider<radium::AutomationNode>{

  const struct SeqTrack *_seqtrack; // not necessary to gc-protect. seqblockautomation is freed from the seqblock finalizer.
  struct SeqBlock *_seqblock; // not necessary to gc-protect. seqblockautomation is freed from the seqblock finalizer.

  enum Seqblock_Automation_Type _sat;

  radium::SeqAutomation<radium::AutomationNode> _automation;

  double _min_value;
  double _default_value;
  double _max_value;

  bool _is_enabled = false;

  bool _is_temporary;

private:

  // Not making copies anywyere (except in make_copy). Adding these to detect bugs.
  SeqblockAutomation(const SeqblockAutomation&) = delete;
  SeqblockAutomation& operator=(const SeqblockAutomation&) = delete;

public:
  
  SeqblockAutomation(const struct SeqTrack *seqtrack, struct SeqBlock *seqblock, enum Seqblock_Automation_Type sat, const dyn_t state, double state_samplerate, bool is_temporary = false)
    : _seqtrack(seqtrack)
    , _seqblock(seqblock)
    , _sat(sat)
    , _min_value(0.0)
    , _default_value(0.0)
    , _max_value(1.0)
    , _is_temporary(is_temporary)
  {

    switch(_sat){

      case SAT_VOLUME:
        _min_value = MIN_DB;
        _max_value = MAX_SEQBLOCK_VOLUME_ENVELOPE_DB;
        _default_value = 0.0;
        break;

      case SAT_GRAIN_OVERLAP:
        _min_value = 0.1;
        _max_value = 50.0;
        _default_value = 2.0;
        break;

      case SAT_GRAIN_LENGTH:
        _min_value = 0.1;
        _max_value = 1000.0;
        _default_value = 50.0;
        break;

      case SAT_GRAIN_JITTER:
        _min_value = 0.0;
        _max_value = 1.0;
        _default_value = 0.0;
        break;

      case SAT_GRAIN_RAMP:
        _min_value = 0.0;
        _max_value = 0.5;
        _default_value = 0.4;
        break;

      case SAT_STRETCH:
        _min_value = -MAX_STRETCH + 1;
        _max_value = MAX_STRETCH - 1;
        _default_value = 0.0;
        break;

      case SAT_SPEED:
        _min_value = -MAX_STRETCH + 1;
        _max_value = MAX_STRETCH - 1;
        _default_value = 0.0;
        break;

      default:
        R_ASSERT(false);
        break;
    }

    R_ASSERT(_default_value >= _min_value);
    R_ASSERT(_default_value <= _max_value);

    if (false==_is_temporary)
      SEQBLOCK_AUTOMATION_cancel_curr_automation();

    R_ASSERT(!equal_doubles(state_samplerate, 0));

    if (state.type != UNINITIALIZED_TYPE) {

      create_from_state(state, state_samplerate);

    } else {

      double duration = seqblock->t.default_duration;

      _automation.add_node(create_node(0, _default_value, LOGTYPE_LINEAR));
      _automation.add_node(create_node(duration, _default_value, LOGTYPE_LINEAR));
    }

    if (_sat==SAT_STRETCH || _sat==SAT_SPEED){
      _automation.new_rt_data_has_been_created_data = this;
      _automation.new_rt_data_has_been_created = SeqblockAutomation::new_rt_data_has_been_created;
    }
  }

  ~SeqblockAutomation(){ 
    if (_is_temporary)
      return;

    if (g_curr_seqblock_automation_seqblock!=NULL && g_curr_seqblock_automation_seqblock->automations[_sat]==this){
      set_no_curr_seqtrack(); // It's a little bit unclear what data is available and not right now. We can think through the situation, and probably conclude that it's fine to call SEQBLOCK_AUTOMATION_cancel_curr_automation() no matter what, but this is also fine, probably clearer, and much simpler since we don't have to worry whether it's safe to call SEQBLOCK_AUTOMATION_cancel_curr_automation.
    } else {
      SEQBLOCK_AUTOMATION_cancel_curr_automation();
    }

    assert_no_curr_seqtrack();
  }

  SeqblockAutomation *make_copy(void) const {
    dyn_t state = get_state();

    return new SeqblockAutomation(_seqtrack, _seqblock, _sat, state, -1, true);
  }

private:


private:

  // TODO: Add *automation argument (automation-to-be), and call this function before using it in RT.
  static void new_rt_data_has_been_created(void *data){

    auto *seqautomation = static_cast<SeqblockAutomation*>(data);

    SEQBLOCK_calculate_time_conversion_table(seqautomation->_seqblock, true);
  }

  

public:

  void create_from_state(const dyn_t state, double state_samplerate){
    dyn_t automation_state = state;
    
    if (state.type==HASH_TYPE){
      automation_state = HASH_get_dyn(state.hash, ":nodes");
      _is_enabled = HASH_has_key(state.hash, ":enabled") && HASH_get_bool(state.hash, ":enabled");
    }
    
    _automation.create_from_state(automation_state, this, state_samplerate);
  }

  const char *get_value_name(void) const {
    if (_sat==SAT_VOLUME)
      return ":db"; // compatibility with old songs.
    else
      return ":value";
  }

  radium::AutomationNode create_node_from_state(hash_t *state, double state_samplerate) const override {
    double time = HASH_get_float(state, ":seqtime");
    return create_node(state_samplerate <= 0 ? time : time*(double)pc->pfreq/state_samplerate,
                       HASH_get_float(state, get_value_name()),
                       HASH_get_int32(state, ":logtype"));
  }

  const char *get_display_string(double value) const {
    switch(_sat){

      case SAT_VOLUME:
        if (value <= MIN_DB)
          return "~inf";
        else
          return talloc_format("%.1f dB", value);

      case SAT_GRAIN_OVERLAP:
        return talloc_format("%.2f X", value);

      case SAT_GRAIN_LENGTH:
        return talloc_format("%.2f ms", value);

      case SAT_GRAIN_JITTER:
        return talloc_format("%.2f%%", pow(value, 3.0) * 100.0);

      case SAT_GRAIN_RAMP:
        return talloc_format("%.2f%%", value * 100.0);

      case SAT_STRETCH:
        return talloc_format("%.2fX", _seqblock->conversion_table.stretch_automation_compensation*get_stretch_from_automation(value)*_seqblock->t.stretch);

      case SAT_SPEED:
        return talloc_format("%.2fX", _seqblock->conversion_table.speed_automation_compensation*(1.0/get_speed_from_automation(value)));

      default:
        R_ASSERT(false);
        return "error";
    }
  }

  bool islegalnodenum(int nodenum) const {
    return nodenum>=0 && (nodenum<=_automation.size()-1);
  }

  dyn_t get_state(void) const {
    hash_t *state = HASH_create(2);
    HASH_put_array(state, ":nodes", _automation.get_state(get_node_state, (void*)(get_value_name())));
    HASH_put_bool(state, ":enabled", _is_enabled);
    return DYN_create_hash(state);
  }

  double get_value(int nodenum) const {
    R_ASSERT_RETURN_IF_FALSE2(islegalnodenum(nodenum), 0.5);
    return _automation.at(nodenum).value;
  }
  
  double get_value_for_time(int64_t time) const {
    return _automation.get_value(time);
  }
  
  double get_time(int nodenum) const {
    double seqblock_start = get_seqblock_noninterior_start(_seqblock);
    R_ASSERT_RETURN_IF_FALSE2(islegalnodenum(nodenum), seqblock_start);
    return _automation.at(nodenum).time*_seqblock->t.stretch*_seqblock->t.speed + seqblock_start;
  }

  int get_logtype(int nodenum) const {
    R_ASSERT_RETURN_IF_FALSE2(islegalnodenum(nodenum), 0);
    return _automation.at(nodenum).logtype;
  }

  void update_seqblock(int nodenum) const {
    int num_nodes = _automation.size();

    if (nodenum <= 0 || nodenum >= num_nodes-1 || _sat==SAT_STRETCH || _sat==SAT_SPEED) {

      // also update everything if changing first or last node to repaint the transparent area under the graph. This area spans the whole block.
      SEQBLOCK_update(_seqtrack, _seqblock);
      
    } else {


      int64_t start_time = floor(get_time(R_MAX(0, nodenum-1)));
      int64_t end_time = ceil(get_time(R_MIN(num_nodes-1, nodenum+1)));

      SEQBLOCK_update2(_seqtrack, _seqblock,
                       start_time, end_time
                       );
    }
  }
  
  int add_node(double seqtime, double value, int logtype){
    if (seqtime < 0)
      seqtime = 0;
    
    auto *seqblock = _seqblock;
    
    double time =
      R_BOUNDARIES(0,
                   (seqtime - get_seqblock_noninterior_start(seqblock)) / (seqblock->t.stretch*seqblock->t.speed),
                   seqblock->t.default_duration
                   );
    
    value = R_BOUNDARIES(_min_value, value, _max_value);
    
    int ret = _automation.add_node(create_node(time, value, logtype));
    
    update_seqblock(ret);

    return ret;
  }

  void delete_node(int nodenum){
    R_ASSERT_RETURN_IF_FALSE(islegalnodenum(nodenum));
    
    if (_automation.size()<=2){
      R_ASSERT_NON_RELEASE(false);
      return;
    }
    
    _automation.delete_node(nodenum);

    update_seqblock(nodenum);
  }

  void set_curr_node(int nodenum){
    R_ASSERT_RETURN_IF_FALSE(islegalnodenum(nodenum));
    
    if (_automation.set_curr_nodenum(nodenum))
      update_seqblock(nodenum);
  }

  void cancel_curr_node(void){
    int curr_node = _automation.get_curr_nodenum();
    if(_automation.set_curr_nodenum(-1))
      update_seqblock(curr_node);
  }

  void set_node(int nodenum, double seqtime, double value, int logtype){
    R_ASSERT_RETURN_IF_FALSE(islegalnodenum(nodenum));
    
    int size = _automation.size();
    
    const radium::AutomationNode *prev = nodenum==0 ? NULL : &_automation.at(nodenum-1);
    radium::AutomationNode node = _automation.at(nodenum);
    const radium::AutomationNode *next = nodenum==size-1 ? NULL : &_automation.at(nodenum+1);
    
    double mintime = prev==NULL ? 0 : prev->time; //next==NULL ? R_MAX(R_MAX(node.time, seqtime), SONG_get_length()) : prev->time;
    double maxtime = next==NULL ? _seqblock->t.default_duration : next->time;
    
    //printf("     nodenum: %d.  mintime: %f, seqtime: %f, maxtime: %f. next: %p\n",nodenum,mintime,(seqtime - get_seqblock_noninterior_start(seqblock)),maxtime,NULL);
    double time = R_BOUNDARIES(mintime,
                               (seqtime - get_seqblock_noninterior_start(_seqblock)) / (_seqblock->t.stretch*_seqblock->t.speed),
                               maxtime
                               );

    value = R_BOUNDARIES(_min_value, value, _max_value);
    
    node.time = time;
    node.value = value;
    node.logtype = logtype;
    
    _automation.replace_node(nodenum, node);

    update_seqblock(nodenum);
  }

  static float get_node_x2(const SeqblockAutomation *seqblockenvelope, const radium::AutomationNode &node, double start_time, double end_time, float x1, float x2) {
    //int64_t abstime = get_abstime_from_seqtime(seqtrack, NULL, node.time);
    
    //printf("      GET_NODE_X2 returned %f - %f. start_time: %f, end_time: %f\n",abstime/48000.0, scale(abstime, start_time, end_time, x1, x2), start_time/48000.0, end_time/48000.0);  
    double duration = seqblockenvelope->_seqblock->t.default_duration;
    return scale(node.time, 0, duration, x1, x2);
  }

  static float get_node_x_callback(const radium::AutomationNode &node, double start_time, double end_time, float x1, float x2, void *data){ 
    return get_node_x2((const struct SeqblockAutomation*)data, node, start_time, end_time, x1, x2);
  }

  float get_node_x(int nodenum) const {

    R_ASSERT_RETURN_IF_FALSE2(islegalnodenum(nodenum), 0);

    double start_time = SEQUENCER_get_visible_start_time();
    double end_time = SEQUENCER_get_visible_end_time();
    
    double noninterior_start = get_seqblock_noninterior_start(_seqblock);
    double noninterior_end = get_seqblock_noninterior_end(_seqblock);
    
    double t_x1 = SEQUENCER_get_x1();
    double t_x2 = SEQUENCER_get_x2();
    
    float x1 = scale_double(noninterior_start,
                            start_time, end_time,
                            t_x1, t_x2);
    float x2 = scale_double(noninterior_end,
                            start_time, end_time,
                            t_x1, t_x2);
    
    const radium::AutomationNode &node = _automation.at(nodenum);
    
    return SeqblockAutomation::get_node_x2(this, node, start_time, end_time, x1, x2);
  }

  static float get_node_y_callback(const radium::AutomationNode &node, float y1, float y2, void *data){
#if 0
    return scale(db2gain(node.value),
                 db2gain(MIN_DB), db2gain(MAX_SEQBLOCK_VOLUME_ENVELOPE_DB),
                 y2, y1);
#else
    const SeqblockAutomation *seqblockautomation = static_cast<const SeqblockAutomation*>(data);
    
    return scale(node.value,
                 seqblockautomation->_min_value, seqblockautomation->_max_value,  // MIN_DB, MAX_SEQBLOCK_VOLUME_ENVELOPE_DB,
                 y2, y1);
#endif
  }

  double get_node_y(int seqtracknum, int nodenum) const {
    float y1 = SEQTRACK_get_y1(seqtracknum) + SEQBLOCK_get_header_height();
    float y2 = SEQTRACK_get_y2(seqtracknum);
    
    R_ASSERT_RETURN_IF_FALSE2(islegalnodenum(nodenum), 0);
    
    return SeqblockAutomation::get_node_y_callback(_automation.at(nodenum), y1, y2, (void*)this);
  }

  
  const radium::AutomationPainter *get_painter(float x1, float y1, float x2, float y2, bool paint_nodes, float seqblock_x1, float seqblock_x2) {
    
    //_automation.set_do_paint_nodes(paint_nodes);
    
    QColor fill_color;
    if (_sat==SAT_VOLUME)
      fill_color = QColor(80,20,200,50);

    QColor color = get_qcolor((enum ColorNums)(AUTOMATION1_COLOR_NUM + (int)_sat));

    return _automation.get_painter(x1, y1, x2, y2, 0, 1, color, SeqblockAutomation::get_node_y_callback, SeqblockAutomation::get_node_x_callback, this, fill_color, seqblock_x1, seqblock_x2);
  }
};



struct SeqblockAutomation *SEQBLOCK_AUTOMATION_create(const struct SeqTrack *seqtrack, struct SeqBlock *seqblock, enum Seqblock_Automation_Type sat, const dyn_t automation_state, double state_samplerate){
  return new SeqblockAutomation(seqtrack, seqblock, sat, automation_state, state_samplerate);
}

void SEQBLOCK_AUTOMATION_free(struct SeqblockAutomation *seqblockenvelope){  // note: seqblockenvelope might be NULL.
  delete seqblockenvelope;
}

double SEQBLOCK_AUTOMATION_get_min_value(const struct SeqblockAutomation *seqblockenvelope){
  return seqblockenvelope->_min_value;
}

double SEQBLOCK_AUTOMATION_get_default_value(const struct SeqblockAutomation *seqblockenvelope){
  return seqblockenvelope->_default_value;
}

double SEQBLOCK_AUTOMATION_get_max_value(const struct SeqblockAutomation *seqblockenvelope){
  return seqblockenvelope->_max_value;
}

const char *SEQBLOCK_AUTOMATION_get_display_string(struct SeqblockAutomation *seqblockenvelope, double value){
  return seqblockenvelope->get_display_string(value);
}

double SEQBLOCK_AUTOMATION_get_value(struct SeqblockAutomation *seqblockenvelope, int nodenum){
  return seqblockenvelope->get_value(nodenum);
}

double SEQBLOCK_AUTOMATION_get_value_for_time(struct SeqblockAutomation *seqblockenvelope, int64_t time){
  return seqblockenvelope->get_value_for_time(time);
}

double SEQBLOCK_AUTOMATION_get_seqtime(struct SeqblockAutomation *seqblockenvelope, int nodenum){
  return seqblockenvelope->get_time(nodenum);
}

int SEQBLOCK_AUTOMATION_get_logtype(struct SeqblockAutomation *seqblockenvelope, int nodenum){
  return seqblockenvelope->get_logtype(nodenum);
}

int SEQBLOCK_AUTOMATION_get_num_nodes(struct SeqblockAutomation *seqblockenvelope){

  return seqblockenvelope->_automation.size();
}
  

int SEQBLOCK_AUTOMATION_add_node(struct SeqblockAutomation *seqblockenvelope, double seqtime, double db, int logtype){
  return seqblockenvelope->add_node(seqtime, db, logtype);
}
                              
void SEQBLOCK_AUTOMATION_delete_node(struct SeqblockAutomation *seqblockenvelope, int nodenum){
  seqblockenvelope->delete_node(nodenum);
}

void SEQBLOCK_AUTOMATION_set_curr_node(struct SeqblockAutomation *seqblockenvelope, int nodenum){
  seqblockenvelope->set_curr_node(nodenum);
}

void SEQBLOCK_AUTOMATION_cancel_curr_node(struct SeqblockAutomation *seqblockenvelope){
  seqblockenvelope->cancel_curr_node();
}

// Legal to call even if there is already a current automation.
void SEQBLOCK_AUTOMATION_set_curr_automation(struct SeqTrack *seqtrack, struct SeqBlock *seqblock, struct SeqblockAutomation *seqblockenvelope){
  if (g_curr_seqblock_automation_seqtrack == seqtrack)
    if (g_curr_seqblock_automation_seqblock == seqblock)
      return;
  
  SEQBLOCK_AUTOMATION_cancel_curr_automation();
  
  if (seqblockenvelope->_automation.do_paint_nodes()){
    
    R_ASSERT_NON_RELEASE(seqtrack==g_curr_seqblock_automation_seqtrack);
    R_ASSERT_NON_RELEASE(seqblock==g_curr_seqblock_automation_seqblock);
    
  } else {

    //printf("SET DO PAINT NODES\n");
    if(seqblockenvelope->_automation.set_do_paint_nodes(true))
      SEQBLOCK_update(seqtrack, seqblock);
    
  }
  
  g_curr_seqblock_automation_seqtrack = seqtrack;
  g_curr_seqblock_automation_seqblock = seqblock;
}

// May be called if it there is no current automation.
void SEQBLOCK_AUTOMATION_cancel_curr_automation(void){
  
  struct SeqTrack *seqtrack = g_curr_seqblock_automation_seqtrack;
  
  if (seqtrack==NULL){

    
  } else {

    struct SeqBlock *seqblock = g_curr_seqblock_automation_seqblock;
    R_ASSERT_RETURN_IF_FALSE(seqblock!=NULL);

    //printf("   CANCELLING curr automation: %s\n", JUCE_get_backtrace());
    //printf("   CANCELLING curr automation:\n");

    bool do_update = false;
    
    int num_automations = SEQBLOCK_num_automations(seqblock);
    for(int i=0;i<num_automations;i++){
      SeqblockAutomation *seqblockenvelope = seqblock->automations[i];
      if (seqblockenvelope->_automation.set_do_paint_nodes(false))
        do_update = true;
                             
    }

    if(do_update)
      SEQBLOCK_update(seqtrack, seqblock);
  }

  set_no_curr_seqtrack();
}


void SEQBLOCK_AUTOMATION_set(struct SeqblockAutomation *seqblockenvelope, int nodenum, double seqtime, double db, int logtype){
  seqblockenvelope->set_node(nodenum, seqtime, db, logtype);
}

void SEQBLOCK_AUTOMATION_default_duration_changed(struct SeqblockAutomation *seqblockenvelope, int64_t new_default_duration, radium::PlayerLockOnlyIfNeeded *lock){
  seqblockenvelope->_automation.duration_has_changed(new_default_duration, lock);
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
      struct SeqTrack *seqtrack = linked_note->seqtrack;

      if (seqblock==NULL || seqtrack==NULL) {

        R_ASSERT(false);

      } else {

        if (seqblock->curr_gain_changed_this_block || seqtrack->note_gain_has_changed_this_block==true) {
          
          double seqblock_automation_volume = seqblock->curr_gain * seqtrack->note_gain * seqtrack->note_gain_muted;
        
          RT_PATCH_change_velocity(seqtrack,
                                   patch,
                                   create_note_t(seqblock,
                                                 note.id,
                                                 note.pitch,
                                                 note.velocity * seqblock_automation_volume,
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

    
  ALL_SEQTRACKS_FOR_EACH(){

    if (seqtrack->for_audiofiles==false && seqtrack->note_gain_has_changed_this_block==true)
      seqtrack->note_gain_has_changed_this_block = false;
    
  }END_ALL_SEQTRACKS_FOR_EACH;

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

static void RT_set_seqblock_curr_gain(struct SeqTrack *seqtrack, struct SeqBlock *seqblock, const double pos, const int64_t seqblock_pos_start, const int64_t seqblock_pos_end){
  struct SeqblockAutomation *seqblockenvelope = seqblock->automations[SAT_VOLUME];

  double new_db = 0.0;
  if (seqblockenvelope->_is_enabled)
    seqblockenvelope->_automation.RT_get_value(pos, new_db, true);

#if DO_DEBUG
  seqblockenvelope->_automation.print();
  printf("new_db: %f. Pos: %f\n\n", new_db, pos);
#endif

  // Add envelope gain.
  //
  if (fabs(new_db-seqblock->envelope_db) > 0.0001){

    //printf("new db: %f. Old: %f (old gain: %f)\n", new_db, seqblock->envelope_db, seqblock->curr_gain);

    seqblock->envelope_db = new_db;

    if (equal_doubles(new_db, 0.0))
      seqblock->curr_gain = 1.0;
    else
      seqblock->curr_gain = db2gain(new_db);

    seqblock->curr_gain_changed_this_block = true;
      
  } else {

    seqblock->curr_gain_changed_this_block = false;

  }
  

  // Add sample gain.
  //
  if (fabsf(seqblock->gain-1.0f) > 0.0001f){
    
    if (seqblock->curr_gain_changed_this_block) {
      
      seqblock->curr_gain *= seqblock->gain;
      
    } else {
      
      seqblock->curr_gain = seqblock->gain;
      seqblock->curr_gain_changed_this_block = true;
      
    }
    
  }
  
      
  // Add fade out/in gain.
  //
  if (seqblock->fadein > 0.0 || seqblock->fadeout > 0.0){

    if (seqblock_pos_end > 0){

      int64_t seqblock_duration = seqblock->t.time2 - seqblock->t.time;          
                    
      double scaled_pos = (double)seqblock_pos_start / (double)seqblock_duration;

      if (scaled_pos > 1){
            
        R_ASSERT_NON_RELEASE(false);
            
      } else if (scaled_pos < 0){

        // why no assertion here? (missing comment)

      } else {

        if (scaled_pos < seqblock->fadein){

          double fadein_pos = scale_double(scaled_pos,
                                           0, seqblock->fadein,
                                           0, 1);
              
          double fadein = seqblock->fade_in_envelope->get_y(fadein_pos);
              
          if (seqblock->curr_gain_changed_this_block)
            seqblock->curr_gain *= fadein;
          else {
            seqblock->curr_gain = fadein;
            seqblock->curr_gain_changed_this_block = true;
          }
        }
            
        if (scaled_pos > (1.0-seqblock->fadeout)) {
              
          double fadeout_pos = scale_double(scaled_pos,
                                            1.0-seqblock->fadeout, 1,
                                            0, 1);

          double fadeout = seqblock->fade_out_envelope->get_y(fadeout_pos);

          if (seqblock->curr_gain_changed_this_block)
            seqblock->curr_gain *= fadeout;
          else {
            seqblock->curr_gain = fadeout;
            seqblock->curr_gain_changed_this_block = true;
          }
        }
            
      }
    }
  }

}

static void RT_set_seqblock_curr_gains(struct SeqTrack *seqtrack){
  R_ASSERT_NON_RELEASE(is_playing_song());

  const int64_t start_time = seqtrack->start_time;
  const int64_t end_time = seqtrack->end_time;

  VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &seqtrack->seqblocks){

    if (end_time >= seqblock->t.time && start_time <= seqblock->t.time2){

      int64_t seqblock_pos_start = start_time - seqblock->t.time;
      int64_t seqblock_pos_end = end_time - seqblock->t.time;

      double s1 = get_seqblock_noninterior_start(seqblock);
      double time = (start_time-s1) / (seqblock->t.stretch*seqblock->t.speed);

      RT_set_seqblock_curr_gain(seqtrack, seqblock, time, seqblock_pos_start, seqblock_pos_end);
    }

  }END_VECTOR_FOR_EACH;
}

bool RT_seqblock_automation_is_enabled(const struct SeqblockAutomation *automation){
  return automation->_is_enabled;
}

const radium::SeqAutomation<radium::AutomationNode> &SEQBLOCK_AUTOMATION_get_SeqAutomation(const struct SeqblockAutomation *automation){
  return automation->_automation;
}

void SEQBLOCK_AUTOMATION_set_enabled(struct SeqblockAutomation *automation, bool enabled){

  bool need_to_recalculate_time_conversion_table = automation->_sat==SAT_STRETCH || automation->_sat==SAT_SPEED;
    
  radium::PlayerPause pause(is_playing_song() && need_to_recalculate_time_conversion_table);
  
  {
    radium::PlayerLock lock(is_playing_song());
    automation->_is_enabled = enabled;
  }

  if (need_to_recalculate_time_conversion_table)
    SEQBLOCK_calculate_time_conversion_table(automation->_seqblock, true);

  SEQBLOCK_update(automation->_seqtrack, automation->_seqblock);
}

bool RT_maybe_get_seqblock_automation_value(struct SeqblockAutomation *automation, double time, double &value){
  if (automation->_is_enabled==false)
    return false;
  
  automation->_automation.RT_get_value(time, value, true);

  if (automation->_sat==SAT_STRETCH){
    double value1 = get_stretch_from_automation(value);
    value = value1 * automation->_seqblock->conversion_table.stretch_automation_compensation;

    //printf("Stretch: %f (%f * %f)\n", value, value1, g_total);
    //printf("automation speed: %f / %f. compensation: %f. Time: %f\n", value1, value, automation->_seqblock->stretch_automation_compensation, time);

  } else if (automation->_sat==SAT_SPEED)
    value = get_speed_from_automation(value) * automation->_seqblock->conversion_table.speed_automation_compensation;

  return true;
}

// Called from player.c
void RT_SEQBLOCK_AUTOMATION_called_before_scheduler(void){
  if (is_playing_song()==false)
    return;
  
  RT_clear_all_volume_automation_block_statuses();
}


// Called from seqtrack.cpp
void RT_SEQBLOCK_AUTOMATION_called_before_editor(struct SeqTrack *seqtrack){
  if (is_playing_song()==false)
    return;

  RT_set_seqblock_curr_gains(seqtrack);
}

// Called from player.c
void RT_SEQBLOCK_AUTOMATION_called_after_scheduler_and_before_audio(void){
  if (is_playing_song()==false)
    return;

  RT_handle_seqblock_volume_automation();
}

// Called from player.c
void RT_SEQBLOCK_AUTOMATION_called_when_player_stopped(void){
#if 1
  ALL_SEQTRACKS_FOR_EACH(){

    //fprintf(stderr, "seqiterator: %d, num_seqtracks: %d\n", seqiterator666, root->song->seqtracks.num_elements);

    VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &seqtrack->seqblocks){

      seqblock->envelope_db = MIN_DB - 1; // <-- To ensure (fabs(new_db-seqblock->envelope_db) > 0.0001) is true in RT_set_seqblock_curr_gains.
      
      //seqblock->curr_gain = -1; // WARNING: When enabling the Seqtrack_plugin, ensure that we don't use seqblock->curr_gain when player is stopped. If we do, it could be hard to hear since the only thing we do is to invert the phase, at least when there's no envelope volume.

    }END_VECTOR_FOR_EACH;

  }END_ALL_SEQTRACKS_FOR_EACH;
#endif
}



// only used for undo/redo
dyn_t SEQBLOCK_AUTOMATION_get_state(const struct SeqblockAutomation *seqblockenvelope){
  return seqblockenvelope->get_state();
}

// only used for undo/redo, and when loading song that has ":envelope" in state instead of seqblock automation.
void SEQBLOCK_AUTOMATION_apply_state(struct SeqblockAutomation *seqblockenvelope, const dyn_t envelope_state, double state_samplerate){
  R_ASSERT(!equal_doubles(state_samplerate, 0));
  seqblockenvelope->create_from_state(envelope_state, state_samplerate);
  SEQBLOCK_update(seqblockenvelope->_seqtrack, seqblockenvelope->_seqblock);
}

float SEQBLOCK_AUTOMATION_get_node_x(const struct SeqblockAutomation *seqblockenvelope, int nodenum){
  return seqblockenvelope->get_node_x(nodenum);
}

float SEQBLOCK_AUTOMATION_get_node_y(const struct SeqblockAutomation *seqblockenvelope, int seqtracknum, int nodenum){
  return seqblockenvelope->get_node_y(seqtracknum, nodenum);
}

const radium::AutomationPainter *SEQBLOCK_AUTOMATION_get_painter(struct SeqblockAutomation *seqblockenvelope, float x1, float y1, float x2, float y2, bool paint_nodes, float seqblock_x1, float seqblock_x2){
  return seqblockenvelope->get_painter(x1, y1, x2, y2, paint_nodes, seqblock_x1, seqblock_x2);
}

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

#define RADIUM_ACCESS_SEQBLOCK_AUTOMATION 1

#include <memory>

#include "nsmtracker.h"
#include "hashmap_proc.h"
#include "patch_proc.h"
#include "seqtrack_proc.h"
#include "player_pause_proc.h"
#include "instruments_proc.h"
#include "Vector.hpp"
#include "Array.hpp"
#include "../Qt/Qt_mix_colors.h"
#include "../Qt/Qt_colors_proc.h"
#include "SeqAutomation.hpp"
#include "../audio/SoundPlugin.h"
#include "../audio/SoundPlugin_proc.h"
#include "../audio/SoundProducer_proc.h"
//#include "../audio/Envelope.hpp"
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

struct AutomationNode{
  double time; // seqtime format
  double value;
  int logtype;
};

static AutomationNode create_node(double seqtime, double db, int logtype){
  AutomationNode node = {
    .time = seqtime,
    .value = db,
    .logtype = logtype
  };
  return node;
}

static hash_t *get_node_state(const AutomationNode &node, void *data){
  hash_t *state = HASH_create(5);
  
  HASH_put_float(state, ":seqtime", node.time);
  HASH_put_float(state, (const char*)data, node.value);
  HASH_put_int(state, ":logtype", node.logtype);

  return state;
}

}


/*
static AutomationNode create_node_from_state(hash_t *state, double state_samplerate){
  double time = HASH_get_float(state, ":seqtime");
  return create_node(state_samplerate < 0 ? time : time*(double)pc->pfreq/state_samplerate,
                     HASH_get_float(state, ":db"),
                     HASH_get_int32(state, ":logtype"));
}
*/



struct TimeConversionTable{
  double stretch_automation_compensation = 1.0;
  double speed_automation_compensation = 1.0;
  int num_time_conversion_table_elements = 0;
  int64_t *time_conversion_table = NULL; // Array that maps seqtime -> sample position. (necessary when automating stretch or speed)    
};

struct SeqblockAutomation : public radium::NodeFromStateProvider<AutomationNode>{

  struct SeqTrack *_seqtrack; // not necessary to gc-protect. seqblockautomation is freed from the seqblock finalizer.
  struct SeqBlock *_seqblock; // not necessary to gc-protect. seqblockautomation is freed from the seqblock finalizer.

  enum Seqblock_Automation_Type _sat;

  radium::SeqAutomation<AutomationNode> _automation;

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
  
  SeqblockAutomation(struct SeqTrack *seqtrack, struct SeqBlock *seqblock, enum Seqblock_Automation_Type sat, const dyn_t state, double state_samplerate, bool is_temporary = false)
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

    R_ASSERT(state_samplerate != 0);

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

  static double get_stretch_from_automation(double value) {
    if (value < 0.0)
      return 1.0 / (1.0 - value);
    else
      return value + 1.0;
  }

  static double get_speed_from_automation(double value) {
    if (value < 0.0)
      return 1.0 - value;
    else
      return 1.0 / (1.0 + value);
  }

  SeqblockAutomation *make_copy(void) const {
    dyn_t state = get_state();

    return new SeqblockAutomation(_seqtrack, _seqblock, _sat, state, -1, true);
  }

private:

  static void apply_to_time_conversion_table_single(int num_elements, int64_t *ret, radium::SeqAutomationIterator<AutomationNode> &iterator, const enum Seqblock_Automation_Type _sat, const double time_inc, double &total_automation_time) {

    total_automation_time = 0;
    double time = 0.0;

    for(int i=0 ; i<num_elements ; i++, time+=time_inc){
      
      ret[i] = total_automation_time;
        
      double value = iterator.get_value(time);
      double stretch_or_speed = _sat==SAT_SPEED ? SeqblockAutomation::get_speed_from_automation(value) : SeqblockAutomation::get_stretch_from_automation(value);
      
      total_automation_time += RADIUM_BLOCKSIZE / stretch_or_speed;
    }

    ret[num_elements] = total_automation_time;
  }

  static void apply_to_time_conversion_table_double(int num_elements, int64_t *ret, radium::SeqAutomationIterator<AutomationNode> &stretch_iterator, radium::SeqAutomationIterator<AutomationNode> &speed_iterator, const double time_inc, double &total_stretch, double &total_speed) {

    total_stretch = 0;
    total_speed = 0;
    double total_automation_time = 0;
    double time = 0.0;

    for(int i=0 ; i<num_elements ; i++, time+=time_inc){
      
      ret[i] = total_automation_time;
        
      double stretch_value = stretch_iterator.get_value(time);
      double stretch = SeqblockAutomation::get_stretch_from_automation(stretch_value);
      double stretch_duration = RADIUM_BLOCKSIZE / stretch;
      total_stretch += stretch_duration;
      
      double speed_value = speed_iterator.get_value(time);
      double speed = SeqblockAutomation::get_speed_from_automation(speed_value); 
      double speed_duration = RADIUM_BLOCKSIZE / speed;
      total_speed += speed_duration;
     
      total_automation_time += stretch_duration + speed_duration;
    }

    ret[num_elements] = total_automation_time;
  }

  // TODO: This function needs to be memoized so that we don't have to calculate for every pixel when moving a seqblock with stretch automation.
  static TimeConversionTable get_time_conversion_table(struct SeqBlock *seqblock) {
    TimeConversionTable table;
      
    const auto *stretch = seqblock->automations[SAT_STRETCH];
    R_ASSERT_RETURN_IF_FALSE2(stretch!=NULL, table);

    const auto *speed = seqblock->automations[SAT_SPEED];
    R_ASSERT_RETURN_IF_FALSE2(speed!=NULL, table);

    bool stretch_enabled = stretch->_is_enabled;
    bool speed_enabled = speed->_is_enabled;

    if(!stretch_enabled && !speed_enabled)
      return table;

    //const SoundPlugin *plugin = (SoundPlugin*) _seqtrack->patch->patchdata;
    //const double resample_ratio = SEQTRACKPLUGIN_get_resampler_ratio(plugin, _seqblock->sample_id);

    const double total_time = seqblock->t.num_samples; //default_duration / resample_ratio; //2.0;//(s2-s1);
    int num_elements = total_time / RADIUM_BLOCKSIZE;
    
    int64_t *array = (int64_t*)talloc_atomic(sizeof(int64_t)*(num_elements+1));

    //_automation.print();

    double time_inc = (double)seqblock->t.default_duration / (double)num_elements;

    double total_stretch = total_time;
    double total_speed = total_time;

    radium::SeqAutomationIterator<AutomationNode> stretch_iterator(stretch->_automation);
    radium::SeqAutomationIterator<AutomationNode> speed_iterator(speed->_automation);

    const bool do_stretch = stretch->_is_enabled;
    const bool do_speed = speed->_is_enabled;

    if (do_stretch && do_speed)
      SeqblockAutomation::apply_to_time_conversion_table_double(num_elements, array, stretch_iterator, speed_iterator, time_inc, total_stretch, total_speed);
    else if (do_stretch)
      SeqblockAutomation::apply_to_time_conversion_table_single(num_elements, array, stretch_iterator, SAT_STRETCH, time_inc, total_stretch);
    else if (do_speed)
      SeqblockAutomation::apply_to_time_conversion_table_single(num_elements, array, speed_iterator, SAT_SPEED, time_inc, total_speed);
    else
      R_ASSERT(false);

    table.stretch_automation_compensation = total_stretch / total_time;
    table.num_time_conversion_table_elements = num_elements;
    table.time_conversion_table = array;

    if (do_stretch){
      table.stretch_automation_compensation = total_stretch / total_time;
    } else {
      table.stretch_automation_compensation = 0.0;
    }

    if (do_speed){
      table.speed_automation_compensation = total_speed / total_time;
    } else {
      table.speed_automation_compensation = 0.0;
    }

    /*
    if (do_stretch && do_speed){

      // todo: apply stretch.
      table.speed_automation_compensation = total_speed / total_time;

    } else {
      table.speed_automation_compensation = total_speed / total_time;
    }
    */
    
    //printf("    Stretch comp: %f. Speed comp: %f. total_time: %f, total_stretch: %f, total_speed: %f\n", table.stretch_automation_compensation, table.speed_automation_compensation, total_time, total_stretch, total_speed);
    //table.stretch_automation_compensation
    //printf("   DIFF: %f - %f. Compensation: %f\n", total_automation_time, total_time, table.stretch_automation_compensation);

    return table;
  }

public:

  static void calculate_time_conversion_table(struct SeqBlock *seqblock) {
    TimeConversionTable table = SeqblockAutomation::get_time_conversion_table(seqblock);
    {
      radium::PlayerLock lock;
      seqblock->stretch_automation_compensation = table.stretch_automation_compensation;
      seqblock->speed_automation_compensation = table.speed_automation_compensation;
      seqblock->num_time_conversion_table_elements = table.num_time_conversion_table_elements;
      seqblock->time_conversion_table = table.time_conversion_table;
    }
  }


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

  AutomationNode create_node_from_state(hash_t *state, double state_samplerate) const override {
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
        return talloc_format("%.2fX", _seqblock->stretch_automation_compensation*SeqblockAutomation::get_stretch_from_automation(value)*_seqblock->t.stretch);

      case SAT_SPEED:
        return talloc_format("%.2fX", _seqblock->speed_automation_compensation*(1.0/get_speed_from_automation(value)));

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
    HASH_put_dyn(state, ":nodes", _automation.get_state(get_node_state, (void*)(get_value_name())));
    HASH_put_bool(state, ":enabled", _is_enabled);
    return DYN_create_hash(state);
  }

  double get_value(int nodenum){
    R_ASSERT_RETURN_IF_FALSE2(islegalnodenum(nodenum), 0.5);
    return _automation.at(nodenum).value;
  }
  
  double get_value_for_time(int64_t time){
    return _automation.get_value(time);
  }
  
  double get_time(int nodenum){
    double seqblock_start = get_seqblock_noninterior_start(_seqblock);
    R_ASSERT_RETURN_IF_FALSE2(islegalnodenum(nodenum), seqblock_start);
    return _automation.at(nodenum).time*_seqblock->t.stretch + seqblock_start;
  }

  int get_logtype(int nodenum){    
    R_ASSERT_RETURN_IF_FALSE2(islegalnodenum(nodenum), 0);
    return _automation.at(nodenum).logtype;
  }

  int add_node(double seqtime, double value, int logtype){
    if (seqtime < 0)
      seqtime = 0;
    
    auto *seqblock = _seqblock;
    
    double time =
      R_BOUNDARIES(0,
                   (seqtime - get_seqblock_noninterior_start(seqblock)) / seqblock->t.stretch,
                   seqblock->t.default_duration
                   );
    
    value = R_BOUNDARIES(_min_value, value, _max_value);
    
    int ret = _automation.add_node(create_node(time, value, logtype));
    
    SEQTRACK_update(_seqtrack);

    return ret;
  }

  void delete_node(int nodenum){
    R_ASSERT_RETURN_IF_FALSE(islegalnodenum(nodenum));
    
    if (_automation.size()<=2){
      R_ASSERT_NON_RELEASE(false);
      return;
    }
    
    _automation.delete_node(nodenum);
    
    SEQTRACK_update(_seqtrack);
  }

  void set_curr_node(int nodenum){
    R_ASSERT_RETURN_IF_FALSE(islegalnodenum(nodenum));
    
    if (_automation.get_curr_nodenum() != nodenum){
      _automation.set_curr_nodenum(nodenum);
      SEQTRACK_update(_seqtrack);
    }
  }

  void cancel_curr_node(void){
    _automation.set_curr_nodenum(-1);
    SEQTRACK_update(_seqtrack);
  }

  void set_node(int nodenum, double seqtime, double value, int logtype){
    R_ASSERT_RETURN_IF_FALSE(islegalnodenum(nodenum));
    
    int size = _automation.size();
    
    const AutomationNode *prev = nodenum==0 ? NULL : &_automation.at(nodenum-1);
    AutomationNode node = _automation.at(nodenum);
    const AutomationNode *next = nodenum==size-1 ? NULL : &_automation.at(nodenum+1);
    
    double mintime = prev==NULL ? 0 : prev->time; //next==NULL ? R_MAX(R_MAX(node.time, seqtime), SONG_get_length()) : prev->time;
    double maxtime = next==NULL ? _seqblock->t.default_duration : next->time;
    
    //printf("     nodenum: %d.  mintime: %f, seqtime: %f, maxtime: %f. next: %p\n",nodenum,mintime,(seqtime - get_seqblock_noninterior_start(seqblock)),maxtime,NULL);
    double time = R_BOUNDARIES(mintime,
                               (seqtime - get_seqblock_noninterior_start(_seqblock)) / _seqblock->t.stretch,
                               maxtime
                               );

    value = R_BOUNDARIES(_min_value, value, _max_value);
    
    node.time = time;
    node.value = value;
    node.logtype = logtype;
    
    _automation.replace_node(nodenum, node);
    
    SEQTRACK_update(_seqtrack);
  }

  static float get_node_x2(const SeqblockAutomation *seqblockenvelope, const AutomationNode &node, double start_time, double end_time, float x1, float x2) {
    //int64_t abstime = get_abstime_from_seqtime(seqtrack, NULL, node.time);
    
    //printf("      GET_NODE_X2 returned %f - %f. start_time: %f, end_time: %f\n",abstime/48000.0, scale(abstime, start_time, end_time, x1, x2), start_time/48000.0, end_time/48000.0);  
    double duration = seqblockenvelope->_seqblock->t.default_duration;
    return scale(node.time, 0, duration, x1, x2);
  }

  static float get_node_x_callback(const AutomationNode &node, double start_time, double end_time, float x1, float x2, void *data){ 
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
    
    const AutomationNode &node = _automation.at(nodenum);
    
    return SeqblockAutomation::get_node_x2(this, node, start_time, end_time, x1, x2);
  }

  static float get_node_y_callback(const AutomationNode &node, float y1, float y2, void *data){
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

  void paint(QPainter *p, float x1, float y1, float x2, float y2, bool paint_nodes, float seqblock_x1, float seqblock_x2) {
    
    //_automation.set_do_paint_nodes(paint_nodes);
    
    QColor fill_color;
    if (_sat==SAT_VOLUME)
      fill_color = QColor(80,20,200,50);

    QColor color = get_qcolor((enum ColorNums)(AUTOMATION1_COLOR_NUM + _sat));

    _automation.paint(p, x1, y1, x2, y2, 0, 1, color, SeqblockAutomation::get_node_y_callback, SeqblockAutomation::get_node_x_callback, this, fill_color, seqblock_x1, seqblock_x2);
  }
};



struct SeqblockAutomation *SEQBLOCK_AUTOMATION_create(struct SeqTrack *seqtrack, struct SeqBlock *seqblock, enum Seqblock_Automation_Type sat, const dyn_t automation_state, double state_samplerate){
  return new SeqblockAutomation(seqtrack, seqblock, sat, automation_state, state_samplerate);
}

void SEQBLOCK_AUTOMATION_free(struct SeqblockAutomation *seqblockenvelope){  // note: seqblockenvelope might be NULL.
  delete seqblockenvelope;
}

double SEQBLOCK_AUTOMATION_get_min_value(struct SeqblockAutomation *seqblockenvelope){
  return seqblockenvelope->_min_value;
}

double SEQBLOCK_AUTOMATION_get_default_value(struct SeqblockAutomation *seqblockenvelope){
  return seqblockenvelope->_default_value;
}

double SEQBLOCK_AUTOMATION_get_max_value(struct SeqblockAutomation *seqblockenvelope){
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
  SEQBLOCK_AUTOMATION_cancel_curr_automation();
  
  if (seqblockenvelope->_automation.do_paint_nodes()){
    
    R_ASSERT_NON_RELEASE(seqtrack==g_curr_seqblock_automation_seqtrack);
    R_ASSERT_NON_RELEASE(seqblock==g_curr_seqblock_automation_seqblock);
    
  } else {

    //printf("SET DO PAINT NODES\n");
    seqblockenvelope->_automation.set_do_paint_nodes(true);
    SEQTRACK_update(seqtrack);
    
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

    int num_automations = SEQBLOCK_num_automations(seqblock);
    for(int i=0;i<num_automations;i++){
      SeqblockAutomation *seqblockenvelope = seqblock->automations[i];
      seqblockenvelope->_automation.set_do_paint_nodes(false);
    }
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

      if (seqblock==NULL) {

        R_ASSERT(false);

      } else {

        if (seqblock->curr_gain_changed_this_block){

          double seqblock_automation_volume = seqblock->curr_gain;
          
          RT_PATCH_change_velocity(linked_note->seqtrack,
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

static void RT_set_seqblock_volume_envelope_values(struct SeqTrack *seqtrack, struct SeqBlock *seqblock, const double pos, const int64_t seqblock_pos_start, const int64_t seqblock_pos_end){
  struct SeqblockAutomation *seqblockenvelope = seqblock->automations[SAT_VOLUME];

  double new_db = 0.0;
  if (seqblockenvelope->_is_enabled)
    seqblockenvelope->_automation.RT_get_value(pos, new_db, NULL, true);

#if DO_DEBUG
  seqblockenvelope->_automation.print();
  printf("new_db: %f. Pos: %f\n\n", new_db, pos);
#endif

  // Add envelope gain.
  //
  if (fabs(new_db-seqblock->envelope_db) > 0.0001){

    //printf("new db: %f. Old: %f (old gain: %f)\n", new_db, seqblock->envelope_db, seqblock->curr_gain);

    seqblock->curr_gain_changed_this_block = true;
    seqblock->envelope_db = new_db;

    if (new_db==0.0)
      seqblock->curr_gain = 1.0;
    else
      seqblock->curr_gain = db2gain(new_db);

  } else {

    seqblock->curr_gain_changed_this_block = false;

    //printf("new db: xxx\n");
        
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

static void RT_set_seqblock_volume_automation_values(struct SeqTrack *seqtrack){
  R_ASSERT_NON_RELEASE(is_playing_song());

  const int64_t start_time = seqtrack->start_time;
  const int64_t end_time = seqtrack->end_time;

  VECTOR_FOR_EACH(struct SeqBlock *, seqblock, &seqtrack->seqblocks){

    if (end_time >= seqblock->t.time && start_time <= seqblock->t.time2){

      int64_t seqblock_pos_start = start_time - seqblock->t.time;
      int64_t seqblock_pos_end = end_time - seqblock->t.time;

      double s1 = get_seqblock_noninterior_start(seqblock);
      double time = (start_time-s1) / seqblock->t.stretch;

      RT_set_seqblock_volume_envelope_values(seqtrack, seqblock, time, seqblock_pos_start, seqblock_pos_end);
    }

  }END_VECTOR_FOR_EACH;
}

bool RT_seqblock_automation_is_enabled(struct SeqblockAutomation *automation){
  return automation->_is_enabled;
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
      
  SEQUENCER_update(SEQUPDATE_TIME);
}

bool RT_maybe_get_seqblock_automation_value(struct SeqblockAutomation *automation, double time, double &value){
  if (automation->_is_enabled==false)
    return false;
  
  automation->_automation.RT_get_value(time, value, NULL, true);

  if (automation->_sat==SAT_STRETCH){
    double value1 = SeqblockAutomation::get_stretch_from_automation(value);
    value = value1 * automation->_seqblock->stretch_automation_compensation;

    //printf("automation speed: %f / %f. compensation: %f. Time: %f\n", value1, value, automation->_seqblock->stretch_automation_compensation, time);

  } else if (automation->_sat==SAT_SPEED)
    value = SeqblockAutomation::get_speed_from_automation(value) * automation->_seqblock->speed_automation_compensation;

  return true;
}

void SEQBLOCK_calculate_time_conversion_table(struct SeqBlock *seqblock, bool seqblock_is_live){
  if (seqblock->block!=NULL)
    return;

  SeqblockAutomation::calculate_time_conversion_table(seqblock);
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

  RT_set_seqblock_volume_automation_values(seqtrack);
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

      seqblock->envelope_db = MIN_DB - 1; // <-- To ensure (fabs(new_db-seqblock->envelope_db) > 0.0001) is true in RT_set_seqblock_volume_automation_values.
      
      //seqblock->curr_gain = -1; // WARNING: When enabling the Seqtrack_plugin, ensure that we don't use seqblock->curr_gain when player is stopped. If we do, it could be hard to hear since the only thing we do is to invert the phase, at least when there's no envelope volume.

    }END_VECTOR_FOR_EACH;

  }END_ALL_SEQTRACKS_FOR_EACH;
#endif
}



// only used for undo/redo
dyn_t SEQBLOCK_AUTOMATION_get_state(const struct SeqblockAutomation *seqblockenvelope){
  return seqblockenvelope->get_state();
}

// only used for undo/redo
void SEQBLOCK_AUTOMATION_apply_state(struct SeqblockAutomation *seqblockenvelope, const dyn_t envelope_state, double state_samplerate){
  R_ASSERT(state_samplerate != 0);
  seqblockenvelope->create_from_state(envelope_state, state_samplerate);
  SEQTRACK_update(seqblockenvelope->_seqtrack);
}

float SEQBLOCK_AUTOMATION_get_node_x(const struct SeqblockAutomation *seqblockenvelope, int nodenum){
  return seqblockenvelope->get_node_x(nodenum);
}

float SEQBLOCK_AUTOMATION_get_node_y(const struct SeqblockAutomation *seqblockenvelope, int seqtracknum, int nodenum){
  return seqblockenvelope->get_node_y(seqtracknum, nodenum);
}

void SEQBLOCK_AUTOMATION_paint(QPainter *p, struct SeqblockAutomation *seqblockenvelope, float x1, float y1, float x2, float y2, bool paint_nodes, float seqblock_x1, float seqblock_x2){
  seqblockenvelope->paint(p, x1, y1, x2, y2, paint_nodes, seqblock_x1, seqblock_x2);
}

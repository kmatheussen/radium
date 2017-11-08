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


#include <QHash>
#include <QDateTime>

#include <vector>


#include "../common/nsmtracker.h"
#include "../common/instruments_proc.h"
#include "../common/OS_visual_input.h"
#include "../common/patch_proc.h"
#include "../common/scheduler_proc.h"

#include "../midi/midi_fx_proc.h"

#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"
#include "SoundPluginRegistry_proc.h"

#include "../mixergui/undo_chip_position_proc.h"
#include "../mixergui/undo_mixer_connections_proc.h"

#include "../api/api_proc.h"


#include "Modulator_plugin_proc.h"

#define M_PI2 (2.0*M_PI)

#define MODULATOR_NAME "Modulator"

enum{
  EFF_ON_OFF,
  EFF_TYPE,

  EFF_MULTIPLIER_NUMERATOR,
  EFF_MULTIPLIER_DENOMINATOR,
  EFF_PHASE_SHIFT,
  
  EFF_MIN,
  EFF_MAX,
  
  EFF_NUM_EFFECTS
};

#define NUM_TYPES 5

#define MAX_MULTIPLIER_NUMERATOR 32
#define MAX_MULTIPLIER_DENOMINATOR 32
                                                         

static double hz_to_radians(double hz){
  return hz*M_PI2*(double)RADIUM_BLOCKSIZE/(double)pc->pfreq; // rate = pc->pfreq / RADIUM_BLOCKSIZE;
}

namespace{

struct ModulatorTarget{
  const struct Patch *patch;
  int effect_num;

  ModulatorTarget(const struct Patch *patch, int effect_num)
    : patch(patch)
    , effect_num(effect_num)
  {}
};


struct GeneratorParameters{
  double is_enabled = true;
  
  double min = 0.0;
  double max = 1.0;

  //bool follows_tempo = true;
  double tempo_numerator = 1.0; // these two are integer values, but stored in doubles.
  double tempo_denominator = 2.0;
  //double tempo_multiplier = 0.5;
  //double hz = 50; // Used if follows_tempo==false

  double phase_shift = 0.0;

  // repeat
  // send out midi in.
};


struct Generator{
  const char * const _name = NULL;

  Generator(const char *name)
    :_name(name)
  {}
  
  virtual void RT_pre_process(const GeneratorParameters &parms){
  }
  virtual void RT_process(const struct Patch *patch, int effect_num, const GeneratorParameters &parms) = 0;
  virtual void RT_post_process(const GeneratorParameters &parms){
  }

  virtual ~Generator() = default; // Crazy c++ stuff.
};


struct OscillatorGenerator : public Generator {

  double _phase = 0.0;
  double _phase_add = 0.002;
  float _curr_value;

  OscillatorGenerator(const char *name)
    : Generator(name)
  {}
  
  virtual double oscillator(double phase) = 0;
  
  void RT_pre_process(const GeneratorParameters &parms) override {

    double beatpos;

    const struct SeqTrack *seqtrack;
    
    if (pc->playtype==PLAYBLOCK)
      seqtrack = root->song->block_seqtrack;
    else
      seqtrack = (struct SeqTrack *)root->song->seqtracks.elements[0]; // FIX.

    if (is_really_playing()) {

      beatpos = RT_LPB_get_beat_position(seqtrack);
      
      _phase = parms.tempo_numerator*beatpos*M_PI2/parms.tempo_denominator + parms.phase_shift*M_PI2;

    }else{

      // note: it makes very little sense using the parms.phase_shift value here.
      
      double bpm = RT_LPB_get_current_BPM(seqtrack);
      double hz = bpm * parms.tempo_numerator / (60.0*parms.tempo_denominator);
      _phase_add = hz_to_radians(hz);
      
    }

    _curr_value = R_BOUNDARIES(parms.min,
                               scale_double(oscillator(_phase),
                                            -1.0f, 1.0f,
                                            parms.min, parms.max),
                               parms.max);

    //printf("_curr_value: %f - %f. %*f\n", _phase, beatpos, 10 + (int)scale(_curr_value, 0, 1, 0, 150), _curr_value);//_curr_value);

    _phase += _phase_add;
  }

  void RT_process(const struct Patch *patch, int effect_num, const GeneratorParameters &parms) override {
    // Should we check if patch has been closed here?
    // Probably not. patch->plugin is probably set to NULL before the plugin is deleted. Yes, looks so in AUDIO_remove_patchdata.

    void *patchdata = patch->patchdata;

    // This check should be good enough.
    if (patchdata==NULL){
#if !defined(RELEASE)
      printf("1111. Note: patch->patchdata==NULL for %s (this should be perfectly legal in brief moments while a patch is deleted)\n", patch->name);
#endif
      return;
    }

    if(patch->instrument==get_audio_instrument()){

      SoundPlugin *plugin = static_cast<SoundPlugin*>(patchdata);
      PLUGIN_set_effect_value(plugin, 0, effect_num, _curr_value, DONT_STORE_VALUE, FX_middle, EFFECT_FORMAT_SCALED);

    } else {

      MIDI_set_effect_value(patch, 0, effect_num, _curr_value);

    }
  }
};


struct SinewaveGenerator : public OscillatorGenerator {

  SinewaveGenerator()
    : OscillatorGenerator("Sine wave")
  {}

  double oscillator(double phase) override {
    //return sin(phase - M_PI/2.0); // nah
    return sin(phase);
  }
};
 

struct TriangleGenerator : public OscillatorGenerator {

  TriangleGenerator()
    : OscillatorGenerator("Triangle")
  {}

  double oscillator(double phase) override {
    
    phase = fmod(phase, M_PI2);
        
    if (phase < M_PI2/4.0)
      return scale_double(phase, 0, M_PI2/4.0, 0, 1);
    else if (phase < M_PI2*3.0/4.0)
      return scale_double(phase, M_PI2/4.0, M_PI2*3.0/4.0, 1, -1);
    else
      return scale_double(phase, M_PI2*3.0/4.0, M_PI2, -1, 0);
  }
};
 

struct SquareGenerator : public OscillatorGenerator {

  SquareGenerator()
    : OscillatorGenerator("Square")
  {}

  double oscillator(double phase) override {
    phase = fmod(phase, M_PI2);
  
    if (phase < M_PI2/4.0)
      return 1;
    else if (phase < M_PI2*3.0/4.0)
      return -1;
    else
      return 1;
  }
};
 

struct SawGenerator : public OscillatorGenerator {

  SawGenerator()
    : OscillatorGenerator("Saw")
  {}

  double oscillator(double phase) override {
    phase = fmod(phase, M_PI2);
  
    if (phase < M_PI)
      return scale_double(phase, 0, M_PI, 0, 1);
    else
      return scale_double(phase, M_PI, M_PI2, -1, 0);
  }
};
 

struct InvertedSawGenerator : public OscillatorGenerator {

  InvertedSawGenerator()
    : OscillatorGenerator("Inverted Saw")
  {}

  double oscillator(double phase) override {
    phase = fmod(phase, M_PI2);
  
    if (phase < M_PI)
      return scale_double(phase, 0, M_PI, 0, -1);
    else
      return scale_double(phase, M_PI, M_PI2, 1, 0);
  }
};
 


static int64_t g_id = 0;

class Modulator{
  
public:
  
  int64_t _id = g_id++;

  int64_t _creation_time;

private:
  
  radium::Vector<const ModulatorTarget*> *_targets = new radium::Vector<const ModulatorTarget*>;

  SinewaveGenerator _sinewave_generator;
  TriangleGenerator _triangle_generator;
  SquareGenerator _square_generator;
  SawGenerator _saw_generator;
  InvertedSawGenerator _inverted_saw_generator;
  
  
public:

  GeneratorParameters _parms;

  Generator *_generator = &_sinewave_generator;

  struct SoundPlugin *_plugin;

  Modulator(struct SoundPlugin *plugin, hash_t *state = NULL)
    : _creation_time(state==NULL ? QDateTime::currentMSecsSinceEpoch() : HASH_get_int(state, "creation_time"))
    , _plugin(plugin)
  {
  }

  ~Modulator(){
    for(auto *target : *_targets)
      delete target;
  }

  hash_t *get_state(void) const {
    hash_t *state = HASH_create(4);

    {
      const volatile struct Patch *modulator_patch = _plugin->patch;
      R_ASSERT_RETURN_IF_FALSE2(modulator_patch != NULL, state);
      HASH_put_int(state, "modulator_patch_id", modulator_patch->id);
    }

    int i = 0;
    for(auto *target : *_targets){
      HASH_put_int_at(state, "target_patch_id", i, target->patch->id);
      HASH_put_int_at(state, "target_effect_num", i, target->effect_num);
      i++;
    }

    return state;
  }

  void apply_state(hash_t *state){
    // Assert that the state is for this modulator.
    {
      const volatile struct Patch *modulator_patch = _plugin->patch;
      R_ASSERT_RETURN_IF_FALSE(modulator_patch != NULL);
      
      int64_t patch_id = HASH_get_int(state, "modulator_patch_id");
      R_ASSERT_RETURN_IF_FALSE(patch_id==modulator_patch->id);
    }

    radium::Vector<const ModulatorTarget*> *new_targets = new radium::Vector<const ModulatorTarget*>;

    int num_new_targets = HASH_get_array_size(state, "target_patch_id");
    for(int i=0 ; i<num_new_targets ; i++){
      int64_t target_patch_id = HASH_get_int_at(state, "target_patch_id", i);
      const struct Patch *patch = PATCH_get_from_id(target_patch_id);
      R_ASSERT_RETURN_IF_FALSE(patch!=NULL);

      int effect_num = HASH_get_int32_at(state, "target_effect_num", i);

      auto *new_target = new ModulatorTarget(patch, effect_num);
      new_targets->push_back(new_target);
    }    

    auto *old_targets = _targets;

    // TODO: Check if new target and old target contains the same.
    PLAYER_lock();{
      _targets = new_targets;
    }PLAYER_unlock();

    for(auto *target : *old_targets)
      delete target;

    delete old_targets;
  }


  int _type = 0;
  void set_type(int type){
    R_ASSERT_RETURN_IF_FALSE(type>=0 && type < NUM_TYPES);
    _type = type;
    if (type==0)
      _generator = &_sinewave_generator;
    else if (type==1)
      _generator = &_triangle_generator;
    else if (type==2)
      _generator = &_square_generator;
    else if (type==3)
      _generator = &_saw_generator;
    else if (type==4)
      _generator = &_inverted_saw_generator;
  }

  int get_type(void) const {
    return _type;
  }
  
  const char *get_type_name(void) const {
    return _generator->_name;
  }
  
  bool has_target(const struct Patch *patch, int effect_num) const {
    for(auto *target : *_targets)
      if (target->patch==patch && target->effect_num==effect_num)
        return true;

    return false;
  }
  
  void add_target(const struct Patch *patch, int effect_num){
    R_ASSERT_RETURN_IF_FALSE(has_target(patch,effect_num)==false);
    
    auto *target = new ModulatorTarget(patch, effect_num);

    _targets->ensure_there_is_room_for_more_without_having_to_allocate_memory(1);

    PLAYER_lock();{
      _targets->push_back(target);
    }PLAYER_unlock();

    _targets->post_add();
  }

  void remove_target(const struct Patch *patch, int effect_num){
    const ModulatorTarget *target = NULL;
    
    for(auto *maybetarget : *_targets)
      if (maybetarget->patch==patch && maybetarget->effect_num==effect_num){
        target = maybetarget;
        break;
      }

    R_ASSERT_RETURN_IF_FALSE(target!=NULL);
    
    ADD_UNDO(MixerConnections_CurrPos());

    PLAYER_lock();{
      _targets->remove(target);
    }PLAYER_unlock();
  }

  void call_me_when_a_patch_is_made_inactive(const struct Patch *patch, radium::PlayerLockOnlyIfNeeded &lock){
  again:
    for(auto *maybetarget : *_targets)
      if (maybetarget->patch==patch){
        lock.maybe_pause((int)_id);
        _targets->remove(maybetarget);
        goto again;
      }
  }

  void RT_process(void){

    if (_parms.is_enabled==false)
      return;

    _generator->RT_pre_process(_parms);

    for(auto *target : *_targets){
      _generator->RT_process(target->patch, target->effect_num, _parms);
    }

    _generator->RT_post_process(_parms);
  }
};

} // end anon. namespace

static bool sort_modulators_by_creation_time(const Modulator *a, const Modulator *b){
  return a->_creation_time < b->_creation_time;
}

static QHash<int64_t, Modulator*> g_modulators;
static radium::Vector<Modulator*> g_modulators2;

// Called from the main audio thread
void RT_MODULATOR_process(void){
  for(auto *modulator : g_modulators2)
    modulator->RT_process();
}

int64_t MODULATOR_get_id(const struct Patch *patch, int effect_num){
  for(int64_t id : g_modulators.keys()){
    auto *modulator = g_modulators[id];
    if(modulator->has_target(patch, effect_num))
      return modulator->_id;
  }

  return -1;
}

int64_t MODULATOR_get_id_from_modulator_patch(const struct Patch *patch){
  for(int64_t id : g_modulators.keys()){
    auto *modulator = g_modulators[id];
    const volatile struct Patch *modulator_patch = modulator->_plugin->patch;
    R_ASSERT(modulator_patch != NULL);
    if (modulator_patch == patch)
      return modulator->_id;
  }

  return -1;
}
  
struct Patch *MODULATOR_get_modulator_patch(const struct Patch *patch, int effect_num){
  for(const auto *modulator : g_modulators2){
    if (effect_num==EFFNUM_INPUT_VOLUME){
      printf("       FOUND target? %s. Found it: %d\n", patch->name, modulator->has_target(patch, effect_num));
    }
    if(modulator->has_target(patch, effect_num)){      
      volatile struct Patch *modulator_patch = modulator->_plugin->patch;
      R_ASSERT(modulator_patch != NULL);
      return (struct Patch*)modulator_patch;
    }
  }

  return NULL;
}


void MODULATOR_add_target(int64_t modulator_id, const struct Patch *patch, int effect_num){
  R_ASSERT_RETURN_IF_FALSE(true==g_modulators.contains(modulator_id));

  R_ASSERT_RETURN_IF_FALSE(MODULATOR_get_id(patch, effect_num)==-1);

  g_modulators[modulator_id]->add_target(patch, effect_num);
}


void MODULATOR_maybe_create_and_add_target(const struct Patch *patch, int effect_num, bool do_replace){
  radium::ScopedUndo scoped_undo;

  int64_t old_modulator_id = MODULATOR_get_id(patch, effect_num);

  if(do_replace)
    R_ASSERT(old_modulator_id >= 0);
  else
    R_ASSERT(old_modulator_id==-1);

  vector_t v = {0};

  int create_new = VECTOR_push_back(&v, "Create new envelope modulator");
  VECTOR_push_back(&v, "--------------");

  auto modulators = g_modulators2.to_std_vector(); // Make a copy so we don't have to lock player while sorting.
  std::sort(modulators.begin(), modulators.end(), sort_modulators_by_creation_time);

  for(auto *modulator : modulators){
    volatile struct Patch *patch = modulator->_plugin->patch;
    R_ASSERT(patch!=NULL);
    VECTOR_push_back(&v, talloc_format("%s: %s", patch==NULL ? "" : patch->name, MODULATOR_get_description(modulator->_id)));
  }

  int64_t new_modulator_id;

  int command = GFX_Menu(root->song->tracker_windows, NULL, "", v, true);
  if (command < 0)
    return;


  if (command==create_new){

    struct Patch *curr_patch = g_currpatch;

    int64_t instrument_id = createAudioInstrument(MODULATOR_NAME, MODULATOR_NAME, "", 0, 0);
    if (instrument_id==-1)
      return;

    if (curr_patch != NULL)
      GFX_PP_Update(curr_patch, false); // Set back current instrument.

    const struct Patch *modulator_patch = PATCH_get_from_id(instrument_id);

    ADD_UNDO(ChipPos_CurrPos(modulator_patch));
    autopositionInstrument(instrument_id);

    SoundPlugin *plugin = static_cast<SoundPlugin*>(modulator_patch->patchdata);
    if(plugin==NULL){        
      R_ASSERT_NON_RELEASE(false);
      return;        
    }

    Modulator *modulator = static_cast<Modulator*>(plugin->data);
    
    new_modulator_id = modulator->_id;

  } else {

    new_modulator_id = modulators[command-2]->_id;

  }

  if(old_modulator_id >= 0)
    MODULATOR_remove_target(old_modulator_id, patch, effect_num);
  else
    ADD_UNDO(MixerConnections_CurrPos());

  MODULATOR_add_target(new_modulator_id, patch, effect_num);
}

void MODULATOR_remove_target(int modulator_id, const struct Patch *patch, int effect_num){
  R_ASSERT_RETURN_IF_FALSE(g_modulators.contains(modulator_id));  
  g_modulators[modulator_id]->remove_target(patch, effect_num);
}

void MODULATOR_call_me_when_a_patch_is_made_inactive(const struct Patch *patch){
  radium::PlayerLockOnlyIfNeeded lock;

  for(auto *modulator : g_modulators2)
    modulator->call_me_when_a_patch_is_made_inactive(patch, lock);
}

int64_t *MODULATOR_get_ids(int *num_modulators){

  const auto &keys = g_modulators.keys();
  *num_modulators = keys.size();

  int64_t *ids = (int64_t*)talloc(sizeof(int64_t)*keys.size());

  int i=keys.size()-1;
  for(int64_t id : g_modulators.keys())
    ids[i--] = id;

  return ids;
}

const char *MODULATOR_get_description(int64_t modulator_id){
  R_ASSERT_RETURN_IF_FALSE2(g_modulators.contains(modulator_id), "");
  auto *modulator = g_modulators[modulator_id];
  return modulator->_generator->_name;
}

hash_t *MODULATOR_get_connections_state(void){
  hash_t *state = HASH_create(g_modulators2.size());

  int i = 0;
  for(auto *modulator : g_modulators2){
    HASH_put_hash_at(state, "modulator", i, modulator->get_state());
    i++;
  }

  return state;
}

static Modulator *get_modulator_from_patch_id(int64_t patch_id){
  for(auto *modulator : g_modulators2){
    const volatile struct Patch *modulator_patch = modulator->_plugin->patch;
    if (modulator_patch==NULL)
      R_ASSERT(false);
    else if (modulator_patch->id==patch_id)
      return modulator;
  }  
  return NULL;
}

void MODULATOR_apply_connections_state(hash_t *state){
  int size = HASH_get_array_size(state, "modulator");
  for(int i=0 ; i<size ; i++){
    hash_t *modulator_state = HASH_get_hash_at(state, "modulator", i);
    if(modulator_state==NULL) return; // assertion was thrown in hashmap.c

    int64_t patch_id = HASH_get_int(modulator_state, "modulator_patch_id");
    Modulator *modulator = get_modulator_from_patch_id(patch_id);
    R_ASSERT_RETURN_IF_FALSE(modulator!=NULL);

    modulator->apply_state(modulator_state);
  }
}

static void RT_process(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs){
  //Modulator *modulator = static_cast<Modulator*>(plugin->data);

  // no need to do anything. no inputs and no outputs.
}


static void set_effect_value(struct SoundPlugin *plugin, int time, int effect_num, float value, enum ValueFormat value_format, FX_when when){
  Modulator *modulator = static_cast<Modulator*>(plugin->data);

  if(value_format==EFFECT_FORMAT_SCALED){
    
    switch(effect_num){
    
    case EFF_TYPE:
      value = scale(value, 0, 1, 0, double(NUM_TYPES)-0.001);
      break;

    
    case EFF_MULTIPLIER_NUMERATOR:
      value = scale(value, 0, 1, 1, MAX_MULTIPLIER_NUMERATOR+0.99);
      break;

    case EFF_MULTIPLIER_DENOMINATOR:
      value = scale(value, 0, 1, 1, MAX_MULTIPLIER_DENOMINATOR+0.99);
      break;
    }
  }

  switch(effect_num){

  case EFF_ON_OFF:
    modulator->_parms.is_enabled = value >= 0.5;
    break;
    
  case EFF_TYPE:
    modulator->set_type(value);
    break;
    
    
  case EFF_MULTIPLIER_NUMERATOR:
    modulator->_parms.tempo_numerator = floor(value);
    break;
    
  case EFF_MULTIPLIER_DENOMINATOR:
    modulator->_parms.tempo_denominator = floor(value);
    break;

    
  case EFF_PHASE_SHIFT:
    modulator->_parms.phase_shift = value;
    break;
    
  case EFF_MIN:
    modulator->_parms.min = R_BOUNDARIES(0, value, modulator->_parms.max);
    break;
    
  case EFF_MAX:
    modulator->_parms.max = R_BOUNDARIES(modulator->_parms.min, value, 1);
    break;

  default:
    RError("Unknown effect number %d. Value: %f\n",effect_num, value);
  }
  
}

static float get_effect_value(struct SoundPlugin *plugin, int effect_num, enum ValueFormat value_format){
  const Modulator *modulator = static_cast<Modulator*>(plugin->data);

  float value;

  switch(effect_num){

  case EFF_ON_OFF:
    value = modulator->_parms.is_enabled;
    break;
    
  case EFF_TYPE:
    value = modulator->get_type();
    break;
    
    
  case EFF_MULTIPLIER_NUMERATOR:
    value = modulator->_parms.tempo_numerator;
    break;
    
  case EFF_MULTIPLIER_DENOMINATOR:
    value = modulator->_parms.tempo_denominator;
    break;

    
  case EFF_PHASE_SHIFT:
    value = modulator->_parms.phase_shift;
    break;
    
  case EFF_MIN:
    value = modulator->_parms.min;
    break;
    
  case EFF_MAX:
    value = modulator->_parms.max;
    break;

  default:
    RError("Unknown effect number %d",effect_num);
    return 0.0f;
  }

  if(value_format==EFFECT_FORMAT_SCALED){
    
    switch(effect_num){
    
    case EFF_TYPE:
      return scale(value, 0, double(NUM_TYPES)-0.001, 0, 1);

    
    case EFF_MULTIPLIER_NUMERATOR:
      return scale(value, 1, MAX_MULTIPLIER_NUMERATOR+0.99, 0, 1);

    case EFF_MULTIPLIER_DENOMINATOR:
      return scale(value, 1, MAX_MULTIPLIER_DENOMINATOR+0.99, 0, 1);

    }
  }


  return value;
}

static void get_display_value_string(SoundPlugin *plugin, int effect_num, char *buffer, int buffersize){
  const Modulator *modulator = static_cast<Modulator*>(plugin->data);

  switch(effect_num){

  case EFF_ON_OFF:
    snprintf(buffer,buffersize-1,"%s",modulator->_parms.is_enabled ? "ON" : "OFF");
    break;
    
  case EFF_TYPE:
    snprintf(buffer, buffersize-1, "%s", modulator->get_type_name());
    break;
    
    
  case EFF_MULTIPLIER_NUMERATOR:
    snprintf(buffer, buffersize-1, "%d", (int)modulator->_parms.tempo_numerator);
    break;
    
  case EFF_MULTIPLIER_DENOMINATOR:
    snprintf(buffer, buffersize-1, "%d", (int)modulator->_parms.tempo_denominator);
    break;

    
  case EFF_PHASE_SHIFT:
    snprintf(buffer, buffersize-1, "%f", modulator->_parms.phase_shift);
    break;
    
  case EFF_MIN:
    snprintf(buffer, buffersize-1, "%f", modulator->_parms.min);
    break;
    
  case EFF_MAX:
    snprintf(buffer, buffersize-1, "%f", modulator->_parms.max);
    break;

  default:
    RError("Unknown effect number %d",effect_num);
    return;
  }

}

static int get_effect_format(struct SoundPlugin *plugin, int effect_num){
  switch(effect_num){

  case EFF_ON_OFF:
    return EFFECT_FORMAT_BOOL;
    break;
    
  case EFF_TYPE:
    return EFFECT_FORMAT_INT;
    
  case EFF_MULTIPLIER_NUMERATOR:
    return EFFECT_FORMAT_INT;
    
  case EFF_MULTIPLIER_DENOMINATOR:
    return EFFECT_FORMAT_INT;

    
  case EFF_PHASE_SHIFT:
    return EFFECT_FORMAT_FLOAT;
    
  case EFF_MIN:
    return EFFECT_FORMAT_FLOAT;
    
  case EFF_MAX:
    return EFFECT_FORMAT_FLOAT;
    
  default:
    RError("Unknown effect number %d",effect_num);
    return EFFECT_FORMAT_FLOAT;
  }
}

static const char *get_effect_name(struct SoundPlugin *plugin, int effect_num){
  
  switch(effect_num){

  case EFF_ON_OFF:
    return "Enabled";
    break;
    
  case EFF_TYPE:
    return "Type";
    
  case EFF_MULTIPLIER_NUMERATOR:
    return "Tempo multiplier";
    
  case EFF_MULTIPLIER_DENOMINATOR:
    return "Tempo divisor";

    
  case EFF_PHASE_SHIFT:
    return "Phase shift";
    
  case EFF_MIN:
    return "Minimum value";
    
  case EFF_MAX:
    return "Maximum value";

  default:
    RError("Unknown effect number %d",effect_num);
    return "(Error)";
  }
}

static const char *get_effect_description(struct SoundPlugin *plugin, int effect_num){
  
  switch(effect_num){
  case EFF_MULTIPLIER_NUMERATOR:
    return "Tip: Try to assign this Envelope Generator (to itself, that is)";
    
  case EFF_MULTIPLIER_DENOMINATOR:
    return "Tip: Try to assign this Envelope Generator (to itself, that is)";
  }

  return "";
}

static void *create_plugin_data(const SoundPluginType *plugin_type, SoundPlugin *plugin, hash_t *state, float sample_rate, int block_size, bool is_loading){
  Modulator *modulator = new Modulator(plugin, state);
  g_modulators[modulator->_id] = modulator;

  g_modulators2.ensure_there_is_room_for_more_without_having_to_allocate_memory(1);
  PLAYER_lock();{
    g_modulators2.push_back(modulator);
  }PLAYER_unlock();
  g_modulators2.post_add();

  return modulator;
}

static void cleanup_plugin_data(SoundPlugin *plugin){
  Modulator *modulator = static_cast<Modulator*>(plugin->data);

  printf("\n\n  Size before: %d\n", g_modulators2.size());
  PLAYER_lock();{
    g_modulators2.remove(modulator);
  }PLAYER_unlock();
  printf("  Size after: %d\n\n\n", g_modulators2.size());

  
  g_modulators.remove(modulator->_id);

  delete modulator;
}

static void create_state(struct SoundPlugin *plugin, hash_t *state){
  R_ASSERT_RETURN_IF_FALSE(state!=NULL);

  Modulator *modulator = static_cast<Modulator*>(plugin->data);

  HASH_put_int(state, "creation_time", modulator->_creation_time);
}

static int RT_get_audio_tail_length(struct SoundPlugin *plugin){
  return 0;
}


void create_modulator_plugin(void){
  SoundPluginType *plugin_type = (SoundPluginType*)V_calloc(1,sizeof(SoundPluginType));

  plugin_type->type_name                = MODULATOR_NAME;
  plugin_type->name                     = MODULATOR_NAME;
  plugin_type->num_inputs               = 0;
  plugin_type->num_outputs              = 0;
  plugin_type->is_instrument            = false;
  plugin_type->note_handling_is_RT      = false;
  plugin_type->num_effects              = EFF_NUM_EFFECTS;
  plugin_type->get_effect_format        = get_effect_format;
  plugin_type->get_effect_name          = get_effect_name;
  plugin_type->get_effect_description   = get_effect_description;
  plugin_type->effect_is_RT             = NULL;
  plugin_type->create_plugin_data       = create_plugin_data;
  plugin_type->cleanup_plugin_data      = cleanup_plugin_data;
  plugin_type->create_state             = create_state;

  plugin_type->RT_get_audio_tail_length = RT_get_audio_tail_length;
  
  plugin_type->RT_process       = RT_process;
  /*
  plugin_type->play_note        = play_note;
  plugin_type->set_note_volume  = set_note_volume;
  plugin_type->stop_note        = stop_note;
  */
  plugin_type->set_effect_value = set_effect_value;
  plugin_type->get_effect_value = get_effect_value;
  plugin_type->get_display_value_string = get_display_value_string;

  plugin_type->will_never_autosuspend = true; // Must always run.
  
  PR_add_plugin_type(plugin_type);
}

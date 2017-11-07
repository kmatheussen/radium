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


#include "../common/nsmtracker.h"
#include "../common/instruments_proc.h"
#include "../common/OS_visual_input.h"
#include "../common/patch_proc.h"
#include "../common/scheduler_proc.h"

#include "../midi/midi_fx_proc.h"

#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"
#include "SoundPluginRegistry_proc.h"

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
    if(patch->instrument==get_audio_instrument()){
      SoundPlugin *plugin = static_cast<SoundPlugin*>(patch->patchdata);
      if(plugin==NULL){
#if !defined(RELEASE)
        //       printf("Note: plugin==NULL for %s\n", patch->name);
#endif
        return;
      }

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

private:
  
  radium::Vector<const ModulatorTarget*> _targets;

  SinewaveGenerator _sinewave_generator;
  TriangleGenerator _triangle_generator;
  SquareGenerator _square_generator;
  SawGenerator _saw_generator;
  InvertedSawGenerator _inverted_saw_generator;
  
  
public:

  GeneratorParameters _parms;

  Generator *_generator = &_sinewave_generator;

  struct SoundPlugin *_plugin;

  Modulator(struct SoundPlugin *plugin)
    : _plugin(plugin)
  {}

  ~Modulator(){
    for(auto *target : _targets)
      delete target; // TODO: Must tell patch that envelope controller has been removed. For the GUI. Or perhaps the gui should just ask if a patch + effect num combo is connected... That sounds cleaner.
  }

  int _type = 0;
  void set_type(int type){
    R_ASSERT_NON_RELEASE(type>=0 && type < NUM_TYPES);
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
    for(auto *target : _targets)
      if (target->patch==patch && target->effect_num==effect_num)
        return true;

    return false;
  }
  
  void add_target(const struct Patch *patch, int effect_num){
    R_ASSERT_RETURN_IF_FALSE(has_target(patch,effect_num)==false);
    
    auto *target = new ModulatorTarget(patch, effect_num);

    _targets.ensure_there_is_room_for_more_without_having_to_allocate_memory(1);

    PLAYER_lock();{
      _targets.push_back(target);
    }PLAYER_unlock();

    _targets.post_add();
  }

  void remove_target(const struct Patch *patch, int effect_num){
    const ModulatorTarget *target = NULL;
    
    for(auto *maybetarget : _targets)
      if (maybetarget->patch==patch && maybetarget->effect_num==effect_num){
        target = maybetarget;
        break;
      }

    R_ASSERT_RETURN_IF_FALSE(target!=NULL);
        
    PLAYER_lock();{
      _targets.remove(target);
    }PLAYER_unlock();
  }
  
  void RT_process(void){

    if (_parms.is_enabled==false)
      return;

    _generator->RT_pre_process(_parms);

    for(auto *target : _targets){
      _generator->RT_process(target->patch, target->effect_num, _parms);
    }

    _generator->RT_post_process(_parms);
  }
};

} // end anon. namespace


static QHash<int64_t, Modulator*> g_modulators;
static radium::Vector<Modulator*> g_modulators2;

// Called from the main audio thread
void RT_MODULATOR_process(void){
  for(auto *controller : g_modulators2)
    controller->RT_process();
}

int64_t MODULATOR_get_controller_id(const struct Patch *patch, int effect_num){
  for(int64_t id : g_modulators.keys()){
    auto *controller = g_modulators[id];
    if(controller->has_target(patch, effect_num))
      return id;
  }

  return -1;
}
                                             

void MODULATOR_add_target(int64_t controller_id, const struct Patch *patch, int effect_num){
  R_ASSERT_RETURN_IF_FALSE(true==g_modulators.contains(controller_id));

  R_ASSERT_RETURN_IF_FALSE(MODULATOR_get_controller_id(patch, effect_num)==-1);

  g_modulators[controller_id]->add_target(patch, effect_num);
}


void MODULATOR_maybe_create_and_add_target(const struct Patch *patch, int effect_num){
  R_ASSERT_RETURN_IF_FALSE(MODULATOR_get_controller_id(patch, effect_num)==-1);
  
  vector_t v = {0};

  int create_new = VECTOR_push_back(&v, "Create new envelope controller");
  VECTOR_push_back(&v, "--------------");
  for(int64_t id : g_modulators.keys()){
    //auto *controller = g_modulators[id];    
    auto *controller = g_modulators[id];
    volatile struct Patch *patch = controller->_plugin->patch;
    VECTOR_push_back(&v, talloc_format("%d: %s (%s)", (int)id, MODULATOR_get_description(id), patch==NULL ? "" : patch->name));
  }

  int64_t controller_id;

  int command = GFX_Menu(root->song->tracker_windows, NULL, "", v, true);
  if (command < 0)
    return;


  if (command==create_new){

    struct Patch *curr_patch = g_currpatch;

    int64_t instrument_id = createAudioInstrument(MODULATOR_NAME, MODULATOR_NAME, "", 0, 0);
    if (instrument_id==-1)
      return;

    autopositionInstrument(instrument_id);

    if (curr_patch != NULL)
      GFX_PP_Update(curr_patch, false); // Set back current instrument.

    const struct Patch *controller_patch = PATCH_get_from_id(instrument_id);
    SoundPlugin *plugin = static_cast<SoundPlugin*>(controller_patch->patchdata);
    if(plugin==NULL){        
      R_ASSERT_NON_RELEASE(false);
      return;        
      }

    Modulator *controller = static_cast<Modulator*>(plugin->data);
    
    controller_id = controller->_id;

  } else {

    controller_id = g_modulators.keys()[command - 2];

  }

  MODULATOR_add_target(controller_id, patch, effect_num);
}

void MODULATOR_remove_target(int controller_id, const struct Patch *patch, int effect_num){
  R_ASSERT_RETURN_IF_FALSE(g_modulators.contains(controller_id));  
  g_modulators[controller_id]->remove_target(patch, effect_num);
}

int64_t *MODULATOR_get_controller_ids(int *num_controllers){

  const auto &keys = g_modulators.keys();
  *num_controllers = keys.size();

  int64_t *ids = (int64_t*)talloc(sizeof(int64_t)*keys.size());

  int i=0;
  for(int64_t id : g_modulators.keys())
    ids[i++] = id;

  return ids;
}

const char *MODULATOR_get_description(int64_t controller_id){
  R_ASSERT_RETURN_IF_FALSE2(g_modulators.contains(controller_id), "");
  auto *controller = g_modulators[controller_id];
  return controller->_generator->_name;
}

static void RT_process(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs){
  //Modulator *controller = static_cast<Modulator*>(plugin->data);

  // no need to do anything. no inputs and no outputs.
}


static void set_effect_value(struct SoundPlugin *plugin, int time, int effect_num, float value, enum ValueFormat value_format, FX_when when){
  Modulator *controller = static_cast<Modulator*>(plugin->data);

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
    controller->_parms.is_enabled = value >= 0.5;
    break;
    
  case EFF_TYPE:
    controller->set_type(value);
    break;
    
    
  case EFF_MULTIPLIER_NUMERATOR:
    controller->_parms.tempo_numerator = floor(value);
    break;
    
  case EFF_MULTIPLIER_DENOMINATOR:
    controller->_parms.tempo_denominator = floor(value);
    break;

    
  case EFF_PHASE_SHIFT:
    controller->_parms.phase_shift = value;
    break;
    
  case EFF_MIN:
    controller->_parms.min = R_BOUNDARIES(0, value, controller->_parms.max);
    break;
    
  case EFF_MAX:
    controller->_parms.max = R_BOUNDARIES(controller->_parms.min, value, 1);
    break;

  default:
    RError("Unknown effect number %d. Value: %f\n",effect_num, value);
  }
  
}

static float get_effect_value(struct SoundPlugin *plugin, int effect_num, enum ValueFormat value_format){
  const Modulator *controller = static_cast<Modulator*>(plugin->data);

  float value;

  switch(effect_num){

  case EFF_ON_OFF:
    value = controller->_parms.is_enabled;
    break;
    
  case EFF_TYPE:
    value = controller->get_type();
    break;
    
    
  case EFF_MULTIPLIER_NUMERATOR:
    value = controller->_parms.tempo_numerator;
    break;
    
  case EFF_MULTIPLIER_DENOMINATOR:
    value = controller->_parms.tempo_denominator;
    break;

    
  case EFF_PHASE_SHIFT:
    value = controller->_parms.phase_shift;
    break;
    
  case EFF_MIN:
    value = controller->_parms.min;
    break;
    
  case EFF_MAX:
    value = controller->_parms.max;
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
  const Modulator *controller = static_cast<Modulator*>(plugin->data);

  switch(effect_num){

  case EFF_ON_OFF:
    snprintf(buffer,buffersize-1,"%s",controller->_parms.is_enabled ? "ON" : "OFF");
    break;
    
  case EFF_TYPE:
    snprintf(buffer, buffersize-1, "%s", controller->get_type_name());
    break;
    
    
  case EFF_MULTIPLIER_NUMERATOR:
    snprintf(buffer, buffersize-1, "%d", (int)controller->_parms.tempo_numerator);
    break;
    
  case EFF_MULTIPLIER_DENOMINATOR:
    snprintf(buffer, buffersize-1, "%d", (int)controller->_parms.tempo_denominator);
    break;

    
  case EFF_PHASE_SHIFT:
    snprintf(buffer, buffersize-1, "%f", controller->_parms.phase_shift);
    break;
    
  case EFF_MIN:
    snprintf(buffer, buffersize-1, "%f", controller->_parms.min);
    break;
    
  case EFF_MAX:
    snprintf(buffer, buffersize-1, "%f", controller->_parms.max);
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
    return "Tempo multiplier numerator";
    
  case EFF_MULTIPLIER_DENOMINATOR:
    return "Tempo multiplier denominator";

    
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
  Modulator *controller = new Modulator(plugin);
  g_modulators[controller->_id] = controller;

  g_modulators2.ensure_there_is_room_for_more_without_having_to_allocate_memory(1);
  PLAYER_lock();{
    g_modulators2.push_back(controller);
  }PLAYER_unlock();
  g_modulators2.post_add();

  return controller;
}

static void cleanup_plugin_data(SoundPlugin *plugin){
  Modulator *controller = static_cast<Modulator*>(plugin->data);

  printf("\n\n  Size before: %d\n", g_modulators2.size());
  PLAYER_lock();{
    g_modulators2.remove(controller);
  }PLAYER_unlock();
  printf("  Size after: %d\n\n\n", g_modulators2.size());

  
  g_modulators.remove(controller->_id);

  delete controller;
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

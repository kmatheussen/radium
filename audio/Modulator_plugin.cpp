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

#include <math.h>

#include <QHash>
#include <QDateTime>

#include <vector>

#include "../common/nsmtracker.h"
#include "../common/instruments_proc.h"
#include "../common/OS_visual_input.h"
#include "../common/patch_proc.h"
#include "../common/scheduler_proc.h"

#include "../embedded_scheme/s7extra_proc.h"

#include "../midi/midi_fx_proc.h"

#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"
#include "SoundPluginRegistry_proc.h"
#include "SoundProducer_proc.h"
#include "Delay.hpp"

#include "../mixergui/undo_chip_position_proc.h"
#include "../mixergui/undo_mixer_connections_proc.h"

#include "../api/api_proc.h"


#include "Modulator_plugin_proc.h"

#define M_PI2 (2.0*M_PI)

#define MODULATOR_NAME "Modulator"

enum{
  EFF_ENABLED,

  EFF_TYPE,

  EFF_MULTIPLIER_NUMERATOR,
  EFF_MULTIPLIER_DENOMINATOR,
  EFF_PHASE_SHIFT,
  
  EFF_MIN,
  EFF_MAX,

  EFF_MANUAL_INPUT,

  EFF_ATTACK,
  EFF_RELEASE,

  EFF_NUM_EFFECTS
};

enum Type{
  SinewaveType,
  TriangleType,
  SquareType,
  SawType,
  InvertedSawType,
  ManualInputType,
  AudioInputType,
  EnvelopeFollowerType,
  RandomType, // must be placed last (see NUM_TYPES)
};

#define NUM_TYPES (1+RandomType)

#define MAX_MULTIPLIER_NUMERATOR 32
#define MAX_MULTIPLIER_DENOMINATOR 32
                             
#define MAX_ATTACK 1000
#define MAX_RELEASE 1000
                            

static double hz_to_radians(double hz){
  return hz*M_PI2*(double)RADIUM_BLOCKSIZE/(double)pc->pfreq; // rate = pc->pfreq / RADIUM_BLOCKSIZE;
}

namespace{

struct ModulatorTarget{
  const struct Patch *patch;
  int effect_num;
  bool enabled;
  
  ModulatorTarget(const struct Patch *patch, int effect_num, bool enabled)
    : patch(patch)
    , effect_num(effect_num)
    , enabled(enabled)
  {}

  ModulatorTarget(const dyn_t dynstate)
    : ModulatorTarget(NULL, EFFNUM_INPUT_VOLUME, true)
  {    
    if (dynstate.type!=HASH_TYPE){
      R_ASSERT(false);
      return;
    }

    const hash_t *state = dynstate.hash;
    
    int64_t patch_id = HASH_get_int(state, ":instrument-id");
    patch = PATCH_get_from_id(patch_id);

    effect_num = HASH_get_int32(state, ":effect-num");
    enabled = HASH_get_bool(state, ":enabled");
  }

  // Note: Used directly in the API (i.e don't change the key names)
  dyn_t get_state(void) const {
    hash_t *state = HASH_create(2);
    HASH_put_int(state, ":instrument-id", patch->id);
    HASH_put_int(state, ":effect-num", effect_num);
    HASH_put_bool(state, ":enabled", enabled);

    return DYN_create_hash(state);
  }

};


struct GeneratorParameters{
  ModulatorEnabledTypes enabled_type = MET_ENABLED;
  
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

  float _curr_value;

private:

  radium::Delay<float> _delay;

public:

  Generator(const char *name)
    : _name(name)
    , _delay(ceil((double)MAX_COMPENSATED_LATENCY*(double)MIXER_get_sample_rate() / (1000.0 * (double)RADIUM_BLOCKSIZE)))
  {}
  
  virtual void RT_block_pre_process(const GeneratorParameters &parms){
  }

  virtual void RT_block_process(int modulator_latency, const struct Patch *patch, int effect_num, const GeneratorParameters &parms) {
    void *patchdata = patch->patchdata;

    if (patchdata==NULL){
      R_ASSERT_NON_RELEASE(false);
      return;
    }

    _delay.write(_curr_value);

    if(patch->instrument==get_audio_instrument()){

      SoundPlugin *plugin = static_cast<SoundPlugin*>(patchdata);

      const int audio_rate_latency = R_MAX(0, RT_SP_get_input_latency(plugin->sp) - modulator_latency);  // If this value is higher than the target latency, we would get negative latency.
      const int block_rate_latency = audio_rate_latency / RADIUM_BLOCKSIZE; // This function is running at block rate, not audio rate.

      float value = R_BOUNDARIES(0, _delay.tap(block_rate_latency), 1);
      //printf("VALUE: %f\n", value);

      PLUGIN_set_effect_value(plugin, 0, effect_num, value, DONT_STORE_VALUE, FX_middle, EFFECT_FORMAT_SCALED);

    } else {

      float value = R_BOUNDARIES(0, _delay.tap(0), 1);

      MIDI_set_effect_value(patch, 0, effect_num, value);

    }
  }

  virtual void RT_post_process(const GeneratorParameters &parms){
  }

  virtual ~Generator() = default; // Crazy c++ stuff.
};


struct OscillatorGenerator : public Generator {

  double _phase = 0.0;
  double _phase_add = 0.002;

  OscillatorGenerator(const char *name)
    : Generator(name)
  {}
  
  virtual double oscillator(double phase) = 0;
  
  void RT_block_pre_process(const GeneratorParameters &parms) override {

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

    _curr_value = scale_double(oscillator(_phase),
                               -1.0f, 1.0f,
                               parms.min, parms.max);
    
    //printf("_curr_value: %f - %f. %*f\n", _phase, beatpos, 10 + (int)scale(_curr_value, 0, 1, 0, 150), _curr_value);//_curr_value);

    _phase += _phase_add;
  }
};


struct SinewaveGenerator : public OscillatorGenerator {

  SinewaveGenerator()
    : OscillatorGenerator("Sine (LFO)")
  {}

  double oscillator(double phase) override {
    //return sin(phase - M_PI/2.0); // nah
    return sin(phase);
  }
};
 

struct TriangleGenerator : public OscillatorGenerator {

  TriangleGenerator()
    : OscillatorGenerator("Triangle (LFO)")
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
    : OscillatorGenerator("Square (LFO)")
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
    : OscillatorGenerator("Saw (LFO)")
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
    : OscillatorGenerator("Inverted Saw (LFO)")
  {}

  double oscillator(double phase) override {
    phase = fmod(phase, M_PI2);
  
    if (phase < M_PI)
      return scale_double(phase, 0, M_PI, 0, -1);
    else
      return scale_double(phase, M_PI, M_PI2, 1, 0);
  }
};

struct RandomGenerator : public OscillatorGenerator {

  RandomGenerator()
    : OscillatorGenerator("Random")
  {}

  int _last_int_value = 0;
  double _last_value;
  double _last_phase = 100;
  
  double oscillator(double phase) override {

    phase = fmod(phase, M_PI2);
    
    if (phase < _last_phase){
      _last_int_value = (_last_int_value + 12345) * 1103515245;
      _last_value = (double)_last_int_value / 2147483647.0;
      //printf("  Val: %f\n", _last_value);
    }

    _last_phase = phase;

    return _last_value;
  }
};

 
/*
Based on code generated by faust from running:
cd audio && ../bin/packages/faust2/compiler/faust -I ../bin/packages/faust2/architecture envelope_follower.dsp -double
*/

struct EnvelopeFollowerGenerator : public Generator {

#define FAUSTFLOAT float

  double _attack = 50;
  FAUSTFLOAT _release = 100;

private:
  
  double fRec1[2] = {0};
  double fRec0[2] = {0};
  double fConst0 = (1.0f / R_MIN(1.92e+05f, R_MAX(1.0f, FAUSTFLOAT(pc->pfreq))));


public:

  EnvelopeFollowerGenerator()
    : Generator("Audio Input Envelope Follower")
  {}

  ~EnvelopeFollowerGenerator(){
  }

  void RT_compute(int count, float* input0, float *output0, const GeneratorParameters &parms) {
    const double fHslider0 = _attack;
    const double fHslider1 = _release;

    double fSlow0 = exp((0.0 - (fConst0 / R_MAX(fConst0, (0.001 * double(fHslider0))))));
    double fSlow1 = exp((0.0 - (fConst0 / R_MAX(fConst0, (0.001 * double(fHslider1))))));
    for (int i = 0; (i < count); i = (i + 1)) {
      double fTemp0 = fabs(double(input0[i]));
      double fTemp1 = ((fRec0[1] > fTemp0)?fSlow1:fSlow0);
      fRec1[0] = ((fRec1[1] * fTemp1) + ((1.0 - fTemp1) * fTemp0));
      fRec0[0] = fRec1[0];
      output0[i] = FAUSTFLOAT(fRec0[0]);
      fRec1[1] = fRec1[0];
      fRec0[1] = fRec0[0];      
    }

    _curr_value = scale(fRec0[0], // last output value
                        0,1,
                        parms.min, parms.max
                        );
  }

};


static int64_t g_id = 0;

class Modulator{
  
public:
  
  int64_t _id = g_id++;

  int64_t _creation_time;

  int64_t gui = -1;
  
private:
  
  radium::Vector<ModulatorTarget*> *_targets = new radium::Vector<ModulatorTarget*>;

  SinewaveGenerator _sinewave_generator;
  TriangleGenerator _triangle_generator;
  SquareGenerator _square_generator;
  SawGenerator _saw_generator;
  InvertedSawGenerator _inverted_saw_generator;
  RandomGenerator _random_generator;
  Generator _manual_parameter_input_generator;
  Generator _audio_input_generator;
  EnvelopeFollowerGenerator _audio_envelope_follower;
  
public:

  GeneratorParameters _parms;

  Generator *_generator = &_sinewave_generator;

  struct SoundPlugin *_plugin;

  bool enable_when_not_playing = true;

  Modulator(struct SoundPlugin *plugin, hash_t *state = NULL)
    : _creation_time(state==NULL ? QDateTime::currentMSecsSinceEpoch() : HASH_get_int(state, "creation_time"))
    , _manual_parameter_input_generator("Manual Parameter Input")
    , _audio_input_generator("Audio Input (no envelope follower, no filtering)")
    , _plugin(plugin)
  {
  }

  ~Modulator(){
    for(auto *target : *_targets)
      delete target;
  }

  void RT_set_audio_generator_value(float val){
    float value =  /*
                     scale(_parms.phase_shift,
                     0, 1,
                     -1, 1)
                     +
                   */
      scale(val,
            0, 1,
            _parms.min, _parms.max);

    _audio_input_generator._curr_value = value;
  }

  void RT_set_attack(float value){
    _audio_envelope_follower._attack = value;
  }

  void RT_set_release(float value){
    _audio_envelope_follower._release = value;
  }

  float RT_get_attack(void) const {
    return _audio_envelope_follower._attack;
  }

  float RT_get_release(void) const {
    return _audio_envelope_follower._release;
  }

  void set_manual_parameter_input(float val){
    _manual_parameter_input_generator._curr_value = scale(val,
                                                          0,1,
                                                          _parms.min, _parms.max);
  }

  float get_manual_parameter_input(void) const {
    return _manual_parameter_input_generator._curr_value;
  }

  hash_t *get_state(void) const {
    hash_t *state = HASH_create(2);

    {
      const volatile struct Patch *modulator_patch = _plugin->patch;
      R_ASSERT_RETURN_IF_FALSE2(modulator_patch != NULL, state);
      HASH_put_int(state, "modulator_patch_id", modulator_patch->id);
    }

    dynvec_t targets = {0};
    
    for(auto *target : *_targets)
      DYNVEC_push_back(&targets, target->get_state());

    HASH_put_dyn(state, "targets", DYN_create_array(targets));
    
    return state;
  }

  void apply_state(const hash_t *state){
    // Assert that the state is for this modulator.
    {
      const volatile struct Patch *modulator_patch = _plugin->patch;
      R_ASSERT_RETURN_IF_FALSE(modulator_patch != NULL);
      
      int64_t patch_id = HASH_get_int(state, "modulator_patch_id");
      R_ASSERT_RETURN_IF_FALSE(patch_id==modulator_patch->id);
    }

    radium::Vector<ModulatorTarget*> *new_targets = new radium::Vector<ModulatorTarget*>;

    const dyn_t dyntargets = HASH_get_dyn(state, "targets");
    R_ASSERT_RETURN_IF_FALSE(dyntargets.type==ARRAY_TYPE);

    const dynvec_t *targets = dyntargets.array;

    for(const dyn_t &target : targets){
      auto *new_target = new ModulatorTarget(target);//targets->elements[i]);
      if (new_target->patch != NULL)
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


  Type _type = SinewaveType;

  void set_type(Type type){
    _type = type;

    switch(type){
      case SinewaveType:
        _generator = &_sinewave_generator;
        break;
      case TriangleType:
        _generator = &_triangle_generator;
        break;
      case SquareType:
        _generator = &_square_generator;
        break;
      case SawType:
        _generator = &_saw_generator;
        break;
      case InvertedSawType:
        _generator = &_inverted_saw_generator;
        break;
      case RandomType:
        _generator = &_random_generator;
        break;
      case ManualInputType:
        _generator = &_manual_parameter_input_generator;
        break;
      case AudioInputType:
        _generator = &_audio_input_generator;
        break;
      case EnvelopeFollowerType:
        _generator = &_audio_envelope_follower;
        break;
    }
  }

  Type get_type(void) const {
    return _type;
  }
  
  const char *get_type_name(void) const {
    return _generator->_name;
  }

  ModulatorTarget *get_target(const struct Patch *patch, int effect_num) const {
    for(auto *maybetarget : *_targets)
      if (maybetarget->patch==patch && maybetarget->effect_num==effect_num)
        return maybetarget;

    return NULL;
  }
  
  bool has_target(const struct Patch *patch, int effect_num) const {
    return get_target(patch, effect_num) != NULL;
  }
  
  void add_target(const struct Patch *patch, int effect_num){
    R_ASSERT_RETURN_IF_FALSE(has_target(patch,effect_num)==false);
    
    auto *target = new ModulatorTarget(patch, effect_num, true);

    _targets->ensure_there_is_room_for_more_without_having_to_allocate_memory(1);

    PLAYER_lock();{
      _targets->push_back(target);
    }PLAYER_unlock();

    _targets->post_add();
  }

  void remove_target(const struct Patch *patch, int effect_num){
    ModulatorTarget *target = get_target(patch, effect_num);
    R_ASSERT_RETURN_IF_FALSE(target!=NULL);
    
    ADD_UNDO(MixerConnections_CurrPos());

    PLAYER_lock();{
      _targets->remove(target);
    }PLAYER_unlock();
  }

  void set_target_enabled(const struct Patch *patch, int effect_num, bool enabled){
    ModulatorTarget *target = get_target(patch, effect_num);
    R_ASSERT_RETURN_IF_FALSE(target!=NULL);

    ADD_UNDO(MixerConnections_CurrPos());
    
    PLAYER_lock();{
      target->enabled = enabled;
    }PLAYER_unlock();
  }

  bool get_target_enabled(const struct Patch *patch, int effect_num){
    const ModulatorTarget *target = get_target(patch, effect_num);
    R_ASSERT_RETURN_IF_FALSE2(target!=NULL, false);

    return target->enabled;
  }
  
  void call_me_when_a_patch_is_made_inactive(const struct Patch *patch, radium::PlayerLockOnlyIfNeeded &lock, radium::UndoOnlyIfNeeded &undo){
  again:
    for(auto *maybetarget : *_targets)
      if (maybetarget->patch==patch){

        if(undo.should_I_make_undo_questionmark())
          ADD_UNDO(MixerConnections_CurrPos());

        lock.maybe_pause((int)_id);

        _targets->remove(maybetarget);
        goto again;
      }
  }

  void RT_block_process(void){

    if (_parms.enabled_type==MET_DISABLED)
      return;

    if (is_really_playing()==false && _parms.enabled_type==MET_ONLY_ENABLE_WHEN_PLAYING)
      return;

    int modulator_latency = 0;
    if (_type==AudioInputType || _type==EnvelopeFollowerType)
      modulator_latency = RT_SP_get_input_latency(_plugin->sp) - 1; // We subtract 1 since _curr_value was set to the last value of the previous block. Now we are called right before processing a new block.

    _generator->RT_block_pre_process(_parms);

    for(auto *target : *_targets){
      if (target->enabled)
        _generator->RT_block_process(modulator_latency, target->patch, target->effect_num, _parms);
    }

    _generator->RT_post_process(_parms);
  }
  void RT_process(int num_frames, float *input, float *output){
    RT_set_audio_generator_value(input[num_frames-1]); // We use the last frame since the value is not going to be used until the next block.

    if (_type==EnvelopeFollowerType)
      _audio_envelope_follower.RT_compute(num_frames, input, output, _parms);
    else {
      float val = _generator->_curr_value;
      for(int i=0 ; i < num_frames ; i++)
        output[i] = val;
    }
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
    modulator->RT_block_process();
}

static Modulator *get_modulator(int64_t modulator_patch_id){
  for(auto *modulator : g_modulators2){
    const volatile struct Patch *modulator_patch = modulator->_plugin->patch;
    R_ASSERT(modulator_patch != NULL);
    if (modulator_patch_id == modulator_patch->id)
      return modulator;
  }

  //R_ASSERT(false);
  return NULL;
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

  {
    int64_t old_modulator_id = MODULATOR_get_id(patch, effect_num);
    
    if(do_replace)
      R_ASSERT(old_modulator_id >= 0);
    else
      R_ASSERT(old_modulator_id==-1);
  }

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

  GFX_Menu3(v,

            [create_new, modulators, patch, effect_num](int command, bool onoff){

              if (PATCH_get_from_id(patch->id)==NULL){
                R_ASSERT_NON_RELEASE(false);
                return; // patch was deleted.
              }

              int64_t new_modulator_id;

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

                int modulator_num = command-2;
                if (modulator_num < 0 || modulator_num >= (int)modulators.size()){
                  RError("Illegal modulator_num: %d. (command: %d)", modulator_num, command); // Shouldn't be possible, but got a crash report for the 'new_modulator_id = modulators[command-2]->_id;' line. (think this was a bug in GFX_Menu actually)
                  return;
                }
    
                new_modulator_id = modulators[modulator_num]->_id;

                if (g_modulators.contains(new_modulator_id)==false){
                  R_ASSERT_NON_RELEASE(false);
                  return; // modulator was deleted while showing popup menu.
                }
              }

              int64_t old_modulator_id = MODULATOR_get_id(patch, effect_num); // Call MODULATOR_get_id another time inside the callback. In case the old value had changed while showing the menu.

              if(old_modulator_id >= 0)
                MODULATOR_remove_target(old_modulator_id, patch, effect_num);
              else
                ADD_UNDO(MixerConnections_CurrPos());

              MODULATOR_add_target(new_modulator_id, patch, effect_num);

            });
}

void MODULATOR_remove_target(int64_t modulator_id, const struct Patch *patch, int effect_num){
  R_ASSERT_RETURN_IF_FALSE(g_modulators.contains(modulator_id));  
  g_modulators[modulator_id]->remove_target(patch, effect_num);
}

void MODULATOR_set_target_enabled(int64_t modulator_id, const struct Patch *patch, int effect_num, bool enabled){
  R_ASSERT_RETURN_IF_FALSE(g_modulators.contains(modulator_id));  
  g_modulators[modulator_id]->set_target_enabled(patch, effect_num, enabled);
}

bool MODULATOR_get_target_enabled(int64_t modulator_id, const struct Patch *patch, int effect_num){
  R_ASSERT_RETURN_IF_FALSE2(g_modulators.contains(modulator_id), false);
  return g_modulators[modulator_id]->get_target_enabled(patch, effect_num);
}

void MODULATOR_call_me_when_a_patch_is_made_inactive(const struct Patch *patch){
  R_ASSERT(Undo_Is_Open() || Undo_Is_Currently_Undoing() || Undo_Is_Currently_Ignoring());

  radium::PlayerLockOnlyIfNeeded lock;
  radium::UndoOnlyIfNeeded undo;

  for(auto *modulator : g_modulators2)
    modulator->call_me_when_a_patch_is_made_inactive(patch, lock, undo);
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

bool MODULATOR_is_modulator(int64_t modulator_patch_id){
  return get_modulator(modulator_patch_id) != NULL;
}

// Note: The result is sent directly to the API
dynvec_t MODULATOR_get_modulator_targets(int64_t modulator_patch_id){
  Modulator *modulator = get_modulator(modulator_patch_id);
  if (modulator==NULL)
    return *g_empty_dynvec.array;

  hash_t *state = modulator->get_state();
  dyn_t dyn = HASH_get_dyn(state, "targets");
  R_ASSERT_RETURN_IF_FALSE2(dyn.type==ARRAY_TYPE, *g_empty_dynvec.array);
  
  return *dyn.array;
}

dyn_t MODULATOR_get_connections_state(void){
  dynvec_t vec = {0};

  for(auto *modulator : g_modulators2)
    DYNVEC_push_back(&vec, DYN_create_hash(modulator->get_state()));

  return DYN_create_array(vec);
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

void MODULATOR_apply_connections_state(const dyn_t dynstate){
  R_ASSERT_RETURN_IF_FALSE(dynstate.type==ARRAY_TYPE);
  
  for(const dyn_t modulator_state : dynstate.array){
    R_ASSERT_RETURN_IF_FALSE(modulator_state.type==HASH_TYPE);

    int64_t patch_id = HASH_get_int(modulator_state.hash, "modulator_patch_id");
    Modulator *modulator = get_modulator_from_patch_id(patch_id);
    R_ASSERT_RETURN_IF_FALSE(modulator!=NULL);

    modulator->apply_state(modulator_state.hash);
  }
}

static void RT_process(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs){
  Modulator *modulator = static_cast<Modulator*>(plugin->data);
  modulator->RT_process(num_frames, inputs[0], outputs[0]);
}


static void set_effect_value(struct SoundPlugin *plugin, int time, int effect_num, float value, enum ValueFormat value_format, FX_when when){
  Modulator *modulator = static_cast<Modulator*>(plugin->data);

  if(value_format==EFFECT_FORMAT_SCALED){
    
    switch(effect_num){

      case EFF_ENABLED:
        value = scale(value, 0, 1, 0, double(MET_NUM_ENABLED_TYPES)-0.001);
        break;

      case EFF_TYPE:
        value = scale(value, 0, 1, 0, double(NUM_TYPES)-0.001);
        break;
        
    
      case EFF_MULTIPLIER_NUMERATOR:
        value = scale(value, 0, 1, 1, MAX_MULTIPLIER_NUMERATOR+0.99);
        break;
        
      case EFF_MULTIPLIER_DENOMINATOR:
        value = scale(value, 0, 1, 1, MAX_MULTIPLIER_DENOMINATOR+0.99);
        break;
        
      case EFF_ATTACK:
        value = scale(value*value, 0, 1, 0, MAX_ATTACK);
        break;
        
      case EFF_RELEASE:
        value = scale(value*value, 0, 1, 0, MAX_RELEASE);
        break;

    }
  }

  switch(effect_num){

  case EFF_ENABLED:
    modulator->_parms.enabled_type = ModulatorEnabledTypes(value);
    break;
    
  case EFF_TYPE:
    {
      int type = value;
      R_ASSERT_RETURN_IF_FALSE(type>=0 && type < NUM_TYPES);
      modulator->set_type((Type)value);
      break;
    }
    
    
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

  case EFF_MANUAL_INPUT:
    modulator->set_manual_parameter_input(value);
    break;

  case EFF_ATTACK:
    modulator->RT_set_attack(value);
    break;

  case EFF_RELEASE:
    modulator->RT_set_release(value);
    break;

  default:
    RError("Unknown effect number %d. Value: %f\n",effect_num, value);
  }
  
}

static float get_effect_value(struct SoundPlugin *plugin, int effect_num, enum ValueFormat value_format){
  const Modulator *modulator = static_cast<Modulator*>(plugin->data);

  float value;

  switch(effect_num){

  case EFF_ENABLED:
    value = float(modulator->_parms.enabled_type);
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

  case EFF_MANUAL_INPUT:
    value = modulator->get_manual_parameter_input();
    break;

  case EFF_ATTACK:
    value = modulator->RT_get_attack();
    break;

  case EFF_RELEASE:
    value = modulator->RT_get_release();
    break;

  default:
    RError("Unknown effect number %d",effect_num);
    return 0.0f;
  }

  if(value_format==EFFECT_FORMAT_SCALED){
    
    switch(effect_num){

    case EFF_ENABLED:
      return scale(value, 0, double(MET_NUM_ENABLED_TYPES)-0.001, 0, 1);

    case EFF_TYPE:
      return scale(value, 0, double(NUM_TYPES)-0.001, 0, 1);

    
    case EFF_MULTIPLIER_NUMERATOR:
      return scale(value, 1, MAX_MULTIPLIER_NUMERATOR+0.99, 0, 1);

    case EFF_MULTIPLIER_DENOMINATOR:
      return scale(value, 1, MAX_MULTIPLIER_DENOMINATOR+0.99, 0, 1);

    case EFF_ATTACK:
      value = sqrtf(scale(value, 0, MAX_ATTACK, 0, 1));
      break;
      
    case EFF_RELEASE:
      value = sqrtf(scale(value, 0, MAX_RELEASE, 0, 1));
      break;

    }
  }


  return value;
}

static void get_display_value_string(SoundPlugin *plugin, int effect_num, char *buffer, int buffersize){
  const Modulator *modulator = static_cast<Modulator*>(plugin->data);

  switch(effect_num){

  case EFF_ENABLED: {
    const char *s = "";
    if (modulator->_parms.enabled_type==MET_DISABLED)
      s = "OFF";
    else if (modulator->_parms.enabled_type==MET_ONLY_ENABLE_WHEN_PLAYING)
      s = "Only when playing";
    else if (modulator->_parms.enabled_type==MET_ENABLED)
      s = "ON";
    else
      R_ASSERT(false);

    snprintf(buffer, buffersize-1, "%s", s);
    break;
  }
    

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

  case EFF_MANUAL_INPUT:
    snprintf(buffer, buffersize-1, "%f", modulator->get_manual_parameter_input());
    break;

  case EFF_ATTACK:
    snprintf(buffer, buffersize-1, "%.2fms", modulator->RT_get_attack());
    break;

  case EFF_RELEASE:
    snprintf(buffer, buffersize-1, "%.2fms", modulator->RT_get_release());
    break;

  default:
    RError("Unknown effect number %d",effect_num);
    return;
  }

}

static int get_effect_format(struct SoundPlugin *plugin, int effect_num){
  switch(effect_num){

  case EFF_ENABLED:
    return EFFECT_FORMAT_INT;
    
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
    
  case EFF_MANUAL_INPUT:
    return EFFECT_FORMAT_FLOAT;

  case EFF_ATTACK:
    return EFFECT_FORMAT_FLOAT;

  case EFF_RELEASE:
    return EFFECT_FORMAT_FLOAT;

  default:
    RError("Unknown effect number %d",effect_num);
    return EFFECT_FORMAT_FLOAT;
  }
}

static const char *get_effect_name(struct SoundPlugin *plugin, int effect_num){
  
  switch(effect_num){

  case EFF_ENABLED:
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
    return "Lower bound";
    
  case EFF_MAX:
    return "Upper bound";

  case EFF_MANUAL_INPUT:
    return "Manual input";

  case EFF_ATTACK:
    return "Attack";

  case EFF_RELEASE:
    return "Release";

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

  case EFF_MANUAL_INPUT:
    return "<br>Modulation value when \"Type\" is set to \"Manual Parameter Input\". Normal operations would be to move the slider manually, use automation, or MIDI learn."
      "<p>"
      "TIP: This slider is convenient to MIDI-learn on if using 3rd party modulators.";

  case EFF_ATTACK:
    return "Used by the envelope follower.";

  case EFF_RELEASE:
    return "Used by the envelope follower.";
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

static bool gui_is_visible(struct SoundPlugin *plugin){
  const volatile struct Patch *modulator_patch = plugin->patch;
  if(modulator_patch==NULL){
    R_ASSERT_NON_RELEASE(false);
    return false;
  }

  Modulator *modulator = static_cast<Modulator*>(plugin->data);

  if (modulator->gui >= 0 && gui_isOpen(modulator->gui))
    return true;

  return false;
}

static bool show_gui(struct SoundPlugin *plugin, int64_t parentgui){
  const volatile struct Patch *modulator_patch = plugin->patch;
  if(modulator_patch==NULL){
    R_ASSERT_NON_RELEASE(false);
    return false;
  }

  Modulator *modulator = static_cast<Modulator*>(plugin->data);

  if (modulator->gui >= 0 && gui_isOpen(modulator->gui)){
    gui_raise(modulator->gui);
    return true;
  }
  
  modulator->gui = S7CALL2(int_int, "FROM_C-create-modulator-gui", modulator_patch->id);

  if (modulator->gui>=0){

    //printf("    PARENT: %d\n", (int)parentgui);
    //gui_setParent(modulator->gui, parentgui);
    gui_setParent(modulator->gui, -1); // We let main window be parent. Other windows might not work very well as parent, for some reason.

    gui_show(modulator->gui);
    return true;
    
  } else {
    
    return false;
    
  }
}

static void hide_gui(struct SoundPlugin *plugin){
  Modulator *modulator = static_cast<Modulator*>(plugin->data);

  if (modulator->gui >= 0){
    if (gui_isOpen(modulator->gui))
      gui_close(modulator->gui);
    modulator->gui = -1;
  }
}

void create_modulator_plugin(void){
  SoundPluginType *plugin_type = (SoundPluginType*)V_calloc(1,sizeof(SoundPluginType));

  plugin_type->type_name                = MODULATOR_NAME;
  plugin_type->name                     = MODULATOR_NAME;
  plugin_type->num_inputs               = 1;
  plugin_type->num_outputs              = 1;
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

  plugin_type->gui_is_visible = gui_is_visible;
  plugin_type->show_gui = show_gui;
  plugin_type->hide_gui = hide_gui;

  plugin_type->will_never_autosuspend = true; // Must always run.
  
  PR_add_plugin_type(plugin_type);
}

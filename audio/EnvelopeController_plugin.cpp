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


#include "EnvelopeController_plugin_proc.h"


#define ENVELOPE_CONTROLLER_NAME "Envelope Controller"

static double hz_to_radians(double hz, double sample_rate){
  return hz*((2.0*M_PI)/sample_rate);
}

namespace{

struct EnvelopeControllerTarget{
  const struct Patch *patch;
  int effect_num;

  EnvelopeControllerTarget(const struct Patch *patch, int effect_num)
    : patch(patch)
    , effect_num(effect_num)
  {}
};


struct GeneratorParameters{
  double min = 0.0;
  double max = 1.0;

  bool follows_tempo = true;
  double tempo_multiplier = 2; // Used if _follows_tempo==true
  double hz = 50; // Used if follows_tempo==false

  double phase_shift = -0.5;
};


struct Generator{
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
  
  void RT_pre_process(const GeneratorParameters &parms) override {

    double bpm;

    double hz;
    double beatpos;

    if (parms.follows_tempo){

      const struct SeqTrack *seqtrack;
    
      if (pc->playtype==PLAYBLOCK)
        seqtrack = root->song->block_seqtrack;
      else
        seqtrack = (struct SeqTrack *)root->song->seqtracks.elements[0]; // FIX.

      Ratio signature = RT_Signature_get_current_Signature(seqtrack);
      
      double beat_position = RT_LPB_get_beat_position(seqtrack);
      //double last_beat = seqtrack->beat_iterator.beat_position_of_last_bar_start;
      beatpos = beat_position;// - last_beat;
      //beatpos = beatpos - floor(beatpos);

      //double bar_duration = 4.0 * (double)signature.numerator / (double)signature.denominator;
      _phase = parms.tempo_multiplier*beatpos*M_PI*2 + parms.phase_shift*M_PI;

      bpm = RT_LPB_get_current_BPM(seqtrack);
      hz = (bpm/60.0) * parms.tempo_multiplier * (double)signature.numerator / (double)signature.denominator;

      _curr_value = R_BOUNDARIES(parms.min,
                                 scale(sin(_phase),
                                       -1.0f, 1.0f,
                                       parms.min, parms.max),
                                 parms.max);

    }else{

      hz = parms.hz;


    _curr_value = R_BOUNDARIES(parms.min,
                               scale(sin(_phase),
                                     -1.0f, 1.0f,
                                     parms.min, parms.max),
                               parms.max);

    }

    _phase_add = hz_to_radians(hz, pc->pfreq);
    printf("_curr_value: %f - %f. %*f\n", _phase, beatpos, 10 + (int)scale(_curr_value, 0, 1, 0, 150), _curr_value);//_curr_value);
    _phase += _phase_add;
  }

  void RT_process(const struct Patch *patch, int effect_num, const GeneratorParameters &parms) override {
    if(patch->instrument==get_audio_instrument()){
      SoundPlugin *plugin = static_cast<SoundPlugin*>(patch->patchdata);
      if(plugin==NULL){        
        R_ASSERT_NON_RELEASE(false);
        return;        
      }

      PLUGIN_set_effect_value(plugin, 0, effect_num, _curr_value, DONT_STORE_VALUE, FX_middle, EFFECT_FORMAT_SCALED);

    } else {

      MIDI_set_effect_value(patch, 0, effect_num, _curr_value);

    }
  }
};

static int64_t g_id = 0;

class EnvelopeController{
  
public:
  
  int64_t _id = g_id++;

private:
  
  radium::Vector<const EnvelopeControllerTarget*> _targets;

  OscillatorGenerator _oscillator;
  
  Generator &_generator = _oscillator;

  GeneratorParameters _parms;

public:

  struct SoundPlugin *_plugin;

  EnvelopeController(struct SoundPlugin *plugin)
    : _plugin(plugin)
  {}

  ~EnvelopeController(){
    for(auto *target : _targets)
      delete target; // TODO: Must tell patch that envelope controller has been removed. For the GUI. Or perhaps the gui should just ask if a patch + effect num combo is connected... That sounds cleaner.
  }

  bool has_target(const struct Patch *patch, int effect_num){
    for(auto *target : _targets)
      if (target->patch==patch && target->effect_num==effect_num)
        return true;

    return false;
  }
  
  void add_target(const struct Patch *patch, int effect_num){
    R_ASSERT_RETURN_IF_FALSE(has_target(patch,effect_num)==false);
    
    auto *target = new EnvelopeControllerTarget(patch, effect_num);

    _targets.ensure_there_is_room_for_more_without_having_to_allocate_memory(1);

    PLAYER_lock();{
      _targets.push_back(target);
    }PLAYER_unlock();

    _targets.post_add();
  }

  void remove_target(const struct Patch *patch, int effect_num){
    const EnvelopeControllerTarget *target = NULL;
    
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

    _generator.RT_pre_process(_parms);

    for(auto *target : _targets){
      _generator.RT_process(target->patch, target->effect_num, _parms);
    }

    _generator.RT_post_process(_parms);
  }
};

} // end anon. namespace


static QHash<int64_t, EnvelopeController*> g_envelope_controllers;
static radium::Vector<EnvelopeController*> g_envelope_controllers2;

// Called from the main audio thread
void RT_ENVELOPECONTROLLER_process(void){
  for(auto *controller : g_envelope_controllers2)
    controller->RT_process();
}

int64_t ENVELOPECONTROLLER_get_controller_id(const struct Patch *patch, int effect_num){
  for(int64_t id : g_envelope_controllers.keys()){
    auto *controller = g_envelope_controllers[id];
    if(controller->has_target(patch, effect_num))
      return id;
  }

  return -1;
}
                                             

void ENVELOPECONTROLLER_add_target(int64_t controller_id, const struct Patch *patch, int effect_num){
  R_ASSERT_RETURN_IF_FALSE(true==g_envelope_controllers.contains(controller_id));

  R_ASSERT_RETURN_IF_FALSE(ENVELOPECONTROLLER_get_controller_id(patch, effect_num)==-1);

  g_envelope_controllers[controller_id]->add_target(patch, effect_num);
}


void ENVELOPECONTROLLER_maybe_create_and_add_target(const struct Patch *patch, int effect_num){
  R_ASSERT_RETURN_IF_FALSE(ENVELOPECONTROLLER_get_controller_id(patch, effect_num)==-1);
  
  vector_t v = {0};

  int create_new = VECTOR_push_back(&v, "Create new envelope controller");
  VECTOR_push_back(&v, "--------------");
  for(int64_t id : g_envelope_controllers.keys()){
    //auto *controller = g_envelope_controllers[id];    
    auto *controller = g_envelope_controllers[id];
    volatile struct Patch *patch = controller->_plugin->patch;
    VECTOR_push_back(&v, talloc_format("%d: %s (%s)", (int)id, ENVELOPECONTROLLER_get_description(id), patch==NULL ? "" : patch->name));
  }

  int64_t controller_id;

  int command = GFX_Menu(root->song->tracker_windows, NULL, "", v, true);
  if (command < 0)
    return;


  if (command==create_new){

    struct Patch *curr_patch = g_currpatch;

    int64_t instrument_id = createAudioInstrument(ENVELOPE_CONTROLLER_NAME, ENVELOPE_CONTROLLER_NAME, "", 0, 0);
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

    EnvelopeController *controller = static_cast<EnvelopeController*>(plugin->data);
    
    controller_id = controller->_id;

  } else {

    controller_id = g_envelope_controllers.keys()[command - 2];

  }

  ENVELOPECONTROLLER_add_target(controller_id, patch, effect_num);
}

void ENVELOPECONTROLLER_remove_target(int controller_id, const struct Patch *patch, int effect_num){
  R_ASSERT_RETURN_IF_FALSE(g_envelope_controllers.contains(controller_id));  
  g_envelope_controllers[controller_id]->remove_target(patch, effect_num);
}

int64_t *ENVELOPECONTROLLER_get_controller_ids(int *num_controllers){

  const auto &keys = g_envelope_controllers.keys();
  *num_controllers = keys.size();

  int64_t *ids = (int64_t*)talloc(sizeof(int64_t)*keys.size());

  int i=0;
  for(int64_t id : g_envelope_controllers.keys())
    ids[i++] = id;

  return ids;
}

const char *ENVELOPECONTROLLER_get_description(int64_t controller_id){
  return "Oscillator";
}

static void RT_process(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs){
  //EnvelopeController *controller = static_cast<EnvelopeController*>(plugin->data);

  // no need to do anything. no inputs and no outputs.
}

static void set_effect_value(struct SoundPlugin *plugin, int time, int effect_num, float value, enum ValueFormat value_format, FX_when when){
  //EnvelopeController *controller = static_cast<EnvelopeController*>(plugin->data);
}

static float get_effect_value(struct SoundPlugin *plugin, int effect_num, enum ValueFormat value_format){
  //EnvelopeController *controller = static_cast<EnvelopeController*>(plugin->data);
  return 0.0f;
}

static void get_display_value_string(SoundPlugin *plugin, int effect_num, char *buffer, int buffersize){
  //EnvelopeController *controller = static_cast<EnvelopeController*>(plugin->data);
  snprintf(buffer,buffersize-1,"%f",0.0f);
}

static void *create_plugin_data(const SoundPluginType *plugin_type, SoundPlugin *plugin, hash_t *state, float sample_rate, int block_size, bool is_loading){
  EnvelopeController *controller = new EnvelopeController(plugin);
  g_envelope_controllers[controller->_id] = controller;

  g_envelope_controllers2.ensure_there_is_room_for_more_without_having_to_allocate_memory(1);
  PLAYER_lock();{
    g_envelope_controllers2.push_back(controller);
  }PLAYER_unlock();
  g_envelope_controllers2.post_add();

  return controller;
}

static void cleanup_plugin_data(SoundPlugin *plugin){
  EnvelopeController *controller = static_cast<EnvelopeController*>(plugin->data);

  PLAYER_lock();{
    g_envelope_controllers2.remove(controller);
  }PLAYER_unlock();
  
  g_envelope_controllers.remove(controller->_id);

  delete controller;
}

static const char *get_effect_name(struct SoundPlugin *plugin, int effect_num){
  return "Volume";
}

static int RT_get_audio_tail_length(struct SoundPlugin *plugin){
  return 0;
}


void create_envelope_controller_plugin(void){
  SoundPluginType *plugin_type = (SoundPluginType*)V_calloc(1,sizeof(SoundPluginType));

  plugin_type->type_name                = ENVELOPE_CONTROLLER_NAME;
  plugin_type->name                     = ENVELOPE_CONTROLLER_NAME;
  plugin_type->num_inputs               = 0;
  plugin_type->num_outputs              = 0;
  plugin_type->is_instrument            = false;
  plugin_type->note_handling_is_RT      = false;
  plugin_type->num_effects              = 1;
  plugin_type->get_effect_format        = NULL;
  plugin_type->get_effect_name          = get_effect_name;
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

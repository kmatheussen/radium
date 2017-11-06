#include <QHash>

#include "../common/nsmtracker.h"
#include "../common/instruments_proc.h"
#include "../common/OS_visual_input.h"

#include "../midi/midi_fx_proc.h"

#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"

#include "SoundPluginRegistry_proc.h"


#include "EnvelopeController_plugin_proc.h"



struct EnvelopeControllerTarget{
  const struct Patch *patch;
  int effect_num;

  EnvelopeControllerTarget(const struct Patch *patch, int effect_num)
    : patch(patch)
    , effect_num(effect_num)
  {}
};

struct Generator{
  virtual void RT_pre_process(int64_t time, int num_frames){
  }
  virtual void RT_process(int64_t time, int num_frames, const struct Patch *patch, int effect_num) = 0;
  virtual void RT_post_process(int64_t time, int num_frames){
  }

  virtual ~Generator() = default; // Crazy c++ stuff.
};

struct OscillatorGenerator : public Generator {
  double _phase = 0.0;
  double _phase_add = 0.002;
  float _curr_value;
  
  void RT_pre_process(int64_t time, int num_frames) override {
    _curr_value = R_BOUNDARIES(0.0f,
                               scale(sin(_phase),
                                     -1.0f, 1.0f,
                                     0.0f, 1.0f),
                               1.0f);
    printf("_curr_value: %*f\n", 10 + (int)scale(_curr_value, 0, 1, 0, 150), _curr_value);
    _phase += _phase_add;
  }

  void RT_process(int64_t time, int num_frames, const struct Patch *patch, int effect_num) override {
    if(patch->instrument==get_audio_instrument()){
      SoundPlugin *plugin = static_cast<SoundPlugin*>(patch->patchdata);
      if(plugin==NULL){        
        R_ASSERT_NON_RELEASE(false);
        return;        
      }

      PLUGIN_set_effect_value(plugin, time, effect_num, _curr_value, DONT_STORE_VALUE, FX_middle, EFFECT_FORMAT_SCALED);

    } else {

      MIDI_set_effect_value(patch, time, effect_num, _curr_value);

    }
  }
};

static int64_t g_id = 0;

class EnvelopeController{
  
public:
  
  int64_t id = g_id++;

private:
  
  radium::Vector<const EnvelopeControllerTarget*> targets;

  OscillatorGenerator oscillator;
  
  Generator &generator = oscillator;

public:

  ~EnvelopeController(){
    for(auto *target : targets)
      delete target; // TODO: Must tell patch that envelope controller has been removed. For the GUI. Or perhaps the gui should just ask if a patch + effect num combo is connected... That sounds cleaner.
  }

  bool has_target(const struct Patch *patch, int effect_num){
    for(auto *target : targets)
      if (target->patch==patch && target->effect_num==effect_num)
        return true;

    return false;
  }
  
  void add_target(const struct Patch *patch, int effect_num){
    R_ASSERT_RETURN_IF_FALSE(has_target(patch,effect_num)==false);
    
    auto *target = new EnvelopeControllerTarget(patch, effect_num);

    targets.ensure_there_is_room_for_more_without_having_to_allocate_memory(1);

    PLAYER_lock();{
      targets.push_back(target);
    }PLAYER_unlock();

    targets.post_add();
  }

  void remove_target(const struct Patch *patch, int effect_num){
    const EnvelopeControllerTarget *target = NULL;
    
    for(auto *maybetarget : targets)
      if (maybetarget->patch==patch && maybetarget->effect_num==effect_num){
        target = maybetarget;
        break;
      }

    R_ASSERT_RETURN_IF_FALSE(target!=NULL);
        
    PLAYER_lock();{
      targets.remove(target);
    }PLAYER_unlock();
  }
  
  void RT_process(int64_t time, int num_frames){

    generator.RT_pre_process(time, num_frames);
      
    for(auto *target : targets){
      generator.RT_process(time, num_frames,target->patch, target->effect_num);
    }
    
    generator.RT_post_process(time, num_frames);
  }
};


static QHash<int64_t, EnvelopeController*> g_envelope_controllers;

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
    VECTOR_push_back(&v, talloc_format("%d: %s", (int)id, ENVELOPECONTROLLER_get_description(id)));
  }

  int command = GFX_Menu(root->song->tracker_windows, NULL, "", v, true);

  if (command==create_new){
    
  }

  if (command <= 0)
    return;

  int64_t controller_id = g_envelope_controllers.keys()[command - 2];

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
  EnvelopeController *controller = static_cast<EnvelopeController*>(plugin->data);
  controller->RT_process(time, num_frames);
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
  EnvelopeController *controller = new EnvelopeController;
  g_envelope_controllers[controller->id] = controller;
  return controller;
}

static void cleanup_plugin_data(SoundPlugin *plugin){
  EnvelopeController *controller = static_cast<EnvelopeController*>(plugin->data);
  g_envelope_controllers.remove(controller->id);
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

  plugin_type->type_name                = "Envelope Controller";
  plugin_type->name                     = "Envelope Controller";
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

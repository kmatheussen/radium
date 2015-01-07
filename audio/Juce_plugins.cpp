/*
  A Juce plugin SoundPlugin.

  Handles VST and AU plugins/instruments.

  The old VST Plugin system is still available (audio/VSTPlugins.cpp), and is used when loading old songs.

  New songs will use this system instead for vst plugins.
*/

#include <math.h>



#include "../common/nsmtracker.h"
#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"

#include "SoundPluginRegistry_proc.h"

#include "../pluginhost/JuceLibraryCode/JuceHeader.h"


#include "Juce_plugins_proc.h"

namespace{
  
  struct PluginWindow  : public DocumentWindow {
    //==============================================================================
    PluginWindow()
      : DocumentWindow ("JUCE Plugin window",
                        Colours::lightgrey,
                        DocumentWindow::allButtons,
                        true)
    {
        // Create an instance of our main content component, and add it to our window..

        // Centre the window on the screen
      //centreWithSize (getWidth(), getHeight());      
      //setUsingNativeTitleBar(true);

      // And show it!
      //setVisible (true);
    }

    ~PluginWindow()
    {
        // (the content component will be deleted automatically, so no need to do it here)
    }

    //==============================================================================
    void closeButtonPressed() override
    {
        // When the user presses the close button, we'll tell the app to quit. This
        // HelloWorldWindow object will be deleted by the JUCEHelloWorldApplication class.      
    }
  };

  struct Data{
    double phase;
    double phase_add;
    double volume;
    double sample_rate;

    AudioPluginInstance *audio_instance;
    DocumentWindow *window;
  };

  struct TypeData{
    const PluginDescription* pluginDescription;
  };
  
}

static void RT_process(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs){
  //SoundPluginType *type = plugin->type;
  Data *data = (Data*)plugin->data;
  int i;
  float *out = outputs[0];
  double phase = data->phase;
  double phase_add = data->phase_add;

  for(i=0;i<num_frames;i++){
    out[i] = sin(phase) * data->volume;
    phase += phase_add;
  }

  data->phase = phase;
}

static double hz_to_radians(double hz, double sample_rate){
  return hz*((2*3.14159)/sample_rate);
}

static double midi_to_hz(int midi){
  if(midi<=0)
    return 0;
  else
    return 8.17579891564*(expf(.0577622650*midi));
}

static double midi_to_radians(int midi, double sample_rate){
  return hz_to_radians(midi_to_hz(midi),sample_rate);
}

static void play_note(struct SoundPlugin *plugin, int64_t time, float note_num, int64_t note_id, float volume,float pan){
  Data *data = (Data*)plugin->data;
  data->phase_add = midi_to_radians(note_num,data->sample_rate);
  data->volume = volume;
  printf("####################################################### Setting volume to %f (play note)\n",volume);
}

static void set_note_volume(struct SoundPlugin *plugin, int64_t time, float note_num, int64_t note_id, float volume){
  Data *data = (Data*)plugin->data;
  data->volume = volume;
  printf("####################################################### Setting volume to %f\n",volume);
}

static void stop_note(struct SoundPlugin *plugin, int64_t time, float note_num, int64_t note_id){
  Data *data = (Data*)plugin->data;
  data->volume = 0.0f;
  printf("####################################################### Setting sine volume to %f (stop note)\n",0.0f);
}

static void set_effect_value(struct SoundPlugin *plugin, int64_t time, int effect_num, float value, enum ValueFormat value_format, FX_when when){
  Data *data = (Data*)plugin->data;
  printf("####################################################### Setting sine volume to %f\n",value);
  data->volume = value;
}

float get_effect_value(struct SoundPlugin *plugin, int effect_num, enum ValueFormat value_format){
  Data *data = (Data*)plugin->data;
  return data->volume;
}

static void get_display_value_string(SoundPlugin *plugin, int effect_num, char *buffer, int buffersize){
  Data *data = (Data*)plugin->data;
  snprintf(buffer,buffersize-1,"%f",data->volume);
}

static void show_gui(struct SoundPlugin *plugin){
  Data *data = (Data*)plugin->data;
  data->window->setVisible(true);
}

static void hide_gui(struct SoundPlugin *plugin){
  Data *data = (Data*)plugin->data;
  data->window->setVisible(false);
}

static void *create_plugin_data(const SoundPluginType *plugin_type, SoundPlugin *plugin, hash_t *state, float sample_rate, int block_size){
  R_ASSERT(MessageManager::getInstance()->isThisTheMessageThread());
    
  Data *data = (Data*)calloc(1, sizeof(Data));
  
  data->phase = 0.0f;
  data->phase_add = 0.062;
  data->volume = 0.5f;
  printf("####################################################### Setting sine volume to 0.5f (create_plugin_data)\n");
  data->sample_rate = sample_rate;

  data->window = new DocumentWindow ("JUCE Plugin window",
                        Colours::lightgrey,
                        DocumentWindow::allButtons,
                                     true);//PluginWindow;

  //data->window.setVisible(true);
  return data;
}

static void cleanup_plugin_data(SoundPlugin *plugin){
  printf(">>>>>>>>>>>>>> Cleanup_plugin_data called for %p\n",plugin);
  free(plugin->data);
}

static const char *get_effect_name(struct SoundPlugin *plugin, int effect_num){
  return "Volume";
}


void create_juce_plugins(void){
  SoundPluginType *plugin_type = (SoundPluginType*)calloc(1,sizeof(SoundPluginType));

  plugin_type->data = calloc(1, sizeof(TypeData));

  plugin_type->type_name                = "Sine Synth2";
  plugin_type->name                     = "Sine Synth2";
  plugin_type->num_inputs               = 0;
  plugin_type->num_outputs              = 1;
  plugin_type->is_instrument            = true;
  plugin_type->note_handling_is_RT      = false;
  plugin_type->num_effects              = 1;
  plugin_type->get_effect_format        = NULL;
  plugin_type->get_effect_name          = get_effect_name;
  plugin_type->effect_is_RT             = NULL;
  plugin_type->create_plugin_data       = create_plugin_data;
  plugin_type->cleanup_plugin_data      = cleanup_plugin_data;
  
  plugin_type->RT_process       = RT_process;
  plugin_type->play_note        = play_note;
  plugin_type->set_note_volume  = set_note_volume;
  plugin_type->stop_note        = stop_note;
  plugin_type->set_effect_value = set_effect_value;
  plugin_type->get_effect_value = get_effect_value;
  plugin_type->get_display_value_string = get_display_value_string;

  plugin_type->show_gui = show_gui;
  plugin_type->hide_gui = hide_gui;

  PR_add_plugin_type(plugin_type);
}

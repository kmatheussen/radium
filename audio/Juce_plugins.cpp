/*
  A Juce plugin SoundPlugin.

  Handles VST and AU plugins/instruments.

  The old VST Plugin system is still available (audio/VSTPlugins.cpp), and is used when loading old songs.

  New songs will use this system instead for vst plugins.
*/

#include <math.h>
#include <string.h>

#include "../common/nsmtracker.h"
#include "../common/patch_proc.h"
#include "../common/PEQ_LPB_proc.h"

#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"

#include "SoundPluginRegistry_proc.h"

#include "../pluginhost/JuceLibraryCode/JuceHeader.h"

#include "../pluginhost/JuceLibraryCode/AppConfig.h"


#  include "vestige/aeffectx.h"  // It should not be a problem to use VESTIGE in this case. It's just used for getting vendor string and product string.



#include "Juce_plugins_proc.h"

extern struct Root *root;

namespace{

  struct PluginWindow;
  struct MyAudioPlayHead;

  struct Data{
    AudioPluginInstance *audio_instance;
    PluginWindow *window;
    AudioProcessorEditor *editor;

    MyAudioPlayHead *playHead;

    MidiBuffer midi_buffer;
    AudioSampleBuffer buffer;

    int num_input_channels;
    int num_output_channels;

    Data(AudioPluginInstance *audio_instance, int num_input_channels, int num_output_channels)
      : audio_instance(audio_instance)
      , window(NULL)
      , editor(NULL)
      , buffer(R_MAX(num_input_channels, num_output_channels), RADIUM_BLOCKSIZE)
      , num_input_channels(num_input_channels)
      , num_output_channels(num_output_channels)
    {}
  };

  struct MyAudioPlayHead : public AudioPlayHead{
    virtual bool getCurrentPosition (CurrentPositionInfo &result) {
      if (pc->isplaying==false && (root==NULL || root->song==NULL || root->song->tracker_windows==NULL || root->song->tracker_windows->wblock==NULL || root->song->tracker_windows->wblock->block==NULL))
        return false;

      struct Blocks *block = pc->isplaying ? pc->block : root->song->tracker_windows->wblock->block;

      if (block->times==NULL)
        return false;
      
      result.bpm = (float)root->tempo * (pc->isplaying ? block->reltempo : 1.0f); // fixme
      //printf("result.bpm: %f\n",result.bpm);

      result.timeSigNumerator = 4;
      result.timeSigDenominator = 4;

      result.timeInSamples = pc->start_time;
      result.timeInSeconds = (double)pc->start_time / (double)pc->pfreq;
      result.editOriginTime = 0; //result.timeInSeconds;

      result.ppqPosition = RT_LPB_get_beat_position();
      result.ppqPositionOfLastBarStart = 0; // fixme

      result.isPlaying = pc->isplaying;
      result.isRecording = false;
      
      result.ppqLoopStart = 0; // fixme
      result.ppqLoopEnd = 0; // fixme

      result.isLooping = pc->playtype==PLAYBLOCK || pc->playtype==PLAYRANGE;

      return true;

    }
  };

  static MyAudioPlayHead myAudioPlayHead;

  struct TypeData{
    const PluginDescription* pluginDescription;
    AudioProcessor::WrapperType wrapper_type;
    const char **effect_names;
  };

  struct PluginWindow  : public DocumentWindow {
    PluginWindow(const char *title)
      : DocumentWindow (title,
                        Colours::lightgrey,
                        DocumentWindow::allButtons,
                        true)
    {
      // Centre the window on the screen
      centreWithSize (getWidth(), getHeight());      
    }

    void closeButtonPressed() override
    {
      setVisible(false);
    }
  };

  struct JuceThread : public Thread {
    Atomic<int> isInited;

    JuceThread()
      : Thread("Juce dispatcher thread")
    {
      isInited.set(0);
    }

    virtual void run(){
      initialiseJuce_GUI();
      isInited.set(1);
      MessageManager::getInstance()->runDispatchLoop(); 
    }
  };
}

static void buffer_size_is_changed(struct SoundPlugin *plugin, int new_buffer_size){
  Data *data = (Data*)plugin->data;
  data->buffer.setSize(data->buffer.getNumChannels(), new_buffer_size);
}

static void RT_process(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs){
  Data *data = (Data*)plugin->data;


  // 1. Process audio

  AudioPluginInstance *instance = data->audio_instance;
  AudioSampleBuffer &buffer = data->buffer;

  for(int ch=0; ch<data->num_input_channels ; ch++)
    memcpy(buffer.getWritePointer(ch), inputs[ch], sizeof(float)*num_frames);
  
  instance->processBlock(buffer, data->midi_buffer);

  for(int ch=0; ch<data->num_output_channels ; ch++)
    memcpy(outputs[ch], buffer.getReadPointer(ch), sizeof(float)*num_frames);


  // 2. Send out midi (untested, need plugin to test with)

  if (instance->producesMidi()) {
    struct Patch *patch = plugin->patch;
    if (patch!=NULL) {
      
      MidiBuffer::Iterator iterator(data->midi_buffer);
      
      MidiMessage message;
      int samplePosition;
      
      while(iterator.getNextEvent(message, samplePosition)){
        if (message.isNoteOn())
          RT_PATCH_send_play_note_to_receivers(patch, message.getNoteNumber(), -1, message.getVelocity() / 127.0f, 0.0f, pc->start_time+samplePosition);
        
        else if (message.isNoteOff())
          RT_PATCH_send_stop_note_to_receivers(patch, message.getNoteNumber(), -1, pc->start_time+samplePosition);
        
        else if (message.isAftertouch())
          RT_PATCH_send_change_velocity_to_receivers(patch, message.getNoteNumber(), -1, message.getChannelPressureValue() / 127.0f, pc->start_time+samplePosition);
      }
    }
  }

}

static void play_note(struct SoundPlugin *plugin, int64_t time, float note_num, int64_t note_id, float volume,float pan){
  Data *data = (Data*)plugin->data;
  MidiBuffer &buffer = data->midi_buffer;

  MidiMessage message(0x90, (int)note_num, (int)(volume*127), 0.0);
  buffer.addEvent(message, time);
}

static void set_note_volume(struct SoundPlugin *plugin, int64_t time, float note_num, int64_t note_id, float volume){
  Data *data = (Data*)plugin->data;
  MidiBuffer &buffer = data->midi_buffer;

  MidiMessage message(0xa0, (int)note_num, (int)(volume*127), 0.0);
  buffer.addEvent(message, time);
}

static void stop_note(struct SoundPlugin *plugin, int64_t time, float note_num, int64_t note_id){
  Data *data = (Data*)plugin->data;
  MidiBuffer &buffer = data->midi_buffer;

  MidiMessage message(0x90, (int)note_num, 0, 0.0);
  buffer.addEvent(message, time);
}

static void set_effect_value(struct SoundPlugin *plugin, int64_t time, int effect_num, float value, enum ValueFormat value_format, FX_when when){
  Data *data = (Data*)plugin->data;
  data->audio_instance->setParameter(effect_num, value);
}

float get_effect_value(struct SoundPlugin *plugin, int effect_num, enum ValueFormat value_format){
  Data *data = (Data*)plugin->data;
  return data->audio_instance->getParameter(effect_num);
}

static void get_display_value_string(SoundPlugin *plugin, int effect_num, char *buffer, int buffersize){
  const MessageManagerLock mmLock;
  
  Data *data = (Data*)plugin->data;
  snprintf(buffer,buffersize-1,"%s",data->audio_instance->getParameterText(effect_num, buffersize-1).toRawUTF8());
}

static void show_gui(struct SoundPlugin *plugin){
  const MessageManagerLock mmLock;
  
  Data *data = (Data*)plugin->data;

  if (data->window==NULL) {
    data->window = new PluginWindow(plugin->patch==NULL ? talloc_format("%s %s",plugin->type->type_name, plugin->type->name) : plugin->patch->name);

    data->editor = data->audio_instance->createEditor();

    data->window->setContentOwned(data->editor, true);
    data->window->setUsingNativeTitleBar(true);

    data->editor->setVisible(true);
  }

  data->window->setVisible(true);
}

static void hide_gui(struct SoundPlugin *plugin){
  const MessageManagerLock mmLock;
  
  Data *data = (Data*)plugin->data;

  if (data->window!=NULL && data->editor!=NULL)
    data->window->setVisible(false);
}


static AudioPluginInstance *get_audio_instance(const PluginDescription *description, float sample_rate, int block_size){
  static bool inited=false;

  static AudioPluginFormatManager formatManager;

  if (inited==false){
    formatManager.addDefaultFormats();
    inited=true;
  }

  String errorMessage;

  AudioPluginInstance *instance = formatManager.createPluginInstance(*description,sample_rate,block_size,errorMessage);

  if (instance==NULL){
    RError("Unable to open VST plugin %s: %s\n",description->fileOrIdentifier.toRawUTF8(), errorMessage.toRawUTF8());
    return NULL;
  }

  instance->setPlayHead(&myAudioPlayHead);

  instance->prepareToPlay(sample_rate, block_size);

  return instance;
}

static void set_plugin_type_data(AudioPluginInstance *audio_instance, SoundPluginType *plugin_type){
  TypeData *type_data = (struct TypeData*)plugin_type->data;

  plugin_type->num_inputs = audio_instance->getNumInputChannels();
  plugin_type->num_outputs = audio_instance->getNumOutputChannels();
    
  plugin_type->plugin_takes_care_of_savable_values = true;

  const char *wrapper_info = "";
  
  if (type_data->wrapper_type == AudioProcessor::wrapperType_VST) {
    AEffect *aeffect = (AEffect*)audio_instance->getPlatformSpecificData();
    {
      char vendor[1024] = {0};
      aeffect->dispatcher(aeffect, effGetVendorString, 0, 0, vendor, 0.0f);
      char product[1024] = {0};
      aeffect->dispatcher(aeffect, effGetProductString, 0, 0, product, 0.0f);

      if(strlen(vendor)>0 || strlen(product)>0)
        wrapper_info = talloc_format("Vendor: %s\nProduct: %s\n",vendor,product);
    }
  }

  plugin_type->info = talloc_format("%sAccepts MIDI: %s\nProduces midi: %s\n",wrapper_info, audio_instance->acceptsMidi()?"Yes":"No", audio_instance->producesMidi()?"Yes":"No");
        
  plugin_type->is_instrument = audio_instance->acceptsMidi(); // doesn't seem like this field ("is_instrument") is ever read...

  plugin_type->num_effects = audio_instance->getNumParameters();

  type_data->effect_names = (const char**)calloc(sizeof(char*),plugin_type->num_effects);
  for(int i = 0 ; i < plugin_type->num_effects ; i++)
    type_data->effect_names[i] = talloc_strdup(audio_instance->getParameterName(i).toRawUTF8());
}

static void *create_plugin_data(const SoundPluginType *plugin_type, SoundPlugin *plugin, hash_t *state, float sample_rate, int block_size){
  const MessageManagerLock mmLock;
  
  TypeData *type_data = (struct TypeData*)plugin_type->data;

  AudioPluginInstance *audio_instance = get_audio_instance(type_data->pluginDescription, sample_rate, block_size);
  if (audio_instance==NULL)
    return NULL;
    
  Data *data = new Data(audio_instance, audio_instance->getNumInputChannels(), audio_instance->getNumOutputChannels());

  if(type_data->effect_names==NULL)
    set_plugin_type_data(audio_instance,(SoundPluginType*)plugin_type); // 'plugin_type' was created here (by using calloc), so it can safely be casted into a non-const.

  // load program state
  if (state!=NULL) {

    if (HASH_has_key(state, "audio_instance_current_program")) {
      int current_program = HASH_get_int(state, "audio_instance_current_program");
      audio_instance->setCurrentProgram(current_program);
    }

    const char *stateAsString = HASH_get_chars(state, "audio_instance_program_state");
    if (stateAsString != NULL) {
      MemoryBlock sourceData;
      sourceData.fromBase64Encoding(stateAsString);
      
      audio_instance->setCurrentProgramStateInformation(sourceData.getData(), sourceData.getSize());
    }
  }

  return data;
}


static void create_state(struct SoundPlugin *plugin, hash_t *state){
  const MessageManagerLock mmLock;

  Data *data = (Data*)plugin->data;
  
  AudioPluginInstance *audio_instance = data->audio_instance;

  // save state
  {
    MemoryBlock destData;
    audio_instance->getStateInformation(destData);

    if (destData.getSize() > 0){
      String stateAsString = destData.toBase64Encoding();    
      HASH_put_chars(state, "audio_instance_state", stateAsString.toRawUTF8());
    }
  }

  // save program state
  {
    MemoryBlock destData;
    audio_instance->getCurrentProgramStateInformation(destData);

    if (destData.getSize() > 0){
      String stateAsString = destData.toBase64Encoding();    
      HASH_put_chars(state, "audio_instance_program_state", stateAsString.toRawUTF8());
    }
  }

  HASH_put_int(state, "audio_instance_current_program", audio_instance->getCurrentProgram());
}

static void recreate_from_state(struct SoundPlugin *plugin, hash_t *state){
  const MessageManagerLock mmLock;

  Data *data = (Data*)plugin->data;
  
  AudioPluginInstance *audio_instance = data->audio_instance;

  const char *stateAsString = HASH_get_chars(state, "audio_instance_state");
  if (stateAsString != NULL) {
    MemoryBlock sourceData;
    sourceData.fromBase64Encoding(stateAsString);

    audio_instance->setStateInformation(sourceData.getData(), sourceData.getSize());
  }
}

static void cleanup_plugin_data(SoundPlugin *plugin){
  const MessageManagerLock mmLock;
 
  printf(">>>>>>>>>>>>>> Cleanup_plugin_data called for %p\n",plugin);
  Data *data = (Data*)plugin->data;

  if (data->window != NULL)
    delete data->window;

  delete data->audio_instance;
  delete data;
}

static const char *get_effect_name(struct SoundPlugin *plugin, int effect_num){
  TypeData *type_data = (struct TypeData*)plugin->type->data;
  return type_data->effect_names[effect_num];
}

static const char *get_effect_description(const struct SoundPluginType *plugin_type, int effect_num){
  TypeData *type_data = (struct TypeData*)plugin_type->data;
  return type_data->effect_names[effect_num];
}

void add_juce_plugin_type(const char *name, const char *filepath){
  const MessageManagerLock mmLock;
  
  SoundPluginType *plugin_type = (SoundPluginType*)calloc(1,sizeof(SoundPluginType));

  TypeData *typeData = (TypeData*)calloc(1, sizeof(TypeData));

  PluginDescription *description = new PluginDescription();
  description->fileOrIdentifier = strdup(filepath);
  typeData->pluginDescription = description;

  typeData->wrapper_type = AudioProcessor::wrapperType_VST;

  plugin_type->data = typeData;

  plugin_type->type_name = "VST";
  plugin_type->name      = strdup(name);

  plugin_type->is_instrument = true; // we don't know yet, so we set it to true.
  
  plugin_type->buffer_size_is_changed = buffer_size_is_changed;

  plugin_type->RT_process = RT_process;
  plugin_type->create_plugin_data = create_plugin_data;
  plugin_type->cleanup_plugin_data = cleanup_plugin_data;
  
  plugin_type->show_gui = show_gui;
  plugin_type->hide_gui = hide_gui;
  
  plugin_type->play_note       = play_note;
  plugin_type->set_note_volume = set_note_volume;
  plugin_type->stop_note       = stop_note;
  
  plugin_type->get_effect_value = get_effect_value;
  plugin_type->set_effect_value = set_effect_value;
  
  plugin_type->get_display_value_string=get_display_value_string;
  plugin_type->get_effect_name=get_effect_name;
  plugin_type->get_effect_description=get_effect_description;

  plugin_type->create_state = create_state;
  plugin_type->recreate_from_state = recreate_from_state;

  PR_add_plugin_type(plugin_type);
}


void PLUGINHOST_init(void){
  
  JuceThread *juce_thread = new JuceThread;
  juce_thread->startThread();

  while(juce_thread->isInited.get()==0)
    Thread::sleep(20);
}

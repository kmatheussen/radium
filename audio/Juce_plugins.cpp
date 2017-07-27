/*
  A Juce plugin SoundPlugin.

  Handles VST and AU plugins/instruments.

  The old VST Plugin system is still available (audio/VSTPlugins.cpp), and is used when loading old songs.

  New songs will use this system instead for vst plugins.
*/

#if defined(COMPILING_JUCE_PLUGINS_O)



#include <math.h>
#include <string.h>

#include "../pluginhost/JuceLibraryCode/AppConfig.h"

#include "../pluginhost/JuceLibraryCode/JuceHeader.h"


#if JUCE_LINUX
  #define FOR_LINUX 1
#endif

#if JUCE_WINDOWS
#define FOR_WINDOWS 1
  #ifdef FOR_LINUX
    #error "gakk"
  #endif
#endif

#if JUCE_MAC
#define FOR_MACOSX 1
  #ifdef FOR_LINUX
    #error "gakk"
  #endif
  #ifdef FOR_WINDOWS
    #error "gakk2"
  #endif
#endif

#define Slider Radium_Slider
#include "../common/nsmtracker.h"
#include "../common/patch_proc.h"
#include "../common/scheduler_proc.h"
//#include "../common/PEQ_Signature_proc.h"
//#include "../common/PEQ_Beats_proc.h"
#include "../common/visual_proc.h"
#include "../common/player_proc.h"
#include "../common/OS_Player_proc.h"
#include "../common/OS_system_proc.h"
#include "../common/OS_settings_proc.h"
#include "../crashreporter/crashreporter_proc.h"
#include "../OpenGL/Widget_proc.h"
#include "../midi/midi_i_plugin_proc.h"

#include "../api/api_proc.h"

#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"
#include "SoundProducer_proc.h"

#include "SoundPluginRegistry_proc.h"

#include "VST_plugins_proc.h"

#if 1 // no more vestige. why did I ever bother with it?
#if FOR_LINUX
  #  undef PRAGMA_ALIGN_SUPPORTED
  #  define __cdecl
#endif
#  include "pluginterfaces/vst2.x/aeffectx.h"
#else
#  include "vestige/aeffectx.h"  // It should not be a problem to use VESTIGE in this case. It's just used for getting vendor string and product string.
#endif

#include "../midi/midi_proc.h"


#include "../midi/midi_juce.cpp"
#include "Juce_plugins_proc.h"

#undef Slider



#if FOR_LINUX
#define CUSTOM_MM_THREAD 1
#else
#define CUSTOM_MM_THREAD 0
#endif

static int g_num_visible_plugin_windows = 0;
static bool g_vst_grab_keyboard = true;

static int RT_get_latency(struct SoundPlugin *plugin);


namespace{

  struct PluginWindow;

  struct Listener : public AudioProcessorListener {

    SoundPlugin *_plugin; // Is never NULL. Listener is removed before plugin is deleted.

    Listener(SoundPlugin *plugin) : _plugin(plugin) {}
    
    // Receives a callback when a parameter is changed.
    void 	audioProcessorParameterChanged (AudioProcessor *processor, int parameterIndex, float newValue) override {
#if !defined(RELEASE)
      printf("   JUCE listener: parm %d changed to %f\n",parameterIndex,newValue);
#endif
      PLUGIN_call_me_when_an_effect_value_has_changed(_plugin,
                                                      parameterIndex,
                                                      newValue, // native
                                                      newValue, // scaled
                                                      true // make undo
                                                      );
    }
 
    // Called to indicate that something else in the plugin has changed, like its program, number of parameters, etc.
    void 	audioProcessorChanged (AudioProcessor *processor) override {
#if !defined(RELEASE)
      printf("    JUCE listener: audioProcessorChanged...\n");
#endif
      volatile struct Patch *patch = _plugin->patch;
      if (patch != NULL)
        ATOMIC_SET(patch->widget_needs_to_be_updated, true);
    }
 

    //Indicates that a parameter change gesture has started.
    void 	audioProcessorParameterChangeGestureBegin (AudioProcessor *processor, int parameterIndex) override {
#if !defined(RELEASE)
      printf("    JUCE listener: gesture starts for %d\n",parameterIndex);
#endif
    }

    
    //Indicates that a parameter change gesture has finished. 
    void 	audioProcessorParameterChangeGestureEnd (AudioProcessor *processor, int parameterIndex) override {
#if !defined(RELEASE)
      printf("    JUCE listener: gesture ends for %d\n",parameterIndex);
#endif
    }
  };

  
  struct MyAudioPlayHead : public AudioPlayHead{

    String _plugin_name;
    struct SoundPlugin *_plugin;  // Set to NULL when SoundPlugin *plugin is deleted. (we can still be alive though, since the deletion of juce plugins are delayed)
    
    double positionOfLastLastBarStart = 0.0;
    bool positionOfLastLastBarStart_is_valid = false;

    double lastLastBarStart = 0.0; // only used for debugging.
        

    MyAudioPlayHead(String plugin_name, struct SoundPlugin *plugin)
      : _plugin_name(plugin_name)
      , _plugin(plugin)
    {}

    void plugin_will_be_deleted(void){
      _plugin = NULL;
    }
    
    // From JUCE documenation: You can ONLY call this from your processBlock() method!
    // I.e. it will only be called from the player thread or a multicore thread.
    virtual bool getCurrentPosition (CurrentPositionInfo &result) {
      memset(&result, 0, sizeof(CurrentPositionInfo));

      if (THREADING_is_main_thread()){
        RT_message("Error in plugin \"%s\": Asked for timing information from the main thread. Please contact the plugin vendor to fix this bug.\n", _plugin_name.toRawUTF8());
        return false;
      }
      
      if (false==PLAYER_someone_has_player_lock()){ // Could complicate things by checking if process() is called specifically for this plugin, but this check probably fires if a plugin misbehaves anyway.
        RT_message("Error in plugin \"%s\": Asked for timing information outside process(). Please contact the plugin vendor to fix this bug.\n", _plugin_name.toRawUTF8());
        return false;
      }
      
      if (ATOMIC_GET(is_starting_up))
        return false;

      if (_plugin==NULL) // This situation is probably picked up by the two RT_message cases above though.
        return false;
        
      //RT_PLUGIN_touch(_plugin); // If the plugin needs timing data, it should probably not be autopaused.

      bool isplaying = is_playing();

      const struct SeqTrack *seqtrack;

      if (pc->playtype==PLAYBLOCK)
        seqtrack = root->song->block_seqtrack;
      else
        seqtrack = (struct SeqTrack *)root->song->seqtracks.elements[0]; // FIX.


      result.bpm = RT_LPB_get_current_BPM(seqtrack);
      //printf("result.bpm: %f\n",result.bpm);

      if (result.bpm==0){
        R_ASSERT_NON_RELEASE(false);
        result.bpm = 1; // At least one vst plugin crashes if bpm is 0.
      }
      
      Ratio signature = RT_Signature_get_current_Signature(seqtrack);
      result.timeSigNumerator = signature.numerator;
      result.timeSigDenominator = signature.denominator;
      //printf("%d/%d\n",signature.numerator,signature.denominator);

      const int latency = RT_SP_get_input_latency(_plugin->sp);

      const double latency_beats = ((double)latency / (double)pc->pfreq) * result.bpm / 60.0;

      if (!isplaying){
              
        result.timeInSamples = -latency;
        result.timeInSeconds = (double)-latency / (double)pc->pfreq;

        result.ppqPosition               = -latency_beats;
        result.ppqPositionOfLastBarStart = -latency_beats;

        positionOfLastLastBarStart_is_valid = false;      

      } else {
        
        result.timeInSamples = pc->absabstime - latency;
        result.timeInSeconds = result.timeInSamples / (double)pc->pfreq;

        //if(ATOMIC_GET(root->editonoff))
        //  printf("timeInSeconds: %f\n", result.timeInSeconds);
        
        result.ppqPosition               = RT_LPB_get_beat_position(seqtrack) - latency_beats;
        result.ppqPositionOfLastBarStart = seqtrack->beat_iterator.beat_position_of_last_bar_start;
        
        if (result.ppqPosition < result.ppqPositionOfLastBarStart) {

          if (positionOfLastLastBarStart_is_valid)            
            result.ppqPositionOfLastBarStart = positionOfLastLastBarStart;
          else {

            // I.e. when starting to play we don't have a previous last bar start value.
            
            double bar_length = 4.0 * (double)signature.numerator / (double)signature.denominator;

#if 1
            R_ASSERT_NON_RELEASE(bar_length!=0);

            double num_bars_to_subtract = ceil( (result.ppqPositionOfLastBarStart - result.ppqPosition) /
                                                bar_length
                                                );
            result.ppqPositionOfLastBarStart -= (bar_length * num_bars_to_subtract);
              
#else
            // This version is cleaner, but it can freeze the program if we have a value that is out of the ordinary.
            do {
              result.ppqPositionOfLastBarStart -= bar_length;
            } while (result.ppqPosition < result.ppqPositionOfLastBarStart);
#endif
          }
          
        } else {
          positionOfLastLastBarStart = result.ppqPositionOfLastBarStart;
          positionOfLastLastBarStart_is_valid = true;
        }
      
      }

#if 0
      result.editOriginTime = 0; //result.timeInSeconds;
#endif


#if 0
      //if (result.ppqPositionOfLastBarStart != lastLastBarStart)
      //  printf("  ppq: %f,  ppqlast: %f, extra: %f. Latency: %d\n",result.ppqPosition,result.ppqPositionOfLastBarStart,latency_beats,latency);
      
      lastLastBarStart = result.ppqPositionOfLastBarStart;
        
      printf("ppq: %f, ppqlast: %f. playing: %d. time: %f\n",
             result.ppqPosition,
             result.ppqPositionOfLastBarStart,
             isplaying,
             result.timeInSeconds);
#endif
      
      result.isPlaying = isplaying;
#if 0
      result.isRecording = false;
      
      result.ppqLoopStart = 0; // fixme (probably nothing to fix. This value is probably only necessary if time jumps back)
      result.ppqLoopEnd = 0; // fixme (same here)

      result.isLooping = false; //pc->playtype==PLAYBLOCK || pc->playtype==PLAYRANGE; (same here)
#endif

      return true;
    }
  };

  static bool is_vst2(const struct SoundPluginType *type) {
    return !strcmp(type->type_name, "VST");
  }
  
  static bool is_vst2(const SoundPlugin *plugin) {
    return is_vst2(plugin->type);
  }

  static bool is_vst3(const SoundPlugin *plugin) {
    return !strcmp(plugin->type->type_name, "VST3");
  }

  /*
  static bool is_vst(const SoundPlugin *plugin) {
    return is_vst2(plugin) || is_vst3(plugin);
  }
  */
  
  /*
  static bool is_vst2(AudioProcessor *processor){
    return processor->wrapperType == AudioProcessor::wrapperType_VST;
  }

  static bool is_vst3(AudioProcessor *processor){
    RError("This function doesnt work");
    return processor->wrapperType == AudioProcessor::wrapperType_VST3;
  }

  static bool is_vst(AudioProcessor *processor){
    return is_vst2(processor) || is_vst3(processor);
  }
  */
  
  struct Data{
    AudioPluginInstance *audio_instance;

    SoundPlugin *_plugin;
    
    PluginWindow *window;

    MyAudioPlayHead playHead;

    MidiBuffer midi_buffer;
    AudioSampleBuffer buffer;

    Listener listener;

    int num_input_channels;
    int num_output_channels;

    int x;
    int y;

    /*
    bool is_vst2(void){
      return ::is_vst2(audio_instance);
    }

    bool is_vst3(void){
      return ::is_vst3(audio_instance);
    }
    
    bool is_vst(void){
      return ::is_vst(audio_instance);
    }
    */
    Data(AudioPluginInstance *audio_instance, SoundPlugin *plugin, int num_input_channels, int num_output_channels)
      : audio_instance(audio_instance)
      , _plugin(plugin)
      , window(NULL)
      , playHead(audio_instance->getPluginDescription().name, plugin)
      , buffer(R_MAX(num_input_channels, num_output_channels), RADIUM_BLOCKSIZE)
      , listener(plugin)
      , num_input_channels(num_input_channels)
      , num_output_channels(num_output_channels)
      , x(-1)
      , y(-1)
    {
      audio_instance->addListener(&listener);
      midi_buffer.ensureSize(1024*16);
    }

    void plugin_will_be_deleted(void){
      audio_instance->removeListener(&listener);
      playHead.plugin_will_be_deleted();
      _plugin = NULL;
    }
  };

  
  /*
  static bool is_au(AudioProcessor *processor){
    return processor->wrapperType == AudioProcessor::wrapperType_AudioUnit ; // || processor->wrapperType == AudioProcessor::wrapperType_AudioUnitv3;
  }
  */

  struct TypeData{
    const wchar_t *file_or_identifier; // used by Juce.
    PluginDescription description;
    const char **effect_names = NULL; // set the first time the plugin is loaded
    bool has_shown_noncompatible_warning = false; // TODO: recreate_from_state is called twice when loading preset.
    TypeData(const wchar_t *file_or_identifier,
             PluginDescription description
             )
      : file_or_identifier(wcsdup(file_or_identifier))
      , description(description)
    {}

    ~TypeData(){
      RError("Not supposed to be called\n");
      free((void*)file_or_identifier);      
    }
  };

  struct ContainerData{
    const wchar_t *file_or_identifier; // used by Juce
  };

  //int button_height = 10;

  struct PluginWindow;
  
  struct PluginWindow  : public DocumentWindow, Button::Listener, Timer {
    Data *data;
    const char *title;
    Component main_component;
    ToggleButton grab_keyboard_button;
    //ToggleButton always_on_top_button;
    ToggleButton bypass_button;
    
    AudioProcessorEditor* const editor;

    virtual void 	buttonClicked (Button *) override {
    }

    virtual void buttonStateChanged (Button *dasbutton) override {
      if (dasbutton == &grab_keyboard_button) {
        
        bool new_state = grab_keyboard_button.getToggleState();

        //printf("ButtonStateChanged called for %p. %d %d. Size: %d\n", this, new_state, g_vst_grab_keyboard,(int)g_plugin_windows.size());
        
        if (new_state != g_vst_grab_keyboard) {
          
          g_vst_grab_keyboard = new_state;
          
          //for (auto *plugin_window : g_plugin_windows)
          //  plugin_window->button.setToggleState(g_vst_grab_keyboard, sendNotification);
        }
        
      }
      /*
      else if (dasbutton == &always_on_top_button) {

        bool new_state = always_on_top_button.getToggleState();
        if (new_state != vstGuiIsAlwaysOnTop())
          setVstGuiAlwaysOnTop(new_state);
      }
      */

      else if (dasbutton == &bypass_button) {
        bool new_state = bypass_button.getToggleState();

        struct SoundPlugin *plugin = data->_plugin;
        R_ASSERT_RETURN_IF_FALSE(plugin!=NULL);
        
        bool is_bypass = !ATOMIC_GET(plugin->effects_are_on);
        
        //printf("ButtonStateChanged called for %p. %d %d. Size: %d\n", this, new_state, g_vst_grab_keyboard,(int)g_plugin_windows.size());
        
        if (new_state != is_bypass) {
          int num_effects = plugin->type->num_effects;
          PLUGIN_set_effect_value(plugin, -1, num_effects+EFFNUM_EFFECTS_ONOFF, new_state ? 0.0 : 1.0, STORE_VALUE, FX_single, EFFECT_FORMAT_SCALED);
          GFX_ScheduleInstrumentRedraw((struct Patch*)plugin->patch);
        }
        
      }
    }
    
    void timerCallback() override {
      // grab button
      {
        bool new_state = grab_keyboard_button.getToggleState();
        if (new_state != g_vst_grab_keyboard)
          grab_keyboard_button.setToggleState(g_vst_grab_keyboard, dontSendNotification);
      }

      // bypass button
      {
        bool new_state = bypass_button.getToggleState();
        struct SoundPlugin *plugin = data->_plugin;
        R_ASSERT_RETURN_IF_FALSE(plugin!=NULL);
        
        bool is_bypass = !ATOMIC_GET(plugin->effects_are_on);
        if (new_state != is_bypass)
          bypass_button.setToggleState(is_bypass, dontSendNotification);
      }
      
      //if (isAlwaysOnTop() != vstGuiIsAlwaysOnTop())
      //  delete this;
      //this->setAlwaysOnTop(vstGuiIsAlwaysOnTop());
    }
    
    PluginWindow(const char *title, Data *data, AudioProcessorEditor* const editor)
      : DocumentWindow (title,
                        Colours::lightgrey,
                        DocumentWindow::closeButton, //allButtons,
                        true)
      , data(data)
      , title(title)
      , editor(editor)
    {

      struct SoundPlugin *plugin = data->_plugin;
      ATOMIC_SET(plugin->auto_suspend_suspended, true);
        
#if !FOR_WINDOWS
      if(vstGuiIsAlwaysOnTop()) {
        this->setAlwaysOnTop(true);
      }
#endif
      //this->setAlwaysOnTop(true);
      
      this->setSize (400, 300);
      this->setUsingNativeTitleBar(true);

      int button_height = root->song->tracker_windows->fontheight * 3 / 2;
      
      main_component.setSize(R_MIN(100, editor->getWidth()), R_MIN(100, editor->getHeight()));
      this->setContentNonOwned(&main_component, true);

#if defined(RELEASE) && FOR_LINUX
#else
      // grab keyboard button
      {
#if FOR_LINUX
        grab_keyboard_button.setButtonText("Grab keyboard (not functional on Linux)");
#else
        grab_keyboard_button.setButtonText("Grab keyboard");
#endif
        
        grab_keyboard_button.setToggleState(g_vst_grab_keyboard, dontSendNotification);
        grab_keyboard_button.setSize(400, button_height);
        grab_keyboard_button.changeWidthToFitText();
        
        grab_keyboard_button.addListener(this);
      
        // add it
        main_component.addAndMakeVisible(&grab_keyboard_button);
        grab_keyboard_button.setTopLeftPosition(0, 0);
      }
#endif
      
      // always-on-top button
#if 0
      {
        always_on_top_button.setButtonText("Always on top");
        
        always_on_top_button.setToggleState(vstGuiIsAlwaysOnTop(), dontSendNotification);
        always_on_top_button.setSize(400, button_height);
        always_on_top_button.changeWidthToFitText();
        
        always_on_top_button.addListener(this);
      
        // add it
        main_component.addAndMakeVisible(&always_on_top_button);
        always_on_top_button.setTopLeftPosition(0, 0);
      }
#endif
     
      // bypass button
#if 1
      {
        bypass_button.setButtonText("Bypass");
        
        bypass_button.setToggleState(vstGuiIsAlwaysOnTop(), dontSendNotification);
        bypass_button.setSize(400, button_height);
        bypass_button.changeWidthToFitText();
        
        bypass_button.addListener(this);
      
        // add it
        main_component.addAndMakeVisible(&bypass_button);
        bypass_button.setTopLeftPosition(0, 0);
      }
#endif
      

      // add vst gui
      main_component.addChildComponent(editor);
      editor->setTopLeftPosition(0, button_height);
      
      if (data->x <= 0 || data->y <= 0) {
        this->centreWithSize (getWidth(), getHeight());
      } else {
        this->setTopLeftPosition(data->x, data->y);
      }

      this->setVisible(true);

      main_component.setSize(editor->getWidth(), editor->getHeight() + button_height);

#if defined(RELEASE) && FOR_LINUX
      bypass_button.setTopLeftPosition(main_component.getWidth()-bypass_button.getWidth(), 0);
#else
      grab_keyboard_button.setTopLeftPosition(main_component.getWidth()-grab_keyboard_button.getWidth(), 0);
      //always_on_top_button.setTopLeftPosition(main_component.getWidth()-grab_keyboard_button.getWidth()-always_on_top_button.getWidth(), 0);
      bypass_button.setTopLeftPosition(main_component.getWidth()-grab_keyboard_button.getWidth()-bypass_button.getWidth(), 0);
#endif

      startTimer(100);

#if FOR_WINDOWS
      if(vstGuiIsAlwaysOnTop()) {
        OS_WINDOWS_set_always_on_top(this->getWindowHandle());
      }
#endif
    }

    ~PluginWindow(){
      GL_lock();{

        radium::ScopedMutex lock(JUCE_show_hide_gui_lock);
    
        delete editor;
        data->window = NULL;
        V_free((void*)title);
        
        struct SoundPlugin *plugin = data->_plugin;
        R_ASSERT_RETURN_IF_FALSE(plugin!=NULL);
        
        ATOMIC_SET(plugin->auto_suspend_suspended, false);

      }GL_unlock();
    }

    /*
    void focusOfChildComponentChanged(FocusChangeType cause) override
    {
      printf("\n\n      child focus %s\n\n", hasKeyboardFocus(true) ? "obtained" : "lost");
      
      if (hasKeyboardFocus(true)) // hasKeyboardFocus doesn't always work for sub windows.
        obtain_keyboard_focus();
      else
        release_keyboard_focus();
    }

    void focusGained(FocusChangeType cause) override
    {
      printf("\n\n    focusGained\n\n");
      //obtain_keyboard_focus();
    }
    
    void focusLost(FocusChangeType cause) override
    {
      printf("\n\n    focusLost\n\n");
      //release_keyboard_focus();
    }
    */
    
    void closeButtonPressed() override
    {
      delete this;      
    }

    void moved() override {
      data->x = getX();
      data->y = getY();
    }

  };


#if CUSTOM_MM_THREAD
  struct JuceThread : public Thread {
    Atomic<int> isInited;

    JuceThread()
      : Thread("Juce dispatcher thread")
    {
      isInited.set(0);
    }

    virtual void run(){
      initialiseJuce_GUI();
      MessageManager::getInstance(); // make sure there is an instance here to avoid theoretical race condition
      isInited.set(1);
      MessageManager::getInstance()->runDispatchLoop(); 
    }
  };
#endif
}

static void buffer_size_is_changed(struct SoundPlugin *plugin, int new_buffer_size){
  Data *data = (Data*)plugin->data;
  data->buffer.setSize(data->buffer.getNumChannels(), new_buffer_size);
}

// 
static void RT_MIDI_send_msg_to_patch_receivers2(struct SeqTrack *seqtrack, struct Patch *patch, MidiMessage message, int64_t seq_time){       
  if (message.isNoteOn()) {
    //printf("Out. Sending note ON %d\n",  message.getNoteNumber());
    note_t note = create_note_t(NULL,
                                -1,
                                message.getNoteNumber(),
                                message.getVelocity() / 127.0f,
                                0.0f,
                                message.getChannel()-1,
                                0,
                                0);
    RT_PATCH_send_play_note_to_receivers(seqtrack, patch, note, seq_time);
    
  } else if (message.isNoteOff()) {
    //printf("Out. Sending note OFF %d\n",  message.getNoteNumber());
    note_t note = create_note_t(NULL,
                                -1,
                                message.getNoteNumber(),
                                0,
                                0.0f,
                                message.getChannel()-1,
                                0,
                                0);
    RT_PATCH_send_stop_note_to_receivers(seqtrack, patch, note, seq_time);
  
  } else if (message.isAftertouch()) {
    note_t note = create_note_t(NULL,
                                -1,
                                message.getNoteNumber(),
                                message.getAfterTouchValue() / 127.0f,                                
                                0.0f,
                                message.getChannel()-1,
                                0,
                                0);
    RT_PATCH_send_change_velocity_to_receivers(seqtrack, patch, note, seq_time);

  } else {
    
    const uint8_t *raw_data = message.getRawData();
    int len = message.getRawDataSize();

    R_ASSERT_RETURN_IF_FALSE(len>=1 && len<=3);

    uint32_t msg;

    if (len==3)
      msg = MIDI_msg_pack3(raw_data[0],raw_data[1],raw_data[2]);
    else if (len==2)
      msg = MIDI_msg_pack2(raw_data[0],raw_data[1]);
    else if (len==1)
      msg = MIDI_msg_pack1(raw_data[0]);
    else
      return;
    
    RT_PATCH_send_raw_midi_message_to_receivers(seqtrack, patch, msg, seq_time);
  }
}

int RT_MIDI_send_msg_to_patch_receivers(struct SeqTrack *seqtrack, struct Patch *patch, void *data, int data_size, int64_t seq_time){
  int num_bytes_used;

  R_ASSERT(data_size>0);
  
  {
    uint8_t *d=(uint8_t*)data;
    if (d[0] < 0x80) {
      RT_message("Illegal value in first byte of MIDI message: %d\n",d[0]);
      return 0;
    }
  }
  
  MidiMessage message(data, data_size, num_bytes_used, 0);
  
  RT_MIDI_send_msg_to_patch_receivers2(seqtrack, patch, message, seq_time);

  return num_bytes_used;
}

static void RT_MIDI_send_msg_to_patch2(struct SeqTrack *seqtrack, struct Patch *patch, MidiMessage message, int64_t seq_time){       
  if (message.isNoteOn()) {
    note_t note = create_note_t(NULL,
                                -1,
                                message.getNoteNumber(),
                                message.getVelocity() / 127.0f,
                                0.0f,
                                message.getChannel()-1,
                                0,
                                0);

    RT_PATCH_play_note(seqtrack, patch, note, seq_time);
  
  } else if (message.isNoteOff()) {
    note_t note = create_note_t(NULL,
                                -1,
                                message.getNoteNumber(),
                                0,
                                0.0f,
                                message.getChannel()-1,
                                0,
                                0);
    RT_PATCH_stop_note(seqtrack, patch, note, seq_time + 1); // We add 1 to the timing here to avoid hanging note if note on and note off are sent simultaneously.
  
  } else if (message.isAftertouch()) {
    note_t note = create_note_t(NULL,
                                -1,
                                message.getNoteNumber(),
                                message.getAfterTouchValue() / 127.0f,                                
                                0.0f,
                                message.getChannel()-1,
                                0,
                                0);
    RT_PATCH_change_velocity(seqtrack, patch, note, seq_time);

  } else {
    
    const uint8_t *raw_data = message.getRawData();
    int len = message.getRawDataSize();

    R_ASSERT_RETURN_IF_FALSE(len>=1 && len<=3);

    uint32_t msg;

    if (len==3)
      msg = MIDI_msg_pack3(raw_data[0],raw_data[1],raw_data[2]);
    else if (len==2)
      msg = MIDI_msg_pack2(raw_data[0],raw_data[1]);
    else if (len==1)
      msg = MIDI_msg_pack1(raw_data[0]);
    else
      return;
    
    RT_PATCH_send_raw_midi_message(seqtrack, patch, msg, seq_time);
  }
}

int RT_MIDI_send_msg_to_patch(struct SeqTrack *seqtrack, struct Patch *patch, void *data, int data_size, int64_t seq_time){
  int num_bytes_used;

  R_ASSERT(data_size>0);
  
  {
    uint8_t *d=(uint8_t*)data;
    if (d[0] < 0x80) {
      RT_message("Illegal value in first byte of MIDI message: %d\n",d[0]);
      return 0;
    }
  }
  
  MidiMessage message(data, data_size, num_bytes_used, 0);
  
  RT_MIDI_send_msg_to_patch2(seqtrack, patch, message, seq_time);

  return num_bytes_used;
}


static void RT_process(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs){

  Data *data = (Data*)plugin->data;

#if 0
  for(int ch=0; ch<data->num_output_channels ; ch++)
    memset(outputs[ch], 0, sizeof(float)*num_frames);
  return;
#endif

  // 1. Process audio

  AudioPluginInstance *instance = data->audio_instance;
  AudioSampleBuffer &buffer = data->buffer;
  
  for(int ch=0; ch<data->num_input_channels ; ch++)
    memcpy(buffer.getWritePointer(ch), inputs[ch], sizeof(float)*num_frames);
    
  int pos = CRASHREPORTER_set_plugin_name(plugin->type->name);{
    instance->processBlock(buffer, data->midi_buffer);
  }CRASHREPORTER_unset_plugin_name(pos);
  
  for(int ch=0; ch<data->num_output_channels ; ch++)
    memcpy(outputs[ch], buffer.getReadPointer(ch), sizeof(float)*num_frames);

  
  // 2. Send out midi
  if (!data->midi_buffer.isEmpty()){

    volatile struct Patch *patch = plugin->patch;

    MidiBuffer::Iterator iterator(data->midi_buffer);
    
    RT_PLAYER_runner_lock();{
      MidiMessage message;
      int samplePosition;

      while(iterator.getNextEvent(message, samplePosition)){

#ifndef RELEASE
        if (samplePosition >= num_frames || samplePosition < 0)
          RT_message("The instrument named \"%s\" of type %s/%s\n"
                     "returned illegal sample position: %d",
                     patch==NULL?"<no name>":patch->name,
                     plugin->type->type_name, plugin->type->name,
                     samplePosition
                     );
#endif
        // Make sure samplePosition has a legal value
        if (samplePosition >= num_frames)
          samplePosition = num_frames-1;
        if (samplePosition < 0)
          samplePosition = 0;

        int len = message.getRawDataSize();
        
#ifndef RELEASE
        R_ASSERT(len > 0);
#endif
        
        if (len>=1 && len<=3) { // Filter out sysex messages.
          if (patch != NULL) {
            struct SeqTrack *seqtrack = RT_get_aux_seqtrack();
            
            int64_t delta_time = PLAYER_get_block_delta_time(seqtrack, seqtrack->start_time+samplePosition);
            
            int64_t radium_time = seqtrack->start_time + delta_time;
            
            RT_MIDI_send_msg_to_patch_receivers2(seqtrack, (struct Patch*)patch, message, radium_time);            
          }
        }
      }

    }RT_PLAYER_runner_unlock();

    data->midi_buffer.clear();
  }

}

static void play_note(struct SoundPlugin *plugin, int time, note_t note){
  Data *data = (Data*)plugin->data;
  MidiBuffer &buffer = data->midi_buffer;

  //printf("In. Play note %d %d\n",(int)note_num,(int)time);
  MidiMessage message(0x90 | note.midi_channel, (int)note.pitch, R_BOUNDARIES(0, (int)(note.velocity*127), 127));
  buffer.addEvent(message, time);
}

static void set_note_volume(struct SoundPlugin *plugin, int time, note_t note){
  Data *data = (Data*)plugin->data;
  MidiBuffer &buffer = data->midi_buffer;

  int velocity = R_BOUNDARIES(0,note.velocity,127);
  MidiMessage message(0xa0 | note.midi_channel, (int)note.pitch, velocity, 0.0);
  buffer.addEvent(message, time);
}

static void stop_note(struct SoundPlugin *plugin, int time, note_t note){
  Data *data = (Data*)plugin->data;
  MidiBuffer &buffer = data->midi_buffer;

  int ox90 = MIDI_get_use_0x90_for_note_off() ? 0x90 : 0x80;

  //printf("In. Stop note %d %d\n",(int)note_num,(int)time);
  MidiMessage message(ox90 | note.midi_channel, (int)note.pitch, 0, 0.0);
  buffer.addEvent(message, time);
}

static void send_raw_midi_message(struct SoundPlugin *plugin, int block_delta_time, uint32_t msg){
  uint8_t data[3];
  data[0] = MIDI_msg_byte1(msg);
  data[1] = MIDI_msg_byte2(msg);
  data[2] = MIDI_msg_byte3(msg);

  //printf("   Data: %x %x %x (%x)\n",(int)data[0],(int)data[1],(int)data[2], msg);
  
  int num_bytes_used;
  MidiMessage message(data, 3, num_bytes_used, 0);
  
  if (num_bytes_used>0) {
    Data *data = (Data*)plugin->data;
    MidiBuffer &buffer = data->midi_buffer;
    buffer.addEvent(message, block_delta_time);
  }
  
  //  else
  //  RError("Illegal midi msg: %x",msg); // Well, the illegal message could have been created by a third party plugin.
}

static int RT_get_latency(struct SoundPlugin *plugin){
  Data *data = (Data*)plugin->data;
  AudioPluginInstance *instance = data->audio_instance;
  int latency = instance->getLatencySamples();
  //printf("\n\n\n    Created new latency %d\n\n\n", latency);
  return latency;
}

static int RT_get_audio_tail_length(struct SoundPlugin *plugin){
  Data *data = (Data*)plugin->data;
  AudioPluginInstance *instance = data->audio_instance;

  if (is_vst2(plugin)){

    // Unfortunately, juce doesn't have a mechanism to tell whether AudioProcessor::getTailLengthSeconds() returns a valid value,
    // so we acces the VST plugin directly.

    AEffect *aeffect = (AEffect*)instance->getPlatformSpecificData();
    
    // "[return value]: tail size (for example the reverb time of a reverb plug-in); 0 is default (return 1 for 'no tail')"
    int tail = aeffect->dispatcher(aeffect, effGetTailSize,
                                   0, 0, 0, 0);
    
    if (tail==0)
      return -1; // i.e unable to report
    
    if (tail==1) // i.e. no tail
      return 0;

    return tail;
    
  } else {

    return ceil(instance->getTailLengthSeconds()*MIXER_get_sample_rate());
    
  }
}

static void set_effect_value(struct SoundPlugin *plugin, int time, int effect_num, float value, enum ValueFormat value_format, FX_when when){
  Data *data = (Data*)plugin->data;

  R_ASSERT(PLAYER_current_thread_has_lock());
  
  //if (effect_num==99)
  //  printf("   Juce_plugins.cpp:set_effect_value.   parm %d: %f\n",effect_num,value);

  if (is_vst2(plugin)){
    // juce::VSTPluginInstance::setParameter obtains the vst lock. That should not be necessary (Radium ensures that we are alone here), plus that it causes glitches in sound.
    // So instead, we call the vst setParameter function directly:
    AEffect *aeffect = (AEffect*)data->audio_instance->getPlatformSpecificData();
    
    aeffect->setParameter(aeffect, effect_num, value);
    
  } else {
    // This caused glitches with vst2 plugins, but there doesn't seem to be locks when using vst3 or au plugins.
    data->audio_instance->setParameter(effect_num, value);
  }
}

static float get_effect_value(struct SoundPlugin *plugin, int effect_num, enum ValueFormat value_format){
  radium::PlayerRecursiveLock lock;

  Data *data = (Data*)plugin->data;
  if (is_vst2(plugin)){
    // juce::VSTPluginInstance::getParameter obtains the vst lock. That should not be necessary (Radium ensures that we are alone here), plus that it causes glitches in sound.
    // So instead, we call the vst getParameter function directly:
    AEffect *aeffect = (AEffect*)data->audio_instance->getPlatformSpecificData();
    
    float val = aeffect->getParameter(aeffect, effect_num);
    
    return val;
  } else {
    // This caused glitches with vst2 plugins, but there doesn't seem to be locks when using vst3 or au plugins.
    return data->audio_instance->getParameter(effect_num);
  }
}

static void get_display_value_string(SoundPlugin *plugin, int effect_num, char *buffer, int buffersize){
  
#if CUSTOM_MM_THREAD
  const MessageManagerLock mmLock;
#endif
  
  Data *data = (Data*)plugin->data;
  
  String l = data->audio_instance->getParameterLabel(effect_num);
  const char *label = l.toRawUTF8();

  if (is_vst2(plugin)) // audio_instance->getParameterText() sometimes crashes. Doing it manually instead.
  {
    char disp[128] = {}; // c++ way of zero-initialization without getting missing-field-initializers warning.
    AEffect *aeffect = (AEffect*)data->audio_instance->getPlatformSpecificData();
    const int effGetParamDisplay = 7;
    aeffect->dispatcher(aeffect,effGetParamDisplay,
			effect_num, 0, (void *) disp, 0.0f);

    if (disp[0]==0){
      snprintf(buffer,buffersize-1,"%f%s",aeffect->getParameter(aeffect,effect_num),label);//plugin->type_data->params[effect_num].label);
    }else{
      snprintf(buffer,buffersize-1,"%s%s",disp,label);
    }
  }
  else {
    String l = data->audio_instance->getParameterText(effect_num, buffersize-1);
    snprintf(buffer,buffersize-1,"%s%s",l.toRawUTF8(),label);
  }
}

static bool gui_is_visible(struct SoundPlugin *plugin){
#if CUSTOM_MM_THREAD
  const MessageManagerLock mmLock;
#endif

  Data *data = (Data*)plugin->data;

  if (data->window==NULL)
    return false;
  else
    return data->window->isVisible();
}

radium::Mutex JUCE_show_hide_gui_lock;

static void show_gui(struct SoundPlugin *plugin){
#if CUSTOM_MM_THREAD
  const MessageManagerLock mmLock;
#endif

  Data *data = (Data*)plugin->data;

  if (data->window==NULL) {

    GL_lock();{

      radium::ScopedMutex lock(JUCE_show_hide_gui_lock);
      
      AudioProcessorEditor *editor = data->audio_instance->createEditor(); //IfNeeded();
      
      if (editor != NULL) {

        const char *title = V_strdup(plugin->patch==NULL ? talloc_format("%s %s",plugin->type->type_name, plugin->type->name) : plugin->patch->name);
        data->window = new PluginWindow(title, data, editor);

        g_num_visible_plugin_windows++;
        
      }

    }GL_unlock();

  }
  
}

//void OS_WINDOWS_move_main_window_to_front(void);

static void hide_gui(struct SoundPlugin *plugin){
#if CUSTOM_MM_THREAD
  const MessageManagerLock mmLock;
#endif
  
  Data *data = (Data*)plugin->data;

  g_num_visible_plugin_windows--;
  
  //data->window->setVisible(false);
  delete data->window; // NOTE: data->window is set to NULL in the window destructor. It's hairy, but there's probably not a better way.
  //OS_WINDOWS_move_main_window_to_front();
}


static AudioPluginInstance *create_audio_instance(const TypeData *type_data, float sample_rate, int block_size){
  
  static bool inited=false;

  static AudioPluginFormatManager formatManager;
    
  if (inited==false){
#if CUSTOM_MM_THREAD
    const MessageManagerLock mmLock;
#endif
    formatManager.addDefaultFormats();
    inited=true;
  }

  String errorMessage;

  const PluginDescription &description = type_data->description;

  printf("  Trying to load -%s-. Identifier: -%s-\n", STRING_get_chars(type_data->file_or_identifier),description.createIdentifierString().toRawUTF8());
  
  {
    radium::ScopedMutex lock(JUCE_show_hide_gui_lock);
    
    AudioPluginInstance *instance = formatManager.createPluginInstance(description, sample_rate, block_size, errorMessage);
    
    if (instance==NULL){
      GFX_addMessage("Unable to open %s plugin %s: %s\n",description.pluginFormatName.toRawUTF8(), description.fileOrIdentifier.toRawUTF8(), errorMessage.toRawUTF8());
      return NULL;
    }
    
    {
#if CUSTOM_MM_THREAD
      const MessageManagerLock mmLock;
#endif
      instance->prepareToPlay(sample_rate, block_size);
    }
  
    return instance;
  }
}


static void set_plugin_type_data(AudioPluginInstance *audio_instance, SoundPluginType *plugin_type){
  TypeData *type_data = (struct TypeData*)plugin_type->data;

  if (audio_instance->hasEditor()==false) {
    plugin_type->show_gui = NULL;
    plugin_type->hide_gui = NULL;
  }

  // The info in PluginDescription is probably fine, but override here just in case.
  plugin_type->num_inputs = audio_instance->getTotalNumInputChannels();
  plugin_type->num_outputs = audio_instance->getTotalNumOutputChannels();
    
  plugin_type->dont_send_effect_values_from_state_into_plugin = true; // Don't need to. Juce takes care of saving and loading all effect parameters (General optimization plus crash fix for buggy CM 505 plugin)

#if 0
  if (false && is_vst2(plugin_type)){
    AEffect *aeffect = (AEffect*)audio_instance->getPlatformSpecificData();
    {
      char vendor[1024] = {0};
      aeffect->dispatcher(aeffect, effGetVendorString, 0, 0, vendor, 0.0f);
      char product[1024] = {0};
      aeffect->dispatcher(aeffect, effGetProductString, 0, 0, product, 0.0f);

      if(strlen(vendor)>0 || strlen(product)>0)
        plugin_type->info = talloc_format("Vendor: %s\nProduct: %s\n",vendor,product);
    }
  }
#endif

  // Add some more info to the info string.
  plugin_type->info = V_strdup(talloc_format("%sAccepts MIDI: %s\nProduces MIDI: %s\n", plugin_type->info, audio_instance->acceptsMidi()?"Yes":"No", audio_instance->producesMidi()?"Yes":"No"));
        
  plugin_type->is_instrument = audio_instance->acceptsMidi(); // doesn't seem like this field ("is_instrument") is ever read...

  plugin_type->num_effects = audio_instance->getNumParameters();

  type_data->effect_names = (const char**)V_calloc(sizeof(char*),plugin_type->num_effects);
  for(int i = 0 ; i < plugin_type->num_effects ; i++)
    //  type_data->effect_names[i] = V_strdup(audio_instance->getParameterName(i).toRawUTF8());
    type_data->effect_names[i] = V_strdup(talloc_format("%d: %s",i,audio_instance->getParameterName(i).toRawUTF8()));
}


static void recreate_from_state(struct SoundPlugin *plugin, hash_t *state, bool is_loading){
#if CUSTOM_MM_THREAD
  const MessageManagerLock mmLock;
#endif

  Data *data = (Data*)plugin->data;
  
  AudioPluginInstance *audio_instance = data->audio_instance;

  TypeData *type_data = (struct TypeData*)plugin->type->data;

  bool is_compatible = true;

  if (HASH_has_key(state, "identifier_string")){
    String identifier_string = HASH_get_chars(state, "identifier_string");
    if (!type_data->description.matchesIdentifierString(identifier_string)){
      if (type_data->has_shown_noncompatible_warning == false){
        GFX_addMessage("Warning: Saved state is not compatible with \"%s\" / \"%s\".\n\nThe state was probably saved for a different plugin with the same name.", plugin->type->type_name, plugin->type->name);
        type_data->has_shown_noncompatible_warning = true;
      }
      is_compatible = false;
    }
  }

  if (is_compatible) {

    if (HASH_has_key(state, "audio_instance_state")) {
      const char *stateAsString = HASH_get_chars(state, "audio_instance_state");
      MemoryBlock sourceData;
      sourceData.fromBase64Encoding(stateAsString);
      audio_instance->setStateInformation(sourceData.getData(), sourceData.getSize());
    }
    
    
    if (HASH_has_key(state, "audio_instance_current_program")) {
      int current_program = HASH_get_int(state, "audio_instance_current_program");
      audio_instance->setCurrentProgram(current_program);
    }
    
    if (HASH_has_key(state, "audio_instance_program_state")){
      const char *programStateAsString = HASH_get_chars(state, "audio_instance_program_state");
      MemoryBlock sourceData;
      sourceData.fromBase64Encoding(programStateAsString);
      
      audio_instance->setCurrentProgramStateInformation(sourceData.getData(), sourceData.getSize());
    }
  }
  
  if (HASH_has_key(state, "x_pos"))
    data->x = HASH_get_int(state, "x_pos");
  
  if (HASH_has_key(state, "y_pos"))
    data->y = HASH_get_int(state, "y_pos");
}


static int num_running_plugins = 0;

static void *create_plugin_data(const SoundPluginType *plugin_type, SoundPlugin *plugin, hash_t *state, float sample_rate, int block_size, bool is_loading){
  
  TypeData *type_data = (struct TypeData*)plugin_type->data;

  if(isFullVersion()==false && num_running_plugins >= 2){
    GFX_Message(NULL,
                "Using more than 2 VST/VST3/AU plugins is only available to subscribers.<p>"
                "Subscribe <a href=\"http://users.notam02.no/~kjetism/radium/download.php\">here</a>.");
    return NULL;
  }

  AudioPluginInstance *audio_instance = create_audio_instance(type_data, sample_rate, block_size);
  if (audio_instance==NULL){
    return NULL;
  }

  {
#if CUSTOM_MM_THREAD
    const MessageManagerLock mmLock;
#endif

    PluginDescription description = audio_instance->getPluginDescription();

    //plugin->name = talloc_strdup(description.name.toUTF8());
    
    Data *data = new Data(audio_instance, plugin, audio_instance->getTotalNumInputChannels(), audio_instance->getTotalNumOutputChannels());
    plugin->data = data;
    
    audio_instance->setPlayHead(&data->playHead);
    
    if(type_data->effect_names==NULL)
      set_plugin_type_data(audio_instance,(SoundPluginType*)plugin_type); // 'plugin_type' was created here (by using calloc), so it can safely be casted into a non-const.
    
    if (state!=NULL)
      recreate_from_state(plugin, state, is_loading);
    
    num_running_plugins++;

    return data;
  }
}


static void create_state(struct SoundPlugin *plugin, hash_t *state){
#if CUSTOM_MM_THREAD
  const MessageManagerLock mmLock;
#endif
  
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

  HASH_put_int(state, "x_pos", data->x);
  HASH_put_int(state, "y_pos", data->y);

  TypeData *type_data = (struct TypeData*)plugin->type->data;

  HASH_put_chars(state, "identifier_string", type_data->description.createIdentifierString().toRawUTF8());
}


namespace{
  // Some plugins require that it takes some time between deleting the window and deleting the instance. If we don't do this, some plugins will crash. Only seen it on OSX though, but it doesn't hurt to do it on all platforms.
  struct DelayDeleteData : public Timer {
    Data *data;
    int downcount;
    
    DelayDeleteData(Data *data, int downcount = 10)
      : data(data)
      , downcount(downcount)
    {
      startTimer(500); // wait at least 0.5 seconds 10 times. (jules recommends a total of 1-2 seconds (https://forum.juce.com/t/crashes-when-close-app-mac-once-in-a-while/12902))
    }

    void timerCallback() override {
      if (downcount > 0) {
        
        fprintf(stderr, "    DelayDeleteData: Downcounting %d\n", downcount);
        downcount--;
        startTimer(500);
        
      } else {
        radium::ScopedMutex lock(JUCE_show_hide_gui_lock);
        
        fprintf(stderr, "    DelayDeleteData: Deleting.\n");
        delete data->audio_instance;
        delete data;
        delete this;
      }
    }
  };
}


static void cleanup_plugin_data(SoundPlugin *plugin){
#if CUSTOM_MM_THREAD
  const MessageManagerLock mmLock;
#endif

  num_running_plugins--;

  printf(">>>>>>>>>>>>>> Cleanup_plugin_data called for %p\n",plugin);
  Data *data = (Data*)plugin->data;

  if (data->window != NULL)
    delete data->window;

  data->plugin_will_be_deleted();

  // Wait a little bit first. (if CPU is VERY buzy, perhaps this waiting could prevent a crash)
#if 0
  // No, too much waiting when deleting several plugins at once. The 10*0.5s waiting scheme is probably good enough anyway.
  MessageManager::getInstance()->runDispatchLoopUntil(100);
#endif
  
  // Then schedule deletion in 5 seconds.
  new DelayDeleteData(data);
}

static const char *get_effect_name(struct SoundPlugin *plugin, int effect_num){
  TypeData *type_data = (struct TypeData*)plugin->type->data;
  return type_data->effect_names[effect_num];
}

/*
static const char *get_effect_description(const struct SoundPluginType *plugin_type, int effect_num){
  TypeData *type_data = (struct TypeData*)plugin_type->data;
  return type_data->effect_names[effect_num];
}
*/

static int get_num_presets(struct SoundPlugin *plugin){
#if CUSTOM_MM_THREAD
  const MessageManagerLock mmLock;
#endif

  Data *data = (Data*)plugin->data;

  //printf("   IS_VST3: %d,  IS_VST2: %d\n", data->is_vst3(), data->is_vst2());
  
  if (is_vst3(plugin)) // Must upgrade JUCE. With the current version, presets are not working with vst3.
    return 0;
  
  AudioPluginInstance *instance = data->audio_instance;

  return instance->getNumPrograms();
}

static int get_current_preset(struct SoundPlugin *plugin){
#if CUSTOM_MM_THREAD
  const MessageManagerLock mmLock;
#endif

  Data *data = (Data*)plugin->data;
  AudioPluginInstance *instance = data->audio_instance;

  return instance->getCurrentProgram();
}

static void set_current_preset(struct SoundPlugin *plugin, int num){
#if CUSTOM_MM_THREAD
  const MessageManagerLock mmLock;
#endif

  Data *data = (Data*)plugin->data;
  AudioPluginInstance *instance = data->audio_instance;

  instance->setCurrentProgram(num);
}

static const char *get_preset_name(struct SoundPlugin *plugin, int num){
#if CUSTOM_MM_THREAD
  const MessageManagerLock mmLock;
#endif

  Data *data = (Data*)plugin->data;
  AudioPluginInstance *instance = data->audio_instance;

  return talloc_strdup(instance->getProgramName(num).toRawUTF8());
}

static void set_preset_name(struct SoundPlugin *plugin, int num, const char* new_name){
#if CUSTOM_MM_THREAD
  const MessageManagerLock mmLock;
#endif

  Data *data = (Data*)plugin->data;
  AudioPluginInstance *instance = data->audio_instance;

  instance->changeProgramName(num, new_name);
}

static void set_non_realtime(struct SoundPlugin *plugin, bool is_non_realtime){
#if CUSTOM_MM_THREAD
  const MessageManagerLock mmLock;
#endif

  Data *data = (Data*)plugin->data;
  AudioPluginInstance *instance = data->audio_instance;

  instance->setNonRealtime(is_non_realtime);
}

static SoundPluginType *create_plugin_type(const PluginDescription &description, const wchar_t *file_or_identifier, SoundPluginTypeContainer *container){ //, const wchar_t *library_file_full_path){
  printf("b02 %s\n",STRING_get_chars(file_or_identifier));
  fflush(stdout);
  //  return;

  SoundPluginType *plugin_type = (SoundPluginType*)V_calloc(1,sizeof(SoundPluginType));

  TypeData *typeData = new TypeData(wcsdup(file_or_identifier),
                                    description
                                    );
  
  plugin_type->data = typeData;

  plugin_type->type_name
    = description.pluginFormatName=="VST" ? "VST"
    : description.pluginFormatName=="VST3" ? "VST3"
    : description.pluginFormatName=="AudioUnit" ? "AU"
    : NULL;

  if (plugin_type->type_name==NULL){
    RError("Unknown type %s", description.pluginFormatName.toRawUTF8());
    plugin_type->type_name = "Unknown";
  }

  plugin_type->name      = V_strdup(description.name.toRawUTF8());

  plugin_type->info = V_strdup(talloc_format("%s\n\nVersion: %s\nVendor: %s\nCategory: %s\nInstrument: %s\n",
                                             description.descriptiveName.toRawUTF8(),
                                             description.version.toRawUTF8(),                                 
                                             description.manufacturerName.toRawUTF8(),
                                             description.category.toRawUTF8(),
                                             description.isInstrument ? "Yes" : "No"
                                             )
                               );
  
  plugin_type->container = container;

  plugin_type->num_inputs = description.numInputChannels;
  plugin_type->num_outputs = description.numOutputChannels;
  plugin_type->category = V_strdup(description.category.toRawUTF8());
  plugin_type->creator = V_strdup(description.manufacturerName.toRawUTF8());

  plugin_type->is_instrument = true; // we don't know yet, so we set it to true.
  
  plugin_type->buffer_size_is_changed = buffer_size_is_changed;

  plugin_type->RT_process = RT_process;
  plugin_type->create_plugin_data = create_plugin_data;
  plugin_type->cleanup_plugin_data = cleanup_plugin_data;

  plugin_type->gui_is_visible = gui_is_visible;
  plugin_type->show_gui = show_gui;
  plugin_type->hide_gui = hide_gui;
  
  plugin_type->play_note       = play_note;
  plugin_type->set_note_volume = set_note_volume;
  plugin_type->stop_note       = stop_note;
  plugin_type->send_raw_midi_message = send_raw_midi_message;

  plugin_type->RT_get_latency = RT_get_latency;
  plugin_type->RT_get_audio_tail_length = RT_get_audio_tail_length;
  
  plugin_type->get_effect_value = get_effect_value;
  plugin_type->set_effect_value = set_effect_value;
  
  plugin_type->get_display_value_string=get_display_value_string;
  plugin_type->get_effect_name=get_effect_name;
  //plugin_type->get_effect_description=get_effect_description;

  plugin_type->create_state = create_state;
  plugin_type->recreate_from_state = recreate_from_state;

  plugin_type->get_num_presets = get_num_presets;
  plugin_type->get_current_preset = get_current_preset;
  plugin_type->set_current_preset = set_current_preset;
  plugin_type->get_preset_name = get_preset_name;
  plugin_type->set_preset_name = set_preset_name;

  plugin_type->set_non_realtime = set_non_realtime;
  
  PR_add_plugin_type_no_menu(plugin_type);
  
  return plugin_type;
}



static String get_container_descriptions_filename(const wchar_t *container_filename){
  String encoded(STRING_get_sha1(container_filename));
  return String(OS_get_dot_radium_path(true)) + OS_get_directory_separator() + SCANNED_PLUGINS_DIRNAME + OS_get_directory_separator() + "v2_PluginDescription_" + encoded;
}


static enum PopulateContainerResult launch_program_calling_write_container_descriptions_to_cache_on_disk(const wchar_t *container_filename){
#if FOR_WINDOWS
  String executable = String(OS_get_full_program_file_path(STRING_create("radium_plugin_scanner.exe")));
#else
  String executable = String(OS_get_full_program_file_path(STRING_create("radium_plugin_scanner")));
#endif
  
  StringArray args;
  args.add(executable);
  args.add(Base64::toBase64(String(container_filename)));
  args.add(Base64::toBase64(get_container_descriptions_filename(container_filename)));

  ChildProcess process;

  if (process.start(args)==false){
    GFX_Message2(NULL, true, "Error: Unable to launch %s", executable.toRawUTF8());
    return POPULATE_RESULT_OTHER_ERROR;
  }

  int s = 3;
  
  while(process.waitForProcessToFinish(s*1000)==false){
    vector_t v = {};
    int w3 = VECTOR_push_back(&v, "Wait 3 seconds more");
    int w20 = VECTOR_push_back(&v, "Wait 20 seconds more");
    VECTOR_push_back(&v, "Cancel");
    int blacklist = VECTOR_push_back(&v, "Cancel, and add to blacklist");
    fprintf(stderr, "Openinig requester\n");
    int ret = GFX_Message2(&v, true, "Waited more than %d seconds for \"%s\" to load\n", s, STRING_get_chars(container_filename));
    fprintf(stderr, "Got answer from requester\n");
    if (ret==w3)
      s = 3;
    else if (ret==w20)
      s = 20;
    else {
      process.kill();
      if(ret==blacklist)
        return POPULATE_RESULT_PLUGIN_MUST_BE_BLACKLISTED;
      else
        return POPULATE_RESULT_OTHER_ERROR;
    }
  }

  return POPULATE_RESULT_IS_OKAY; // as far as we know. (ChildProcess doesn't have an hasCrashed() function).
}

static enum PopulateContainerResult get_container_descriptions_from_disk(const SoundPluginTypeContainer *container, OwnedArray<PluginDescription> &descriptions){
  String filename = get_container_descriptions_filename(container->filename);
  
  fprintf(stderr, "   READING.  %s: Reading from description file \"%s\".\n", String(container->filename).toRawUTF8(), filename.toRawUTF8());
    
  File file(filename);
  if (file.existsAsFile()==false){
    //GFX_Message2(NULL, true, "Error. Could not find file \"%s\"\n", filename.toRawUTF8());
    //return POPULATE_RESULT_OTHER_ERROR;
    return POPULATE_RESULT_PLUGIN_MUST_BE_BLACKLISTED; // Likely to have been caused by the plugin crashing
  }

  XmlElement *xml = XmlDocument::parse(file);
  if (xml==NULL){
    //GFX_Message2(NULL, true, "Error. Unable to parse xml file \"%s\". You might want to delete this file and try again.\n", filename.toRawUTF8());
    //return POPULATE_RESULT_OTHER_ERROR;
    return POPULATE_RESULT_PLUGIN_MUST_BE_BLACKLISTED; // Likely to have been caused by the plugin crashing
  }

  int size = xml->getNumChildElements();
    
  for(int i = 0 ; i < size ; i++){
    XmlElement *xml_description = xml->getChildElement(i);
    PluginDescription description;
    if (description.loadFromXml(*xml_description)==false){
      /*
      GFX_Message2(NULL, true, "Error: Unable to create description %d for %s from xml file \"%s\". You might want to delete this file and try again.\n",
                   i,
                   String(container->filename).toRawUTF8(),
                   filename.toRawUTF8());
      return POPULATE_RESULT_OTHER_ERROR;
      */
      return POPULATE_RESULT_PLUGIN_MUST_BE_BLACKLISTED; // Likely to have been caused by the plugin crashing
    }
    descriptions.add(new PluginDescription(description));
  }

  return POPULATE_RESULT_IS_OKAY;
}

static enum PopulateContainerResult get_container_descriptions(SoundPluginTypeContainer *container, OwnedArray<PluginDescription> &descriptions){
#if 0
  if (!container_descriptions_are_cached_on_disk(container))
    if (write_container_descriptions_to_cache_on_disk(container)==false){
      fprintf(stderr, "Couldn't cache container descriptions\n");
      return true;
    }

  R_ASSERT_RETURN_IF_FALSE2(container_descriptions_are_cached_on_disk(container), false);
#else

  String filename = get_container_descriptions_filename(container->filename);
  File file(filename);
  if (file.exists()==true){
    if (file.deleteFile()==false){
      GFX_Message2(NULL, true, "Error. Unable to delete file \"%s\"\n", filename.toRawUTF8());
      return POPULATE_RESULT_OTHER_ERROR;
    }
  }

  enum PopulateContainerResult result = launch_program_calling_write_container_descriptions_to_cache_on_disk(container->filename);
  if (result != POPULATE_RESULT_IS_OKAY)
    return result;
  
#endif
  
  return get_container_descriptions_from_disk(container, descriptions);
}

static enum PopulateContainerResult populate(SoundPluginTypeContainer *container){
  R_ASSERT_RETURN_IF_FALSE2(container->is_populated==false, POPULATE_RESULT_OTHER_ERROR);

  container->is_populated = true;
  container->num_types = 0;

  ContainerData *data = (ContainerData*)container->data;

  OwnedArray<PluginDescription> descriptions;

  
#if 0
  
#if 0 //FOR_MACOSX
  DirectoryIterator iter(File(data->library_file_full_path), false, "*", File::findFiles);
  while (iter.next()) {
    printf("Checking -%s- (%s)\n",iter.getFile().getFullPathName().toRawUTF8(), STRING_get_chars(data->library_file_full_path));
    add_descriptions_from_plugin_file(descriptions, iter.getFile().getFullPathName());
  }
#else
  add_descriptions_from_plugin_file(descriptions, container->filename);
#endif

#else
  
  enum PopulateContainerResult populate_result = get_container_descriptions(container, descriptions);
  if (populate_result != POPULATE_RESULT_IS_OKAY)
    return populate_result;
  
#endif
  
  int size = descriptions.size();

  container->num_types = size;
  container->plugin_types = (SoundPluginType**)V_calloc(size, sizeof(SoundPluginType));

  int i = 0;
  for (auto description : descriptions){
    container->plugin_types[i] = create_plugin_type(*description, data->file_or_identifier, container);
    i++;
  }

  if (size==0)
    GFX_addMessage("No valid plugins found in %s", STRING_get_chars(container->filename));

  return POPULATE_RESULT_IS_OKAY;
}

#if 0
static void populate_old(SoundPluginTypeContainer *container){
  if (container->is_populated)
    return;

  container->is_populated = true;

  ContainerData *data = (ContainerData*)container->data;

  vector_t uids = VST_get_uids(container->filename);

  int size = uids.num_elements;

  if (size==0)
    return;

  container->num_types = size;
  container->plugin_types = (SoundPluginType**)V_calloc(size, sizeof(SoundPluginType));

  for(int i = 0 ; i < size ; i++){
    radium_vst_uids_t *element = (radium_vst_uids_t*)uids.elements[i];
    const char *name = element->name;
    if (name==NULL)
      name = container->name;
    //fprintf(stderr,"i: %d, %d, name: %s, containername: %s\n",i,name==NULL,name,container->name);fflush(stderr);
    container->plugin_types[i] = create_plugin_type(name, element->uid, data->file_or_identifier, container);
  }

  fflush(stderr);  
}
#endif

void add_juce_plugin_type(const char *name, const wchar_t *file_or_identifier, const wchar_t *library_file_full_path, const char *container_type_name){
  SoundPluginTypeContainer *container = (SoundPluginTypeContainer*)V_calloc(1,sizeof(SoundPluginTypeContainer));

  container->type_name = V_strdup(container_type_name);
  container->name = V_strdup(name);
  container->populate = populate;
  container->filename = wcsdup(library_file_full_path);
  
  ContainerData *data = (ContainerData*)V_calloc(1, sizeof(ContainerData));
  data->file_or_identifier = wcsdup(file_or_identifier);

  container->data = data;
  
  PR_add_plugin_container(container);
}
 

#if 0 // Only works in linux.

namespace{
  struct ProgressWindow  : public DocumentWindow {
    ThreadWithProgressWindow *progress_bar = NULL;
    //ProgressBar *progress_bar = NULL;
    double progress_progress = 0.0;
    
    ProgressWindow(const char *title)
      : DocumentWindow (title,
                        Colours::lightgrey,
                        DocumentWindow::allButtons,
                        true)
    {      
      progress_bar = new ThreadWithProgressWindow(title, true, false);
      //progress_bar = new ProgressBar(progress_progress);
      progress_bar->setSize(600,20);

      //progress_bar->setLookAndFeel(new LookAndFeel_V2);
        
      //addAndMakeVisible(progress_bar);
        
      //this->setAlwaysOnTop(true);
      //this->setSize (600, 20);
      this->setUsingNativeTitleBar(true);
      
      this->setContentOwned(progress_bar, true);

      this->centreWithSize (getWidth(), getHeight());
      
      this->setVisible(true);

      progress_bar->runThread();
    }
  };
}

static ProgressWindow *g_progress_window;

void GFX_OpenProgress(const char *message){
#if CUSTOM_MM_THREAD
  const MessageManagerLock mmLock;
#endif

  delete g_progress_window;

  g_progress_window = new ProgressWindow(message);
}

void GFX_ShowProgressMessage(const char *message){
  if (g_progress_window == NULL)
    GFX_OpenProgress("...");

  {
#if CUSTOM_MM_THREAD
    const MessageManagerLock mmLock;
#endif
    //g_progress_window->progress_bar->setTextToDisplay(message);
    g_progress_window->progress_bar->setStatusMessage(message);
    g_progress_window->progress_bar->setProgress(g_progress_window->progress_progress);
    g_progress_window->progress_progress += 0.1;
    //if(g_progress_progress > 1.0)
    //  g_progress_progress = 0.0;
  }
}

void GFX_CloseProgress(void){
#if CUSTOM_MM_THREAD
    const MessageManagerLock mmLock;
#endif
  delete g_progress_window;
  g_progress_window = NULL;
}

#endif

void *JUCE_lock(void){
  return new MessageManagerLock;
}

void JUCE_unlock(void *lock){
  MessageManagerLock *mmLock = static_cast<MessageManagerLock *>(lock);
  delete mmLock;
}

bool JUCE_native_gui_grabs_keyboard(void){
  return g_num_visible_plugin_windows > 0 && g_vst_grab_keyboard;
}

char *JUCE_download(const char *url_url){
  URL url(url_url);
  
  String text =
    url
    //    .withParameter("C", "M")
    //  .withParameter("O", "D")
    .readEntireTextStream();

  return strdup(text.toRawUTF8());
}

static String g_backtrace;

const char *JUCE_get_backtrace(void){
  g_backtrace = SystemStats::getStackBacktrace();
  return g_backtrace.toUTF8();
}

void JUCE_get_min_max_val(const float *array, const int num_elements, float *min_val, float *max_val){
  auto both = FloatVectorOperations::findMinAndMax(array,num_elements);
  *min_val = both.getStart();
  *max_val = both.getEnd();
}

float JUCE_get_max_val(const float *array, const int num_elements){
  float a;
  float b;
  JUCE_get_min_max_val(array, num_elements, &a, &b);
  return R_MAX(-a,b);
}


#define TEST_GET_MAX_VAL 0

#if TEST_GET_MAX_VAL

static float RT_get_max_val(const float *array, const int num_elements){
  float ret=0.0f;
  float minus_ret = 0.0f;
  
  for(int i=0;i<num_elements;i++){
    float val = array[i];
    if(val>ret){
      ret=val;
      minus_ret = -val;
    }else if (val<minus_ret){
      ret = -val;
      minus_ret = val;
    }
  }

  return ret;
}

// Found here: http://codereview.stackexchange.com/questions/5143/min-max-function-of-1d-array-in-c-c/
// performance is approx. the same as FloatVectorOperations::findMinAndMax(array,num_elements);
static void x86_sse_find_peaks(const float *buf, unsigned nframes, float *min, float *max)
{
    __m128 current_max, current_min, work;

    // Load max and min values into all four slots of the XMM registers
    current_min = _mm_set1_ps(*min);
    current_max = _mm_set1_ps(*max);

    // Work input until "buf" reaches 16 byte alignment
    while ( ((unsigned long)buf) % 16 != 0 && nframes > 0) {

        // Load the next float into the work buffer
        work = _mm_set1_ps(*buf);

        current_min = _mm_min_ps(current_min, work);
        current_max = _mm_max_ps(current_max, work);

        buf++;
        nframes--;
    }

    // use 64 byte prefetch for quadruple quads
    while (nframes >= 16) {
        //__builtin_prefetch(buf+64,0,0); // for GCC 4.3.2+

        work = _mm_load_ps(buf);
        current_min = _mm_min_ps(current_min, work);
        current_max = _mm_max_ps(current_max, work);
        buf+=4;
        work = _mm_load_ps(buf);
        current_min = _mm_min_ps(current_min, work);
        current_max = _mm_max_ps(current_max, work);
        buf+=4;
        work = _mm_load_ps(buf);
        current_min = _mm_min_ps(current_min, work);
        current_max = _mm_max_ps(current_max, work);
        buf+=4;
        work = _mm_load_ps(buf);
        current_min = _mm_min_ps(current_min, work);
        current_max = _mm_max_ps(current_max, work);
        buf+=4;
        nframes-=16;
    }

    // work through aligned buffers
    while (nframes >= 4) {

        work = _mm_load_ps(buf);

        current_min = _mm_min_ps(current_min, work);
        current_max = _mm_max_ps(current_max, work);

        buf+=4;
        nframes-=4;
    }

    // work through the rest < 4 samples
    while ( nframes > 0) {

        // Load the next float into the work buffer
        work = _mm_set1_ps(*buf);

        current_min = _mm_min_ps(current_min, work);
        current_max = _mm_max_ps(current_max, work);

        buf++;
        nframes--;
    }

    // Find min & max value in current_max through shuffle tricks

    work = current_min;
    work = _mm_shuffle_ps(work, work, _MM_SHUFFLE(2, 3, 0, 1));
    work = _mm_min_ps (work, current_min);
    current_min = work;
    work = _mm_shuffle_ps(work, work, _MM_SHUFFLE(1, 0, 3, 2));
    work = _mm_min_ps (work, current_min);

    _mm_store_ss(min, work);

    work = current_max;
    work = _mm_shuffle_ps(work, work, _MM_SHUFFLE(2, 3, 0, 1));
    work = _mm_max_ps (work, current_max);
    current_max = work;
    work = _mm_shuffle_ps(work, work, _MM_SHUFFLE(1, 0, 3, 2));
    work = _mm_max_ps (work, current_max);

    _mm_store_ss(max, work);
}

static float RT_get_max_val3(const float *array, const int num_elements){
  float a;
  float b;
  x86_sse_find_peaks(array, num_elements, &a, &b);
  return R_MAX(-a,b);
}

extern double monotonic_seconds();

static void testing(void){
  const int num_iterations = 1024*512;
  float test_data[64];

  // test run
  for(int i=0;i<50;i++){
    RT_get_max_val(test_data,64);
    RT_get_max_val2(test_data,64);
    RT_get_max_val3(test_data,64);
  }
  
  double result = 0.0f;
  
  double start_time = monotonic_seconds();
  
  for(int i=0;i<num_iterations;i++){
    result +=
#if 0
      RT_get_max_val(test_data,64);
#else
    RT_get_max_val2(test_data,64);
    //FloatVectorOperations::findMinAndMax(test_data,64).getStart();
    //FloatVectorOperations::findMinimum(test_data,64);
#endif
  }
  
  double end_time = monotonic_seconds();
  double duration = end_time - start_time;

  float a = RT_get_max_val(test_data,64);
  float b = RT_get_max_val2(test_data,64);

  fprintf(stderr,"time: %f (%f), %f %f\n",duration,result,a,b);fflush(stderr);  
}
#endif

void PLUGINHOST_load_fxbp(SoundPlugin *plugin, wchar_t *wfilename){
  Data *data = (Data*)plugin->data;
  AudioPluginInstance *instance = data->audio_instance;

  String filename(wfilename);
  File file(filename);

  MemoryBlock memoryBlock;
  
  bool success = file.loadFileAsData(memoryBlock);
  if (success==false){
    GFX_Message2(NULL, true, "Unable to load %s", STRING_get_chars(wfilename));
    return;
  }
      
  success = VSTPluginFormat::loadFromFXBFile(instance, memoryBlock.getData(), memoryBlock.getSize());
  if (success==false){
    GFX_Message2(NULL, true, "Could not use %s for this plugin", STRING_get_chars(wfilename));
    return;
  }
  
  printf("************** size: %d\n",(int)memoryBlock.getSize());
}
  
static void save_fxbp(SoundPlugin *plugin, wchar_t *wfilename, bool is_fxb){
  Data *data = (Data*)plugin->data;
  AudioPluginInstance *instance = data->audio_instance;

  MemoryBlock memoryBlock;
  bool result = VSTPluginFormat::saveToFXBFile(instance, memoryBlock, is_fxb);
  if (result==false){
    GFX_Message2(NULL, true, "Unable to create FXB/FXP data for this plugin");
    return;
  }
  
  String filename(wfilename);

  File file(filename);

  Result result2 = file.create();

  if (result2.failed()){
    GFX_Message2(NULL, true, "Unable to create file %s (%s)", STRING_get_chars(wfilename), result2.getErrorMessage().toRawUTF8());
    return;
  }
  
  bool result3 = file.replaceWithData(memoryBlock.getData(), memoryBlock.getSize());
  if (result3==false){
    GFX_Message2(NULL, true, "Unable to write data to file %s (disk full?)", STRING_get_chars(wfilename));
    return;
  }
  
  
  printf("\n\n\n ***************** result: %d\n\n\n\n",result);
}

void PLUGINHOST_save_fxb(SoundPlugin *plugin, wchar_t *filename){
  save_fxbp(plugin, filename, true);
}
  
void PLUGINHOST_save_fxp(SoundPlugin *plugin, wchar_t *filename){
  save_fxbp(plugin, filename, false);
}

void PLUGINHOST_init(void){
#if TEST_GET_MAX_VAL
  testing();
  exit(0);
#endif

#if 0
  
  // Provoce a very hard crash inside juce:
  
  fprintf(stderr,"init1\n");
  const char crash[] = {3,1,1,1,1,-128,-128,-128,-128,16,1,1,1,1,1,77,26,52,
                        111,4,77,26,52,111,4,1,1,1,1,1,1,0};
  String(CharPointer_UTF8(crash));
  fprintf(stderr,"init2\n");
  
#endif
  
#if CUSTOM_MM_THREAD // Seems like a separate thread is only necessary on linux.

  JuceThread *juce_thread = new JuceThread;
  juce_thread->startThread();

  while(juce_thread->isInited.get()==0)
    Thread::sleep(20);
  
#else

  initialiseJuce_GUI();

  //Font::setDefaultSansSerifFont 


#endif
}

#endif // defined(COMPILING_JUCE_PLUGINS_O)

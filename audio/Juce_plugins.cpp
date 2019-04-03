/* Copyright 2014 Kjetil S. Matheussen

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

/*
  A Juce plugin SoundPlugin.

  Handles VST and AU plugins/instruments.

  The old VST Plugin system is still available (audio/VSTPlugins.cpp), and is used when loading old songs.

  New songs will use this system instead for vst plugins.
*/

#if defined(COMPILING_JUCE_PLUGINS_O)



#include <math.h>
#include <string.h>

#include <map>

#define DONT_SET_USING_JUCE_NAMESPACE 1
#define JUCE_DONT_DECLARE_PROJECTINFO 1

#include "../pluginhost/JuceLibraryCode/AppConfig.h"

#include "../pluginhost/JuceLibraryCode/JuceHeader.h"


#define USE_EMBEDDED_NATIVE_WINDOW 0

#if JUCE_LINUX
#define CHANGE_GUI_VISIBILITY_INSTEAD_OF_REOPENING 0 // Seems to be some bugs in juce when opening vst guis.
#else
#define CHANGE_GUI_VISIBILITY_INSTEAD_OF_REOPENING 1 // There was a good reason this wasn't enabled earlier, but I don't remember that reason. Seems to work now though.
#endif

#define TRY_TO_RESIZE_EDITOR 0 // This doesn't seem to work, except for the juce demo plugin. Instead change window size when plugin gui size changes (and not the other way).
    

  

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
#include "../common/threading.h"
#include "../common/sequencer_timing_proc.h"

#include "../Qt/Qt_instruments_proc.h"

#include "../crashreporter/crashreporter_proc.h"

#include "../OpenGL/Widget_proc.h"

#include "../midi/midi_instrument_proc.h"

#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"
#include "SoundProducer_proc.h"

#include "SoundPluginRegistry_proc.h"

#include "../api/api_proc.h"


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
#include "../api/api_gui_proc.h"


#include "Juce_plugins_proc.h"

//#undef Slider



#include "../midi/midi_juce.cpp"
#include "Smooth.cpp"




static int g_num_visible_plugin_windows = 0;
static bool g_vst_grab_keyboard = true;

static int RT_get_latency(const struct SoundPlugin *plugin);


namespace{

#if FOR_LINUX
  static const bool g_use_custom_mm_thread = true;
#elif FOR_WINDOWS  
  static const bool g_use_custom_mm_thread = false;
#elif FOR_MACOSX
  static const bool g_use_custom_mm_thread = false;
#else
  #error "unknown platform"
#endif
  
  class MMLock{
    juce::MessageManager::Lock mmLock;

  public:
    MMLock(void)
    {
      if (g_use_custom_mm_thread){
        mmLock.enter();
      }
    }

    ~MMLock(){
      if (g_use_custom_mm_thread)
        mmLock.exit();
    }    
  };

  
  void *run_callback(void *arg){
    std::function<void(void)> *callback = static_cast<std::function<void(void)> *>(arg);
    (*callback)();
    return NULL;
  }
  
  void run_on_message_thread(std::function<void(void)> callback){
    if (THREADING_is_main_thread())
      juce::MessageManager::getInstance()->callFunctionOnMessageThread(run_callback, &callback); // Note that 'callFunctionOnMessageThread' checks if this is the message thread and calls directly if so.
    else
      R_ASSERT(false); // Calling callFunctionOnMessageThread on a player thread can cause deadlock.
  }
  
  /*
  static bool is_au(const struct SoundPluginType *type) {
    return !strcmp(type->type_name, "AU");
  }
  
  static bool is_au(const struct SoundPlugin *plugin) {
    return is_au(plugin->type);
  }
  */

  static bool is_vst2(const struct SoundPluginType *type) {
    return !strcmp(type->type_name, "VST");
  }
  
  static bool is_vst2(const SoundPlugin *plugin) {
    return is_vst2(plugin->type);
  }

  static bool is_vst3(const SoundPluginType *type){
    return !strcmp(type->type_name, "VST3");
  }

  static bool is_vst3(const SoundPlugin *plugin) {
    return is_vst3(plugin->type);
  }

  /*
  static bool is_vst(const struct SoundPluginType *type) {
    return is_vst2(type) || is_vst3(type);
  }
  */
  
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
  

  struct PluginWindow;

  struct Listener : public juce::AudioProcessorListener {

    SoundPlugin *_plugin; // Is never NULL. Listener is removed before plugin is deleted.

    Listener(SoundPlugin *plugin) : _plugin(plugin) {}
    
    // Receives a callback when a parameter is changed.
    void 	audioProcessorParameterChanged (juce::AudioProcessor *processor, int parameterIndex, float newValue) override {

#if !defined(RELEASE)
      printf("   JUCE listener: parm %d changed to %f. has_inited: %d. is_shutting_down: %d\n",parameterIndex, newValue, ATOMIC_GET(_plugin->MT_has_initialized), ATOMIC_GET(_plugin->is_shutting_down));
#endif

      if (ATOMIC_GET(_plugin->MT_has_initialized) && !ATOMIC_GET(_plugin->is_shutting_down))
        PLUGIN_call_me_when_an_effect_value_has_changed(_plugin,
                                                        parameterIndex,
                                                        newValue, // native
                                                        newValue, // scaled
                                                        true // make undo
                                                        );
    }
 
    // Called to indicate that something else in the plugin has changed, like its program, number of parameters, etc.
    void 	audioProcessorChanged (juce::AudioProcessor *processor) override {
#if !defined(RELEASE)
      printf("    JUCE listener: audioProcessorChanged...\n");
#endif
      volatile struct Patch *patch = _plugin->patch;
      if (patch != NULL)
        ATOMIC_SET(patch->widget_needs_to_be_updated, true);
    }
 

    //Indicates that a parameter change gesture has started.
    void 	audioProcessorParameterChangeGestureBegin (juce::AudioProcessor *processor, int parameterIndex) override {
#if !defined(RELEASE)
      printf("    JUCE listener: gesture starts for %d\n",parameterIndex);
#endif
    }

    
    //Indicates that a parameter change gesture has finished. 
    void 	audioProcessorParameterChangeGestureEnd (juce::AudioProcessor *processor, int parameterIndex) override {
#if !defined(RELEASE)
      printf("    JUCE listener: gesture ends for %d\n",parameterIndex);
#endif
    }
  };

  
  struct MyAudioPlayHead : public juce::AudioPlayHead{

    juce::String _plugin_name;
    struct SoundPlugin *_plugin;  // Set to NULL when SoundPlugin *plugin is deleted. (we can still be alive though, since the deletion of juce plugins are delayed)
    
    double positionOfLastLastBarStart = 0.0;
    bool positionOfLastLastBarStart_is_valid = false;

    double lastLastBarStart = 0.0; // only used for debugging.
        

    MyAudioPlayHead(juce::String plugin_name, struct SoundPlugin *plugin)
      : _plugin_name(plugin_name)
      , _plugin(plugin)
    {}

    void plugin_will_be_deleted(void){
      _plugin = NULL;
    }
    
    // From JUCE documenation: You can ONLY call this from your processBlock() method!
    // I.e. it will only be called from the player thread or a multicore thread.
    bool getCurrentPosition (CurrentPositionInfo &result) override {
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

      bool isplaying = is_really_playing();

      const struct SeqTrack *seqtrack;

      if (pc->playtype==PLAYBLOCK)
        seqtrack = root->song->block_seqtrack;
      else
        seqtrack = (struct SeqTrack *)root->song->seqtracks.elements[0]; // FIX.


      result.bpm = RT_LPB_get_current_BPM(seqtrack);
      //printf("result.bpm: %f\n",result.bpm);

      if (result.bpm==0){
        //R_ASSERT_NON_RELEASE(false); // Note: BPM is supposed to b 0 when playing very slowly so it's not impossible to get a false positive.
        result.bpm = 1; // Never set bpm to 0. At least one vst plugin crashes if bpm is 0.
      }
      
      StaticRatio signature = RT_Signature_get_current_Signature(seqtrack);
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

        bool using_sequencer_timing = root->song->use_sequencer_tempos_and_signatures;

        // Note: If changing ppqPositionOfLastBarStart, we might also have to change pos->bar_start_tick in Mixer.cpp/RT_rjack_timebase. (note that we dsubtract "latency_beats" here.)
        result.ppqPosition               = RT_LPB_get_beat_position(seqtrack) - latency_beats;
        result.ppqPositionOfLastBarStart = (using_sequencer_timing ? g_rt_sequencer_ppq_of_last_bar_start-latency_beats : seqtrack->beat_iterator.beat_position_of_last_bar_start);
        
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

  struct Data{
    juce::AudioPluginInstance *audio_instance;

    juce::MidiKeyboardState keyboardState;
    
    SoundPlugin *_plugin;
    
    DEFINE_ATOMIC(PluginWindow *, window) = NULL;

    MyAudioPlayHead playHead;

    juce::MidiBuffer midi_buffer;
    juce::AudioSampleBuffer buffer;

    Listener listener;

    int num_input_channels;
    int num_output_channels;

    /*
    int x;
    int y;
    */
    std::map<int64_t, int> xs; // key is parentgui. (Juce::HashMap has a better API, but it didn't compile with gcc7.)
    std::map<int64_t, int> ys; // key is parentgui. (Juce::HashMap has a better API, but it didn't compile with gcc7.)
    
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
    Data(juce::AudioPluginInstance *audio_instance, SoundPlugin *plugin, int num_input_channels, int num_output_channels)
      : audio_instance(audio_instance)
      , _plugin(plugin)
      , playHead(audio_instance->getPluginDescription().name, plugin)
      , buffer(R_MAX(num_input_channels, num_output_channels), RADIUM_BLOCKSIZE)
      , listener(plugin)
      , num_input_channels(num_input_channels)
      , num_output_channels(num_output_channels)
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
    juce::PluginDescription description;
    const char **effect_names = NULL; // set the first time the plugin is loaded
    bool has_shown_noncompatible_warning = false; // TODO: recreate_from_state is called twice when loading preset.
    TypeData(const wchar_t *file_or_identifier,
             juce::PluginDescription description
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

  struct ToolTipMouseListener : public juce::MouseListener {
    void mouseMove (const juce::MouseEvent &event){
      gui_toolTip("Make plugin take care of scaling the user interface itself.\n"
                  "\n"
                  "Disabling this option can fix strange graphics or strange\n"
                  "mouse behavior. The graphics will also be blury."
                  );
    }
    
  };
  
  struct PluginWindow  : public juce::DocumentWindow, juce::Button::Listener, juce::Timer, juce::ComponentListener {
    Data *data;
    const char *title;
    juce::Component main_component;
    juce::ToggleButton grab_keyboard_button;
    //ToggleButton always_on_top_button;
    juce::ToggleButton bypass_button;
    juce::ToggleButton dpi_awareness_button;
    
    juce::ToggleButton ab_buttons[8];
    bool ab_button_valid[8] = {};

    ToolTipMouseListener tooltipmouselistener;
    
    juce::MidiKeyboardComponent *midi_keyboard = NULL;
    
    juce::AudioProcessorEditor* const editor;

    int64_t g_async_function_id = -1;
    
    int64_t parentgui;

    bool _show_dpi_button;
          

#if USE_EMBEDDED_NATIVE_WINDOW
    void *_embedded_native_window = NULL;
#endif

    bool try_to_resize_editor(void) const {
#if TRY_TO_RESIZE_EDITOR
      return true;
#else
      if (false)  // GenericAudioProcessorEditor doesn't resize its elements, so there's very little point returning true.
        return dynamic_cast<juce::GenericAudioProcessorEditor*>(editor) != NULL;
      else
        return false;
#endif
    }

    void buttonClicked (juce::Button *) override {
    }

    void buttonStateChanged (juce::Button *dasbutton) override {

      //printf("Button state changed %p %p\n", dasbutton, &dpi_awareness_button);
      
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

        /*
        if(new_state)
          editor->setScaleFactor(2.0);
        else
          editor->setScaleFactor(1.0);
        */
        
        struct SoundPlugin *plugin = data->_plugin;
        R_ASSERT_RETURN_IF_FALSE(plugin!=NULL);
        
        bool is_bypass = !ATOMIC_GET(plugin->effects_are_on);
        
        //printf("ButtonStateChanged called for %p. %d %d. Size: %d\n", this, new_state, g_vst_grab_keyboard,(int)g_plugin_windows.size());
        
        if (new_state != is_bypass) {
          int num_effects = plugin->type->num_effects;
          PLUGIN_set_effect_value(plugin, -1, num_effects+EFFNUM_EFFECTS_ONOFF, new_state ? 0.0 : 1.0, STORE_VALUE, FX_single, EFFECT_FORMAT_SCALED);
        }
        
      } else if (dasbutton == &dpi_awareness_button) {
        bool new_state = dpi_awareness_button.getToggleState();

        //printf("new state: %d\n", new_state);

        struct SoundPlugin *plugin = data->_plugin;
        R_ASSERT_RETURN_IF_FALSE(plugin!=NULL);
        
        if (new_state != plugin->is_dpi_aware){
          
          plugin->is_dpi_aware = new_state;

          int64_t instrument_id = plugin->patch->id;
          int64_t parentgui2 = parentgui;
          
          delete this;
          
          THREADING_run_on_main_thread_async([instrument_id, parentgui2]
                                             {
                                               showInstrumentGui(instrument_id, parentgui2, false);
                                             }
                                             );
        }
        
      } else {

        int num = -1;
        for(int i=0;i<8;i++){
          if (dasbutton==&ab_buttons[i]){
            num = i;
            break;
          }
        }

        //printf("   buttonstateChanged. Num: %d. State: %d\n", num, dasbutton->getToggleState());
               
        if (num >= 0) {
          if (dasbutton->getToggleState()){

            struct SoundPlugin *plugin = data->_plugin;
            R_ASSERT_RETURN_IF_FALSE(plugin!=NULL);

            if (safe_int_read(&plugin->curr_ab_num) != num) {
              
              struct Patch *patch = const_cast<struct Patch*>(plugin->patch);
            
              if (patch==NULL){
                
                R_ASSERT_NON_RELEASE(false);
                
              } else {

                if (THREADING_is_main_thread())
                  AUDIOWIDGET_set_ab(patch, num);
                else
                  // Theoretically, more than one button could be clicked before g_async_function_id goes back to -1, but that's not important since waiting for g_async_function_id is only to avoid flickering.
                  g_async_function_id =
                    THREADING_run_on_main_thread_async([patch, num](){ // Need to run async to avoid deadlock if QCheckbox->setChecked() calls the callback directly.
                        AUDIOWIDGET_set_ab(patch, num);
                      });
              }
            }
          }
        }
      }
      
    }
    /*
    void paint(Graphics& g) override {
      g.fillAll(Colours::black);
    }
    */
    void timerCallback() override {
      // grab button
      {
        bool new_state = grab_keyboard_button.getToggleState();
        if (new_state != g_vst_grab_keyboard)
          grab_keyboard_button.setToggleState(g_vst_grab_keyboard, juce::NotificationType::dontSendNotification);
      }

      struct SoundPlugin *plugin = data->_plugin;
      R_ASSERT_RETURN_IF_FALSE(plugin!=NULL);

      // bypass button
      {
        bool new_state = bypass_button.getToggleState();
        bool is_bypass = !ATOMIC_GET_RELAXED(plugin->effects_are_on);
        if (new_state != is_bypass)
          bypass_button.setToggleState(is_bypass, juce::NotificationType::dontSendNotification);
      }

      // dpi awareness button
#if 0
      {
        bool new_state = dpi_awareness_button.getToggleState();
        bool is_dpi_aware = plugin->is_dpi_aware;
        if (new_state != is_dpi_aware)
          dpi_awareness_button.setToggleState(is_dpi_aware, juce::NotificationType::dontSendNotification);
      }
#endif
      
      // ab buttons
      bool check_ab_buttons = false;
      
      if (g_async_function_id == -1) {
        
        check_ab_buttons = true;
        
      } else {

        if (THREADING_async_function_has_run(g_async_function_id)==true){
          g_async_function_id = -1;
          check_ab_buttons = true;
        }
          
      }

      if (check_ab_buttons) {
        
        int num = safe_int_read(&plugin->curr_ab_num);
        
        R_ASSERT_RETURN_IF_FALSE(num >= 0 && num < 8);
        
        if (ab_buttons[num].getToggleState()==false)
          ab_buttons[num].setToggleState(true, juce::NotificationType::dontSendNotification);
        
        for(int i=0;i<8;i++){
          bool is_selected = num==i;
          
          bool new_valid = plugin->ab_is_valid[i] || is_selected;
          
          if (new_valid != ab_button_valid[i]){
            
            const char *buttonnames = "ABCDEFGH";
            
            if (new_valid)
              ab_buttons[i].setButtonText(juce::String(&buttonnames[i], 1) + "*");
            else
              ab_buttons[i].setButtonText(juce::String(&buttonnames[i], 1));
            
            ab_button_valid[i] = new_valid;
          }
        }
      }

      //printf("...Sizes (main: %d, editor: %d). Height: (main: %d, editor: %d)\n", main_component.getWidth(), editor->getWidth(), main_component.getHeight(), editor->getHeight());
      
      //printf("Width: %d\n", editor->getWidth());
             
      //if (isAlwaysOnTop() != vstGuiIsAlwaysOnTop())
      //  delete this;
      //this->setAlwaysOnTop(vstGuiIsAlwaysOnTop());
    }

    int get_button_height(void) const {
      return root->song->tracker_windows->fontheight * 3 / 2;
    }
    
    int get_keyboard_height(void) const {
      if(midi_keyboard==NULL)
        return 0;
      else
        return get_button_height() * 3;
    }
    
    void position_components(int editor_width, int editor_height){

      int x = 0;

      int rightmost_ab_button_x = 0;
      
#if defined(RELEASE) && FOR_LINUX
      
      rightmost_ab_button_x = editor_width-bypass_button.getWidth();
      bypass_button.setTopLeftPosition(R_MAX(x, rightmost_ab_button_x), 0);
      x = bypass_button.getX() + bypass_button.getWidth();
      
#else
      
      rightmost_ab_button_x = editor_width-grab_keyboard_button.getWidth()-bypass_button.getWidth();
        
      if(_show_dpi_button)
        rightmost_ab_button_x -= dpi_awareness_button.getWidth();      

      x = R_MAX(0, rightmost_ab_button_x);

      bypass_button.setTopLeftPosition(x, 0);
      x += bypass_button.getWidth();
      
      grab_keyboard_button.setTopLeftPosition(x, 0);
      x += grab_keyboard_button.getWidth();
      
      //always_on_top_button.setTopLeftPosition(this->getWidth()-grab_keyboard_button.getWidth()-always_on_top_button.getWidth(), 0);
      
#endif

      if(_show_dpi_button)
        dpi_awareness_button.setTopLeftPosition(x, 0);

      //int total_button_width = x;
      
      x = 0;
      for(int i=0;i<8;i++){
        
        int width = ab_buttons[i].getWidth();

        ab_buttons[i].setTopLeftPosition(x, 0);
        
        if (x+width > rightmost_ab_button_x) {
          if (i==1)
            ab_buttons[0].setVisible(false);
          ab_buttons[i].setVisible(false);
        } else {
          ab_buttons[i].setVisible(true);
        }
        
        x += width;
      }

      if (midi_keyboard != NULL){
        midi_keyboard->setSize(editor_width, get_keyboard_height());
        midi_keyboard->setTopLeftPosition(0, get_button_height() + editor_height);
      }
    }


    void componentMovedOrResized(juce::Component &component, bool wasMoved, bool wasResized) override {

      if (wasResized){
        
#if !defined(RELEASE)
        //        printf("Resized. Width: %d (main: %d, editor: %d). Height: %d (main: %d, editor: %d)\n", component.getWidth(), main_component.getWidth(), editor->getWidth(), component.getHeight(), main_component.getHeight(), editor->getHeight());
#endif

        int editor_width;
        int editor_height;

        if(try_to_resize_editor()){
          
          editor_width = main_component.getWidth();
          editor_height = main_component.getHeight() - get_button_height() - get_keyboard_height();
          
          editor->setSize(editor_width, editor_height);
          
        }else{
          
          editor_width = editor->getWidth();
          editor_height = editor->getHeight();
        
          main_component.setSize(editor_width, editor_height + get_button_height() + get_keyboard_height());
          
        }

        position_components(editor_width,editor_height);
        
      }

    }

    PluginWindow(const char *title, Data *data, juce::AudioProcessorEditor* const editor, int64_t parentgui, bool vst_gui_always_on_top, bool show_dpi_button)
      : DocumentWindow (title,
                        juce::Colours::lightgrey,
                        juce::DocumentWindow::closeButton, //allButtons,
                        true)
      , data(data)
      , title(title)
      , editor(editor)
      , parentgui(parentgui)
      , _show_dpi_button(show_dpi_button)
    {

      if (showVirtualMidiKeyboardBelowNativeGUIs() && data->audio_instance->acceptsMidi())
        midi_keyboard = new juce::MidiKeyboardComponent(data->keyboardState, juce::MidiKeyboardComponent::horizontalKeyboard);

      struct SoundPlugin *plugin = data->_plugin;
      ATOMIC_SET(plugin->auto_suspend_suspended, true);

      this->setOpaque(true);
      this->addToDesktop();//getDesktopWindowStyleFlags());

      static juce::LookAndFeel_V3 *lookandfeel = NULL;
      if (lookandfeel==NULL)
        lookandfeel = new juce::LookAndFeel_V3();

      this->setLookAndFeel(lookandfeel);
      
      if(try_to_resize_editor())
        this->setResizable(true, true);
      
#if !FOR_WINDOWS // We have a more reliable way to do this on Windows (done at the end of this function)
      if(vst_gui_always_on_top){
        this->setAlwaysOnTop(true);
      }
#endif
      
      int button_height = get_button_height();
      int keyboard_height = get_keyboard_height();

      R_ASSERT_NON_RELEASE(editor->getWidth() > 0);
      R_ASSERT_NON_RELEASE(editor->getHeight() > 0);
      
      int initial_width = R_MAX(100, editor->getWidth());
      int initial_height = R_MAX(100, editor->getHeight() + button_height + keyboard_height);
      
      this->setSize (initial_width, initial_height);
      this->setUsingNativeTitleBar(true);
      //this->setUsingNativeTitleBar(false);

      main_component.setSize(initial_width, initial_height);

      if(try_to_resize_editor())
        this->setContentNonOwned(&main_component, false);
      else
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
        
        grab_keyboard_button.setToggleState(g_vst_grab_keyboard, juce::NotificationType::dontSendNotification);
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
        
        always_on_top_button.setToggleState(vst_gui_always_on_top, juce::NotificationType::dontSendNotification);
        always_on_top_button.setSize(400, button_height);
        always_on_top_button.changeWidthToFitText();
        
        always_on_top_button.addListener(this);
      
        // add it
        main_component.addAndMakeVisible(&always_on_top_button);
        always_on_top_button.setTopLeftPosition(0, 0);
      }
#endif
     
      // dpi awareness button
      if(_show_dpi_button){

        dpi_awareness_button.setButtonText("DPI");
        
        dpi_awareness_button.setToggleState(plugin->is_dpi_aware, juce::NotificationType::dontSendNotification);
        dpi_awareness_button.setSize(400, button_height);
        dpi_awareness_button.changeWidthToFitText();
        
        dpi_awareness_button.addListener(this);

        dpi_awareness_button.addMouseListener(&tooltipmouselistener, false); // Couldn't get JUCE tooltips to work properly. Showing Qt tooltip instead.
        
        // add it
        main_component.addAndMakeVisible(&dpi_awareness_button);
        dpi_awareness_button.setTopLeftPosition(0, 0);
      }

      // bypass button
      {
        bypass_button.setButtonText("Bypass");
        
        bypass_button.setToggleState(!ATOMIC_GET(plugin->effects_are_on), juce::NotificationType::dontSendNotification);
        bypass_button.setSize(400, button_height);
        bypass_button.changeWidthToFitText();
        
        bypass_button.addListener(this);
      
        // add it
        main_component.addAndMakeVisible(&bypass_button);
        bypass_button.setTopLeftPosition(0, 0);
      }

      // a/b buttons
      {
        int curr_ab_num = safe_int_read(&plugin->curr_ab_num);
        
        for(int i=7;i>=0;i--){
          
          const char *buttonnames = "ABCDEFGH";

          bool is_selected = curr_ab_num==i;
          
          ab_buttons[i].setButtonText(juce::String(&buttonnames[i], 1));
          ab_buttons[i].setToggleState(is_selected, juce::NotificationType::dontSendNotification);
          ab_buttons[i].setSize(40, button_height);
          ab_buttons[i].changeWidthToFitText();
          ab_buttons[i].setRadioGroupId(1, juce::NotificationType::dontSendNotification);
          ab_buttons[i].addListener(this);

          if (plugin->ab_is_valid[i] || is_selected){
            ab_buttons[i].setButtonText(juce::String(&buttonnames[i], 1) + "*");
            ab_button_valid[i] = true;
          }
                
          // add it
          main_component.addAndMakeVisible(&ab_buttons[i]);
          ab_buttons[i].setTopLeftPosition(0, 0);
        }
      }

      // add vst gui
      main_component.addChildComponent(editor);
      if (dynamic_cast<juce::GenericAudioProcessorEditor*>(editor) != NULL)
        editor->setLookAndFeel(new juce::LookAndFeel_V4);
      editor->setTopLeftPosition(0, button_height);
      editor->setVisible(true);
      
      if (midi_keyboard != NULL) {
        main_component.addChildComponent(midi_keyboard);
        midi_keyboard->setTopLeftPosition(0, button_height + editor->getHeight());
        midi_keyboard->setSize(editor->getWidth(), keyboard_height);
        midi_keyboard->setVisible(true);
      }
      
      main_component.setSize(editor->getWidth(), editor->getHeight() + button_height + keyboard_height);
          
      position_components(editor->getWidth(), editor->getHeight());

      if(try_to_resize_editor())
        main_component.addComponentListener(this);
      else
        editor->addComponentListener(this);

      {
        int num_x = data->xs.count(parentgui);
        int num_y = data->ys.count(parentgui);
        
        R_ASSERT(num_x==0 || num_x==1);
        R_ASSERT(num_y==0 || num_y==1);
        
        bool has_x = num_x==1;
        bool has_y = num_y==1;
        
        R_ASSERT(has_x==has_y);

        int x = -1;
        int y = -1;
        
        if (has_x)
          x = data->xs[parentgui];
        
        if (has_y)
          y = data->ys[parentgui];


        if (x <= 0 || y <= 0) {
          this->centreWithSize (getWidth(), getHeight());
        } else {
          this->setTopLeftPosition(x, y);
        }

        this->setVisible(true);
         
     }


      startTimer(100);

#if USE_EMBEDDED_NATIVE_WINDOW
      auto closeit = [this] (void *handle){
        printf("CLOSEIT called\n");
        run_on_message_thread([&](){
            //MMLock mmLock();
        
            _embedded_native_window = NULL; // It's not valid anymore.
        
            delete this;
          });
      };
#endif
      
      {
#if USE_EMBEDDED_NATIVE_WINDOW
        _embedded_native_window = OS_GFX_create_embedded_native_window(this->getWindowHandle(), this->getX(), this->getY(), main_component.getWidth(), main_component.getHeight(), closeit);
#endif
      }                                          

#if FOR_WINDOWS
      {
        //OS_WINDOWS_set_always_on_top(this->getWindowHandle());
        void *parent = API_get_native_gui_handle(parentgui);
        if (parent != NULL)
          OS_WINDOWS_set_window_on_top_of(parent, this->getWindowHandle());
        else{
          R_ASSERT(false);
        }
      }
#endif

      g_num_visible_plugin_windows++;
    }

    ~PluginWindow(){

      //printf("     \n\n\n\nPLUGINWINDOW DELETED\n\n\n\n");
      
      data->xs[parentgui] = getX();
      data->ys[parentgui] = getY();

      g_num_visible_plugin_windows--;
      
      GL_lock();{

        radium::ScopedMutex lock(JUCE_show_hide_gui_lock);
    
        delete editor;
        ATOMIC_SET(data->window, NULL);
        V_free((void*)title);

        delete midi_keyboard;
        
        struct SoundPlugin *plugin = data->_plugin;
        R_ASSERT_RETURN_IF_FALSE(plugin!=NULL);
        
        ATOMIC_SET(plugin->auto_suspend_suspended, false);

      }GL_unlock();

      PLUGIN_call_me_when_gui_closes(data->_plugin);

#if USE_EMBEDDED_NATIVE_WINDOW
      if (_embedded_native_window != NULL){
        OS_GFX_close_embedded_native_window(_embedded_native_window);
        _embedded_native_window = NULL;        
      }
#endif
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

  };


  struct JuceThread : public juce::Thread {
    juce::Atomic<int> isInited;

    JuceThread()
      : Thread("Juce dispatcher thread")
    {
      isInited.set(0);
    }

    void run() override {
      THREADING_init_juce_thread_type();
      
      juce::initialiseJuce_GUI();
      
      juce::MessageManager::getInstance(); // make sure there is an instance here to avoid theoretical race condition
      
      isInited.set(1);
      
      juce::MessageManager::getInstance()->runDispatchLoop();

      juce::shutdownJuce_GUI();
    }
  };
}

static void buffer_size_is_changed(struct SoundPlugin *plugin, int new_buffer_size){
  Data *data = (Data*)plugin->data;
  data->buffer.setSize(data->buffer.getNumChannels(), new_buffer_size);
}

// 
static void RT_MIDI_send_msg_to_patch_receivers2(struct SeqTrack *seqtrack, struct Patch *patch, juce::MidiMessage message, int64_t seq_time){       
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
  
  juce::MidiMessage message(data, data_size, num_bytes_used, 0);
  
  RT_MIDI_send_msg_to_patch_receivers2(seqtrack, patch, message, seq_time);

  return num_bytes_used;
}

static void RT_MIDI_send_msg_to_patch2(struct SeqTrack *seqtrack, struct Patch *patch, juce::MidiMessage message, int64_t seq_time){       
  if (message.isNoteOn()) {
    note_t note = create_note_t(NULL,
                                -1,
                                message.getNoteNumber(),
                                message.getVelocity() / 127.0f,
                                0.0f,
                                message.getChannel()-1,
                                0,
                                0);

    RT_PATCH_play_note(seqtrack, patch, note, NULL, seq_time);
  
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
  
  juce::MidiMessage message(data, data_size, num_bytes_used, 0);
  
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

  // Add events to the midi buffer created by the virtual keyboard.
  data->keyboardState.processNextMidiBuffer(data->midi_buffer, 0, num_frames, true);
 

  // 1. Process audio

  juce::AudioPluginInstance *instance = data->audio_instance;
  juce::AudioSampleBuffer &buffer = data->buffer;
  
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

    juce::MidiBuffer::Iterator iterator(data->midi_buffer);
    
    RT_PLAYER_runner_lock();{
      juce::MidiMessage message;
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
  juce::MidiBuffer &buffer = data->midi_buffer;

  //printf("In. Play note %d %d\n",(int)note_num,(int)time);
  juce::MidiMessage message(0x90 | note.midi_channel, (int)note.pitch, R_BOUNDARIES(0, (int)(note.velocity*127), 127));
  buffer.addEvent(message, time);
}

static void set_note_volume(struct SoundPlugin *plugin, int time, note_t note){
  Data *data = (Data*)plugin->data;
  juce::MidiBuffer &buffer = data->midi_buffer;

  int velocity = R_BOUNDARIES(0,note.velocity,127);
  juce::MidiMessage message(0xa0 | note.midi_channel, (int)note.pitch, velocity, 0.0);
  buffer.addEvent(message, time);
}

static void stop_note(struct SoundPlugin *plugin, int time, note_t note){
  Data *data = (Data*)plugin->data;
  juce::MidiBuffer &buffer = data->midi_buffer;

  int ox90 = MIDI_get_use_0x90_for_note_off() ? 0x90 : 0x80;

  //printf("In. Stop note %d %d\n",(int)note_num,(int)time);
  juce::MidiMessage message(ox90 | note.midi_channel, (int)note.pitch, 0, 0.0);
  buffer.addEvent(message, time);
}

static void send_raw_midi_message(struct SoundPlugin *plugin, int block_delta_time, uint32_t msg){
  uint8_t data[3];
  data[0] = MIDI_msg_byte1(msg);
  data[1] = MIDI_msg_byte2(msg);
  data[2] = MIDI_msg_byte3(msg);

  //printf("   Data: %x %x %x (%x)\n",(int)data[0],(int)data[1],(int)data[2], msg);
  
  int num_bytes_used;
  juce::MidiMessage message(data, 3, num_bytes_used, 0);
  
  if (num_bytes_used>0) {
    Data *data = (Data*)plugin->data;
    juce::MidiBuffer &buffer = data->midi_buffer;
    buffer.addEvent(message, block_delta_time);
  }
  
  //  else
  //  RError("Illegal midi msg: %x",msg); // Well, the illegal message could have been created by a third party plugin.
}

static int RT_get_latency(const struct SoundPlugin *plugin){
  Data *data = (Data*)plugin->data;
  juce::AudioPluginInstance *instance = data->audio_instance;
  int latency = instance->getLatencySamples();
  //printf("\n\n\n    Created new latency %d\n\n\n", latency);
  return latency;
}

static int RT_get_audio_tail_length(const struct SoundPlugin *plugin){
  Data *data = (Data*)plugin->data;
  juce::AudioPluginInstance *instance = data->audio_instance;

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

    return ceil(instance->getTailLengthSeconds()*MIXER_get_sample_rate()); // FIX: check if getTailLengthSeconds() can return infinite.
    
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
    data->audio_instance->getParameters()[effect_num]->setValue(value);
  }
}

float get_scaled_value_from_native_value(struct SoundPlugin *plugin, int effect_num, float native_value){
  return native_value;
}

static float get_effect_value(struct SoundPlugin *plugin, int effect_num, enum ValueFormat value_format){
  //static int num_calls=0;
  //printf("    %d: GET_EFFECT_VALUE: %d\n", ++num_calls, effect_num);
  
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
    return data->audio_instance->getParameters()[effect_num]->getValue();
  }
}

static void get_display_value_string(SoundPlugin *plugin, int effect_num, char *buffer, int buffersize){

  R_ASSERT_NON_RELEASE(!PLAYER_current_thread_has_lock()); // assert that we don't hold the player lock here since we obtain the mmLock below.
  
  //const MMLock mmLock; // FIX: Why do we use MessageManagerLock in get_display_value_string and not here? Partly answer: We can't hold mmLock in get_effect_value since it's called from the player and/or while holding the player lock. Not sure if we need the mmLock here though.

  Data *data = (Data*)plugin->data;
  
  run_on_message_thread([&](){
      
      juce::String l = data->audio_instance->getParameters()[effect_num]->getLabel();
      const char *label = l.toRawUTF8();

      if (is_vst2(plugin)) // audio_instance->getParameterText() sometimes crashes. Doing it manually instead.
        {
          char disp[128] = {}; // c++ way of zero-initialization without getting missing-field-initializers warning.
          AEffect *aeffect = (AEffect*)data->audio_instance->getPlatformSpecificData();
          const int effGetParamDisplay = 7;
          
          {
            radium::PlayerRecursiveLock lock;
            aeffect->dispatcher(aeffect,effGetParamDisplay,
                                effect_num, 0, (void *) disp, 0.0f);
          }
          
          if (disp[0]==0){
            snprintf(buffer,buffersize-1,"%f%s",aeffect->getParameter(aeffect,effect_num),label);//plugin->type_data->params[effect_num].label);
          }else{
            snprintf(buffer,buffersize-1,"%s%s",disp,label);
          }
        }
      else {
        float value;
        {
          radium::PlayerRecursiveLock lock;
          value = data->audio_instance->getParameters()[effect_num]->getValue();
        }
        juce::String l = data->audio_instance->getParameters()[effect_num]->getText(value, buffersize-1);
        snprintf(buffer,buffersize-1,"%s%s",l.toRawUTF8(),label);
      }
      
    });
}

// This function is called extremely often.
static bool gui_is_visible(struct SoundPlugin *plugin){

  //const MMLock mmLock; // Must place here. Even checkin gif data->window==NULL must be protected by lock.
    
  Data *data = (Data*)plugin->data;

#if CHANGE_GUI_VISIBILITY_INSTEAD_OF_REOPENING

  bool ret = false;

  run_on_message_thread([&](){
      if (ATOMIC_GET_RELAXED(data->window)==NULL) {
        
        ret = false;
        
      } else {
        
        ret = ATOMIC_GET_RELAXED(data->window)->isVisible();
    
      }
    });
  
  return ret;
  
#else

  return ATOMIC_GET_RELAXED(data->window)!=NULL;
  
#endif
}

radium::Mutex JUCE_show_hide_gui_lock;

static bool show_gui(struct SoundPlugin *plugin, int64_t parentgui){
  //  const MMLock mmLock;
  
  showVirtualMidiKeyboardBelowNativeGUIs(); // To avoid calling SETTINGS_read_bool from the message thread. (only need to call one time, but it will only read from the disk the first time)

  bool ret = false;
  
  Data *data = (Data*)plugin->data;

  bool vst_gui_always_on_top = vstGuiIsAlwaysOnTop();
  const char *title = plugin->patch==NULL ? talloc_format("%s %s",plugin->type->type_name, plugin->type->name) : plugin->patch->name;

  bool is_scaling_display = OS_WINDOWS_is_scaling_display();
  
  run_on_message_thread([&ret, title, plugin, data, is_scaling_display, parentgui, vst_gui_always_on_top](){

      if (ATOMIC_GET_RELAXED(data->window)==NULL) {
        
        GL_lock();{
          
          radium::ScopedMutex lock(JUCE_show_hide_gui_lock);
          
          bool has_editor = data->audio_instance->hasEditor();
          bool show_dpi_button = has_editor && is_scaling_display; // OS_WINDOWS_is_scaling_display() returns false when not using windows.
          
          radium::ScopedSetDpiContextAwareness awareness(show_dpi_button && !plugin->is_dpi_aware);
          
          juce::AudioProcessorEditor *editor;
          
          if (has_editor)
            editor = data->audio_instance->createEditor(); //IfNeeded();
          else{
            editor = new juce::GenericAudioProcessorEditor(data->audio_instance);        
          }
          
          if (editor != NULL) {
            
            ATOMIC_SET(data->window, new PluginWindow(V_strdup(title), data, editor, parentgui, vst_gui_always_on_top, show_dpi_button));
            
            ret = true;
          }
          
        }GL_unlock();
        
#if CHANGE_GUI_VISIBILITY_INSTEAD_OF_REOPENING
      } else {
        ATOMIC_GET_RELAXED(data->window)->setVisible(true);
        ret = true;
#endif
      }

    });

  return ret;
}

//void OS_WINDOWS_move_main_window_to_front(void);

static void hide_gui(struct SoundPlugin *plugin){
  //const MMLock mmLock;
  
  Data *data = (Data*)plugin->data;

  run_on_message_thread([&](){

      auto *window = ATOMIC_GET_RELAXED(data->window);
      
#if CHANGE_GUI_VISIBILITY_INSTEAD_OF_REOPENING
      if (window != NULL)
        window->setVisible(false);
#else
      // Note, delete window is also called when clicking the DPI button.
      delete window; // NOTE: data->window is set to NULL in the window destructor. It's hairy, but there's probably not a better way.
#endif
      
    });
  
  //OS_WINDOWS_move_main_window_to_front();
}


static juce::AudioPluginInstance *create_audio_instance(const TypeData *type_data, float sample_rate, int block_size){
  
  static bool inited=false;

  static juce::AudioPluginFormatManager *formatManager = NULL;
    
  if (inited==false){
    //const MMLock mmLock;
    run_on_message_thread([&](){
        formatManager = new juce::AudioPluginFormatManager;
        formatManager->addDefaultFormats();
        inited=true;
      });
  }

  juce::String errorMessage;

  const juce::PluginDescription &description = type_data->description;

  printf("  Trying to load -%S-. Identifier: -%s-\n", type_data->file_or_identifier,description.createIdentifierString().toRawUTF8());
  
  {
    radium::ScopedMutex lock(JUCE_show_hide_gui_lock);
    
    juce::AudioPluginInstance *instance;
    
    {
      // const MMLock mmLock; Leads to deadlock. Also, AudioPluginFormat::createInstanceFromDescription is explicitly made to handle calls not made from the message thread.
      instance = formatManager->createPluginInstance(description, sample_rate, block_size, errorMessage);
    }
    
    if (instance==NULL){
      GFX_addMessage("Unable to open %s plugin %s: %s\n",description.pluginFormatName.toRawUTF8(), description.fileOrIdentifier.toRawUTF8(), errorMessage.toRawUTF8());
      return NULL;
    }
    
    {
      //const MMLock mmLock;
      run_on_message_thread([&](){
          instance->prepareToPlay(sample_rate, block_size);
        });
    }
  
    return instance;
  }
}


static void set_plugin_type_data(juce::AudioPluginInstance *audio_instance, SoundPluginType *plugin_type){
  TypeData *type_data = (struct TypeData*)plugin_type->data;

  /*
  if (audio_instance->hasEditor()==false) {
    plugin_type->show_gui = NULL;
    plugin_type->hide_gui = NULL;
  }
  */
  
  // The info in PluginDescription is probably fine, but override here just in case.
  plugin_type->num_inputs = audio_instance->getTotalNumInputChannels();
  plugin_type->num_outputs = audio_instance->getTotalNumOutputChannels();

  {
    // Maybe this isn't always true...
    //plugin_type->state_contains_effect_values = true;
    
    // This is always true
    plugin_type->state_may_contain_effect_values = true;
  }
  
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
  char temp[1024];
  snprintf(temp, 1023, "%sAccepts MIDI: %s\nProduces MIDI: %s\n", plugin_type->info, audio_instance->acceptsMidi()?"Yes":"No", audio_instance->producesMidi()?"Yes":"No");
  plugin_type->info = V_strdup(temp);
        
  plugin_type->is_instrument = audio_instance->acceptsMidi(); // doesn't seem like this field ("is_instrument") is ever read...

  plugin_type->num_effects = audio_instance->getParameters().size();

  type_data->effect_names = (const char**)V_calloc(sizeof(char*),plugin_type->num_effects);
  for(int i = 0 ; i < plugin_type->num_effects ; i++){
    //  type_data->effect_names[i] = V_strdup(audio_instance->getParameterName(i).toRawUTF8());
    snprintf(temp, 1023, "%d: %s",i,audio_instance->getParameters()[i]->getName(1000).toRawUTF8());
    type_data->effect_names[i] = V_strdup(temp);
  }
}


static void recreate_from_state(struct SoundPlugin *plugin, hash_t *state, bool is_loading){
  //const MMLock mmLock;

  Data *data = (Data*)plugin->data;
  
  juce::AudioPluginInstance *audio_instance = data->audio_instance;

  TypeData *type_data = (struct TypeData*)plugin->type->data;

  bool is_compatible = true;

  if (HASH_has_key(state, "identifier_string")){
    bool matches;
    const char *identifier_string = HASH_get_chars(state, "identifier_string");
    run_on_message_thread([&](){
        matches = type_data->description.matchesIdentifierString(identifier_string);
      });
    if (!matches){
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
      run_on_message_thread([&](){
          juce::MemoryBlock sourceData;
          sourceData.fromBase64Encoding(stateAsString);
          audio_instance->setStateInformation(sourceData.getData(), sourceData.getSize());
        });
    }
    
    
    if (HASH_has_key(state, "audio_instance_current_program")) {
      int current_program = HASH_get_int(state, "audio_instance_current_program");
      run_on_message_thread([&](){
          audio_instance->setCurrentProgram(current_program);
        });
    }
    
    if (HASH_has_key(state, "audio_instance_program_state")){
      const char *programStateAsString = HASH_get_chars(state, "audio_instance_program_state");
      run_on_message_thread([&](){
          juce::MemoryBlock sourceData;
          sourceData.fromBase64Encoding(programStateAsString);
      
          audio_instance->setCurrentProgramStateInformation(sourceData.getData(), sourceData.getSize());
        });          
    }
  }

  /*
  if (HASH_has_key(state, "x_pos"))
    data->x = HASH_get_int(state, "x_pos");
  
  if (HASH_has_key(state, "y_pos"))
    data->y = HASH_get_int(state, "y_pos");
  */
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

  if(is_vst2(plugin_type) || is_vst3(plugin_type)){
    printf("TYPE_NAME: -%s-, NAME: -%s-. Version: %s\n", plugin_type->type_name, plugin_type->name, type_data->description.version.toRawUTF8());
    if (juce::String(plugin_type->name)=="Moody Sampler" && (type_data->description.version=="V5.0" || type_data->description.version=="0.5.0")){
      vector_t v = {};

      int yes = VECTOR_push_back(&v, "Yes");
      int no = VECTOR_push_back(&v, "No");(void)no;
      
      int ret = GFX_Message(&v,
                            "Warning, this plugin (\"Moody Sampler\") has been marked as unstable.<p>Reason: Crashes program when deleted.<p>Load it anyway?");
      if (ret!=yes)
        return NULL;
    }
  }
  
  juce::AudioPluginInstance *audio_instance = create_audio_instance(type_data, sample_rate, block_size);
  if (audio_instance==NULL){
    return NULL;
  }

  {
    //const MMLock mmLock;
    Data *data;
    
    run_on_message_thread([&](){
        juce::PluginDescription description = audio_instance->getPluginDescription();

        //plugin->name = talloc_strdup(description.name.toUTF8());
        
        data = new Data(audio_instance, plugin, audio_instance->getTotalNumInputChannels(), audio_instance->getTotalNumOutputChannels());
        plugin->data = data;
        
        audio_instance->setPlayHead(&data->playHead);
        
        if(type_data->effect_names==NULL)
          set_plugin_type_data(audio_instance,(SoundPluginType*)plugin_type); // 'plugin_type' was created here (by using calloc), so it can safely be casted into a non-const.
      });
    
    if (state!=NULL)
      recreate_from_state(plugin, state, is_loading);
    
    num_running_plugins++;

    return data;
  }
}


static void create_state(struct SoundPlugin *plugin, hash_t *state){
  //const MMLock mmLock;
  
  Data *data = (Data*)plugin->data;
  
  juce::AudioPluginInstance *audio_instance = data->audio_instance;

  // save state
  {    
    juce::MemoryBlock destData;
    run_on_message_thread([&](){
        audio_instance->getStateInformation(destData);
      });

    if (destData.getSize() > 0){
      juce::String stateAsString = destData.toBase64Encoding();    
      HASH_put_chars(state, "audio_instance_state", stateAsString.toRawUTF8());
    }
  }

  // save program state
  {
    juce::MemoryBlock destData;
    run_on_message_thread([&](){
        audio_instance->getCurrentProgramStateInformation(destData);
      });
    
    if (destData.getSize() > 0){
      juce::String stateAsString = destData.toBase64Encoding();    
      HASH_put_chars(state, "audio_instance_program_state", stateAsString.toRawUTF8());
    }
  }

  int current_program;
  
  run_on_message_thread([&](){
      current_program = audio_instance->getCurrentProgram();
    });
  HASH_put_int(state, "audio_instance_current_program", current_program);

  /*
  HASH_put_int(state, "x_pos", data->x);
  HASH_put_int(state, "y_pos", data->y);
  */
  
  {
    TypeData *type_data = (struct TypeData*)plugin->type->data;
    const char *identifier_string;
    run_on_message_thread([&](){
        identifier_string = V_strdup(type_data->description.createIdentifierString().toRawUTF8());
      });  
    
    HASH_put_chars(state, "identifier_string", identifier_string);
    V_free((void*)identifier_string);
  }
}


namespace{
  // Some plugins require that it takes some time between deleting the window and deleting the instance. If we don't do this, some plugins will crash. Only seen it on OSX though, but it doesn't hurt to do it on all platforms.
  struct DelayDeleteData : public juce::Timer {
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
  //const MMLock mmLock;

  num_running_plugins--;
      
  run_on_message_thread([&](){
      
      printf(">>>>>>>>>>>>>> Cleanup_plugin_data called for %p\n",plugin);
      Data *data = (Data*)plugin->data;
      
      delete ATOMIC_GET_RELAXED(data->window);
      
      data->plugin_will_be_deleted();
      
      // Wait a little bit first. (if CPU is VERY buzy, perhaps this waiting could prevent a crash)
#if 0
      // No, too much waiting when deleting several plugins at once. The 10*0.5s waiting scheme is probably good enough anyway.
      juce::MessageManager::getInstance()->runDispatchLoopUntil(100);
#endif
      
      // Then schedule deletion in 5 seconds.
      new DelayDeleteData(data);
    });
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
  //const MMLock mmLock;

  Data *data = (Data*)plugin->data;

  //printf("   IS_VST3: %d,  IS_VST2: %d\n", data->is_vst3(), data->is_vst2());
  
  if (is_vst3(plugin)) // Must upgrade JUCE. With the current version, presets are not working with vst3.
    return 0;
  
  juce::AudioPluginInstance *instance = data->audio_instance;

  int ret;

  run_on_message_thread([&](){
      ret = instance->getNumPrograms();
    });
  
  return ret;
}

static int get_current_preset(struct SoundPlugin *plugin){
  //const MMLock mmLock;

  Data *data = (Data*)plugin->data;
  juce::AudioPluginInstance *instance = data->audio_instance;

  int ret;
  run_on_message_thread([&](){
      ret = instance->getCurrentProgram();
    });
  return ret;
}

static void set_current_preset(struct SoundPlugin *plugin, int num){
  //const MMLock mmLock;

  Data *data = (Data*)plugin->data;
  juce::AudioPluginInstance *instance = data->audio_instance;

  run_on_message_thread([&](){
      instance->setCurrentProgram(num);
    });
}

static const char *get_preset_name(struct SoundPlugin *plugin, int num){
  //const MMLock mmLock;

  Data *data = (Data*)plugin->data;
  juce::AudioPluginInstance *instance = data->audio_instance;

  const char *ret;
  run_on_message_thread([&](){
      ret = V_strdup(instance->getProgramName(num).toRawUTF8());
    });
  const char *ret2 = talloc_strdup(ret);
  V_free((void*)ret);
  return ret2;
}

static void set_preset_name(struct SoundPlugin *plugin, int num, const char* new_name){
  //const MMLock mmLock;

  Data *data = (Data*)plugin->data;
  juce::AudioPluginInstance *instance = data->audio_instance;

  run_on_message_thread([&](){
      instance->changeProgramName(num, new_name);
    });
}

static void set_non_realtime(struct SoundPlugin *plugin, bool is_non_realtime){
  //const MMLock mmLock;

  Data *data = (Data*)plugin->data;
  juce::AudioPluginInstance *instance = data->audio_instance;

  // run_on_message_thread([&](){...} here makes Radium hang on OSX and Windows.
      
  if (THREADING_is_player_or_runner_thread()) {

    // There will probably be a data race here if RT_process is called from a different runner. But it shouldn't be a problem.
    // (I didn't want to think about whether it's safe to have a custom mutex here when writing this code.)
    instance->setNonRealtime(is_non_realtime);
    
  } else {
    
    radium::PlayerRecursiveLock lock; // It's not documented in Juce, but setNonRealtime() should be called from the audio process thread.
    instance->setNonRealtime(is_non_realtime);
    
  }
}

static SoundPluginType *create_plugin_type(const juce::PluginDescription &description, const wchar_t *file_or_identifier, SoundPluginTypeContainer *container){ //, const wchar_t *library_file_full_path){
  printf("b02 %S\n",file_or_identifier);
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
  
  plugin_type->set_effect_value = set_effect_value;
  plugin_type->get_effect_value = get_effect_value;
  plugin_type->get_scaled_value_from_native_value = get_scaled_value_from_native_value;
  
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



static juce::String get_container_descriptions_filename(const wchar_t *container_filename){
  juce::String encoded(STRING_get_sha1(container_filename));
  return juce::String(OS_get_dot_radium_path(true)) + OS_get_directory_separator() + SCANNED_PLUGINS_DIRNAME + OS_get_directory_separator() + "v2_PluginDescription_" + encoded;
}


static enum PopulateContainerResult launch_program_calling_write_container_descriptions_to_cache_on_disk(const wchar_t *container_filename){
#if FOR_WINDOWS
  juce::String executable = juce::String(OS_get_full_program_file_path(STRING_create("radium_plugin_scanner.exe"))); // Don't need to surround string with " and " with juce. You need to do that in Qt.
#else
  juce::String executable = juce::String(OS_get_full_program_file_path(STRING_create("radium_plugin_scanner")));
#endif
  
  juce::StringArray args;
  args.add(executable);
  args.add(juce::Base64::toBase64(juce::String(container_filename)));
  args.add(juce::Base64::toBase64(get_container_descriptions_filename(container_filename)));

  juce::ChildProcess process;

  if (process.start(args)==false){
    GFX_Message2(NULL, true, "Error: Unable to launch %s", executable.toRawUTF8());
    return POPULATE_RESULT_OTHER_ERROR;
  }

  int s = 3;
  
  while(process.waitForProcessToFinish(s*1000)==false){
    vector_t v = {};

    int w3 = VECTOR_push_back(&v, "Wait 3 seconds more");
    int w20 = VECTOR_push_back(&v, "Wait 20 seconds more");
    int cancel = VECTOR_push_back(&v, "Cancel");

    int blacklist = VECTOR_push_back(&v, "Cancel, and add to blacklist");
    fprintf(stderr, "Openinig requester\n");

    int ret = GFX_Message2(&v, true, "Waited more than %d seconds for \"%S\" to load\n", s, container_filename);
    fprintf(stderr, "Got answer from requester\n");

    if (ret==w3)
      s = 3;
    else if (ret==w20)
      s = 20;
    else if (ret==cancel){
      process.kill();
      return POPULATE_RESULT_OTHER_ERROR;
    }
    else if (ret==blacklist){
      process.kill();
      return POPULATE_RESULT_PLUGIN_MUST_BE_BLACKLISTED;
    } else
      s = 3; // timeout, for instance.
  }

  return POPULATE_RESULT_IS_OKAY; // as far as we know. (ChildProcess doesn't have an hasCrashed() function).
}

static enum PopulateContainerResult get_container_descriptions_from_disk(const SoundPluginTypeContainer *container, juce::OwnedArray<juce::PluginDescription> &descriptions){
  juce::String filename = get_container_descriptions_filename(container->filename);
  
  fprintf(stderr, "   READING.  %s: Reading from description file \"%s\".\n", juce::String(container->filename).toRawUTF8(), filename.toRawUTF8());
    
  juce::File file(filename);
  if (file.existsAsFile()==false){
    //GFX_Message2(NULL, true, "Error. Could not find file \"%s\"\n", filename.toRawUTF8());
    //return POPULATE_RESULT_OTHER_ERROR;
    return POPULATE_RESULT_PLUGIN_MUST_BE_BLACKLISTED; // Likely to have been caused by the plugin crashing
  }

  juce::XmlElement *xml = juce::XmlDocument::parse(file);
  if (xml==NULL){
    //GFX_Message2(NULL, true, "Error. Unable to parse xml file \"%s\". You might want to delete this file and try again.\n", filename.toRawUTF8());
    //return POPULATE_RESULT_OTHER_ERROR;
    return POPULATE_RESULT_PLUGIN_MUST_BE_BLACKLISTED; // Likely to have been caused by the plugin crashing
  }

  int size = xml->getNumChildElements();
    
  for(int i = 0 ; i < size ; i++){
    juce::XmlElement *xml_description = xml->getChildElement(i);
    juce::PluginDescription description;
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
    descriptions.add(new juce::PluginDescription(description));
  }

  return POPULATE_RESULT_IS_OKAY;
}

static enum PopulateContainerResult get_container_descriptions(SoundPluginTypeContainer *container, juce::OwnedArray<juce::PluginDescription> &descriptions){
#if 0
  if (!container_descriptions_are_cached_on_disk(container))
    if (write_container_descriptions_to_cache_on_disk(container)==false){
      fprintf(stderr, "Couldn't cache container descriptions\n");
      return true;
    }

  R_ASSERT_RETURN_IF_FALSE2(container_descriptions_are_cached_on_disk(container), false);
#else

  juce::String filename = get_container_descriptions_filename(container->filename);
  juce::File file(filename);
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
  //return POPULATE_RESULT_OTHER_ERROR;

  ContainerData *data = (ContainerData*)container->data;

  juce::OwnedArray<juce::PluginDescription> descriptions;

  
#if 0
  
#if 0 //FOR_MACOSX
  DirectoryIterator iter(File(data->library_file_full_path), false, "*", File::findFiles);
  while (iter.next()) {
    printf("Checking -%s- (%S)\n",iter.getFile().getFullPathName().toRawUTF8(), data->library_file_full_path);
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
    GFX_addMessage("No valid plugins found in %S", container->filename);

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
  container->filename = V_wcsdup(library_file_full_path);
  
  ContainerData *data = (ContainerData*)V_calloc(1, sizeof(ContainerData));
  data->file_or_identifier = wcsdup(file_or_identifier);

  container->data = data;
  
  PR_add_plugin_container(container);
}
 

#if 0 // Only works in linux.

namespace{
  struct ProgressWindow  : public juce::DocumentWindow {
    juce::ThreadWithProgressWindow *progress_bar = NULL;
    //ProgressBar *progress_bar = NULL;
    double progress_progress = 0.0;
    
    ProgressWindow(const char *title)
      : DocumentWindow (title,
                        juce::Colours::lightgrey,
                        juce::DocumentWindow::allButtons,
                        true)
    {      
      progress_bar = new juce::ThreadWithProgressWindow(title, true, false);
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
  //const MMLock mmLock;

  run_on_message_thread([&](){
      delete g_progress_window;
      
      g_progress_window = new juce::ProgressWindow(message);
    });
}

void GFX_ShowProgressMessage(const char *message){
  if (g_progress_window == NULL)
    GFX_OpenProgress("...");

  {
    //const MMLock mmLock;
    run_on_message_thread([&](){
        //g_progress_window->progress_bar->setTextToDisplay(message);
        g_progress_window->progress_bar->setStatusMessage(message);
        g_progress_window->progress_bar->setProgress(g_progress_window->progress_progress);
        g_progress_window->progress_progress += 0.1;
        //if(g_progress_progress > 1.0)
        //  g_progress_progress = 0.0;
      });
  }
}

void GFX_CloseProgress(void){
  //const MMLock mmLock;
  run_on_message_thread([&](){
      delete g_progress_window;
      g_progress_window = NULL;
    });
}

#endif

void *JUCE_lock(void){
  return new juce::MessageManagerLock; // Can not use MMLock here.
}

void JUCE_unlock(void *lock){
  juce::MessageManagerLock *mmLock = static_cast<juce::MessageManagerLock *>(lock);
  delete mmLock;
}

bool JUCE_native_gui_grabs_keyboard(void){
  return g_num_visible_plugin_windows > 0 && g_vst_grab_keyboard;
}

char *JUCE_download(const char *url_url){
  juce::URL url(url_url);
  
  juce::String text =
    url
    //    .withParameter("C", "M")
    //  .withParameter("O", "D")
    .readEntireTextStream();

  return strdup(text.toRawUTF8());
}

static juce::String g_backtrace;

const char *JUCE_get_backtrace(void){
  static int num=0;
  printf("Getting backtrace %d\n", num);
  g_backtrace = juce::SystemStats::getStackBacktrace();
  printf("Got backtrace %d\n", num);
  num++;
  return g_backtrace.toUTF8();
}


void JUCE_add_sound(float *dst, const float *src, int num_frames){
  juce::FloatVectorOperations::add(dst, src, num_frames);  
}

void JUCE_get_min_max_val(const float *array, const int num_elements, float *min_val, float *max_val){
  auto both = juce::FloatVectorOperations::findMinAndMax(array,num_elements);
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

static float RT_get_max_val2(const float *array, const int num_elements){
  return JUCE_get_max_val(array, num_elements);
}

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
  juce::AudioPluginInstance *instance = data->audio_instance;

  juce::String filename(wfilename);
  juce::File file(filename);

  juce::MemoryBlock memoryBlock;
  
  bool success = file.loadFileAsData(memoryBlock);
  if (success==false){
    GFX_Message2(NULL, true, "Unable to load %S", wfilename);
    return;
  }
      
  success = juce::VSTPluginFormat::loadFromFXBFile(instance, memoryBlock.getData(), memoryBlock.getSize());
  if (success==false){
    GFX_Message2(NULL, true, "Could not use %S for this plugin", wfilename);
    return;
  }
  
  printf("************** size: %d\n",(int)memoryBlock.getSize());
}
  
static void save_fxbp(SoundPlugin *plugin, wchar_t *wfilename, bool is_fxb){
  Data *data = (Data*)plugin->data;
  juce::AudioPluginInstance *instance = data->audio_instance;

  juce::MemoryBlock memoryBlock;
  bool result = juce::VSTPluginFormat::saveToFXBFile(instance, memoryBlock, is_fxb);
  if (result==false){
    GFX_Message2(NULL, true, "Unable to create FXB/FXP data for this plugin");
    return;
  }
  
  juce::String filename(wfilename);

  juce::File file(filename);

  juce::Result result2 = file.create();

  if (result2.failed()){
    GFX_Message2(NULL, true, "Unable to create file %S (%s)", wfilename, result2.getErrorMessage().toRawUTF8());
    return;
  }
  
  bool result3 = file.replaceWithData(memoryBlock.getData(), memoryBlock.getSize());
  if (result3==false){
    GFX_Message2(NULL, true, "Unable to write data to file %S (disk full?)", wfilename);
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

static JuceThread *g_juce_thread = NULL;

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
  juce::String(CharPointer_UTF8(crash));
  fprintf(stderr,"init2\n");
  
#endif

  if (g_use_custom_mm_thread){

    g_juce_thread = new JuceThread;
    g_juce_thread->startThread();
    
    while(g_juce_thread->isInited.get()==0)
      juce::Thread::sleep(20);
    
  } else {

    juce::initialiseJuce_GUI();

  }
  
  //juce::Font::setDefaultSansSerifFont
}

void PLUGINHOST_shut_down(void){
  printf(" PLUGINHOST_shut_down: about to...\n");

  if (g_use_custom_mm_thread){
    run_on_message_thread([&](){
        juce::MessageManager::getInstance()->stopDispatchLoop();
      });
    
    if (g_juce_thread != NULL)
      g_juce_thread->stopThread(5000);
    
  } else {
    
    juce::shutdownJuce_GUI();

  }
  
  printf(" PLUGINHOST_shut_down: Got it\n");
}

#endif // defined(COMPILING_JUCE_PLUGINS_O)

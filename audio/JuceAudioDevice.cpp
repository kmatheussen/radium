/* Copyright 2021 Kjetil S. Matheussen

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


#if defined(FOR_WINDOWS)
#include <windows.h>
#endif

#if defined(FOR_MACOSX)
#include "../macosx/machthreads_proc.h"
#endif

#include "../common/threading_lowlevel.h"
#include "../common/settings_proc.h"


int g_juce_num_input_audio_channels = 0;
const float **g_juce_input_audio_channels = NULL;

int g_juce_num_output_audio_channels = 0;
float **g_juce_output_audio_channels = NULL;

  
namespace radium{

class PrefsWindow;
  
static class PrefsWindow *g_prefs_dialog = NULL;

#define KEEP_PREFS_WINDOW_ON_TOP_REGULARLY 1

class PrefsWindow
  : public juce::DialogWindow
#if KEEP_PREFS_WINDOW_ON_TOP_REGULARLY
  , public juce::Timer
#endif
{
  
public:
  
  PrefsWindow()
    : juce::DialogWindow("Audio Settings",
                         juce::Colours::lightgrey,
                         //juce::Colours::black,
                         true)
  {
#if 0
    juce::AffineTransform aff;
    setTransform(aff.scaled(g_gfx_scale, g_gfx_scale, 0, 0));
#endif
    
    g_prefs_dialog = this;

    {
      static juce::LookAndFeel_V3 *s_lookandfeel = NULL;
      if (s_lookandfeel==NULL)
        s_lookandfeel = new juce::LookAndFeel_V3();
      
      this->setLookAndFeel(s_lookandfeel);
    }
    
#if KEEP_PREFS_WINDOW_ON_TOP_REGULARLY
    startTimer(1000);
#endif
  }

  ~PrefsWindow(){
    g_prefs_dialog = NULL;
  }

#if KEEP_PREFS_WINDOW_ON_TOP_REGULARLY
  void timerCallback() override final {
    //printf("TIMERCALLGACN\n");
    if (!is_showing_RT_message() && !GFX_is_showing_message())
      toFront(false); // Linux: shouldAlsoGainFocus==false doesn't work.
  }
#endif
  
#if 0
  void resized(void) override {
    if (getContentComponent() != NULL){
      juce::AffineTransform aff;
      getContentComponent()->setTransform(aff.scaled(g_gfx_scale, g_gfx_scale));
    }
    ResizableWindow::resized();
  }
#endif
    
  void closeButtonPressed() override
  {
    delete this;
  }
};

class JucePlayer : public juce::AudioIODeviceCallback, public juce::ChangeListener {

  
private:
  juce::AudioDeviceManager _audio_device_manager;
  
  std::unique_ptr<juce::XmlElement> _settings;

  JUCE_audio_device_callback _callback;
  void *_callback_data;

  std::function<void(int,float)> _called_before_starting_audio;

public:

  double _last_reported_samplerate = -1;
  double _samplerate;
  int _buffer_size;
  bool _is_running = false;
  
  double _time_cycle_start = 0;
  //DEFINE_ATOMIC(double, _time_cycle_start) = 0;

  JucePlayer(JUCE_audio_device_callback callback, void *callback_data, const wchar_t *settings_string, std::function<void(int,float)> called_before_starting_audio)
    : _callback(callback)
    , _callback_data(callback_data)
    , _called_before_starting_audio(called_before_starting_audio) 
  {
    _audio_device_manager.addChangeListener(this);
    
    _is_running = initJuceAudio(settings_string);
  }

  ~JucePlayer(){
    delete g_prefs_dialog;
  }
  
  void changeListenerCallback(juce::ChangeBroadcaster *source) override {

#if !defined(RELEASE) // Guess we could be called from a realtime thread here.
    printf("Some audio change thing: %p\n", _audio_device_manager.getCurrentAudioDevice());
#endif
    
    if(_audio_device_manager.getCurrentAudioDevice()!=NULL){

      
      double old_samplerate = _last_reported_samplerate;

      _buffer_size = _audio_device_manager.getCurrentAudioDevice()->getCurrentBufferSizeSamples();

      double new_samplerate = _audio_device_manager.getCurrentAudioDevice()->getCurrentSampleRate();
      _last_reported_samplerate = new_samplerate;
      
      _settings = _audio_device_manager.createStateXml(); // This is probably not RT safe, but it's probably not that important.

      if (_settings.get()!=NULL) {

        juce::String settings_string = _settings->toString();
        
        THREADING_run_on_main_thread_async([settings_string]
                                           {
                                             SETTINGS_write_wchars("audio_device", STRING_replace(STRING_replace(settings_string.toWideCharPointer(), "\r\n", " "), "\n", " "));
                                           });
        
        //printf("old: %f. _last_reported: %f. Settings: \"%s\"\n", old_samplerate, _last_reported_samplerate, _settings->toString().toRawUTF8());
      }
      
      
      if (old_samplerate >= 0 && !equal_doubles(old_samplerate, _last_reported_samplerate)){

        THREADING_run_on_main_thread_async([]
                                           {
                                             GFX_Message(NULL, "Radium should be restarted after changing samplerate");
                                           });
        /*
        juce::AlertWindow::showMessageBox (juce::AlertWindow::WarningIcon,
                                           "Radium",
                                           "Radium must be restarted after changing samplerate");
        */
      }
      
    }else{

#if !defined(RELEASE) // Guess we could be called from a realtime thread here.
      fprintf(stderr,"Gakkegakke\n");
#endif
      
    }
  }
  
  bool initJuceAudio(const wchar_t *settings_string){
    
    if (wcslen(settings_string) > 0)
      _settings = juce::XmlDocument::parse(juce::String(settings_string));

    juce::AudioDeviceManager::AudioDeviceSetup preferred;
    preferred.bufferSize = 1024;
    //preferred.sampleRate = 44100; // It's most important to let the audio device manager try to find something that works with buffersize 1024. Maybe setting samplerate could screw that up.
    preferred.inputChannels = 0; // We don't want any input channels either, by default, since this can also screw up the sound sometimes.
    preferred.useDefaultInputChannels = false;

#if defined(FOR_WINDOWS)
    
    // Force normal shared WASAPI for the default device. We do this to avoid the "Windows Audio (low latency)" device, which seems to be stuck at 480 frames, which is not supported by Radium.
    
    for (auto *type : _audio_device_manager.getAvailableDeviceTypes()) {
      if (type->getTypeName()=="Windows Audio") {
        
        printf("Type: %s\n", type->getTypeName().toRawUTF8());
        
        const int defaultN = type->getDefaultDeviceIndex(false);
        const auto names = type->getDeviceNames(false);
        
        if (defaultN >= 0 && defaultN < names.size()) {
          preferred.outputDeviceName = names[defaultN];
          printf("   Default: %s\n", preferred.outputDeviceName.toRawUTF8());
        }
        
        break;
      }
    }
#endif
    
    const juce::String error (_audio_device_manager.initialise (0, /* number of input channels */
                                                                8, /* number of output channels */
                                                                _settings.get(),
                                                                true, //,  /* select default device on failure */ 
                                                                "",
                                                                &preferred
                                                                ));
 
    // start the IO device pulling its data from our callback..
    _audio_device_manager.addAudioCallback (this);
      
    if (_audio_device_manager.getCurrentAudioDevice()==NULL || error.isNotEmpty()) {

      // We also give error in Mixer.cpp though, but the error message might be useful.
      if (error.isNotEmpty())
        GFX_Message(NULL, "Audio device manager: \"%S\"", error.toWideCharPointer());

      _last_reported_samplerate = 48000;
      _samplerate = 48000;
      
      return false;
      
    } else {
      
      //isinitialized=true;
	
      return true;
      
    }
  }

  int get_num_xruns(void){
#if 1
    // Probably safe to do this in the main thread.
    auto *device = _audio_device_manager.getCurrentAudioDevice();
    if (device==NULL)
      return 0;
    else
      return device->getXRunCount();
#else
    int ret = 0;
    run_on_message_thread([this, &ret](){
        auto device = _audio_device_manager.getCurrentAudioDevice();
        if (device==NULL){
          R_ASSERT_NON_RELEASE(false);
        } else {
          ret = device->getXRunCount();
          //printf("RET: %d\n", ret);
        }
      });
    return ret;
#endif
  }
  
#if 1
  bool _has_set_realtime_priority = false;
#endif

  void audioDeviceIOCallback(const float **inputChannelData, 
			     int 	totalNumInputChannels, 
			     float ** 	outputChannelData, 
			     int 	totalNumOutputChannels, 
			     int 	numSamples
			     )
    override
  {    
    if (numSamples < RADIUM_BLOCKSIZE || MIXER_dummy_driver_is_running()){

      if (!MIXER_dummy_driver_is_running()){
        
        R_ASSERT_NON_RELEASE(false);
        MIXER_start_dummy_driver();
        
      }
      
      R_ASSERT_NON_RELEASE(numSamples < RADIUM_BLOCKSIZE);
      
      return;
      
    } else {

      R_ASSERT_NON_RELEASE(!MIXER_dummy_driver_is_running());
      R_ASSERT_NON_RELEASE(numSamples >= RADIUM_BLOCKSIZE);
      
    }
    
    double now = RT_TIME_get_ms();
    //printf("Time: %f\n", now - _time_cycle_start);
    _time_cycle_start = now;
    //ATOMIC_DOUBLE_SET(_time_cycle_start, now);
    
    /*
    // First find the real number of totalNumOutputChannels.
    {
      int i;
      for(i=0;i<totalNumOutputChannels;i++)
	if(outputChannelData[i]==NULL)
	  break;
      totalNumOutputChannels=i;
    }
    */

    //printf("AudioDeviceIOCallback called: %d. rate: %f\n", numSamples, _samplerate);

#if 0
    if(numSamples != _buffer_size){
      R_ASSERT_NON_RELEASE(false);
      _buffer_size = numSamples;
    }
#endif

#if 1
    // Juce doesn't use realtime priority for audio on windows. Boost it up.
    if (_has_set_realtime_priority==false){
            
      THREADING_acquire_player_thread_priority(); // Note: If we call JUCE_audio_set_audio_thread_priority_of_current_thread() instead, error message won't be shown if it fails.
      
      _has_set_realtime_priority=true;
    }
#endif
    
#if !defined(RELEASE)
    // Assert that it's legal to read input
    for(int i=0;i<totalNumInputChannels;i++)
      if(inputChannelData[i]!=NULL){
        if (totalNumOutputChannels > 0 && outputChannelData[0] != NULL)
          memcpy(outputChannelData[0], inputChannelData[i], sizeof(float)*numSamples);
      }
#endif
    
    for(int i=0;i<totalNumOutputChannels;i++)
      if(outputChannelData[i]!=NULL){
        memset(outputChannelData[i], 0, sizeof(float)*numSamples);
      }
    
    g_juce_num_input_audio_channels = totalNumInputChannels;
    g_juce_input_audio_channels = inputChannelData;

    g_juce_num_output_audio_channels = totalNumOutputChannels;
    g_juce_output_audio_channels = outputChannelData;

    _callback(numSamples, _callback_data);
    
    //printf("Fraction: %f\n", MIXER_get_curr_audio_block_cycle_fraction());
  }

  void prefs(){

    delete g_prefs_dialog;
    
    juce::AudioDeviceSelectorComponent *audioSettingsComp = new juce::AudioDeviceSelectorComponent(_audio_device_manager,
                                                                                                  0, // min input
                                                                                                  2000, // max input
                                                                                                  0, // min output
                                                                                                  2000, // max output
                                                                                                  false, // show midi input options
                                                                                                  false, // show midi output options
                                                                                                  false, // show channels as stereo pairs
                                                                                                  false // hideAdvancedOptionsWithButton 
                                                                                                  );
    
    // ...and show it in a DialogWindow...
    audioSettingsComp->setSize (400, 170);

    {
    }
 
    //printf("Approximate: %f\n", juce::Component::getApproximateScaleFactorForComponent(audioSettingsComp));
    //getchar();
    
    g_prefs_dialog = new PrefsWindow();
#if 0
    juce::AffineTransform aff;
    g_prefs_dialog->setTransform(aff.scaled(g_gfx_scale, g_gfx_scale));
#endif
    
    g_prefs_dialog->setOpaque(true);
    g_prefs_dialog->addToDesktop();
    g_prefs_dialog->setUsingNativeTitleBar(true);

    g_prefs_dialog->setContentOwned(audioSettingsComp, true);

    g_prefs_dialog->centreWithSize(g_prefs_dialog->getWidth(), g_prefs_dialog->getHeight());
    //g_prefs_dialog->centreWithSize(400*g_gfx_scale, 170*g_gfx_scale);//g_prefs_dialog->getWidth(), g_prefs_dialog->getHeight());
    g_prefs_dialog->setVisible(true);
    
    /*
    
    juce::DialogWindow::showDialog ("Audio Settings",
                                    audioSettingsComp,
                                    NULL,
                                    juce::Colours::black,
                                    //juce::Colours::findColourForName("black"), //juce::Colour((juce::uint8)0xb90,(juce::uint8)0x60,(juce::uint8)0x60,(juce::uint8)0xd0),
                                    true
                                    );
    */

#if !KEEP_PREFS_WINDOW_ON_TOP_REGULARLY
#ifdef FOR_WINDOWS
    {
      void *parent = API_get_native_gui_handle(-1);
      if (parent != NULL)
        OS_WINDOWS_set_window_on_top_of(parent, g_prefs_dialog->getWindowHandle());
      else{
        R_ASSERT(false);
      }
    }
#endif
#endif
    
    /*
    if(_audio_device_manager.getCurrentAudioDevice()==NULL)
      printf("gakk\n");
    */
  }
  
  void audioDeviceAboutToStart (juce::AudioIODevice *device) override
  {

    _has_set_realtime_priority = false;
    
    double sampleRate = device->getCurrentSampleRate ();
    
    int buffer_size = device->getCurrentBufferSizeSamples();

#if defined(FOR_MACOSX)
    MACH_THREADS_set_period_and_buffer_size(sampleRate, buffer_size);
#endif
    
    //printf("Samplerate set to %f\n",(float)sampleRate);
    
    _samplerate = sampleRate;

    _called_before_starting_audio(buffer_size, _samplerate);
    
    g_audio_system_input_latency = device->getInputLatencyInSamples();
    g_audio_system_output_latency = device->getOutputLatencyInSamples();

#if !defined(RELEASE)
    printf("Latency in/out: %d / %d\n", g_audio_system_input_latency, g_audio_system_output_latency);
#endif
    
    if (buffer_size < RADIUM_BLOCKSIZE || (buffer_size % RADIUM_BLOCKSIZE) != 0){
      THREADING_run_on_main_thread_async([buffer_size]
                                         {
                                           GFX_Message(NULL,
                                                       "Error: Soundcard buffer size must be a multiple of the internal block size."
                                                       "<UL>"
                                                       "<LI>Soundcard buffer size: %d"
                                                       "<LI>Internal block size: %d"
                                                       "</UL>"
                                                       "You can adjust the internal block size under <b>Edit -> Preferences -> Audio -> internal block size.</b>",
                                                       buffer_size, RADIUM_BLOCKSIZE);
                                         });
    }

    if (buffer_size < RADIUM_BLOCKSIZE) {
      
      if (!MIXER_dummy_driver_is_running())
        MIXER_start_dummy_driver();
      
    } else {
      
      if (MIXER_dummy_driver_is_running())
        MIXER_stop_dummy_driver();
      
    }
  }
  
  void audioDeviceStopped() override
  {
    printf("Audio device stopped\n");

    g_juce_num_input_audio_channels = 0;
    g_juce_input_audio_channels = NULL;

    g_juce_num_output_audio_channels = 0;
    g_juce_output_audio_channels = NULL;

    if (!MIXER_dummy_driver_is_running())
      MIXER_start_dummy_driver();
    
    return;
  }


};

}

static radium::JucePlayer *g_juce_player = NULL;

bool JUCE_audio_set_audio_thread_priority(radium_thread_t thread){

#if !defined(RELEASE)
  g_t_current_thread_is_RT = R_IS_RT;
#endif
      
#if defined(FOR_MACOSX)
  
  return MACH_THREADS_jack_acquire_real_time_scheduling(thread);
  
#elif defined(FOR_WINDOWS)
  
  if (SetThreadPriority(thread, THREAD_PRIORITY_TIME_CRITICAL))
    return true;
  else
    return false;
  
#else
  
  auto policy = SCHED_RR; // Maybe use SCHED_FIFO instead.
  auto min_priority = sched_get_priority_min (policy);
  auto max_priority = sched_get_priority_max (policy);

  struct sched_param param = {};

  param.sched_priority = R_BOUNDARIES(min_priority, scale(1,0,2,min_priority,max_priority), max_priority);
  
  if (pthread_setschedparam(thread, policy, &param) == 0) {
    return true;
  } else
    return false;
  
#endif
  
  // not good enough: (on windows)
  //return juce::Thread::setCurrentThreadPriority(juce::Thread::realtimeAudioPriority);
}

bool JUCE_audio_set_audio_thread_priority_of_current_thread(void){
  return JUCE_audio_set_audio_thread_priority(GET_CURRENT_THREAD());
}

bool JUCE_audio_set_normal_thread_priority(radium_thread_t thread){

#if !defined(RELEASE)
  g_t_current_thread_is_RT = R_IS_NOT_RT;
#endif

#if defined(FOR_MACOSX)
  
  return MACH_THREADS_jack_drop_real_time_scheduling(thread);
  
#elif defined(FOR_WINDOWS)
  
  if (SetThreadPriority(thread, THREAD_PRIORITY_NORMAL))
    return true;
  else
    return false;
  
#else
  
  auto policy = SCHED_OTHER;
  
  struct sched_param param = {};
  param.sched_priority = 0;
  
  if (pthread_setschedparam(thread, policy, &param) == 0)
    return true;
  else
    return false;
#endif
}
  
double JUCE_audio_get_sample_rate(void){
  return g_juce_player->_samplerate;
}

int JUCE_audio_get_buffer_size(void){
  return g_juce_player->_buffer_size;
};

int JUCE_get_num_xruns(void){
  if (g_juce_player==NULL){
    R_ASSERT_NON_RELEASE(false);
    return 0;
  }

  return g_juce_player->get_num_xruns();
}

double JUCE_audio_time_at_cycle_start(void){
  //R_ASSERT_NON_RELEASE(THREADING_is_player_thread());
#if !defined(RELEASE)
  if (!THREADING_is_player_thread())
    printf("JUCE_audio_time_at_cycle_start: Warning, not called from player thread\n");
#endif
  return g_juce_player->_time_cycle_start;
  //return ATOMIC_DOUBLE_GET(g_juce_player->_time_cycle_start);
}

void JUCE_audio_open_preferences_window(void){
  if (g_juce_player==NULL){
    GFX_Message(NULL, "Radium is currently using Jack. Can not configure audio inside Radium when using Jack.");
    return;
  }
  
  juce::MessageManager::getInstance()->callAsync([](){
      g_juce_player->prefs();
    });
}

void JUCE_audio_close_preferences_window(void){
  run_on_message_thread([](){
      delete radium::g_prefs_dialog;
    });
}

bool JUCE_init_audio_device(JUCE_audio_device_callback callback, void *callback_data, std::function<void(int,float)> called_before_starting_audio){
  const wchar_t *settings_string = SETTINGS_read_wchars("audio_device", L"");

  run_on_message_thread([&](){
    g_juce_player = new radium::JucePlayer(callback, callback_data, settings_string, called_before_starting_audio);
  });
  
  return g_juce_player->_is_running;
}

void JUCE_stop_audio_device(void){
  run_on_message_thread([](){
      delete g_juce_player;
    });
}


int g_juce_num_input_audio_channels;
const float **g_juce_input_audio_channels;

int g_juce_num_output_audio_channels;  
float **g_juce_output_audio_channels;

  
namespace radium{

class PrefsWindow;
  
static class PrefsWindow *g_prefs_dialog = NULL;


class PrefsWindow
  : public juce::DialogWindow
#if defined(FOR_LINUX)
  , public juce::Timer
#endif
{
  
public:
  
  PrefsWindow()
    : juce::DialogWindow("Audio Settings", juce::Colours::black, true)
  {
    g_prefs_dialog = this;
#if defined(FOR_LINUX)
    startTimer(1000);
#endif
  }

  ~PrefsWindow(){
    g_prefs_dialog = NULL;
  }

#if defined(FOR_LINUX)
  void timerCallback() final {
    //printf("TIMERCALLGACN\n");
    toFront(false); // Linux: shouldAlsoGainFocus==false doesn't work.
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

  bool _legal_blocksize = true;
  
public:

  double _last_reported_samplerate = -1;
  double _samplerate;
  int _buffer_size;
  bool _is_running = false;
  
  double _time_cycle_start = 0;
  //DEFINE_ATOMIC(double, _time_cycle_start) = 0;
  
  JucePlayer(JUCE_audio_device_callback callback, void *callback_data, const wchar_t *settings_string)
    : _callback(callback)
    , _callback_data(callback_data)
  {
    _audio_device_manager.addChangeListener(this);
    
    _is_running = initJuceAudio(settings_string);
  }

  ~JucePlayer(){
    delete g_prefs_dialog;
  }
  
  void changeListenerCallback(juce::ChangeBroadcaster *source) override {
    
    printf("Some audio change thing: %p\n", _audio_device_manager.getCurrentAudioDevice());
    
    if(_audio_device_manager.getCurrentAudioDevice()!=NULL){

      
      double old_samplerate = _last_reported_samplerate;

      _buffer_size = _audio_device_manager.getCurrentAudioDevice()->getCurrentBufferSizeSamples();

      double new_samplerate = _audio_device_manager.getCurrentAudioDevice()->getCurrentSampleRate();
      _last_reported_samplerate = new_samplerate;
      
      _settings = _audio_device_manager.createStateXml();

      if (_settings.get()!=NULL) {

        const wchar_t *w = _settings->toString().toWideCharPointer();
        
        THREADING_run_on_main_thread_and_wait([w] // Must wait since 'w' is probably deleted when we go out of scope.
                                              {
                                                SETTINGS_write_wchars("audio_device", STRING_replace(STRING_replace(w, "\r\n", " "), "\n", " "));
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
      
      fprintf(stderr,"Gakkegakke\n");
      
    }
  }
  
  bool initJuceAudio(const wchar_t *settings_string){
    
    if (wcslen(settings_string) > 0)
      _settings = juce::XmlDocument::parse(juce::String(settings_string));

    const juce::String error (_audio_device_manager.initialise (8, /* number of input channels */
                                                                8, /* number of output channels */
                                                                _settings.get(),
                                                                true  /* select default device on failure */));
    
    if (_audio_device_manager.getCurrentAudioDevice()==NULL || error.isNotEmpty()) {

      // We also give error in Mixer.cpp though, but the error message might be useful. Should never happen though. I think JUCE falls back to dummy driver.
      GFX_Message(NULL, "Audio device manager: \"%S\"", error.toWideCharPointer());
      
      return false;
      
    } else {
      
      //isinitialized=true;
	
      // start the IO device pulling its data from our callback..
      _audio_device_manager.addAudioCallback (this);
      return true;
      
    }
  }

  bool _has_inited = false;
  
  void audioDeviceIOCallback(const float ** 	inputChannelData, 
			     int 	totalNumInputChannels, 
			     float ** 	outputChannelData, 
			     int 	totalNumOutputChannels, 
			     int 	numSamples
			     )
    override
  {
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
    
    if (_has_inited==false){
      THREADING_acquire_player_thread_priority();
      _has_inited=true;
    }

    for(int i=0;i<totalNumOutputChannels;i++)
      if(outputChannelData[i]!=NULL){
        memset(outputChannelData[i], 0, sizeof(float)*numSamples);
      }
    
    g_juce_num_input_audio_channels = totalNumInputChannels;
    g_juce_input_audio_channels = inputChannelData;

    g_juce_num_output_audio_channels = totalNumOutputChannels;
    g_juce_output_audio_channels = outputChannelData;

    _callback(numSamples, _samplerate, _callback_data);
    
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

    g_prefs_dialog = new PrefsWindow();

    g_prefs_dialog->setOpaque(true);
    g_prefs_dialog->addToDesktop();
    g_prefs_dialog->setUsingNativeTitleBar(true);

    g_prefs_dialog->setContentOwned(audioSettingsComp, true);

    g_prefs_dialog->centreWithSize(g_prefs_dialog->getWidth(), g_prefs_dialog->getHeight());
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
    
#if FOR_WINDOWS
    {
      void *parent = API_get_native_gui_handle(-1);
      if (parent != NULL)
        OS_WINDOWS_set_window_on_top_of(parent, g_prefs_dialog->getWindowHandle());
      else{
        R_ASSERT(false);
      }
    }
#endif

    /*
    if(_audio_device_manager.getCurrentAudioDevice()==NULL)
      printf("gakk\n");
    */
  }
  
  void audioDeviceAboutToStart (juce::AudioIODevice *device) override
  {
    double sampleRate = device->getCurrentSampleRate ();
    
    //printf("Samplerate set to %f\n",(float)sampleRate);
    
    _samplerate = sampleRate;

    g_audio_system_input_latency = device->getInputLatencyInSamples();
    g_audio_system_output_latency = device->getOutputLatencyInSamples();

#if !defined(RELEASE)
    printf("Latency in/out: %d / %d\n", g_audio_system_input_latency, g_audio_system_output_latency);
#endif
    
    int buffer_size = device->getCurrentBufferSizeSamples();

    if (buffer_size < RADIUM_BLOCKSIZE || (buffer_size % RADIUM_BLOCKSIZE) != 0)
      GFX_Message(NULL,
                  "Error: Soundcard buffer size must be a multiple of the internal block size."
                  "<UL>"
                  "<LI>Soundcard buffer size: %d"
                  "<LI>Internal block size: %d"
                  "</UL>"
                  "You can adjust the internal block size under <b>Edit -> Preferences -> Audio -> internal block size.</b>",
                  buffer_size, RADIUM_BLOCKSIZE);
  }
  
  void audioDeviceStopped() override
  {
    printf("Audio device stopped\n");
    return;
  }


};

}

static radium::JucePlayer *g_juce_player = NULL;

double JUCE_audio_get_sample_rate(void){
  return g_juce_player->_samplerate;
}

int JUCE_audio_get_buffer_size(void){
  return g_juce_player->_buffer_size;
};

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

bool JUCE_init_audio_device(JUCE_audio_device_callback callback, void *callback_data){
  const wchar_t *settings_string = SETTINGS_read_wchars("audio_device", L"");

  run_on_message_thread([&](){
      g_juce_player = new radium::JucePlayer(callback, callback_data, settings_string);
    });
  
  return g_juce_player->_is_running;
}

void JUCE_stop_audio_device(void){
  run_on_message_thread([](){
      delete g_juce_player;
    });
}

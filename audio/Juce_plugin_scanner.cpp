#if defined(COMPILING_RADIUM_PLUGIN_SCANNER)

#include <math.h>
#include <string.h>

#define DONT_SET_USING_JUCE_NAMESPACE 1
#define JUCE_DONT_DECLARE_PROJECTINFO 1

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


/*
static bool container_descriptions_are_cached_on_disk(const wchar_t *container_filename){
  String filename = get_container_descriptions_filename(container_filename);
  File file(filename);
  return file.existsAsFile();
}
*/

static void show_alert(juce::String message){
  fprintf(stderr," show_alert: -%s-\n", message.toRawUTF8());
#if FOR_MACOSX
  // AlertWindow::showMessageBox didn't work right out of the box on OSX. But this workaround is better anyway since it doesn't block the execution, and still there will only be maximum one message window open at the same time.
  juce::String command = "osascript -e 'tell application \"Finder\"' -e 'activate' -e 'display dialog \"Radium plugin scanner: " + message + "\" buttons {\"OK\"}' -e 'end tell'&"; // https://stackoverflow.com/questions/13484482/no-user-interaction-allowed-when-running-applescript-in-python
  system(command.toRawUTF8());
#else
  juce::initialiseJuce_GUI();
  juce::AlertWindow::showMessageBox(juce::AlertWindow::AlertIconType::WarningIcon,
                              "Radium plugin scanner",
                              message);
#endif
}

// This function calls functions in JUCE that loads the plugins in order to create PluginDescription objects.
// The function might crash for buggy plugins.
//
static void add_descriptions_from_plugin_file(juce::OwnedArray<juce::PluginDescription> &descriptions, juce::String description_filename){
  //  CRASHREPORTER_dont_report();{ // If findAllTypesForFile crashes, it's the plugin that crashes and not Radium (and we don't want to show a message about "Radium crashing" when it actually didn't). If we crash here, the plugin is blacklisted, and that's the important thing.

    juce::VSTPluginFormat vst2_format;
    vst2_format.findAllTypesForFile(descriptions, description_filename);
    
#if !defined(FOR_LINUX)
    juce::VST3PluginFormat vst3_format;
    vst3_format.findAllTypesForFile(descriptions, description_filename);
#endif
    
#if FOR_MACOSX
    juce::AudioUnitPluginFormat au_format;
    au_format.findAllTypesForFile(descriptions, description_filename);
#endif

  //}CRASHREPORTER_do_report();
}

static void write_container_descriptions_to_cache_on_disk(juce::String container_filename, juce::String description_filename){
  juce::OwnedArray<juce::PluginDescription> descriptions;
  add_descriptions_from_plugin_file(descriptions, container_filename); // BANG!

  juce::XmlElement xml_descriptions("plugin_descriptions");
  
  for (auto description : descriptions){
    juce::XmlElement *xml = new juce::XmlElement(*description->createXml().get()); // This can probably be done in a simpler way.
    xml_descriptions.addChildElement(xml);
  }

  //testing
  //Thread::sleep(10000);
  
  //fprintf(stderr, "===...===...   %s: GOING TO Write Plugin description file \"%s\".\n", juce::String(container_filename).toRawUTF8(), description_filename.toRawUTF8());

  juce::File file(description_filename);

  juce::XmlElement::TextFormat options;
  options.dtd = "";
  options.customEncoding = "UTF-8";
  options.lineWrapLength = 60;

  
  if (xml_descriptions.writeTo(file, options)==false){
    //GFX_Message2(NULL, true, "Error: Unable to write to file \"%s\".\n", description_filename.toRawUTF8());
    fprintf(stdout, "Error: Unable to write to file \"%s\".\n", description_filename.toRawUTF8());
    show_alert(juce::String("Unable to write to file \"") + description_filename + "\"");
  }
}


#if 0
  struct MyMidiInputCallback : juce::MidiInputCallback{
    juce::MidiInput *midi_input;
    juce::String port_name;

    MyMidiInputCallback()
      : midi_input(NULL)
    {}

    /*
    ~MyMidiInputCallback(){
      printf("_________________Deleted \"%s\"\n", port_name->name);
      getchar();
    }
    */
    
    void handleIncomingMidiMessage(juce::MidiInput *source,
                                   const juce::MidiMessage &message 
                                   )
      override
    {

      /*
      int64_t message_ms = message.timeStamp;
      int64_t now_ms = Time::getMillisecondCounter();
      */

      //printf("got message to %s (%d %d %d)\n",(const char*)midi_input->getName().toUTF8(),(int)raw[0],(int)raw[1],(int)raw[2]);
    }
  };

static void testmidi(void){
  printf("HELLO!\n");
  fprintf(stderr,"HELLO!\n");
  fflush(stdout);
  fflush(stderr);

  juce::initialiseJuce_GUI();
      
  auto *midi_input_callback = new MyMidiInputCallback();

  {
    juce::MidiInput *midi_input = NULL;
    
    midi_input = juce::MidiInput::openDevice(0, midi_input_callback);
    
    midi_input_callback->midi_input = midi_input;
    midi_input_callback->port_name = midi_input->getName();
    
    midi_input->start();
  }

  juce::MessageManager::getInstance()->runDispatchLoopUntil(1000);

  midi_input_callback->midi_input->stop();
  delete midi_input_callback->midi_input;
  delete midi_input_callback;

  juce::shutdownJuce_GUI();

  //juce::MessageManager::getInstance()->stopDispatchLoop();
  //juce::MessageManager::getInstance()->deleteInstance();
}
#endif

int main(int argc, char **argv){

  //printf("Launched 1 -%s- (%s)\n",argv[1], Base64::toBase64(argv[1]).toRawUTF8());
  //fprintf(stderr,"Launched 2 -%s-\n",argv[2]);

  /*
  char container_filename_data[1024] = {0};
  char filename_data[1024] = {0};
  */

#if 0
  
  testmidi();
  
#else
  
  juce::MemoryOutputStream a(1024);
  juce::MemoryOutputStream b(1024);
  
  //show_alert("testing alerting");

  if (argc == 2 && !strcmp(argv[1], "test_backtrace")){
    juce::SystemStats::getStackBacktrace();
    //juce::Thread::sleep(1000*100);
    return 0;
  }
  
  if (argc != 3){
    show_alert(juce::String("Wrong number of arguments. Expected 2, found ") + juce::String::formatted("%d.", argc-1));
    return -3;
  }
  
  if (juce::Base64::convertFromBase64(a, argv[1])==false){
    printf("1: Unable to convert base64 -%s-\n",argv[1]);
    show_alert(juce::String("Erroneous input arguments: \"") + juce::String(argv[1]) + "\", \"" + juce::String(argv[2]) + "\"");
    return -1;
  }
     
  if (juce::Base64::convertFromBase64(b, argv[2])==false){
    printf("2: Unable to convert base64 -%s-\n",argv[2]);
    show_alert(juce::String("Erroneous input arguments: \"") + juce::String(argv[1]) + "\", \"" + juce::String(argv[2]) + "\"");
    return -2;
  }

  juce::String container_filename = a.toUTF8();
  juce::String description_filename = b.toUTF8();
  
  printf("Launched: -%s- -%s-\n", container_filename.toRawUTF8(), description_filename.toRawUTF8());
  write_container_descriptions_to_cache_on_disk(container_filename, description_filename);

#endif
  
  return 0;
}


#endif //  defined(COMPILING_RADIUM_PLUGIN_SCANNER)

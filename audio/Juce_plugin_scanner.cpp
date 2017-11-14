#if defined(COMPILING_RADIUM_PLUGIN_SCANNER)

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


/*
static bool container_descriptions_are_cached_on_disk(const wchar_t *container_filename){
  String filename = get_container_descriptions_filename(container_filename);
  File file(filename);
  return file.existsAsFile();
}
*/

static void show_alert(String message){
  fprintf(stderr," show_alert: -%s-\n", message.toRawUTF8());
#if FOR_MACOSX
  // AlertWindow::showMessageBox didn't work right out of the box on OSX. But this workaround is better anyway since it doesn't block the execution, and still there will only be maximum one message window open at the same time.
  String command = "osascript -e 'tell application \"Finder\"' -e 'activate' -e 'display dialog \"Radium plugin scanner: " + message + "\" buttons {\"OK\"}' -e 'end tell'&"; // https://stackoverflow.com/questions/13484482/no-user-interaction-allowed-when-running-applescript-in-python
  system(command.toRawUTF8());
#else
  initialiseJuce_GUI();
  AlertWindow::showMessageBox(AlertWindow::AlertIconType::WarningIcon,
                              "Radium plugin scanner",
                              message);
#endif
}

// This function calls functions in JUCE that loads the plugins in order to create PluginDescription objects.
// The function might crash for buggy plugins.
//
static void add_descriptions_from_plugin_file(OwnedArray<PluginDescription> &descriptions, String description_filename){
  //  CRASHREPORTER_dont_report();{ // If findAllTypesForFile crashes, it's the plugin that crashes and not Radium (and we don't want to show a message about "Radium crashing" when it actually didn't). If we crash here, the plugin is blacklisted, and that's the important thing.

    VSTPluginFormat vst2_format;
    vst2_format.findAllTypesForFile(descriptions, description_filename);
    
#if !defined(FOR_LINUX)
    VST3PluginFormat vst3_format;
    vst3_format.findAllTypesForFile(descriptions, description_filename);
#endif
    
#if FOR_MACOSX
    AudioUnitPluginFormat au_format;
    au_format.findAllTypesForFile(descriptions, description_filename);
#endif

  //}CRASHREPORTER_do_report();
}

static void write_container_descriptions_to_cache_on_disk(String container_filename, String description_filename){
  OwnedArray<PluginDescription> descriptions;
  add_descriptions_from_plugin_file(descriptions, container_filename); // BANG!

  XmlElement xml_descriptions("plugin_descriptions");
  
  for (auto description : descriptions){
    XmlElement *xml = description->createXml();
    xml_descriptions.addChildElement(xml);
  }

  //testing
  //Thread::sleep(10000);
  
  //fprintf(stderr, "===...===...   %s: GOING TO Write Plugin description file \"%s\".\n", String(container_filename).toRawUTF8(), description_filename.toRawUTF8());

  File file(description_filename);
  
  if (xml_descriptions.writeToFile(file, "")==false){
    //GFX_Message2(NULL, true, "Error: Unable to write to file \"%s\".\n", description_filename.toRawUTF8());
    fprintf(stdout, "Error: Unable to write to file \"%s\".\n", description_filename.toRawUTF8());
    show_alert(String("Unable to write to file \"") + description_filename + "\"");
  }
}

int main(int argc, char **argv){

  //printf("Launched 1 -%s- (%s)\n",argv[1], Base64::toBase64(argv[1]).toRawUTF8());
  //fprintf(stderr,"Launched 2 -%s-\n",argv[2]);

  /*
  char container_filename_data[1024] = {0};
  char filename_data[1024] = {0};
  */
  
  MemoryOutputStream a(1024);
  MemoryOutputStream b(1024);
  
  //show_alert("testing alerting");
  
  if (argc != 3){
    show_alert(String("Wrong number of arguments. Expected 2, found ") + String::formatted("%d.", argc-1));
    return -3;
  }
  
  if (Base64::convertFromBase64(a, argv[1])==false){
    printf("1: Unable to convert base64 -%s-\n",argv[1]);
    show_alert(String("Erroneous input arguments: \"") + String(argv[1]) + "\", \"" + String(argv[2]) + "\"");
    return -1;
  }
     
  if (Base64::convertFromBase64(b, argv[2])==false){
    printf("2: Unable to convert base64 -%s-\n",argv[2]);
    show_alert(String("Erroneous input arguments: \"") + String(argv[1]) + "\", \"" + String(argv[2]) + "\"");
    return -2;
  }

  String container_filename = a.toUTF8();
  String description_filename = b.toUTF8();
  
  printf("Launched: -%s- -%s-\n", container_filename.toRawUTF8(), description_filename.toRawUTF8());
  write_container_descriptions_to_cache_on_disk(container_filename, description_filename);
  return 0;
}


#endif //  defined(COMPILING_RADIUM_PLUGIN_SCANNER)

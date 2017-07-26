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


// This function calls functions in JUCE that loads the plugins in order to create PluginDescription objects.
// The function might crash for buggy plugins.
//
static void add_descriptions_from_plugin_file(OwnedArray<PluginDescription> &descriptions, String filename){
  //  CRASHREPORTER_dont_report();{ // If findAllTypesForFile crashes, it's the plugin that crashes and not Radium (and we don't want to show a message about "Radium crashing" when it actually didn't). If we crash here, the plugin is blacklisted, and that's the important thing.
    
    VSTPluginFormat vst2_format;
    vst2_format.findAllTypesForFile(descriptions, filename);
    
#if !defined(FOR_LINUX)
    VST3PluginFormat vst3_format;
    vst3_format.findAllTypesForFile(descriptions, filename);
#endif
    
#if FOR_MACOSX
    AudioUnitPluginFormat au_format;
    au_format.findAllTypesForFile(descriptions, filename);
#endif

  //}CRASHREPORTER_do_report();
}

static void write_container_descriptions_to_cache_on_disk(String container_filename, String filename){
  OwnedArray<PluginDescription> descriptions;
  add_descriptions_from_plugin_file(descriptions, container_filename); // BANG!

  XmlElement xml_descriptions("plugin_descriptions");
  
  for (auto description : descriptions){
    XmlElement *xml = description->createXml();
    xml_descriptions.addChildElement(xml);
  }

  //testing
  //Thread::sleep(10000);
  
  fprintf(stderr, "===...===...   %s: GOING TO Write Plugin description file \"%s\".\n", String(container_filename).toRawUTF8(), filename.toRawUTF8());
    
  File file(filename);
  
  if (xml_descriptions.writeToFile(file, "")==false)
    //GFX_Message2(NULL, true, "Error: Unable to write to file \"%s\".\n", filename.toRawUTF8());
    fprintf(stdout, "Error: Unable to write to file \"%s\".\n", filename.toRawUTF8());
}

int main(int argc, char **argv){

  printf("Launched 1 -%s- (%s)\n",argv[1], Base64::toBase64(argv[1]).toRawUTF8());
  fprintf(stderr,"Launched 2 -%s-\n",argv[2]);

  char container_filename[1024] = {0};
  char filename[1024] = {0};
  
  MemoryOutputStream a(container_filename, 1000);
  MemoryOutputStream b(filename, 1000);

  if (Base64::convertFromBase64(a, argv[1])==false){
    printf("1: Unable to convert base64 -%s-\n",argv[1]);
    return -1;
  }
     
  if (Base64::convertFromBase64(b, argv[2])==false){
    printf("2: Unable to convert base64 -%s-\n",argv[2]);
    return -2;
  }
     
  printf("Launched: -%s- -%s-\n", container_filename, filename);
  write_container_descriptions_to_cache_on_disk(container_filename, filename);
  return 0;
}


#endif //  defined(COMPILING_RADIUM_PLUGIN_SCANNER)

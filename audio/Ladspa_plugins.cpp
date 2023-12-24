/* Copyright 2012 Kjetil S. Matheussen

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


#include <inttypes.h>
#include <algorithm>
#include <vector>

#include <QDesktopServices>

#include <math.h>
#include <dirent.h>
#include <errno.h>
#include <string.h>

#include <lrdf.h>
#include <ladspa.h>

#include <QFileInfo>
#include <QLibrary>
#include <QDir>
#include <QDirIterator>
#include <QHash>

#if defined(__linux__)
#  define LIB_SUFFIX "so"
#endif

#if defined(FOR_MACOSX)
#  define LIB_SUFFIX "so"
#endif

#if defined(FOR_WINDOWS)
#define LIB_SUFFIX "dll"
#endif

#include "../common/nsmtracker.h"
#include "../common/OS_settings_proc.h"
#include "../common/OS_disk_proc.h"
#include "../common/vector_proc.h"
#include "../common/visual_proc.h"
#include "../common/threading.h"
#include "../common/settings_proc.h"
#include "../common/disk.h"

#include "../crashreporter/crashreporter_proc.h"

#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"
#include "SoundPluginRegistry_proc.h"
#include "Mixer_proc.h"

namespace{
  
struct Data{
  LADSPA_Handle handles[2];
  float *control_values;  
  float **inputs;
  float **outputs;
  unsigned long *input_portnums;
  unsigned long *output_portnums;
  float latency_output_control_port;
};

 
class QLibraryHolder{
  QString _filename;
  QLibrary *_qlibrary = NULL;
  bool _is_loaded = false; // Need to keep local track. Seems like QLibrary::isLoaded() also returns true if the library was loaded in the system, not just inside the QLibrary.

public:
  
  QLibraryHolder(QString filename)
    : _filename(filename)
  {}

  QLibrary *get(void){
    if (!is_loaded()) {
      R_ASSERT_NON_RELEASE(false);
      load();
    }
    
    return _qlibrary;
  }

  bool has_library(void){
    return _qlibrary != NULL;
  }

  bool is_loaded(void){
    return _qlibrary!=NULL && _is_loaded;
  }

  void load(void){
    if (_qlibrary==NULL)
      _qlibrary = new QLibrary(_filename);
    
    _qlibrary->load();
    _is_loaded = true;
  }
  
  void maybe_unload(void){
    if (_qlibrary != NULL)
      _qlibrary->unload();
    _is_loaded = false;
  }
};
 

// Used to avoid having lots of unused dynamic libraries loaded into memory at all time (and sometimes using up TLS)
struct Library{
  
  const wchar_t *filename = NULL; // used for error messages
  LADSPA_Descriptor_Function get_descriptor_func = NULL;
  int num_instances = 0;
  int num_library_references = 0; // library is unloaded when this value decreases from 1 to 0, and loaded when increasing from 0 to 1.
  int num_times_loaded = 0; // for debugging

private:
  QLibraryHolder _qlibrary;

  void unload(void){
    
    R_ASSERT_NON_RELEASE(_qlibrary.has_library());
    
    _qlibrary.maybe_unload();

    get_descriptor_func = NULL;
  }

  
  bool load_descriptor_func(void){

    R_ASSERT_NON_RELEASE(this->get_descriptor_func==NULL);
    
    LADSPA_Descriptor_Function get_descriptor_func = (LADSPA_Descriptor_Function) _qlibrary.get()->resolve("ladspa_descriptor");
    
    if(get_descriptor_func==NULL){
      GFX_Message(NULL, "Unable to load plugin. Has the plugin file \"%S\" disappeared?", filename);
      return false;
    }
  
    this->get_descriptor_func = get_descriptor_func;
    return true;
  }

public:
  Library(QString qfilename)
    : _qlibrary(qfilename)
  {
    filename = STRING_create(qfilename, false); // ("false" means not GC-allocated)
  }

#if !defined(RELEASE)
  ~Library(){
    abort(); // not supposed to happen
  }
#endif
  
  bool add_reference(void){

    R_ASSERT_NON_RELEASE(num_library_references >= 0);
      
    if (num_library_references==0){
      printf("**** Loading %S\n",filename);

      /*
      if (STRING_equals(filename, "/tmp/radium_bin/ladspa/am_pitchshift_1433.so")){
        printf("    (press return)\n");
        getchar();
      }
      */

#if !defined(RELEASE)
      if (_qlibrary.is_loaded()){
        printf("    Error. (press return)\n");
        getchar();
      }
#endif
                           
      _qlibrary.load();
      
      if (!load_descriptor_func()) {
        unload();
        return false;
      }
      
    } else {

      R_ASSERT_NON_RELEASE(_qlibrary.is_loaded());
      
    }
    
    num_library_references++;
    num_times_loaded++;
    
    return true;
  }

  void remove_reference(void){
    R_ASSERT_NON_RELEASE(num_library_references > 0);
    R_ASSERT_NON_RELEASE(_qlibrary.is_loaded());
        
    num_library_references--;

    if (num_library_references==0) {
      printf("**** Unloading %S\n",filename);

      /*
      if (STRING_equals(filename, "/tmp/radium_bin/ladspa/am_pitchshift_1433.so")){
        printf("    (press return)\n");
        getchar();
      }
      */
      
      unload();
    }
  }

};
 


struct TypeData{
  bool has_content;
  
  Library *library; // Referenced from here.
  
  const LADSPA_Descriptor *descriptor;
  int num_typedata_references; // descriptor is initialized when library->num_library_references changes value from 0 to 1, and deleted when changed from 1 to 0.
  
  int index; // index in this file

  unsigned long UniqueID; // same value as descriptor->UniqueID, but copied here to avoid loading the library to get the value.
  const char *Name; // same here

  float output_control_port_value;
  float *min_values;
  float *default_values;
  float *max_values;
  bool uses_two_handles;

  LADSPA_PortRangeHintDescriptor *hint_descriptors;
  const char **effect_names;
};
}

static std::vector<SoundPluginType*> g_plugin_types;

static void RT_process(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs){
  const SoundPluginType *type = plugin->type;
  TypeData *type_data = (TypeData*)type->data;
  const LADSPA_Descriptor *descriptor = type_data->descriptor;
  Data *data = (Data*)plugin->data;

  // Make sure input and output audio ports are set correctly.
  //
  if(type_data->uses_two_handles) {
    
    if (data->inputs[0] != inputs[0]){
      descriptor->connect_port(data->handles[0], data->input_portnums[0], inputs[0]);
      data->inputs[0] = inputs[0];
      //printf("1: %s\n", plugin->patch->name);
    }
    
    if (data->inputs[1] != inputs[1]){
      descriptor->connect_port(data->handles[1], data->input_portnums[0], inputs[1]);
      data->inputs[1] = inputs[1];
      //printf("2: %s\n", plugin->patch->name);
    }
    
    if (data->outputs[0] != outputs[0]){
      descriptor->connect_port(data->handles[0], data->output_portnums[0], outputs[0]);
      data->outputs[0] = outputs[0];
      //printf("3: %s\n", plugin->patch->name);
    }
    
    if (data->outputs[1] != outputs[1]){
      descriptor->connect_port(data->handles[1], data->output_portnums[0], outputs[1]);
      data->outputs[1] = outputs[1];
      //printf("4: %s\n", plugin->patch->name);
    }
    
  } else {

    for(int ch=0;ch<type->num_inputs;ch++)
      if (data->inputs[ch] != inputs[ch]){
        descriptor->connect_port(data->handles[0], data->input_portnums[ch], inputs[ch]);
        data->inputs[ch] = inputs[ch];
        //printf("5: %s. ch: %d\n", plugin->patch->name, ch);
      }

    for(int ch=0;ch<type->num_outputs;ch++)
      if (data->outputs[ch] != outputs[ch]){
        descriptor->connect_port(data->handles[0], data->output_portnums[ch], outputs[ch]);
        data->outputs[ch] = outputs[ch];
        //printf("6: %s. ch: %d\n", plugin->patch->name, ch);
      }

  }

  // Process
  //
  {
    int pos = CRASHREPORTER_set_plugin_name(plugin->type->name); {
      descriptor->run(data->handles[0], num_frames);
      if(type_data->uses_two_handles)
        descriptor->run(data->handles[1], num_frames);
    } CRASHREPORTER_unset_plugin_name(pos);
  }
}

static int RT_get_latency(const struct SoundPlugin *plugin){
  Data *data = (Data*)plugin->data;

  int latency = data->latency_output_control_port;

  /*
  if (latency != 0)
    printf("plugin %s has latency %d\n",plugin->patch->name, latency);
  */
  
  if (latency < 0)
    latency = 0;
  
  return latency;
}

static void delete_audio_ports(const SoundPluginType *type, Data *data){
  V_free(data->inputs);
  V_free(data->outputs);
  V_free(data->input_portnums);
  V_free(data->output_portnums);
}

static void setup_audio_ports(const SoundPluginType *type, Data *data, int block_size){
  TypeData *type_data = (TypeData*)type->data;
  const LADSPA_Descriptor *descriptor = type_data->descriptor;

  data->inputs=(float**)V_calloc(sizeof(float*),type->num_inputs);
  data->outputs=(float**)V_calloc(sizeof(float*),type->num_outputs);
  
  data->input_portnums=(unsigned long*)V_calloc(sizeof(unsigned long),type->num_inputs);
  data->output_portnums=(unsigned long*)V_calloc(sizeof(unsigned long),type->num_outputs);
  
  {
    int input_num = 0;
    int output_num = 0;

    for(unsigned int portnum=0;portnum<descriptor->PortCount;portnum++){
      const LADSPA_PortDescriptor portdescriptor = descriptor->PortDescriptors[portnum];

      if(LADSPA_IS_PORT_AUDIO(portdescriptor) && LADSPA_IS_PORT_INPUT(portdescriptor)){
        data->input_portnums[input_num] = portnum;
        input_num++;
      }
    
      if(LADSPA_IS_PORT_AUDIO(portdescriptor) && LADSPA_IS_PORT_OUTPUT(portdescriptor)){
        data->output_portnums[output_num] = portnum;
        output_num++;
      }

    }
  }
}

static bool add_type_data_reference(TypeData *type_data){

  if (type_data->num_typedata_references==0){

    R_ASSERT(type_data->descriptor==NULL);

    if (type_data->library->add_reference()==false)
      return false;
    
    type_data->descriptor = type_data->library->get_descriptor_func(type_data->index);
  
    if (type_data->descriptor==NULL) {
      GFX_Message(NULL, "Unable to load plugin #%d in file \"%S\". That is not supposed to happen since it was possible to load the plugin when the program was initializing.", type_data->index, type_data->library->filename);
      type_data->library->remove_reference();
      return false;
    }
  }

  type_data->num_typedata_references++;
  return true;
}
  
static void remove_type_data_reference(TypeData *type_data){
  type_data->num_typedata_references--;

  if (type_data->num_typedata_references==0){
    type_data->library->remove_reference();
    type_data->descriptor = NULL;
  }
}

static LADSPA_PortRangeHintDescriptor get_hintdescriptor(const LADSPA_Descriptor *descriptor, int effect_num){

  int effect_num2 = 0;
  for(unsigned int portnum=0;portnum<descriptor->PortCount;portnum++){
    const LADSPA_PortDescriptor portdescriptor = descriptor->PortDescriptors[portnum];
    if(LADSPA_IS_PORT_CONTROL(portdescriptor) && LADSPA_IS_PORT_INPUT(portdescriptor)){
      if(effect_num2==effect_num){
        const LADSPA_PortRangeHint *range_hint = &descriptor->PortRangeHints[portnum];
        const LADSPA_PortRangeHintDescriptor hints = range_hint->HintDescriptor;
        return hints;
      }else
        effect_num2++;
    }
  }

  RWarning("Unknown effect %d for Ladspa plugin \"%s\"\n", effect_num, descriptor->Name);
  return 0;
}

static void fill_in_type_data_info_from_descriptor(TypeData *type_data, const SoundPluginType *plugin_type, const LADSPA_Descriptor *descriptor){
  if (type_data->has_content)
    return;
  
  type_data->has_content = true;
  
  //type_data->descriptor = descriptor; // We unload the library later in this function, and then 'descriptor' won't be valid anymore.
  type_data->UniqueID = descriptor->UniqueID;
  type_data->Name = V_strdup(descriptor->Name);
    
  type_data->min_values     = (float*)V_calloc(sizeof(float),plugin_type->num_effects);
  type_data->default_values = (float*)V_calloc(sizeof(float),plugin_type->num_effects);
  type_data->max_values     = (float*)V_calloc(sizeof(float),plugin_type->num_effects);

  // Find type_data->hint_descriptors.
  // type_data->hint_descriptor maps effect_num to descriptor for effect_num. (speeds up set_effect())
  //
  // Note that we are not storing pointers to LADSPA_PortRangeHintDescriptor (which would not work). Instead
  // we are copying the memory areas of LADSPA_PortRangeHintDescriptor from the dynamically loaded library and into type_data.
  // (LADSPA_PortRangeHintDescriptor is actually just an integer)
  //
  type_data->hint_descriptors = (LADSPA_PortRangeHintDescriptor*)V_calloc(sizeof(LADSPA_PortRangeHintDescriptor), plugin_type->num_effects);
  for(int i = 0 ; i < plugin_type->num_effects ; i++)
    type_data->hint_descriptors[i] = get_hintdescriptor(descriptor,i);

  // Find type_data->min_values and type_data->max_values
  //
  {
    int effect_num = 0;
    type_data->effect_names = (const char**)V_calloc(sizeof(char*),plugin_type->num_effects);
      
    for(unsigned int portnum=0;portnum<descriptor->PortCount;portnum++){
      const LADSPA_PortDescriptor portdescriptor = descriptor->PortDescriptors[portnum];

      //if(!strcmp(plugin_type->type_name,"Simple Low Pass Filter"))
      //  printf("Handling %s. portcount: num_ports: %d. is_control: %d is_input: %d\n",plugin_type->name,(int)descriptor->PortCount,LADSPA_IS_PORT_CONTROL(portdescriptor), LADSPA_IS_PORT_INPUT(portdescriptor));

      if(LADSPA_IS_PORT_CONTROL(portdescriptor) && LADSPA_IS_PORT_INPUT(portdescriptor)){
        const LADSPA_PortRangeHint *range_hint = &descriptor->PortRangeHints[portnum];
        LADSPA_PortRangeHintDescriptor hints = range_hint->HintDescriptor;

        if(LADSPA_IS_HINT_BOUNDED_BELOW(hints))
          type_data->min_values[effect_num] = range_hint->LowerBound;

        if(LADSPA_IS_HINT_BOUNDED_ABOVE(hints))
          type_data->max_values[effect_num] = range_hint->UpperBound;
        else
          type_data->max_values[effect_num] = 1.0f;
          
        if(LADSPA_IS_HINT_SAMPLE_RATE(hints)){
          type_data->min_values[effect_num] *= 44100.0f; //MIXER_get_sample_rate(); No, the number must be a constant.
          type_data->max_values[effect_num] *= 44100.0f; //MIXER_get_sample_rate(); Same here.
        }

        float min_value = type_data->min_values[effect_num];
        float max_value = type_data->max_values[effect_num];
        float default_value = 0.0f;

        //if(!strcmp(plugin_type->type_name,"Simple Low Pass Filter"))
        //  printf("Before hints. min/max/default: %f/%f/%f\n",min_value,max_value,default_value);

        if(LADSPA_IS_HINT_HAS_DEFAULT(hints)){
            
          // This switch block is copied from QTracktor, and modified.
          switch (hints & LADSPA_HINT_DEFAULT_MASK) {
            case LADSPA_HINT_DEFAULT_MINIMUM:
              default_value = min_value;
              break;
            case LADSPA_HINT_DEFAULT_LOW:
              if (LADSPA_IS_HINT_LOGARITHMIC(hints)){
                default_value = ::expf(
                                       ::logf(min_value) * 0.75f + ::logf(max_value) * 0.25f);
              } else {
                default_value = (min_value * 0.75f + max_value * 0.25f);
              }
              break;
            case LADSPA_HINT_DEFAULT_MIDDLE:
              if (LADSPA_IS_HINT_LOGARITHMIC(hints)) {
                default_value = ::sqrt(min_value * max_value);
              } else {
                default_value = (min_value + max_value) * 0.5f;
              }
              break;
            case LADSPA_HINT_DEFAULT_HIGH:
              if (LADSPA_IS_HINT_LOGARITHMIC(hints)) {
                default_value = ::expf(
                                       ::logf(min_value) * 0.25f + ::logf(max_value) * 0.75f);
              } else {
                default_value= (min_value * 0.25f + max_value * 0.75f);
              }
              break;
            case LADSPA_HINT_DEFAULT_MAXIMUM:
              default_value = max_value;
              break;
            case LADSPA_HINT_DEFAULT_0:
              default_value = 0.0f;
              break;
            case LADSPA_HINT_DEFAULT_1:
              default_value = 1.0f;
              break;
            case LADSPA_HINT_DEFAULT_100:
              default_value = 100.0f;
              break;
            case LADSPA_HINT_DEFAULT_440:
              default_value = 440.0f; // this needs to be scaled(440,0,20000,min,max), i think.
              break;
            default:
              default_value = (min_value+max_value)/2.0f;
          }

        }else{
          default_value = (min_value+max_value)/2.0f;
        }

        if(default_value<min_value)
          default_value=min_value;
        if(default_value>max_value)
          default_value=max_value;

        //if(!strcmp(plugin_type->name,"Organ"))
        //printf("Setting effect %s / %d to %f, %f, %f\n",plugin_type->name,effect_num,min_value,default_value,max_value);
        type_data->default_values[effect_num] = default_value;

        //if(!strcmp(plugin_type->type_name,"Simple Low Pass Filter"))
        //  printf("After hints. min/max/default: %f/%f/%f\n",min_value,max_value,default_value);

        type_data->effect_names[effect_num] = V_strdup(talloc_format("%d: %s", effect_num, descriptor->PortNames[portnum]));
          
        effect_num++;
      }
    }
  }
}


static void *create_plugin_data(const SoundPluginType *plugin_type, SoundPlugin *plugin, hash_t *state, float sample_rate, int block_size, bool is_loading){
  R_ASSERT(THREADING_is_main_thread());
    
  if (QString(plugin_type->name).startsWith("TAP ")){

    bool doit = true;

    if(g_is_loading && is_radium_internal_file(dc.filename_with_full_path)){
      doit = false;
    }

    if (doit && SETTINGS_read_bool("show_tap_plugins_warning", true)) {

      vector_t v = {};
      VECTOR_push_back(&v,"Ok");
      VECTOR_push_back(&v,"Don't show this message again");
      
      int result = GFX_Message(&v,
                               "Warning!"
                               "<p>"
                               "TAP plugins have a history of being unstable. Use at your own risk."
                               );
      
      if (result==1)
        SETTINGS_write_bool("show_tap_plugins_warning", false);
    }
  }
  
  Data *data = (Data*)V_calloc(1, sizeof(Data));

  TypeData *type_data = (TypeData*)plugin_type->data;

  if (add_type_data_reference(type_data)==false)
    return NULL;

  const LADSPA_Descriptor *descriptor = type_data->descriptor;
  if (type_data->descriptor==NULL){
    Library *library = type_data->library;
    R_ASSERT_RETURN_IF_FALSE2(library!=NULL, NULL);
    RError("2. type_data->descriptor==NULL. num_references: %d, num_times_loaded: %d, filename: \"%S\"",library->num_library_references,library->num_times_loaded,library->filename);
    return NULL;
  }
  
  fill_in_type_data_info_from_descriptor(type_data, plugin_type, descriptor);
  
  data->control_values = (float*)V_calloc(sizeof(float),descriptor->PortCount);

  data->handles[0] = descriptor->instantiate(descriptor,sample_rate);
  if(data->handles[0]==NULL){
    fprintf(stderr,"Could not instantiate ladspa plugin\n");
    return NULL;
  }

  if(type_data->uses_two_handles==true){
    data->handles[1] = descriptor->instantiate(descriptor,sample_rate);
    if(data->handles[1]==NULL){
      fprintf(stderr,"Could not instantiate ladspa plugin\n");
      return NULL;
    }
  }

  setup_audio_ports(plugin_type,data,block_size);

  {
    int effect_num = 0;

    for(unsigned int portnum=0;portnum<descriptor->PortCount;portnum++){
      const LADSPA_PortDescriptor portdescriptor = descriptor->PortDescriptors[portnum];

      if(LADSPA_IS_PORT_CONTROL(portdescriptor) && LADSPA_IS_PORT_INPUT(portdescriptor)){
        data->control_values[effect_num] = type_data->default_values[effect_num];
        //printf("Default value %d: %f\n",portnum,type_data->default_values[effect_num]);
        descriptor->connect_port(data->handles[0], portnum, &data->control_values[effect_num]);
        if(type_data->uses_two_handles==true)
          descriptor->connect_port(data->handles[1], portnum, &data->control_values[effect_num]);

        printf("Connecting port %d, effect_num %d, to value %f\n",portnum,effect_num,type_data->default_values[effect_num]);
        effect_num++;
      }
      
      if(LADSPA_IS_PORT_CONTROL(portdescriptor) && LADSPA_IS_PORT_OUTPUT(portdescriptor)){
        
        if (!strcmp("latency", descriptor->PortNames[portnum]))
          descriptor->connect_port(data->handles[0], portnum, &data->latency_output_control_port);
        else
          descriptor->connect_port(data->handles[0], portnum, &type_data->output_control_port_value);
        
        if(type_data->uses_two_handles==true)
          descriptor->connect_port(data->handles[1], portnum, &type_data->output_control_port_value);
      }
    }
  }


  if(descriptor->activate!=NULL){
    descriptor->activate(data->handles[0]);
    if(type_data->uses_two_handles==true)
      descriptor->activate(data->handles[1]);
  }

  return data;
}

static void cleanup_plugin_data(SoundPlugin *plugin){
  R_ASSERT(THREADING_is_main_thread());  

  const SoundPluginType *type=plugin->type;
  TypeData *type_data = (TypeData*)type->data;

  const LADSPA_Descriptor *descriptor = type_data->descriptor;

  Data *data = (Data*)plugin->data;
  if(descriptor->deactivate!=NULL){
    descriptor->deactivate(data->handles[0]);
    if(type_data->uses_two_handles==true)
      descriptor->deactivate(data->handles[1]);
  }

  descriptor->cleanup(data->handles[0]);
  if(type_data->uses_two_handles==true)
    descriptor->cleanup(data->handles[1]);

  delete_audio_ports(type,data);

  V_free(data->control_values);
  V_free(data);
  
  remove_type_data_reference(type_data);
}

static void buffer_size_is_changed(SoundPlugin *plugin, int new_buffer_size){
  const SoundPluginType *type=plugin->type;
  TypeData *type_data = (TypeData*)type->data;
  const LADSPA_Descriptor *descriptor = type_data->descriptor;
  Data *data = (Data*)plugin->data;

  if(descriptor->deactivate!=NULL){
    descriptor->deactivate(data->handles[0]);
    if(type_data->uses_two_handles==true)
      descriptor->deactivate(data->handles[1]);
  }
  {
    delete_audio_ports(type,data);
    setup_audio_ports(type,data,new_buffer_size);
  }
  if(descriptor->activate!=NULL){
    descriptor->activate(data->handles[0]);  
    if(type_data->uses_two_handles==true)
      descriptor->activate(data->handles[1]);
  }
}

static float frequency_2_slider(float freq, const float min_freq, const float max_freq){
#if 0
  fprintf(stderr, "frequency_2_slider(): %f %f %f - %f %f %f - %f %f %f\n",
          freq, min_freq, max_freq,
          logf(min_freq), logf(max_freq), min_output,
          logf(freq), logf(max_freq), logf(freq)/logf(max_freq)
          );
#endif
  
  if (freq <= 0.01 || min_freq <= 0.01 || max_freq <= 0.01)
    return scale(freq, min_freq, max_freq, 0, 1);
  
  const float min_output = logf(min_freq)/logf(max_freq);
  
  return scale( logf(freq)/logf(max_freq),
                min_output, 1.0,
                0.0, 1.0);
}

static float slider_2_frequency(float slider, const float min_freq, const float max_freq){
  if (min_freq <= 0.01 || max_freq <= 0.01)
    return scale(slider, 0, 1, min_freq, max_freq);
  
  const float min_output = logf(min_freq)/logf(max_freq);
  return powf(max_freq, scale(slider,
                              0,1,
                              min_output, 1));
}

static void set_effect_value(SoundPlugin *plugin, int time, int effect_num, float value, enum ValueFormat value_format, FX_when when){
  const SoundPluginType *type = plugin->type;
  TypeData *type_data = (TypeData*)type->data;
  Data *data = (Data*)plugin->data;

  if(value_format==EFFECT_FORMAT_SCALED){

    LADSPA_PortRangeHintDescriptor hints = type_data->hint_descriptors[effect_num];

    if(LADSPA_IS_HINT_LOGARITHMIC(hints)){

      data->control_values[effect_num] = slider_2_frequency(value,
                                                            type_data->min_values[effect_num],
                                                            type_data->max_values[effect_num]
                                                            );


    } else {

      data->control_values[effect_num] = scale(value,0.0f,1.0f,
                                               type_data->min_values[effect_num],
                                               type_data->max_values[effect_num]);

    }

  }else{
    data->control_values[effect_num] = value;
  }

#if 0
  printf("set_effect_value. effect_num: %d, value (0-1): %f, ladspa value: %f\n",
         effect_num,
         value,
         data->control_values[effect_num]);
#endif
}

static float get_effect_value(SoundPlugin *plugin, int effect_num, enum ValueFormat value_format){
  const SoundPluginType *type = plugin->type;
  TypeData *type_data = (TypeData*)type->data;
  Data *data = (Data*)plugin->data;
#if 0
  printf("min: %f, val: %f, max: %f\n",
         type_data->min_values[effect_num],
         data->control_values[effect_num],
         type_data->max_values[effect_num]);
#endif

  if(value_format==EFFECT_FORMAT_SCALED){
    const LADSPA_PortRangeHintDescriptor hints = type_data->hint_descriptors[effect_num];

#if 0
    fprintf(stderr, "A/B/C: %f %f %f\n",
            data->control_values[effect_num], 
            type_data->min_values[effect_num],
            type_data->max_values[effect_num]);
#endif
    
    if(LADSPA_IS_HINT_LOGARITHMIC(hints)){
      return frequency_2_slider(data->control_values[effect_num], 
                                type_data->min_values[effect_num],
                                type_data->max_values[effect_num]);

    } else {
      
      return scale(data->control_values[effect_num],
                   type_data->min_values[effect_num], type_data->max_values[effect_num],
                   0.0f,1.0f);

    }
  }else{
    return data->control_values[effect_num];
  }
}


static const char *get_effect_name(const SoundPlugin *plugin, int effect_num){
  TypeData *type_data = (struct TypeData*)plugin->type->data;
  return type_data->effect_names[effect_num];
}

static int get_effect_format(SoundPlugin *plugin, int effect_num){
  TypeData *type_data = (struct TypeData*)plugin->type->data;
  
  const LADSPA_PortRangeHintDescriptor hints = type_data->hint_descriptors[effect_num];

  if(LADSPA_IS_HINT_TOGGLED(hints))
    return EFFECT_FORMAT_BOOL;

  else if(LADSPA_IS_HINT_INTEGER(hints))
    return EFFECT_FORMAT_INT;

  else
    return EFFECT_FORMAT_FLOAT;
}

static void get_display_value_string(SoundPlugin *plugin, int effect_num, char *buffer, int buffersize){
  Data *data = (Data*)plugin->data;
  float value = safe_float_read(&data->control_values[effect_num]);

  switch(get_effect_format(plugin,effect_num)){
  case EFFECT_FORMAT_FLOAT:
    snprintf(buffer,buffersize-1,"%f",value);
    break;
  case EFFECT_FORMAT_INT:
    snprintf(buffer,buffersize-1,"%d",(int)value);
    break;
  case EFFECT_FORMAT_BOOL:
    snprintf(buffer,buffersize-1,"%s",value<=0.5f?"Off":"On");
    break;
  }
}

static const char *create_info_string(int num_inputs, int num_outputs, const LADSPA_Descriptor *descriptor){
  const char *ret = talloc_format("\"%s\" is a LADSPA Plugin made by %s.\n"
                                  "Copyright: %s.\n",
                                  descriptor->Name,
                                  descriptor->Maker,descriptor->Copyright
                                  );

  if (num_inputs > 0){

    ret = talloc_format("%s\nAudio inputs:", ret);
    
    int ch = 0;
    
    for(unsigned int portnum=0;portnum<descriptor->PortCount;portnum++){
      const LADSPA_PortDescriptor portdescriptor = descriptor->PortDescriptors[portnum];

      if(LADSPA_IS_PORT_AUDIO(portdescriptor) && LADSPA_IS_PORT_INPUT(portdescriptor)){        
        //const LADSPA_PortDescriptor portdescriptor = descriptor->PortDescriptors[portnum];
        ret = talloc_format("%s\n  ch%d: \"%s\"", ret, ch++, descriptor->PortNames[portnum]);
      }
    }
  }
  
  if (num_outputs > 0){

    if (num_inputs > 0)
      ret = talloc_format("%s\n", ret);
    
    ret = talloc_format("%s\nAudio outputs:", ret);
    
    int ch = 0;
    
    for(unsigned int portnum=0;portnum<descriptor->PortCount;portnum++){
      const LADSPA_PortDescriptor portdescriptor = descriptor->PortDescriptors[portnum];

      if(LADSPA_IS_PORT_AUDIO(portdescriptor) && LADSPA_IS_PORT_OUTPUT(portdescriptor)){        
        //const LADSPA_PortDescriptor portdescriptor = descriptor->PortDescriptors[portnum];
        ret = talloc_format("%s\n  ch%d: \"%s\"", ret, ch++, descriptor->PortNames[portnum]);
      }
    }
  }
  
  if (num_inputs==1 && num_outputs==1)
    ret = talloc_format("%s\n\n(this is a mono plugin automatically transformed into a stereo plugin by Radium)", ret);
  
  return V_strdup(ret);
}

static char *create_creator_string(const LADSPA_Descriptor *descriptor){
  char *creator = V_strdup(descriptor->Maker);
  int len = (int)strlen(creator);
  for(int i = 0 ; i < len ; i++){
    if (creator[i]=='<' || creator[i]=='('){
      if (i>0 && creator[i-1]==' ')
        creator[i-1]=0;
      else
        creator[i]=0;
      break;
    }
  }
  return creator;
}

// This function is suppressed from tsan. (not working though, can't get full backtrace)
/*
static const LADSPA_Descriptor *call_ladspa_get_descriptor_func(LADSPA_Descriptor_Function get_descriptor_func, int i){
  return get_descriptor_func(i);
}
*/

static TypeData *create_empty_type_data(Library *library, int index){
  auto *ret = (TypeData*)V_calloc(1,sizeof(TypeData));
  ret->library = library;
  ret->index = index;
  return ret;
}
  
static filepath_t get_library_cache_filename(const Library *library){
  R_ASSERT(library->filename!=NULL);
  return make_filepath(talloc_wformat(L"%S.cached_library_info", DISK_get_filename_without_suffix(make_filepath(library->filename)).id));
}

static filepath_t get_instance_cache_filename(const Library *library, int index){
  R_ASSERT(library->filename!=NULL);
  return make_filepath(talloc_wformat(L"%S.%d.cached_type_info", DISK_get_filename_without_suffix(make_filepath(library->filename)).id, index));
}

static bool maybe_fill_in_cached_plugin(TypeData *type_data, SoundPluginType *plugin_type, Library *library, int index){
  //return false; // uncomment to regenerate all cache files.
  
  if (!is_radium_internal_file(make_filepath(library->filename)))
    return false;

  filepath_t filename = get_instance_cache_filename(library, index);
  if (!DISK_file_exists(filename)){
    //abort();
    return false;
  }
  
  radium::ScopedReadFile file(filename);

  if (file._file==NULL)
    return false;
  
  hash_t *state = HASH_load2(file._file, true);
  if (state==NULL)
    return false;

  if (!HASH_has_key(state, "UniqueID"))
    return false;
  if (!HASH_has_key(state, "Name"))
    return false;
  if (!HASH_has_key(state, "plugin_type_state"))
    return false;  
  if (!HASH_has_key(state, "index"))
    return false;  
  if (!HASH_has_key(state, "uses_two_handles"))
    return false;  
  
  type_data->UniqueID = HASH_get_int(state, "UniqueID");
  type_data->Name = V_strdup(HASH_get_chars(state, "Name"));

  R_ASSERT(HASH_get_int32(state, "index")==index);
  type_data->index = HASH_get_int32(state, "index");

  type_data->uses_two_handles = HASH_get_bool(state, "uses_two_handles");
  
  if (!PLUGINTYPE_maybe_apply_state(plugin_type, HASH_get_hash(state, "plugin_type_state")))
    return false;

  return true;
}

static void maybe_create_cache_file_for_plugin(const SoundPluginType *plugin_type, const Library *library){
#if defined(RELEASE)
  return; // We only do this for the included ladspa files to avoid long loading time caused by virus killers scanning library files.
#endif

  if (!is_radium_internal_file(make_filepath(library->filename)))
    return;

  TypeData *type_data = (TypeData*)plugin_type->data;
  
  hash_t *state = HASH_create(4);

  HASH_put_hash(state, "plugin_type_state", PLUGINTYPE_get_state(plugin_type));
  HASH_put_int(state, "UniqueID", type_data->UniqueID);
  HASH_put_chars(state, "Name", type_data->Name);
  HASH_put_int(state, "index", type_data->index);
  HASH_put_int(state, "uses_two_handles", type_data->uses_two_handles);
                
  filepath_t filename = get_instance_cache_filename(library, type_data->index);

  printf("    CACHE filename: -%S-\n", filename.id);
  
  radium::ScopedWriteFile file(filename);

  if (file._file==NULL)
    return;

  HASH_save(state, file._file);
}


static SoundPluginType *create_plugin_type(const LADSPA_Descriptor *descriptor, Library *library, int index, bool is_system_pitchshift){
  SoundPluginType *plugin_type = (SoundPluginType*)V_calloc(1,sizeof(SoundPluginType));

  plugin_type->type_name = "Ladspa";
  plugin_type->is_instrument = false;

  TypeData *type_data = create_empty_type_data(library, index);
  plugin_type->data = type_data;
      
  bool loaded_from_cache = is_system_pitchshift ? false : maybe_fill_in_cached_plugin(type_data, plugin_type, library, index);
  
  if (!loaded_from_cache){

    bool had_descriptor = descriptor!=NULL;
    
    if (!had_descriptor) {
      if (add_type_data_reference(type_data)==false)
        return NULL;
      
      descriptor = type_data->descriptor;
    }


    plugin_type->name      = V_strdup(is_system_pitchshift ? "System AM pitchshift" : descriptor->Name);
    plugin_type->creator   = create_creator_string(descriptor);


    // Find num_effects, num_inputs, and num_outputs
    //
    for(unsigned int portnum=0;portnum<descriptor->PortCount;portnum++){
      const LADSPA_PortDescriptor portdescriptor = descriptor->PortDescriptors[portnum];
      
      if(LADSPA_IS_PORT_CONTROL(portdescriptor) && LADSPA_IS_PORT_INPUT(portdescriptor)){
        //printf("Adding effect %d\n",plugin_type->num_effects);
        plugin_type->num_effects++;
      }
      
      if(LADSPA_IS_PORT_AUDIO(portdescriptor) && LADSPA_IS_PORT_INPUT(portdescriptor)){
        plugin_type->num_inputs++;
      }
      
      if(LADSPA_IS_PORT_AUDIO(portdescriptor) && LADSPA_IS_PORT_OUTPUT(portdescriptor)){
        plugin_type->num_outputs++;
      }
    }

    fill_in_type_data_info_from_descriptor(type_data, plugin_type, descriptor); // needed by maybe_create_cache_file_for_plugin

    plugin_type->info  = create_info_string(plugin_type->num_inputs, plugin_type->num_outputs, descriptor);

    if (!had_descriptor)
      remove_type_data_reference(type_data);
  }

  if(!is_system_pitchshift && plugin_type->num_inputs==1 && plugin_type->num_outputs==1){ // Use two mono plugin instances to create one stereo plugin.
    plugin_type->num_inputs = 2;
    plugin_type->num_outputs = 2;
    type_data->uses_two_handles = true;
  }

  if (!loaded_from_cache && !is_system_pitchshift)
    maybe_create_cache_file_for_plugin(plugin_type, library);

  plugin_type->buffer_size_is_changed = buffer_size_is_changed;

  plugin_type->RT_process = RT_process;
  plugin_type->RT_get_latency = RT_get_latency;
    
  plugin_type->create_plugin_data = create_plugin_data;
  plugin_type->cleanup_plugin_data = cleanup_plugin_data;

  //plugin_type->show_gui = show_gui;

  //plugin_type->play_note       = play_note;
  //plugin_type->set_note_volume = set_note_volume;
  //plugin_type->stop_note       = stop_note;

  plugin_type->get_display_value_string = get_display_value_string;

  plugin_type->set_effect_value = set_effect_value;
  plugin_type->get_effect_value = get_effect_value;

  plugin_type->get_effect_name=get_effect_name;
  //plugin_type->get_effect_description=get_effect_description;
  plugin_type->get_effect_format = get_effect_format;

  return plugin_type;
}

static bool maybe_fill_in_cached_library(Library *library){
  if (!is_radium_internal_file(make_filepath(library->filename)))
    return false;
  
  filepath_t filename = get_library_cache_filename(library);
  if (!DISK_file_exists(filename)){
    //abort();
    return false;
  }

  radium::ScopedReadFile file(filename);

  if (file._file==NULL)
    return false;
  
  hash_t *state = HASH_load2(file._file, true);
  if (state==NULL)
    return false;

  if (!HASH_has_key(state, "num_instances"))
    return false;

  library->num_instances = HASH_get_int32(state, "num_instances");
  
  return true;
}

static void maybe_create_cache_file_for_library(const Library *library){
  #if defined(RELEASE)
  return; // We only do this for the included ladspa files to avoid long loading time caused by virus killers scanning library files.
#endif

  if (!is_radium_internal_file(make_filepath(library->filename)))
    return;

  hash_t *state = HASH_create(4);

  HASH_put_int(state, "num_instances", library->num_instances);
                
  filepath_t filename = get_library_cache_filename(library);
  
  radium::ScopedWriteFile file(filename);

  if (file._file==NULL)
    return;

  HASH_save(state, file._file);
}

bool g_has_added_system_pitchshift = false;

static void add_ladspa_plugin_type(QString filename) {
  //return; // <- TODO: ThreadSanitizer complains on somethine when loading each ladspa plugins, but there is no proper backtrace so I haven't taken the time to find the cause of it yet.
  
  //QString filename = file_info.absoluteFilePath();
  
  printf("\"%s\"... ",filename.toUtf8().constData());

  Library *library = new Library(filename);
  
  //printf("Resolved \"%s\"\n",myLib.fileName().toUtf8().constData());

  bool is_am_pitchshift = filename.contains("am_pitchshift");
  
  auto addit = [library](const LADSPA_Descriptor *descriptor, int index, bool is_system_pitchshift){

    if (is_system_pitchshift)
      g_has_added_system_pitchshift = true;
    
    SoundPluginType *plugin_type = create_plugin_type(descriptor, library, index, is_system_pitchshift);
    PR_add_plugin_type_no_menu(plugin_type);
    g_plugin_types.push_back(plugin_type);
  };

  bool got_library_from_cache = is_am_pitchshift ? false : maybe_fill_in_cached_library(library);
  
  if (got_library_from_cache){
    
    for(int index=0 ; index < library->num_instances ; index++)
      addit(NULL, index, false);
    
  } else {

    QScopedPointer<QLibrary> qlibrary(new QLibrary(filename)); // There's probably nothing to gain by getting QLibrary from 'library'. We are only getting parameters and so forth here, not using the plugin.
    
    LADSPA_Descriptor_Function get_descriptor_func = (LADSPA_Descriptor_Function) qlibrary->resolve("ladspa_descriptor");

    if(get_descriptor_func==NULL){
      if (qlibrary->errorString().contains("dlopen: cannot load any more object with static TLS")){
        
        if (PR_is_initing_vst_first()) {
          
          vector_t v = {}; // c++ way of zero-initialization without getting missing-field-initializers warning.
          
          int init_ladspa_first = VECTOR_push_back(&v,"Init LADSPA plugins first");
          VECTOR_push_back(&v,"Continue without loading this plugin library.");
          
          int result = GFX_Message(&v,
                                   "Error: Empty thread local storage.\n"
                                   "\n"
                                   "Unable to load LADSPA library file \"%s\".\n"
                                   "\n"
                                   "This is not a bug in Radium or the plugin, but a system limitation most likely provoked by\n"
                                   "the TLS settings of an earlier loaded plugin. (In other words: There's probably nothing wrong with this plugin!).\n"
                                   "\n"
                                   "You may be able to work around this problem by initing LADSPA plugins before VST plugins.\n"
                                   "In case you want to try this, press the \"Init LADSPA plugins first\" button below and start radium again.\n",
                                   qlibrary->fileName().toUtf8().constData()
                                   );
          if (result==init_ladspa_first)
            PR_set_init_ladspa_first();
          
        } else {
          GFX_Message(NULL,
                      "Error: Empty thread local storage.\n"
                      "\n"
                      "Unable to load LADSPA library file \"%s\".\n"
                      "\n"
                      "This is not a bug in Radium or the plugin, but a system limitation most likely provoked by\n"
                      "the TLS settings of an earlier loaded plugin. (In other words: There's probably nothing wrong with this plugin!).\n",
                      qlibrary->fileName().toUtf8().constData()
                      );
        }
      }
      
      QString error_string = qlibrary->errorString();

      fprintf(stderr,"(failed: \"%s\") ", error_string.toUtf8().constData());
      fflush(stderr);
      
#if !defined(RELEASE)
      abort();
#endif
      return;
    }
    
    const LADSPA_Descriptor *descriptor;
    
    for(int index = 0; (descriptor=get_descriptor_func(index)) != NULL ; index++){
      
      library->num_instances++;
      addit(descriptor, index, false);
      if (is_am_pitchshift)
        addit(descriptor, index, true);
    }

    maybe_create_cache_file_for_library(library);
  }

}

static SoundPluginType *get_plugin_type_from_id(unsigned long id){
  for(unsigned int i=0;i<g_plugin_types.size();i++){
    SoundPluginType *plugin_type = g_plugin_types[i];
    TypeData *type_data = (TypeData*)plugin_type->data;
    if(type_data->UniqueID == id)
      return plugin_type;
  }
  return NULL;
}

namespace{
struct plugin_type_pred{
  bool operator()(SoundPluginType *a, SoundPluginType *b) const{
    TypeData *type_data_a = (TypeData*)a->data;
    TypeData *type_data_b = (TypeData*)b->data;
    return strcasecmp(type_data_a->Name, type_data_b->Name) < 0;
  }
};
}

static std::vector<SoundPluginType*> g_added_plugin_types;

// This function made is made by first copying the menu_descend function from jack-rack by Bob Ham, and then modify it.
static void menu_descend(const char * uri,
                         const char * base)
{
  printf("   base ---> \"%s\"\n",base);
  if(strlen(base)>0)
    PR_add_menu_entry(PluginMenuEntry::level_up(base));
  unsigned int i;
  
  lrdf_uris *uris = lrdf_get_subclasses(uri);
  if (uris){
    for (i = 0; i < uris->count; i++){
      /* get the sub menu */
          const char * label = lrdf_get_label (uris->items[i]);
          char newbase[500];
          snprintf (newbase, 499,"%s/%s", base, label);
          
          menu_descend (uris->items[i], label); //newbase);
    }
    
    lrdf_free_uris (uris);
  }
    
  
  uris = lrdf_get_instances(uri);
  if (uris){
    std::vector<SoundPluginType*> plugin_types;
    for (i = 0; i < uris->count; i++){
      SoundPluginType *plugin_type=get_plugin_type_from_id(lrdf_get_uid (uris->items[i]));
      if(plugin_type!=NULL){
        plugin_types.push_back(plugin_type);
        g_added_plugin_types.push_back(plugin_type);
      }
    }

    std::sort(plugin_types.begin(), plugin_types.end(), plugin_type_pred());

    for(unsigned int i=0;i<plugin_types.size();i++){
      //printf("  \"%s\"\n",plugin_types.at(i)->name);
      if (strcmp(plugin_types.at(i)->name, "System AM pitchshift"))
        PR_add_menu_entry(PluginMenuEntry::normal(plugin_types.at(i)));
    }
    
    lrdf_free_uris (uris);
  }

  if(strlen(base)>0)
    PR_add_menu_entry(PluginMenuEntry::level_down());
}

static void get_dir_uris (QStringList &lrdf_uris, const QString &dirname){
  QDir dir(dirname);
  QFileInfoList filelist = dir.entryInfoList(QDir::AllEntries|QDir::NoDotAndDotDot);
  
  for(QFileInfo file_info : filelist){
    
    if (file_info.isDir())
      continue;
    
    QString file_name = file_info.fileName();
    
    if (file_name.endsWith(".rdf") || file_name.endsWith(".rdfs"))
      lrdf_uris.push_back(file_info.absoluteFilePath());        
  }
}

static void get_path_uris (QStringList &lrdf_uris){
  QStringList lrdf_paths;

  lrdf_paths << STRING_get_qstring(OS_get_full_program_file_path("rdf").id); // place this one first so that the user doesn't override the rdfs in bin/rdf/

#if __linux__

  if(getenv("LADSPA_RDF_PATH")!=NULL){
    auto d = QString(getenv("LADSPA_RDF_PATH")).split(":");
    for(QString path : d)
      lrdf_paths << path;
  }
  
  lrdf_paths << "/usr/local/share/ladspa/rdf";
  lrdf_paths << "/usr/share/ladspa/rdf";

#endif

  for(QString path : lrdf_paths)
    get_dir_uris(lrdf_uris, path);
}

static QStringList filter_out_same_basename_lrdf(QStringList &lrdf_uris){
  QHash<QString,int> basenames;
  QStringList ret;

  for(QString lrdf_uri :lrdf_uris){
    QFileInfo fileInfo(lrdf_uri);
    if(basenames.contains(fileInfo.baseName())==false){
      basenames[fileInfo.baseName()] = 1;
      ret.push_back(lrdf_uri);
    }
  }

  return ret;
}

static void init_lrdf (){
  QStringList lrdf_uris;
  
  get_path_uris (lrdf_uris);

  lrdf_uris = filter_out_same_basename_lrdf(lrdf_uris);


  printf("Get paths\n");
  fflush(stdout);

  lrdf_init ();  
  printf("lrdf init\n");
  fflush(stdout);

  for(QString lrdf_uri : lrdf_uris){
    //printf("Trying to read %d, size: %d. data: %s\n",i,(int)lrdf_uris.size(),lrdf_uris.at(i));
    fflush(stdout);

    int err = 0;
    
// which method to use to workaround lack of wide character support in liblrdf. (problem on windows)
// COPY_TO_TEMP might not work if home path contains non-ascii character.
#define COPY_TO_TEMP 0
#define CHANGE_CHDIR 1
    
#if COPY_TO_TEMP
    // Since liblrdf only accepepts char* as filename format, we copy the rdf file to a temporary path to make it load on windows when the program path contains non-ascii characters.
    const wchar_t *temp_file = DISK_copy_to_temp_file(STRING_create(lrdf_uri));
    
    if (temp_file==NULL){
      GFX_addMessage("Unable to create temp file when reading LRDF file '%s'", lrdf_uri.toUtf8().constData());
    } else {

      //err = lrdf_read_file((QString("file:")+lrdf_uri).toUtf8().constData());

      err = lrdf_read_file(QFile::encodeName(STRING_get_qstring(STRING_append(STRING_create("file:"),
                                                                              temp_file)
                                                                )
                                             ).constData()
                           );
      
      if (DISK_delete_file(temp_file)==false){
        GFX_addMessage("Unable to delete temp file '%S' when reading LRDF file '%s'", temp_file, lrdf_uri.toUtf8().constData());
      }
    }

#elif CHANGE_CHDIR

    {      
      QFileInfo file_info(lrdf_uri);
      QString filename = file_info.fileName();
      
      QDir dir = file_info.absoluteDir();
      QDir current_dir = QDir::current();

      QDir::setCurrent(dir.absolutePath());
      err = lrdf_read_file(QFile::encodeName(QString("file:") + filename));
      QDir::setCurrent(current_dir.absolutePath());
    }
    
#else
    
    err = lrdf_read_file(QFile::encodeName(QString("file:") + lrdf_uri));

#endif

#undef COPY_TO_TEMP
#undef CHANGE_CHDIR

    if (err)
      GFX_addMessage("%s: could not parse LRDF file '%s'\n", __FUNCTION__, lrdf_uri.toUtf8().constData());


    //printf("Finished reading %d. Success? %s\n", i, err ? "no":"yes");
    //fflush(stdout);

  }
}

static void init_categorized_menues(){
  menu_descend(LADSPA_BASE "Plugin", "");
}

static void init_uncategorized_menues(){
  std::sort(g_plugin_types.begin(), g_plugin_types.end(), plugin_type_pred());
  std::sort(g_added_plugin_types.begin(), g_added_plugin_types.end(), plugin_type_pred());

  if(g_plugin_types.size()==0)
    return;

  //unsigned int num_remaining_elements = g_plugin_types.size() - g_added_plugin_types.size();

  std::vector<SoundPluginType*> diff(g_plugin_types.size()*sizeof(SoundPluginType*));//(num_remaining_elements);
  std::vector<SoundPluginType*>::iterator end=std::set_difference(g_plugin_types.begin(),      g_plugin_types.end(),
                                                                  g_added_plugin_types.begin(), g_added_plugin_types.end(),
                                                                  diff.begin(),                 plugin_type_pred());

    
  fprintf(stderr,"g_plugin_size: %d, added_size: %d, diff size: %d\n",(int)g_plugin_types.size(),(int)g_added_plugin_types.size(),(int)diff.size());

  //if(g_added_plugin_types.size()==0)
  //  return;

  PR_add_menu_entry(PluginMenuEntry::level_up("Uncategorized"));
  {
    char last=0;
    for(unsigned int i=0;i<diff.size() && diff.at(i)!=*end;i++){  // valgrind complains about "Invalid read of size 8" here. Maybe use QVector instead. This code is not easy to understand.
      //fprintf(stderr,"i: %d\n",(int)i);
      SoundPluginType *plugin_type = diff.at(i);
      const char *name = plugin_type->name;
      if(name[0]==last){
        if (strcmp(name, "System AM pitchshift"))
          PR_add_menu_entry(PluginMenuEntry::normal(plugin_type));
      }else{
        if(last!=0)
          PR_add_menu_entry(PluginMenuEntry::level_down());
        last=name[0];
        char temp[2];
        temp[0]=last;
        temp[1]=0;
        PR_add_menu_entry(PluginMenuEntry::level_up(temp));
        if (strcmp(name, "System AM pitchshift"))
          PR_add_menu_entry(PluginMenuEntry::normal(plugin_type));
      }
      
      //printf("Plugin %d/%d/%d: %s\n",(int)i,(int)num_remaining_elements,(int)diff.size(),diff.at(i)->name);
      //printf("Plugin: %d/%d/%d: %s / %s / %s\n",(int)i,(int)num_remaining_elements,(int)diff.size(),diff.at(i)->name, g_plugin_types.at(i)->name,g_added_plugin_types.at(i)->name);
    }
    if(last!=0)
      PR_add_menu_entry(PluginMenuEntry::level_down());
  }
  PR_add_menu_entry(PluginMenuEntry::level_down());
}

static void init_menues(){
  init_lrdf ();

  init_categorized_menues();
  init_uncategorized_menues();
}

static bool is_included_ladspa_plugins_path(QString dirname){
  return dirname == STRING_get_qstring(OS_get_full_program_file_path("ladspa").id);
}

static QStringList get_included_ladspa_plugins_list(void){
  const char *filenames = R""""(
alias_1407
allpass_1895
ambisonic0
ambisonic1
ambisonic2
ambisonic3
amp_1181
am_pitchshift_1433
autowah
bandpass_a_iir_1893
bandpass_iir_1892
blvco
bode_shifter_1431
bode_shifter_cv_1432
butterworth_1902
calf
caps
chebstortion_1430
cmt
comb_1190
comb_1887
comb_splitter_1411
const_1909
crossover_dist_1404
cs_chorus
cs_phaser
dc_remove_1207
decay_1886
decimator_1202
declip_1195
delay_1898
delayorama_1402
diode_1185
divider_1186
dj_eq_1901
dj_flanger_1438
dyson_compress_1403
fad_delay_1192
fast_lookahead_limiter_1913
filters
flanger_1191
foldover_1213
foverdrive_1196
freq_tracker_1418
g2reverb
gate_1410
giant_flange_1437
gong_1424
gong_beater_1439
gsm_1215
gverb_1216
hard_limiter_1413
harmonic_gen_1220
highpass_iir_1890
hilbert_1440
imp_1199
impulse_1885
inv_1429
karaoke_1409
latency_1914
lcr_delay_1436
lowpass_iir_1891
ls_filter_1908
matrix_ms_st_1421
matrix_spatialiser_1422
matrix_st_ms_1420
mbeq_1197
mod_delay_1419
multivoice_chorus_1201
mvchpf24
mvclpf24
notch_iir_1894
phasers_1217
pitch_scale_1193
pitch_scale_1194
plate_1423
pointer_cast_1910
rate_shifter_1417
retro_flange_1208
revdelay_1605
ringmod_1188
satan_maximiser_1408
sc1_1425
sc2_1426
sc3_1427
sc4_1882
sc4m_1916
se4_1883
shaper_1187
sifter_1210
sin_cos_1881
single_para_1203
sinus_wavewrapper_1198
smooth_decimate_1414
split_1406
step_muxer_1212
stereo-plugins
svf_1214
tap_autopan
tap_chorusflanger
tap_deesser
tap_doubler
tap_dynamics_m
tap_dynamics_st
tap_echo
tape_delay_1211
tap_eqbw
tap_eq
tap_limiter
tap_pinknoise
tap_pitch
tap_reflector
tap_reverb
tap_rotspeak
tap_sigmoid
tap_tremolo
tap_tubewarmth
tap_vibrato
transient_1206
triple_para_1204
valve_1209
valve_rect_1405
vocoder_1337
vynil_1905
wave_terrain_1412
xfade_1915
zita-reverbs
zm1_1428
)"""";

  const QStringList list = QString(filenames).split("\n");
  QStringList ret;

  for(int i = 0 ; i < list.size() ; i++){
    QString l = list.at(i).trimmed();
    if (!l.isEmpty())
      ret << l + "." + LIB_SUFFIX;
  }

  return ret;
}

void create_ladspa_plugins(void){
  
  QStringList ladspa_path;

#if defined(FOR_LINUX) && !defined(IS_LINUX_BINARY) && defined(RELEASE)  // I.e. Only custom linux release builds.

  if(getenv("LADSPA_PATH")==NULL){
    //MyQMessageBox::information(NULL, "LADSPA_PATH is not set.", "LADSPA_PATH is not set.");
    //return;
#ifdef USE_QT5
    QString home_ladspa_path = QDir::homePath() + "/.ladspa";
#else
    QString home_ladspa_path = QDesktopServices::storageLocation(QDesktopServices::HomeLocation) + "/.ladspa";
#endif

    if(OS_has_full_program_file_path("ladspa"))
      ladspa_path << STRING_get_qstring(OS_get_full_program_file_path("ladspa").id);
    
    ladspa_path << home_ladspa_path;
    ladspa_path << "/usr/lib64/ladspa";
    ladspa_path << "/usr/lib/ladspa";
    ladspa_path << "/usr/local/lib64/ladspa";
    ladspa_path << "/usr/local/lib/ladspa";

  } else 
    ladspa_path << QString(getenv("LADSPA_PATH")).split(":");

#else
  
  // We don't use system ladspa plugins in the binaries because they might use libraries not compatible with the ones included with radium. (happens with guitarix, which links in glib, preventing radium from even starting.)
  
  ladspa_path << STRING_get_qstring(OS_get_full_program_file_path("ladspa").id);
  
#endif

  for(QString dirname : ladspa_path){

#if 1

    QStringList list;
    
    if (is_included_ladspa_plugins_path(dirname)) {
      
      list = get_included_ladspa_plugins_list(); // Hopefully THIS will not trigger the builtin virus checker on windows (causing long startup time). That virus thing on windows is really agressive...
      
    } else {
    
      QDir dir(dirname);
      
      dir.setFilter(QDir::Files | QDir::NoDotAndDotDot);
      dir.setSorting(QDir::Name);
      
      list = dir.entryList();
    }

    int i = 0;
    const int size = list.size();
    
    for (QString filename : list) {

      i++;

      const QString fullpath = dirname + QDir::separator() + filename;
            
      if(filename.endsWith(LIB_SUFFIX)) {
        GFX_ShowProgressMessage(QString("Adding LADSPA plugin %1 / %2: \"%3\".").arg(i).arg(size).arg(fullpath).toUtf8().constData(), true);

        //msleep(300);

        add_ladspa_plugin_type(fullpath);
      }
      
    }
    
#else

    // no sorting
    
    QDirIterator it(dirname, QDir::Files | QDir::NoDotAndDotDot, QDirIterator::FollowSymlinks);
    while (it.hasNext()) {
      QString path = it.next();
      if(path.endsWith(LIB_SUFFIX))
        add_ladspa_plugin_type(path);
    }
    
#endif

  }

  printf("\n");
  
  init_menues();
}

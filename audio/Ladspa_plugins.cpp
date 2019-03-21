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
  float latency_output_control_port;
};

struct Library{ // Used to avoid having lots of unused dynamic libraries loaded into memory at all time (and sometimes using up TLS)
  const char *filename; // used for error messages
  QLibrary *library;
  LADSPA_Descriptor_Function get_descriptor_func;
  int num_library_references; // library is unloaded when this value decreases from 1 to 0, and loaded when increasing from 0 to 1.
  int num_times_loaded; // for debugging
};

struct TypeData{
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

  for(int ch=0;ch<type->num_inputs;ch++)
    memcpy(data->inputs[ch],inputs[ch],sizeof(float)*num_frames);
  {
    int pos = CRASHREPORTER_set_plugin_name(plugin->type->name); {
      descriptor->run(data->handles[0], num_frames);
      if(type_data->uses_two_handles)
        descriptor->run(data->handles[1], num_frames);
    } CRASHREPORTER_unset_plugin_name(pos);
  }
  for(int ch=0;ch<type->num_outputs;ch++)
    memcpy(outputs[ch],data->outputs[ch],sizeof(float)*num_frames);
}

static int RT_get_latency(const struct SoundPlugin *plugin){
  Data *data = (Data*)plugin->data;

  int latency = data->latency_output_control_port;
  
  //if (latency != 0)
  //  printf("plugin %s has latency %d\n",plugin->patch->name, latency);
  
  if (latency < 0)
    latency = 0;
  
  return latency;
}

static void delete_audio_ports(const SoundPluginType *type, Data *data){
  for(int i=0; i<type->num_inputs;i++)
    V_free(data->inputs[i]);
  V_free(data->inputs);

  for(int i=0; i<type->num_outputs;i++)
    V_free(data->outputs[i]);
  V_free(data->outputs);
}

static void setup_audio_ports(const SoundPluginType *type, Data *data, int block_size){
  TypeData *type_data = (TypeData*)type->data;
  const LADSPA_Descriptor *descriptor = type_data->descriptor;

  data->inputs=(float**)V_malloc(sizeof(float*)*type->num_inputs);
  data->outputs=(float**)V_malloc(sizeof(float*)*type->num_outputs);
  
  for(int i=0; i<type->num_inputs;i++)
    data->inputs[i]=(float*)V_calloc(sizeof(float),block_size);
  
  for(int i=0; i<type->num_outputs;i++)
    data->outputs[i]=(float*)V_calloc(sizeof(float),block_size);

  {
    int input_num = 0;
    int output_num = 0;

    for(unsigned int portnum=0;portnum<descriptor->PortCount;portnum++){
      const LADSPA_PortDescriptor portdescriptor = descriptor->PortDescriptors[portnum];

      if(LADSPA_IS_PORT_AUDIO(portdescriptor) && LADSPA_IS_PORT_INPUT(portdescriptor)){
        descriptor->connect_port(data->handles[0], portnum, data->inputs[input_num]);
        if(type_data->uses_two_handles==true)
          descriptor->connect_port(data->handles[1], portnum, data->inputs[1]);

        input_num++;
      }
    
      if(LADSPA_IS_PORT_AUDIO(portdescriptor) && LADSPA_IS_PORT_OUTPUT(portdescriptor)){
        descriptor->connect_port(data->handles[0], portnum, data->outputs[output_num]);
        if(type_data->uses_two_handles==true)
          descriptor->connect_port(data->handles[1], portnum, data->outputs[1]);

        output_num++;
      }

    }
  }
}

static bool add_library_reference(Library *library){
  
  if (library->num_library_references==0){
    printf("**** Loading %s\n",library->filename);
        
    LADSPA_Descriptor_Function get_descriptor_func = (LADSPA_Descriptor_Function) library->library->resolve("ladspa_descriptor");

    if(get_descriptor_func==NULL){
      GFX_Message(NULL, "Unable to load plugin. Has the plugin file \"%s\" disappeared?", library->filename);
      return false;
    }

    library->get_descriptor_func = get_descriptor_func;
  }

  library->num_library_references++;
  library->num_times_loaded++;
  
  return true;
}

static void remove_library_reference(Library *library){    
  library->num_library_references--;

  if (library->num_library_references==0) {
    printf("**** Unloading %s\n",library->filename);
    library->library->unload();
  }
}

static bool add_type_data_reference(TypeData *type_data){

  if (type_data->num_typedata_references==0){

    R_ASSERT(type_data->descriptor==NULL);

    if (add_library_reference(type_data->library)==false)
      return false;
    
    type_data->descriptor = type_data->library->get_descriptor_func(type_data->index);
  
    if (type_data->descriptor==NULL) {
      GFX_Message(NULL, "Unable to load plugin #%d in file \"%s\". That is not supposed to happen since it was possible to load the plugin when the program was initializing.", type_data->index, type_data->library->filename);
      remove_library_reference(type_data->library);
      return false;
    }
  }

  type_data->num_typedata_references++;
  return true;
}
  
static void remove_type_data_reference(TypeData *type_data){
  type_data->num_typedata_references--;

  if (type_data->num_typedata_references==0){
    remove_library_reference(type_data->library);
    type_data->descriptor = NULL;
  }
}

static void *create_plugin_data(const SoundPluginType *plugin_type, SoundPlugin *plugin, hash_t *state, float sample_rate, int block_size, bool is_loading){
  R_ASSERT(THREADING_is_main_thread());
    
  Data *data = (Data*)V_calloc(1, sizeof(Data));
  TypeData *type_data = (TypeData*)plugin_type->data;

  if (add_type_data_reference(type_data)==false)
    return NULL;

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
                               "TAP plugins might be be unstable. Use at your own risk."
                               );
      
      if (result==1)
        SETTINGS_write_bool("show_tap_plugins_warning", false);
    }
  }
  
  const LADSPA_Descriptor *descriptor = type_data->descriptor;
  if (type_data->descriptor==NULL){
    Library *library = type_data->library;
    R_ASSERT_RETURN_IF_FALSE2(library!=NULL, NULL);
    RError("2. type_data->descriptor==NULL. num_references: %d, num_times_loaded: %d, filename: \"%s\"",library->num_library_references,library->num_times_loaded,library->filename);
    return NULL;
  }
  
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

static float frequency_2_slider(float freq, const float min_freq, const float max_freq){
  const float min_output = logf(min_freq)/logf(max_freq);
  return scale( logf(freq)/logf(max_freq),
                min_output, 1.0,
                0.0, 1.0);
}

static float slider_2_frequency(float slider, const float min_freq, const float max_freq){
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


static const char *get_effect_name(SoundPlugin *plugin, int effect_num){
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

static char *create_info_string(const LADSPA_Descriptor *descriptor){
  char temp[4096];
  sprintf(temp,
          "\"%s\" is a LADSPA Plugin made by %s.\n"
          "Copyright: %s.",
          descriptor->Name,descriptor->Maker,descriptor->Copyright);
  return V_strdup(temp);
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

static void add_ladspa_plugin_type(const QFileInfo &file_info){
  //return; // <- TODO: ThreadSanitizer complains on somethine when loading each ladspa plugins, but there is no proper backtrace so I haven't taken the time to find the cause of it yet.
  
  QString filename = file_info.absoluteFilePath();
  
  fprintf(stderr,"\"%s\"... ",filename.toUtf8().constData());
  fflush(stderr);

  QLibrary *qlibrary = new QLibrary(filename);

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
    
    delete qlibrary;
    fprintf(stderr,"(failed: \"%s\") ", error_string.toUtf8().constData());
    fflush(stderr);

#if !defined(RELEASE)
    abort();
#endif
    return;
  }

  Library *library = (Library*)V_calloc(1, sizeof(Library));
  library->library = qlibrary;
  library->filename = V_strdup(filename.toUtf8().constData());
  
  //printf("Resolved \"%s\"\n",myLib.fileName().toUtf8().constData());

  const LADSPA_Descriptor *descriptor;

  for(int i = 0; (descriptor=get_descriptor_func(i)) != NULL ; i++){
    SoundPluginType *plugin_type = (SoundPluginType*)V_calloc(1,sizeof(SoundPluginType));
    TypeData *type_data = (TypeData*)V_calloc(1,sizeof(TypeData));

    plugin_type->data = type_data;

    type_data->library = library;
    //type_data->descriptor = descriptor; // We unload the library later in this function, and then 'descriptor' won't be valid anymore.
    type_data->UniqueID = descriptor->UniqueID;
    type_data->Name = V_strdup(descriptor->Name);
    type_data->index = i;
    
#if 0
    QString basename = file_info.fileName();
    basename.resize(basename.size()-strlen(LIB_SUFFIX)-1);
    type_data->filename = V_strdup(basename.toUtf8().constData());
#endif

    plugin_type->type_name = "Ladspa";
    plugin_type->name      = V_strdup(descriptor->Name);
    plugin_type->info      = create_info_string(descriptor);
    plugin_type->creator   = create_creator_string(descriptor);

    plugin_type->is_instrument = false;

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
        if(!strcmp(plugin_type->type_name,"Simple Low Pass Filter"))
          printf("Handling %s. portcount: num_ports: %d. is_control: %d is_input: %d\n",plugin_type->name,(int)descriptor->PortCount,LADSPA_IS_PORT_CONTROL(portdescriptor), LADSPA_IS_PORT_INPUT(portdescriptor));

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

          if(!strcmp(plugin_type->type_name,"Simple Low Pass Filter"))
            printf("Before hints. min/max/default: %f/%f/%f\n",min_value,max_value,default_value);

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

          if(!strcmp(plugin_type->type_name,"Simple Low Pass Filter"))
            printf("After hints. min/max/default: %f/%f/%f\n",min_value,max_value,default_value);

          type_data->effect_names[effect_num] = V_strdup(talloc_format("%d: %s", effect_num, descriptor->PortNames[portnum]));
          
          effect_num++;
        }
      }
    }

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

    if(plugin_type->num_inputs==1 && plugin_type->num_outputs==1){ // Use two mono plugin instances to create one stereo plugin.
      plugin_type->num_inputs = 2;
      plugin_type->num_outputs = 2;
      type_data->uses_two_handles = true;
    }

    PR_add_plugin_type_no_menu(plugin_type);
    g_plugin_types.push_back(plugin_type);
  }

  qlibrary->unload();
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
          sprintf (newbase, "%s/%s", base, label);
          
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

  lrdf_paths << OS_get_full_program_file_path("rdf"); // place this one first so that the user doesn't override the rdfs in bin/rdf/

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
        PR_add_menu_entry(PluginMenuEntry::normal(plugin_type));
      }else{
        if(last!=0)
          PR_add_menu_entry(PluginMenuEntry::level_down());
        last=name[0];
        char temp[2];
        temp[0]=last;
        temp[1]=0;
        PR_add_menu_entry(PluginMenuEntry::level_up(temp));
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

void create_ladspa_plugins(void){
#if !defined(RELEASE) && defined(FOR_WINDOWS)
  return; // takes long time to load ladspa plugins in gdb.
#endif

  
  QStringList ladspa_path;

#if FOR_LINUX && !defined(IS_LINUX_BINARY)  // We don't use system ladspa plugins in the binaries because they might use incompatible libraries with the ones included with radium. (happens with guitarix, which links in glib, preventing radium from even starting.)
  
  if(getenv("LADSPA_PATH")==NULL){
    //MyQMessageBox::information(NULL, "LADSPA_PATH is not set.", "LADSPA_PATH is not set.");
    //return;
#ifdef USE_QT5
    QString home_ladspa_path = QDir::homePath() + "/.ladspa";
#else
    QString home_ladspa_path = QDesktopServices::storageLocation(QDesktopServices::HomeLocation) + "/.ladspa";
#endif

    if(OS_has_full_program_file_path("ladspa"))
      ladspa_path << OS_get_full_program_file_path("ladspa");
    
    ladspa_path << home_ladspa_path;
    ladspa_path << "/usr/lib64/ladspa";
    ladspa_path << "/usr/lib/ladspa";
    ladspa_path << "/usr/local/lib64/ladspa";
    ladspa_path << "/usr/local/lib/ladspa";

  } else 
    ladspa_path << QString(getenv("LADSPA_PATH")).split(":");

#else
  
  ladspa_path << OS_get_full_program_file_path("ladspa");
  
#endif

  for(QString dirname : ladspa_path){

    QDir dir(dirname);

    dir.setFilter(QDir::Files | QDir::NoDotAndDotDot);
    dir.setSorting(QDir::Name);
    
    QFileInfoList list = dir.entryInfoList();
    
    for (int i = 0; i < list.size(); ++i) {
      QFileInfo fileInfo = list.at(i);
      if(fileInfo.suffix()==LIB_SUFFIX)
        add_ladspa_plugin_type(fileInfo);
    }

  }

  init_menues();
}

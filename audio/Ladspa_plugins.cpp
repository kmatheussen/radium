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



// Huge amount of ladspa plugins for windows:
// http://opensourcepack.blogspot.se/2012/03/ported-ladspa-plugins-collection.html
//
// ladspa for osx might be included with ardour. Got it. (http://ardour.org/files/Plugins.tar.bz2)

#include <algorithm>
#include <vector>

#include <math.h>
#include <dirent.h>
#include <errno.h>
#include <string.h>

#include <lrdf.h>
#include <ladspa.h>

#include <QFileInfo>
#include <QLibrary>
#include <QDir>
#include <QMessageBox>

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

#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"
#include "SoundPluginRegistry_proc.h"
#include "Mixer_proc.h"

struct Data{
  LADSPA_Handle handles[2];
  float *control_values;  
  float **inputs;
  float **outputs;
};

struct TypeData{
  const LADSPA_Descriptor *descriptor;
  const char *filename;
  float output_control_port_value;
  float *min_values;
  float *default_values;
  float *max_values;
  bool uses_two_handles;
};

static float scale(float x, float x1, float x2, float y1, float y2){
  return y1 + ( ((x-x1)*(y2-y1))
                /
                (x2-x1)
                );
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
    descriptor->run(data->handles[0], num_frames);
    if(type_data->uses_two_handles)
      descriptor->run(data->handles[1], num_frames);
  }
  for(int ch=0;ch<type->num_outputs;ch++)
    memcpy(outputs[ch],data->outputs[ch],sizeof(float)*num_frames);
}

static void delete_audio_ports(const SoundPluginType *type, Data *data){
  for(int i=0; i<type->num_inputs;i++)
    free(data->inputs[i]);
  free(data->inputs);

  for(int i=0; i<type->num_outputs;i++)
    free(data->outputs[i]);
  free(data->outputs);
}

static void setup_audio_ports(const SoundPluginType *type, Data *data, int block_size){
  TypeData *type_data = (TypeData*)type->data;
  const LADSPA_Descriptor *descriptor = type_data->descriptor;

  data->inputs=(float**)malloc(sizeof(float*)*type->num_inputs);
  data->outputs=(float**)malloc(sizeof(float*)*type->num_outputs);
  
  for(int i=0; i<type->num_inputs;i++)
    data->inputs[i]=(float*)calloc(sizeof(float),block_size);
  
  for(int i=0; i<type->num_outputs;i++)
    data->outputs[i]=(float*)calloc(sizeof(float),block_size);

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

static void *create_plugin_data(const SoundPluginType *plugin_type, SoundPlugin *plugin, float sample_rate, int block_size){
  Data *data = (Data*)calloc(1, sizeof(Data));
  TypeData *type_data = (TypeData*)plugin_type->data;
  const LADSPA_Descriptor *descriptor = type_data->descriptor;

  data->control_values = (float*)calloc(sizeof(float),descriptor->PortCount);

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

  free(data->control_values);
  free(data);
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

static const LADSPA_PortRangeHintDescriptor get_hintdescriptor(const SoundPluginType *plugin_type, int effect_num){
  TypeData *type_data = (TypeData*)plugin_type->data;
  const LADSPA_Descriptor *descriptor = type_data->descriptor;

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

  RWarning("Unknown effect\n",effect_num);
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

static void set_effect_value(SoundPlugin *plugin, int64_t time, int effect_num, float value, enum ValueFormat value_format){
  const SoundPluginType *type = plugin->type;
  TypeData *type_data = (TypeData*)type->data;
  Data *data = (Data*)plugin->data;

  if(value_format==PLUGIN_FORMAT_SCALED){

    LADSPA_PortRangeHintDescriptor hints = get_hintdescriptor(type,effect_num);

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

  if(value_format==PLUGIN_FORMAT_SCALED){
    const LADSPA_PortRangeHintDescriptor hints = get_hintdescriptor(type,effect_num);

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


static const char *get_effect_name(const SoundPluginType *plugin_type, int effect_num){
  TypeData *type_data = (TypeData*)plugin_type->data;
  const LADSPA_Descriptor *descriptor = type_data->descriptor;

  int effect_num2 = 0;
  for(unsigned int portnum=0;portnum<descriptor->PortCount;portnum++){
    const LADSPA_PortDescriptor portdescriptor = descriptor->PortDescriptors[portnum];
    if(LADSPA_IS_PORT_CONTROL(portdescriptor) && LADSPA_IS_PORT_INPUT(portdescriptor)){
      if(effect_num2==effect_num)
        return descriptor->PortNames[portnum];
      else
        effect_num2++;
    }
  }
  return NULL;
}

static int get_effect_format(const SoundPluginType *type, int effect_num){
  const LADSPA_PortRangeHintDescriptor hints = get_hintdescriptor(type,effect_num);

  if(LADSPA_IS_HINT_TOGGLED(hints))
    return EFFECT_FORMAT_BOOL;

  else if(LADSPA_IS_HINT_INTEGER(hints))
    return EFFECT_FORMAT_INT;

  else
    return EFFECT_FORMAT_FLOAT;
}

static void get_display_value_string(SoundPlugin *plugin, int effect_num, char *buffer, int buffersize){
  const SoundPluginType *type = plugin->type;
  Data *data = (Data*)plugin->data;
  float value = data->control_values[effect_num];

  switch(get_effect_format(type,effect_num)){
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
  return strdup(temp);
}

static void add_ladspa_plugin_type(QFileInfo file_info){
  QString filename = file_info.absoluteFilePath();

  fprintf(stderr,"Trying to open \"%s\"\n",filename.ascii());

  QLibrary myLib(filename);

  LADSPA_Descriptor_Function get_descriptor_func = (LADSPA_Descriptor_Function) myLib.resolve("ladspa_descriptor");

  if(get_descriptor_func==NULL){
    fprintf(stderr,"nope: \"%s\"\n",filename.ascii());
    return;
  }

  //printf("Resolved \"%s\"\n",myLib.fileName().ascii());

  const LADSPA_Descriptor *descriptor;

  for(int i = 0; (descriptor=get_descriptor_func(i)) != NULL ; i++){
    SoundPluginType *plugin_type = (SoundPluginType*)calloc(1,sizeof(SoundPluginType));
    TypeData *type_data = (TypeData*)calloc(1,sizeof(TypeData));

    plugin_type->data = type_data;
    type_data->descriptor = descriptor;

    QString basename = file_info.fileName();
    basename.resize(basename.size()-strlen(LIB_SUFFIX)-1);
    type_data->filename = strdup(basename.ascii());

    plugin_type->type_name = "Ladspa";
    plugin_type->name      = descriptor->Name;
    plugin_type->info      = create_info_string(descriptor);

    plugin_type->is_instrument = false;

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

    type_data->min_values     = (float*)calloc(sizeof(float),plugin_type->num_effects);
    type_data->default_values = (float*)calloc(sizeof(float),plugin_type->num_effects);
    type_data->max_values     = (float*)calloc(sizeof(float),plugin_type->num_effects);

    {
      int effect_num = 0;
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
          
          effect_num++;
        }
      }
    }

    plugin_type->buffer_size_is_changed = buffer_size_is_changed;

    plugin_type->RT_process = RT_process;
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
}

static SoundPluginType *get_plugin_type_from_id(unsigned long id){
  for(unsigned int i=0;i<g_plugin_types.size();i++){
    SoundPluginType *plugin_type = g_plugin_types[i];
    TypeData *type_data = (TypeData*)plugin_type->data;
    const LADSPA_Descriptor *descriptor = type_data->descriptor;
    if(descriptor->UniqueID == id)
      return plugin_type;
  }
  return NULL;
}

namespace{
struct plugin_type_pred{
  bool operator()(SoundPluginType *a, SoundPluginType *b) const{
    TypeData *type_data_a = (TypeData*)a->data;
    const LADSPA_Descriptor *descriptor_a = type_data_a->descriptor;

    TypeData *type_data_b = (TypeData*)b->data;
    const LADSPA_Descriptor *descriptor_b = type_data_b->descriptor;
    
    return strcasecmp(descriptor_a->Name, descriptor_b->Name) < 0;
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

// This function is copied from jack-rack by Bob Ham. Slightly modified.
static void get_dir_uris (std::vector<char*> &lrdf_uris, const char * dir){
  DIR * dir_stream;
  struct dirent * dir_entry;
  char * file_name;
  int err;
  size_t dirlen;
  char * extension;

  const char *separator = OS_get_directory_separator();
  
  dir_stream = opendir (dir);
  if (!dir_stream)
    return;
  
  dirlen = strlen (dir);
  
  while ( (dir_entry = readdir (dir_stream)) )
    {
      /* check if it's a .rdf or .rdfs */
      extension = strrchr (dir_entry->d_name, '.');
      if (!extension)
        continue;
      if (strcmp (extension, ".rdf") != 0 &&
          strcmp (extension, ".rdfs") != 0)
        continue;
  
      file_name = (char*)malloc (dirlen + 1 + strlen (dir_entry->d_name) + 1 + 7 + 100);
    
      sprintf(file_name,"%s:%s%s%s%s%s",
              "file",
              "","",
              //separator,
              //separator,
              dir,
              dir[strlen(dir)-1]==separator[0] ? "" : separator,
              dir_entry->d_name);
    
      lrdf_uris.push_back(file_name);

      printf("Found LRDF description file '%s'\n", file_name);
    }
  
  err = closedir (dir_stream);
  if (err)
    fprintf (stderr, "%s: error closing directory '%s': %s\n", __FUNCTION__, dir, strerror (errno));
}

// This function is copied from jack-rack by Bob Ham. Slightly modified.
static void get_path_uris (std::vector<char*> &lrdf_uris){
  char lrdf_path[2000];

#if __linux__

#if 0
  sprintf(lrdf_path,"%s:/usr/local/share/ladspa/rdf:/usr/share/ladspa/rdf:%s/rdf",
          getenv("LADSPA_RDF_PATH")==NULL ? "" : getenv("LADSPA_RDF_PATH"),
          OS_get_program_path());

#else

  sprintf(lrdf_path,"%s/rdf",
          OS_get_program_path());
#endif


  char *dir = strtok (lrdf_path, ":");
  do
    get_dir_uris (lrdf_uris, dir);
  while ((dir = strtok (NULL, ":")));

#endif // __linux__

#if defined(FOR_WINDOWS) || defined(FOR_MACOSX)
  sprintf(lrdf_path,"%s%srdf",OS_get_program_path(), OS_get_directory_separator());
  get_dir_uris(lrdf_uris,lrdf_path);
#endif
}

// This function is copied from jack-rack by Bob Ham. Slightly modified.
static void init_lrdf (){
  std::vector<char*> lrdf_uris;
  int err;
  
  get_path_uris (lrdf_uris);
  printf("Get paths\n");
  fflush(stdout);

  lrdf_init ();  
  printf("lrdf init\n");
  fflush(stdout);

  for(unsigned int i=0;i<lrdf_uris.size();i++){
    printf("Trying to read %d, size: %d. data: %s\n",i,(int)lrdf_uris.size(),lrdf_uris.at(i));
    fflush(stdout);
    err = lrdf_read_file (lrdf_uris.at(i));
    printf("Finished reading %d. Success? %s\n", i, err ? "no":"yes");
    fflush(stdout);

    if (err)
      fprintf (stderr, "%s: could not parse LRDF file '%s'\n", __FUNCTION__, lrdf_uris.at(i));
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

  std::vector<SoundPluginType*> diff(g_plugin_types.size());//(num_remaining_elements);
  std::vector<SoundPluginType*>::iterator end=std::set_difference(g_plugin_types.begin(), g_plugin_types.end(),
                                                                 g_added_plugin_types.begin(), g_added_plugin_types.end(),
                                                                 diff.begin(),
                                                                 plugin_type_pred());
  
  fprintf(stderr,"g_plugin_size: %d, added_size: %d, diff size: %d\n",(int)g_plugin_types.size(),(int)g_added_plugin_types.size(),(int)diff.size());

  //if(g_added_plugin_types.size()==0)
  //  return;

  PR_add_menu_entry(PluginMenuEntry::level_up("Uncategorized"));
  {
    char last=0;
    for(unsigned int i=0;i<diff.size() && diff.at(i)!=*end;i++){
      fprintf(stderr,"i: %d\n",(int)i);
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
  char ladspa_path[1024];

#if __linux__
  if(getenv("LADSPA_PATH")==NULL){
    QMessageBox::information(NULL, "LADSPA_PATH is not set.", "LADSPA_PATH is not set.");
    return;
  }
  sprintf(ladspa_path,"%s",getenv("LADSPA_PATH"));
#endif

#if defined(FOR_WINDOWS) || defined(FOR_MACOSX)
  sprintf(ladspa_path,"%s",QSring(QString(OS_get_program_path()) + OS_get_directory_separator() + "ladspa").ascii());
#endif

  char *dirname = strtok (ladspa_path, ":");
  do{

    QDir dir(dirname);

    dir.setFilter(QDir::Files | QDir::NoDotAndDotDot);
    dir.setSorting(QDir::Name);
    
    QFileInfoList list = dir.entryInfoList();
    
    for (int i = 0; i < list.size(); ++i) {
      QFileInfo fileInfo = list.at(i);
      if(fileInfo.suffix()==LIB_SUFFIX)
        add_ladspa_plugin_type(fileInfo);
    }

  }while ((dirname = strtok (NULL, ":")));

  init_menues();
}

/* Copyright 2013 Kjetil S. Matheussen

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



#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

#include <libpds.h>

#include "../common/nsmtracker.h"
#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"
#include "undo_pd_controllers_proc.h"

#include "../common/OS_Player_proc.h"

#include "../Qt/Qt_pd_plugin_widget_callbacks_proc.h"
#include "SoundPluginRegistry_proc.h"

#include "Qt_instruments_proc.h"

#include "Pd_plugin.h"

#include "Pd_plugin_proc.h"


#define NUM_CONTROLLERS 40

typedef struct{
  pd_t *pd;

  Pd_Controller controllers[NUM_CONTROLLERS];
  void *file;

  const char *directory;
  const char *filename;

  void *controller_binding;

  void *qtgui;
} Data;


static void RT_process(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs){
  //SoundPluginType *type = plugin->type;
  Data *data = (Data*)plugin->data;
  pd_t *pd = data->pd;

  libpds_process_float_noninterleaved(pd, num_frames / libpds_blocksize(pd), (const float**) inputs, outputs);
}

static void play_note(struct SoundPlugin *plugin, int64_t time, int note_num, float volume, float pan){
  Data *data = (Data*)plugin->data;
  pd_t *pd = data->pd;
  printf("####################################################### Setting volume to %f (play note)\n",volume);
  libpds_noteon(pd, 0, note_num, volume*127);
}

static void set_note_volume(struct SoundPlugin *plugin, int64_t time, int note_num, float volume){
  Data *data = (Data*)plugin->data;
  pd_t *pd = data->pd;
  printf("####################################################### Setting volume to %f\n",volume);
  libpds_polyaftertouch(pd, 0, note_num, volume*127);
}

static void stop_note(struct SoundPlugin *plugin, int64_t time, int note_num, float volume){
  Data *data = (Data*)plugin->data;
  pd_t *pd = data->pd;
  printf("####################################################### Setting pd volume to %f (stop note)\n",0.0f);
  libpds_noteon(pd, 0, note_num, 0);
}

static void set_effect_value(struct SoundPlugin *plugin, int64_t time, int effect_num, float value, enum ValueFormat value_format) {
  Data *data = (Data*)plugin->data;
  pd_t *pd = data->pd;
  float real_value;

  if(value_format==PLUGIN_FORMAT_SCALED && data->controllers[effect_num].type!=2)
    real_value = scale(value, 0.0, 1.0, 
                       data->controllers[effect_num].min_value, data->controllers[effect_num].max_value);
  else
    real_value = value;

  data->controllers[effect_num].value = real_value;
  const char *name = data->controllers[effect_num].name;
  if(name != NULL) {
    printf("####################################################### Setting pd volume to %f / real_value: %f, for -%s-\n",value, real_value,name);
    libpds_float(pd, name, real_value);
  }
}

static float get_effect_value(struct SoundPlugin *plugin, int effect_num, enum ValueFormat value_format) {
  Data *data = (Data*)plugin->data;
  float raw = data->controllers[effect_num].value;
  if(value_format==PLUGIN_FORMAT_SCALED && data->controllers[effect_num].type!=2)
    return scale(raw, data->controllers[effect_num].min_value, data->controllers[effect_num].max_value,
                 0.0f, 1.0f);
  else
    return raw;
}

static void get_display_value_string(SoundPlugin *plugin, int effect_num, char *buffer, int buffersize){
  Data *data = (Data*)plugin->data;
  const char *name = data->controllers[effect_num].name;
  if(data->controllers[effect_num].type==0)
    snprintf(buffer,buffersize-1,"%s: %f",name==NULL?"<not set>":name, data->controllers[effect_num].value);
  else
    snprintf(buffer,buffersize-1,"%s: %d",name==NULL?"<not set>":name, (int)data->controllers[effect_num].value);
}

static void show_gui(struct SoundPlugin *plugin){
  Data *data = (Data*)plugin->data;
  printf("####################################################### Showing Pd gui\n");
  PLAYER_lock();{
    libpds_show_gui(data->pd);
  }PLAYER_unlock();
}

static void hide_gui(struct SoundPlugin *plugin){
  Data *data = (Data*)plugin->data;
  printf("####################################################### Showing Pd gui\n");
  PLAYER_lock();{
    libpds_hide_gui(data->pd);
  }PLAYER_unlock();
}

static void pdprint(const char *s) {
  printf("PD. %s", s);
}

static void pdnoteon(int ch, int pitch, int vel) {
  printf("PD. noteon: %d %d %d\n", ch, pitch, vel);
}

static void pdfloathook(void *d, const char *sym, float val){
  Pd_Controller *controller = (Pd_Controller*)d;

  float old_value = controller->value;

  //printf("got value %f (old: %f) for %s (data->name: %s)\n",val,old_value,sym,data->controllers[0].name);

  float scaled_value = scale(val, controller->min_value, controller->max_value,
                             0.0f, 1.0f);

  if (fabs(old_value - val) > 1.0/10000.0)
    PLUGIN_set_effect_value(controller->plugin, -1, controller->num, scaled_value, PLUGIN_STORED_TYPE, PLUGIN_STORE_VALUE);
}

static void bind_receiver(Pd_Controller *controller){
  char receive_symbol_name[1024];
  snprintf(receive_symbol_name, 1023, "%s-receiver", controller->name);
  controller->pd_binding = libpds_bind(((Data*)controller->plugin->data)->pd, receive_symbol_name, controller);
}

static void add_controller(SoundPlugin *plugin, Data *data, const char *controller_name, int type){
  Pd_Controller *controller;
  int controller_num;
  for(controller_num=0;controller_num<NUM_CONTROLLERS;controller_num++) {
    controller = &data->controllers[controller_num];

    if(controller->name != NULL && !strcmp(controller->name, controller_name))
      return; // already there.

    if(controller->name == NULL || !strcmp(controller->name, ""))
      break;
  }

  if(controller_num==NUM_CONTROLLERS)
    return;

  controller->type = 0;
  controller->min_value = 0;
  controller->value = 0;
  controller->max_value = 1;  
  controller->name = strdup(controller_name);

  controller->has_gui = true;

  bind_receiver(controller);

  PDGUI_schedule_clearing(data->qtgui);
}

// Note that hooks are always called from the player thread.
static void pdmessagehook(void *d, const char *source, const char *controller_name, int argc, t_atom *argv){
  SoundPlugin *plugin = (SoundPlugin*)d;
  Data *data = (Data*)plugin->data;

  t_atom a = argv[0];
  char *type_name = libpd_get_symbol(a); // do something with the C string s
  printf("Got something: -%s- -%s- -%s-\n",source,controller_name,type_name);

  if (!strcmp(type_name, "float")) 
    add_controller(plugin, data, controller_name, 0);
  else
    printf("Unknown type: -%s-\n",type_name);
}

static void *create_plugin_data(const SoundPluginType *plugin_type, struct SoundPlugin *plugin, float sample_rate, int block_size){
  Data *data = calloc(1,sizeof(Data));

  int i;
  for(i=0;i<NUM_CONTROLLERS;i++) {
    data->controllers[i].plugin = plugin;
    data->controllers[i].num = i;
    data->controllers[i].max_value = 1.0f;
  }

  int blocksize;
  pd_t *pd;

  PLAYER_lock();{
    pd = libpds_create(true, "/home/ksvalast/libpd/pure-data");
    data->pd = pd;

    libpds_set_printhook(pd, pdprint);
    libpds_set_noteonhook(pd, pdnoteon);
    libpds_set_floathook(pd, pdfloathook);
    libpds_set_messagehook(pd, pdmessagehook);

    libpds_init_audio(pd, 2, 2, sample_rate);


    blocksize = libpds_blocksize(pd);
  }PLAYER_unlock();

  if( (block_size % blocksize) != 0)
    RWarning("PD's blocksize of %d is not dividable by Radium's block size of %d. You will get bad sound. Adjust your audio settings.", blocksize, block_size);

  // compute audio    [; pd dsp 1(
  PLAYER_lock();{
    libpds_start_message(pd, 1); // one entry in list
    libpds_add_float(pd, 1.0f);
    libpds_finish_message(pd, "pd", "dsp");

    plugin->data = data; // plugin->data is used before this function ends.
    data->controller_binding = libpds_bind(pd, "radium_controller", plugin);

    data->directory = "/home/ksvalast/libpd/samples/guiTest/";
    data->filename = "test.pd";
    data->file = libpds_openfile(pd, data->filename, data->directory);
  }PLAYER_unlock();

  printf("####################################################### Setting pd volume to 0.5f (create_plugin_data)\n");

  return data;
}

static void cleanup_plugin_data(SoundPlugin *plugin){
  Data *data = (Data*)plugin->data;
  printf(">>>>>>>>>>>>>> Cleanup_plugin_data called for %p\n",plugin);

  PLAYER_lock();{
    libpds_closefile(data->pd, data->file);
    
    libpds_delete(data->pd);
  }PLAYER_unlock();

  PDGUI_clear(data->qtgui);

  int i;
  for(i=0;i<NUM_CONTROLLERS;i++){
    free(data->controllers[i].name);
  }

  free(data);
}

static const char *get_effect_name(const struct SoundPluginType *plugin_type, int effect_num){
  static char names[NUM_CONTROLLERS][128];
  static bool inited=false;
  if(inited==false){
    int i;
    for(i=0;i<NUM_CONTROLLERS;i++)
      sprintf(names[i],"Pd_Controller %d",i);
    inited=true;
  }
  return names[effect_num];
}

void PD_set_qtgui(SoundPlugin *plugin, void *qtgui){
  Data *data = (Data*)plugin->data;
  data->qtgui = qtgui;
}

Pd_Controller *PD_get_controller(SoundPlugin *plugin, int n){
  Data *data = (Data*)plugin->data;
  return &data->controllers[n];
}

void PD_set_controller_name(SoundPlugin *plugin, int n, const char *name){
  Data *data = (Data*)plugin->data;
  Pd_Controller *controller = &data->controllers[n];
  char *old_name = controller->name;

  if(old_name!=NULL && !strcmp(old_name, name))
    return;

  char *new_name = strdup(name);

  // Should check if it is different before binding. (but also if it is already binded)

  PLAYER_lock();{
    controller->name = new_name;

    if(controller->pd_binding != NULL)
      libpds_unbind(data->pd, controller->pd_binding);

    bind_receiver(controller);

  }PLAYER_unlock();

  free(old_name);  
}


void PD_recreate_controllers_from_state(SoundPlugin *plugin, hash_t *state){
  Data *data=(Data*)plugin->data;

  PDGUI_clear(data->qtgui);

  int i;
  for(i=0;i<NUM_CONTROLLERS;i++) {
    Pd_Controller *controller = &data->controllers[i];

    if(controller->pd_binding!=NULL) {
      PLAYER_lock();{
        libpds_unbind(data->pd, controller->pd_binding);
      }PLAYER_unlock();
      controller->pd_binding = NULL;
    }

    const char *name = HASH_has_key_at(state, "name", i) ? HASH_get_string_at(state, "name", i) : NULL;
    if(name==NULL || !strcmp(name,""))
      controller->name    = NULL;
    else {
      controller->name    = strdup(name);
    }
    controller->type      = HASH_get_int_at(state, "type", i);
    controller->min_value = HASH_get_float_at(state, "min_value", i);
    controller->value = HASH_get_float_at(state, "value", i);
    controller->max_value = HASH_get_float_at(state, "max_value", i);
    controller->has_gui   = HASH_get_int_at(state, "has_gui", i)==1 ? true : false;

    if(controller->name != NULL) {
      PLAYER_lock();{
        bind_receiver(controller);
      }PLAYER_unlock();
    }
  }

  if(plugin->patch != NULL)
    GFX_update_instrument_widget(plugin->patch);
}


static void recreate_from_state(struct SoundPlugin *plugin, hash_t *state){
  PD_recreate_controllers_from_state(plugin, state);
}

void PD_create_controllers_from_state(SoundPlugin *plugin, hash_t *state){
  Data *data=(Data*)plugin->data;

  HASH_put_string(state, "directory", data->directory);
  HASH_put_string(state, "filename", data->filename);

  int i;
  for(i=0;i<NUM_CONTROLLERS;i++) {
    Pd_Controller *controller = &data->controllers[i];
    if(controller->name != NULL)
      HASH_put_string_at(state, "name", i, controller->name);
    HASH_put_int_at(state, "type", i, controller->type);
    HASH_put_float_at(state, "min_value", i, controller->min_value);
    HASH_put_float_at(state, "value", i, controller->value);
    HASH_put_float_at(state, "max_value", i, controller->max_value);
    HASH_put_int_at(state, "has_gui", i, controller->has_gui ? 1 : 0);
  }
}

static void create_state(struct SoundPlugin *plugin, hash_t *state){
  PD_create_controllers_from_state(plugin, state);
}

// Warning! undo is created here (for simplicity). It's not common to call the undo creation function here, so beware of possible circular dependencies in the future.
void PD_delete_controller(SoundPlugin *plugin, int controller_num){
  Data *data=(Data*)plugin->data;

  Undo_PdControllers_CurrPos(plugin->patch);

  int i;
  hash_t *state = HASH_create(NUM_CONTROLLERS);

  for(i=0;i<NUM_CONTROLLERS-1;i++) {
    int s = i>=controller_num ? i+1 : i;
    Pd_Controller *controller = &data->controllers[s];
    HASH_put_string_at(state, "name", i, controller->name);
    HASH_put_int_at(state, "type", i, controller->type);
    HASH_put_float_at(state, "min_value", i, controller->min_value);
    HASH_put_float_at(state, "value", i, controller->value);
    HASH_put_float_at(state, "max_value", i, controller->max_value);
    HASH_put_int_at(state, "has_gui", i, controller->has_gui);
  }

  HASH_put_string_at(state, "name", NUM_CONTROLLERS-1, "");
  HASH_put_int_at(state, "type", NUM_CONTROLLERS-1, 0);
  HASH_put_float_at(state, "min_value", NUM_CONTROLLERS-1, 0.0);
  HASH_put_float_at(state, "value", NUM_CONTROLLERS-1, 0.0);
  HASH_put_float_at(state, "max_value", NUM_CONTROLLERS-1, 1.0);
  HASH_put_int_at(state, "has_gui", NUM_CONTROLLERS-1, 0);

  recreate_from_state(plugin, state);
}


static SoundPluginType plugin_type = {
 type_name                : "Pd",
 name                     : "Pd",
 info                     : "Pd is (mainly) made by Miller Puckette. Radium uses a modified version of libpd to access it.",
 num_inputs               : 2,
 num_outputs              : 2,
 is_instrument            : true,
 note_handling_is_RT      : false,
 num_effects              : NUM_CONTROLLERS,
 get_effect_format        : NULL,
 get_effect_name          : get_effect_name,
 effect_is_RT             : NULL,
 create_plugin_data       : create_plugin_data,
 cleanup_plugin_data      : cleanup_plugin_data,

 show_gui         : show_gui,
 hide_gui         : hide_gui,

 RT_process       : RT_process,
 play_note        : play_note,
 set_note_volume  : set_note_volume,
 stop_note        : stop_note,
 set_effect_value : set_effect_value,
 get_effect_value : get_effect_value,
 get_display_value_string : get_display_value_string,

 recreate_from_state : recreate_from_state,
 create_state        : create_state,

 data                     : NULL
};

void create_pd_plugin(void){
  PR_add_plugin_type(&plugin_type);
}



/* Copyright 2020 Kjetil S. Matheussen

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

#include <memory>

#include <QPair>
#include <QSet>

#include "../common/nsmtracker.h"
#include "../common/Vector.hpp"
#include "../common/patch_proc.h"
#include "../common/hashmap_proc.h"

#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"
#include "SoundProducer_proc.h"
#include "SoundPluginRegistry_proc.h"
#include "Juce_plugins_proc.h"


#include "SendReceive_plugins_proc.h"


#define DO_DEBUG 0

#if DO_DEBUG
#  if defined(RELEASE)
#    error "error"
#  endif
#  define D(n) n
#else
#  define D(n)
#endif




static SoundPluginType send_type2 = {};
static SoundPluginType receive_type2 = {};

static SoundPluginType send_type8 = {};
static SoundPluginType receive_type8 = {};

SoundPluginType *g_receive_type2 = &receive_type2;
SoundPluginType *g_receive_type8 = &receive_type8;

namespace{

  class RT_Ch{
    radium::Mutex _mutex; // must be locked when writing to _audio or _has_valid_data when sending.
    
    std::unique_ptr<float> _audio;

  public:
    
    bool _has_valid_data = false;
    
    RT_Ch(int block_size)
      : _audio(new float[block_size])
    {
    }
    
    void receive(float *output, int num_frames) {
      R_ASSERT_NON_RELEASE(THREADING_is_player_or_runner_thread());

      if (_has_valid_data) {
        
        memcpy(output, _audio.get(), sizeof(float) * num_frames);
        _has_valid_data = false; // probably not necessary.
        
      } else {
        
        memset(output, 0, sizeof(float)*num_frames);
        
      }
    }
    
    void send(float *input, int num_frames){
      R_ASSERT_NON_RELEASE(THREADING_is_player_or_runner_thread());

      radium::ScopedMutex lock(_mutex); // Locking should be perfectly fine here. All visitors should be RT_runner threads or the player thread.
      
      if (_has_valid_data) {
        
        JUCE_add_sound(_audio.get(), input, num_frames);

      } else {
        
        memcpy(_audio.get(), input, sizeof(float) * num_frames);
        _has_valid_data = true;
        
      }
    }
    
  };

  struct Data {
    QString _name = "";

    Data(hash_t *state){
      if (state)
        if (HASH_has_key(state, "name"))
          _name = HASH_get_qstring(state, "name");
    }
  };
  
  struct ReceiveData : public Data {

    SoundPlugin *plugin;
    int num_channels;
    bool RT_compensate_latency = true; // must hold player lock when writing.
    
    // These two variables are swapped at each cycle.
    RT_Ch **RT_receive_chs; // Used by the receiver.
    RT_Ch **RT_send_chs;    // Used by the senders

    
    ReceiveData(SoundPlugin *plugin, hash_t *state, int num_channels, int block_size)
      : Data(state)
      , plugin(plugin)
      , num_channels(num_channels)
    {
      if (state)
        if (HASH_has_key(state, "compensate_latency"))
          RT_compensate_latency = HASH_get_bool(state, "compensate_latency");
      
      RT_receive_chs = (RT_Ch **)V_malloc(sizeof(RT_Ch*) * num_channels);
      RT_send_chs = (RT_Ch **)V_malloc(sizeof(RT_Ch*) * num_channels);
  
      for(int ch=0;ch<num_channels;ch++){
        RT_receive_chs[ch] = new RT_Ch(block_size);
        RT_send_chs[ch] = new RT_Ch(block_size);
      }

    }

    ~ReceiveData(){
      for(int ch=0;ch<num_channels;ch++){
        delete RT_receive_chs[ch];
        delete RT_send_chs[ch];
      }
      
      V_free(RT_receive_chs);
      V_free(RT_send_chs);
    }

  };
  
  struct SendData : public Data {
    
    radium::Vector<ReceiveData*> *_receivers = new radium::Vector<ReceiveData*>;

    SendData(hash_t *state)
      : Data(state)
    {}

    ~SendData(){
      delete _receivers;
    }

    radium::Vector<ReceiveData*> *create_updated_receivers(const QVector<ReceiveData*> &from_receivers) const {
      radium::Vector<ReceiveData*> *to_receivers = new radium::Vector<ReceiveData*>;
      
      for(ReceiveData* receiver : from_receivers)
        to_receivers->push_back(receiver);

      return to_receivers;
    }
    
    radium::Vector<ReceiveData*> *maybe_create_updated_receivers(const QVector<ReceiveData*> &new_receivers) const {
      if (_receivers->size() != new_receivers.size())
        return create_updated_receivers(new_receivers);
      
      for(auto *new_receiver : new_receivers)
        if (!_receivers->contains(new_receiver))
          return create_updated_receivers(new_receivers);

      return NULL;
    }

  };
}


/*
static radium::mutex g_names_map_lock;

static const QHash<const_char*, ReceiveData*> *g_RT_names_map; // g_names_lock is 
*/



static void RT_receive_process(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs){
  ReceiveData *data = static_cast<ReceiveData*>(plugin->data);

  D(printf("Receiving ? -> %s. num_channels: %d\n", plugin->patch->name, data->num_channels));
  
  for(int ch=0;ch<data->num_channels;ch++)
    data->RT_receive_chs[ch]->receive(outputs[ch], num_frames);
}


static void RT_send_process(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs){
  SendData *data = static_cast<SendData*>(plugin->data);
  
  for(ReceiveData *receiver : *data->_receivers) {

    RT_PLUGIN_touch(receiver->plugin);
    
    int num_channels = R_MIN(plugin->type->num_inputs, receiver->num_channels);

    D(printf("Sending %s -> %s. Num channels: %d\n", plugin->patch->name, receiver->plugin->patch->name, num_channels));

    for(int ch=0;ch<num_channels;ch++)
      receiver->RT_send_chs[ch]->send(inputs[ch], num_frames);
  }
}

namespace{
  enum class Type{
    SENDER,
    RECEIVER,
    SOMETHING_ELSE
  };
}

static Type get_type(SoundPlugin *plugin){
  if (plugin->type==&send_type2 || plugin->type==&send_type8)
    return Type::SENDER;
  else if (plugin->type==&receive_type2 || plugin->type==&receive_type8)
    return Type::RECEIVER;
  else
    return Type::SOMETHING_ELSE;
}

static bool is_sender(SoundPlugin *plugin){
  return get_type(plugin)==Type::SENDER;
}

static bool is_receiver(SoundPlugin *plugin){
  return get_type(plugin)==Type::RECEIVER;
}

static Data *get_data(SoundPlugin *plugin){
  switch(get_type(plugin)){      
    case Type::SENDER: return static_cast<SendData*>(plugin->data);
    case Type::RECEIVER: return static_cast<ReceiveData*>(plugin->data);
    case Type::SOMETHING_ELSE:
      break;
  }

  R_ASSERT(false);
  return new Data(NULL);
}

static void update_all_send_receivers(ReceiveData *receiver_to_be_deleted = NULL){
  QSet<QString> names;
  
  QHash<QString, QVector<ReceiveData*>> receiverss;
  QHash<QString, QVector<SendData*>> senderss;

  D(printf("-----------------------------\n"));
  
  for(auto *sp : MIXER_get_all_SoundProducers()){
    
    SoundPlugin *plugin = SP_get_plugin(sp);
    
    switch(get_type(plugin)){
      
      case Type::SENDER:
        {
          SendData *sender = static_cast<SendData*>(plugin->data);

          names << sender->_name;
          senderss[sender->_name].push_back(sender);
          D(printf("  Sender \"%s\" -> \"%s\"\n", plugin->patch->name, sender->_name.toUtf8().constData()));
        }
        break;
        
      case Type::RECEIVER:
        {
          ReceiveData *receiver = static_cast<ReceiveData*>(plugin->data);
          
          if (receiver != receiver_to_be_deleted) {
            
            names << receiver->_name;
            receiverss[receiver->_name].push_back(receiver);
            D(printf("  Receiver \"%s\" -> \"%s\"\n", plugin->patch->name, receiver->_name.toUtf8().constData()));
            
          } else {
            
            R_ASSERT_NON_RELEASE(false); // Plugins are removed from audio graph before they are deleted.
            
          }
        }
        break;
        
      case Type::SOMETHING_ELSE:
        break;
    }
  }
  
  struct Update{
    SendData *sender;
    radium::Vector<ReceiveData*> *receivers;
    Update(){ // stupid c++
    }
    Update(SendData *sender, radium::Vector<ReceiveData*> *receivers)
      : sender(sender)
      , receivers(receivers)
    {
      R_ASSERT(sender!=NULL);
      R_ASSERT(receivers!=NULL);
    }
  };
  
  QVector<Update> updates;
  
  for(QString name : names){
    QVector<ReceiveData*> receivers = receiverss[name];
    printf("AA %s: %d\n", name.toUtf8().constData(), senderss[name].size());
    
    for(SendData *sender : senderss[name]){
      radium::Vector<ReceiveData*> *new_receivedata = sender->maybe_create_updated_receivers(receivers);
      printf("BB: %p. Num receivers for sender: %d\n", new_receivedata, new_receivedata==NULL ? -1 : new_receivedata->size());
      
      if (new_receivedata != NULL)        
        updates.push_back(Update(sender, new_receivedata));
    }
  }
  
  printf("Size of names: %d. Size of updates: %d\n", names.size(), updates.size());
         
  // don't really trust c++...
  for(auto update : updates){
    R_ASSERT_RETURN_IF_FALSE(update.receivers!=NULL);
    R_ASSERT_RETURN_IF_FALSE(update.sender!=NULL);
    R_ASSERT_RETURN_IF_FALSE(update.sender->_receivers!=NULL);
  }

  
  if (updates.size() > 0) {
    
    QVector<radium::Vector<ReceiveData*>*> to_free;
    
    for(auto update : updates)
      to_free.push_back(update.sender->_receivers);

    // Apply all changes at once.
    {
      radium::PlayerLock lock;
      for(auto update : updates)
        update.sender->_receivers = update.receivers;
    }
    
    for(auto *receivers : to_free)
      delete receivers;
  }

  printf("-----------------------------\n");
}

void SEND_RECEIVE_update_send_receivers(void){
  update_all_send_receivers();
}

const char *SEND_RECEIVE_maybe_generate_patch_name(SoundPlugin *plugin){
  switch(get_type(plugin)){
      
    case Type::SENDER:
      {
        SendData *sender = static_cast<SendData*>(plugin->data);
        return talloc_format("%S=>", STRING_create(sender->_name));
      }
      break;
      
    case Type::RECEIVER:
      {
        ReceiveData *receiver = static_cast<ReceiveData*>(plugin->data);
        return talloc_format("=>%S", STRING_create(receiver->_name));
      }
      
    case Type::SOMETHING_ELSE:
      break;
  }

  return NULL;
}

const wchar_t *SEND_RECEIVE_get_name(SoundPlugin *plugin){
  return STRING_create(get_data(plugin)->_name);

#if 0
    
  struct Patch *patch = plugin->patch;
  if (patch==NULL){
    R_ASSERT_NON_RELEASE(false);
    return "";
  }

  R_ASSERT_RETURN_IF_FALSE2(patch->name!=NULL, "");

  //QString name(patch->name);
  
  wchar_t *name = STRING_create(patch->name);
  int dashpos = STRING_find_pos(name, 0, "=>");
  
  if (dashpos==-1)
    return "";

  if (is_sender(plugin))
    return STRING_get_chars(STRING_trim(STRING_remove_end(name, dashpos)));
  else
    return STRING_get_chars(STRING_trim(STRING_remove_start(name, dashpos + 2)));
#endif
}

bool SEND_RECEIVE_set_name(SoundPlugin *plugin, const wchar_t *new_name, bool update_patch_name){
  struct Patch *patch = plugin->patch;
  if (patch==NULL){
    R_ASSERT_NON_RELEASE(false);
    return false;
  }

  R_ASSERT_RETURN_IF_FALSE2(get_type(plugin)!=Type::SOMETHING_ELSE, false);
      
  new_name = STRING_trim(new_name);

  {
    const wchar_t *old_name;
    
    old_name = STRING_create(get_data(plugin)->_name);
    
    old_name = STRING_trim(old_name);
    
    if (STRING_equals2(new_name, old_name))
      return false;
  }
  
  get_data(plugin)->_name = STRING_get_qstring(new_name);

  update_all_send_receivers();

  if (update_patch_name){
    PATCH_set_name2(patch,
                    talloc_format(is_sender(plugin) ? "%S=>" : "=>%S",
                                  new_name
                                  ),
                    false
                    );
    patch->name_is_edited = true;
  }
      
  return true;
}


bool SEND_RECEIVE_handle_new_patchname(SoundPlugin *plugin, const char *s_patch_name){

  Type type = get_type(plugin);

  if (type==Type::SOMETHING_ELSE)
    return false;
  
  wchar_t *patch_name = STRING_create(s_patch_name);
  
  int dash_pos = STRING_find_pos(patch_name, 0, "=>");

  wchar_t *new_name;
  
  if (dash_pos==-1)
    new_name = patch_name;
  else if (type==Type::SENDER)
    new_name = STRING_remove_end(patch_name, dash_pos);
  else
    new_name = STRING_remove_start(patch_name, dash_pos+2);

  return SEND_RECEIVE_set_name(plugin, STRING_trim(new_name), false);
}


void SEND_RECEIVE_set_compensate_latency(SoundPlugin *plugin, bool doit){
  R_ASSERT_RETURN_IF_FALSE(is_receiver(plugin));
  ReceiveData *receiver = static_cast<ReceiveData*>(plugin->data);

  if (receiver->RT_compensate_latency==doit)
    return;
  
  {
    radium::PlayerLock lock;
    receiver->RT_compensate_latency = doit;
  }
}
  
bool SEND_RECEIVE_get_compensate_latency(SoundPlugin *plugin){
  R_ASSERT_RETURN_IF_FALSE2(is_receiver(plugin), false);
  ReceiveData *receiver = static_cast<ReceiveData*>(plugin->data);
  return receiver->RT_compensate_latency;
}
  
// This function is called from the audio engine before starting a new cycle.
void RT_SEND_RECEIVE_swap_receiver_send_channels(SoundPlugin *plugin){
  R_ASSERT_NON_RELEASE(is_receiver(plugin));
  
  ReceiveData *receiver = static_cast<ReceiveData*>(plugin->data);

  // swap
  auto *temp = receiver->RT_receive_chs;
  receiver->RT_receive_chs = receiver->RT_send_chs;
  receiver->RT_send_chs = temp;

  for(int ch=0;ch<receiver->num_channels;ch++)
    receiver->RT_send_chs[ch]->_has_valid_data = false;
}

static void *create_receive_plugin_data(const SoundPluginType *plugin_type, struct SoundPlugin *plugin, hash_t *state, float sample_rate, int block_size, bool is_loading){
  auto *ret = (void*)new ReceiveData(plugin, state, plugin_type->num_outputs, block_size);
  if (!is_loading)
    update_all_send_receivers();
  return ret;
}


static void *create_send_plugin_data(const SoundPluginType *plugin_type, struct SoundPlugin *plugin, hash_t *state, float sample_rate, int block_size, bool is_loading){
  auto *ret = (void*)new SendData(state);
  if (!is_loading)
    update_all_send_receivers();
  return ret;
}


static void cleanup_receive_plugin_data(SoundPlugin *plugin){
  ReceiveData *receiver = static_cast<ReceiveData*>(plugin->data);
  update_all_send_receivers(receiver);
  
  delete receiver;
}



static void cleanup_send_plugin_data(SoundPlugin *plugin){
  delete static_cast<SendData*>(plugin->data);
  update_all_send_receivers();
}

static void create_send_state(struct SoundPlugin *plugin, hash_t *state){
  SendData *sender = static_cast<SendData*>(plugin->data);
  
  HASH_put_string(state, "name", sender->_name);
}

static void create_receiver_state(struct SoundPlugin *plugin, hash_t *state){
  ReceiveData *receiver = static_cast<ReceiveData*>(plugin->data);
  
  HASH_put_string(state, "name", receiver->_name);
  HASH_put_bool(state, "compensate_latency", receiver->RT_compensate_latency);
}

static int RT_send_get_audio_tail_length(const struct SoundPlugin *plugin){
  return 0;
}

static int RT_receive_get_audio_tail_length(const struct SoundPlugin *plugin){
  ReceiveData *receiver = static_cast<ReceiveData*>(plugin->data);
  
  if(receiver->RT_compensate_latency)
    return 0;
  else
    return MIXER_get_buffer_size();
}

static int RT_receive_get_latency(const struct SoundPlugin *plugin){
  ReceiveData *receiver = static_cast<ReceiveData*>(plugin->data);
  
  if(receiver->RT_compensate_latency)
    return MIXER_get_buffer_size();
  else
    return 0;
}


void create_sendreceive_plugins(void){
  static bool has_inited = false;

  if (has_inited==false)
  {
    // send 2ch
    //////////////
    send_type2.type_name                = "Send";
    send_type2.name                     = "Send";
    send_type2.info                     = "Send and Send8 send audio to all Receive and Receive8 objects with the same name. Audio is delayed by one Radium buffer so this is not the same as using an audio connection. The main purpose of Send/Receive is to create recursive audio graphs.";
    send_type2.num_inputs               = 2;
    send_type2.num_outputs              = 0;
    send_type2.is_instrument            = false;
    send_type2.num_effects              = 0;
    send_type2.will_always_autosuspend  = true,
    send_type2.create_plugin_data       = create_send_plugin_data;
    send_type2.cleanup_plugin_data      = cleanup_send_plugin_data;
    send_type2.create_state             = create_send_state;
    
    send_type2.RT_process               = RT_send_process;
    send_type2.RT_get_audio_tail_length = RT_send_get_audio_tail_length;
      

    // send 8ch
    ////////////
    send_type8.type_name                = "Send";
    send_type8.name                     = "Send8";
    send_type8.info                     = send_type2.info;
    send_type8.num_inputs               = 8;
    send_type8.num_outputs              = 0;
    send_type8.is_instrument            = false;
    send_type8.num_effects              = 0;
    send_type8.will_always_autosuspend  = true,
    send_type8.create_plugin_data       = create_send_plugin_data;
    send_type8.cleanup_plugin_data      = cleanup_send_plugin_data;
    send_type8.create_state             = create_send_state;
    
    send_type8.RT_process               = RT_send_process;
    send_type8.RT_get_audio_tail_length = RT_send_get_audio_tail_length;
    
    //info
  
    // receive 2ch
    //////////////
    receive_type2.type_name                = "Receive";
    receive_type2.name                     = "Receive";
    receive_type2.info                     = send_type2.info;
    receive_type2.num_inputs               = 0;
    receive_type2.num_outputs              = 2;
    receive_type2.is_instrument            = false;
    receive_type2.num_effects              = 0;
    receive_type2.will_always_autosuspend  = true,
    receive_type2.create_plugin_data       = create_receive_plugin_data;
    receive_type2.cleanup_plugin_data      = cleanup_receive_plugin_data;
    receive_type2.create_state             = create_receiver_state;
    
    receive_type2.RT_process               = RT_receive_process;
    receive_type2.RT_get_audio_tail_length = RT_receive_get_audio_tail_length;
    receive_type2.RT_get_latency           = RT_receive_get_latency;
      

    // receive 8ch
    ////////////
    receive_type8.type_name                = "Receive";
    receive_type8.name                     = "Receive8";
    receive_type8.info                     = send_type2.info;
    receive_type8.num_inputs               = 0;
    receive_type8.num_outputs              = 8;
    receive_type8.is_instrument            = false;
    receive_type8.num_effects              = 0;
    receive_type8.will_always_autosuspend  = true,
    receive_type8.create_plugin_data       = create_receive_plugin_data;
    receive_type8.cleanup_plugin_data      = cleanup_receive_plugin_data;
    receive_type8.create_state             = create_receiver_state;
    
    receive_type8.RT_process               = RT_receive_process;
    receive_type8.RT_get_audio_tail_length = RT_receive_get_audio_tail_length;
    receive_type8.RT_get_latency           = RT_receive_get_latency;
  }

  
  has_inited = true;

  PR_add_plugin_type(&send_type2);
  PR_add_plugin_type(&send_type8);
  PR_add_plugin_type(&receive_type2);
  PR_add_plugin_type(&receive_type8);
}

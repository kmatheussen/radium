
#include <QHash>

#include "../common/nsmtracker.h"
#include "../common/Mutex.hpp"
#include "../common/Vector.hpp"
#include "../common/threading_lowlevel.h"
#include "../common/settings_proc.h"

#include "OS_midi_proc.h"
#include "midi_i_input_proc.h"


#include "midi_ports_proc.h"


namespace radium{

struct MidiOutputPort {
  radium::Mutex _midi_out_mutex;

  int _num_users = 0;

  const symbol_t *_port_name;
  
  radium::MidiOutputPortOs *_os_port;
  
  bool _is_RT;
  
  MidiOutputPort(const symbol_t *port_name, radium::MidiOutputPortOs *os_port, bool is_RT)
    : _port_name(port_name)
    , _os_port(os_port)
    , _is_RT(is_RT)
  {}

  ~MidiOutputPort(){
    if (_num_users==0){
      close_os_port();
    } else {
      R_ASSERT_NON_RELEASE(false);
    }
  }

  void close_os_port(void){
    R_ASSERT_RETURN_IF_FALSE(_num_users==0);
    R_ASSERT_RETURN_IF_FALSE(_os_port != NULL);
    MIDI_OS_delete_output_port(_os_port);
    _os_port = NULL;
  }
    
  /* Keep track of last used LSB/MSB/preset. If patchdata has different values than this (when playing a note), new values are sent to the midi port. */
  char _LSB[16] = {};
  char _MSB[16] = {};
  char _preset[16] = {};

  /* To keep track of how many times the notes has to be turned off. */
  int _num_ons[16][128] = {};
};

}

static QHash<const symbol_t*, radium::MidiOutputPort*> g_output_ports;

const symbol_t *MIDIPORT_get_port_name(radium::MidiOutputPort *port){
  return port->_port_name;
}




/******************************************************/
/**************** Output ******************************/
/******************************************************/


radium::MidiOutputPort *MIDIPORT_open_output(const char *name, bool create_new_if_not_existing, bool may_be_used_in_realtime_code){
  const symbol_t *port_name = MIDI_OS_get_port_name(name);
  
  radium::MidiOutputPort *port = g_output_ports.value(port_name, NULL);

  if (port==NULL){
    
    radium::MidiOutputPortOs *os_port = MIDI_OS_create_output_port(port_name, create_new_if_not_existing);
    
    if (os_port==NULL) {
      R_ASSERT(create_new_if_not_existing==false);
      return NULL;
    }
    
    port = new radium::MidiOutputPort(port_name, os_port, may_be_used_in_realtime_code);

    R_ASSERT(!g_output_ports.contains(port_name));
    
    g_output_ports[port_name] = port;
  }

  if (may_be_used_in_realtime_code && !port->_is_RT) {
    radium::ScopedMutex lock(port->_midi_out_mutex);

    port->_is_RT = true;
  }
    
  port->_num_users++;

  return port;  
}

void MIDIPORT_close_output(radium::MidiOutputPort *port){
  R_ASSERT_RETURN_IF_FALSE(port != NULL);
  R_ASSERT_RETURN_IF_FALSE(port->_num_users > 0);

  port->_num_users--;

  if (port->_num_users==0) {
    R_ASSERT_RETURN_IF_FALSE(g_output_ports.remove(port->_port_name)==1);

    // FIX: This is a memory leak. It's here because I got a crash once that could indicate that we were using a deleted port.
#if 0
    delete port;
#else
    port->close_os_port();
#endif
    
  }
}

static void OnOffNotesTrack(
                            radium::MidiOutputPort *midi_port,
                            const int cc,
                            const int data1,
                            const int data2
){
  if (cc >= 0xa0 || cc < 0x80)
    return;
  
  const int channel = cc & 0xf;
  const int command = cc & 0xf0;

  if (command < 0x90) {
    
    if(midi_port->_num_ons[channel][data1]>0)
      midi_port->_num_ons[channel][data1]--;
    
  } else {

    if (data2==0) {
      
      if(midi_port->_num_ons[channel][data1]>0)
        midi_port->_num_ons[channel][data1]--;
      
    } else {

      midi_port->_num_ons[channel][data1]++;
      
    }
  }
}

static void HandlePreset(radium::MidiOutputPort *midi_port,
                         const int cc,
                         const int data1,
                         const int data2,
                         const int LSB,
                         const int MSB,
                         const int preset
                         )
{

    if (cc >= 0xa0 || cc < 0x80)
      return;
    
    const int channel = cc & 0xf;

    bool set_preset = false;
    
    if (LSB != -1 && (midi_port->_LSB[channel] != LSB || midi_port->_MSB[channel] != MSB)) {

      if(MSB!=-1){
        MIDI_OS_sendMessage(midi_port->_os_port,
                            0xb0|channel,
                            32,
                            MSB
                            );
        
        midi_port->_MSB[channel] = MSB;
      }

      MIDI_OS_sendMessage(midi_port->_os_port,
                          0xb0|channel,
                          32,
                          LSB
                          );

      midi_port->_LSB[channel] = LSB;

      set_preset = true;

    } else if (preset!=-1 && midi_port->_preset[channel] != preset) {

      set_preset = true;      

    }

    if (set_preset) {
      
      MIDI_OS_sendMessage(midi_port->_os_port,
                          0xc0|channel,
                          preset,
                          0
                          );
      
      midi_port->_preset[channel] = preset;

    }
}


void MIDIPORT_send_message(
                           radium::MidiOutputPort *midi_port,
                           const int data1,
                           const int data2,
                           const int data3,
                           const int LSB,
                           const int MSB,
                           const int preset
                           )
{
#if !defined(RELEASE)
#if !defined(FOR_MACOSX)
  if (THREADING_has_player_thread_priority()) {
    
    if (!midi_port->_is_RT)
      abort();

  } else {

    if (!THREADING_is_main_thread())
      abort();
  }
#endif
#endif

  R_ASSERT_RETURN_IF_FALSE(midi_port->_num_users > 0);
                           
  R_ASSERT_RETURN_IF_FALSE(data1 < 0xf0);
  R_ASSERT_RETURN_IF_FALSE(data2 < 0x100);
  R_ASSERT_RETURN_IF_FALSE(data3 < 0x100);
  
  R_ASSERT_RETURN_IF_FALSE(data1 >= 0);
  R_ASSERT_RETURN_IF_FALSE(data2 >= 0);
  R_ASSERT_RETURN_IF_FALSE(data3 >= 0);

  if (midi_port->_os_port==NULL)
    return;

  {
    radium::ScopedPlayerThreadPriority player_thread_priority(THREADING_is_main_thread() && midi_port->_is_RT); // prevent priority inversion.
    
    radium::ScopedMutex lock(midi_port->_midi_out_mutex);
    
    HandlePreset(midi_port, data1, data2, data3, LSB, MSB, preset);
    
    MIDI_OS_sendMessage(midi_port->_os_port,data1,data2,data3);
    
    OnOffNotesTrack(midi_port, data1, data2, data3);
  }
}

namespace{
  struct SysexLock : public radium::AbstractMutex{

    const bool _do_boost_priority;
    priority_t _priority_before;
    radium::Mutex &_midi_out_mutex;
    
    SysexLock(radium::MidiOutputPort *midi_port)
      : _do_boost_priority(THREADING_is_main_thread() && midi_port->_is_RT)
      , _midi_out_mutex(midi_port->_midi_out_mutex)
    {
    }
    
    void lock(void) override {
      if (_do_boost_priority) {
        _priority_before = THREADING_get_priority();
        THREADING_acquire_player_thread_priority();
      }

      _midi_out_mutex.lock();
    }
    
    void unlock(void)  override {
      _midi_out_mutex.unlock();
      
      if (_do_boost_priority)
        THREADING_set_priority(_priority_before);
    }
  };
}

void MIDIPORT_send_sysex(
                         radium::MidiOutputPort *midi_port,
                         int len,
                         uint8_t *data)
{
#if !defined(RELEASE)
#if !defined(FOR_MACOSX)
  if (THREADING_has_player_thread_priority())
    abort();
#endif
  
  if (!THREADING_is_main_thread())
    abort();
#endif
  
  R_ASSERT_NON_RELEASE(len >= 2);
  R_ASSERT_NON_RELEASE(data[0]==0xf0);
  R_ASSERT_NON_RELEASE(data[len-1]==0xf7);
                       
  SysexLock lock(midi_port);
  
  MIDI_OS_sendMessage(midi_port->_os_port, len, data, &lock);
}

void MIDIPORT_stop_all_notes(radium::MidiOutputPort *midi_port){
  R_ASSERT_RETURN_IF_FALSE(midi_port->_num_users > 0);
  
  for(int ch=0;ch<16;ch++){
      
    PLAYER_maybe_pause_lock_a_little_bit(ch);

    for(int notenum=0;notenum<128;notenum++) {

      int security = 10000;
    
      while(midi_port->_num_ons[ch][notenum] > 0 && security > 0){
        //printf("%p/%d: Sending stop to note %d\n", patch, ch, notenum);

        MIDIPORT_send_message(midi_port,
                              0x80|ch,
                              notenum,
                              0x00
                              );

        security--;
      }
    }
    
  }
}







/*****************************************************/
/**************** INPUT ******************************/
/*****************************************************/

namespace radium{
  
struct MidiInputPort {
  
  const symbol_t *_port_name;

  Midi_input_callback _callback;
  void *_callback_arg;
    
  MidiInputPort(const symbol_t *port_name, Midi_input_callback callback, void *callback_arg)
    : _port_name(port_name)
    , _callback(callback)
    , _callback_arg(callback_arg)
  {}

  void input_message_has_been_received(const radium::MidiMessage &message) const {
    _callback(_port_name, message, _callback_arg);
  }
};

}

namespace{
struct MidiInputPortOs_Holder{
  int _num_users = 0;
  radium::MidiInputPortOs *_os_port;
};
}

static QHash<const symbol_t*, MidiInputPortOs_Holder> g_midi_input_port_os_holder; // only accessed from main thread. (could use shared_ptr here, but code usually becomes messy when using them)

static radium::Mutex g_input_ports_lock;
static radium::Vector<radium::MidiInputPort*> g_input_ports;

const symbol_t *MIDIPORT_get_port_name(radium::MidiInputPort *port){
  return port->_port_name;
}


// called from juce_midi.cpp
void MIDI_InputMessageHasBeenReceived(const symbol_t *port_name, const radium::MidiMessage &message){
  radium::ScopedMutex lock(g_input_ports_lock);
  
  for(const radium::MidiInputPort *port : g_input_ports){
    //printf("N1: \"%s\" N2: \"%s\". Equal: %d\n", port_name->name, port->_port_name->name, port->_port_name==port_name);
    if (port->_port_name==port_name)
      port->input_message_has_been_received(message);
  }
}

radium::MidiInputPort *MIDIPORT_open_input(const char *name, Midi_input_callback callback, void *callback_arg, bool create_new_if_not_existing){
  R_ASSERT(THREADING_is_main_thread());

  const symbol_t *port_name = MIDI_OS_get_port_name(name);
  
  MidiInputPortOs_Holder &holder = g_midi_input_port_os_holder[port_name];

  if (holder._num_users==0){
    
    R_ASSERT_NON_RELEASE(holder._os_port==NULL);
    holder._os_port = MIDI_OS_create_input_port(port_name, create_new_if_not_existing);
    
    if (holder._os_port==NULL){

      R_ASSERT(create_new_if_not_existing==false);
      
      g_midi_input_port_os_holder.remove(port_name); // Free memory only. Not needed othervice.
      return NULL;

    }
  }

  holder._num_users++;
  
  radium::MidiInputPort *port = new radium::MidiInputPort(port_name, callback, callback_arg);

  g_input_ports.ensure_there_is_room_for_more_without_having_to_allocate_memory();
  
  {
    radium::ScopedPlayerThreadPriority player_thread_priority; // prevent priority inversion.
    radium::ScopedMutex lock(g_input_ports_lock);
    g_input_ports.push_back(port);
  }

  g_input_ports.post_add();
  
  return port;
}


void MIDIPORT_close_input(radium::MidiInputPort *port){
  R_ASSERT_RETURN_IF_FALSE(port != NULL);
  R_ASSERT_RETURN_IF_FALSE(g_input_ports.contains(port));

  MidiInputPortOs_Holder &holder = g_midi_input_port_os_holder[port->_port_name];

  R_ASSERT_RETURN_IF_FALSE(holder._num_users > 0);

  int pos = g_input_ports.find_pos(port);
  R_ASSERT_RETURN_IF_FALSE(pos >= 0);
  
  {
    radium::ScopedPlayerThreadPriority player_thread_priority; // prevent priority inversion.
    radium::ScopedMutex lock(g_input_ports_lock);
    g_input_ports.remove_pos(pos);
  }
    
  holder._num_users--;

  if (holder._num_users==0) {
    MIDI_OS_delete_input_port(holder._os_port);
    g_midi_input_port_os_holder.remove(port->_port_name);
  }

  delete port;
}



/************************************************************************/
/**************** Editor input ports and MIDI learn input ports *********/
/************************************************************************/

// Note: for simplicity, we only use the word "editor" in variable and function names. But these ports are also used for MIDI learn.

static radium::Vector<radium::MidiInputPort*> g_editor_inports;


static void update_editor_ports_config_settings(void){
  SETTINGS_write_int("midi_num_inports", g_editor_inports.size());
  
  for(int i = 0 ; i < g_editor_inports.size() ; i++)
    SETTINGS_write_string(talloc_format("midi_input_port_%d",i), g_editor_inports[i]->_port_name->name);
}

static void editor_midi_input_callback(const symbol_t *port_name, const radium::MidiMessage &message, void *arg){
  if (message._sysex_msg==NULL)
    MIDI_editor_and_midi_learn_InputMessageHasBeenReceived(port_name,
                                                           MIDI_msg_byte1(message._msg),
                                                           MIDI_msg_byte2(message._msg),
                                                           MIDI_msg_byte3(message._msg)
                                                           );
}

bool MIDI_has_editor_input_port(const char *name){
  for(const radium::MidiInputPort *port : g_editor_inports)
    if (!strcmp(port->_port_name->name, name))
      return true;

  return false;
}

static bool add_editor_input_port(const char *name){
  if (MIDI_has_editor_input_port(name)){
    R_ASSERT_NON_RELEASE(false);
    return false;
  }
      
  radium::MidiInputPort *port = MIDIPORT_open_input(name, editor_midi_input_callback, NULL, true);
  
  R_ASSERT_RETURN_IF_FALSE2(port!=NULL, false);
  
  g_editor_inports.push_back(port);

  return true;
}
  
void MIDI_add_editor_input_port(const char *name){
  if (add_editor_input_port(name)){
    update_editor_ports_config_settings();
    PREFERENCES_update();
  }    
}

void MIDI_remove_editor_input_port(const char *name){
  for(radium::MidiInputPort *port : g_editor_inports) {
    
    if (!strcmp(port->_port_name->name, name)) {
      
      MIDIPORT_close_input(port);
      g_editor_inports.remove(port);

      update_editor_ports_config_settings();
      
      return;
    }
    
  }

  R_ASSERT(false);
}

const char **MIDIPORT_get_editor_input_ports(int *retsize){
  
  const char **ret = (const char**)talloc(sizeof(const char*)*g_editor_inports.size());
  int i = 0;
  for(auto *port : g_editor_inports)
    ret[i++] = port->_port_name->name;

  R_ASSERT(i==g_editor_inports.size());
  
  *retsize = i;
  
  return ret;
}



/*****************************************************/
/**************** INIT / SHUTDOWN ********************/
/*****************************************************/

void MIDIPORT_init(void){
  static bool s_has_inited = false;

  if (s_has_inited)
    return;
  else
    s_has_inited = true;

  int num_inports = SETTINGS_read_int("midi_num_inports",0);
  for(int i = 0 ; i < num_inports ; i++){
    const char *name = SETTINGS_read_string(talloc_format("midi_input_port_%d",i),NULL);
    if(name!=NULL){
      add_editor_input_port(name);
    }
  }
}

void MIDIPORT_shut_down(void){
  while(g_input_ports.size() > 0)
    MIDIPORT_close_input(g_input_ports.at_ref(0));
      
  R_ASSERT_NON_RELEASE(g_input_ports.size()==0);

  while(g_output_ports.size() > 0)
    MIDIPORT_close_output(g_output_ports.values().first());

  R_ASSERT_NON_RELEASE(g_output_ports.size()==0);
}
  

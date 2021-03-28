/* Copyright 2021 Kjetil S. Matheussen

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


#include "../common/includepython.h"

#include <inttypes.h>
#include <functional>


#include "../common/nsmtracker.h"
#include "../common/Mutex.hpp"
#include "../common/QueueStack.hpp"

#include "../embedded_scheme/s7extra_proc.h"

#include "../midi/midi_proc.h"
#include "../midi/midi_ports_proc.h"
#include "../midi/OS_midi_proc.h"

#include "api_common_proc.h"


#include "radium_proc.h"
#include "api_midi_proc.h"


static int64_t g_output_port_counter = 0;
static int64_t g_input_port_counter = 0;

static QHash<int64_t, radium::MidiOutputPort*> g_output_ports;

namespace{
  struct InputPort{
    radium::MidiInputPort *_port;
    radium::ProtectedS7Extra<func_t*> _callback = radium::ProtectedS7Extra<func_t*>("midi_input_callback");

    InputPort(radium::MidiInputPort *port, func_t *callback)
      : _port(port)
      , _callback(callback)
    {}
  };
}
static QHash<int64_t, InputPort*> g_input_ports;
static QSet<func_t*> g_deleted_callbacks;

#define UNPROTECT_MESSAGE 0xffffffff

namespace{
typedef struct _midi_event_t{
  func_t *_callback;

  union{
    uint32_t _msg;
    uint32_t _sysex_len;
  };
  RT_Mem<uint8_t> *_sysex_msg;
} midi_event_t;
}

static boost::lockfree::queue<midi_event_t, boost::lockfree::capacity<8000> > *g_midi_event_queue = new boost::lockfree::queue<midi_event_t, boost::lockfree::capacity<8000> >;




/********** OUTPUT *******************/

void sendMidiMessage(int64_t port_id, int byte1, int byte2, int byte3){
  if (byte1 >= 0x100 || byte2 >= 0x7f || byte3 >= 0x7f || byte1 < 0x80 || byte2 < 0 || byte3 < 0 || byte1==0xf0){
    handleError("sendMidiMessage: Illegal MIDI message %x %d %d", byte1, byte2, byte3);
    return;
  }
  
  radium::MidiOutputPort *port = g_output_ports.value(port_id);
  if (port==NULL){

    if (port_id >= 0 && port_id < g_output_port_counter)
      handleError("sendMidiMessage: MIDI output port %d has been closed.", (int)port_id);
    else
      handleError("sendMidiMessage: MIDI output port %d has never been opened.", (int)port_id);

    return;
  }

  MIDIPORT_send_message(port, byte1, byte2, byte3);
}

void sendSysex(int64_t port_id, dynvec_t bytes){
  radium::MidiOutputPort *port = g_output_ports.value(port_id);
  if (port==NULL){

    if (port_id >= 0 && port_id < g_output_port_counter)
      handleError("sendSysex: MIDI output port %d has been closed.", (int)port_id);
    else
      handleError("sendSysex: MIDI output port %d has never been opened.", (int)port_id);

    return;
  }

  if (bytes.num_elements < 2 || bytes.num_elements > 10000000){
    handleError("sendSysex: Illlegal message. Size: %d", bytes.num_elements);
    return;
  }

  uint8_t *data = (uint8_t *)talloc(sizeof(uint8_t)*bytes.num_elements);

  int i = 0;
  
  for(dyn_t dyn : bytes) {

    if (dyn.type != INT_TYPE){
      handleError("sendSysex: Element #%d is not an integer. Found: %s", i, DYN_type_name(dyn.type));
      return;
    }

    int b = dyn.int_number;

    if (i==0) {
      
      if (b != 0xf0) {
        handleError("sendSysex: First byte is not 0xf0. Found: %x", b);
        return;
      }

    } else if (i==bytes.num_elements-1) {

      if (b != 0xff) {
        handleError("sendSysex: Last byte is not 0xf7. Found: %x", b);
        return;
      }

    } else {

      if (b < 0 || b > 0x7f) {
        handleError("sendSysex: Byte #%d has illegal value %x", i, b);
        return;
      }

    }

    data[i] = b;
    
    i++;
  }
  
  MIDIPORT_send_sysex(port, bytes.num_elements, data);
}

// returns -1 on error
int64_t openMidiOutputPort(const_char* portname, bool create_new_if_not_existing){

  radium::MidiOutputPort *port = MIDIPORT_open_output(portname, create_new_if_not_existing, false);
  if (port==NULL){
    R_ASSERT(!create_new_if_not_existing);
    handleError("Unable to open output MIDI port \"%s\"", portname);
    return -1;
  }

  int64_t ret = g_output_port_counter;
  
  g_output_ports[ret] = port;

  g_output_port_counter++;

  return ret;
}

void closeMidiOutputPort(int64_t port_id){
  radium::MidiOutputPort *port = g_output_ports.value(port_id, NULL);
  if (port==NULL){

    if (port_id >= 0 && port_id < g_output_port_counter)
      handleError("closeMidiOutputPort: MIDI output port %d has already been closed.", (int)port_id);
    else
      handleError("closeMidiOutputPort: MIDI output port %d has never exited.", (int)port_id);

    return;
  }

  int n = g_output_ports.remove(port_id);
  R_ASSERT_NON_RELEASE(n==1);
  
  MIDIPORT_close_output(port);
}



/********** INPUT *******************/


static int64_t get_port_id_from_callback(func_t *callback){
  QHashIterator<int64_t, InputPort*> i(g_input_ports);
  while (i.hasNext()) {
    i.next();

    InputPort *port = i.value();
    
    if (port==NULL) {
      
      R_ASSERT(false);
      
    } else if (port->_callback.get()==callback) {
      
      return i.key();
    }
  }

  return -1;
}

void API_MIDI_called_regularly(void){
  midi_event_t event;
  
  while(g_midi_event_queue->pop(event)==true){
    
    func_t *callback = event._callback;

    uint32_t msg = event._msg;

    R_ASSERT_RETURN_IF_FALSE(callback != NULL);

    //printf("Calling callback %d %d %d\n", MIDI_msg_byte1(msg), MIDI_msg_byte2(msg), MIDI_msg_byte3(msg));

    if (msg==UNPROTECT_MESSAGE) {
      
      int n = g_deleted_callbacks.remove(callback);
      R_ASSERT_NON_RELEASE(n==1);
      
    } else if (!g_deleted_callbacks.contains(callback)) {

      bool ret;
      
      if (event._sysex_msg != NULL) {

        //printf("  API: Got sysex. Lenght: %d\n", event._sysex_len);

        dynvec_t bytes = DYNVEC_create(event._sysex_len);
        
        for(int i=0 ; i < (int)event._sysex_len ; i++)
          bytes.elements[i] = DYN_create_int(RT_data(event._sysex_msg)[i]);

        ret = S7CALL(bool_dyn, callback, DYN_create_array(bytes));

        RT_free(event._sysex_msg, "API_MIDI_called_regularly");
        
      } else {

        int len = MIDI_msg_len(msg);
        
        dynvec_t bytes = DYNVEC_create(len);

        bytes.elements[0] = DYN_create_int(MIDI_msg_byte1(msg));

        if (len > 1)
          bytes.elements[1] = DYN_create_int(MIDI_msg_byte2(msg));

        if (len > 2)
          bytes.elements[2] = DYN_create_int(MIDI_msg_byte3(msg));
        
        ret = S7CALL(bool_dyn, callback, DYN_create_array(bytes));
          
      }
      
        if (false==ret) {
          
          int64_t port_id = get_port_id_from_callback(callback);
          
          if (port_id >= 0)
            closeMidiInputPort(port_id, false);
          
        }

    }
    
  }
}

static void midi_input_callback(const symbol_t *port_name, const radium::MidiMessage &message, void *arg){

  //printf("    API: midi_input_callback called. Message.sysex_msg: %p\n", message._sysex_msg);
  
  func_t *callback = static_cast<func_t*>(arg);
  
  R_ASSERT_RETURN_IF_FALSE(callback!=NULL);
  
  midi_event_t event;
  event._callback = callback;

  if (message._sysex_msg != NULL) {
    
    event._sysex_msg = message._sysex_msg;
    RT_inc_ref(event._sysex_msg);

    event._sysex_len = message._sysex_len;
    
  } else {

    event._msg = message._msg;
    event._sysex_msg = NULL;
    
  }
  
  for(int i=0;i<10000;i++){
    
    if (g_midi_event_queue->bounded_push(event))
      return;
    
    msleep(10);
  }

  
  RT_message("MIDI API queue full");
}

// returns -1 on error
int64_t openMidiInputPort(const_char* portname, func_t* callback, bool create_new_if_not_existing){
  
  radium::MidiInputPort *port = MIDIPORT_open_input(portname, midi_input_callback, callback, create_new_if_not_existing);
  if (port==NULL){
    handleError("Unable to open input MIDI port \"%s\"", portname);
    return -1;
  }

  int64_t ret = g_input_port_counter;
  InputPort *input_port = new InputPort(port, callback);
  g_input_ports[ret] = input_port;

  g_input_port_counter++;

  return ret;
}

void closeMidiInputPort(int64_t port_id, bool throw_error_if_already_closed){
  InputPort *input_port = g_input_ports.value(port_id, NULL);
  
  if (input_port==NULL){
    if (throw_error_if_already_closed) {
      if (port_id >= 0 && port_id < g_input_port_counter)
        handleError("closeMidiInputPort: Input MIDI port %d has already been closed.", (int)port_id);
      else
        handleError("closeMidiInputPort: Input MIDI port %d has never existed.", (int)port_id);
    }
    return;
  }

  g_input_ports.remove(port_id);
  
  R_ASSERT_RETURN_IF_FALSE(input_port->_port!=NULL);

  MIDIPORT_close_input(input_port->_port);

  func_t *callback = input_port->_callback.get();
  R_ASSERT_RETURN_IF_FALSE(callback!=NULL);

  g_deleted_callbacks << callback;
                  
  delete input_port;
  
  // Put message on queue to remove the callback from g_deleted_callbacks;
  midi_event_t event;
  event._callback = callback;
  event._msg = UNPROTECT_MESSAGE;

  if (!g_midi_event_queue->bounded_push(event))
    GFX_Message(NULL, "MIDI API queue full"); // We don't bother trying again. It's just a memory leak, and this situation will probably never happen.
}

void closeAllMidiInputPorts(void){
  while(g_input_ports.size() > 0)
    closeMidiInputPort(g_input_ports.keys().at(0), false);
}


// returns an array of strings.
dynvec_t getMidiInputPortNames(void){
  dynvec_t ret = {};

  int size;
  const char **elements = MIDI_OS_getInputPortNames(&size);

  for(int i=0;i<size;i++)
    DYNVEC_push_back(&ret, DYN_create_string(elements[i]));
  
  return ret;
}

// returns an array of strings.
dynvec_t getMidiOutputPortNames(void){
  dynvec_t ret = {};

  int size;
  const char **elements = MIDI_OS_getOutputPortNames(&size);

  for(int i=0;i<size;i++)
    DYNVEC_push_back(&ret, DYN_create_string(elements[i]));
  
  return ret;
}


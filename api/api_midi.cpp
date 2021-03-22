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
  uint32_t _msg;
} midi_event_t;
}

static boost::lockfree::queue<midi_event_t, boost::lockfree::capacity<8000> > *g_midi_event_queue = new boost::lockfree::queue<midi_event_t, boost::lockfree::capacity<8000> >;




/********** OUTPUT *******************/

void sendMidiMessage(int64_t port_id, int byte1, int byte2, int byte3){
  if (byte1 >= 0x100 || byte2 >= 0x100 || byte3 >= 0x100){
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

// returns -1 on error
int64_t openMidiOutputPort(const_char* portname, bool create_new_if_not_existing){
  // FIX: Check if port is open already.

  radium::MidiOutputPort *port = MIDIPORT_open_output(portname, create_new_if_not_existing, false);
  if (port==NULL){
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

    printf("Calling callback %d %d %d\n", MIDI_msg_byte1(msg), MIDI_msg_byte2(msg), MIDI_msg_byte3(msg));

    if (msg==UNPROTECT_MESSAGE) {
      
      int n = g_deleted_callbacks.remove(callback);
      R_ASSERT_NON_RELEASE(n==1);
      
    } else if (!g_deleted_callbacks.contains(callback)) {
      
      if (false==S7CALL(bool_int_int_int, callback, MIDI_msg_byte1(msg), MIDI_msg_byte2(msg), MIDI_msg_byte3(msg))) {
        
        int64_t port_id = get_port_id_from_callback(callback);
        
        if (port_id >= 0)
          closeMidiInputPort(port_id, false);
        
      }
      
    }
    
  }
}

static void midi_input_callback(const symbol_t *port_name, int cc, int data1, int data2, void *arg){

  func_t *callback = static_cast<func_t*>(arg);
  
  R_ASSERT_RETURN_IF_FALSE(callback!=NULL);
  
  midi_event_t event;
  event._callback = callback;
  event._msg = MIDI_msg_pack3(cc, data1, data2);

  if (!g_midi_event_queue->bounded_push(event))
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


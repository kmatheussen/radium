
#pragma once

#include "midi_proc.h"
#include "../common/RT_memory_allocator_proc.h"


namespace radium{
  struct MidiInputPort;
  struct MidiOutputPort;

  struct MidiMessage{
        
    union{
      uint32_t _msg;
      uint32_t _sysex_len;
    };
    
    RT_Mem<uint8_t> *_sysex_msg;

    ~MidiMessage(){
      RT_free(_sysex_msg, "~MidiMessage");
    }
    
    MidiMessage(){
    }
    
    MidiMessage(const MidiMessage &m)
    {
      if (m._sysex_msg != NULL) {
        
        _sysex_len = m._sysex_len;
        _sysex_msg = m._sysex_msg;
        RT_inc_ref(_sysex_msg);
        
      } else {
        
        _msg = m._msg;
        _sysex_msg = NULL;
        
      }      
    }

    MidiMessage(int byte1, int byte2, int byte3)
      : _msg(MIDI_msg_pack3(byte1, byte2, byte3))
      , _sysex_msg(NULL)
    {
#if !defined(RELEASE)
      if (byte1 >= 0x100 || byte2 > 0x7f || byte3 > 0x7f || byte1 < 0x80 || byte2 < 0 || byte3 < 0 || byte1==0xf0){
        fprintf(stderr, "%x %x %x\n", byte1, byte2, byte3);
        abort();
      }
#endif
    }

    MidiMessage(RT_Mem<uint8_t> *sysex_msg, uint32_t sysex_len)
      : _sysex_len(sysex_len)
      , _sysex_msg(sysex_msg)
    {

      RT_inc_ref(_sysex_msg);
      
#if !defined(RELEASE)
      if (sysex_len < 2)
        abort();
      if (RT_data<uint8_t>(sysex_msg)[0] != 0xf0)
        abort();
      if (RT_data<uint8_t>(sysex_msg)[sysex_len-1] != 0xf7)
        abort();
#endif
    }
  };
}

typedef void (*Midi_input_callback) (const symbol_t *port_name, const radium::MidiMessage &message, void *arg);

extern const symbol_t *MIDIPORT_get_port_name(radium::MidiInputPort *port);
extern const symbol_t *MIDIPORT_get_port_name(radium::MidiOutputPort *port);

extern void MIDI_InputMessageHasBeenReceived(const symbol_t *port_name, const radium::MidiMessage &message);

extern radium::MidiInputPort *MIDIPORT_open_input(const char *name, Midi_input_callback callback, void *callback_arg, bool create_new_if_not_existing);


// 'may_be_used_in_realtime_code' must be set to true to avoid priority inversion if 'MIDIPORT_send_message' may be called from a realtime thread.
//
// Beware: Current implentation assumes that MIDIPORT_send_message is only called from the main thread or from a realtime thread.
// Priority inversion may happen if sent from a different non-RT thread than the main thread.
//
extern radium::MidiOutputPort *MIDIPORT_open_output(const char *name, bool create_new_if_not_existing, bool may_be_used_in_realtime_code);

extern void MIDIPORT_close_input(radium::MidiInputPort *midiport);
extern void MIDIPORT_close_output(radium::MidiOutputPort *midiport);

extern const char **MIDIPORT_get_connected_input_ports(int *retsize);

extern void MIDIPORT_init(void);
void MIDIPORT_shut_down(void);

 // Beware: Current implentation assumes that MIDIPORT_send_message is only sent from the main thread if not sent from a realtime thread. Priority inversion may happen if sent from a different non-RT thread.
extern void MIDIPORT_send_message(
                                  radium::MidiOutputPort *midi_port,
                                  const int data1,
                                  const int data2,
                                  const int data3,
                                  const int LSB = -1,
                                  const int MSB = -1,
                                  const int preset = -1);


// 'data' must start with 0xf0 and end with 0xf7. 'len' must also include these two bytes.
extern void MIDIPORT_send_sysex(
                                radium::MidiOutputPort *midi_port,
                                int len,
                                uint8_t *data);

void MIDIPORT_stop_all_notes(radium::MidiOutputPort *midi_port);

// called from juce_midi.cpp
void MIDI_InputMessageHasBeenReceived(const symbol_t *port_name, const radium::MidiMessage &message);

// Editor/midi learn ports
bool MIDI_has_editor_input_port(const char *name);
void MIDI_add_editor_input_port(const char *name);
void MIDI_remove_editor_input_port(const char *name);
const char **MIDIPORT_get_editor_input_ports(int *retsize);

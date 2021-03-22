
namespace radium{
  struct MidiInputPort;
  struct MidiOutputPort;
}

typedef void (*Midi_input_callback) (const symbol_t *port_name, int cc, int data1, int data2, void *arg);

extern const symbol_t *MIDIPORT_get_port_name(radium::MidiInputPort *port);
extern const symbol_t *MIDIPORT_get_port_name(radium::MidiOutputPort *port);

extern void MIDI_InputMessageHasBeenReceived(symbol_t *port_name, int cc, int data1, int data2);

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

void MIDIPORT_stop_all_notes(radium::MidiOutputPort *midi_port);

// called from juce_midi.cpp
void MIDI_InputMessageHasBeenReceived(const symbol_t *port_name, int cc,int data1,int data2);

// Editor/midi learn ports
bool MIDI_has_editor_input_port(const char *name);
void MIDI_add_editor_input_port(const char *name);
void MIDI_remove_editor_input_port(const char *name);
const char **MIDIPORT_get_editor_input_ports(int *retsize);

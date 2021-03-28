/* Copyright 2003 Kjetil S. Matheussen

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


#ifndef OS_MIDI_PROC_H
#define OS_MIDI_PROC_H

extern LANGSPEC void MIDI_OS_InitTiming(void);

#ifdef __cplusplus

namespace radium{
  struct MidiOutputPortOs;
  struct MidiInputPortOs;
  struct AbstractMutex;
}


const symbol_t *MIDI_OS_get_port_name(const char *name_c);

extern const char **MIDI_OS_getOutputPortNames(int *retsize); // returns all ports that's possible to connect to (whether we are connected or not)
extern const char **MIDI_OS_getInputPortNames(int *retsize); // returns all ports that's possible to connect to (whether we are connected or not)

extern const char *MIDI_OS_getDefaultOutputPort(void);


extern void MIDI_OS_sendMessage(radium::MidiOutputPortOs *port,
                                int cc,
                                int data1,
                                int data2
                                );

// This function is not realtime safe. Supply a mutex if the underlying port needs to be protected from simultaneous access.
extern void MIDI_OS_sendMessage(radium::MidiOutputPortOs *port,
                                int len,
                                uint8_t *data,
                                radium::AbstractMutex *mutex = NULL);

extern void MIDI_OS_PlayFromStart(radium::MidiOutputPortOs *port);

extern radium::MidiOutputPortOs *MIDI_OS_create_output_port(const symbol_t *port_name, bool create_new_if_not_existing);
extern radium::MidiInputPortOs *MIDI_OS_create_input_port(const symbol_t *port_name, bool create_new_if_not_existing);

extern void MIDI_OS_delete_input_port(radium::MidiInputPortOs *port);
extern void MIDI_OS_delete_output_port(radium::MidiOutputPortOs *port);

#endif


#endif

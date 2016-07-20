/* Copyright 2000 Kjetil S. Matheussen


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

#ifndef _RADIUM_MIDI_MIDI_I_INPUT_PROC_H
#define _RADIUM_MIDI_MIDI_I_INPUT_PROC_H


extern LANGSPEC void MIDI_insert_recorded_midi_events(void);

extern LANGSPEC void RT_MIDI_handle_play_buffer(void);

extern LANGSPEC bool MIDI_get_record_accurately(void);
extern LANGSPEC void MIDI_set_record_accurately(bool accurately);
extern LANGSPEC bool MIDI_get_record_velocity(void);
extern LANGSPEC void MIDI_set_record_velocity(bool doit);

extern LANGSPEC void MIDI_InputMessageHasBeenReceived(const char *port_name, int cc,int data1,int data2);

extern LANGSPEC void MIDI_SetThroughPatch(struct Patch *patch);

extern LANGSPEC void MIDI_HandleInputMessage(void);

extern LANGSPEC void MIDI_input_init(void);

struct MidiLearn;
extern LANGSPEC void MIDI_add_midi_learn(struct MidiLearn *midi_learn);
extern LANGSPEC void MIDI_remove_midi_learn(struct MidiLearn *midi_learn, bool show_error_if_not_here);
    
#ifdef USE_QT4

#include <QString>

#include "midi_proc.h"

struct MidiLearn{

  DEFINE_ATOMIC(bool, is_enabled);

private:
  
  DEFINE_ATOMIC(bool, is_learning);
  DEFINE_ATOMIC(const char*, port_name);
  DEFINE_ATOMIC(int, data1);
  DEFINE_ATOMIC(int, data2);
  
public:

  MidiLearn(const MidiLearn&) = delete;
  MidiLearn& operator=(const MidiLearn&) = delete;

  MidiLearn()
  {
    ATOMIC_SET(is_enabled, true);
    
    ATOMIC_SET(is_learning, true);
    ATOMIC_SET(port_name, NULL);
    ATOMIC_SET(data1, 0);
    ATOMIC_SET(data2, 0);
  }

  virtual ~MidiLearn() = default; // Crazy c++ stuff. https://www.securecoding.cert.org/confluence/display/cplusplus/OOP52-CPP.+Do+not+delete+a+polymorphic+object+without+a+virtual+destructor

  hash_t *create_state(void);
  void init_from_state(hash_t *hash);
  
  bool RT_maybe_use(const char *port_name, uint32_t msg);

  QString get_source_info(void){
    if (ATOMIC_GET(is_learning))
      return talloc_format("Learning...", ATOMIC_GET(data1), ATOMIC_GET(data2));
    else if (ATOMIC_GET(data1)>=0xe0)
      return talloc_format("%s: %2X", ATOMIC_GET(port_name), ATOMIC_GET(data1));
    else
      return talloc_format("%s: %2X / %2X", ATOMIC_GET(port_name), ATOMIC_GET(data1), ATOMIC_GET(data2));
  }

  virtual QString get_dest_info(void){
    return QString("MidiLearn::get_dest_info. Error: This method is supposed to be overridden.");
  }
  
  virtual void delete_me(void){
    printf("MidiLearn::delete_me. Error: This method is supposed to be overridden.\n");
  }
  
  virtual void RT_callback(float val){
    printf("MidiLearn::callback got %f. Error: This method is supposed to be overridden.\n", val);
  }
};

#endif


#endif

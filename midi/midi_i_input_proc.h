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

extern DEFINE_ATOMIC(struct Patch *, g_through_patch);

struct SoundPlugin;

extern LANGSPEC void MIDI_add_automation_recording_event(struct SoundPlugin *plugin, int effect_num, float value);

extern LANGSPEC bool MIDI_insert_recorded_midi_events(void);
extern LANGSPEC bool MIDI_insert_recorded_midi_gfx_events(void);

extern LANGSPEC void RT_MIDI_handle_play_buffer(void);

extern LANGSPEC bool MIDI_get_record_accurately(void);
extern LANGSPEC void MIDI_set_record_accurately(bool accurately);
extern LANGSPEC bool MIDI_get_record_velocity(void);
extern LANGSPEC void MIDI_set_record_velocity(bool doit);

extern LANGSPEC void MIDI_editor_and_midi_learn_InputMessageHasBeenReceived(const symbol_t *port_name, int cc,int data1,int data2);

extern LANGSPEC void MIDI_SetThroughPatch(struct Patch *patch);
extern LANGSPEC struct Patch *MIDI_GetThroughPatch(void);

extern LANGSPEC void MIDI_HandleInputMessage(void);

extern LANGSPEC void MIDI_input_init(void);

#ifdef __cplusplus
namespace radium{
  struct MidiLearn;
}

extern void MIDI_add_midi_learn(radium::MidiLearn *midi_learn);
extern void MIDI_remove_midi_learn(radium::MidiLearn *midi_learn, bool show_error_if_not_here);

#endif

#ifdef USE_QT4

#include <QString>

#include "midi_proc.h"

namespace radium{
  
struct MidiLearn{

  DEFINE_ATOMIC(bool, is_enabled);
  DEFINE_ATOMIC(bool, is_incremental);

private:
  
  DEFINE_ATOMIC(bool, is_learning);
  DEFINE_ATOMIC(const symbol_t*, port_name);
  DEFINE_ATOMIC(int, byte1);
  DEFINE_ATOMIC(int, byte2);

  DEFINE_ATOMIC(int, last_value); // used in incremental mode
  
public:

  MidiLearn(const MidiLearn&) = delete;
  MidiLearn& operator=(const MidiLearn&) = delete;

  MidiLearn()
  {
    ATOMIC_SET(is_enabled, true);
    ATOMIC_SET(is_learning, true);
    ATOMIC_SET(is_incremental, false);

    ATOMIC_SET(port_name, NULL);
    ATOMIC_SET(byte1, 0);
    ATOMIC_SET(byte2, 0);

    ATOMIC_SET(last_value, -1);
  }

  virtual ~MidiLearn() = default; // Crazy c++ stuff. https://www.securecoding.cert.org/confluence/display/cplusplus/OOP52-CPP.+Do+not+delete+a+polymorphic+object+without+a+virtual+destructor

  virtual hash_t *create_state(void);
  virtual void init_from_state(hash_t *hash);

  
  bool RT_matching(const symbol_t *port_name, uint32_t msg);

  // Only call once per message!
  int RT_get_value_for_incremental_mode(uint32_t msg);
  
private:

  bool RT_maybe_use(const symbol_t *port_name, uint32_t msg);

public:
  
  virtual bool RT_get_automation_recording_data(struct SoundPlugin **plugin, int *effect_num) = 0;

  QString get_source_info(void){
    if (ATOMIC_GET(is_learning))
      return "Learning...";
    else {

      const symbol_t *portname = ATOMIC_GET(port_name);
      R_ASSERT_RETURN_IF_FALSE2(portname!=NULL, "(error)");
                                
      if (ATOMIC_GET(byte1)>=0xe0)
        return talloc_format("%s: %2X", portname->name, ATOMIC_GET(byte1));
      else
        return talloc_format("%s: %2X / %2X", portname->name, ATOMIC_GET(byte1), ATOMIC_GET(byte2));

    }
  }

  // may be overridden. Returns -2 if no instrument should match, or -1 if all instruments should match.
  virtual instrument_t RT_get_instrument_id(void){
    return make_instrument(-1);
  }
  
  virtual QString get_dest_info(void) = 0;
  
  virtual void delete_me(void) = 0;
  
  virtual void RT_callback(float val) = 0;

  static void RT_maybe_use_forall(instrument_t instrument_id, const symbol_t *port_name, uint32_t msg);
};
}

#endif


#endif

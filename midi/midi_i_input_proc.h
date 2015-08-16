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



extern LANGSPEC void MIDI_insert_recorded_midi_events(void);

extern LANGSPEC void RT_MIDI_handle_play_buffer(void);

extern LANGSPEC bool MIDI_get_record_accurately(void);
extern LANGSPEC void MIDI_set_record_accurately(bool accurately);
extern LANGSPEC bool MIDI_get_record_velocity(void);
extern LANGSPEC void MIDI_set_record_velocity(bool doit);

extern LANGSPEC void MIDI_InputMessageHasBeenReceived(int cc,int data1,int data2);

extern LANGSPEC void MIDI_SetThroughPatch(struct Patch *patch);

extern LANGSPEC void MIDI_HandleInputMessage(void);

extern LANGSPEC void MIDI_input_init(void);

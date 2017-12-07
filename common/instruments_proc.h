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


extern LANGSPEC bool OpenInstruments(void);
extern LANGSPEC void CloseAllInstruments(void);
extern LANGSPEC void StopAllInstruments(void);
extern LANGSPEC void RT_StopAllInstruments(void);
extern LANGSPEC void InitAllInstrumentsWhenPlayingSong(int64_t abstime);

extern LANGSPEC struct Instruments *get_all_instruments(void);
extern LANGSPEC struct Instruments *get_default_instrument(void);
extern LANGSPEC struct Instruments *get_MIDI_instrument(void);
extern LANGSPEC struct Instruments *get_audio_instrument(void);

extern LANGSPEC struct Instruments *get_instrument_from_type(int type);
extern LANGSPEC int get_type_from_instrument(struct Instruments *instrument);

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

#ifndef _RADIUM_COMMON_BLOCKS_PROC_H
#define _RADIUM_COMMON_BLOCKS_PROC_H

extern LANGSPEC void CloseBlock(NInt blocknum);
extern LANGSPEC void CloseAllBlocks(void);
extern LANGSPEC void NewBlock(
	struct Blocks *block,
	NInt num_tracks,
	int num_lines,
	const char *name
);
extern LANGSPEC struct Blocks *AppendBlock(void);
extern LANGSPEC void AppendBlock_spes(int num_lines,NInt num_tracks);

extern LANGSPEC void BLOCKS_add_tempo_multiplier_midi_learn(void);
extern LANGSPEC void BLOCKS_remove_tempo_multiplier_midi_learn(void);
extern LANGSPEC bool BLOCKS_has_tempo_multiplier_midi_learn(void);
extern LANGSPEC void BLOCKS_called_very_often(void);
extern LANGSPEC hash_t *BLOCKS_get_state(void);
extern LANGSPEC void BLOCKS_create_from_state(hash_t *state);

#endif


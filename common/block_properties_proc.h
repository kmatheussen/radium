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



#ifndef _RADIUM_COMMON_BLOCK_PROPERTIES_PROC_H
#define _RADIUM_COMMON_BLOCK_PROPERTIES_PROC_H

extern LANGSPEC void Block_Set_num_lines(
	struct Blocks *block,
	int num_lines
);

#ifdef __cplusplus
#include "player_pause_proc.h"
void Block_Set_num_lines2(
                          struct Blocks *block,
                          int num_lines,
                          radium::PlayerPauseOnlyIfNeeded &player_pause
                          );
#endif
  
extern LANGSPEC void Block_Set_num_tracks(
	struct Blocks *block,
	NInt num_tracks
);

extern LANGSPEC void Block_set_name(struct Blocks *block, const char *new_name);

extern LANGSPEC void Block_Properties(
	struct Blocks *block,
	NInt num_tracks,
	int num_lines
);

extern LANGSPEC void Block_Properties_CurrPos(
	struct Tracker_Windows *window
);

#endif



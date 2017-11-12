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


#ifndef TRACKER_INCLUDE

#include "nsmtracker.h"

extern void CloseBlock(NInt blocknum);
extern void CloseAllBlocks(void);
extern void NewBlock(
	struct Blocks *block,
	NInt num_tracks,
	int num_lines,
	const char *name
);
extern struct Blocks *AppendBlock(void);
extern void AppendBlock_spes(int num_lines,NInt num_tracks);

#endif


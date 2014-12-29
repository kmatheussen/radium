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


/* This version is hopefully faster than linking all object-files.
   At least, it should be faster for cpu's with small/no cache.
   But it allso depends on the inline settings.
*/


#define TRACKER_INCLUDE 1
#define VERSION 0.1

#include "debug.c"
#include "placement.c"
#include "list.c"
#include "tbox.c"
#include "area.c"
#include "memory.c"
#include "time.c"
#include "Amiga_bs.c"
#include "blocklist.c"
#include "Amiga_bs_edit.c"
#include "instruments.c"					//
#include "gfx_subtrack.c"
#include "realline_calc.c"
#include "nodelines.c"						//
#include "Amiga_readstr.c"
#include "GFX_Amiga_egc.c"
#include "plug-ins/camd_i_plugin.c"		//
#include "sliders.c"
#include "common.c"
#include "subtrack.c"
#include "cursor.c"
#include "resizewindow.c"
#include "gfx_wtext.c"
#include "gfx_wtrackheaders.c"
#include "gfx_wtracks.c"
#include "gfx_wblocks.c"
#include "temponodes.c"
#include "mouse_temponodeborder.c"
#include "localzooms.c"
#include "tempos.c"
#include "LPB.c"
#include "fxlines.c"
#include "reallines.c"
#include "notestext.c"
#include "notes.c"
#include "velocities.c"
#include "trackreallines.c"
#include "mouse_fxnode.c"
#include "mouse_vellinenode.c"
#include "mouse_vellinestart.c"
#include "mouse_vellineend.c"
#include "mouse_fxarea.c"
#include "mouse_wtrackborder.c"
#include "mouse_wtrack.c"
#include "mouse_temponodes.c"
#include "tracks.c"
#include "blocks.c"
#include "wtracks.c"
#include "wblocks.c"
#include "Amiga_Ptask2Mtask.c"
#include "Ptask2Mtask.c"
#include "PEQmempool.c"
#include "PEQrealline.c"
#include "Amiga_player.c"
#include "player.c"
#include "windows.c"
#include "song.c"
#include "mouse.c"
#include "eventreciever.c"
#include "main.c"



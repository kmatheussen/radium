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


#include <stdlib.h>

#include "nsmtracker.h"
#include "playerclass.h"
#include "windows_proc.h"
#include "blocks_proc.h"
#include "instruments_proc.h"
#include "trackreallines_proc.h"
#include "blocklist_proc.h"

#include "song_proc.h"



extern struct Root *root;
extern PlayerClass *pc;

void ClearSong(void){
	CloseAllTrackerWindows();
}

bool NewSong(void){

	root->tempo=128;
	root->lpb=4;
	pc->initplaying=false;
	pc->isplaying=false;

	AppendBlock();

	BL_init();


	if(OpenTrackerWindow(20,20,00,00)==-1) return false;


	return true;
}



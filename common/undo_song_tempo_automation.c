/* Copyright 2016 Kjetil S. Matheussen

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



#include "nsmtracker.h"
#include "undo.h"

#include "song_tempo_automation_proc.h"

#include "undo_song_tempo_automation_proc.h"


static void *Undo_Do_SongTempoAutomation(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
);


void ADD_UNDO_FUNC(SongTempoAutomation(void)){
  struct Tracker_Windows *window = root->song->tracker_windows;

  Undo_Add(
           window->l.num,
           window->wblock->l.num,
           window->curr_track,
           window->wblock->curr_realline,
           TEMPOAUTOMATION_get_state(),
           Undo_Do_SongTempoAutomation,
           "Song Tempo Automation"
           );
}

static void *Undo_Do_SongTempoAutomation(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
){
  hash_t *ret = TEMPOAUTOMATION_get_state();

  TEMPOAUTOMATION_create_from_state(pointer, -1);

  return ret;
}

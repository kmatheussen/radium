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

#include "seqtrack_proc.h"

#include "undo_sequencer_proc.h"



/////////////////////////////////////////////////////////////////////
// 1. Everything.                                                  //
/////////////////////////////////////////////////////////////////////

static void *Undo_Do_Sequencer(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
);


void ADD_UNDO_FUNC(Sequencer(void)){
  struct Tracker_Windows *window = root->song->tracker_windows;

  Undo_Add(
           window->l.num,
           window->wblock->l.num,
           window->curr_track,
           window->wblock->curr_realline,
           SEQUENCER_get_state(false),
           Undo_Do_Sequencer,
           "Sequencer"
           );
}

static void *Undo_Do_Sequencer(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
){
  hash_t *ret = SEQUENCER_get_state(false);

  SEQUENCER_create_from_state(pointer, root->song);

  return ret;
}




/////////////////////////////////////////////////////////////////////////
// 2. Just automation. (doesn't require player to pause when modified) //
/////////////////////////////////////////////////////////////////////////

static void *Undo_Do_SeqAutomations(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
);


void ADD_UNDO_FUNC(SeqAutomations(void)){
  struct Tracker_Windows *window = root->song->tracker_windows;

  Undo_Add_dont_stop_playing(
                             window->l.num,
                             window->wblock->l.num,
                             window->curr_track,
                             window->wblock->curr_realline,
                             SEQUENCER_get_automations_state(),
                             Undo_Do_SeqAutomations,
                             "SeqAutomations"
                             );
}

static void *Undo_Do_SeqAutomations(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
){
  hash_t *ret = SEQUENCER_get_automations_state();

  SEQUENCER_create_automations_from_state(pointer);

  return ret;
}


/////////////////////////////////////////////////////////////////////////////
// 3. Just block envelope. (doesn't require player to pause when modified) //
/////////////////////////////////////////////////////////////////////////////

static void *Undo_Do_SeqEnvelopes(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
);

void ADD_UNDO_FUNC(SeqEnvelopes(void)){
  struct Tracker_Windows *window = root->song->tracker_windows;

  Undo_Add_dont_stop_playing(
                             window->l.num,
                             window->wblock->l.num,
                             window->curr_track,
                             window->wblock->curr_realline,
                             SEQUENCER_get_envelopes_state(),
                             Undo_Do_SeqEnvelopes,
                             "SeqEnvelopes"
                             );
}

static void *Undo_Do_SeqEnvelopes(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
){
  hash_t *ret = SEQUENCER_get_envelopes_state();

  SEQUENCER_create_envelopes_from_state(pointer);

  return ret;
}


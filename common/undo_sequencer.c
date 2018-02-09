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
           SEQUENCER_get_state(),
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
  hash_t *ret = SEQUENCER_get_state();

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



/////////////////////////////////////////////////////////////////////////////////
// 4. Just block fade in/out. (doesn't require player to pause when modified) //
///////////////////////////////////////////////////////////////////////////////

static void *Undo_Do_SeqblockFades(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
);

struct SeqFades{
  int seqtracknum;
  int seqblocknum;
  double fadein;
  double fadeout;
};

static struct SeqBlock *get_seqblock(int seqtracknum, int seqblocknum){
  R_ASSERT_RETURN_IF_FALSE2(seqtracknum>=0, NULL);
  R_ASSERT_RETURN_IF_FALSE2(seqtracknum<root->song->seqtracks.num_elements, NULL);
  
  struct SeqTrack *seqtrack = root->song->seqtracks.elements[seqtracknum];
  
  R_ASSERT_RETURN_IF_FALSE2(seqblocknum>=0, NULL);
  R_ASSERT_RETURN_IF_FALSE2(seqblocknum<seqtrack->seqblocks.num_elements, NULL);

  return seqtrack->seqblocks.elements[seqblocknum];
}

static struct SeqFades *get_seqfades(int seqtracknum, int seqblocknum){
  struct SeqBlock *seqblock = get_seqblock(seqtracknum, seqblocknum);
  if (seqblock==NULL)
    return NULL;
  
  struct SeqFades *seq_fades = talloc(sizeof(struct SeqFades));

  seq_fades->seqtracknum = seqtracknum;
  seq_fades->seqblocknum = seqblocknum;

  seq_fades->fadein = seqblock->fadein;
  seq_fades->fadeout = seqblock->fadeout;
  
  return seq_fades;
}

void ADD_UNDO_FUNC(SeqblockFades(int seqtracknum, int seqblocknum)){
  struct Tracker_Windows *window = root->song->tracker_windows;
  
  struct SeqFades *seq_fades = get_seqfades(seqtracknum, seqblocknum);
  if (seq_fades==NULL)
    return;
  
  Undo_Add_dont_stop_playing(
                             window->l.num,
                             window->wblock->l.num,
                             window->curr_track,
                             window->wblock->curr_realline,
                             seq_fades,
                             Undo_Do_SeqblockFades,
                             "SeqEnvelopes"
                             );
}

static void *Undo_Do_SeqblockFades(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
){
  struct SeqFades *seq_fades = pointer;
  int seqtracknum = seq_fades->seqtracknum;
  int seqblocknum = seq_fades->seqblocknum;

  struct SeqFades *ret = get_seqfades(seqtracknum, seqblocknum);
  if (ret==NULL)
    return seq_fades; // Something is seriously wrong (assertion window was shown above), but I'm not sure if we can return NULL from this function so we return seq_fades instead.

  struct SeqBlock *seqblock = get_seqblock(seqtracknum, seqblocknum);
  
  bool needlock = is_playing_song();
  
  if(needlock) PLAYER_lock();
  {
    seqblock->fadein = seq_fades->fadein;
    seqblock->fadeout = seq_fades->fadeout;
  }
  if(needlock) PLAYER_unlock();

  return ret;
}


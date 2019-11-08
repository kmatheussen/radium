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

#include "sequencer_proc.h"

#include "undo_sequencer_proc.h"

static struct SeqTrack *get_seqtrack(int seqtracknum){
  R_ASSERT_RETURN_IF_FALSE2(seqtracknum>=0, NULL);
  R_ASSERT_RETURN_IF_FALSE2(seqtracknum<root->song->seqtracks.num_elements, NULL);
  
  return root->song->seqtracks.elements[seqtracknum];
}

static struct SeqBlock *get_seqblock(int seqtracknum, int seqblocknum, struct SeqTrack **seqtrack_to){
  struct SeqTrack *seqtrack = get_seqtrack(seqtracknum);
  if (seqtrack==NULL)
    return NULL;

  if (seqtrack_to != NULL)
    *seqtrack_to = seqtrack;
  
  R_ASSERT_RETURN_IF_FALSE2(seqblocknum>=0, NULL);
  R_ASSERT_RETURN_IF_FALSE2(seqblocknum<seqtrack->seqblocks.num_elements, NULL);

  return seqtrack->seqblocks.elements[seqblocknum];
}



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




/////////////////////////////////////////////////////////////////////
// 1.5. A seqblock                                                 //
/////////////////////////////////////////////////////////////////////

static void *Undo_Do_Seqblock(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
);


static hash_t *get_seqblock_state(int seqtracknum, int seqblocknum){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = get_seqblock(seqtracknum, seqblocknum, &seqtrack);
  if (seqblock==NULL)
    return NULL;
  
  hash_t *state = SEQBLOCK_get_state(seqtrack, seqblock, true);
  
  HASH_put_int(state, ":seqtracknum", seqtracknum);
  HASH_put_int(state, ":seqblocknum", seqblocknum);
  
  return state;
}

void ADD_UNDO_FUNC(Seqblock(int seqtracknum, int seqblocknum)){
  struct Tracker_Windows *window = root->song->tracker_windows;

  hash_t *state = get_seqblock_state(seqtracknum, seqblocknum);
  if (state==NULL)
    return;
  
  Undo_Add(
           window->l.num,
           window->wblock->l.num,
           window->curr_track,
           window->wblock->curr_realline,
           state, 
           Undo_Do_Seqblock,
           "Seqblock"
           );
}

static void *Undo_Do_Seqblock(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
){
  hash_t *state = (hash_t*)pointer;

  int seqtracknum = HASH_get_int32(state, ":seqtracknum");
  int seqblocknum = HASH_get_int32(state, ":seqblocknum");

  hash_t *ret = get_seqblock_state(seqtracknum, seqblocknum);
  if (ret==NULL) // not supposed to happen.
    return state;
  
  SEQBLOCK_replace_seqblock(state, true, SHOW_ASSERTION);
    
  return ret;
}




/////////////////////////////////////////////////////////////////////////
// 2. Just automation. (doesn't require player to pause when modified) //
/////////////////////////////////////////////////////////////////////////

static void *Undo_Do_SeqtrackAutomations(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
);


void ADD_UNDO_FUNC(SeqtrackAutomations(void)){
  struct Tracker_Windows *window = root->song->tracker_windows;

  Undo_Add_dont_stop_playing(
                             window->l.num,
                             window->wblock->l.num,
                             window->curr_track,
                             window->wblock->curr_realline,
                             SEQUENCER_get_automations_state(),
                             Undo_Do_SeqtrackAutomations,
                             "SeqtrackAutomations"
                             );
}

static void *Undo_Do_SeqtrackAutomations(
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

static void *Undo_Do_SeqSeqblockAutomation(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
);

void ADD_UNDO_FUNC(SeqblockAutomation(int automationnum, int seqblocknum, int seqtracknum)){
  struct Tracker_Windows *window = root->song->tracker_windows;

  Undo_Add_dont_stop_playing(
                             window->l.num,
                             window->wblock->l.num,
                             window->curr_track,
                             window->wblock->curr_realline,
                             SEQUENCER_get_seqblock_automation_state(automationnum, seqblocknum, seqtracknum),
                             Undo_Do_SeqSeqblockAutomation,
                             "SeqSeqblockAutomation"
                             );
}

static void *Undo_Do_SeqSeqblockAutomation(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
){
  return SEQUENCER_create_seqblock_automation_from_state(pointer, true);
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

static struct SeqFades *get_seqfades(int seqtracknum, int seqblocknum){
  struct SeqBlock *seqblock = get_seqblock(seqtracknum, seqblocknum, NULL);
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
                             "SeqSeqblockFades"
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

  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = get_seqblock(seqtracknum, seqblocknum, &seqtrack);
  if (seqblock==NULL)
    return seq_fades;

  bool needlock = is_playing_song(); // TODO: Check if this is ok. RT_set_seqblock_curr_gain is called SCHEDULER_called_per_block, but SCHEDULER_called_per_block is always called. Not very important we hold the lock though.
  
  if(needlock) PLAYER_lock();
  {
    seqblock->fadein = seq_fades->fadein;
    seqblock->fadeout = seq_fades->fadeout;
  }
  if(needlock) PLAYER_unlock();

  SEQBLOCK_update(seqtrack, seqblock);
  
  return ret;
}



///////////////////////////////////////////////////////////////////////////////////////////
// 5. Just seqtrack note gain and mute. (doesn't require player to pause when modified) //
/////////////////////////////////////////////////////////////////////////////////////////

static void *Undo_Do_EditorSeqtrackVolume(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
);

struct EditorSeqtrackVolume{
  int seqtracknum;
  float note_gain;
  float note_gain_muted;
};

static struct EditorSeqtrackVolume *get_editorseqtrackvolume(int seqtracknum){
  struct SeqTrack *seqtrack = get_seqtrack(seqtracknum);
  if (seqtrack==NULL)
    return NULL;

  R_ASSERT_RETURN_IF_FALSE2(seqtrack->for_audiofiles==false, NULL);
  
  struct EditorSeqtrackVolume *seqtrack_volume = talloc(sizeof(struct EditorSeqtrackVolume));

  seqtrack_volume->seqtracknum = seqtracknum;

  seqtrack_volume->note_gain = seqtrack->note_gain;
  seqtrack_volume->note_gain_muted = seqtrack->note_gain_muted;
  
  return seqtrack_volume;
}

void ADD_UNDO_FUNC(EditorSeqtrackVolume(int seqtracknum)){
  struct Tracker_Windows *window = root->song->tracker_windows;
  
  struct EditorSeqtrackVolume *seqtrack_volume = get_editorseqtrackvolume(seqtracknum);
  if (seqtrack_volume==NULL)
    return;
  
  Undo_Add_dont_stop_playing(
                             window->l.num,
                             window->wblock->l.num,
                             window->curr_track,
                             window->wblock->curr_realline,
                             seqtrack_volume,
                             Undo_Do_EditorSeqtrackVolume,
                             "EditorSeqtrackVolume"
                             );
}

static void *Undo_Do_EditorSeqtrackVolume(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
){
  struct EditorSeqtrackVolume *seqtrack_volume = pointer;
  int seqtracknum = seqtrack_volume->seqtracknum;
  
  struct EditorSeqtrackVolume *ret = get_editorseqtrackvolume(seqtracknum);
  if (ret==NULL)
    return seqtrack_volume; // Something is seriously wrong (assertion window was shown above), but I'm not sure if we can return NULL from this function so we return seqtrack_volume instead.

  struct SeqTrack *seqtrack = get_seqtrack(seqtracknum);
  if (seqtrack==NULL)
    return seqtrack_volume;

  R_ASSERT_RETURN_IF_FALSE2(seqtrack->for_audiofiles==false, seqtrack_volume);
  
  bool needlock = true; //is_playing_song();
  
  if(needlock) PLAYER_lock();
  {
    seqtrack->note_gain = seqtrack_volume->note_gain;
    seqtrack->note_gain_muted = seqtrack_volume->note_gain_muted;
    seqtrack->note_gain_has_changed_this_block = true;
  }
  if(needlock) PLAYER_unlock();

  SEQUENCER_update(SEQUPDATE_HEADERS);
  
  return ret;
}



///////////////////////////////////////////////////////////////////////////////////////////
// 6. Undo seqtrack config
/////////////////////////////////////////////////////////////////////////////////////////

static void *Undo_Do_SeqtrackConfig(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
);

struct SeqtrackConfig{
  int num;
  hash_t *configs[NUM_SEQTRACKS_CONFIGS];
};

static struct SeqtrackConfig *create_curr_seqtrack_config(void){
  struct SeqtrackConfig *seqtrack_config = talloc(sizeof(struct SeqtrackConfig));
  seqtrack_config->num = root->song->curr_seqtrack_config_num;

  for(int i=0;i<NUM_SEQTRACKS_CONFIGS;i++)
    seqtrack_config->configs[i] = SEQTRACKS_get_config(i);

  return seqtrack_config;
}

void ADD_UNDO_FUNC(SeqtrackConfig(void)){
  struct Tracker_Windows *window = root->song->tracker_windows;
  
  struct SeqtrackConfig *seqtrack_config = create_curr_seqtrack_config();
    
  Undo_Add_dont_stop_playing(
                             window->l.num,
                             window->wblock->l.num,
                             window->curr_track,
                             window->wblock->curr_realline,
                             seqtrack_config,
                             Undo_Do_SeqtrackConfig,
                             "SeqtrackConfig"
                             );
}

static void *Undo_Do_SeqtrackConfig(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
){
  struct SeqtrackConfig *seqtrack_config = pointer;

  struct SeqtrackConfig *ret = create_curr_seqtrack_config();

  for(int i=0;i<NUM_SEQTRACKS_CONFIGS;i++)
    SEQTRACKS_set_config(i, seqtrack_config->configs[i]);

  SEQTRACKS_set_curr_config(seqtrack_config->num);

  return ret;
}


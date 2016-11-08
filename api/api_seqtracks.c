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

#include "../common/includepython.h"

#include "../common/nsmtracker.h"
#include "../common/seqtrack_proc.h"
#include "../common/time_proc.h"
#include "../common/undo_sequencer_proc.h"
#include "../common/visual_proc.h"
#include "../common/OS_Bs_edit_proc.h"

#include "../audio/Mixer_proc.h"

#include "api_common_proc.h"

#include "radium_proc.h"


// sequencer

float getSequencerX1(void){
  return SEQUENCER_get_x1();
}

float getSequencerX2(void){
  return SEQUENCER_get_x2();
}

float getSequencerY1(void){
  return SEQUENCER_get_y1();
}

float getSequencerY2(void){
  return SEQUENCER_get_y2();
}

void undoSequencer(void){
  ADD_UNDO(Sequencer());
}

// sequencer

int64_t getSequencerSongLengthInFrames(void){
  return (SONG_get_length() + SEQUENCER_EXTRA_SONG_LENGTH) * MIXER_get_sample_rate();
}

int64_t getSequencerVisibleStartTime(void){
  return SEQUENCER_get_visible_start_time();
}

int64_t getSequencerVisibleEndTime(void){
  return SEQUENCER_get_visible_end_time();
}

void setSequencerVisibleStartTime(int64_t value){
  printf("                   Set: %f\n", value/48000.0);
  SEQUENCER_set_visible_start_time(value);
}

void setSequencerVisibleEndTime(int64_t value){
  SEQUENCER_set_visible_end_time(value);
}




float getSeqnavX1(void){
  return SEQNAV_get_x1();
}

float getSeqnavX2(void){
  return SEQNAV_get_x2();
}

float getSeqnavY1(void){
  return SEQNAV_get_y1();
}

float getSeqnavY2(void){
  return SEQNAV_get_y2();
}


float getSeqnavLeftSizeHandleX1(void){
  return SEQNAV_get_left_handle_x();
}

float getSeqnavLeftSizeHandleX2(void){
  return SEQNAV_get_left_handle_x() + SEQNAV_SIZE_HANDLE_WIDTH;
}

float getSeqnavLeftSizeHandleY1(void){
  return getSeqnavY1();
}

float getSeqnavLeftSizeHandleY2(void){
  return getSeqnavY2();
}

float getSeqnavRightSizeHandleX1(void){
  return SEQNAV_get_right_handle_x() - SEQNAV_SIZE_HANDLE_WIDTH;
}

float getSeqnavRightSizeHandleX2(void){
  return SEQNAV_get_right_handle_x();
}

float getSeqnavRightSizeHandleY1(void){
  return getSeqnavY1();
}

float getSeqnavRightSizeHandleY2(void){
  return getSeqnavY2();
}

void appendSeqtrack(void){
  undoSequencer();
  SEQUENCER_append_seqtrack(NULL);

  root->song->curr_seqtracknum = root->song->seqtracks.num_elements -1;
  BS_UpdatePlayList();
}

void insertSeqtrack(int pos){
  if (pos==-1)
    pos = root->song->curr_seqtracknum;
  
  if (pos < 0 || pos > root->song->seqtracks.num_elements){
    GFX_Message(NULL, "Position #%d not legal", pos);
    return;
  }

  undoSequencer();
  SEQUENCER_insert_seqtrack(NULL, pos);

  root->song->curr_seqtracknum = pos;
  BS_UpdatePlayList();
}

void deleteSeqtrack(int seqtracknum){
  if (seqtracknum==-1)
    seqtracknum = root->song->curr_seqtracknum;
  
  if (seqtracknum < 0 || seqtracknum >= root->song->seqtracks.num_elements){
    GFX_Message(NULL, "Sequencer track #%d does not exist", seqtracknum);
    return;
  }

  if (root->song->seqtracks.num_elements==1){
    GFX_Message(NULL, "Must have at least one sequencer track");
    return;
  }    
  
  undoSequencer();
  SEQUENCER_delete_seqtrack(seqtracknum);
}

void selectSeqtrack(int seqtracknum){
  if (seqtracknum < 0 || seqtracknum >= root->song->seqtracks.num_elements){
    GFX_Message(NULL, "Sequencer track #%d does not exist", seqtracknum);
    return;
  }

  root->song->curr_seqtracknum = seqtracknum;
  BS_UpdatePlayList();
  SEQUENCER_update();
}

int getCurrSeqtrack(void){
  return root->song->curr_seqtracknum;
}

int getNumSeqtracks(void){
  return root->song->seqtracks.num_elements;
}


// seqtracks

float getSeqtrackX1(int seqtracknum){
  if (seqtracknum < 0 || seqtracknum >= root->song->seqtracks.num_elements){
    GFX_Message(NULL, "Sequencer track #%d does not exist", seqtracknum);
    return 0;
  }
  return SEQTRACK_get_x1(seqtracknum);
}

float getSeqtrackX2(int seqtracknum){
  if (seqtracknum < 0 || seqtracknum >= root->song->seqtracks.num_elements){
    GFX_Message(NULL, "Sequencer track #%d does not exist", seqtracknum);
    return 0;
  }
  return SEQTRACK_get_x2(seqtracknum);
}

float getSeqtrackY1(int seqtracknum){
  if (seqtracknum < 0 || seqtracknum >= root->song->seqtracks.num_elements){
    GFX_Message(NULL, "Sequencer track #%d does not exist", seqtracknum);
    return 0;
  }
  return SEQTRACK_get_y1(seqtracknum);
}

float getSeqtrackY2(int seqtracknum){
  if (seqtracknum < 0 || seqtracknum >= root->song->seqtracks.num_elements){
    GFX_Message(NULL, "Sequencer track #%d does not exist", seqtracknum);
    return 0;
  }
  return SEQTRACK_get_y2(seqtracknum);
}

int64_t findClosestSeqtrackBarStart(int seqtracknum, int64_t pos){
  if (seqtracknum < 0 || seqtracknum >= root->song->seqtracks.num_elements){
    GFX_Message(NULL, "Sequencer track #%d does not exist", seqtracknum);
    return 0;
  }
  
  return SEQUENCER_find_closest_bar_start(seqtracknum, pos);
}

void insertSilenceToSeqtrack(int seqtracknum, int64_t pos, int64_t duration){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return;

  ADD_UNDO(Sequencer());

  SEQTRACK_insert_silence(seqtrack, pos, duration);
}

int addBlockToSeqtrack(int seqtracknum, int blocknum, int64_t pos){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return -1;

  struct Blocks *block = getBlockFromNum(blocknum);
  if (block==NULL)
    return -1;

  ADD_UNDO(Sequencer());

  return SEQTRACK_insert_block(seqtrack, block, pos);
}

// seqblocks

int getNumSeqblocks(int seqtracknum){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return 0;
  else
    return seqtrack->seqblocks.num_elements;
}

int64_t getSeqblockStartTime(int seqblocknum, int seqtracknum){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack);
  if (seqblock==NULL)
    return 0;

  SEQTRACK_update_all_seqblock_start_and_end_times(seqtrack);
    
  return seqblock->start_time * MIXER_get_sample_rate(); //seqblock->time;
}

int64_t getSeqblockEndTime(int seqblocknum, int seqtracknum){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack);
  if (seqblock==NULL)
    return 0;

  SEQTRACK_update_all_seqblock_start_and_end_times(seqtrack);

  return seqblock->end_time * MIXER_get_sample_rate();
//return seqblock->time + getBlockSTimeLength(seqblock->block);
}

float getSeqblockX1(int seqblocknum, int seqtracknum){
  if (getSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_x1(seqblocknum, seqtracknum);
}

float getSeqblockX2(int seqblocknum, int seqtracknum){
  if (getSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_x2(seqblocknum, seqtracknum);
}

float getSeqblockY1(int seqblocknum, int seqtracknum){
  if (getSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_y1(seqblocknum, seqtracknum);
}

float getSeqblockY2(int seqblocknum, int seqtracknum){
  if (getSeqblockFromNum(seqblocknum, seqtracknum)==NULL)
    return 0;
  
  return SEQBLOCK_get_y2(seqblocknum, seqtracknum);
}

void moveSeqblock(int seqblocknum, int64_t abstime, int seqtracknum, int new_seqtracknum){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack);
  if (seqblock==NULL)
    return;

  if (new_seqtracknum==-1)
    new_seqtracknum = seqtracknum;

  struct SeqTrack *new_seqtrack = getSeqtrackFromNum(new_seqtracknum);
  if (new_seqtrack==NULL)
    return;
  
  root->song->curr_seqtracknum = new_seqtracknum;
  
  //printf("Trying to move seqblocknum %d/%d to %d\n",seqtracknum,seqblocknum,(int)abstime);
  SEQTRACK_move_seqblock(seqtrack, seqblock, abstime);
}

void moveSeqblockGfx(int seqblocknum, int64_t abstime, int seqtracknum, int new_seqtracknum){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack);
  if (seqblock==NULL)
    return;

  if (new_seqtracknum==-1)
    new_seqtracknum = seqtracknum;

  struct SeqTrack *new_seqtrack = getSeqtrackFromNum(new_seqtracknum);
  if (new_seqtrack==NULL)
    return;
  
  root->song->curr_seqtracknum = new_seqtracknum;
  
  //printf("Trying to move seqblocknum %d/%d to %d\n",seqtracknum,seqblocknum,(int)abstime);
  SEQTRACK_move_gfx_seqblock(seqtrack, seqblock, abstime);
}

void deleteSeqblock(int seqblocknum, int seqtracknum){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack);
  if (seqblock==NULL)
    return;

  undoSequencer();
  
  SEQTRACK_delete_seqblock(seqtrack, seqblock);

  root->song->curr_seqtracknum = R_MAX(seqtracknum -1, 0);
  BS_UpdatePlayList();
}

int getSeqblockBlocknum(int seqblocknum, int seqtracknum){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack);
  if (seqblock==NULL)
    return 0;

  return seqblock->block->l.num;
}

/*
void selectSeqblock(int seqblocknum, int seqtracknum){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack);
  if (seqblock==NULL)
    return;

  root->song->curr_seqtracknum = seqtracknum;

  selectBlock(seqblock->block->l.num, -1);
}
*/

void selectSeqblock(bool is_selected, int seqblocknum, int seqtracknum){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack);
  if (seqblock==NULL)
    return;

  seqblock->is_selected = is_selected;

  SEQUENCER_update();
}


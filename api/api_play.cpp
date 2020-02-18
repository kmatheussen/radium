/* Copyright 2001 Kjetil S. Matheussen

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

#define __STDC_FORMAT_MACROS 1

#include "Python.h"

#include <inttypes.h>


#include "../common/nsmtracker.h"
#include "../common/playerclass.h"
#include "../common/player_proc.h"
#include "../common/OS_Player_proc.h"
#include "../common/time_proc.h"
#include "../common/realline_calc_proc.h"
#include "../common/wblocks_proc.h"
#include "../common/cursor_updown_proc.h"

#include "../audio/Mixer_proc.h"

#include "../common/sequencer_proc.h"


#include "api_common_proc.h"


extern PlayerClass *pc;


void playBlockFromStart(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL) return;

  PlayBlockFromStart(window,true); // true == do_loop
}

void playBlockFromCurrent(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL) return;

  PlayBlockCurrPos(window);
}

void playSongFromStart(void){
  PlaySongFromStart();
}

void playSongFromCurrent(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL) return;

  PlaySongCurrPos();
}

void playSongFromCurrentSequencerPosition(void){
  //printf("        PLAY CURR SEQPOS: %f\n",ATOMIC_DOUBLE_GET(pc->song_abstime));
  PlaySong(ATOMIC_DOUBLE_GET(pc->song_abstime));
}

void playRangeFromStart(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL) return;

  PlayRangeFromStart(window);
}

void playRangeFromCurrent(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL) return;

  PlayRangeCurrPos(window);
}

void setSongPos(int64_t pos){
  if (pos < 0){
    handleError("setSongPos: Song pos must be 0 or higher: %" PRId64 "\n", pos);
    return;
  }

  PLAYER_set_song_pos(pos, -1, false, true);
}

int64_t getSongPos(void){
  return ATOMIC_DOUBLE_GET(pc->song_abstime);
}

void incSongPos(double inc){
  setSongPos(R_BOUNDARIES(0, ATOMIC_DOUBLE_GET(pc->song_abstime) + (inc*pc->pfreq), getSongLengthInFrames()));
}

int64_t getLastSongPosStart(void){
  return pc->last_song_starttime;
}

void setLastSongPosStart(int64_t pos){
  if (pos < 0){
    handleError("setLastSongPos: Song pos must be 0 or higher: %" PRId64 "\n", pos);
    return;
  }

  pc->last_song_starttime = pos;
}


void playSongFromPos(int64_t pos){
  PlaySong(pos);
}

void playStop(void){
  PlayStop();
}

void volumeUp(void){
  PLAYER_volumeUp(1);
}

void volumeDown(void){
  PLAYER_volumeDown(1);
}

void mute(void){
  PLAYER_mute();
}


bool isPlaying(void){
  return is_playing();
}

bool isPlayingBlock(void){
  return isPlaying() && pc->playtype==PLAYBLOCK;
}

bool isPlayingSong(void){
  return isPlaying() && pc->playtype==PLAYSONG;
}

double lineDuration(int line, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return 1.0;

  struct Blocks *block = wblock->block;
  
  if (line < 0){
    handleError("lineDuration: Line must be 0 or larger: %d",line);
    return 1.0;
  }

  if (line >= block->num_lines){
    handleError("lineDuration: Line can not be bigger than the number of lines in block. Line: %d. (number of lines in block: %d)", line, block->num_lines);
    return 1.0;
  }
  
  Place p1 = {line, 0, 1};
  Place p2 = {line+1, 0, 1};

  STime s1 = Place2NonSwingingSTime(block, &p1);
  STime s2 = Place2NonSwingingSTime(block, &p2);

  return (double)(s2-s1) / (double)MIXER_get_sample_rate();
}


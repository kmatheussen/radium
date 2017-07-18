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





#include "nsmtracker.h"
#include "playerclass.h"
#include "player_proc.h"
#include "OS_Player_proc.h"
#include "threading.h"
#include "placement_proc.h"

#include "player_pause_proc.h"


extern PlayerClass *pc;


extern void (*Ptask2MtaskCallBack)(void);

/*
void PC_Pause(void){
	PlayStop();
}
*/

int g_pausing_level = 0;
static bool g_was_playing = false;
static int g_playtype = 0;
static bool g_was_playing_range = false;
static DEFINE_ATOMIC(int, g_pause_realline) = 0;
static DEFINE_ATOMIC(int, g_pause_blocknum) = 0;
static int64_t g_pause_song_abstime = 0;

// Called from the realtime thread.
void PC_Pause_set_pos(int blocknum, int realline){
  ATOMIC_SET(g_pause_realline, realline);
  ATOMIC_SET(g_pause_blocknum, blocknum);
}

// Note that it's perfectly fine calling PlayStop() between calling PC_Pause and PC_StopPause. PC_StopPause will still work as it's supposed to.
void PC_Pause(void){
  R_ASSERT(THREADING_is_main_thread());

  //printf("   000 Enter pause %d\n", g_pausing_level);

  g_pausing_level++;

  //printf("   Enter pause %d\n", g_pausing_level);
  
  if (g_pausing_level > 1)
    return;
  
  g_was_playing = false;
  g_playtype = 0;
  g_was_playing_range = false;
  
  if (is_playing()){

    if(g_is_starting_up==false){
      struct Tracker_Windows *window = root->song->tracker_windows;
      //g_pause_realline = ATOMIC_GET(window->wblock->till_curr_realline);
      window->message = "Temporarily stopping player";
      window->message_duration_left = 100;
    }

    //Play_get_curr_playing_realline(&g_pause_realline, &g_pause_blocknum);
    
    g_playtype = pc->playtype;
    g_was_playing_range = pc->is_playing_range;
    g_pause_song_abstime = ATOMIC_DOUBLE_GET(pc->song_abstime);
    PlayStop();
    g_was_playing = true;

    //printf("   PAUSING\n\n\n");
  }  
}

static const Place *get_place_from_realline(const struct WBlocks *wblock, int realline){
  int bounded_realline = R_BOUNDARIES(0, realline, wblock->num_reallines-1);
  return &wblock->reallines[bounded_realline]->l.p;
}

static void stop_pause(struct Tracker_Windows *window, bool force_play_block){
  R_ASSERT(THREADING_is_main_thread());

  g_pausing_level--;

  if (g_pausing_level < 0){
    R_ASSERT(false);
    g_pausing_level = 0;
  }

  //printf("   Leaving pause %d. relaline: %d\n", g_pausing_level, ATOMIC_GET(g_pause_realline));
  
  if (g_pausing_level>0)
    return;

  if (window==NULL)
    window = root->song->tracker_windows;

  if (window==NULL)
    return;
  
  struct WBlocks *wblock = window->wblock;

  if (wblock==NULL)
    return;
  
  if (g_was_playing) {

    //printf("\n\n   RESTARTING\n");
    
    if (force_play_block) {

      const Place *place = get_place_from_realline(wblock, ATOMIC_GET(g_pause_realline));
      PlayBlockCurrPos2(window, place);

    } else if (g_playtype==PLAYSONG) {
      
      PlaySong(g_pause_song_abstime);

    } else {
      
      const Place *place = get_place_from_realline(wblock, ATOMIC_GET(g_pause_realline));
    
      if (g_was_playing_range)
        PlayRangeCurrPos2(window, place);
      
      else if (g_playtype==PLAYBLOCK)
        PlayBlockCurrPos2(window, place);
      
    }
  }
}

void PC_StopPause(struct Tracker_Windows *window){
  stop_pause(window, false);
}

void PC_StopPause_ForcePlayBlock(struct Tracker_Windows *window){
  stop_pause(window, true);
}

// Old code below. It was more sophisticated, but also a bit more complicated.

#if 0

#include "PEQcommon_proc.h"
#include "PEQnotes_proc.h"
#include "PEQrealline_proc.h"
#include "PEQline_proc.h"
#include "PEQblock_proc.h"
#include "PEQfxs_proc.h"
#include "PEQ_clock_proc.h"
#include "PEQ_LPB_proc.h"
#include "PEQ_Signature_proc.h"
#include "PEQ_Beats_proc.h"


void PC_Pause(void){

  if( ! is_playing()){
    return;					// There is only two threads, so its impossible to start playing
									// before PC_StopPause is called.
  }

#ifdef NOPAUSEPLAY
	PlayStop();
#else
	PausePlayer();				// Will not return before the player has really paused.

	PC_ReturnElements_fromPlayPos(pc->playpos);

	(*Ptask2MtaskCallBack)();
#endif
}


/*
void PC_StopPause(void){
	return;
}
*/

extern struct Root *root;

void PC_StopPause(void){
#ifdef NOPAUSEPLAY
  return;
#else
	Place place;
	STime pausetime;
	struct PEventQueue *peq;

	if( ! is_playing()) return;

	pausetime=pc->pausetime;

	PlaceSetFirstPos(&place);

	InitPEQclock();
        InitPEQ_LPB(pc->block,place);
        InitPEQ_Signature(pc->block,place);
        InitPEQ_Beat(pc->block,place);
	InitPEQrealline(pc->block,&place);
	InitPEQline(pc->block,&place);
	InitPEQblock(pc->block,&place);
	InitAllPEQnotes(pc->block,&place);

	peq=pc->peq;
	while(peq!=NULL && pausetime>=peq->l.time){
		PC_RemoveFirst();

		(*peq->TreatMe)(pausetime,peq,0);

		peq=pc->peq;
	}

	ATOMIC_SET(root->setfirstpos, false);

	StopPausePlayer();
#endif
}


#endif



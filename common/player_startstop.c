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







#include <gc.h>

#include "nsmtracker.h"
#include "playerclass.h"
#include "placement_proc.h"
#include "OS_Player_proc.h"
#if 0
#include "PEQrealline_proc.h"
#include "PEQline_proc.h"
#include "PEQblock_proc.h"
#include "PEQnotes_proc.h"
#include "PEQ_clock_proc.h"
#include "PEQ_LPB_proc.h"
#include "PEQ_Signature_proc.h"
#include "PEQ_Beats_proc.h"
#include "PEQmempool_proc.h"
#endif
#include "instruments_proc.h"
#include "blocklist_proc.h"
#include "OS_Ptask2Mtask_proc.h"
#include "time_proc.h"
#include "OS_Bs_edit_proc.h"
#include "list_proc.h"
#include "clipboard_range_calc_proc.h"
#include "gfx_wblocks_proc.h"
#include "patch_proc.h"
#include "cursor_updown_proc.h"
#include "../midi/midi_i_input_proc.h"
#include "../audio/Mixer_proc.h"
#include "scheduler_proc.h"

#include "player_proc.h"

// Safer (and simpler) if set to 1, except that we might run out of memory while playing.
#define STOP_GC_WHILE_PLAYING 0


/******************** NOTES ******************************

  Even more "methods" for the playerclass struct.

*********************************************************/

extern PlayerClass *pc;

static bool g_player_was_stopped_manually = true;


extern void (*Ptask2MtaskCallBack)(void);

static void PlayStopReally(bool doit){
    g_player_was_stopped_manually = true;
    
        //ATOMIC_SET(pc->isplaying, false);
	//ATOMIC_SET(pc->initplaying, false);
        //ATOMIC_SET(pc->playertask_has_been_called, false);

        if (ATOMIC_GET(is_starting_up))
          return;

        if(ATOMIC_GET(pc->player_state) == PLAYER_STATE_STOPPED)
          return;
        
	StopAllInstruments();

        ATOMIC_SET(pc->player_state, PLAYER_STATE_STOPPING);
        
        printf("PlayStopReally called: %s\n",doit==true?"true":"false");

        if (PLAYER_current_thread_has_lock()){
          RError("Potential deadlock detected: Calling PlayStopReally while holding player lock.");
          return;
        }

        if (PLAYER_is_running())
          while(ATOMIC_GET(pc->player_state) != PLAYER_STATE_STOPPED)
            OS_WaitForAShortTime(5);

        R_ASSERT(is_playing()==false);

        //R_ASSERT(is_playing()==false);
                
#if !USE_OPENGL
	if(doit) (*Ptask2MtaskCallBack)();
#endif

        ATOMIC_ADD(pc->play_id, 1);

        struct Tracker_Windows *window = root->song->tracker_windows;
        struct WBlocks *wblock = window->wblock;

        ScrollEditorToRealLine(window,wblock,wblock->curr_realline);

        R_ASSERT(is_playing()==false);
                
#if !USE_OPENGL
        DrawWBlockSpesific(window,wblock,wblock->curr_realline,wblock->curr_realline); // clear cursor shade.
        UpdateAllWTracks(window,wblock,wblock->curr_realline,wblock->curr_realline); // same here.
#endif
        
#if STOP_GC_WHILE_PLAYING
        printf("[hb gakkgakk: %d\n",GC_dont_gc);
#endif

        R_ASSERT(is_playing()==false);
                
#if STOP_GC_WHILE_PLAYING
#error "must make gc_dont_gc thread safe"
        //while(GC_is_disabled())
        while(GC_dont_gc>0)
          Threadsafe_GC_enable();
#endif
        
        MIDI_insert_recorded_midi_events();

        R_ASSERT(is_playing()==false);
        
        //InitPEQmempool(); // Clean memory used by player so it can be freed by the garbage collector.

        // Clean all seqtrack->curr_seqblock values
        PLAYER_lock();{
          VECTOR_FOR_EACH(struct SeqTrack *seqtrack, &root->song->seqtracks){
            seqtrack->curr_seqblock = NULL;
          }END_VECTOR_FOR_EACH;
        }PLAYER_unlock();

          
        R_ASSERT(is_playing()==false);
}


void PlayStop(void){
  g_player_was_stopped_manually = true;
  
  if(!is_playing()){
    StopAllInstruments();
    R_ASSERT(is_playing()==false);
  }
  else
    PlayStopReally(true);
}

static void start_player(int playtype, int playpos, bool set_curr_playlist, Place *place, struct Blocks *block){
  R_ASSERT(ATOMIC_GET(pc->player_state)==PLAYER_STATE_STOPPED);

  g_player_was_stopped_manually = false;
    
  // GC isn't used in the player thread, but the player thread sometimes holds pointers to gc-allocated memory.
  //while(GC_is_disabled()==false){
  //printf("Calling gc_disable: %d\n",GC_dont_gc);
#if STOP_GC_WHILE_PLAYING
#error "must make gc_dont_gc thread safe"
  while(GC_dont_gc<=0){
    Threadsafe_GC_disable();
  }
#endif

  ATOMIC_SET(root->song_state_is_locked, true);
  
  PLAYER_lock();{
    
    pc->playpos=playpos;
    ATOMIC_ADD(pc->play_id, 1);

    if (set_curr_playlist)
      ATOMIC_SET(root->curr_playlist, playpos);
    
    pc->playtype = playtype;
    
    atomic_pointer_write((void**)&pc->block, block);
    
    ATOMIC_SET(root->curr_blocknum, pc->block->l.num);
    
  }PLAYER_unlock();

  
  printf("Play. root->curr_block: %d. Block: %p\n",ATOMIC_GET(root->curr_blocknum),pc->block);
  //abort();
  fflush(stdout);


#if 0
  // player is stopped, so we can do these things here.
  InitPEQclock();
  InitPEQ_LPB(pc->block,place);
  InitPEQ_Signature(pc->block,place);
  InitPEQ_Beat(pc->block,place);
  InitPEQrealline(block,place);
  InitPEQline(block,place);
  InitPEQblock(block,place);
  InitAllPEQnotes(block,place);
#else

  int64_t block_start_time = 0;
  if (playtype==PLAYSONG) {
    PLAYER_lock();{
      struct SeqTrack *seqtrack = RT_get_curr_seqtrack();
      struct SeqBlock *seqblock = seqtrack->seqblocks.elements[playpos];
      block_start_time = seqblock->time;
    }PLAYER_unlock();
  }

  int64_t block_time = Place2STime(block,place);
  int64_t global_start_time = block_start_time + block_time;

  if (playtype==PLAYSONG)
    start_seqtrack_song_scheduling(global_start_time);
  else
    start_seqtrack_block_scheduling(*place);
  
#endif
  
  ATOMIC_SET(pc->player_state, PLAYER_STATE_STARTING_TO_PLAY);
  while(ATOMIC_GET(pc->player_state) != PLAYER_STATE_PLAYING && ATOMIC_GET(pc->player_state) != PLAYER_STATE_STOPPED)
    OS_WaitForAShortTime(2);
}

// pc->is_playing_range must be set before calling this function
static void PlayBlock(
	struct Blocks *block,
	Place *place,
        bool do_loop
){
  int playtype;
  if(do_loop==true)
    playtype=PLAYBLOCK;
  else
    playtype=PLAYBLOCK_NONLOOP;

  start_player(playtype, 0, false, place, block);
}

void PlayBlockFromStart(struct Tracker_Windows *window,bool do_loop){
	PlayStopReally(false);

	ATOMIC_SET(root->setfirstpos, true);
        
        {
          struct WBlocks *wblock=window->wblock;
          Place place;
          PlaceSetFirstPos(&place);
          pc->is_playing_range = false;
          PlayBlock(wblock->block,&place,do_loop);
        }
}

void PlayBlockCurrPos2(struct Tracker_Windows *window, Place *place){
	struct WBlocks *wblock;
	PlayStopReally(false);

	ATOMIC_SET(root->setfirstpos, false);

	wblock=window->wblock;

	if(wblock->curr_realline==0)
          ATOMIC_SET(root->setfirstpos, true);

        pc->is_playing_range = false;
	PlayBlock(wblock->block,place,true);
}

void PlayBlockCurrPos(struct Tracker_Windows *window){
  Place *place = &window->wblock->reallines[window->wblock->curr_realline]->l.p;
  PlayBlockCurrPos2(window, place);
}

static void PlayRange(struct Tracker_Windows *window, Place *place){
  struct WBlocks *wblock = window->wblock;
  
  //Place *place_start = getRangeStartPlace(wblock);
  Place *place_end   = getRangeEndPlace(wblock);
  pc->range_duration = Place2STime(wblock->block, place_end) - Place2STime(wblock->block, place);
  
  pc->is_playing_range = true;
  PlayBlock(wblock->block,place,true);
}


void PlayRangeFromStart(struct Tracker_Windows *window){
	PlayStopReally(false);
        
	struct WBlocks *wblock = window->wblock;

	if( ! wblock->isranged) return;

        Place *place = getRangeStartPlace(wblock);
          
	ATOMIC_SET(root->setfirstpos, false);

	if(wblock->rangey1==0)
          ATOMIC_SET(root->setfirstpos, true);

        PlayRange(window, place);
}

void PlayRangeCurrPos2(struct Tracker_Windows *window, Place *place){
  PlayStopReally(false);

  struct WBlocks *wblock = window->wblock;

  if( ! wblock->isranged) return;
        
  if (place==NULL)
    place = &wblock->reallines[wblock->curr_realline]->l.p;

  /*
  Place *start_place=getRangeStartPlace(wblock);
  Place *end_place=getRangeEndPlace(wblock);
  */
  
  PlayRange(window, place);
}

void PlayRangeCurrPos(struct Tracker_Windows *window){
  PlayRangeCurrPos2(window, NULL);
}

static int g_playing_realline = 0;
static int g_playing_blocknum = 0;
void Play_set_curr_playing_realline(int realline, int blocknum){
  g_playing_realline = realline;
  g_playing_blocknum = blocknum;
}

void Play_get_curr_playing_realline(int *realline, int *blocknum){
  *realline = g_playing_realline;
  *blocknum = g_playing_blocknum;
}

// called very often
static void EditorFollowsPlayCursorLoop(void){
  if (ATOMIC_GET(root->play_cursor_onoff)==false)
    return;
  
  if (ATOMIC_GET(root->editor_follows_play_cursor_onoff)==false)
    return;

  struct Tracker_Windows *window = root->song->tracker_windows;
  struct WBlocks *wblock = window->wblock;

  //printf("%d %d\n",g_playing_realline,wblock->bot_realline);

  if (g_playing_blocknum != -1 && wblock->l.num != g_playing_blocknum)
    return;

  bool scrollit = false;

  if (g_playing_realline > wblock->bot_realline)
    scrollit = true;

  if (g_playing_realline < wblock->top_realline)
    scrollit = true;

  //printf("%3d -- %3d -- %3d\n", wblock->top_realline, g_playing_realline, wblock->bot_realline);
  
  if (scrollit) {
    int diff = wblock->top_realline - wblock->curr_realline;
    int goal = g_playing_realline - diff - 1;
    if (goal >= wblock->num_reallines)
      goal = wblock->num_reallines-1;
    //printf("SCROLLING. now: %d. Goal: %d\n", wblock->curr_realline, goal);
    ScrollEditorToRealLine(window,wblock,goal);
  }

}

// called very often
static void PlayHandleRangeLoop(void){
  //printf("is_range: %d, is_playing: %d, stopped_manually: %d\n",pc->is_playing_range, is_playing(), g_player_was_stopped_manually);
  
  if (pc->is_playing_range == false)
    return;

  if(ATOMIC_GET(pc->player_state)==PLAYER_STATE_STOPPED && g_player_was_stopped_manually==false) {
    StopAllInstruments();
    if (MIXER_is_saving())
      PlayStopReally(true);
    else
      PlayRangeFromStart(root->song->tracker_windows);
  }
}

// called very often
void PlayCallVeryOften(void){
  if(ATOMIC_GET(pc->player_state)==PLAYER_STATE_PLAYING)
    EditorFollowsPlayCursorLoop();
  
  PlayHandleRangeLoop();
}

static void PlaySong(
	Place *place,
	int playpos,
        bool set_curr_playlist
){
  struct Blocks *block=BS_GetBlockFromPos(playpos);

  printf("Play song. blocknum:%d. Block: %p\n",block->l.num, block);

  pc->is_playing_range = false;

  start_player(PLAYSONG, playpos, set_curr_playlist, place, block);

  // GC isn't used in the player thread, but the player thread sometimes holds pointers to gc-allocated memory.
#if STOP_GC_WHILE_PLAYING
#error "must make gc_dont_gc thread safe"
  while(GC_is_disabled()==false)
    Threadsafe_GC_disable();
#endif
}


void PlaySongFromStart(struct Tracker_Windows *window){
	PlayStopReally(false);

        BS_SelectPlaylistPos(0);
        
	//debug("root->curr_block: %d\n",root->curr_block);
	ATOMIC_SET(root->setfirstpos, true);

	InitAllInstrumentsForPlaySongFromStart();

        Place place;
        PlaceSetFirstPos(&place);
        PlaySong(&place,0,true);
}


void PlaySongCurrPos2(struct Tracker_Windows *window, Place *place){
	struct Blocks *block;
	struct WBlocks *wblock;
	int playpos;
	bool changeblock=false;

	wblock=window->wblock;

	PlayStopReally(false);

	ATOMIC_SET(root->setfirstpos, false);

	playpos = ATOMIC_GET(root->curr_playlist);
                
	block=BS_GetBlockFromPos(playpos);
	if(block==NULL) return;

	if(wblock->l.num!=block->l.num){
		wblock=ListFindElement1(&window->wblocks->l,block->l.num);
		changeblock=true;
		ATOMIC_SET(root->setfirstpos, true);
	}

	if(
		! changeblock &&
		playpos==root->song->length-1 &&
		wblock->curr_realline==wblock->num_reallines  // ??. Never supposed to happen.
	){
		return;
	}

	if(wblock->curr_realline==0)
          ATOMIC_SET(root->setfirstpos, true);


	if(changeblock){
		place=PlaceGetFirstPos();
	}else{
          if (place==NULL || PlaceLegal(block, place)==false)
            place=&wblock->reallines[wblock->curr_realline]->l.p;
        }

#if 0
	place->line++;
	debug("nextline: %d\n",Place2STime(wblock->block,place));
	place->line--;
#endif
        
	PlaySong(place,playpos,false);
}

void PlaySongCurrPos(struct Tracker_Windows *window){
  PlaySongCurrPos2(window, NULL);
}

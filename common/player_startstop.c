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
#include "seqtrack_proc.h"
#include "song_tempo_automation_proc.h"
#include "../OpenGL/Widget_proc.h"

#include "player_proc.h"

// Safer (and simpler) if set to 1, except that we might run out of memory while playing.
#define STOP_GC_WHILE_PLAYING 0


/******************** NOTES ******************************

  Even more "methods" for the playerclass struct.

*********************************************************/

extern PlayerClass *pc;

static bool g_player_was_stopped_manually = true;


extern void (*Ptask2MtaskCallBack)(void);

static void clear_scheduler_and_stop_player(void){
  bool is_clear;
  PLAYER_lock();{
    is_clear = SCHEDULER_all_is_clear();
  }PLAYER_unlock();

  if (is_clear)
    return;
      
  ATOMIC_SET(pc->player_state, PLAYER_STATE_STOPPING);
  while(ATOMIC_GET(pc->player_state) != PLAYER_STATE_STOPPED)
    OS_WaitForAShortTime(3);
}

static void PlayStopReally(bool doit){
    g_player_was_stopped_manually = true;
    
        //ATOMIC_SET(pc->isplaying, false);
	//ATOMIC_SET(pc->initplaying, false);
        //ATOMIC_SET(pc->playertask_has_been_called, false);

        if (ATOMIC_GET(is_starting_up))
          return;

        if(ATOMIC_GET(pc->player_state) == PLAYER_STATE_STOPPED) {
          clear_scheduler_and_stop_player(); // must clear, and it doesn't hurt to stop player one more time.
          return;
        }
        
        StopAllInstruments();

        if (PLAYER_current_thread_has_lock()){
          RError("Potential deadlock detected: Calling PlayStopReally while holding player lock.");
          return;
        }

        clear_scheduler_and_stop_player();
        
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
          ALL_SEQTRACKS_FOR_EACH(){
            seqtrack->curr_seqblock = NULL;
          }END_ALL_SEQTRACKS_FOR_EACH;
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

static void start_player(int playtype, int64_t abstime, Place *place, struct Blocks *block, struct SeqTrack *seqtrack, struct SeqBlock *seqblock){
  R_ASSERT(ATOMIC_GET(pc->player_state)==PLAYER_STATE_STOPPED);

  g_player_was_stopped_manually = false;

  {
    struct Blocks *block = root->song->blocks;
    while(block!=NULL){
      ATOMIC_DOUBLE_SET(block->player_time, -100.0);
      block = NextBlock(block);
    }
  }
  
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
  
  ATOMIC_ADD(pc->play_id, 1);

  
  pc->playtype = playtype;

  
  if (playtype==PLAYSONG) {

    R_ASSERT(block==NULL);
    
    player_start_data_t startdata = {0}; 
    startdata.playtype = playtype;
    startdata.abstime = abstime;
    if (place!=NULL)
      startdata.place = *place;
    startdata.seqtrack = seqtrack;
    startdata.seqblock = seqblock;
    
    start_seqtrack_song_scheduling(&startdata);
    
  } else {

    R_ASSERT(playtype=PLAYBLOCK);
    R_ASSERT(abstime==0);
    R_ASSERT(seqtrack==NULL);
    R_ASSERT(seqblock==NULL);

    R_ASSERT_RETURN_IF_FALSE(block!=NULL);
    R_ASSERT_RETURN_IF_FALSE(place!=NULL);
    
    start_seqtrack_block_scheduling(block, *place);
    
  }

  // We can set pc->absabstime here without getting a tsan hit since pc->absabstime is neither read nor written to in a player thread while the player is stopped.
  if (playtype==PLAYBLOCK)
    pc->absabstime = abstime;
  else
    pc->absabstime = TEMPOAUTOMATION_get_absabstime(abstime);
  
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

  start_player(playtype, 0, place, block, NULL, NULL);
}

void PlayBlockFromStart(struct Tracker_Windows *window,bool do_loop){
	PlayStopReally(false);

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

	wblock=window->wblock;

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

  static bool got_lock = false; // Lock editor graphics while stopping and playing to avoid some jumpiness.

  if(ATOMIC_GET(pc->player_state)==PLAYER_STATE_STOPPING && g_player_was_stopped_manually==false) {
    GL_draw_lock();
    got_lock=true;
  }
  
  if(ATOMIC_GET(pc->player_state)==PLAYER_STATE_STOPPED && g_player_was_stopped_manually==false) {
    if(got_lock==false)
      GL_draw_lock();
    
    StopAllInstruments();
    
    if (MIXER_is_saving())
      PlayStopReally(true);
    else
      PlayRangeFromStart(root->song->tracker_windows);

    GL_draw_unlock();
    
    got_lock=false;
  }
}

static void PlayHandleSequencerLoop(void){
  //printf("is_range: %d, is_playing: %d, stopped_manually: %d\n",pc->is_playing_range, is_playing(), g_player_was_stopped_manually);
  
  if (!SEQUENCER_is_looping())
    return;

  if (pc->is_playing_range==true)
    return;

  static bool got_lock = false; // Lock editor graphics while stopping and playing to avoid some jumpiness.

  if(ATOMIC_GET(pc->player_state)==PLAYER_STATE_STOPPING && g_player_was_stopped_manually==false) {
    GL_draw_lock();
    got_lock=true;
  }
  
  if(ATOMIC_GET(pc->player_state)==PLAYER_STATE_STOPPED && g_player_was_stopped_manually==false) {
    if(got_lock==false)
      GL_draw_lock();
    
    StopAllInstruments();
    
    if (MIXER_is_saving())
      PlayStopReally(true);
    else
      PlaySong(SEQUENCER_get_loop_start());
    
    GL_draw_unlock();
    
    got_lock=false;
  }
}

// called very often
void PlayCallVeryOften(void){
  if(ATOMIC_GET(pc->player_state)==PLAYER_STATE_PLAYING)
    EditorFollowsPlayCursorLoop();
  
  PlayHandleRangeLoop();
  PlayHandleSequencerLoop();
}


void PlaySong(int64_t abstime){
  printf("Play song. abstime: %d\n", (int)abstime);

  PlayStopReally(false);

  if (abstime==0)
    InitAllInstrumentsForPlaySongFromStart();

  pc->is_playing_range = false;

  start_player(PLAYSONG, abstime, NULL, NULL, NULL, NULL);

  // GC isn't used in the player thread, but the player thread sometimes holds pointers to gc-allocated memory.
#if STOP_GC_WHILE_PLAYING
#error "must make gc_dont_gc thread safe"
  while(GC_is_disabled()==false)
    Threadsafe_GC_disable();
#endif
}

void PlaySongCurrPos(void){

  struct WBlocks *wblock=root->song->tracker_windows->wblock;
  
  int playlistpos = BS_GetCurrPlaylistPos();

  struct SeqTrack *seqtrack = SEQUENCER_get_curr_seqtrack();
  
  struct SeqBlock *seqblock = BS_GetSeqBlockFromPos(playlistpos);
  if (seqblock==NULL)
    return;

  Place *place;
  if (wblock->block != seqblock->block){
    place=PlaceGetFirstPos();
  }else{
    place=&wblock->reallines[wblock->curr_realline]->l.p;
  }

  int64_t abstime = get_abstime_from_seqtime(seqtrack, seqblock, Place2STime(seqblock->block, place));
  
  PlaySong(abstime);
}

void PlaySongFromStart(void){
  if (SEQUENCER_is_looping())
    PlaySong(SEQUENCER_get_loop_start());
  else
    PlaySong(0);
}


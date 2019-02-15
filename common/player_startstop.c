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
#include "realline_calc_proc.h"

#include "../api/api_proc.h"

#include "player_proc.h"


// Safer (and simpler) if set to 1, except that we might run out of memory while playing.
#define STOP_GC_WHILE_PLAYING 0


/******************** NOTES ******************************

  Even more "methods" for the playerclass struct.

*********************************************************/

extern PlayerClass *pc;

bool g_player_was_stopped_manually = true;
bool g_player_was_stopped_because_it_reached_sequencer_loop_end = false;

extern void (*Ptask2MtaskCallBack)(void);

static void clear_scheduler_and_stop_player_and_releases_player_lock(void){
  bool is_clear = SCHEDULER_all_is_clear();
  
  PLAYER_unlock();

  if (is_clear){
    
    // R_ASSERT(is_playing()==false); // We are still playing if there are samples left in the sequencer.
    
    //printf("          IS_CLEAR\n");
    //return;
  }

  if (ATOMIC_GET(pc->player_state) != PLAYER_STATE_STOPPED){
    ATOMIC_SET(pc->player_state, PLAYER_STATE_STOPPING);

    RSEMAPHORE_wait(g_player_stopped_semaphore, 1);
  }
}

int g_assert_not_stopping_player = 0;

static void PlayStopReally(bool doit, bool stop_jack_transport_as_well){
#if 0 //!defined(RELEASE)
  printf("  PLAYER_LOCK  \n");
  if (ATOMIC_GET(root->editonoff)==false){
    static int downcount = 1;
    fprintf(stderr," Aborting since edit is turned off. (this is a debug feature and not a bug!). count down: %d\n", downcount);
    if (downcount==0)
      abort();
    downcount--;
  }
  /*
  printf("   >> Obtaining player lock\n");
  if (is_playing())
    abort();
  */
#endif

  //printf("  Stopping. Realline now: %d\n", root->song->tracker_windows->wblock->curr_realline);

  //printf(" Backtgrace: %s\n", JUCE_get_backtrace());
  
    
  if (PLAYER_current_thread_has_lock()){
    RError("Potential deadlock detected: Calling PlayStopReally while holding player lock.");
    return;
  }

  R_ASSERT(g_assert_not_stopping_player==0);
  
  if (stop_jack_transport_as_well)
    if (useJackTransport()){
      MIXER_TRANSPORT_stop();
      // We can not exit here. Some code depends on the player to have stopped when PlayStopReally returns.
    }
  
  g_player_was_stopped_manually = true;
  g_player_was_stopped_because_it_reached_sequencer_loop_end = false;

  //ATOMIC_SET(pc->isplaying, false);
  //ATOMIC_SET(pc->initplaying, false);
  //ATOMIC_SET(pc->playertask_has_been_called, false);
  
  if (ATOMIC_GET_RELAXED(is_starting_up)){
    return;
  }

  if(ATOMIC_GET(pc->player_state) == PLAYER_STATE_STOPPED) {
    PLAYER_lock();
    clear_scheduler_and_stop_player_and_releases_player_lock(); // must clear, and it doesn't hurt to stop player one more time.
    return;
  }
  
  PLAYER_lock();{
    
    StopAllInstruments();
    
  }clear_scheduler_and_stop_player_and_releases_player_lock();
  
  
  R_ASSERT_NON_RELEASE(ATOMIC_GET(pc->player_state) == PLAYER_STATE_STOPPED);
  
  //R_ASSERT(is_playing()==false);
  
#if !USE_OPENGL
  if(doit) (*Ptask2MtaskCallBack)();
#endif
  
  ATOMIC_ADD(pc->play_id, 1);
  
  struct Tracker_Windows *window = root->song->tracker_windows;
  struct WBlocks *wblock = window->wblock;

  //printf("  Setting realline to %d\n", wblock->curr_realline);
  ScrollEditorToRealLine(window,wblock,wblock->curr_realline);
  
  R_ASSERT_NON_RELEASE(ATOMIC_GET(pc->player_state) == PLAYER_STATE_STOPPED);
  
#if !USE_OPENGL
  DrawWBlockSpesific(window,wblock,wblock->curr_realline,wblock->curr_realline); // clear cursor shade.
  UpdateAllWTracks(window,wblock,wblock->curr_realline,wblock->curr_realline); // same here.
#endif
  
#if STOP_GC_WHILE_PLAYING
  printf("[hb gakkgakk: %d\n",GC_dont_gc);
#endif

  R_ASSERT_NON_RELEASE(ATOMIC_GET(pc->player_state) == PLAYER_STATE_STOPPED);
  
#if STOP_GC_WHILE_PLAYING
#error "must make gc_dont_gc thread safe"
  //while(GC_is_disabled())
  while(GC_dont_gc>0)
    Threadsafe_GC_enable();
#endif
  
  MIDI_insert_recorded_midi_events();
  MIXER_set_all_plugins_to_not_recording();
    
  R_ASSERT_NON_RELEASE(ATOMIC_GET(pc->player_state) == PLAYER_STATE_STOPPED);
  
  //InitPEQmempool(); // Clean memory used by player so it can be freed by the garbage collector.
  
  // Clean all seqtrack->curr_seqblock values
  PLAYER_lock();{
    ALL_SEQTRACKS_FOR_EACH(){
      atomic_pointer_write((void**)&seqtrack->curr_seqblock, NULL);
    }END_ALL_SEQTRACKS_FOR_EACH;
  }PLAYER_unlock();
  
  
  R_ASSERT_NON_RELEASE(ATOMIC_GET(pc->player_state) == PLAYER_STATE_STOPPED);
}

static void play_stop(bool called_from_jack_transport){
  g_player_was_stopped_manually = true;
  g_player_was_stopped_because_it_reached_sequencer_loop_end = false;

  if(!is_playing()){
    StopAllInstruments();
    R_ASSERT(is_playing()==false);
    return;
  }

  bool was_playing_song = pc->playtype==PLAYSONG;
  
  if (called_from_jack_transport)
    PlayStopReally(true, false);
  else
    PlayStopReally(true, true);

  if(was_playing_song)
    SEQUENCER_update(SEQUPDATE_TIME|SEQUPDATE_NAVIGATOR); // Update start-pos-cursor
}
                      
void PlayStop(void){
  play_stop(false);
}

void PlayStop_from_jack_transport(void){
  play_stop(true);
}


static void start_player(int playtype, double abstime, int64_t absabstime, const Place *place, struct Blocks *block, struct SeqTrack *seqtrack, struct SeqBlock *seqblock){
  R_ASSERT(ATOMIC_GET(pc->player_state)==PLAYER_STATE_STOPPED);

  if (PLAYER_is_running()==false)
    return;
  
  if (abstime<0)
    R_ASSERT(absabstime>=0);
  else if (absabstime<0)
    R_ASSERT(abstime>=0);
  else
    R_ASSERT(false);
  
    
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

  if (abstime < 0){
    if (playtype==PLAYBLOCK)
      abstime = absabstime;
    else
      abstime = TEMPOAUTOMATION_get_abstime_from_absabstime(absabstime);
  }
  
  if (absabstime < 0){
    if (playtype==PLAYBLOCK)
      absabstime = abstime;
    else
      absabstime = TEMPOAUTOMATION_get_absabstime(abstime);
  }

  
  if (playtype==PLAYSONG) {

    R_ASSERT(block==NULL);

    pc->last_song_starttime = abstime;
    
    g_initing_starting_to_play_song = true;
    {
      //printf("...............INiting song playing. abstime: %d. absabstime: %d\n", (int)abstime, (int)absabstime);
      InitAllInstrumentsWhenPlayingSong(abstime);
    }
    g_initing_starting_to_play_song = false;

    player_start_data_t startdata = {0}; 
    startdata.playtype = playtype;
    startdata.abstime = abstime;
    if (place!=NULL)
      startdata.place = *place;
    startdata.seqtrack = seqtrack;
    startdata.seqblock = seqblock;
    
    start_seqtrack_song_scheduling(&startdata, playtype);
    
  } else {

    R_ASSERT(playtype=PLAYBLOCK);
    R_ASSERT(abstime==0);
    R_ASSERT(seqtrack==NULL);
    R_ASSERT(seqblock==NULL);

    R_ASSERT_RETURN_IF_FALSE(block!=NULL);
    R_ASSERT_RETURN_IF_FALSE(place!=NULL);
    
    start_seqtrack_block_scheduling(block, *place, playtype);
    
  }

  root->song->tracker_windows->must_redraw_editor = true; // Because we have set new curr_seqblock values.
  
  // We can set pc->absabstime here without getting a tsan hit since pc->absabstime is neither read nor written to in a player thread while the player is stopped.
  pc->absabstime = absabstime;

  R_ASSERT(ATOMIC_GET(pc->player_state)==PLAYER_STATE_STOPPED);
    
  ATOMIC_SET(pc->player_state, PLAYER_STATE_STARTING_TO_PLAY);
  while(ATOMIC_GET(pc->player_state) != PLAYER_STATE_PLAYING && ATOMIC_GET(pc->player_state) != PLAYER_STATE_STOPPED){
    //printf("  WATING FOR STATE. Now: %d\n", ATOMIC_GET(pc->player_state));
    OS_WaitForAShortTime(2);
  }
}

// pc->is_playing_range must be set before calling this function
static void PlayBlock(
	struct Blocks *block,
	const Place *place,
        bool do_loop
){
  int playtype;
  if(do_loop==true)
    playtype=PLAYBLOCK;
  else
    playtype=PLAYBLOCK_NONLOOP;

  start_player(playtype, 0.0, -1, place, block, NULL, NULL);
}

void PlayBlockFromStart(struct Tracker_Windows *window,bool do_loop){
        PlayStopReally(false, true);

        {
          struct WBlocks *wblock=window->wblock;
          Place place;
          PlaceSetFirstPos(&place);
          pc->is_playing_range = false;
          PlayBlock(wblock->block,&place,do_loop);
        }
}

void PlayBlockCurrPos2(struct Tracker_Windows *window, const Place *place){
	struct WBlocks *wblock;
	PlayStopReally(false, true);

	wblock=window->wblock;

        pc->is_playing_range = false;
	PlayBlock(wblock->block,place,true);
}

void PlayBlockCurrPos(struct Tracker_Windows *window){
  const Place *place = &window->wblock->reallines[window->wblock->curr_realline]->l.p;
  PlayBlockCurrPos2(window, place);
}

static void PlayRange(struct Tracker_Windows *window, const Place *place){
  struct WBlocks *wblock = window->wblock;
  
  //Place *place_start = GetRangeStartPlace(wblock);
  const Place *place_end   = &wblock->rangey2;
  pc->range_duration = Place2STime(wblock->block, place_end) - Place2STime(wblock->block, place);
  
  pc->is_playing_range = true;
  PlayBlock(wblock->block,place,true);
}


void PlayRangeFromStart(struct Tracker_Windows *window){
        PlayStopReally(false, true);
        
	struct WBlocks *wblock = window->wblock;

	if( ! wblock->isranged) return;

        const Place *place = &wblock->rangey1;
          
        PlayRange(window, place);
}

void PlayRangeCurrPos2(struct Tracker_Windows *window, const Place *place){
  PlayStopReally(false, true);

  struct WBlocks *wblock = window->wblock;

  if( ! wblock->isranged) return;
        
  if (place==NULL)
    place = &wblock->reallines[wblock->curr_realline]->l.p;

  /*
  Place *start_place=GetRangeStartPlace(wblock);
  Place *end_place=GetRangeEndPlace(wblock);
  */
  
  PlayRange(window, place);
}

void PlayRangeCurrPos(struct Tracker_Windows *window){
  PlayRangeCurrPos2(window, NULL);
}

static int g_playing_realline = 0; // none of this stuff is necessary now since Play_get_curr_playing_realline isn't called anymore.
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

// Lock editor graphics while stopping and playing to avoid some jumpiness.
// However, I can't see the jumpiness anymore, plus that I can provoke deadlock if playing a range very fast, so set it to 0.
#define DO_LOCK_DRAW_LOCK 0

static void maybe_draw_lock(bool *got_lock){
  if(*got_lock==false){
#if DO_USE_DRAW_LOCK
    GL_draw_lock();
#endif
    *got_lock=true;
  }
}

static void maybe_draw_unlock(bool *got_lock){
  if (*got_lock==true){
#if DO_USE_DRAW_LOCK
    GL_draw_unlock();
#endif
    *got_lock=false;
  }
}

// called very often
static void PlayHandleRangeLoop(void){
  //printf("is_range: %d, is_playing: %d, stopped_manually: %d\n",pc->is_playing_range, is_playing(), g_player_was_stopped_manually);

  static bool got_lock = false;
  
  if (pc->is_playing_range == false){
    maybe_draw_unlock(&got_lock);
    return;
  }

  if(ATOMIC_GET(pc->player_state)==PLAYER_STATE_STOPPING && g_player_was_stopped_manually==false) {
    maybe_draw_lock(&got_lock);
  }
  
  if(ATOMIC_GET(pc->player_state)==PLAYER_STATE_STOPPED) {
    if (g_player_was_stopped_manually==false) {
      maybe_draw_lock(&got_lock);
      //printf(" here\n");
      StopAllInstruments();
      
      if (MIXER_is_saving())
        PlayStopReally(true, true);
      else
        PlayRangeFromStart(root->song->tracker_windows);
    }

    maybe_draw_unlock(&got_lock);
  }
}

static void PlayHandleSequencerLoop(void){
  //printf("is_range: %d, is_playing: %d, stopped_manually: %d\n",pc->is_playing_range, is_playing(), g_player_was_stopped_manually);

  static bool got_lock = false; // Lock editor graphics while stopping and playing to avoid some jumpiness.
  
  if (!SEQUENCER_is_looping() || pc->is_playing_range==true) {
    maybe_draw_unlock(&got_lock);
    return;
  }

  if(ATOMIC_GET(pc->player_state)==PLAYER_STATE_STOPPING && g_player_was_stopped_manually==false) {
    maybe_draw_lock(&got_lock);
  }
  
  if(ATOMIC_GET(pc->player_state)==PLAYER_STATE_STOPPED) {
    if (g_player_was_stopped_manually==false  && g_player_was_stopped_because_it_reached_sequencer_loop_end==true) {
      maybe_draw_lock(&got_lock);

      StopAllInstruments(); // PlaySong calls PlayStopReally, but PlayStopReally doesn't call StoppAllInstruments if player_state==PLAYER_STATE_STOPPED (which it is now).

      g_player_was_stopped_manually = true; // When using jack transport, playstop is not called directly, and then PlaySong will be called several times.
            
      if (MIXER_is_saving())
        PlayStopReally(true, true);
      else
        PlaySong(SEQUENCER_get_loop_start());

    }

    maybe_draw_unlock(&got_lock);
  }
}

bool PlayerIsCurrentlyPlayingLoop(void){
  if (SEQUENCER_is_looping() || pc->is_playing_range==true)
    if(g_player_was_stopped_manually==false)
      return true;

  return false;
}

// called very often
void PlayCallVeryOften(void){
  if(ATOMIC_GET(pc->player_state)==PLAYER_STATE_PLAYING)
    EditorFollowsPlayCursorLoop();

  PlayHandleRangeLoop();
  PlayHandleSequencerLoop();
}

bool g_initing_starting_to_play_song = false;

// All calls to 'start_player', where the first argument is PLAYSONG, MUST go through here.
static void play_song(double abstime, int64_t absabstime, bool called_from_jack_transport){
  //printf("Play song. abstime: %f, absabstime: %f\n", abstime, (double)absabstime/44100.0);

  if (called_from_jack_transport==false && useJackTransport()){
    R_ASSERT_RETURN_IF_FALSE(abstime>=0);
    R_ASSERT_RETURN_IF_FALSE(absabstime<0);
    MIXER_TRANSPORT_play(abstime); // This call will eventually trigger a new call to play_song, where 'called_from_jack_transport' is true.
    return;
  }

  PlayStopReally(false, false);

  pc->is_playing_range = false;
  
  start_player(PLAYSONG, abstime, absabstime, NULL, NULL, NULL, NULL);

  // GC isn't used in the player thread, but the player thread sometimes holds pointers to gc-allocated memory.
#if STOP_GC_WHILE_PLAYING
#error "must make gc_dont_gc thread safe"
  while(GC_is_disabled()==false)
    Threadsafe_GC_disable();
#endif

  SEQUENCER_update(SEQUPDATE_TIME|SEQUPDATE_NAVIGATOR); // Update start-pos-cursor
}

void PlaySong(double abstime){
  play_song(R_MAX(0, abstime), -1, false);
}

void PlaySong_from_jack_transport(int64_t absabstime){
  play_song(-1.0, R_MAX(0, absabstime), true);
}

void PlaySongCurrPos(void){

  struct SeqTrack *curr_seqtrack = SEQUENCER_get_curr_seqtrack();
  if (curr_seqtrack->for_audiofiles){
    PlaySong(ATOMIC_DOUBLE_GET(pc->song_abstime));
    return;
  }
  
  struct WBlocks *wblock=root->song->tracker_windows->wblock;
  
  int playlistpos = BS_GetCurrPlaylistPos();

  struct SeqBlock *seqblock = BS_GetSeqBlockFromPos(playlistpos);
  if (seqblock==NULL){
    R_ASSERT_NON_RELEASE(false);
    return;
  }

  if (seqblock->block==NULL){

    R_ASSERT_NON_RELEASE(false);
    PlaySong(ATOMIC_DOUBLE_GET(pc->song_abstime));
    
  } else {
    
    const Place *place;
    if (wblock->block != seqblock->block){
      place=PlaceGetFirstPos();
    }else{
      place=&wblock->reallines[wblock->curr_realline]->l.p;
    }
    
    int64_t stime = Place2STime(seqblock->block, place);
    int64_t seqtime = seqblock->t.time + blocktime_to_seqtime(seqblock, stime);
  
    PlaySong(seqtime);
  }
}

void PlaySongFromStart(void){
  if (SEQUENCER_is_looping())
    PlaySong(SEQUENCER_get_loop_start());
  else
    PlaySong(0);
}


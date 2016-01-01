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
#include "PEQrealline_proc.h"
#include "PEQline_proc.h"
#include "PEQblock_proc.h"
#include "PEQnotes_proc.h"
#include "instruments_proc.h"
#include "blocklist_proc.h"
#include "OS_Ptask2Mtask_proc.h"
#include "time_proc.h"
#include "PEQ_clock_proc.h"
#include "OS_Bs_edit_proc.h"
#include "list_proc.h"
#include "clipboard_range_calc_proc.h"
#include "gfx_wblocks_proc.h"
#include "patch_proc.h"
#include "cursor_updown_proc.h"
#include "PEQ_LPB_proc.h"
#include "PEQ_Signature_proc.h"
#include "PEQ_Beats_proc.h"
#include "../midi/midi_i_input_proc.h"
#include "../audio/Mixer_proc.h"

#include "player_proc.h"



/******************** NOTES ******************************

  Even more "methods" for the playerclass struct.

*********************************************************/

extern PlayerClass *pc;
extern struct Root *root;

extern void (*Ptask2MtaskCallBack)(void);

static void PlayStopReally(bool doit){ 
	pc->isplaying=false;
	pc->initplaying=false;
        pc->playertask_has_been_called = false;
        pc->is_playing_range = false;
        
        printf("PlayStopReally called: %s\n",doit==true?"true":"false");

        if (PLAYER_current_thread_has_lock()){
          RError("Potential deadlock detected: Calling PlayStopReally while holding player lock.");
          return;
        }

        if (PLAYER_is_running())
          while(pc->peq!=NULL) OS_WaitForAShortTime(20);

	StopAllInstruments();

#if !USE_OPENGL
	if(doit) (*Ptask2MtaskCallBack)();
#endif

	pc->end_time=0;
        pc->end_time_f=0;

        pc->play_id++;

        struct Tracker_Windows *window = root->song->tracker_windows;
        struct WBlocks *wblock = window->wblock;

        ScrollEditorToRealLine(window,wblock,wblock->curr_realline);

#if !USE_OPENGL
        DrawWBlockSpesific(window,wblock,wblock->curr_realline,wblock->curr_realline); // clear cursor shade.
        UpdateAllWTracks(window,wblock,wblock->curr_realline,wblock->curr_realline); // same here.
#endif
        printf("[hb gakkgakk: %d\n",GC_dont_gc);
        PATCH_reset_time();

        //while(GC_is_disabled())
        //while(GC_dont_gc>0)
        //  GC_enable();

        MIDI_insert_recorded_midi_events();
}

void PlayStop(void){
	if(! pc->isplaying)
          StopAllInstruments();
        else
          PlayStopReally(true);
}

static void PlayBlock(
	struct Blocks *block,
	Place *place,
        bool do_loop
){

        // GC isn't used in the player thread, but the player thread sometimes holds pointers to gc-allocated memory.
        //while(GC_is_disabled()==false){
          //printf("Calling gc_disable: %d\n",GC_dont_gc);
  //while(GC_dont_gc<=0){
  //        GC_disable();
  //      }

	pc->initplaying=true;

		pc->playpos=0;
                pc->play_id++;
                
                if(do_loop==true)
                  pc->playtype=PLAYBLOCK;
                else
                  pc->playtype=PLAYBLOCK_NONLOOP;

		pc->block=block;

		root->curr_block=pc->block->l.num;
		printf("Play block. root->curr_block: %d. Block: %p\n",root->curr_block,pc->block);
                //abort();
                fflush(stdout);

#if !USE_OPENGL
		pc->isplaying=true;
		(*Ptask2MtaskCallBack)();
#endif
		pc->isplaying=false;

                PATCH_reset_time();
		InitPEQclock();
                InitPEQ_LPB(pc->block,place);
                InitPEQ_Signature(pc->block,place);
                InitPEQ_Beat(pc->block,place);
		InitPEQrealline(block,place);
		InitPEQline(block,place);
		InitPEQblock(block,place);
		InitAllPEQnotes(block,place);

		StartPlayer();							// An OS spesific function.
		pc->isplaying=true;


	pc->initplaying=false;
}

void PlayBlockFromStart(struct Tracker_Windows *window,bool do_loop){
	PlayStopReally(false);

	root->setfirstpos=true;
	pc->seqtime=0;

        {
          struct WBlocks *wblock=window->wblock;
          Place place;
          PlaceSetFirstPos(&place);
          PlayBlock(wblock->block,&place,do_loop);
        }
}

void PlayBlockCurrPos(struct Tracker_Windows *window){
	struct WBlocks *wblock;
	Place *place;
	PlayStopReally(false);

	root->setfirstpos=false;

	wblock=window->wblock;

	if(wblock->curr_realline==0) root->setfirstpos=true;

	place       = &wblock->reallines[wblock->curr_realline]->l.p;
	pc->seqtime = -Place2STime(wblock->block,place);

//	printf("contblock, time: %d\n",pc->seqtime);

	PlayBlock(wblock->block,place,true);
}


void PlayRangeCurrPos(struct Tracker_Windows *window){
	struct WBlocks *wblock;
	Place *place;

	PlayStopReally(false);

	wblock=window->wblock;

	if( ! wblock->isranged) return;

	root->setfirstpos=false;

	if(wblock->rangey1==0) root->setfirstpos=true;

	place=getRangeStartPlace(wblock);
	pc->seqtime=-Place2STime(wblock->block,place);

//	printf("playrange, time: %d\n",pc->seqtime);

        Place *place_start = getRangeStartPlace(wblock);
        Place *place_end   = getRangeEndPlace(wblock);
        pc->range_duration = Place2STime(wblock->block, place_end) - Place2STime(wblock->block, place_start);
        pc->is_playing_range = true;
        
	PlayBlock(wblock->block,place,true);
}

static int g_playing_realline = 0;
static int g_playing_blocknum = 0;
void Play_set_curr_playing_realline(int realline, int blocknum){
  g_playing_realline = realline;
  g_playing_blocknum = blocknum;
}

// called very often
static void EditorFollowsPlayCursorLoop(void){
  if (root->play_cursor_onoff==false)
    return;
  
  if (root->editor_follows_play_cursor_onoff==false)
    return;

  struct Tracker_Windows *window = root->song->tracker_windows;
  struct WBlocks *wblock = window->wblock;

  //printf("%d %d\n",g_playing_realline,wblock->bot_realline);

  if (wblock->l.num != g_playing_blocknum)
    return;

  bool scrollit = false;

  if (g_playing_realline > wblock->bot_realline)
    scrollit = true;

  if (g_playing_realline < wblock->top_realline)
    scrollit = true;

  if (scrollit) {      
    int diff = wblock->top_realline - wblock->curr_realline;
    ScrollEditorToRealLine(window,wblock,g_playing_realline - diff - 1);
  }

}

// called very often
static void PlayHandleRangeLoop(void){

  struct Blocks *block = pc->block;
  
  if (pc->is_playing_range == false || block==NULL)
    return;

  //printf("duration: %d\nrealtime: %d\n\n", (int)duration, (int)pc->therealtime);

  
  STime start_therealtime = pc->therealtime;

  if (start_therealtime >= pc->range_duration/block->reltempo) {
    PlayRangeCurrPos(root->song->tracker_windows);
    int counter = 0;
    while (pc->therealtime == start_therealtime && counter < 50){ // Wait for the player to start up.
      OS_WaitForAShortTime(20);
      counter++;
    }
  }
}

// called very often
void PlayCallVeryOften(void){
  EditorFollowsPlayCursorLoop();    
  PlayHandleRangeLoop();
}

static void PlaySong(
	Place *place,
	int playpos
){
	debug("haaasfdfsafsa, root->song->length: %d\n\n\n",root->song->length);
	pc->initplaying=true;

		struct Blocks *block=BL_GetBlockFromPos(playpos);

		printf("Play song. blocknum:%d. Block: %p\n",block->l.num, block);

		pc->playpos=playpos;
                pc->play_id++;
                
		root->curr_playlist=playpos;

		pc->playtype=PLAYSONG;

		pc->block=block;

		root->curr_block=block->l.num;
#if !USE_OPENGL
		pc->isplaying=true;
		(*Ptask2MtaskCallBack)();
#endif
		pc->isplaying=false;

                PATCH_reset_time();
		InitPEQclock();
                InitPEQ_LPB(pc->block,place);
                InitPEQ_Signature(pc->block,place);
                InitPEQ_Beat(pc->block,place);
		InitPEQrealline(block,place);
		InitPEQline(block,place);
		InitPEQblock(block,place);
		InitAllPEQnotes(block,place);

		StartPlayer();							// An OS spesific function.
		pc->isplaying=true;


	pc->initplaying=false;

        // GC isn't used in the player thread, but the player thread sometimes holds pointers to gc-allocated memory.
        //while(GC_is_disabled()==false)
        //  GC_disable();
}


void PlaySongFromStart(struct Tracker_Windows *window){
	PlayStopReally(false);

	BS_SelectPlaylistPos(0);
	debug("root->curr_block: %d\n",root->curr_block);
	root->setfirstpos=true;
	pc->seqtime=0;

	InitAllInstrumentsForPlaySongFromStart();

        {
          Place place;
          PlaceSetFirstPos(&place);
          PlaySong(&place,0);
        }
}

void PlaySongCurrPos(struct Tracker_Windows *window){
	struct Blocks *block;
	struct WBlocks *wblock;
	Place *place;
	int playpos;
	bool changeblock=false;

	wblock=window->wblock;

	PlayStopReally(false);

	root->setfirstpos=false;

	playpos=root->curr_playlist;
                
	block=BL_GetBlockFromPos(playpos);
	if(block==NULL) return;

	if(wblock->l.num!=block->l.num){
		wblock=ListFindElement1(&window->wblocks->l,block->l.num);
		changeblock=true;
		root->setfirstpos=true;
	}

	if(
		! changeblock &&
		playpos==root->song->length-1 &&
		wblock->curr_realline==wblock->num_reallines  // ??. Never supposed to happen.
	){
		return;
	}

	if(wblock->curr_realline==0) root->setfirstpos=true;


	debug("contsong, playpos: %d , root->curr_block: %d\n",playpos,root->curr_block);

	if(changeblock){
		place=PlaceGetFirstPos();
		pc->seqtime=0;
	}else{
		place=&wblock->reallines[wblock->curr_realline]->l.p;
		pc->seqtime=-Place2STime(wblock->block,place);
	}
	debug("contsong, time: %d, playpos: %d , root->curr_block: %d\n",pc->seqtime,playpos,root->curr_block);

	place->line++;
	debug("nextline: %d\n",Place2STime(wblock->block,place));
	place->line--;

	PlaySong(place,playpos);
}











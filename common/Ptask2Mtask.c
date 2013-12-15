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


/************* overview ********************************

	The player-task sometimes needs to let the main-task
	do some of its work. The Ptask2Mtask function is
	called from the player-task, and
	recieves a pointer to a function that must be called
	from the main-task with 'pointer' as argument.

	The player-task could do all this work by itself, but
	that would slow down and allso make it pretty complicated
	because semaphores or things like that has to be
	added.

	Used when scrolling, change of block, etc.

	NOTE. The main-task doesn't have to be finished processing
	the function before Ptask2Mtask is called again.

*******************************************************/

#include <math.h>

#include "nsmtracker.h"
#include "playerclass.h"
#include "wblocks_proc.h"
#include "cursor_updown_proc.h"
#include "list_proc.h"
#include "common_proc.h"
#include "time_proc.h"
#include "blocklist_proc.h"
#include "OS_Bs_edit_proc.h"
#include "gfx_proc.h"
#include "settings_proc.h"

#include "OS_Player_proc.h"


extern PlayerClass *pc;
extern struct Root *root;

//static int 
static STime last_time = 0;
struct WBlocks *last_wblock = NULL;

int scrolls_per_second = -1;
int default_scrolls_per_second = 20;

// TODO / WARNING, this value must be calculated. (this value is calculated from 48000/60)
static double g_vblank_stime_interval = 800.0;

static double get_realline_stime(struct WBlocks *wblock, int realline){  
  return Place2STime(wblock->block, &wblock->reallines[realline]->l.p);
}

static double get_current_time(double ideally, double g_vblank_stime_interval){
  if (fabs(pc->start_time - ideally) > g_vblank_stime_interval*8)
    return pc->start_time;
  else
    return ideally;
}

static void find_current_wblock_and_realline(struct Tracker_Windows *window, struct WBlocks **wblock, double *realline){
  static double s_last_stime = 0.0;

  double ideally = s_last_stime + (g_vblank_stime_interval*(double)pc->block->reltempo);
  double current_stime = get_current_time(ideally, g_vblank_stime_interval);
  double block_stime;

  if(ideally != current_stime)
    printf("NOT RETURNING IDEALLY: %f - %f\n",ideally,current_stime);

  s_last_stime = current_stime;

  if (pc->playtype==PLAYBLOCK || pc->playtype==PLAYBLOCK_NONLOOP) {
    *wblock = window->wblock;
    double current_block_sduration = getBlockSTimeLength((*wblock)->block);
    block_stime = fmod(current_stime, current_block_sduration);
  } else {
    abort();
    block_stime = 0.0;
  }

  double prev_line_stime = 0.0;
  int i_realline;
  for(i_realline=1; i_realline<(*wblock)->num_reallines; i_realline++){
    double curr_line_stime = get_realline_stime(*wblock, i_realline);
    if (curr_line_stime >= block_stime) {
      *realline = scale_double(block_stime,
                               prev_line_stime, curr_line_stime,
                               i_realline-1, i_realline);
      return;
    }
    prev_line_stime = curr_line_stime;
  }

  *realline = (*wblock)->num_reallines-0.001; // should not happen that we get here.
}


static void UpdateReallineByLines(struct Tracker_Windows *window, struct WBlocks *wblock, double d_till_curr_realline){
  int till_curr_realline = (int)d_till_curr_realline;

  if(scrolls_per_second==-1)
    scrolls_per_second = SETTINGS_read_int("scrolls_per_second", default_scrolls_per_second);

  bool do_scrolling = false;
  
  if(wblock != last_wblock)
    do_scrolling = true;
  
  else if (last_time > pc->therealtime)
    do_scrolling = true;
  
  else if(till_curr_realline < wblock->curr_realline)
    do_scrolling = true;
  
  else if(till_curr_realline > wblock->curr_realline){
    STime from_time = (STime) ((double)Place2STime(wblock->block, &wblock->reallines[wblock->curr_realline]->l.p) / wblock->block->reltempo);
    STime to_time   = (STime) ((double)Place2STime(wblock->block, &wblock->reallines[till_curr_realline]->l.p) / wblock->block->reltempo);
    
    STime time = to_time - from_time;
    
    STime time_necessary_to_scroll = pc->pfreq / scrolls_per_second;
    
    if(time>=time_necessary_to_scroll)
      do_scrolling = true;
  }
  
  if(do_scrolling==true) {
    ScrollEditorToRealLine(window,wblock,till_curr_realline);
    last_time = pc->therealtime;
    last_wblock = wblock;
  }
}

double g_curr_subrealline = 0.0;

static void UpdateReallineByPixels(struct Tracker_Windows *window, struct WBlocks *wblock, double till_curr_realline){
  // Perhaps later add filtering here, depending on how finegrained the subpixel painting is.
  int i_till_curr_realline = (int)till_curr_realline;
  g_curr_subrealline = scale_double(till_curr_realline - (double)i_till_curr_realline, 0, 1, 0, window->fontheight);
  //printf("g_curr_subrealline: %f, till_curr_realline: %f\n",g_curr_subrealline, till_curr_realline);
  Blt_scrollMark(window);
  ScrollEditorToRealLine(window,wblock,i_till_curr_realline);
}

void P2MUpdateSongPosCallBack(void){
  struct Tracker_Windows *window      = root->song->tracker_windows;
  bool                    setfirstpos = root->setfirstpos;
  NInt                    curr_block  = root->curr_block;

  if(pc->playtype==PLAYSONG)
    BS_SelectPlaylistPos(root->curr_playlist);

  if(window->playalong==true){

    struct WBlocks *wblock=ListFindElement1(&window->wblocks->l,curr_block);
    int till_curr_realline=wblock->till_curr_realline;
    double d_till_curr_realline=wblock->till_curr_realline;

    if(root->editonoff){
      find_current_wblock_and_realline(window, &wblock, &d_till_curr_realline);
      if(R_ABS(till_curr_realline - (int)d_till_curr_realline)>1){
        printf("player: %d, calc: %f\n",till_curr_realline,d_till_curr_realline);
      }
      till_curr_realline = (int)d_till_curr_realline;
    }

    if(window->curr_block!=curr_block){
      if(setfirstpos){
        wblock->curr_realline=0;
        SetWBlock_Top_And_Bot_Realline(window,wblock);
      }
      SelectWBlock(
                   window,
                   wblock
                   );
    }
                  
    if(setfirstpos) // The player routine (PEQblock.c) sets this one.
      till_curr_realline=wblock->till_curr_realline=0;

    //fprintf(stderr,"tilline: %d\n",till_curr_realline);
          
    if(root->editonoff)
      UpdateReallineByPixels(window, wblock, d_till_curr_realline);
    else
      UpdateReallineByLines(window, wblock, d_till_curr_realline);
          
  }

  root->setfirstpos=false;
}

//void (*Ptask2MtaskCallBack)(void)= &P2MUpdateSongPosCallBack;



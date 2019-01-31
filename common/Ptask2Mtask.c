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




#include "nsmtracker.h"
#include "playerclass.h"
#include "wblocks_proc.h"
#include "cursor_updown_proc.h"
#include "list_proc.h"
#include "common_proc.h"
#include "time_proc.h"
#include "OS_Bs_edit_proc.h"
#include "gfx_proc.h"
#include "settings_proc.h"


extern PlayerClass *pc;
extern struct Root *root;

//static int 
struct WBlocks *last_wblock = NULL;

int scrolls_per_second = -1;
int default_scrolls_per_second = 20;


#if USE_OPENGL

#include "../OpenGL/Widget_proc.h"
#include "../OpenGL/Render_proc.h"

#include "Ptask2Mtask_proc.h"


// Simpler version when using opengl
void P2MUpdateSongPosCallBack(void){

  bool isplaying = ATOMIC_GET(pc->player_state)==PLAYER_STATE_PLAYING;
        
  struct Tracker_Windows *window=root->song->tracker_windows;
  struct WBlocks *wblock;

  if (isplaying){

    struct Blocks *block = RT_get_curr_visible_block();
    
    if (block==NULL){
          
      if (window->curr_block != -1){
        window->curr_block = -1;
        GL_create(window);
      }

      //printf("         Returning\n");
      return;
      
    }

    wblock = ListFindElement1(&window->wblocks->l, block->l.num);
    
  } else {

    wblock = window->wblock;

  }

  
  NInt curr_block_num = wblock->l.num;
    

  //printf("  P2Mupdatesongposcallback: window->curr_block: %d\n", window->curr_block);

  
  if (isplaying) {

    int old_curr_realline = wblock->curr_realline;
    int till_curr_realline = R_BOUNDARIES(0, ATOMIC_GET(wblock->till_curr_realline), wblock->num_reallines-1); // till_curr_realline can be set from any thread, at any time, to any value.
    
    if (!ATOMIC_GET(root->play_cursor_onoff)){
      //printf("P2MUpdateSongPosCallBack: Setting curr_realline to %d\n",till_curr_realline);
      wblock->curr_realline = till_curr_realline;
      wblock->top_realline += till_curr_realline - old_curr_realline;
      wblock->bot_realline += till_curr_realline - old_curr_realline;
    }  

  }

  

  if(window->curr_block!=curr_block_num){

#if 0
    if (ATOMIC_GET(root->editonoff)==false)
      GL_create(window); // <-- Faster update (no, doesn't seem to make a difference), but it's also complicated to avoid calling GL_create twice when doing this. (need separate update variables for editor and non-editor)
#endif

    //printf("Bef. w: %d\n",window->curr_block);

    SelectWBlock(
                 window,
                 wblock
                 );
    
    //printf("Aft. w: %d\n",window->curr_block);
  }      

  //GE_set_curr_realline(wblock->curr_realline);
  //  printf("till_curr_realline: %d\n",wblock->till_curr_realline);
  //ScrollEditorToRealLine(window,wblock,wblock->curr_realline);
}

#else

static STime last_time = 0;

void P2MUpdateSongPosCallBack(void){
	struct Tracker_Windows *window=root->song->tracker_windows;
	NInt curr_block=ATOMIC_GET(root->curr_blocknum);
	struct WBlocks *wblock;
	int till_curr_realline;

        if(scrolls_per_second==-1)
          scrolls_per_second = SETTINGS_read_int("scrolls_per_second", default_scrolls_per_second);

	if(pc->playtype==PLAYSONG)
          BS_SelectPlaylistPos(root->curr_playlist);

	while(window!=NULL){
		if(window->playalong==true){

                  DO_GFX({
                        wblock=ListFindElement1(&window->wblocks->l,curr_blocknum);
			till_curr_realline=ATOMIC_GET(wblock->till_curr_realline);
                        
			if(window->curr_block!=curr_block){
				SelectWBlock(
					window,
					wblock
				);
			}


			//fprintf(stderr,"tilline: %d\n",till_curr_realline);
#if 0
                        if(wblock->curr_realline != till_curr_realline)
                          ScrollEditorToRealLine(window,wblock,till_curr_realline);
#else
                        {
                          bool do_scrolling = false;

                          if(wblock != last_wblock)
                            do_scrolling = true;
                          
                          else if (last_time > ATOMIC_GET(pc->therealtime))
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
                            last_time = ATOMIC_GET(pc->therealtime);
                            last_wblock = wblock;
                          }
                        }
#endif

                    });
        }
		window=NextWindow(window);
}


}

void (*Ptask2MtaskCallBack)(void)= &P2MUpdateSongPosCallBack;


#endif


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

#include <string.h>
#include <math.h>

#include "nsmtracker.h"
#include "placement_proc.h"
#include "list_proc.h"
#include "localzooms_proc.h"
#include "reallines_proc.h"
#include "wtracks_proc.h"
#include "common_proc.h"
#include "tempos_proc.h"
#include "temponodes_proc.h"
#include "blocks_proc.h"
#include "windows_proc.h"
#include "OS_Bs_edit_proc.h"
#include "fxlines_proc.h"
#include "cursor_proc.h"
#include "undo_block_insertdelete_proc.h"
#include "playerclass.h"
#include "player_proc.h"
#include "player_pause_proc.h"
#include "sequencer_proc.h"
#include "visual_proc.h"
#include "Signature_proc.h"
#include "swingtext_proc.h"
#include "../OpenGL/Widget_proc.h"

#include "../api/api_proc.h"

#include "wblocks_proc.h"



extern struct Root *root;
extern PlayerClass *pc;

DEFINE_ATOMIC(int, g_curr_midi_channel) = 0;
DEFINE_ATOMIC(struct Blocks *, g_curr_block) = NULL;


void CloseWBlock(struct Tracker_Windows *window, NInt blocknum){
	struct WBlocks *temp;

	temp=(struct WBlocks *)ListFindElement1(&window->wblocks->l,blocknum);

	if(temp==NULL) return;

	ListRemoveElement1(&window->wblocks,&temp->l);
}



bool WBlock_legalizeStartEndReallines(const struct WBlocks *wblock,int *start_realline,int *end_realline){

  if(*start_realline<wblock->top_realline) *start_realline=wblock->top_realline;
  if(*start_realline<0) *start_realline=0;
  if(*end_realline>=wblock->num_reallines) *end_realline=wblock->num_reallines-1;
  if(*end_realline>wblock->bot_realline) *end_realline=wblock->bot_realline;

  if(*end_realline<*start_realline){
    return false;
  }

  return true;
}


/*********************************************************************
   FUNCTION
     Sentraliced place to set these two important variables. A lot
     of troubles have been discovered because of these two, and
     it was a lot of troubles setting then correct. By having just
     one place where they are set, it will at least be easier to
     once and for all find a proper way to set them. Seems like it
     works now, though.
*********************************************************************/
void SetWBlock_Top_And_Bot_Realline(
	const struct Tracker_Windows *window,
	struct WBlocks *wblock
){
	wblock->bot_realline=
		wblock->curr_realline+(1+wblock->num_visiblelines)/2 - 1;

	/*
	wblock->top_realline=
		wblock->curr_realline+
		(wblock->t.y1 - GetCursorY1Pos(window,wblock))/window->fontheight;
	*/
	wblock->top_realline=wblock->bot_realline-wblock->num_visiblelines + 1;
}


static int GetMaxSignatureWidth(const struct Blocks *block){
  int ret = 3;
  struct Signatures *signature = block->signatures;
  while(signature != NULL){
    char temp[128];
    sprintf(temp,"%d/%d",(int)signature->signature.numerator,(int)signature->signature.denominator);
    ret = R_MAX(ret, (int)strlen(temp));
    signature = NextSignature(signature);
  }
  return ret;
}

static int num_characters_in_num(int num){
  if (num==0)
    return 1;
  
  else if (num<0)
    return ((int)log10(-num)) + 2;

  else
    return ((int)log10(num)) + 1;
}

static void get_highest_bars_and_beats(const struct Blocks *block, int *highest_bar, int *highest_beat){
  const struct Beats *beat = block->beats;
  
  int bar_num=0,beat_num=0;
  
  while(beat!=NULL){
    if (beat->bar_num > bar_num)
      bar_num = beat->bar_num;
    if (beat->beat_num > beat_num)
      beat_num = beat->beat_num;
    
    beat = NextBeat(beat);
  }

  *highest_bar = bar_num;
  *highest_beat = beat_num;
}

static void SetLineNumAreaCoordinates(
                                      const struct Tracker_Windows *window,
                                      struct WBlocks *wblock,
                                      int linenumarea_x1
                                      )
{
  int highest_bar_num = 0;
  int highest_beat_num = 0;
  int highest_linenum = 0;
  int highest_zoomline_num = -1;
  
  if (wblock->reallines == NULL) { // If it's a new block or a newly loaded block, we just set some values. (These values probably don't matter anyway in those situations)
    
    highest_bar_num = 4;
    highest_beat_num = 4;
    highest_linenum = 64;
    highest_zoomline_num = 0;
    
  } else {

    get_highest_bars_and_beats(wblock->block, &highest_bar_num, &highest_beat_num);

    int realline;
    for(realline=0 ; realline < wblock->num_reallines ; realline++){
      const struct LocalZooms *localzoom = wblock->reallines[realline];
      
      if (localzoom->l.p.line > highest_linenum)
        highest_linenum = localzoom->l.p.line;
      
      if (localzoom->level>0 && localzoom->zoomline>0 && localzoom->autogenerated==false)
        if (localzoom->zoomline > highest_zoomline_num)
          highest_zoomline_num = localzoom->zoomline;
    }
  }

  wblock->linenumarea.x = linenumarea_x1;

  wblock->bars_max_num_characters = num_characters_in_num(highest_bar_num);
  wblock->beats_max_num_characters = num_characters_in_num(highest_beat_num);
  wblock->linenumbers_max_num_characters = R_MAX(3, num_characters_in_num(highest_linenum)); // need at least 3 characters to show the LZ value in the header.
  wblock->zoomlines_max_num_characters = highest_zoomline_num==-1 ? 0 : num_characters_in_num(highest_zoomline_num+1);
  
  wblock->beats_x        = wblock->linenumarea.x + wblock->bars_max_num_characters      * window->fontwidth + 4;
  wblock->zoomlines_x    = wblock->beats_x       + wblock->beats_max_num_characters     * window->fontwidth + 4;

  if (linenumbersVisible()) {
    wblock->linenumarea.x2 = wblock->linenumarea.x + wblock->linenumbers_max_num_characters * window->fontwidth;
  } else {
    wblock->linenumarea.x2 = wblock->zoomlines_x   + wblock->zoomlines_max_num_characters * window->fontwidth;
  }
  
  wblock->linenumarea.width = wblock->linenumarea.x2 - wblock->linenumarea.x;
}
                                      

void UpdateWBlockCoordinates(
                             struct Tracker_Windows *window,
                             struct WBlocks *wblock
){
  //    printf("   UpdateWblockCoordinates   curr_track: %d,   wtrack->l.num: %d.  Left: %d\n",window->curr_track,wblock->wtrack==NULL?-1000:wblock->wtrack->l.num,wblock->left_track);
          
  	wblock->signaturearea.width = window->fontwidth*GetMaxSignatureWidth(wblock->block);
    
	wblock->a.x1  = window->leftslider.width; //R_MAX(window->fontwidth+3,window->leftslider.width+1);
	wblock->a.y1  = 0;
	wblock->a.x2 = window->width + 3; // Not quite sure why we have to add 2 here.
	wblock->a.y2 = window->height - window->bottomslider_height - 2;

        
        // Tempocolor
        /////////////////////        
        wblock->tempocolorarea.x  = wblock->a.x1; // + 3;
	wblock->tempocolorarea.x2 = wblock->tempocolorarea.x  + wblock->tempocolorarea.width;

        
        // BPM
        /////////////////////        
        wblock->tempoTypearea.x  = wblock->tempocolorarea.x2 + 3;
	wblock->tempoTypearea.x2 = wblock->tempoTypearea.x   + wblock->tempoTypearea.width;
	wblock->tempoarea.x      = wblock->tempoTypearea.x2  + 3;
	wblock->tempoarea.x2     = wblock->tempoarea.x       + wblock->tempoarea.width;
        int next_x2;

        if (window->show_bpm_track)
          next_x2 = wblock->tempoarea.x2 + 3;
        else
          next_x2 = wblock->tempocolorarea.x2 + 3;
        
        wblock->linearea.y  = wblock->a.y1+(window->systemfontheight*2) + WTRACKS_SPACE*3;
	wblock->linearea.y2 = wblock->a.y2;
        
        // LPB
        /////////////////////
        wblock->lpbTypearea.x  = next_x2;
	wblock->lpbTypearea.x2   = wblock->lpbTypearea.x     + wblock->lpbTypearea.width;
	wblock->lpbarea.x        = wblock->lpbTypearea.x2    + 3;
	wblock->lpbarea.x2       = wblock->lpbarea.x         + wblock->lpbarea.width;
        if (window->show_lpb_track)
          next_x2 = wblock->lpbarea.x2 + 3;

        // Signature
        /////////////////////
	wblock->signaturearea.x        = next_x2;
	wblock->signaturearea.x2       = wblock->signaturearea.x + wblock->signaturearea.width;
        if (window->show_signature_track)
          next_x2 = wblock->signaturearea.x2 + 3;

        
        // Bars/beats
        /////////////////////
        SetLineNumAreaCoordinates(window, wblock, next_x2);
        next_x2 = wblock->linenumarea.x2 + 1;

        
        // SWING
        /////////////////////
        if (window->show_swing_track){
          wblock->swingtext_fits_reallines = swingtext_fits_reallines(wblock, wblock->block->filledout_swings.array);
          if (wblock->swingtext_fits_reallines)
            wblock->swingTypearea.width  = 0;
          else
            wblock->swingTypearea.width  = window->fontwidth;
        }
        wblock->swingTypearea.x  = next_x2;
	wblock->swingTypearea.x2 = wblock->swingTypearea.x     + wblock->swingTypearea.width;
	wblock->swingarea.x      = wblock->swingTypearea.x2    ; // + 3;
	wblock->swingarea.x2     = wblock->swingarea.x         + wblock->swingarea.width;
        if (window->show_swing_track){
          next_x2 = wblock->swingarea.x2 + 3;
        }
        
        // Temponode
        /////////////////////////
        wblock->temponodearea.x = next_x2 + 2;
	wblock->temponodearea.x2 = wblock->temponodearea.x   + wblock->temponodearea.width;
        if (window->show_reltempo_track)
          next_x2 = wblock->temponodearea.x2 + 2;
        

        wblock->t.x1 = next_x2;
	wblock->t.x2 = wblock->a.x2;
	wblock->t.y1 = wblock->linearea.y + 2;
	wblock->t.y2 = wblock->a.y2;


        wblock->bottombar.x1 = wblock->t.x1 + 1;
        wblock->bottombar.y1 = wblock->a.y2-1;
        wblock->bottombar.x2 = wblock->a.y2-1;
        wblock->bottombar.y2 = window->height;

        int midi_record_button_width = wblock->bottombar.y2-wblock->bottombar.y1 + 2;
        
	wblock->reltempo.x1=midi_record_button_width;
	wblock->reltempo.y1=wblock->bottombar.y1+2;
	wblock->reltempo.x2=wblock->bottombar.x1 - 1;
	wblock->reltempo.y2=wblock->bottombar.y2-1;

        {
          int m_width = midi_record_button_width;
          int r_width = wblock->reltempo.x2 - wblock->reltempo.x1;
          if (m_width > r_width){
            wblock->reltempo.x1 = R_MAX(1, (wblock->t.x1 - 3) / 2);
          }
        }
        
	wblock->num_visiblelines=(wblock->t.y2-wblock->t.y1)/window->fontheight;
	if((wblock->num_visiblelines-2)*window->fontheight+wblock->t.y1>=wblock->t.y2){
		wblock->num_visiblelines--;
	}

	SetWBlock_Top_And_Bot_Realline(window,wblock);

	UpdateAllWTracksCoordinates(window,wblock);

        GFX_PositionUpperLeftArea(window, wblock);
        
        if (window->wblock==wblock)
          ValidateCursorPos(window);
}

void UpdateAllWBlockCoordinates(
	struct Tracker_Windows *window
){
	struct WBlocks *wblock=window->wblocks;

	while(wblock!=NULL){
		UpdateWBlockCoordinates(window,wblock);
		wblock=NextWBlock(wblock);
	}
}

void UpdateWBlockWidths(struct Tracker_Windows *window,struct WBlocks *wblock){
  wblock->linenumarea.width    = window->fontwidth*R_MAX(3, num_characters_in_num(wblock->block->num_lines));

  //SetZoomLevelAreaWidth(window,wblock);

	wblock->tempocolorarea.width = window->fontwidth*5/2;
	//wblock->signatureTypearea.width    = 0; //window->fontwidth;
	wblock->signaturearea.width        = window->fontwidth*GetMaxSignatureWidth(wblock->block);
	wblock->lpbTypearea.width    = window->fontwidth;
	wblock->lpbarea.width        = window->fontwidth*2;
	wblock->tempoTypearea.width  = window->fontwidth;
	wblock->tempoarea.width      = window->fontwidth*4;

        if (window->show_swing_track){
          wblock->swingtext_fits_reallines = swingtext_fits_reallines(wblock, wblock->block->filledout_swings.array);
          if (wblock->swingtext_fits_reallines)
            wblock->swingTypearea.width  = 0;
          else
            wblock->swingTypearea.width  = window->fontwidth;
        }else
          wblock->swingTypearea.width  = window->fontwidth;

        wblock->swingarea.width      = window->fontwidth*3;
        if(wblock->temponodearea.width==0)
          wblock->temponodearea.width  = window->fontwidth*7;
}

void UpdateAllWBlockWidths(struct Tracker_Windows *window){
	struct WBlocks *wblock;
	wblock=window->wblocks;
	while(wblock!=NULL){
		UpdateWBlockWidths(window,wblock);
		wblock=NextWBlock(wblock);
	}
}

void NewWBlock(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct Blocks *block
){

	wblock->block=block;
	wblock->l.num=block->l.num;

	wblock->num_reallines=wblock->block->num_lines;
        wblock->num_expand_lines = 1;

	wblock->tempotrackonoff=1;

        wblock->rangey1 = p_Create(0,0,1);
        wblock->rangey2 = p_Create(0,0,1);

        UpdateWTracks(window,wblock);

        // do this as early as possible. wblock->reallines is used lots of places.
        NewLocalZooms(window,wblock);
	UpdateRealLines(window,wblock);
        

	//wblock->zoomlinearea.width = 0;
	UpdateWBlockWidths(window,wblock);

	//wblock->left_subtrack= -1;

	wblock->reltempomax=2.0;

	UpdateWBlockCoordinates(window,wblock);
	wblock->wtrack=wblock->wtracks;
	UpdateAllWTracksCoordinates(window,wblock);

	//UpdateWTempos(window,wblock);
	//UpdateWLPBs(window,wblock);
#if !USE_OPENGL
	UpdateWTempoNodes(window,wblock);
#endif
	wblock->isgfxdatahere=true;

	ListAddElement1(&window->wblocks,&wblock->l);
}


/*
  FUNCTION
     Make the WBlocks list the same as the Blocks list
     for window 'window'. Can be called after new window
     has been made, or _one_ block has been added.
*/

void UpdateWBlocks(struct Tracker_Windows *window){
	struct WBlocks *wblock=window->wblocks;
	struct Blocks *block=root->song->blocks;

	struct WBlocks *new;

	while(block!=NULL){
		if(wblock==NULL){
			wblock=talloc(sizeof(struct WBlocks));
			NewWBlock(window,wblock,block);
		}

		if(block->l.num!=wblock->l.num){
			new=talloc(sizeof(struct WBlocks));
			NewWBlock(window,new,(struct Blocks *)block);
			return;
		}

		block=NextBlock(block);
		wblock=NextWBlock(wblock);
	}
}


void SelectWBlock(struct Tracker_Windows *window,struct WBlocks *wblock, bool force_select){
        
      if(wblock==NULL) return;

      R_ASSERT_RETURN_IF_FALSE(window!=NULL);

      if (!force_select)
        if (EDITOR_is_legal_track(window, window->curr_track))
          if (window->wblock==wblock && window->curr_block==wblock->l.num){
            //window->must_redraw=true; // At least do this. Just in case. (should probably never return here)
            return;
          }
      
      EVENTLOG_add_event("SelectWBlock 1");
      
      g_assert_not_stopping_player++; // A lot of things happens here. Assert that we are not stopping the player.
      
      
      ATOMIC_WRITE(window->wblock, wblock);
      if( ! wblock->isgfxdatahere){
#if !USE_OPENGL
        UpdateWTempoNodes(window,wblock);
        UpdateAllFXNodeLines(window,wblock);
#endif              
        wblock->isgfxdatahere=true;
      }

      if(wblock->curr_realline>=wblock->num_reallines){
        wblock->curr_realline=wblock->num_reallines-1;
      }
      
      /*
        if (window->curr_track < 0){
        ATOMIC_WRITE(wblock->wtrack, wblock->wtracks);
        } else {
          if (window->curr_track >= ListFindNumElements1(&wblock->wtracks->l))
          ATOMIC_WRITE(window->curr_track, ListFindNumElements1(&wblock->wtracks->l)-1);
          ATOMIC_WRITE(wblock->wtrack, ListFindElement1(&wblock->wtracks->l,window->curr_track));
          }
      */

      R_ASSERT_RETURN_IF_FALSE(wblock->wtrack!=NULL);

      EVENTLOG_add_event("SelectWBlock 2");
      
      NInt newcurrtrack=wblock->wtrack->l.num;
      int newcurrtracksub=window->curr_track_sub;
      
      window->curr_track_sub=-1;
      ATOMIC_WRITE(window->curr_track, newcurrtrack);
      
      SetCursorPosConcrete(window,wblock,newcurrtrack,newcurrtracksub);
      
      //if (SetCursorPosConcrete(window,wblock,newcurrtrack,newcurrtracksub)==false)
      //  ATOMIC_WRITE(window->curr_track, 0);
      
      window->curr_block=wblock->l.num;
      ATOMIC_SET(g_curr_block, wblock->block);
      
      //printf("   curr_track: %d,   wtrack->l.num: %d\n",window->curr_track,wblock->wtrack->l.num);
      
      //MinimizeBlock_CurrPos(window);
      //window->must_redraw = false;
      
      //window->must_redraw = true;

      EVENTLOG_add_event("SelectWBlock 3");
      
      BS_SelectBlock(wblock->block);
      
      if(false==is_playing() && false==PlayerIsCurrentlyPlayingLoop()){
        R_ASSERT_RETURN_IF_FALSE(wblock->wtrack->track != NULL);
        GFX_update_instrument_patch_gui(wblock->wtrack->track->patch);
      }
      
      //window->must_redraw = false;
      //MinimizeBlock_CurrPos(window);
      //wblock->block->is_dirty = true;
      
      GE_set_curr_realline(wblock->curr_realline);

      EVENTLOG_add_event("SelectWBlock 4");
      
      SEQUENCER_update(SEQUPDATE_TIME);
      
      window->must_redraw = true;

      g_assert_not_stopping_player--;
}

/*
void SelectPrevWBlock(struct Tracker_Windows *window){
  EVENTLOG_add_event("SelectPrevWBlock");
  BS_SelectBlocklistPos(BS_GetCurrBlocklistPos()-1);
  //SelectWBlock(window,ListPrevElement1(&window->wblocks->l,&window->wblock->l));
}

void SelectNextWBlock(struct Tracker_Windows *window){
  BS_SelectBlocklistPos(BS_GetCurrBlocklistPos()+1);
  //SelectWBlock(window,NextWBlock(window->wblock));
}
*/

void SelectPrevPlaylistWBlock(struct Tracker_Windows *window, bool change_song_pos_too){
  /*
  struct SeqBlock *seqblock = BS_GetPrevPlaylistBlock();
  if (seqblock==NULL)
    return;
  */
  
  BS_SelectPlaylistPos(getCurrPlaylistPos()-1, change_song_pos_too);
}

void SelectNextPlaylistWBlock(struct Tracker_Windows *window, bool change_song_pos_too){
  /*
  struct SeqBlock *seqblock = BS_GetNextPlaylistBlock();
  if (seqblock==NULL)
    return;
  */
  
  BS_SelectPlaylistPos(getCurrPlaylistPos()+1, change_song_pos_too);
}

extern size_t allocated;

struct Blocks *AppendWBlock(struct Tracker_Windows *window){

	ADD_UNDO(Block_Insert(root->song->num_blocks));

        struct Blocks *ret;
        
        PC_Pause();{
          ret = AppendBlock();
          UpdateWBlocks(window);
          SelectWBlock(
                       window,
                       (struct WBlocks *)ListLast1(&window->wblocks->l),
                       true
                       );
          BS_UpdateBlockList();
        }PC_StopPause(NULL);

        return ret;
}

void AppendWBlock_spes(struct Tracker_Windows *window,int num_lines,NInt num_tracks){

	ADD_UNDO(Block_Insert(root->song->num_blocks));

        PC_Pause();{
          AppendBlock_spes(num_lines,num_tracks);
          UpdateWBlocks(window);
          SelectWBlock(
                       window,
                       (struct WBlocks *)ListLast1(&window->wblocks->l),
                       true
                       );
          BS_UpdateBlockList();
        }PC_StopPause(NULL);
}
















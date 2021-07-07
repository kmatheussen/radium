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
#include "TimeData.hpp"
#include "block_properties_proc.h"
#include "undo_blocks_proc.h"
#include "clipboard_track_cut_proc.h"
#include "clipboard_track_copy_proc.h"
#include "clipboard_track_paste_proc.h"
#include "fxlines_proc.h"
#include "list_proc.h"
#include "vector_proc.h"
#include "windows_proc.h"
#include "cursor_proc.h"
#include "player_proc.h"
#include "player_pause_proc.h"

#include "../api/api_proc.h"

#include "track_insert_proc.h"


void DeleteTracks(
                  struct Tracker_Windows *window,
                  struct WBlocks *wblock,
                  NInt tracknum,
                  NInt todelete
                  )
{
	struct Blocks *block=wblock->block;

        if(tracknum>=block->num_tracks+1 || tracknum<0) return;
                
	if(tracknum-todelete>=block->num_tracks){
	  todelete=block->num_tracks-tracknum;
	}

        PC_Pause();
        {
          
          if(block->num_tracks==1){
            CB_CutTrack_CurrPos(window);
            goto exit;
          }
          
          int new_num_tracks=R_MAX(tracknum, block->num_tracks-todelete);
          
          fprintf(stderr,"delete track. Curr: %d, new_num_tracks: %d, todelete: %d\n",tracknum,new_num_tracks,todelete);
          
          for(int lokke=tracknum ; lokke<new_num_tracks-1+todelete;lokke++){
            
            if (lokke+todelete >= block->num_tracks)
              break;
            
            struct WTracks *wtrack=CB_CopyTrack(
                                                wblock,
                                                (struct WTracks*)ListFindElement1(&wblock->wtracks->l,lokke+todelete)
                                                );
            co_CB_PasteTrack(
                             wblock,
                             wtrack,
                             (struct WTracks*)ListFindElement1(&wblock->wtracks->l,lokke)
                             );
          }
	
          Block_Set_num_tracks(block,new_num_tracks);
          
        }
        
 exit:
        PC_StopPause(NULL);
}



void InsertTracks(
                  struct Tracker_Windows *window,
                  struct WBlocks *wblock,
                  NInt tracknum,
                  NInt toinsert
){
	NInt lokke;
	NInt num_tracks;
	struct Blocks *block=wblock->block;
	struct WTracks *wtrack;
	struct Tracks *track;

	if(tracknum>=block->num_tracks+1 || tracknum<0) return;

	if(toinsert<=0){
          if(toinsert<0) DeleteTracks(window,wblock,tracknum,-toinsert);
          return;
	}

	num_tracks=block->num_tracks+toinsert;

        PC_Pause();{
          Block_Set_num_tracks(block,num_tracks);

          for(lokke=num_tracks-1;lokke>=tracknum+toinsert;lokke--){
            wtrack=CB_CopyTrack(
                                wblock,
                                (struct WTracks*)ListFindElement1(&wblock->wtracks->l,lokke-toinsert)
                                );
            co_CB_PasteTrack(
                             wblock,
                             wtrack,
                             (struct WTracks*)ListFindElement1(&wblock->wtracks->l,lokke)
                             );
          }
          
          for(lokke=tracknum;lokke<tracknum+toinsert;lokke++){
            wtrack=(struct WTracks*)ListFindElement1(&wblock->wtracks->l,lokke);
            track=wtrack->track;
            
            track->notes=NULL;
            //track->stops=NULL;
            r::StopTimeData::Writer(track->stops2).clear();
            VECTOR_clean(&track->fxs);
            track->patch=NULL;
          }
        }PC_StopPause(NULL);
}


void InsertTracks_CurrPos(
	struct Tracker_Windows *window,
	NInt toinsert
){
	struct WBlocks *wblock;
	NInt curr_track;

	curr_track=window->curr_track;
	if(curr_track<0) return;

	ADD_UNDO(Block_CurrPos(window));

	wblock=window->wblock;

	InsertTracks(window,wblock,curr_track,toinsert);

	if(curr_track>=wblock->block->num_tracks){
		curr_track=wblock->block->num_tracks-1;
	}

        setCurrentTrack(0, -1, window->l.num);
        setCurrentTrack(curr_track, -1, window->l.num);
          
	window->must_redraw = true;
}







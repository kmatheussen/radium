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
#include "block_properties_proc.h"
#include "undo_blocks_proc.h"
#include "clipboard_track_cut_proc.h"
#include "clipboard_track_copy_proc.h"
#include "clipboard_track_paste_proc.h"
#include "fxlines_proc.h"
#include "list_proc.h"
#include "windows_proc.h"
#include "cursor_proc.h"
#include "player_proc.h"

#include "track_insert_proc.h"


/* Private function. Should only be called by InsertTracks. */

static void DeleteTracks(
                         struct Tracker_Windows *window,
                         struct WBlocks *wblock,
                         NInt tracknum,
                         NInt todelete
                         ){
	NInt lokke;
	NInt num_tracks;
	struct Blocks *block=wblock->block;
	struct WTracks *wtrack;

	if(tracknum-todelete>=block->num_tracks){
	  todelete=block->num_tracks-tracknum;
	}
	
	if(block->num_tracks==1){
          CB_CutTrack_CurrPos(window);
          return;
        }

	num_tracks=block->num_tracks-todelete;
	
	fprintf(stderr,"delete track. Curr: %d, num_tracks: %d, todelete: %d\n",tracknum,num_tracks,todelete);

	for(lokke=tracknum ; lokke<num_tracks-1+todelete;lokke++){
	  wtrack=CB_CopyTrack(
			      wblock,
			      ListFindElement1(&wblock->wtracks->l,lokke+todelete)
			      );
	  CB_PasteTrack(
			wblock,
			wtrack,
			ListFindElement1(&wblock->wtracks->l,lokke)
			);
	}
	
	Block_Set_num_tracks(block,num_tracks);

}



/* Not private function. toinsert may be negative. */

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

	Block_Set_num_tracks(block,num_tracks);

	for(lokke=num_tracks-1;lokke>=tracknum+toinsert;lokke--){
		wtrack=CB_CopyTrack(
			wblock,
			ListFindElement1(&wblock->wtracks->l,lokke-toinsert)
		);
		CB_PasteTrack(
			wblock,
			wtrack,
			ListFindElement1(&wblock->wtracks->l,lokke)
		);
	}

	for(lokke=tracknum;lokke<tracknum+toinsert;lokke++){
		wtrack=ListFindElement1(&wblock->wtracks->l,lokke);
		track=wtrack->track;

		track->notes=NULL;
		track->stops=NULL;
		track->fxs=NULL;
		track->patch=NULL;
	}
}


void InsertTracks_CurrPos(
	struct Tracker_Windows *window,
	NInt toinsert
){
	struct WBlocks *wblock;
	NInt curr_track;

	PlayStop();

	curr_track=window->curr_track;
	if(curr_track<0) return;

	Undo_Block_CurrPos(window);

	wblock=window->wblock;

	InsertTracks(window,wblock,curr_track,toinsert);

	if(curr_track>=wblock->block->num_tracks){
		curr_track=wblock->block->num_tracks-1;
	}

	SetCursorPosConcrete(window,wblock,0,-1);

#if !USE_OPENGL
	UpdateAllFXNodeLines(window,wblock);
#endif
	SetCursorPosConcrete(window,wblock,curr_track,-1);

	window->must_redraw = true;

}







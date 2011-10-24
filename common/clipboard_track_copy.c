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
#include "clipboard_range_copy_proc.h"
#include "clipboard_range_calc_proc.h"
#include <string.h>
#include "placement_proc.h"
#include "clipboard_tempos_copy_proc.h"

#include "clipboard_track_copy_proc.h"




struct WTracks *cb_wtrack=NULL;

extern struct LPBs *cb_lpb;
extern struct Tempos *cb_tempo;
extern struct TempoNodes *cb_temponode;


struct WTracks *CB_CopyTrack(
	struct WBlocks *wblock,
	struct WTracks *wtrack
){
	Place *p1,p2;

	struct WTracks *towtrack;
	struct Tracks *totrack;
	struct Tracks *track=wtrack->track;

	towtrack=talloc(sizeof(struct WTracks));
	memcpy(towtrack,wtrack,sizeof(struct WTracks));

	towtrack->track=totrack=talloc(sizeof(struct Tracks));
	memcpy(totrack,track,sizeof(struct Tracks));

	towtrack->track->trackname=talloc_atomic(strlen(wtrack->track->trackname)+1);
	memcpy(towtrack->track->trackname,wtrack->track->trackname,strlen(wtrack->track->trackname)+1);

	p1=PlaceGetFirstPos();
	PlaceSetLastPos(wblock->block,&p2);

	totrack->notes=NULL;
	totrack->stops=NULL;
	totrack->fxs=NULL;

	CopyRange_notes(&totrack->notes,track->notes,p1,&p2);
	CopyRange_stops(&totrack->stops,track->stops,p1,&p2);
	CopyRange_fxs(&totrack->fxs,track->fxs,p1,&p2);

	return towtrack;
}


void CB_CopyTrack_CurrPos(
	struct Tracker_Windows *window
){
	struct WBlocks *wblock=window->wblock;
	struct Blocks *block=wblock->block;
	struct WTracks *wtrack=wblock->wtrack;

	switch(window->curr_track){
		case LPBTRACK:
			cb_lpb=CB_CopyLPBs(block->lpbs);
			break;
		case TEMPOTRACK:
			cb_tempo=CB_CopyTempos(block->tempos);
			break;
		case TEMPONODETRACK:
			cb_temponode=CB_CopyTempoNodes(block->temponodes);
			break;
		default:
			cb_wtrack=CB_CopyTrack(wblock,wtrack);
			break;
	}
}




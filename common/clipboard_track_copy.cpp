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



#include "nsmtracker.h"
#include "TimeData.hpp"
#include "FX.hpp"
#include "vector_proc.h"
#include "clipboard_range_copy_proc.h"
#include "clipboard_range_calc_proc.h"
#include "placement_proc.h"
#include "clipboard_tempos_copy_proc.h"
#include "wtracks_proc.h"
#include "tracks_proc.h"
#include "fxtext_proc.h"
#include "swingtext_proc.h"

#include "clipboard_track_copy_proc.h"



//bool cb_wtrack_only_contains_one_fxs = false; // (moved into struct WTracks to avoid keeping track of whether the value is valid all the time)
struct WTracks *cb_wtrack=NULL;

extern struct Swing *cb_swing;
extern struct Signatures *cb_signature;
extern struct LPBs *cb_lpb;
extern struct Tempos *cb_tempo;
extern struct TempoNodes *cb_temponode;

/*
struct FXs *cb_fxs=NULL;
struct FXs *CB_CopyFX(
                      struct WTracks *wtrack
                      )
{
}
*/

// Also used in clipboard_track_cut.c
struct WTracks *internal_copy_track(
	const struct WBlocks *wblock,
	const struct WTracks *wtrack,
	const bool only_copy_current_fx_if_possible
	)
{
	struct WTracks *towtrack;
	struct Tracks *totrack;
	struct Tracks *track=wtrack->track;

	towtrack=WTRACK_new();
	memcpy(towtrack,wtrack,sizeof(struct WTracks));

	towtrack->track = totrack = TRACK_create(track->l.num);
        {
          auto *stops = totrack->stops2;
          memcpy(totrack,track,sizeof(struct Tracks));
          totrack->stops2 = stops;
        }
        
        towtrack->track->trackname=talloc_strdup(wtrack->track->trackname);

	const Place *p1=PlaceGetFirstPos();
        Place p2;
	PlaceSetLastPos(wblock->block,&p2);

	totrack->notes = NULL;
        totrack->swings=NULL;
        memset(&totrack->fxs, 0, sizeof(vector_t));

        struct FXs *fxs = NULL;
        int subsubtrack = -1;

	if (only_copy_current_fx_if_possible)
	{
		subsubtrack = FXTEXT_subsubtrack(root->song->tracker_windows, wtrack, &fxs);

		if (subsubtrack >= 0)
			R_ASSERT(fxs != NULL);
		else
			R_ASSERT(fxs == NULL);
	}
	
        if (fxs==NULL)
	{
		// When !only_copy_current_fx_if_possible or subsubtrack < 0.
		
		CopyRange_notes(&totrack->notes,track->notes,p1,&p2);
		CopyRange_stops(totrack->stops2,track->stops2,p1,&p2);
		totrack->swings = CB_CopySwings(track->swings, &p2);
		CopyRange_fxs(wblock->block->num_lines, &totrack->fxs,&track->fxs, make_ratio(0,1), make_ratio(wblock->block->num_lines, 1));
	}
	else
	{
		vector_t fxss = {};
		VECTOR_push_back(&fxss, fxs);
		CopyRange_fxs(wblock->block->num_lines, &totrack->fxs,&fxss, make_ratio(0,1), make_ratio(wblock->block->num_lines, 1));
		
		towtrack->cb_wtrack_only_contains_one_fxs = true;
        }
        
	return towtrack;
}

struct WTracks *CB_CopyTrack(
                             const struct WBlocks *wblock,
                             const struct WTracks *wtrack
                             )
{
	struct WTracks *ret = internal_copy_track(wblock, wtrack, false);
	return ret;
}

void CB_CopyTrack_CurrPos(
                          const struct Tracker_Windows *window
){
	const struct WBlocks *wblock=window->wblock;
	const struct Blocks *block=wblock->block;
	const struct WTracks *wtrack=wblock->wtrack;

	switch(window->curr_track)
	{
		case SWINGTRACK:
                        cb_swing=CB_CopySwings(block->swings, NULL);
			break;
		case SIGNATURETRACK:
			cb_signature=CB_CopySignatures(block->signatures);
			break;
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
			if (SWINGTEXT_subsubtrack(window, wtrack) != -1){
				cb_swing = CB_CopySwings(wtrack->track->swings,NULL);
			} else {
				cb_wtrack=internal_copy_track(wblock, wtrack, true);
			}
			break;
	}
}



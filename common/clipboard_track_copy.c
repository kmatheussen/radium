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
#include "vector_proc.h"
#include "clipboard_range_copy_proc.h"
#include "clipboard_range_calc_proc.h"
#include "placement_proc.h"
#include "clipboard_tempos_copy_proc.h"
#include "wtracks_proc.h"
#include "fxtext_proc.h"

#include "clipboard_track_copy_proc.h"



bool cb_wtrack_only_contains_one_fxs = false;
struct WTracks *cb_wtrack=NULL;

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


struct WTracks *internal_copy_track(
	struct WBlocks *wblock,
	struct WTracks *wtrack,
        bool always_copy_all_fxs,
        bool *only_one_fxs_was_copied
){
  if (!always_copy_all_fxs)
    R_ASSERT(only_one_fxs_was_copied!=NULL);
  
	Place *p1,p2;

	struct WTracks *towtrack;
	struct Tracks *totrack;
	struct Tracks *track=wtrack->track;

	towtrack=WTRACK_new();
	memcpy(towtrack,wtrack,sizeof(struct WTracks));

	towtrack->track = totrack = tcopy(track, sizeof(struct Tracks));

        // Null out some data we don't need so it can be GC-ed.
#if !USE_OPENGL
        towtrack->wfxnodes = NULL;
        towtrack->wpitches = NULL;
#endif

        towtrack->track->trackname=talloc_strdup(wtrack->track->trackname);

	p1=PlaceGetFirstPos();
	PlaceSetLastPos(wblock->block,&p2);

	totrack->notes=NULL;
	totrack->stops=NULL;
        memset(&totrack->fxs, 0, sizeof(vector_t));

        struct FXs *fxs;
        int subsubtrack = FXTEXT_subsubtrack(root->song->tracker_windows, wtrack, &fxs);

        if (always_copy_all_fxs || subsubtrack==-1) {
          
          CopyRange_notes(&totrack->notes,track->notes,p1,&p2);
          CopyRange_stops(&totrack->stops,track->stops,p1,&p2);
          CopyRange_fxs(&totrack->fxs,&track->fxs,p1,&p2);
          if (only_one_fxs_was_copied != NULL)
            *only_one_fxs_was_copied = false;
          
        } else{
          vector_t fxss = {0};
          VECTOR_push_back(&fxss, fxs);
          CopyRange_fxs(&totrack->fxs,&fxss,p1,&p2);
          *only_one_fxs_was_copied = true;
        }
        
	return towtrack;
}

struct WTracks *CB_CopyTrack(
                             struct WBlocks *wblock,
                             struct WTracks *wtrack
                             )
{
  return internal_copy_track(wblock, wtrack, true, NULL);
}

void CB_CopyTrack_CurrPos(
	struct Tracker_Windows *window
){
	struct WBlocks *wblock=window->wblock;
	struct Blocks *block=wblock->block;
	struct WTracks *wtrack=wblock->wtrack;

	switch(window->curr_track){
		case SIGNATURETRACK:
			cb_signature=CB_CopySignatures(block->lpbs);
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
                        cb_wtrack=internal_copy_track(wblock, wtrack, false, &cb_wtrack_only_contains_one_fxs);
			break;
	}
}




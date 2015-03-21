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
#include "clipboard_track_copy_proc.h"
#include "wtracks_proc.h"
#include "clipboard_tempos_copy_proc.h"
#include "time_proc.h"
#include "temponodes_proc.h"
#include "temponodes_legalize_proc.h"
#include "tempos_proc.h"
#include "gfx_wblocks_proc.h"
#include "undo_tracks_proc.h"
#include "undo_tempos_proc.h"
#include "undo_temponodes_proc.h"
#include "player_proc.h"
#include "windows_proc.h"
#include "notes_proc.h"

#include "clipboard_track_cut_proc.h"

extern struct FXs *cb_fxs;

void CB_CutFx_Force(
	struct WBlocks *wblock,
	struct WTracks *wtrack
){
	struct FXs *fxs=wtrack->track->fxs;

	cb_wtrack=CB_CopyTrack(wblock,wtrack);
	wtrack->track->notes=NULL;
	wtrack->track->stops=NULL;

	while(fxs!=NULL){
		(*fxs->fx->closeFX)(fxs->fx,wtrack->track);
		fxs=NextFX(fxs);
	}

	wtrack->track->fxs=NULL;
}

void CB_CutTrack(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack
){
	struct FXs *fxs=wtrack->track->fxs;

	cb_fxs=CB_CopyFxs(wblock,wtrack);

	while(fxs!=NULL){
		(*fxs->fx->closeFX)(fxs->fx,wtrack->track);
		fxs=NextFX(fxs);
	}

	wtrack->track->fxs=NULL;
}

void CB_CutTrack_CurrPos(
	struct Tracker_Windows *window
){
	struct WBlocks *wblock=window->wblock;
	struct Blocks *block=wblock->block;
	struct WTracks *wtrack=wblock->wtrack;

	PlayStop();

        Undo_Track_CurrPos(window);

        if (wtrack != NULL)
          cb_fxs = CB_CutFx(wblock, wtrack);

        window->must_redraw=true;
}



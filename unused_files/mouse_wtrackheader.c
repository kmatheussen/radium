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
#include "tbox_proc.h"
#include "gfx_wtrackheader_volpan_proc.h"
#include "undo_trackheader_proc.h"
#include "gfx_statusbar_proc.h"
#include "player_proc.h"
#include "list_proc.h"
#include "patch_proc.h"
#include "trackreallines_proc.h"
#include "gfx_wtracks_proc.h"
#include "tracks_proc.h"

#include "mouse_wtrackheader_proc.h"

static void update_pan_status(struct Tracker_Windows *window,
                              struct WBlocks *wblock,
                              struct WTracks *wtrack)
{
  GFX_SetChangeInt(window,wblock,"Track Pan",wtrack->track->pan);
  GFX_DrawStatusBar(window,wblock);

  if(TRACK_has_peaks(wtrack->track)==true){
    UpdateTrackReallines(window,window->wblock,wtrack);
#if !USE_OPENGL
    DrawUpWTrack(window,window->wblock,wtrack);
#endif
  }
}

static void update_volume_status(struct Tracker_Windows *window,
                              struct WBlocks *wblock,
                              struct WTracks *wtrack)
{
  GFX_SetChangeInt(window,wblock,"Track RelVolume",wtrack->track->volume);
  GFX_DrawStatusBar(window,wblock);

  if(TRACK_has_peaks(wtrack->track)==true){
    UpdateTrackReallines(window,wblock,wtrack);
#if !USE_OPENGL
    DrawUpWTrack(window,wblock,wtrack);
#endif
  }
}

static int MoveWTrackPan_Mouse(
	struct Tracker_Windows *window,
	float x,float y
){
	struct WBlocks *wblock=window->wblock;
	struct WTracks *wtrack=(struct WTracks *)window->prevaction.pointer1;

	if(isInList1(&wblock->wtracks->l,&wtrack->l)==false) return 0;

	wtrack->track->pan=R_BOUNDARIES(
		-MAXTRACKPAN,
		2*MAXTRACKPAN*(x-wtrack->pan.x1)/((wtrack->pan.x2-wtrack->pan.x1)) - MAXTRACKPAN,
		MAXTRACKPAN
	);

	if(wtrack->track->panonoff && wtrack->track->patch!=NULL){
		(*wtrack->track->patch->changeTrackPan)(wtrack->track->pan,wtrack->track);
	}

        update_pan_status(window,wblock,wtrack);

	UpdatePanSlider(window,wblock,wtrack);

	return 0;
}



static int MoveWTrackVolume_Mouse(
	struct Tracker_Windows *window,
	float x,float y
){
	struct WBlocks *wblock=window->wblock;
	struct WTracks *wtrack=(struct WTracks *)window->prevaction.pointer1;

	if(isInList1(&wblock->wtracks->l,&wtrack->l)==false) return 0;

	wtrack->track->volume=R_BOUNDARIES(
		0,
		MAXTRACKVOL*(x-wtrack->volume.x1)/(wtrack->volume.x2-wtrack->volume.x1),
		MAXTRACKVOL
	);

        update_volume_status(window,wblock,wtrack);

	UpdateVolumeSlider(window,wblock,wtrack);

	return 0;
}




static void SetMouseActionPanSlider(
	struct Tracker_Windows *window,
	struct MouseAction *action,
	struct WTracks *wtrack,
	int x,
	int click
){
	void *temp;

	action->action=PANSLIDER;
	if(click==0){
          update_pan_status(window,window->wblock,wtrack);
          return;
        }

	Undo_TrackHeader(
		window,
		window->wblock->block,
		wtrack->track,
		window->wblock->curr_realline
	);

	action->pointer1=wtrack;
	action->MouseUpFunction=&MoveWTrackPan_Mouse;

	temp=window->prevaction.pointer1;
	window->prevaction.pointer1=wtrack;
	MoveWTrackPan_Mouse(window,x,0);
	window->prevaction.pointer1=temp;

}




static void SetMouseActionVolumeSlider(
	struct Tracker_Windows *window,
	struct MouseAction *action,
	struct WTracks *wtrack,
	int x,
	int click
){
	void *temp;

	action->action=VOLUMESLIDER;
	if(click==0){
          update_volume_status(window,window->wblock,wtrack);
          return;
        }

	Undo_TrackHeader(
		window,
		window->wblock->block,
		wtrack->track,
		window->wblock->curr_realline
	);

	action->pointer1=wtrack;
	action->MouseUpFunction=&MoveWTrackVolume_Mouse;

	temp=window->prevaction.pointer1;
	window->prevaction.pointer1=wtrack;
	MoveWTrackVolume_Mouse(window,x,0);
	window->prevaction.pointer1=temp;
}




void SetMouseActionWTrackHeader(
	struct Tracker_Windows *window,
	struct MouseAction *action,
	struct WTracks *wtrack,
	int x,int y,
	int click
){
  return;
  
  if(insideTBox(&wtrack->pan,x,y)){
		SetMouseActionPanSlider(window,action,wtrack,x,click);
		return;
	}
	if(insideTBox(&wtrack->volume,x,y)){
		SetMouseActionVolumeSlider(window,action,wtrack,x,click);
		return;
	}
	if(click==0) return;

        if(y<window->fontheight){
          printf("gakk %d\n",(int)wtrack->l.num);
          PATCH_select_patch_for_track(window,wtrack,true);
          return;
        }

	if(insideTBox(&wtrack->panonoff,x,y)){
		Undo_TrackHeader(
			window,
			window->wblock->block,
			wtrack->track,
			window->wblock->curr_realline
		);
		wtrack->track->panonoff=wtrack->track->panonoff?false:true;
		if(wtrack->track->panonoff && wtrack->track->patch!=NULL){
			(*wtrack->track->patch->changeTrackPan)(wtrack->track->pan,wtrack->track);
		}
		UpdatePanSlider(window,window->wblock,wtrack);

                if(TRACK_has_peaks(wtrack->track)==true){
                  UpdateTrackReallines(window,window->wblock,wtrack);
#if !USE_OPENGL
                  DrawUpWTrack(window,window->wblock,wtrack);
#endif
                }

		return;
	}
	if(insideTBox(&wtrack->volumeonoff,x,y)){
		Undo_TrackHeader(
			window,
			window->wblock->block,
			wtrack->track,
			window->wblock->curr_realline
		);
		wtrack->track->volumeonoff=wtrack->track->volumeonoff?false:true;
		UpdateVolumeSlider(window,window->wblock,wtrack);

                if(TRACK_has_peaks(wtrack->track)==true){
                  UpdateTrackReallines(window,window->wblock,wtrack);
#if !USE_OPENGL
                  DrawUpWTrack(window,window->wblock,wtrack);
#endif
                }
	}
}




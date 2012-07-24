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
#include "gfx_slider_proc.h"
#include "visual_proc.h"
#include "blts_proc.h"

#include "gfx_wtrackheader_volume_proc.h"



void UpdateVolumeSlider(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack
){
	int x1,x2,y1,y2;

	DrawSlider(
		window,
		&wtrack->volume,
		(float)wtrack->track->volume,
		0.0f,
		(float)MAXTRACKVOL,
		wtrack->track->volumeonoff,
		true
	);



	/* On/off box stuff. */

	GFX_T_Box(
                  window,1,
                  wtrack->volumeonoff.x1,wtrack->volumeonoff.y1,
                  wtrack->volumeonoff.x2,wtrack->volumeonoff.y2,
                  PAINT_BUFFER
	);

	x1=wtrack->volumeonoff.x1+2;
	x2=wtrack->volumeonoff.x2-2;
	while(x2<x1){
		x1--;
		x2++;
	}
	y1=wtrack->volumeonoff.y1+2;
	y2=wtrack->volumeonoff.y2-2;
	while(y2<y1){
		y1--;
		y2++;
	}

	if(wtrack->track->volumeonoff){
		GFX_T_FilledBox(
                                window,2,
                                x1,y1,
                                x2,y2,
                                PAINT_BUFFER
                                );
	}else{
		GFX_T_FilledBox(
                                window,0,
                                x1,y1,
                                x2,y2,
                                PAINT_BUFFER
		);
	}
	Blt_marktrackheader(window,wtrack->l.num,wtrack->l.num);
}

void UpdatePanSlider(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack
){
	int x1,x2,y1,y2;

	DrawSlider(
		window,
		&wtrack->pan,
		(float)(MAXTRACKPAN+wtrack->track->pan),
		0.0f,
		(float)(MAXTRACKPAN*2),
		wtrack->track->panonoff,
		true
	);


	/* On/off box stuff. */

	GFX_T_Box(
                  window,1,
                  wtrack->panonoff.x1,wtrack->panonoff.y1,
                  wtrack->panonoff.x2,wtrack->panonoff.y2,
                  PAINT_BUFFER
	);

	x1=wtrack->panonoff.x1+2;
	x2=wtrack->panonoff.x2-2;
	while(x2<x1){
		x1--;
		x2++;
	}

	y1=wtrack->panonoff.y1+2;
	y2=wtrack->panonoff.y2-2;
	while(y2<y1){
		y1--;
		y2++;
	}

	if(wtrack->track->panonoff){
          GFX_T_FilledBox(
                          window,2,
                          x1,y1,
                          x2,y2,
                          PAINT_BUFFER
                          );
	}else{
          GFX_T_FilledBox(
                          window,0,
                          x1,y1,
                          x2,y2,
                          PAINT_BUFFER
                          );
	}
	Blt_marktrackheader(window,wtrack->l.num,wtrack->l.num);
}





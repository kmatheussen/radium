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

#include "gfx_wtrackheader_volpan_proc.h"


static void DrawOnOffBox(
                         struct Tracker_Windows *window,
                         const TBox box,
                         bool onoff
                         )
{
	/* On/off box stuff. */

  // border
  GFX_SetMixColor(window,BLACK_COLOR_NUM,LOW_EDITOR_BACKGROUND_COLOR_NUM,300);

  GFX_T_Box(
            window,BLACK_COLOR_NUM,
            box.x1,box.y1,
            box.x2,box.y2,
            PAINT_BUFFER
            );
  

  int x1=box.x1+1;
  int x2=box.x2-1;
  
  int y1=box.y1+1;
  int y2=box.y2-1;

  if (onoff)
    GFX_SetMixColor(window,BLACK_COLOR_NUM,LOW_EDITOR_BACKGROUND_COLOR_NUM,200);
  
  GFX_T_FilledBox(
                  window,HIGH_EDITOR_BACKGROUND_COLOR_NUM,
                  x1,y1,
                  x2,y2,
                  PAINT_BUFFER
                  );
}

void UpdateVolumeSlider(
	struct Tracker_Windows *window,
	const struct WBlocks *wblock,
	const struct WTracks *wtrack
){

	DrawSlider(
		window,
		&wtrack->volume,
		(float)wtrack->track->volume,
		0.0f,
		(float)MAXTRACKVOL,
		wtrack->track->volumeonoff,
		PAINT_BUFFER
	);

        DrawOnOffBox(window, wtrack->volumeonoff, wtrack->track->volumeonoff);
}

void UpdatePanSlider(
	struct Tracker_Windows *window,
	const struct WBlocks *wblock,
	const struct WTracks *wtrack
){

	DrawSlider(
		window,
		&wtrack->pan,
		(float)(MAXTRACKPAN+wtrack->track->pan),
		0.0f,
		(float)(MAXTRACKPAN*2),
		wtrack->track->panonoff,
		PAINT_BUFFER
	);

        DrawOnOffBox(window, wtrack->panonoff, wtrack->track->panonoff);
}





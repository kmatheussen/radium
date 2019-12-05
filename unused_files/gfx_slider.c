/* Copyright 2001 Kjetil S. Matheussen

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
#include "visual_proc.h"

#include "gfx_slider_proc.h"

void DrawSlider(
                struct Tracker_Windows *window,
                const TBox *box,
                float f_val,
                float f_min,
                float f_max,
                bool onoff,
                int where
){

  int y1=box->y1;
  int y2=box->y2 + 1;

  int x = scale(f_val, f_min,f_max, box->x1, box->x2);

  if (!onoff)
    GFX_SetMixColor(window,EDITOR_SLIDERS_COLOR_NUM,LOW_EDITOR_BACKGROUND_COLOR_NUM,300);
  
  //printf("   slider x: %d -> %d -> %d\n", box->x1+2, x, box->x2-2);
  
  GFX_FilledBox(
                window,EDITOR_SLIDERS_COLOR_NUM,
                box->x1+2,y1,
                x,y2,
                where
                );

  //  if (!onoff)
  //  GFX_SetMixColor(window,0,0,500);

  // background
  GFX_SetMixColor(window,LOW_EDITOR_BACKGROUND_COLOR_NUM,HIGH_BACKGROUND_COLOR_NUM,500);
  GFX_FilledBox(
                window,LOW_EDITOR_BACKGROUND_COLOR_NUM,
                x,y1,
                box->x2,y2,
                where
                );
  
  // border
  GFX_SetMixColor(window,BLACK_COLOR_NUM,LOW_EDITOR_BACKGROUND_COLOR_NUM,300);
  GFX_Box(
          window,TEXT_COLOR_NUM,
          box->x1+1,y1,
          box->x2,y2,
          where
          );
}


#if 0
void DrawSlider_old(
	struct Tracker_Windows *window,
	TBox *box,
	float f_val,
	float f_min,
	float f_max,
	bool onoff,
	bool t
){

	int y1=box->y1+1;
	int y2=box->y2-1;

	int max=(int)(1000*(f_max-f_min));
	int val=(int)(1000*(f_val-f_min));

	if(y2<y1){
		y1--;
		y2++;
	}

	if(t){
          GFX_T_Box(
		window,1,
		box->x1,box->y1,
		box->x2,box->y2,
                PAINT_BUFFER
	);

	if(!onoff){
		GFX_T_FilledBox(
			window,0,
			box->x1+1,y1,
			box->x2-1,y2,
                        PAINT_BUFFER
		);
		GFX_T_FilledBox(
			window,3,
			R_BOUNDARIES(
				box->x1+1,
				box->x1+((box->x2-box->x1)*val/max)-3,
				box->x2-1
			),
			y1,
			R_BOUNDARIES(
				box->x1+1,
				box->x1+((box->x2-box->x1)*val/max)+3,
				box->x2-1
			),
			y2,
                        PAINT_BUFFER
		);
		GFX_T_FilledBox(
			window,1,
			R_BOUNDARIES(
				box->x1+1,
				box->x1+((box->x2-box->x1)*val/max),
				box->x2-1
			),
			y1,
			R_BOUNDARIES(
				box->x1+1,
				box->x1+((box->x2-box->x1)*val/max),
				box->x2-1
			),
			y2,
                        PAINT_BUFFER
		);
		return;
	}

	GFX_T_FilledBox(
		window,2,
		box->x1+1,y1,
		box->x2-1,y2,
                PAINT_BUFFER
	);

	GFX_T_FilledBox(
		window,3,
		R_BOUNDARIES(
			box->x1+1,
			box->x1+((box->x2-box->x1)*val/max)-9,
			box->x2-1
		),
		y1,
		R_BOUNDARIES(
			box->x1+1,
			box->x1+((box->x2-box->x1)*val/max)+9,
			box->x2-1
		),
		y2,
                PAINT_BUFFER
	);

	GFX_T_FilledBox(
		window,1,
		R_BOUNDARIES(
			box->x1+1,
			box->x1+((box->x2-box->x1)*val/max)-3,
			box->x2-1
		),
		y1,
		R_BOUNDARIES(
			box->x1+1,
			box->x1+((box->x2-box->x1)*val/max)+3,
			box->x2-1
		),
		y2,
                PAINT_BUFFER
	);

	GFX_T_FilledBox(
		window,2,
		R_BOUNDARIES(
			box->x1+1,
			box->x1+((box->x2-box->x1)*val/max),
			box->x2-1
		),
		y1,
		R_BOUNDARIES(
			box->x1+1,
			box->x1+((box->x2-box->x1)*val/max),
			box->x2-1
		),
		y2,
                PAINT_BUFFER
                          );
	}else{
	GFX_Box(
		window,1,
		box->x1,box->y1,
		box->x2,box->y2,
                PAINT_DIRECTLY
	);

	if(!onoff){
		GFX_FilledBox(
			window,0,
			box->x1+1,y1,
			box->x2-1,y2,
                        PAINT_DIRECTLY

		);
		GFX_FilledBox(
			window,3,
			R_BOUNDARIES(
				box->x1+1,
				box->x1+((box->x2-box->x1)*val/max)-3,
				box->x2-1
			),
			y1,
			R_BOUNDARIES(
				box->x1+1,
				box->x1+((box->x2-box->x1)*val/max)+3,
				box->x2-1
			),
			y2,
                        PAINT_DIRECTLY
		);
		GFX_FilledBox(
			window,1,
			R_BOUNDARIES(
				box->x1+1,
				box->x1+((box->x2-box->x1)*val/max),
				box->x2-1
			),
			y1,
			R_BOUNDARIES(
				box->x1+1,
				box->x1+((box->x2-box->x1)*val/max),
				box->x2-1
			),
			y2,
                        PAINT_DIRECTLY
		);
		return;
	}

	GFX_FilledBox(
		window,2,
		box->x1+1,y1,
		box->x2-1,y2,
                PAINT_DIRECTLY

	);

	GFX_FilledBox(
		window,3,
		R_BOUNDARIES(
			box->x1+1,
			box->x1+((box->x2-box->x1)*val/max)-9,
			box->x2-1
		),
		y1,
		R_BOUNDARIES(
			box->x1+1,
			box->x1+((box->x2-box->x1)*val/max)+9,
			box->x2-1
		),
		y2,
                PAINT_DIRECTLY
	);

	GFX_FilledBox(
		window,1,
		R_BOUNDARIES(
			box->x1+1,
			box->x1+((box->x2-box->x1)*val/max)-3,
			box->x2-1
		),
		y1,
		R_BOUNDARIES(
			box->x1+1,
			box->x1+((box->x2-box->x1)*val/max)+3,
			box->x2-1
		),
		y2,
                PAINT_DIRECTLY
	);

	GFX_FilledBox(
		window,2,
		R_BOUNDARIES(
			box->x1+1,
			box->x1+((box->x2-box->x1)*val/max),
			box->x2-1
		),
		y1,
		R_BOUNDARIES(
			box->x1+1,
			box->x1+((box->x2-box->x1)*val/max),
			box->x2-1
		),
		y2,
                PAINT_DIRECTLY
	);
	}
}
#endif

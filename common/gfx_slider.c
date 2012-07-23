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
	GFX_P_T_Box(
		window,1,
		box->x1,box->y1,
		box->x2,box->y2
	);

	if(!onoff){
		GFX_P_T_FilledBox(
			window,0,
			box->x1+1,y1,
			box->x2-1,y2

		);
		GFX_P_T_FilledBox(
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
			y2
		);
		GFX_P_T_FilledBox(
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
			y2
		);
		return;
	}

	GFX_P_T_FilledBox(
		window,2,
		box->x1+1,y1,
		box->x2-1,y2

	);

	GFX_P_T_FilledBox(
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
		y2
	);

	GFX_P_T_FilledBox(
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
		y2
	);

	GFX_P_T_FilledBox(
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
		y2
	);
	}else{
	GFX_Box(
		window,1,
		box->x1,box->y1,
		box->x2,box->y2
	);

	if(!onoff){
		GFX_FilledBox(
			window,0,
			box->x1+1,y1,
			box->x2-1,y2

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
			y2
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
			y2
		);
		return;
	}

	GFX_FilledBox(
		window,2,
		box->x1+1,y1,
		box->x2-1,y2

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
		y2
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
		y2
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
		y2
	);
	}
}

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
#include "visual_proc.h"
#include "list_proc.h"

#include "sliders_proc.h"




/*********************************************************
  FUNCTION
    Updates the bottom slider. Call whenever necesarry.
    The bottom slider displays the current tracknumber
    graphically.
*********************************************************/
#define BottomSliderBox(a,b,c) \
	GFX_Slider_FilledBox( \
		window,a,b,y1,c,y2 \
	)

void UpdateBottomSlider(struct Tracker_Windows *window){
	struct WBlocks *wblock=window->wblock;

	struct WTracks *wtrack;

	int sx1=window->bottomslider.x;
	int sx2=window->bottomslider.x2;
	int vx1=0,vx2=0;
	int rx1=0;
	int rx2;

	int x;
	int x2;

	int lx=window->bottomslider.lx;
	int lx2=window->bottomslider.lx2;
	int mdx=0;

	int y1,y2;

	y1=wblock->t.y2+2;
	y2=window->height - 2;

	while(y2<=y1){
		y1--;
		y2++;
	}

	wtrack=wblock->wtracks;
	while(wtrack!=NULL){
		if(wtrack->l.num==wblock->left_track) vx1=mdx;
		mdx+=wtrack->fxwidth+(wtrack->notelength*window->fontwidth);
		if(wtrack->l.num==wblock->right_track) vx2=mdx - (wtrack->x2-wblock->a.x2);
		wtrack=NextWTrack(wtrack);
	}
	vx2=min(vx2,mdx);
	rx2=mdx;

	x=sx1 + (sx2-sx1)*(vx1-rx1)/(rx2-rx1) + 2;
	x2=sx1 + (sx2-sx1)*(vx2-rx1)/(rx2-rx1) - 2;

	if(lx2<=x || x2<=lx){
		BottomSliderBox(0,lx,lx2);
		//		BottomSliderBox(2,x,x2);
		GFX_Slider_FilledBox( 
				     window,2,x,y1+1,x2,y2-1
				     );

		window->bottomslider.lx=x;
		window->bottomslider.lx2=x2;
		return;
	}

	if(lx<x) BottomSliderBox(0,lx,x-1);
	else if(lx>x) // BottomSliderBox(2,x,lx-1);
	  GFX_Slider_FilledBox(window,2,x,y1+1,lx-1,y2-1);

	if(lx2>x2) BottomSliderBox(0,x2+1,lx2);
	else if(lx2<x2) // BottomSliderBox(2,lx2+1,x2);
	  GFX_Slider_FilledBox(window,2,lx2+1,y1+1,x2,y2-1);


	window->bottomslider.lx=x;
	window->bottomslider.lx2=x2;
}


/********************************************************
  FUNCTION
    Do only call when screen is blank.
********************************************************/

void DrawBottomSlider(struct Tracker_Windows *window){
	int y1,y2;

	window->bottomslider.x=window->wblock->t.x1;

	GFX_Box(
		window,
		1,
		window->bottomslider.x,
		window->height - window->bottomslider.width,
		window->bottomslider.x2,
		window->height-1
	);

	GFX_Box(
		window,
		0,
		window->bottomslider.x+1,
		window->height - window->bottomslider.width+1,
		window->bottomslider.x2-1,
		window->height-2
	);

	y1=window->height - window->bottomslider.width + 2;
	y2=window->height - 3;

	while(y2<=y1){
		y1--;
		y2++;
	}

	window->bottomslider.lx=window->bottomslider.x + 2,
	window->bottomslider.lx2=window->bottomslider.x2 - 2,

	GFX_FilledBox(
		window,
		2,
		window->bottomslider.lx,
		y1,
		window->bottomslider.lx2,
		y2
	);

	UpdateBottomSlider(window);
}




/*********************************************************
  FUNCTION
    Updates the left slider. Call whenever necesarry.
*********************************************************/

#define LeftSliderBox(a,b,c) GFX_Slider_FilledBox(window,a,2,b,window->leftslider.width-2,c)
void UpdateLeftSlider(struct Tracker_Windows *window){
	struct WBlocks *wblock=window->wblock;

	int dx=(wblock->t.y2 - wblock->t.y1 - 4)
	       * wblock->num_visiblelines
	       / (wblock->num_reallines + wblock->num_visiblelines - 2);

	int x=wblock->t.y1 + 2 + (wblock->curr_realline * dx / wblock->num_visiblelines);
	int x2=x+dx;

	int lx=window->leftslider.lx;
	int lx2=window->leftslider.lx2;

	if(wblock->curr_realline==wblock->num_reallines-1) x2=window->leftslider.x2-2;

	if(lx2<=x || x2<=lx){
		LeftSliderBox(0,lx,lx2);
		LeftSliderBox(2,x,x2);
		window->leftslider.lx=x;
		window->leftslider.lx2=x2;
		return;
	}

	if(lx<x) LeftSliderBox(0,lx,x-1);
	else if(lx>x) LeftSliderBox(2,x,lx-1);
	if(lx2>x2) LeftSliderBox(0,x2+1,lx2);
	else if(lx2<x2) LeftSliderBox(2,lx2+1,x2);


	window->leftslider.lx=x;
	window->leftslider.lx2=x2;
}



/********************************************************
  FUNCTION
    Do only call when screen is blank.
********************************************************/

void DrawLeftSlider(struct Tracker_Windows *window){
	window->leftslider.x=window->wblock->t.y1;
	window->leftslider.x2=window->wblock->t.y2+1;

	GFX_Box(
		window,
		1,
		0,
		window->leftslider.x,
		window->leftslider.width,
		window->leftslider.x2
	);


	window->leftslider.lx=window->leftslider.x+2;
	window->leftslider.lx2=window->leftslider.x2-2;

	GFX_FilledBox(
		window,
		2,
		2,
		window->leftslider.lx,
		window->leftslider.width - 2,
		window->leftslider.lx2
	);


	UpdateLeftSlider(window);

}





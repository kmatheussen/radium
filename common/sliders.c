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
#include "settings_proc.h"
#include "wtracks_proc.h"

#include "sliders_proc.h"


// Painting scroll bar.


#if 0

/*********************************************************
  FUNCTION
    Updates the bottom slider. Call whenever necesarry.
    The bottom slider displays the current tracknumber
    graphically.
*********************************************************/
#define BottomSliderBox(a,b,c) \
	GFX_FilledBox( \
                      window,a,b,y1,c,y2,PAINT_DIRECTLY       \
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
		if (wtrack->l.num==wblock->left_track)
                  vx1=mdx;
                
		mdx += wtrack->fxwidth + (wtrack->notelength*window->fontwidth);
                
		if(wtrack->l.num==wblock->right_track)
                  vx2=mdx - (wtrack->x2-wblock->a.x2);
                
		wtrack=NextWTrack(wtrack);
	}
	vx2=R_MIN(vx2,mdx);
	rx2=mdx;

	x  = sx1 + (sx2-sx1)*(vx1-rx1)/(rx2-rx1) + 2;
	x2 = sx1 + (sx2-sx1)*(vx2-rx1)/(rx2-rx1) - 2;

	if(lx2<=x || x2<=lx){
		BottomSliderBox(0,lx,lx2);
		//		BottomSliderBox(2,x,x2);
		GFX_FilledBox( 
                              window,2,x,y1+1,x2,y2-1,
                              PAINT_DIRECTLY
                               );

		window->bottomslider.lx=x;
		window->bottomslider.lx2=x2;
		return;
	}

	if(lx<x) BottomSliderBox(0,lx,x-1);
	else if(lx>x) // BottomSliderBox(2,x,lx-1);
	  GFX_FilledBox(window,2,x,y1+1,lx-1,y2-1,PAINT_DIRECTLY);

	if(lx2>x2) BottomSliderBox(0,x2+1,lx2);
	else if(lx2<x2) // BottomSliderBox(2,lx2+1,x2);
	  GFX_FilledBox(window,2,lx2+1,y1+1,x2,y2-1,PAINT_DIRECTLY);


	window->bottomslider.lx=x;
	window->bottomslider.lx2=x2;
}
#endif

/*
typedef struct {
  int leftmost_visible_x;
  int rightmost_visible_x;
  int total_width;
} SliderData;

static SliderData get_sliderdata(const struct Tracker_Windows *window, const struct WBlocks *wblock){
  SliderData data;

  struct WTracks *wtrack = wblock->wtracks;

  int x = 0;

  if (wblock->left_track < 0)
    data.leftmost_visible_x = 0;

  while(wtrack != NULL){
    int num_subtracks = WTRACK_num_subtracks(wtrack);
    int track_width = WTRACK_getWidth(window, wtrack);

    if (wtrack->l.num == wblock->left_track){
      if (wblock->left_subtrack==-1)
        data.leftmost_visible_x = x;
      else
        data.leftmost_visible_x = x + scale(wblock->left_subtrack, 0, num_subtracks, (wtrack->notelength*window->fontwidth)+2, track_width);
    }

    x += track_width;

    if (wtrack->l.num == wblock->right_track)
      data.rightmost_visible_x = x - (wtrack->x2-wblock->a.x2);

    wtrack = NextWTrack(wtrack);
  }

  data.total_width = x;

  return data;
}
*/

TBox GetBottomSliderCoordinates(const struct Tracker_Windows *window, const struct WBlocks *wblock, int *inner_x1, int *inner_x2){
  TBox box = {
    .x1 = window->bottomslider.x,
    .y1 = wblock->reltempo.y1,
    .x2 = window->bottomslider.x2,
    .y2 = wblock->reltempo.y2
  };

  if (inner_x1 != NULL){
    //SliderData data = get_sliderdata(window, wblock);
    int total_width = WTRACKS_getWidth(window, wblock);
                     
    //*inner_x1 = scale(data.leftmost_visible_x, 0, data.total_width, box.x1, box.x2);
    //*inner_x2 = scale(data.rightmost_visible_x, 0, data.total_width, box.x1, box.x2);
    *inner_x1 = scale(0, wblock->skew_x, total_width + wblock->skew_x, box.x1, box.x2);
    *inner_x2 = scale(box.x2-box.x1, wblock->skew_x, total_width + wblock->skew_x, box.x1, box.x2);
  }

  return box;
}


/********************************************************
  FUNCTION
    Do only call when screen is blank.
********************************************************/

// Note that this is the track selector slider, not the block tempo slider.
void DrawBottomSlider(struct Tracker_Windows *window){
  window->bottomslider.x = window->wblock->t.x1;

  int inner_x1;
  int inner_x2;
  TBox box = GetBottomSliderCoordinates(window, window->wblock, &inner_x1, &inner_x2);

  // background
  GFX_FilledBox(
                window,
                HIGH_BACKGROUND_COLOR_NUM,
                window->wblock->reltempo.x2,box.y1,
                box.x2,box.y2,
                PAINT_DIRECTLY
                );


  if (window->track_slider_is_moving)
    GFX_SetMixColor(window, TRACK_SLIDER_COLOR_NUM, BLACK_COLOR_NUM, 500);
  
  // slider
  GFX_FilledBox(
                window,TRACK_SLIDER_COLOR_NUM,
                inner_x1,box.y1,
                inner_x2,box.y2,
                PAINT_DIRECTLY
                );
  // border
  GFX_Box(
          window,TRACK_SLIDER_COLOR_NUM,
          box.x1,box.y1,
          box.x2,box.y2,
          PAINT_DIRECTLY
          );  
                
}


#if 0
void DrawBottomSlider_old(struct Tracker_Windows *window){

	window->bottomslider.x = window->wblock->t.x1;

        int y1 = window->height - window->bottomslider.width + 2;
	int y2 = window->height - 3;

        while(y2<=y1){
          y1--;
          y2++;
	}

	GFX_Box(
		window,
		1,
		window->bottomslider.x,
                y1 - 2,
		window->bottomslider.x2,
                y2 + 2,
                PAINT_DIRECTLY
	);

	GFX_Box(
		window,
		0,
		window->bottomslider.x+1,
                y1 - 1,
		window->bottomslider.x2-1,
                y1 + 1,
                PAINT_DIRECTLY
	);

	window->bottomslider.lx=window->bottomslider.x + 2,
	window->bottomslider.lx2=window->bottomslider.x2 - 2,

	GFX_FilledBox(
		window,
		2,
		window->bottomslider.lx,
		y1,
		window->bottomslider.lx2,
		y2,
                PAINT_DIRECTLY
	);

	UpdateBottomSlider(window);
}


#endif


/*********************************************************
  FUNCTION
    Updates the left slider. Call whenever necesarry.
*********************************************************/

#if !USE_OPENGL

#define LeftSliderBox(color,b,c) GFX_FilledBox(window,color,2,b,window->leftslider.width-2,c,PAINT_DIRECTLY)
void UpdateLeftSlider(struct Tracker_Windows *window){
	struct WBlocks *wblock=window->wblock;

        if((wblock->num_reallines + wblock->num_visiblelines - 2)==0)
          return;
        if(wblock->num_visiblelines==0)
          return;

	int dx=(wblock->t.y2 - wblock->t.y1 - 4)
	       * wblock->num_visiblelines
	       / (wblock->num_reallines + wblock->num_visiblelines - 2);

	int x=wblock->t.y1 + 2 + (wblock->curr_realline * dx / wblock->num_visiblelines);
	int x2=x+dx;

	int lx=window->leftslider.lx;
	int lx2=window->leftslider.lx2;

	if(wblock->curr_realline==wblock->num_reallines-1)
          x2=window->leftslider.x2-2;

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
		window->leftslider.x2,
                PAINT_DIRECTLY
	);


	window->leftslider.lx=window->leftslider.x+2;
	window->leftslider.lx2=window->leftslider.x2-2;

	GFX_FilledBox(
		window,
		2,
		2,
		window->leftslider.lx,
		window->leftslider.width - 2,
		window->leftslider.lx2,
                PAINT_DIRECTLY
	);


	UpdateLeftSlider(window);
}
#endif // !USE_OPENGL



void InitSliderValues(struct Tracker_Windows *window){
  window->leftslider.show=1;
  window->bottomslider.show=1;

  //	twindow->bottomslider.width=twindow->fontwidth;
  window->bottomslider.width = SETTINGS_read_int32("bottom_slider_height",window->fontheight*2/3);
  window->leftslider.width   = SETTINGS_read_int32("left_slider_width",8);
}

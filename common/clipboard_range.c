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
#include "placement_proc.h"
#include "wtracks_proc.h"
#include "clipboard_range.h"

#include "clipboard_range_proc.h"



struct Range *range=NULL;


void SetRange(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	NInt starttrack,
	NInt endtrack,
	Place startplace,
	Place endplace
){

        R_ASSERT_RETURN_IF_FALSE(p_Less_Than(startplace, endplace));
        R_ASSERT_RETURN_IF_FALSE(p_Greater_Or_Equal(startplace, p_Create(0,0,1)));
        R_ASSERT_RETURN_IF_FALSE(p_Less_Or_Equal(endplace, p_Create(wblock->block->num_lines, 0, 1)));

	wblock->isranged=true;
	wblock->rangex1=starttrack;
	wblock->rangex2=endtrack;

	wblock->rangey1=startplace;
	wblock->rangey2=endplace;
}



static void MarkRange(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	NInt track,
	int realline
){
	NInt starttrack,endtrack;
	Place startplace, endplace;

        R_ASSERT_RETURN_IF_FALSE(realline>=0);
        R_ASSERT_RETURN_IF_FALSE(realline<wblock->num_reallines);

	if(wblock->isranged){

		starttrack=wblock->rangex1;
		endtrack=track;
		if(starttrack>endtrack){
			starttrack=track;
			endtrack=wblock->rangex2;
		}
		
		startplace=wblock->rangey1;

                if (realline+1 >= wblock->num_reallines)
                  endplace = p_Create(wblock->block->num_lines, 0, 1);
                else
                  endplace = wblock->reallines[realline+1]->l.p;

		if(p_Greater_Or_Equal(startplace, endplace)){
                  startplace = wblock->reallines[realline]->l.p;
                  endplace = wblock->rangey2;
		}

		while(p_Greater_Or_Equal(startplace, endplace)){
                  realline--;
                  R_ASSERT_RETURN_IF_FALSE(realline>=0);
                  startplace = wblock->reallines[realline]->l.p;
                }

	}else{

		endtrack=starttrack=track;

		int startrealline=realline;
		int endrealline=realline+1;

                if (startrealline < 0){
                  R_ASSERT_NON_RELEASE(false);
                  startrealline = 0;
                  endrealline = 1;
                }

                if (startrealline>=wblock->num_reallines){
                  R_ASSERT_NON_RELEASE(false);
                  startrealline = wblock->num_reallines -1 ;
                  endrealline = startrealline+1;
                }

                if (endrealline > wblock->num_reallines){
                  R_ASSERT_NON_RELEASE(false);
                  startrealline = wblock->num_reallines -1 ;
                  endrealline = startrealline+1;
                }
                
                startplace = wblock->reallines[startrealline]->l.p;

                if (endrealline >= wblock->num_reallines)
                  endplace = p_Create(wblock->block->num_lines, 0, 1);
                else
                  endplace = wblock->reallines[endrealline]->l.p;
	}

	SetRange(window,wblock,starttrack,endtrack,startplace,endplace);
}


void MarkRange_CurrPos(struct Tracker_Windows *window){
	struct WBlocks *wblock=window->wblock;

	MarkRange(window,wblock,wblock->wtrack->l.num,wblock->curr_realline);

#if !USE_OPENGL
	bool isranged=wblock->isranged;
	NInt y1=wblock->rangey1;
	NInt y2=wblock->rangey2;

	if( isranged && (wblock->rangey1>y1 || wblock->rangey2<y2)){
		UpdateAndClearSomeTrackReallinesAndGfxWTracks(
			window,
			window->wblock,
			window->wblock->rangex1,
			window->wblock->rangex2
		);
	}else{
		UpdateAllWTracks(window,wblock,0,wblock->num_reallines);
	}
#endif

        window->must_redraw = true;
}


static void CancelRange(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
){
  wblock->isranged=false;
}

void CancelRange_CurrPos(struct Tracker_Windows *window){
	CancelRange(window,window->wblock);
#if !USE_OPENGL
	UpdateAndClearSomeTrackReallinesAndGfxWTracks(
		window,
		window->wblock,
		window->wblock->rangex1,
		window->wblock->rangex2
	);
#endif

        window->must_redraw = true;
}


/********************************************************
  FUNCTION
    Makes sure that the range is legal. Must be
    called after shrinking the number of reallines
    in a wblock, or the number of tracks.
********************************************************/
void MakeRangeLegal(
	struct WBlocks *wblock
){

	if(p_Greater_Or_Equal(wblock->rangey1, wblock->rangey2)){
          if (wblock->rangey1.line==0)
            wblock->rangey2 = p_Add(wblock->rangey1, p_Create(1,0,1));
          else
            wblock->rangey1 = p_Sub(wblock->rangey2, p_Create(1,0,1));
        };

        if (p_Greater_Than(wblock->rangey2, p_Create(wblock->block->num_lines, 0, 1)))
          wblock->rangey2=p_Create(wblock->block->num_lines, 0, 1);

        if (p_Greater_Than(wblock->rangey1, p_Create(wblock->block->num_lines, 0, 1))){
          wblock->rangey1=p_Create(wblock->block->num_lines-1, 0, 1);
          wblock->rangey2=p_Create(wblock->block->num_lines,   0, 1);
        }

        if(p_Greater_Or_Equal(wblock->rangey1, wblock->rangey2)){
          R_ASSERT_NON_RELEASE(false);
          wblock->rangey1 = p_Create(0,0,1);
          wblock->rangey1 = p_Create(1,0,1);
        }

	wblock->rangex2=R_MIN(wblock->rangex2,wblock->block->num_tracks-1);
	wblock->rangex1=R_MIN(wblock->rangex1,wblock->rangex2);
}


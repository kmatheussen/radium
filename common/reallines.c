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





/***********************************************************************
  OOPS. There was a bug here. And it wasn't possibly to fix without
  rewriting a lot. So I hacked a bit...and, well, its not very easy to
  understand the code here now I guess. And its pretty slow too.
***********************************************************************/



#include <stdlib.h>
#include <math.h>

#include "nsmtracker.h"
#include "localzooms_proc.h"
#include "trackreallines_proc.h"
#include "windows_proc.h"
#include "tempos_proc.h"
#include "LPB_proc.h"
#include "temponodes_proc.h"
#include "fxlines_proc.h"
#include "clipboard_range_proc.h"
#include "undo_reallines_proc.h"
#include "player_proc.h"
#include "wblocks_proc.h"
#include "gfx_wblocks_proc.h"
#include "gfx_wtracks_proc.h"
#include "pixmap_proc.h"
#include "visual_proc.h"
#include "sliders_proc.h"

#include "reallines_proc.h"





/************************************************************************
  FUNCTION
    Update all structures that depends on the reallines for the wblock
    'wblock'.
************************************************************************/
void UpdateReallinesDependens(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
){
	UpdateAllTrackReallines(window,wblock);
	UpdateWTempos(window,wblock);
	UpdateWLPBs(window,wblock);
	UpdateWTempoNodes(window,wblock);
	UpdateAllFXNodeLines(window,wblock);
	wblock->num_reallines_last=wblock->num_reallines;
	wblock->isgfxdatahere=true;
}


/************************************************************************
  FUNCTION
    Returns the localzoom element that has its 'uplevel' attribute
    pointing to the first element in a localzoom list that the 'realline'
    belongs to. Returns NULL if it is allready placed at level 0.

  NOTES
    'realline' allso gets the realline value for the new position
    localzoom 'localzoom' will get later if we are unexpanding.

    The next two procedures are to be seen as one function.
************************************************************************/
struct LocalZooms *FindLocalZoomRootTheHardWay(
	struct WBlocks *wblock,
	struct LocalZooms *localzoom,
	struct LocalZooms *org
){
	struct LocalZooms *ret;

	while(localzoom!=NULL){
		if(localzoom==org) return org;

		if(localzoom->uplevel!=NULL){
			ret=FindLocalZoomRootTheHardWay(wblock,localzoom->uplevel,org);
			if(ret==org) return localzoom;
			if(ret!=NULL) return ret;
		}
		localzoom=NextLocalZoom(localzoom);
	}
	return NULL;
}

struct LocalZooms *FindLocalZoomRoot(
	struct WBlocks *wblock,
	int *realline
){
	struct LocalZooms **reallines=wblock->reallines;
	struct LocalZooms *localzoom=reallines[*realline];
	struct LocalZooms *org=localzoom;
	int level=localzoom->level;
//	int set=0;
	if(0==level) return NULL;

	while(localzoom->level>=level){
//		if(set==0 && localzoom->zoomline==0){set=1; org=localzoom;}
		(*realline)--;
		if(*realline<0){
			localzoom=wblock->localzooms;
			goto firstline;
		}
		localzoom=reallines[*realline];
	}
	
	localzoom=NextLocalZoom(localzoom);

firstline:

	while(localzoom->level<level-1)
		localzoom=localzoom->uplevel;


	if(localzoom==NULL || localzoom->uplevel!=org){
		localzoom=FindLocalZoomRootTheHardWay(wblock,wblock->localzooms,org);
		*realline=localzoom->realline-1;
	}


	return localzoom;
	
}

/**************************************************************************
  FUNCTION
    Find out how many reallines there is in a wblock.
**************************************************************************/
int FindNumberOfRealLines(
	struct LocalZooms *localzoom,
	int realline
){
	while(localzoom!=NULL){
		if(localzoom->uplevel!=NULL){
			realline=FindNumberOfRealLines(localzoom->uplevel,realline);
		}else{
			realline++;
		}
		localzoom=NextLocalZoom(localzoom);
	}
	return realline;
}

/************************************************************************
  FUNCTION
    When the localzooms in the wblock has been changed, or a wblock
    has just been made. (the next two procedures are to be seen on as
    one function.)
************************************************************************/

int UpdateRealLinesRec(
	struct LocalZooms **reallines,
	struct LocalZooms *localzoom,
	int realline
){
	while(localzoom!=NULL){
		localzoom->realline=realline;
		if(localzoom->uplevel!=NULL){
			realline=UpdateRealLinesRec(reallines,localzoom->uplevel,realline);
		}else{
			reallines[realline]=localzoom;
			realline++;
		}
		localzoom=NextLocalZoom(localzoom);
	}
	return realline;
}

void UpdateRealLines(struct Tracker_Windows *window,struct WBlocks *wblock){
	struct LocalZooms *localzoom=wblock->localzooms;

	wblock->num_reallines=FindNumberOfRealLines(localzoom,0);

	if(wblock->num_reallines!=wblock->num_reallines_last || wblock->reallines==NULL){
		wblock->reallines=talloc(wblock->num_reallines * sizeof(struct LocalZooms *));
	}

	UpdateRealLinesRec(wblock->reallines,localzoom,0);


}

int FindHighestLocalzoomLevel(struct WBlocks *wblock){
  int highest=0;
  int i;
  for(i=0;i<wblock->num_reallines;i++)
    highest = R_MAX(wblock->reallines[i]->level, highest);
  return highest;
}


void ExpandLine(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	int realline,
	int num_newreallines
){
	struct LocalZooms **reallines=wblock->reallines;
	struct LocalZooms *localzoom=reallines[realline];
	int lokke;


	if(localzoom->Tdividor*num_newreallines>=MAX_UINT32){
		fprintf(stderr,"Too many levels, can't expand.\n");
		return;
	}

	for(lokke=0;lokke<num_newreallines;lokke++){
		NewLocalZoom(
			&localzoom->uplevel,
			localzoom->Tline,
			(uint_32)lokke+(localzoom->Tcounter*num_newreallines),
			(uint_32)num_newreallines * localzoom->Tdividor,
			lokke,
			localzoom->level+1,
			lokke+localzoom->Tline
		);
	}

	if(
		 ( (int)log10((float)(localzoom->level+1))+1 )*window->fontwidth > 
		 wblock->zoomlevelarea.width
	){
		wblock->zoomlevelarea.width+=window->fontwidth;
	}

	wblock->num_reallines+=num_newreallines-1;

	UpdateRealLines(window,wblock);


	MakeRangeLegal(wblock);

}

void ExpandLineCurrPos(
	struct Tracker_Windows *window,
	int num_newreallines
){
	struct WBlocks *wblock=window->wblock;

	PlayStop();

	Undo_Reallines_CurrPos(window);

	ExpandLine(window,wblock,wblock->curr_realline,num_newreallines);

	UpdateReallinesDependens(window,wblock);

	DrawUpTrackerWindow(window);
}

int FindNumReallinesFor(struct LocalZooms *localzoom){
	int ret=0;

	while(localzoom!=NULL){
		if(localzoom->uplevel!=NULL){
			ret+=FindNumReallinesFor(localzoom->uplevel);
		}else{
			ret++;
		}
		localzoom=NextLocalZoom(localzoom);
	}
	return ret;
}

void Unexpand(struct Tracker_Windows *window,struct WBlocks *wblock,int realline){

	struct LocalZooms *localzoom=FindLocalZoomRoot(wblock,&realline);
	if(localzoom==NULL) return;
	wblock->num_reallines-=(FindNumReallinesFor(localzoom->uplevel)-1 );
	localzoom->uplevel=NULL;

	wblock->curr_realline=realline+1;

	UpdateRealLines(window,wblock);


}

void UnexpandCurrPos(struct Tracker_Windows *window){
	struct WBlocks *wblock=window->wblock;
	int realline=wblock->curr_realline;

	PlayStop();

	Undo_Reallines_CurrPos(window);

	Unexpand(window,wblock,realline);

	if(wblock->num_reallines==wblock->block->num_lines){
		wblock->zoomlevelarea.width=0;
	}

	UpdateReallinesDependens(window,wblock);

	DrawUpTrackerWindow(window);

}


void Zoom(struct Tracker_Windows *window,struct WBlocks *wblock,int numtozoom){
	int curr_realline_org=wblock->curr_realline;
	int curr_realline=curr_realline_org;
	int num_reallines=wblock->num_reallines;
	int num_toexpand;
	int zoomlevelareawidth;

	PlayStop();

	zoomlevelareawidth=wblock->zoomlevelarea.width;

	Undo_Reallines_CurrPos(window);


	Unexpand(window,wblock,curr_realline);

	curr_realline=wblock->curr_realline;

	num_toexpand=(num_reallines-wblock->num_reallines)+numtozoom+1;

	if(num_toexpand>1){

		ExpandLine(window,wblock,curr_realline,num_toexpand);

		wblock->curr_realline=curr_realline_org;

	}

	if(wblock->num_reallines==wblock->block->num_lines){
		wblock->zoomlevelarea.width=0;
	}


	if(zoomlevelareawidth!=wblock->zoomlevelarea.width){

	  UpdateReallinesDependens(window,wblock);
	  DrawUpTrackerWindow(window);

	}else{
	  PixMap_reset(window);

	  //	  GFX_P_FilledBox(window,0,0,0,window->width-1,window->height-1);
	  
	  UpdateReallinesDependens(window,wblock);
	  DrawUpAllWTracks(window,wblock);
	  UpdateLeftSlider(window);

	  GFX_P_FilledBox(
			  window,0,
			  wblock->zoomlevelarea.x,
			  wblock->t.y1,
			  wblock->temponodearea.x2,
			  wblock->t.y2
			  );

	  DrawWBlockSpesific(
			     window,
			     wblock,
			     wblock->top_realline,
			     wblock->bot_realline
			     );

	  
	  //UpdateAllWTracks(window,wblock,wblock->top_realline,wblock->bot_realline);
	  //	  DrawWBlock(window,wblock);
	}
}













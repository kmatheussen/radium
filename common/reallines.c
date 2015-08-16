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


/*
  Warning: This code is fragile, most likely buggy, and generally horrible.

  If new functionality is needed, the current
  code should not be edited. Instead, just implement the new functionality
  by using the current functions somehow. Alternatively, rewrite everything.
 */



/***********************************************************************
  OOPS. There was a bug here. And it wasn't possibly to fix without
  rewriting a lot. So I hacked a bit...and, well, its not very easy to
  understand the code here now I guess. And its pretty slow too.
***********************************************************************/


#include <string.h>
#include <stdlib.h>
#include <math.h>

#include "nsmtracker.h"
#include "localzooms_proc.h"
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
#include "pixmap_proc.h"
#include "visual_proc.h"
#include "sliders_proc.h"
#include "OS_Player_proc.h"
#include "realline_calc_proc.h"

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
        //UpdateWTempos(window,wblock);
	//UpdateWLPBs(window,wblock);
#if !USE_OPENGL
	//UpdateWTempoNodes(window,wblock);
	UpdateAllFXNodeLines(window,wblock);
#endif
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
static struct LocalZooms *FindLocalZoomRootTheHardWay(
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

static struct LocalZooms *FindLocalZoomRoot(
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

        if (localzoom==NULL)
          return NULL;
        
	localzoom=NextLocalZoom(localzoom);

firstline:
        
        if (localzoom==NULL)
          return NULL;
        
	while(localzoom->level<level-1) {          
          localzoom=localzoom->uplevel;
          if (localzoom==NULL)
            return NULL;
        }

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
static int FindNumberOfRealLines(
	struct LocalZooms *localzoom,
	int realline
){
	while(localzoom!=NULL){
		if(localzoom->uplevel!=NULL){
			realline=FindNumberOfRealLines(localzoom->uplevel,realline);
                }else if(false && localzoom->level==0){
                  realline++;
                  realline++;
                }else if(false && localzoom->Tline%2) {
                  
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

static int UpdateRealLinesRec(
	struct LocalZooms **reallines,
	struct LocalZooms *localzoom,
	int realline
){
	while(localzoom!=NULL){
		localzoom->realline=realline;
		if(localzoom->uplevel!=NULL){
			realline=UpdateRealLinesRec(reallines,localzoom->uplevel,realline);
                }else if(false && localzoom->level==0){
                  struct LocalZooms *lz = talloc(sizeof(struct LocalZooms));
                  lz->l.p.line    = localzoom->l.p.line;
                  lz->l.p.counter = 0;
                  lz->l.p.dividor = 2;
                  lz->realline    = realline;
                  lz->zoomline    = 0;
                  lz->level       = 1;
                  reallines[realline++]=lz;
                  {
                    struct LocalZooms *lz = talloc(sizeof(struct LocalZooms));
                    lz->l.p.line    = localzoom->l.p.line;
                    lz->l.p.counter = 1;
                    lz->l.p.dividor = 2;
                    lz->realline    = realline;
                    lz->zoomline    = 1;
                    lz->level       = 1;
                    reallines[realline++]=lz;
                  }
                }else if(false && localzoom->Tline%2) {
		}else{
                  reallines[realline++]=localzoom;
                }
		localzoom=NextLocalZoom(localzoom);
	}
	return realline;
}

static struct LocalZooms **get_shrinked_reallines(const struct Tracker_Windows *window, const struct WBlocks *wblock, int factor, int *num_returned_reallines){
  R_ASSERT(factor>1);

  int realline = 0;
  int line = 0;

  int array_length = wblock->block->num_lines;
  struct LocalZooms **reallines = talloc(array_length * sizeof(struct LocalZooms *));
  
  while(line < wblock->block->num_lines){

    struct LocalZooms *lz = talloc(sizeof(struct LocalZooms));
    lz->l.p.line    = line;
    //lz->l.p.counter = 0;
    lz->l.p.dividor = 1;
    lz->realline    = realline;
    //lz->zoomline    = 0;
    lz->level       = 1;

    reallines[realline]=lz;

    line     += factor;
    realline += 1;
  }

  if (realline<2) // can not have less than 2 reallines in a block.
    return NULL;

  *num_returned_reallines = realline;

  return reallines;
}


static void set_curr_realline(struct WBlocks *wblock, int new_curr_realline){
  //printf("new_curr_realline: %d, num_reallines: %d\n",new_curr_realline,wblock->num_reallines);
  wblock->curr_realline = R_BOUNDARIES(0, new_curr_realline, wblock->num_reallines - 1);
}

static void set_curr_realline_from_place(const struct Tracker_Windows *window, struct WBlocks *wblock, Place *curr_place){
  if (curr_place!=NULL) {

    Place *curr_place2 = &wblock->reallines[wblock->curr_realline]->l.p;
    if (PlaceEqual(curr_place, curr_place2))
      return;
    
    set_curr_realline(wblock, FindRealLineFor(wblock, 0, curr_place));
  }
}

static struct LocalZooms **GenerateExpandedReallines(const struct Tracker_Windows *window, const struct WBlocks *wblock, int *num_new_reallines){
  struct LocalZooms *localzoom=wblock->localzooms;
    
  *num_new_reallines = FindNumberOfRealLines(localzoom,0);

  struct LocalZooms **reallines = talloc(*num_new_reallines * sizeof(struct LocalZooms *));
    
  UpdateRealLinesRec(reallines,localzoom,0);
  //wblock->num_reallines=UpdateRealLinesRec(wblock->reallines,localzoom,0);

  return reallines;
}

static struct LocalZooms **GenerateReallines(const struct Tracker_Windows *window, const struct WBlocks *wblock, int *num_new_reallines){
  if (wblock->num_expand_lines > 0)
    return GenerateExpandedReallines(window, wblock, num_new_reallines);

  struct LocalZooms **reallines = get_shrinked_reallines(window, wblock, -wblock->num_expand_lines, num_new_reallines);
  
  if (reallines==NULL) // 'get_shrinked_reallines' fails if trying to shrink too much.
    return GenerateExpandedReallines(window, wblock, num_new_reallines);
  else
    return reallines;
}

void UpdateRealLines(struct Tracker_Windows *window,struct WBlocks *wblock){
  Place *curr_place = NULL;

  if (wblock->reallines!=NULL && wblock->curr_realline<wblock->num_reallines)
    curr_place = &wblock->reallines[wblock->curr_realline]->l.p;

  {
    int num_reallines;
    struct LocalZooms **reallines = GenerateReallines(window, wblock, &num_reallines);

    //if(num_reallines==wblock->block->num_lines)
    //  wblock->zoomlinearea.width=0;

    PLAYER_lock();{
      wblock->reallines = reallines;
      wblock->num_reallines = num_reallines;
    }PLAYER_unlock();

    if(wblock->curr_realline >= wblock->num_reallines)
      wblock->curr_realline = wblock->num_reallines-1;
  }
  
  set_curr_realline_from_place(window, wblock, curr_place);

  UpdateWBlockCoordinates(window, wblock);
}



#if 0
int FindHighestLocalzoomLevel(struct WBlocks *wblock){
  int highest=0;
  int i;
  for(i=0;i<wblock->num_reallines;i++)
    highest = R_MAX(wblock->reallines[i]->level, highest);
  return highest;
}
#endif

#if 0
static int FindLargestZoomLineNum(struct WBlocks *wblock){
  int highest=0;
  int i;
  for(i=0;i<wblock->num_reallines;i++)
    if(wblock->reallines[i]->level>0)
      highest = R_MAX(wblock->reallines[i]->zoomline, highest);
  return highest;
}

void SetZoomLevelAreaWidth(const struct Tracker_Windows *window,
                           struct WBlocks *wblock)
{
  if(wblock->reallines==NULL){
    wblock->zoomlinearea.width = 0;
  }else{
    int largest = FindLargestZoomLineNum(wblock);
    if(largest==0 || largest==1)
      wblock->zoomlinearea.width = 0;
    else if(largest<10)
      wblock->zoomlinearea.width = window->fontwidth;
    else
      wblock->zoomlinearea.width = ((int)log10(largest)+1) * window->fontwidth;
  }
}
#endif

static void ExpandLineInternal(
                               const struct Tracker_Windows *window,
                               struct WBlocks *wblock,
                               int realline,
                               int num_newreallines,
                               bool autogenerated
){
	struct LocalZooms **reallines = wblock->reallines;
	struct LocalZooms  *localzoom = reallines[realline];
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
			lokke+localzoom->Tline,
                        autogenerated
		);
	}
}

static bool ensure_positive_expand_lines(struct WBlocks *wblock){
  if (wblock->num_expand_lines < 0) {
    GFX_Message(NULL, "Currently not possible to zoom in on single line when LZ (Line zoom) is less than 1/1");
    return false;
  }
    
  return true;
}

static void ExpandLine(
                       struct Tracker_Windows *window,
                       struct WBlocks *wblock,
                       int realline,
                       int num_newreallines
){

  if (!ensure_positive_expand_lines(wblock))
    return;

  ExpandLineInternal(window,wblock,realline,num_newreallines,false);

  UpdateRealLines(window,wblock);

  //UpdateWBlockCoordinates(window, wblock);
  //SetZoomLevelAreaWidth(window,wblock);

  MakeRangeLegal(wblock);
}

void ExpandLineCurrPos(
                       struct Tracker_Windows *window,
                       int num_newreallines
){


	struct WBlocks *wblock=window->wblock;

        if (!ensure_positive_expand_lines(wblock))
          return;

	Undo_Reallines_CurrPos(window);

        ExpandLine(window,wblock,wblock->curr_realline,num_newreallines);
          
        UpdateReallinesDependens(window,wblock);

	window->must_redraw = true;
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

  if (!ensure_positive_expand_lines(wblock))
    return;

  
	struct LocalZooms *localzoom=FindLocalZoomRoot(wblock,&realline);
	if(localzoom==NULL) return;

	localzoom->uplevel=NULL;

	set_curr_realline(wblock, realline+1);

	UpdateRealLines(window,wblock);


}

void UnexpandCurrPos(struct Tracker_Windows *window){

	struct WBlocks *wblock=window->wblock;
	int realline=wblock->curr_realline;

        if (!ensure_positive_expand_lines(wblock))
          return;

	Undo_Reallines_CurrPos(window);

        Unexpand(window,wblock,realline);
          
        UpdateReallinesDependens(window,wblock);

	window->must_redraw = true;

}


void Zoom(struct Tracker_Windows *window,struct WBlocks *wblock,int numtozoom){

    if (!ensure_positive_expand_lines(wblock))
    return;

	int curr_realline_org = wblock->curr_realline;
	int curr_realline     = curr_realline_org;
	int num_reallines     = wblock->num_reallines;
	int num_toexpand;
	//int zoomlineareawidth;

	//PlayStop();

	//zoomlineareawidth=wblock->zoomlinearea.width;

	Undo_Reallines_CurrPos(window);

        {

          Unexpand(window,wblock,curr_realline);

          curr_realline=wblock->curr_realline;

          num_toexpand=(num_reallines-wblock->num_reallines)+numtozoom+1;

          if(num_toexpand>1){

            ExpandLine(window,wblock,curr_realline,num_toexpand);

            set_curr_realline(wblock, curr_realline_org);

          }

#if 0
          if(wblock->num_reallines==wblock->block->num_lines){
		wblock->zoomlinearea.width=0;
          }
#endif

          if(true){ //zoomlineareawidth!=wblock->zoomlinearea.width){

            UpdateReallinesDependens(window,wblock);

          }else{

#if !USE_OPENGL
            PixMap_reset(window);
#endif
	  //	  GFX_FilledBox(window,0,0,0,window->width-1,window->height-1,PAINT_BUFFEr);
	  
            UpdateReallinesDependens(window,wblock);
#if !USE_OPENGL
            DrawUpAllWTracks(window,wblock,NULL);
            UpdateLeftSlider(window);

            EraseAllLines(window,wblock,
                          wblock->linenumarea.x,
                          wblock->temponodearea.x2
                        );
          /*
	  GFX_FilledBox(
                        window,0,
                        wblock->linenumarea.x,
                        wblock->t.y1,
                        wblock->temponodearea.x2,
                        wblock->t.y2,
                        PAINT_BUFFER
                        );
          */

            DrawWBlockSpesific(
                               window,
                               wblock,
                               wblock->top_realline,
                               wblock->bot_realline
                               );

	  
	  //UpdateAllWTracks(window,wblock,wblock->top_realline,wblock->bot_realline);
	  //	  DrawWBlock(window,wblock);
#endif
          }

        }

        window->must_redraw = true;
}

void LineZoomBlock(struct Tracker_Windows *window, struct WBlocks *wblock, int num_lines){
  int realline;

  if (num_lines==-1 || num_lines==0){
    if(num_lines==0)
      RError("num_expand_lines can not be 0 (divide by zero)");
    num_lines = 1;
  }

  Undo_Reallines_CurrPos(window);

  if (num_lines<1) {
    wblock->num_expand_lines = num_lines;
    UpdateRealLines(window,wblock);
    return;
  }

  if (num_lines==1)
    wblock->num_expand_lines = num_lines;

  R_ASSERT(wblock->curr_realline>=0);
  R_ASSERT(wblock->curr_realline<wblock->num_reallines);
  
  int curr_realline = wblock->curr_realline;
  Place curr_place = wblock->reallines[curr_realline]->l.p;

  {
    struct LocalZooms *localzoom = wblock->localzooms;
    while(localzoom != NULL) {
      if (localzoom->uplevel != NULL)
        if (localzoom->uplevel->autogenerated==true)
          localzoom->uplevel = NULL;
      localzoom = NextLocalZoom(localzoom);
    }
    
    UpdateRealLines(window,wblock);
    
    for(realline = wblock->num_reallines - 1; realline>=0 ; realline--){
      struct LocalZooms *localzoom=wblock->reallines[realline];
      
      if(localzoom->Tcounter==0 && localzoom->level==0){
        if(num_lines>1)
          ExpandLineInternal(window,wblock,realline,num_lines,true);
      }
    }

    UpdateRealLines(window,wblock);

    //UpdateWBlockCoordinates(window, wblock);
    //SetZoomLevelAreaWidth(window,wblock);

    MakeRangeLegal(wblock);

    UpdateReallinesDependens(window,wblock);

  }

  wblock->num_expand_lines = num_lines;

  set_curr_realline(wblock, (int)floorf(FindReallineForF(wblock, 0, &curr_place)));

  if (wblock->curr_realline <= wblock->num_reallines-2)
    if (wblock->reallines[wblock->curr_realline]->l.p.counter < curr_place.counter)
      if (wblock->reallines[wblock->curr_realline]->l.p.line == wblock->reallines[wblock->curr_realline+1]->l.p.line)
        set_curr_realline(wblock, wblock->curr_realline+1);

  window->must_redraw = true;
}

void LineZoomBlockInc(struct Tracker_Windows *window, struct WBlocks *wblock, int inc_num_lines){
  int num_expand_lines = wblock->num_expand_lines + inc_num_lines;

  if (num_expand_lines==0 || num_expand_lines==-1){ // 0 is a very illegal value (divide by zero), and -1 is the same as 1 (1/1 vs. 1).
    if (inc_num_lines > 0)
      num_expand_lines = 1;
    else
      num_expand_lines = -2;
  }

  printf("num_expand_lines: %d\n",num_expand_lines);
  
  LineZoomBlock(window,wblock,num_expand_lines);
}

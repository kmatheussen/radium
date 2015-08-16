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
#include "gfx_wblocks_proc.h"
#include "gfx_wtext_proc.h"
#include "list_proc.h"
#include "common_proc.h"
#include "gfx_subtrack_proc.h"
#include "cursor_proc.h"
#include "placement_proc.h"
#include "clipboard_range_calc_proc.h"
#include "wblocks_proc.h"
#include "blts_proc.h"
#include "nodeboxes_proc.h"
#include "nodelines_proc.h"
#include "tracks_proc.h"

#include "gfx_wtracks_proc.h"



extern struct Root *root;

#if !USE_OPENGL

extern char *NotesTexts3[131];
extern char *NotesTexts2[131];

/*
int OnC[4]={0,1,2,3};
int OffC[4]={3,2,1,0};
int *Col=OffC;

0,1,2 : 1
3   : 2
4   : 3
5   : 4
6   : 5
7   : 6
8,9,A : 7
*/
static const int Col[4]={0,1,2,3};
static const int NCol[11]={1,1,2,3,4,5,6,8,12,13,14};

void ClearTrack(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int start_realline,
	int end_realline
){

        if(window->must_redraw==true)
          return;

	if(WBlock_legalizeStartEndReallines(wblock,&start_realline,&end_realline)==false){
	  return;
	}

        EraseLines(window, wblock, 
                   R_MAX(wtrack->notearea.x,wblock->temponodearea.x2+2),
                   R_MIN(wtrack->fxarea.x2,wblock->a.x2),
                   start_realline, end_realline+1);
#if 0
        int lokke;

	for(lokke=start_realline;lokke<=end_realline;lokke++){
          int colornum = Col[0];
          
#if 0
          if (wtrack->track->patch!=NULL)
            colornum = wtrack->track->patch->colornum;

          printf("Clearing track with colornum %d %p\n",colornum,wtrack->track->patch);
#endif
	  GFX_T_FilledBox(
			  window,colornum,
			  R_MAX(wtrack->notearea.x,wblock->temponodearea.x2+2),
			  GetReallineY1Pos(window,wblock,lokke),
			  R_MIN(wtrack->fxarea.x2,wblock->a.x2),
			  GetReallineY2Pos(window,wblock,lokke),
                          PAINT_BUFFER
			  );
	}
	//	SetCursorPos(window);
#endif
}

void WTRACK_DrawTrackBorders(
			       struct Tracker_Windows *window,
			       struct WBlocks *wblock,
			       struct WTracks *wtrack,
			       int realline,
			       int start_subtrack,
			       int end_subtrack
){
  int lokke;
  int y1=GetReallineY1Pos(window,wblock,realline);
  int y2=GetReallineY2Pos(window,wblock,realline);

  if(wtrack->x2>wblock->temponodearea.x2+3 && wtrack->x2+2<wblock->a.x2)
    GFX_T_DrawTrackBorderDouble(
				window,
				wtrack->x2+1,
				y1,
				y2,
                                PAINT_BUFFER
				);

  if(wtrack->notearea.x2>wblock->temponodearea.x2 && wtrack->notearea.x2+1<wblock->a.x2)
    GFX_T_DrawTrackBorderSingle(
				window,
				wtrack->notearea.x2+1,
				y1,
				y2,
                                PAINT_BUFFER
				);

  for(lokke=start_subtrack>0 ? start_subtrack : 1;lokke<=end_subtrack;lokke++){
    GFX_T_DrawTrackBorderSingle(
				window,
				GetXSubTrack1(wtrack,lokke)-1,
				y1,
				y2,
                                PAINT_BUFFER
				);
  }
}

static void set_note_color(struct Tracker_Windows *window, int notenum){
  notenum = R_BOUNDARIES(0,notenum,127);
  const int split1 = 50;
  const int split2 = 95;
  if(notenum<split1)
    GFX_SetMixColor(window,5,1,scale(notenum,0,split1,0,1000));
  else if(notenum<split2)
    GFX_SetMixColor(window,6,5,scale(notenum,split1,split2,0,1000));
  else
    GFX_SetMixColor(window,2,6,scale(notenum,split2,160,0,1000));
}


static void draw_wtrack_text(struct Tracker_Windows *window,
                             struct WBlocks *wblock,
                             struct WTracks *wtrack,
                             int realline,
                             TBox within
                             )
{
  struct TrackRealline *trackrealline= &wtrack->trackreallines[realline];
  float notenum = trackrealline->note;
  //bool isgliding = false;
  int colnum = Col[1];

  if(trackrealline->pitch!=NULL){
    //isgliding = true;
    colnum = 5;
  }

  char **NotesTexts=wtrack->notelength==3?NotesTexts3:NotesTexts2;

  bool isranged=wblock->isranged && wblock->rangex1<=wtrack->l.num && wblock->rangex2>=wtrack->l.num;

  if(isranged){

    bool ranged=false,notranged=false;

    if(notenum!=0){
      if(notenum==NOTE_MUL){
        struct TrackReallineElements *element=trackrealline->trackreallineelements;
        while(element!=NULL){
          if(element->type==TRE_THISNOTELINES || element->type==TRE_STOPLINE){
            struct ListHeader3 *l=element->pointer;
            if( isPlaceRanged(wblock,&l->p) ){
              ranged=true;
            }else{
              notranged=true;
            }
            if(ranged && notranged) break;
          }
          element=element->next;
        }
        if(ranged && notranged){
          SetInvertTextLine(
                            window,
                            wblock,
                            colnum,
                            NotesTexts[NOTE_MUR],	//Is this possible? Will it ever happen?
                            wtrack->notearea.x,
                            realline,
                            true
                            );
        }else{
          if(ranged){
            SetInvertTextLine(
                              window,
                              wblock,
                              colnum,
                              NotesTexts[(int)notenum],
                              wtrack->notearea.x,
                              realline,
                              true
                              );
          }else{
            SetTextLine(
                        window,
                        wblock,
                        colnum,
                        NotesTexts[(int)notenum],
                        wtrack->notearea.x,
                        realline,
                        true
                        );
          }
        }
      }else{
        struct TrackReallineElements *element=trackrealline->trackreallineelements;
        while(element->type!=TRE_THISNOTELINES && element->type!=TRE_STOPLINE){
          element=element->next;
          if(element==NULL) // quick fix for bug. This code will be removed soon anyway.
            return;
        }
        struct ListHeader3 *l=element->pointer;
        if( isPlaceRanged(wblock,&l->p) ){
          SetInvertTextLine(
                            window,
                            wblock,
                            colnum,
                            NotesTexts[(int)notenum],
                            wtrack->notearea.x,
                            realline,
                            true
                            );
        }else{
          SetTextLine(
                      window,
                      wblock,
                      //								colnum,
                      notenum==NOTE_STP||notenum==NOTE_MUL ? 1 : notenum+16, //  NCol[notenum/12],
                      NotesTexts[(int)notenum],
                      wtrack->notearea.x,
                      realline,
                      true
                      );
        }
      }
    }else{
      if(realline>=wblock->rangey1 && realline<wblock->rangey2){
        SetInvertTextLineNotext(
                                window,wblock,colnum,
                                wtrack->notelength,
                                wtrack->notearea.x,
                                realline,
                                true
                                );
      }
    }

  }else{

    if(notenum!=0 && wtrack->noteshowtype==TEXTTYPE){

      if( (true || wblock->mouse_track!=wtrack->l.num) && notenum>0 && notenum<128){


        //GFX_SetMixColor(window,6,5,scale(notenum,0,127,0,1000));
        set_note_color(window,notenum);
        GFX_T_FilledBox(
                        window,
                        5,
                        wtrack->notearea.x,within.y1+2,
                        wtrack->notearea.x2-1,within.y2,
                        PAINT_BUFFER);
      }


      if (wblock->mouse_track == wtrack->l.num || wtrack->is_wide==true) {
          ///printf("notenum: %f\n",notenum);

        int cents = R_BOUNDARIES(0,(notenum - (int)notenum)*100,99);
        if (cents==0)

          GFX_T_Text(window,colnum,NotesTexts[(int)notenum],wtrack->notearea.x,within.y1,wtrack->notearea.x2,TEXT_CENTER,PAINT_BUFFER);

        else {
          GFX_T_Text(window,colnum,NotesTexts[(int)notenum],wtrack->notearea.x,within.y1,wtrack->notearea.x+3*12,TEXT_CENTER,PAINT_BUFFER);
          char temp[10];
          sprintf(temp,",%d",cents);
          GFX_T_Text(window,colnum,temp,wtrack->notearea.x+3*12,within.y1,wtrack->notearea.x2,TEXT_CENTER,PAINT_BUFFER);
        }

      } else {
        SetTextLine(
                    window,
                    wblock,
                    colnum, //(notenum==NOTE_STP || notenum==NOTE_MUL) ? 1 : notenum+16, //NCol[notenum/12],
                    NotesTexts[(int)notenum],
                    wtrack->notearea.x,
                    realline,
                    true
                    );
      }
    }
  }
}

static void draw_wtrack_peaks(struct Tracker_Windows *window,
                              struct WBlocks *wblock,
                              struct WTracks *wtrack,
                              int realline,
                              TBox within
                              )
{
  struct TrackRealline *trackrealline= &wtrack->trackreallines[realline];
  struct TrackReallineElements *element;

  for(element=trackrealline->trackreallineelements;element!=NULL;element=element->next){

    if(element->type==TRE_VELLINE){

      int x=GetXSubTrack1(wtrack,element->subtype);
      int x2=GetXSubTrack2(wtrack,element->subtype);

      // fill velocity area
      {

#if 0 // same color as note name. Cool, but messy
        GFX_SetMixColor(window, element->note->note+16, 15, 400);
#else
        //GFX_SetMixColor(window,6,5,scale(R_BOUNDARIES(0,element->note->note,127),0,127,0,1000));
        set_note_color(window,element->note->note);
        //GFX_SetMixColor(window, 5, 15, 100);
#endif

        GFX_Polygon(window,
                    1,
                    x, within.y1,
                    x2, within.y2,
                    4,
                    element->velocity_polygon,
                    PAINT_BUFFER
                    );
      }
            
      // fill wave data (if available)
      {
        int num_peaks = element->num_peaks;
        
        if(num_peaks>0){
        //GFX_SetMixColor(window, 5, 15, 100);
          
          GFX_Polygon(window,
                      0,
                      x+1, within.y1,
                      x2-1, within.y2,
                      num_peaks*2,
                      element->peaks[0],
                      PAINT_BUFFER
                      );
          
          // For stereo. This is the right channel.
          if(element->peaks[1] != NULL)
            GFX_Polygon(window,
                        0,
                        x+1, within.y1,
                        x2-1, within.y2,
                        num_peaks*2,
                        element->peaks[1],
                        PAINT_BUFFER
                        );

#if 0 // waveform border
          GFX_SetMixColor(window, 1, 15, 500);
          GFX_Polyline(window,
                      1,
                      x-1, within.y1,
                      x2-1, within.y2,
                      num_peaks,
                      element->peaks[0],
                      PAINT_BUFFER
                      );
          GFX_SetMixColor(window, 1, 15, 500);
          GFX_Polyline(window,
                      1,
                      x, within.y1,
                      x2, within.y2,
                      num_peaks,
                      &element->peaks[0][num_peaks],
                      PAINT_BUFFER
                      );
#endif // waveform border
        }
      }


      // velocity border
#if 1
      GFX_SetMixColor(window, 1, 15, 300);
      GFX_Polyline(window,
                   1,
                   x, within.y1,
                   x2, within.y2,
                   2,
                   &element->velocity_polygon[2],
                   PAINT_BUFFER
                   );
#else
      GFX_T_Line(window,
                 1,
                 scale(element->x1,0,1,x,x2),
                 scale(element->y1,0,1,within.y1,within.y2),
                 scale(element->x2,0,1,x,x2),
                 scale(element->y2,0,1,within.y1,within.y2),
                 PAINT_BUFFER);
#endif

    }
  }
}

#if 0
static void draw_skewed_box_old(struct Tracker_Windows *window,
                            int color,
                            int x1,int y1,int x2, int y2,
                            int where)
{
  GFX_SetMixColor(window, Col[1],5, 500);
  GFX_T_Line(window,color,
                 x1+1,y1+1,
                 x1+2,y2-1,
                 where);

  GFX_SetMixColor(window, Col[2],4, 500);
  GFX_T_Line(window,color,
                 x1+2,y2-1,
                 x2-1,y2-2,
                 where);

  GFX_SetMixColor(window, Col[2],4, 500);
  //GFX_SetMixColor(window, 8,9, 700);
  GFX_T_Line(window,color,
                 x2-1,y2-2,
                 x2-2,y1+2,
                 where);

  GFX_SetMixColor(window, 8,9, 700);
  GFX_T_Line(window,color,
                 x2-2,y1+2,
                 x1+1,y1+1,
                 where);
}
#endif

static void draw_skewed_box(struct Tracker_Windows *window,
                            int color,
                            int x1,int y1,int x2, int y2,
                            int where)
{
  // vertical left
  GFX_SetMixColor(window, color, 2, 100);
  GFX_T_Line(window,color,
                 x1+1,y1+1,
                 x1+2,y2-1,
                 where);

  // horizontal bottom
  GFX_SetMixColor(window, color, 1, 300);
  GFX_T_Line(window,color,
                 x1+2,y2-1,
                 x2-1,y2-2,
                 where);

  // vertical right
  GFX_SetMixColor(window, color,1, 400);
  //GFX_SetMixColor(window, 8,9, 700);
  GFX_T_Line(window,color,
                 x2-1,y2-2,
                 x2-2,y1+2,
                 where);

  // horizontal top
  GFX_SetMixColor(window, color, 2, 300);
  GFX_T_Line(window,color,
                 x2-2,y1+2,
                 x1+1,y1+1,
                 where);
}

static void draw_wtrack_notegraphics(struct Tracker_Windows *window,
                                     struct WBlocks *wblock,
                                     struct WTracks *wtrack,
                                     int realline,
                                     TBox within
                                     )
{
  struct TrackRealline *trackrealline= &wtrack->trackreallines[realline];
  struct TrackReallineElements *element;

  TBox within2;
  within2.y1=within.y1;
  within2.y2=within.y2;

  // Note graphics
  //
  for(element=trackrealline->trackreallineelements;element!=NULL;element=element->next){
    WArea warea2;
    TBox get;

    bool show_read_lines = wblock->mouse_track==wtrack->l.num && wblock->mouse_note==element->note;

    warea2.x=GetXSubTrack1(wtrack,element->subtype);
    warea2.x2=GetXSubTrack2(wtrack,element->subtype);
    warea2.width=warea2.x2-warea2.x;
    within2.x1=warea2.x;
    within2.x2=warea2.x2;

    //		  if(element->subtype>end_subtrack || element->subtype<start_subtrack) continue;
    switch(element->type){
    case TRE_THISNOTELINES:
      //					if(start_subtrack<=0)
      if(wtrack->noteshowtype==TEXTTYPE && element->subtype>0){ // Filter out subtype==0 here (subtype==subtrack), since THISNOTELINES is used for other things as well.
        GFX_T_Line(
                   window,13,//Col[3],
                   (int)(wtrack->fxarea.x+element->x1),
                   (int)(within2.y1+(element->y1*(within2.y2-within2.y1))),
                   R_MIN(wblock->t.x2,(int)(warea2.x+(warea2.width*element->x2))-1),
                   (int)(within2.y1+(element->y2*(within2.y2-within2.y1))),
                   PAINT_BUFFER
                   );
      }
      break;


#define USE_TRIANGLE 0

    case TRE_VELLINEEND:
#define  dasize (int)GetNodeSize(window,wblock,wtrack)
      GetNodeBox_customsize(element,&warea2,&within2,&get,dasize*3/2,dasize*2/3);
      if(show_read_lines){
#if USE_TRIANGLE 
        GFX_T_Line(window,Col[3], (get.x1+get.x2)/2, get.y2, get.x1, get.y1, PAINT_BUFFER);
        GFX_T_Line(window,Col[3], (get.x1+get.x2)/2, get.y2, get.x2, get.y1, PAINT_BUFFER);
        GFX_T_Line(window,Col[3], get.x1, get.y1, get.x2, get.y1, PAINT_BUFFER);
#else
        //GFX_T_Line(window,Col[3], get.x1, get.y2, get.x2, get.y2, PAINT_BUFFER);
        //GFX_T_Box(window,Col[3],get.x1,get.y1,get.x2,get.y2, PAINT_BUFFER);
        draw_skewed_box(window,5,get.x1,get.y1,get.x2,get.y2, PAINT_BUFFER);
        //GFX_T_FilledBox(window,Col[2],get.x1+1,get.y1+1,get.x2-1,get.y2-1, PAINT_BUFFER);
#endif
      }
      break;
    case TRE_VELLINESTART:
      GetNodeBox_customsize(element,&warea2,&within2,&get,dasize*3/2,dasize*2/3);
      if(show_read_lines){
#if USE_TRIANGLE
        GFX_T_Line(window,Col[3], (get.x1+get.x2)/2, get.y1, get.x1, get.y2, PAINT_BUFFER);
        GFX_T_Line(window,Col[3], (get.x1+get.x2)/2, get.y1, get.x2, get.y2, PAINT_BUFFER);
        GFX_T_Line(window,Col[3], get.x1, get.y2, get.x2, get.y2, PAINT_BUFFER);
#else
        //GFX_T_Box(window,Col[3],get.x1,get.y1,get.x2,get.y2, PAINT_BUFFER);
        draw_skewed_box(window,4,get.x1,get.y1,get.x2,get.y2, PAINT_BUFFER);
        //GFX_T_FilledBox(window,Col[2],get.x1+1,get.y1+1,get.x2-1,get.y2-1, PAINT_BUFFER);
#endif
      }
      break;
    case TRE_VELLINENODE:
      GetNodeBox(window,wblock,wtrack,element,&warea2,&within2,&get);
      if(show_read_lines){
        draw_skewed_box(window,8,get.x1,get.y1,get.x2,get.y2, PAINT_BUFFER);
        //GFX_T_Box(window,Col[3],get.x1,get.y1,get.x2,get.y2, PAINT_BUFFER);
        //GFX_T_FilledBox(window,Col[2],get.x1+1,get.y1+1,get.x2-1,get.y2-1, PAINT_BUFFER);
      }
      break;

    case TRE_VELLINE:

      if(wtrack->noteshowtype!=TEXTTYPE){
        struct Notes *note=element->pointer;

        // Graphical representation of note
        GFX_T_Line(
                   window,
                   note->note+16, //NCol[note->note/12],
                   (int)(wtrack->notearea.x+(wtrack->notearea.x2-wtrack->notearea.x)*(note->note-((note->note/12)*12))/12),
                   (int)(within.y1+(element->y1*(within.y2-within.y1))),
                   (int)(wtrack->notearea.x+(wtrack->notearea.x2-wtrack->notearea.x)*(note->note-((note->note/12)*12))/12),
                   (int)(within.y1+(element->y2*(within.y2-within.y1))),
                   PAINT_BUFFER
                   );
      }
#if 0
      // drawn up in draw_wtrack_peaks (drawn up by a polygon instead now)
      GetNodeLine(element,&warea2,&within2,&get);
      GFX_T_Line(window,Col[1],get.x1,get.y1,get.x2,get.y2,PAINT_BUFFER);
#endif

      break;
    case TRE_STOPLINE:
      GFX_T_Line(
                 window,Col[1],
                 wtrack->fxarea.x,
                 (int)(within.y1+element->y1),
                 wtrack->fxarea.x2,
                 (int)(within.y1+element->y1),
                 PAINT_BUFFER
                 );
      break;
    case TRE_THISPITCHLINES:
      GFX_SetMixColor(window, 11, 1, 800);
      GFX_T_Line(
                 window,11,
                 wtrack->fxarea.x,
                 (int)(within.y1+element->y1),
                 wtrack->fxarea.x2,
                 (int)(within.y1+element->y1),
                 PAINT_BUFFER
                 );
      break;
    case TRE_REALSTARTSTOP:
      {
        struct Notes *note=element->pointer;
        GFX_SetMixColor(window, 11, 1, 800);
        GFX_T_Line(
      //window,Col[2],
                   window,11,
                   wtrack->noteshowtype==TEXTTYPE ? wtrack->notearea.x //within2.x1
                                                  : wtrack->notearea.x+(wtrack->notearea.x2-wtrack->notearea.x)*(note->note-((note->note/12)*12))/12,
                   (int)(within2.y1+(element->y1*(within2.y2-within2.y1))),
                   within2.x2,
                   (int)(within2.y1+(element->y1*(within2.y2-within2.y1))),
                   PAINT_BUFFER
                   );
      }
      break;
    }
  }
}

static void draw_wtrack_fxgraphics(struct Tracker_Windows *window,
                                     struct WBlocks *wblock,
                                     struct WTracks *wtrack,
                                     int realline,
                                     TBox within
                                     )
{
  TBox get;

  WArea warea;
  warea.x=wtrack->fxarea.x;
  warea.x2=wtrack->fxarea.x2;
  warea.width=warea.x2-warea.x;

  bool show_read_lines = wblock->mouse_track==wtrack->l.num;

  WFXNodes *wfxnode=wtrack->wfxnodes[realline];
  while(wfxnode!=NULL){
    switch(wfxnode->type){
    case TRE_FXLINE:
      GetNodeLine(wfxnode,&warea,&within,&get);
      GFX_T_Line(window,wfxnode->subtype,get.x1,get.y1,get.x2,get.y2, PAINT_BUFFER);

      /*
        GFX_T_Line(
        window,wfxnode->subtype, //Col[2],
        wtrack->fxarea.x+wfxnode->x1,
        GetReallineY1Pos(window,wblock,lokke)+wfxnode->y1,
        wtrack->fxarea.x+wfxnode->x2,
        GetReallineY1Pos(window,wblock,lokke)+wfxnode->y2,
        PAINT_BUFFER
        );
      */
      break;
    case TRE_FXNODE:
      /*
        GFX_T_FilledBox(
        window,0,//wfxnode->subtype, //Col[3],
        wtrack->fxarea.x+wfxnode->x1+1,
        GetReallineY1Pos(window,wblock,lokke)+wfxnode->y1+1,
        wtrack->fxarea.x+wfxnode->x2-1,
        GetReallineY1Pos(window,wblock,lokke)+wfxnode->y2-1,
        PAINT_BUFFER
        );
      */
      if(show_read_lines){
        GetNodeBox(window,wblock,wtrack,wfxnode,&warea,&within,&get);
        //GFX_T_Box(window,wfxnode->subtype,get.x1,get.y1,get.x2,get.y2, PAINT_BUFFER);

        draw_skewed_box(window,wfxnode->subtype,get.x1,get.y1,get.x2,get.y2,PAINT_BUFFER);
#if 0
        GFX_T_Line(window,wfxnode->subtype,
                   get.x1+1,get.y1+1,
                   get.x1+2,get.y2-1,
                   PAINT_BUFFER);

        GFX_T_Line(window,wfxnode->subtype,
                   get.x1+2,get.y2-1,
                 get.x2-1,get.y2-2,
                   PAINT_BUFFER);
        
        GFX_T_Line(window,wfxnode->subtype,
                   get.x2-1,get.y2-2,
                   get.x2-2,get.y1+2,
                   PAINT_BUFFER);
        
        GFX_T_Line(window,wfxnode->subtype,
                   get.x2-2,get.y1+2,
                   get.x1+1,get.y1+1,
                   PAINT_BUFFER);
#endif
      }

      /*
        GFX_T_Box(
        //						window,wfxnode->subtype==1?2:wfxnode->subtype>=4?wfxnode->subtype%3+1==1?2:Col[1]:Col[1],
        window,wfxnode->subtype,
        //						window,1,
        wtrack->fxarea.x+wfxnode->x1,
        GetReallineY1Pos(window,wblock,lokke)+wfxnode->y1,
        wtrack->fxarea.x+wfxnode->x2,
        GetReallineY1Pos(window,wblock,lokke)+wfxnode->y2,
        PAINT_BUFFER
        );
      */
      break;
    }
    wfxnode=wfxnode->next;
  }
}

static void draw_wtrack_pitches(struct Tracker_Windows *window,
                                struct WBlocks *wblock,
                                struct WTracks *wtrack,
                                int realline,
                                TBox within
                                )
{
  TBox get;

  TBox within2;
  within2.y1=within.y1;
  within2.y2=within.y2;
  within2.x1 = wtrack->x;
  within2.x2 = wtrack->fxarea.x - 2;

  WArea warea;
  warea.x=within2.x1;
  warea.x2=within2.x2;
  warea.width=warea.x2-warea.x;

  bool show_read_lines = wblock->mouse_track==wtrack->l.num;

  WPitches *wpitch=wtrack->wpitches[realline];
  while(wpitch!=NULL){
    switch(wpitch->type){
    case TRE_PITCHLINE:
      if(wpitch->x1 != wpitch->x2 || show_read_lines) {
        GetNodeLine(wpitch,&warea,&within2,&get);
        GFX_T_Line(window, 7, get.x1,get.y1, get.x2,get.y2, PAINT_BUFFER);
      }
      break;
    case TRE_PITCHNODE:
      if(false && show_read_lines){
        GetNodeBox(window,wblock,wtrack,wpitch,&warea,&within2,&get);
        draw_skewed_box(window, 5, get.x1,get.y1, get.x2,get.y2, PAINT_BUFFER);
      }

      break;
    }
    wpitch=wpitch->next;
  }
}


void UpdateWTrack(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int start_realline,
	int end_realline
){
	int realline;

	TBox within;

	int start_subtrack= -1;
	int end_subtrack;

        if(window->must_redraw==true)
          return;

	if(WBlock_legalizeStartEndReallines(wblock,&start_realline,&end_realline)==false){
	  return;
	}

	within.x1=wtrack->fxarea.x;
	within.x2=wtrack->fxarea.x2;

        if(within.x2<=wblock->temponodearea.x2)
          return;

#if 0
        // hack. This function should not be called when this is true.
        if(wtrack->x >= window->width)
          return;
#endif

	if(wtrack->l.num==wblock->left_track){
		start_subtrack=wblock->left_subtrack;
	}

	if(wtrack->l.num==wblock->right_track){
	  end_subtrack=wblock->right_subtrack;
	  // end_subtrack=wtrack->num_vel-1;
	}else{
		end_subtrack=wtrack->num_vel-1;
	}

	for(realline=start_realline;realline<=end_realline;realline++){
	  within.y1=GetReallineY1Pos(window,wblock,realline);
	  within.y2=GetReallineY2Pos(window,wblock,realline);

          GFX_SetClipRect(window,R_MAX(within.x1,wblock->temponodearea.x2),within.y1,within.x2,within.y2+1,PAINT_BUFFER);
          {
            draw_wtrack_peaks(window,wblock,wtrack,realline,within);
            draw_wtrack_fxgraphics(window,wblock,wtrack,realline,within);
          }
          GFX_CancelClipRect(window,PAINT_BUFFER);

          draw_wtrack_notegraphics(window,wblock,wtrack,realline,within);
          draw_wtrack_text(window,wblock,wtrack,realline,within);

          //GFX_SetClipRect(window,wtrack->x,within.y1,within.x1,within.y2+1,PAINT_BUFFER);
          {
            draw_wtrack_pitches(window,wblock,wtrack,realline,within);
          }
          //GFX_CancelClipRect(window,PAINT_BUFFER);

          WTRACK_DrawTrackBorders(window,wblock,wtrack,realline,start_subtrack,end_subtrack);
	}

	Blt_markSTrack(window,wtrack->l.num,start_realline,end_realline);
}


void DrawUpWTrack(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack
	){
  ClearTrack(window,wblock,wtrack,wblock->top_realline,wblock->bot_realline);
  UpdateWTrack(window,wblock,wtrack,wblock->top_realline,wblock->bot_realline);
}

void DrawUpAllWTracks(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
        struct Patch *patch // Filter. If patch==NULL, draw up everything
){
	struct WTracks *wtrack=ListFindElement1(&wblock->wtracks->l,wblock->left_track);

	while(wtrack!=NULL && wtrack->l.num<=wblock->right_track){
          if(wtrack->track->patch==patch || patch==NULL)
            DrawUpWTrack(window,wblock,wtrack);
	  wtrack=NextWTrack(wtrack);
	}
}


void DrawUpAllPeakWTracks(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
        struct Patch *patch // Filter. If patch==NULL, draw up everything
){
	struct WTracks *wtrack=ListFindElement1(&wblock->wtracks->l,wblock->left_track);

	while(wtrack!=NULL && wtrack->l.num<=wblock->right_track){
          if(wtrack->track->patch==patch || patch==NULL)
            if(TRACK_has_peaks(wtrack->track))
              DrawUpWTrack(window,wblock,wtrack);
	  wtrack=NextWTrack(wtrack);
	}
}


/*******************************************************
   Draw Up all visible tracks.
*******************************************************/
void UpdateAllWTracks(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	int start_realline,
	int end_realline
){
	struct WTracks *wtrack=ListFindElement1(&wblock->wtracks->l,wblock->left_track);

	while(wtrack!=NULL && wtrack->l.num<=wblock->right_track){
          UpdateWTrack(window,wblock,wtrack,start_realline,end_realline);
          wtrack=NextWTrack(wtrack);
	}
}


void UpdateAndClearSomeWTracks(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	NInt starttrack,
	NInt endtrack,
	int start_realline,
	int end_realline
){
	NInt lokke;

	struct WTracks *wtrack=ListFindElement1(&wblock->wtracks->l,starttrack);
	if(wtrack==NULL) return;

	endtrack=R_MIN(endtrack,wblock->right_track);

	for(lokke=0;lokke<=endtrack-starttrack;lokke++){
		ClearTrack(window,wblock,wtrack,start_realline,end_realline);
		UpdateWTrack(window,wblock,wtrack,start_realline,end_realline);
		wtrack=NextWTrack(wtrack);
		if(wtrack==NULL) break;
	}

}



#endif // !USE_OPENGL

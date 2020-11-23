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
#include "vector_proc.h"
#include "windows_proc.h"
#include "sliders_proc.h"
#include "visual_proc.h"
#include "common_proc.h"
#include "wtracks_proc.h"
#include "wblocks_proc.h"
#include "pixmap_proc.h"
#include "scroll_proc.h"
#include "scroll_play_proc.h"
#include "list_proc.h"
#include "trackreallines2_proc.h"
#include "centtext_proc.h"
#include "chancetext_proc.h"
#include "veltext_proc.h"
#include "fxtext_proc.h"
#include "swingtext_proc.h"
#include "realline_calc_proc.h"
#include "../OpenGL/Widget_proc.h"

#include "../api/api_proc.h"

#include "cursor_updown_proc.h"

extern int g_downscroll;

int getScrollMultiplication(void){
  if (doScrollEditLines())
    return R_MAX(1, g_downscroll);
  else
    return 1;
}

bool ScrollEditorDown(struct Tracker_Windows *window,int num_lines, const struct Notes *dont_play_this_note){
	struct WBlocks *wblock = window->wblock;

	if(num_lines==0){
          int start_realline = wblock->curr_realline;
          int end_realline = wblock->curr_realline + 1;
          Scroll_play_down(wblock,start_realline,end_realline, dont_play_this_note);
          return true;
        }
        
        bool ret = false;
        
	if(
		wblock->curr_realline<wblock->num_reallines-1 &&
		wblock->curr_realline+num_lines > wblock->num_reallines-1
	){
		num_lines=wblock->num_reallines - wblock->curr_realline;
	}

	if(num_lines/getScrollMultiplication()==1 || num_lines/getScrollMultiplication()==-1){
          Scroll_play_down(wblock,wblock->curr_realline,wblock->curr_realline+num_lines-1, dont_play_this_note);
          ret = true;
        }

	if(wblock->curr_realline < wblock->num_reallines-1) {

          num_lines = R_MIN(num_lines, wblock->num_reallines-1-wblock->curr_realline); //wblock->curr_realline+num_lines < wblock->num_reallines){
	  Scroll_scroll(window,num_lines);
          
	}else{
		/* When on the bottom line. */

//		RError("top: %d, num/2: %d\n",wblock->top_realline,wblock->num_visiblelines/2);
		if(wblock->top_realline <= wblock->num_visiblelines/2){
//			RError("jepp\n");
			Scroll_scroll(window,-wblock->curr_realline);

		}else{

                  if (wblock->curr_realline == wblock->num_reallines-1)
                    wblock->curr_realline=0;
                  else{
                    ScrollEditorDown(window,  wblock->num_reallines - wblock->curr_realline - 1, dont_play_this_note);
                    return ret;
                  }
                        GE_set_curr_realline(0);

			SetWBlock_Top_And_Bot_Realline(window,wblock);

#if !USE_OPENGL
			PixMap_reset(window);

                        struct WTracks *wtrack=ListLast1(&wblock->wtracks->l);
                        int x2=wtrack->fxarea.x2;

                        /*
			GFX_FilledBox(
				window,
				0,
				wblock->a.x1,wblock->t.y1,
				x2,wblock->t.y2,
                                PAINT_BUFFER
			);
                        */
                        EraseAllLines(window, wblock, wblock->a.x1, x2);

			DrawWBlockSpesific(
				window,
				wblock,
				0,
				wblock->num_visiblelines
			);

			UpdateAllWTracks(
				window,
				wblock,
				0,
				wblock->num_visiblelines
			);
			/*
			GFX_FilledBox(
				      window,
				      0,
				      wblock->a.x1,wblock->t.y1,
				      wblock->t.x2,
				      Common_oldGetReallineY2Pos(window,wblock,wblock->curr_realline-1)
				      );
			*/
#endif
		}
	}

#if !USE_OPENGL
	Blt_scrollMark(window);

	UpdateLeftSlider(window);
#endif

        return ret;
}


bool MaybeScrollEditorDownAfterEditing(struct Tracker_Windows *window, const struct Notes *dont_play_this_note){
  if(!is_playing_current_block() || ATOMIC_GET(root->play_cursor_onoff)==true)
    return ScrollEditorDown(window,g_downscroll, dont_play_this_note);
  
  return false;
}

void ScrollEditorUp(struct Tracker_Windows *window,int num_lines){
	struct WBlocks *wblock;

	if(num_lines==0) return;

	wblock=window->wblock;

	if(wblock->curr_realline>0 && wblock->curr_realline-num_lines<0){
		num_lines = wblock->curr_realline;
	}

	if(num_lines/getScrollMultiplication()==1 || num_lines/getScrollMultiplication()==-1)
          Scroll_play_up(wblock,wblock->curr_realline-num_lines+1,wblock->curr_realline);

	if(wblock->curr_realline-num_lines>=0){

	  Scroll_scroll(window,-num_lines);
          
	}else{

		if(wblock->bot_realline >= (wblock->num_reallines-(wblock->num_visiblelines/2)-1)){
			Scroll_scroll(window,wblock->num_reallines-1);
		}else{
			wblock->curr_realline=wblock->num_reallines-1;
                        GE_set_curr_realline(wblock->curr_realline);

			SetWBlock_Top_And_Bot_Realline(window,wblock);

#if !USE_OPENGL
			PixMap_reset(window);

                        struct WTracks *wtrack=ListLast1(&wblock->wtracks->l);
                        int x2=wtrack->fxarea.x2;

                        EraseAllLines(window, wblock, wblock->a.x1, x2);

			DrawWBlockSpesific(
				window,
				wblock,
				wblock->top_realline,
				wblock->curr_realline
			);

			UpdateAllWTracks(
				window,
				wblock,
				wblock->top_realline,
				wblock->curr_realline
			);

			/*
			GFX_FilledBox(
				      window,
				      0,
				      wblock->a.x1,Common_oldGetReallineY1Pos(window,wblock,wblock->curr_realline+1),
				      wblock->t.x2,
				      wblock->t.y2
			      );
			*/
#endif
		}
	}

#if !USE_OPENGL
	Blt_scrollMark(window);

	UpdateLeftSlider(window);
#endif
}

static QMap<int,bool> get_list3_trss(struct Tracker_Windows *window, struct WBlocks *wblock, struct ListHeader3 *l){
  QMap<int,bool> ret;
  int realline = 0;
  while(l != NULL){
    realline = FindRealLineFor(wblock,realline,&l->p);
    ret[realline] = true;
    l = l->next;
  }
  return ret;
}


template <class T>
static void scroll_next(struct Tracker_Windows *window, struct WBlocks *wblock, T *t){
  scroll_next(window, wblock, get_list3_trss(window, wblock, LCAST(t)));
}

template <class T>
static void scroll_next(struct Tracker_Windows *window, struct WBlocks *wblock, const QMap<int, T> &trss){
	int curr_realline=wblock->curr_realline;

        if(curr_realline==wblock->num_reallines-1){ // last line
          ScrollEditorDown(window,1,NULL);
          return;
        }

	int new_realline;

        for(new_realline=curr_realline+1 ; new_realline < wblock->num_reallines-1 ; new_realline++)
          if (trss.contains(new_realline))
            break;

	ScrollEditorDown(window,new_realline-curr_realline,NULL);
}

template <class T>
static void scroll_prev(struct Tracker_Windows *window, struct WBlocks *wblock, const QMap<int, T> &trss){
	int curr_realline=wblock->curr_realline;

        if (curr_realline==0){
          ScrollEditorUp(window,1);
          return;
	}

        int new_realline;

        for(new_realline=curr_realline-1 ; new_realline > 0 ; new_realline--)
          if (trss.contains(new_realline))
            break;

	ScrollEditorUp(window,curr_realline-new_realline);
}

template <class T>
static void scroll_prev(struct Tracker_Windows *window, struct WBlocks *wblock, T *t){
  scroll_prev(window, wblock, get_list3_trss(window, wblock, LCAST(t)));
}

template <class T>
static void scroll_nextprev(struct Tracker_Windows *window, struct WBlocks *wblock, bool do_next, const QMap<int, T> &trss){
  if (do_next)
    scroll_next(window, wblock, trss);
  else
    scroll_prev(window, wblock, trss);
}

template <class T>
static void scroll_nextprev(struct Tracker_Windows *window, struct WBlocks *wblock, bool do_next, T *t){
  scroll_nextprev(window, wblock, do_next, get_list3_trss(window, wblock, LCAST(t)));
}


void ScrollEditorNextNote(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack){
        const Trss &trss = TRSS_get(wblock, wtrack);

        scroll_next(window, wblock, trss);
}

void ScrollEditorPrevNote(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack){
        const Trss &trss = TRSS_get(wblock, wtrack);
        scroll_prev(window, wblock, trss);
}

static Waveform_trss get_waveform_trss(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack, int polyphony_num){
  Waveform_trss trss;
  
  struct Notes *note = wtrack->track->notes;
  
  while(note != NULL){
    if (note->polyphony_num == polyphony_num) {
      int realline = FindRealLineForNote(wblock, 0, note);
      trss[realline] = true;
      int realline2 = FindRealLineForEndNote(wblock, 0, note);
      trss[realline2] = true;
    }
    note = NextNote(note);
  }

  return trss;
}

void ScrollEditorNextWaveform(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack, int polyphony_num){
  const Waveform_trss &trss = get_waveform_trss(window, wblock, wtrack, polyphony_num);
  scroll_next(window, wblock, trss);
}

void ScrollEditorPrevWaveform(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack, int polyphony_num){
  const Waveform_trss &trss = get_waveform_trss(window, wblock, wtrack, polyphony_num);
  scroll_prev(window, wblock, trss);
}

void ScrollEditorNextVelocity(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack){
  const VelText_trss &trss = VELTEXTS_get(wblock, wtrack);
  scroll_next(window, wblock, trss);
}

void ScrollEditorPrevVelocity(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack){
  const VelText_trss &trss = VELTEXTS_get(wblock, wtrack);
  scroll_prev(window, wblock, trss);
}

void ScrollEditorNextFx(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack, struct FXs *fxs){
  const FXText_trss &trss = FXTEXTS_get(wblock, wtrack, fxs);
  scroll_next(window, wblock, trss);
}

void ScrollEditorPrevFx(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack, struct FXs *fxs){
  const FXText_trss &trss = FXTEXTS_get(wblock, wtrack, fxs);
  scroll_prev(window, wblock, trss);
}

static void ScrollEditorPrevNext(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack, bool do_next){
  switch(window->curr_track){
      
    case SWINGTRACK:        
      scroll_nextprev(window, wblock, do_next, wblock->block->swings);
      break;
        
    case SIGNATURETRACK:
      scroll_nextprev(window, wblock, do_next, wblock->block->signatures);
      break;
        
    case LPBTRACK:
      scroll_nextprev(window, wblock, do_next, wblock->block->lpbs);
      break;
        
    case TEMPOTRACK:
      scroll_nextprev(window, wblock, do_next, wblock->block->tempos);
      break;
        
    case TEMPONODETRACK:
      scroll_nextprev(window, wblock, do_next, wblock->block->temponodes);
      break;
        
    default:

      R_ASSERT_NON_RELEASE(window->curr_track >= 0);
      
      if (window->curr_track < 0)
        return;
      
      struct Tracks *track = wtrack->track;
      struct FXs *fxs;

      if (SWINGTEXT_subsubtrack(window, wtrack) >= 0){
        scroll_nextprev(window, wblock, do_next, track->swings);

      } else if (FXTEXT_subsubtrack(window, wtrack, &fxs) >= 0){      
        scroll_nextprev(window, wblock, do_next, FXTEXTS_get(wblock, wtrack, fxs));
          
      } else if (VELTEXT_subsubtrack(window, wtrack) >= 0){      
        scroll_nextprev(window, wblock, do_next, VELTEXTS_get(wblock, wtrack));
        
      } else if (window->curr_track_sub==-1 || CENTTEXT_subsubtrack(window, wtrack)!=-1 || CHANCETEXT_subsubtrack(window, wtrack)!=-1){
        scroll_nextprev(window, wblock, do_next, TRSS_get(wblock, wtrack));
        
      } else {
        
        int curr_polyphony_num = window->curr_track_sub - WTRACK_num_non_polyphonic_subtracks(wtrack);

        if (curr_polyphony_num >= 0){
          
          scroll_nextprev(window, wblock, do_next, get_waveform_trss(window, wblock, wtrack, curr_polyphony_num));
          
        } else {

          R_ASSERT_NON_RELEASE(false);
          
        }

      }
      break;
  }
}

void ScrollEditorNextSomething(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack){
  ScrollEditorPrevNext(window, wblock, wtrack, true);
}

void ScrollEditorPrevSomething(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack){
  ScrollEditorPrevNext(window, wblock, wtrack, false);
}

void ScrollEditorToRealLine(
	struct Tracker_Windows *window,
	//struct WBlocks *wblock,
	int till_curr_realline
){
        struct WBlocks *wblock = window->wblock;
  
	int curr_realline=wblock->curr_realline;

        //printf("Going to scroll to line %d. Now: %d \n",till_curr_realline,curr_realline);

/*
	if(till_curr_realline<0){
		till_curr_realline=wblock->num_reallines-1;
	}

	if(till_curr_realline>=wblock->num_reallines){
		till_curr_realline=0;
	}
*/

	if( till_curr_realline < curr_realline ){
		ScrollEditorUp(
			window,
			curr_realline - till_curr_realline
		);
	}else{
		if( till_curr_realline > curr_realline ){
			ScrollEditorDown(
				window,
				till_curr_realline - curr_realline,
                                NULL
			);
		}
	}
}

void ScrollEditorToRealLine_CurrPos(
	struct Tracker_Windows *window,
	int till_curr_realline
){
  ScrollEditorToRealLine(window,/*window->wblock,*/till_curr_realline);
}

/*
void ScrollEditor(
	struct Tracker_Windows *window,
	int num_reallines
){
	struct WBlocks *wblock=window->wblock;
	int curr_realline=wblock->curr_realline;

	ScrollEditorToRealLine(window,wblock,curr_realline+num_reallines);
}
*/

void ScrollEditorToLine_CurrPos(
	struct Tracker_Windows *window,
	int line
){
	struct WBlocks *wblock=window->wblock;

        int realline = FindRealLineFor(wblock, 0, PlaceCreate2(line));
	ScrollEditorToRealLine(window,/*wblock,*/realline);
}

void ScrollEditorToPercentLine_CurrPos(
	struct Tracker_Windows *window,
	int percent
){
	struct WBlocks *wblock=window->wblock;
	struct Blocks *block=wblock->block;

	int line=block->num_lines*percent/100;

	ScrollEditorToLine_CurrPos(window,line);
}

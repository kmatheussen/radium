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
#include "common_proc.h"
#include "gfx_subtrack_proc.h"
#include "list_proc.h"
#include "windows_proc.h"
#include "wtracks_proc.h"
#include "gfx_wblocks_proc.h"
#include "sliders_proc.h"
#include "blts_proc.h"
#include "wblocks_proc.h"
#include "gfx_wtrackheaders_proc.h"
#include "../midi/midi_i_input_proc.h"
#include "notes_proc.h"
#include "veltext_proc.h"
#include "fxtext_proc.h"
#include "centtext_proc.h"
#include "chancetext_proc.h"
#include "bpmtext_proc.h"
#include "lpbtext_proc.h"
#include "swingtext_proc.h"
#include "signaturetext_proc.h"

#include "../api/api_proc.h"

#include "cursor_proc.h"


void R_SetCursorPos(struct Tracker_Windows *window){
#if !USE_OPENGL
	struct WBlocks *wblock=window->wblock;
	Blt_markAll(window,LINENUMBTRACK,wblock->right_track,wblock->curr_realline,wblock->curr_realline);
#endif
}

static bool nextTrackHasSwingtext(struct WTracks *wtrack){
  R_ASSERT_RETURN_IF_FALSE2(wtrack!=NULL, false);

  struct WTracks *next = NextWTrack(wtrack);
  if (next==NULL)
    return false;

  return next->swingtext_on;
}

int CursorRight(struct Tracker_Windows *window,struct WBlocks *wblock){
	struct WTracks *wtrack=wblock->wtrack;
	//struct WTracks *leftwtrack;
	//struct WTracks *rightwtrack;
	int update=0;
	//int x2;
        
	if(window->curr_track>=0){

		window->curr_track_sub++;

                if (wtrack->swingtext_on==true && window->curr_track_sub==3)
                  window->curr_track_sub = -1;

                if (wtrack->swingtext_on==true && window->curr_track_sub==0)
                  window->curr_track_sub = 3;

                int num_subtracks = GetNumSubtracks(wtrack);

		if(window->curr_track_sub>=num_subtracks){
			window->curr_track++;
			if(NextWTrack(wtrack)==NULL){
				window->curr_track--;
				window->curr_track_sub--;
				return 0;
			}else{

                          if (nextTrackHasSwingtext(wtrack))
                            window->curr_track_sub = 0;
                          else
                            window->curr_track_sub = -1;

                          ATOMIC_WRITE(wblock->wtrack, NextWTrack(wtrack));
			}
		}

                /*
		while(
			window->curr_track>wblock->right_track
			||
			(
			 window->curr_track==wblock->right_track
			 && window->curr_track_sub>wblock->right_subtrack
			 )
		){
			leftwtrack=ListFindElement1(&wblock->wtracks->l,wblock->left_track);
                        int num_subtracks = GetNumSubtracks(leftwtrack);
			wblock->left_subtrack++;
			if(wblock->left_subtrack>=num_subtracks){
                          if (wblock->left_track < wblock->block->num_tracks-1) {
                            wblock->left_subtrack= -1;
                            wblock->left_track++;
                            //return 0;
                          } else {
                            UpdateAllWTracksCoordinates(window,wblock);
                            wblock->left_subtrack--;
                            return 1;
                          }
			}
			leftwtrack=ListFindElement1(&wblock->wtracks->l,wblock->left_track);
			if(
				wblock->left_track==wblock->block->num_tracks-1 &&
				wblock->left_subtrack==num_subtracks-1
			){
                                UpdateAllWTracksCoordinates(window,wblock);
				return 2;
			}
			UpdateAllWTracksCoordinates(window,wblock);
			update=1;
		}
                */
                /*
		for(;;){
		  rightwtrack=ListFindElement1(&wblock->wtracks->l,window->curr_track);
                  int num_subtracks = GetNumSubtracks(rightwtrack);
		  x2=GetXSubTrack2(rightwtrack,window->curr_track_sub);
		  if(x2>wblock->a.x2){
			leftwtrack=ListFindElement1(&wblock->wtracks->l,wblock->left_track);
			wblock->left_subtrack++;
			if(wblock->left_subtrack>=num_subtracks){
                          if (wblock->left_track < wblock->block->num_tracks-1) {
				wblock->left_subtrack= -1;
				wblock->left_track++;
                          } else {
                            wblock->left_subtrack--;
                            UpdateAllWTracksCoordinates(window,wblock);
                            return 1;
                          }
			}
			leftwtrack=ListFindElement1(&wblock->wtracks->l,wblock->left_track);
			UpdateAllWTracksCoordinates(window,wblock);
			update=1;
		  }else{
		    break;
		  }
                }
                */

        } else if (window->curr_track==TEMPOTRACK && window->curr_othertrack_sub<3) {

                window->curr_othertrack_sub++;
                   
        } else if (window->curr_track==LPBTRACK && window->curr_othertrack_sub<2) {

                window->curr_othertrack_sub++;
                   
        } else if (window->curr_track==SWINGTRACK && window->curr_othertrack_sub<1) {

                window->curr_othertrack_sub++;
                   
	}else{
                ATOMIC_INC(window->curr_track, 1);

                if (window->curr_track==LPBTRACK && window->show_lpb_track==false)
                  ATOMIC_INC(window->curr_track, 1);

                if (window->curr_track==SWINGTRACK && window->show_swing_track==false)
                  ATOMIC_INC(window->curr_track, 1);
                
                if (window->curr_track==SIGNATURETRACK && window->show_signature_track==false)
                  ATOMIC_INC(window->curr_track, 1);
                
                if (window->curr_track==LINENUMBTRACK)
                  ATOMIC_INC(window->curr_track, 1);

                if (window->curr_track==TEMPONODETRACK && window->show_reltempo_track==false)
                  ATOMIC_INC(window->curr_track, 1);

		if (0==window->curr_track) {
                  if (wblock->wtracks->swingtext_on)
                    window->curr_track_sub = 0;
                  else
                    window->curr_track_sub = -1;
                }
                
                if (window->curr_track==SWINGTRACK)
                  window->curr_othertrack_sub = 0;

                if (window->curr_track==LPBTRACK)
                  window->curr_othertrack_sub = 0;

                if (window->curr_track==TEMPOTRACK)
                  window->curr_othertrack_sub = 0;
                
	}
	if(update==1){
		return 2;
	}else{
		return 1;
	}
}

static void set_curr_track_to_leftmost_legal_track(struct Tracker_Windows *window){
  if (false)
    printf("what?\n");
  
  else if (window->show_bpm_track==true) {
    ATOMIC_WRITE(window->curr_track, TEMPOTRACK);
    window->curr_othertrack_sub = 0;
    
  } else if (window->show_lpb_track==true)
    ATOMIC_WRITE(window->curr_track, LPBTRACK);
  
  else if (window->show_signature_track==true)
    ATOMIC_WRITE(window->curr_track, SIGNATURETRACK);
  
  else if (window->show_swing_track==true)
    ATOMIC_WRITE(window->curr_track, SWINGTRACK);
  
  else if (window->show_reltempo_track==true)
    ATOMIC_WRITE(window->curr_track, TEMPONODETRACK);
  
  else {
    ATOMIC_WRITE(window->curr_track, get_leftmost_visible_wtrack(window->wblock)->l.num);
    window->curr_track_sub = -1;
  }
}

int CursorLeft(struct Tracker_Windows *window,struct WBlocks *wblock){
  struct WTracks *wtrack = wblock->wtrack;
  bool is_going_to_be_at_normal_track = false;

  if (window->curr_track>=0){
    is_going_to_be_at_normal_track = true;
    if (window->curr_track==0){
      if (wtrack->swingtext_on){
        if (window->curr_track_sub==0)
          is_going_to_be_at_normal_track = false;
      }else{
        if (window->curr_track_sub==-1)
          is_going_to_be_at_normal_track = false;
      }
    }
  }
        
      if(is_going_to_be_at_normal_track){

                int prev = window->curr_track_sub;

		window->curr_track_sub--;

                bool move_to_prev_track = false;

                if (wtrack->swingtext_on && window->curr_track_sub==-1){
                  move_to_prev_track = true;

                } else if (wtrack->swingtext_on && window->curr_track_sub==-2){
                  window->curr_track_sub = 2;

                } else if (wtrack->swingtext_on && window->curr_track_sub==2){
                  window->curr_track_sub = -1;

                } else if (window->curr_track_sub==-2){
                    move_to_prev_track=true;
                    
                }

                printf("prev: %d. now: %d. move prevtrack: %d\n", prev, window->curr_track_sub, move_to_prev_track);


                if (move_to_prev_track){
                  do{
                    ATOMIC_INC(window->curr_track, -1);
                    R_ASSERT_RETURN_IF_FALSE2(window->curr_track >= 0, 0);
                    ATOMIC_WRITE(wblock->wtrack, ListFindElement1(&wblock->wtracks->l,window->curr_track));
                  }while(wblock->wtrack==NULL);
                  int num_subtracks = GetNumSubtracks(wblock->wtrack);
                  window->curr_track_sub=num_subtracks-1;
                }

                /*
		if(
			window->curr_track<wblock->left_track ||
			(window->curr_track==wblock->left_track && window->curr_track_sub<wblock->left_subtrack)
		){
                        wblock->left_subtrack=-1;//window->curr_track_sub;
			wblock->left_track=window->curr_track;
                        printf("   left_track: %d, left_subtrack: %d. curr_track: %d\n",wblock->left_track, wblock->left_subtrack,window->curr_track);
			//UpdateAllWTracksCoordinates(window,wblock);
                        UpdateWBlockCoordinates(window,wblock);
			return 2;
		}else{
                        printf("   left_track: %d, left_subtrack: %d, curr_track: %d\n",wblock->left_track, wblock->left_subtrack,window->curr_track);
			return 1;
		}
                */
                return 1;

        } else if (window->curr_track==TEMPOTRACK && window->curr_othertrack_sub>0) {
          
          window->curr_othertrack_sub--;
          return 1;
          
        } else if (window->curr_track==LPBTRACK && window->curr_othertrack_sub>0) {
          
          window->curr_othertrack_sub--;
          return 1;
          
        } else if (window->curr_track==SWINGTRACK && window->curr_othertrack_sub>0) {
          
          window->curr_othertrack_sub--;
          return 1;
          
	}else{
                
                if (window->curr_track==TEMPOTRACK)
                  return 0;
                
		ATOMIC_INC(window->curr_track, -1);

                if (window->curr_track==TEMPONODETRACK && window->show_reltempo_track==false)
                  ATOMIC_INC(window->curr_track, -1);

                if (window->curr_track==LINENUMBTRACK)
                  ATOMIC_INC(window->curr_track, -1);

                if (window->curr_track==SWINGTRACK && window->show_swing_track==false)
                  ATOMIC_INC(window->curr_track, -1);
                
                if (window->curr_track==SIGNATURETRACK && window->show_signature_track==false)
                  ATOMIC_INC(window->curr_track, -1);
                
                if (window->curr_track==LPBTRACK && window->show_lpb_track==false)
                  ATOMIC_INC(window->curr_track, -1);
                
                if (window->curr_track==TEMPOTRACK && window->show_bpm_track==false)
                  set_curr_track_to_leftmost_legal_track(window);

                if (window->curr_track==SWINGTRACK)
                  window->curr_othertrack_sub = 2;

                if (window->curr_track==LPBTRACK)
                  window->curr_othertrack_sub = 1;

                if (window->curr_track==TEMPOTRACK)
                  window->curr_othertrack_sub = 3;
                
		return 1;
	}
}

static void show_curr_track_in_statusbar(struct Tracker_Windows *window,struct WBlocks *wblock){
  struct WTracks *wtrack = wblock->wtrack;
  const char *message = NULL;
  struct FXs *fxs;

#define PRE "" //Moved cursor to "

  if (VELTEXT_subsubtrack(window, wtrack) >= 0){
    message = talloc_format(PRE "Velocity text, track #%d", wtrack->l.num);;

  } else if (FXTEXT_subsubtrack(window, wtrack, &fxs) >= 0){
    message = talloc_format(PRE "Fx text, track #%d", wtrack->l.num);;

  } else if (CHANCETEXT_subsubtrack(window, wtrack) >= 0){
    message = talloc_format(PRE "Chance text, track #%d", wtrack->l.num);;

  } else if (CENTTEXT_subsubtrack(window, wtrack) >= 0) {
    message = talloc_format(PRE "Cent text, track #%d", wtrack->l.num);;

  } else if (BPMTEXT_subsubtrack(window) >= 0) {
    message = PRE "BPM track";

  } else if (LPBTEXT_subsubtrack(window) >= 0) {
    message = PRE "LPB track";

  } else if (SIGNATURETEXT_subsubtrack(window) >= 0) {
    message = PRE "Signature track";

  } else if (SWINGTEXT_subsubtrack(window, window->curr_track < 0 ? NULL : wtrack) >= 0) {
    if (window->curr_track >= 0)
      message = PRE "Swing text sub track";
    else
      message = PRE "Global Swing track";

  } else if (window->curr_track >= 0) {
    if (window->curr_track_sub==-1)
      message = talloc_format(PRE "Note text, track #%d", wtrack->l.num);
    else {
      message = talloc_format(PRE "Automation area, track #%d", wtrack->l.num);
      //int veltracknum = (int)scale_int64(window->curr_track_sub, 0, GetNumSubtracks(wtrack) - ;
    }
      
  } else {
    //message = talloc_format(PRE "the %s track", get_track_name(window->curr_track));
    message = talloc_format(PRE "%s track", get_track_name(window->curr_track));
  }
  
  setStatusbarText(message, window->l.num);

#undef PRE
}

static void TrackSelectUpdate(struct Tracker_Windows *window,struct WBlocks *wblock, int unused){

        GFX_update_instrument_patch_gui(wblock->wtrack->track->patch);

        show_curr_track_in_statusbar(window, wblock);
        
        window->must_redraw=true;
}

void CursorRight_CurrPos(struct Tracker_Windows *window){
	struct WBlocks *wblock=window->wblock;
	TrackSelectUpdate(window,wblock,CursorRight(window,wblock));
}



void CursorLeft_CurrPos(struct Tracker_Windows *window){
	struct WBlocks *wblock=window->wblock;
	TrackSelectUpdate(window,wblock,CursorLeft(window,wblock));
}

int CursorNextTrack(struct Tracker_Windows *window,struct WBlocks *wblock){
	int curr_track=window->curr_track;
	int ret=0,tempret;

	while(curr_track==window->curr_track){
		tempret=CursorRight(window,wblock);
		ret=R_MAX(tempret,ret);
	}
	return ret;
}


bool SetCursorPosConcrete(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	NInt tracknum,
	int subtrack
){
        struct WTracks *wtrack;
	int ret=0,tempret;

	if(tracknum>=wblock->block->num_tracks || tracknum<TEMPOTRACK) return false;

	if(tracknum<0){
		if(tracknum==window->curr_track) return true;

		if(tracknum>window->curr_track){
			while(window->curr_track!=tracknum){
				tempret=CursorRight(window,wblock);
				ret=R_MAX(tempret,ret);
			}
		}else{
			while(window->curr_track!=tracknum){
				tempret=CursorLeft(window,wblock);
				ret=R_MAX(tempret,ret);
			}
		}
	}else{
		wtrack=ListFindElement1(&wblock->wtracks->l,tracknum);
                R_ASSERT_RETURN_IF_FALSE2(wtrack!=NULL, false);

                int num_subtracks = GetNumSubtracks(wtrack);

		subtrack=R_MIN(num_subtracks-1,subtrack);

		if(tracknum==window->curr_track && subtrack==window->curr_track_sub)
                  return false;
                
                int last_tracknum = -2000;
                int last_subtracknum = -2000;
                int last_curr_othertrack_sub = -2000;

                bool move_right = false;

		if(tracknum > window->curr_track){
                  //printf("mr1\n");
                  move_right = true;
                }

                else if (tracknum==window->curr_track) {
                  if (wtrack->swingtext_on && window->curr_track_sub < 3){
                    if (subtrack==-1){
                      //printf("mr2\n");
                      move_right = true;
                    }else if (subtrack >2){
                      //printf("mr3\n");
                      move_right = true;
                    }
                  } else {
                    if (subtrack > window->curr_track_sub){
                      //printf("mr4\n");
                      move_right = true;
                    }
                  }
                }

		if(move_right) {
			while(window->curr_track!=tracknum || window->curr_track_sub!=subtrack){
				tempret=CursorRight(window,wblock);
                                //printf("wtrack->num: %d, curr_track: %d, curr_track_sub: %d, num_tracks: %d\n",wtrack->l.num, window->curr_track,window->curr_track_sub,wblock->block->num_tracks);
				ret=R_MAX(tempret,ret);
                                if (window->curr_track==last_tracknum && window->curr_track_sub==last_subtracknum && window->curr_othertrack_sub==last_curr_othertrack_sub){
                                  R_ASSERT_NON_RELEASE(false);
                                  break; // prevent infinite loop in case of bug
                                }
                                last_tracknum = window->curr_track;
                                last_subtracknum = window->curr_track_sub;
                                last_curr_othertrack_sub = window->curr_othertrack_sub;
			}
		}else{
			while(window->curr_track!=tracknum || window->curr_track_sub!=subtrack){
				tempret=CursorLeft(window,wblock);
				ret=R_MAX(tempret,ret);
                                if (window->curr_track==last_tracknum && window->curr_track_sub==last_subtracknum && window->curr_othertrack_sub==last_curr_othertrack_sub){
                                  R_ASSERT_NON_RELEASE(false);
                                  break; // prevent infinite loop in case of bug
                                }
                                last_tracknum = window->curr_track;
                                last_subtracknum = window->curr_track_sub;
                                last_curr_othertrack_sub = window->curr_othertrack_sub;
			}
		}
	}

	return true;
}

void SetCursorPosConcrete_CurrPos(
	struct Tracker_Windows *window,
	NInt tracknum
){
	struct WBlocks *wblock=window->wblock;
	//int ret;

	if(tracknum>=wblock->block->num_tracks) return;

	SetCursorPosConcrete(
                             window,
                             wblock,
                             tracknum,
                             -1
                             );
        
	TrackSelectUpdate(window,wblock,-1);
}


void CursorNextTrack_CurrPos(struct Tracker_Windows *window){
	struct WBlocks *wblock=window->wblock;
	//struct WTracks *wtrack=wblock->wtrack;
	//int ret;

	if(wblock->wtrack->l.next==NULL){
          SetCursorPosConcrete(window,wblock,0,-1);
	}else if (window->curr_track < -1){
          CursorNextTrack(window,wblock);
        } else {
          SetCursorPosConcrete(
                               window,
                               wblock,
                               window->curr_track + 1,
                               -1
                               );
          //ret=CursorNextTrack(window,wblock);
	}

	TrackSelectUpdate(window,wblock,-1);
}

static int find_track_left(struct Tracker_Windows *window){

  if (window->curr_track==TEMPOTRACK)
    return window->wblock->block->num_tracks-1;
  
  int tracknum = window->curr_track-1;
  
  if (tracknum==TEMPONODETRACK && window->show_reltempo_track==false)
    tracknum--;

  if (tracknum==LINENUMBTRACK)
    tracknum--;

  if (tracknum==SWINGTRACK && window->show_swing_track==false)
    tracknum--;

  if (tracknum==SIGNATURETRACK && window->show_signature_track==false)
    tracknum--;

  if (tracknum==LPBTRACK && window->show_lpb_track==false)
    tracknum--;

  if (tracknum==TEMPOTRACK && window->show_bpm_track==false)
    tracknum = window->wblock->block->num_tracks-1;

  return tracknum;
}

void CursorPrevTrack_CurrPos(struct Tracker_Windows *window){
	struct WBlocks *wblock=window->wblock;
	NInt curr_track=window->curr_track;

        int new_track = find_track_left(window);
        if (new_track==curr_track) // can happen.
          return;
        
        SetCursorPosConcrete(window,wblock,new_track,-1);
        
	TrackSelectUpdate(window,wblock,0);
}



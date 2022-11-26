/* Copyright 2014 Kjetil S. Matheussen

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

#include "../common/includepython.h"

#include "../common/nsmtracker.h"
#include "../common/TimeData.hpp"
#include "../common/FX.hpp"
#include "../common/ratio_funcs.h"
#include "../common/placement_proc.h"
#include "../common/vector_proc.h"
#include "../common/list_proc.h"
#include "../common/undo.h"
#include "../common/undo_reltemposlider_proc.h"
#include "../common/time_proc.h"
#include "../common/trackreallines2_proc.h"
#include "../common/common_proc.h"
#include "../common/temponodes_proc.h"
#include "../common/undo_temponodes_proc.h"
#include "../common/realline_calc_proc.h"
#include "../common/notes_proc.h"
#include "../common/pitches_proc.h"
#include "../common/undo_notes_proc.h"
#include "../common/gfx_subtrack_proc.h"
#include "../common/velocities_proc.h"
#include "../common/visual_proc.h"
#include "../common/undo_fxs_proc.h"
#include "../common/fxlines_proc.h"
#include "../common/instruments_proc.h"
#include "../common/blocks_proc.h"
#include "../common/wblocks_proc.h"
#include "../common/OS_Player_proc.h"
#include "../common/player_proc.h"
#include "../common/player_pause_proc.h"
#include "../common/undo_trackheader_proc.h"
#include "../common/patch_proc.h"
#include "../common/nodelines_proc.h"
#include "../common/wtracks_proc.h"
#include "../common/tracks_proc.h"
#include "../common/undo_blocks_proc.h"
#include "../common/tempos_proc.h"
#include "../common/undo_tempos_proc.h"
#include "../common/sequencer_proc.h"
#include "../common/sliders_proc.h"
#include "../common/fxtext_proc.h"
#include "../common/windows_proc.h"
#include "../common/cursor_proc.h"
#include "../common/Signature_proc.h"
#include "../common/Beats_proc.h"

#include "../mixergui/QM_MixerWidget.h"

#include "../OpenGL/Render_proc.h"

#include "../embedded_scheme/s7extra_proc.h"

#include "../windows/W_Keyboard_proc.h"

#include "api_common_proc.h"

#include "radium_proc.h"

#include "api_mouse_proc.h"


extern volatile float g_scroll_pos;



// various
///////////////////////////////////////////////////

void ensureCleanStateOutsideMouseCycle(void){
  VECTOR_FOR_EACH(struct SeqTrack *, seqtrack, &root->song->seqtracks){
    radium::Scoped_Update_RT_GFX_variables scoped_gfx_variables(seqtrack);
    
    if(seqtrack->gfx_seqblocks != NULL){
      printf("\n\n\n  seqtrack->gfx_seqblocks != NULL. seqtracknum: %d\n\n\n", iterator666);
#if 0 //!defined(RELEASE)
      printf("                ensureCleanStateOutsideMouseCycle1: PRESS return to continue\n");
      getchar();
#endif
      seqtrack->gfx_seqblocks = NULL;
    }
    if(seqtrack->gfx_gfx_seqblocks.num_elements != 0){
      printf("  seqtrack->gfx_gfx_seqblocks != NULL. seqtracknum: %d\n", iterator666);
#if 0 //!defined(RELEASE)
      printf("                ensureCleanStateOutsideMouseCycle2: PRESS return to continue\n");
      getchar();
#endif
      VECTOR_clean(&seqtrack->gfx_gfx_seqblocks);
    }
  }END_VECTOR_FOR_EACH;
}


void API_setCurrentNode(struct ListHeader3 *new_current_node){
  if (g_current_node != new_current_node || g_current_node_id >= 0){
    g_current_node = new_current_node;
    g_current_node_id = -1;
    root->song->tracker_windows->must_redraw_editor = true;
    //printf("current node dirty\n");
  }
}

void API_setCurrentNode2(int64_t id){
  if (g_current_node_id != id || g_current_node!=NULL) {
    g_current_node = NULL;
    g_current_node_id = id;
    root->song->tracker_windows->must_redraw_editor = true;
    //printf("current node dirty\n");
  }
}

void cancelCurrentNode(void){
  API_setCurrentNode(NULL);
}

void API_setIndicatorNode(const struct ListHeader3 *new_indicator_node){
  if (g_indicator_node != new_indicator_node || g_indicator_node_id >= 0){
    g_indicator_node = new_indicator_node;
    g_indicator_node_id = -1;
    root->song->tracker_windows->must_redraw_editor = true;
    //printf("indicator node dirty\n");
  }
}

void API_setIndicatorNode2(int64_t id){
  if (g_indicator_node_id != id || g_indicator_node!=NULL){
    g_indicator_node = NULL;
    g_indicator_node_id = id;
    root->song->tracker_windows->must_redraw_editor = true;
    //printf("indicator node dirty\n");
  }
}

void cancelIndicatorNode(void){
  API_setIndicatorNode(NULL);
  g_indicator_velocity_num = -1;
  g_indicator_pitch_num = -1;
}


float getHalfOfNodeWidth(void){
  return get_min_node_size();
}

float get_scroll_pos(void){
  return safe_volatile_float_read(&g_scroll_pos);
}

static float get_mouse_realline_y1(const struct Tracker_Windows *window, int realline){
  //printf("fontheight: %f\n",(float)window->fontheight);
  //printf("wblock->t.y1: %f. scroll_pos: %f\n",(float)window->wblock->t.y1,(float)scroll_pos);
  return window->fontheight*realline - get_scroll_pos() + window->wblock->t.y1;
}

static float get_mouse_realline_y2(const struct Tracker_Windows *window, int realline){
  return window->fontheight*(realline+1) - get_scroll_pos() + window->wblock->t.y1;
}

float getTopVisibleY(int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if(wblock==NULL)
    return 0;

  return get_mouse_realline_y1(window, R_MAX(0, wblock->top_realline));
}

float getBotVisibleY(int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if(wblock==NULL)
    return 0;

  return get_mouse_realline_y2(window, R_MIN(wblock->num_reallines-1, wblock->bot_realline));
}

void setMouseTrack(int tracknum){
  struct Tracker_Windows *window = root->song->tracker_windows;
  struct WBlocks *wblock = window->wblock;

  if(tracknum != wblock->mouse_track){
    wblock->mouse_track = tracknum;
    window->must_redraw_editor = true;
  }
}

void setNoMouseTrack(void){
  setMouseTrack(NOTRACK);
}

void setMouseTrackToReltempo(void){
  setMouseTrack(TEMPONODETRACK);
}



// placement (block time)
///////////////////////////////////////////////////

Place getPlaceFromY(float y, int blocknum, int windownum) {
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if (wblock==NULL)
    return p_Create(0,0,1);

  return GetPlaceFromY(window,
                       wblock,
                       y
                       );
}

float getYFromPlace(Place place, int blocknum, int windownum) {
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if (wblock==NULL)
    return 0;

  float reallineF = FindReallineForF(wblock, 0, &place);
  float abs_y = get_realline_y(window, reallineF);

  int top_realline = wblock->top_realline;
  float y = abs_y - ((float)top_realline*(float)window->fontheight) + wblock->t.y1;
  return y;
}

static double get_gridded_abs_y(struct Tracker_Windows *window, float abs_y){
  double grid = (double)root->grid_numerator / (double)root->grid_denominator;

  double abs_realline = (double)abs_y / window->fontheight;
  
  double rounded = round(abs_realline / grid);
    
  return rounded * grid * window->fontheight;
}

Place getPlaceInGridFromY(float y, int blocknum, int windownum) {
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if (wblock==NULL)
    return p_Create(0,0,1);

  // This is a hack, but it's a relatively clean one, and modifying GetReallineAndPlaceFromY is a quite major work (old code, hard to understand, it should be rewritten).
  float abs_y = (y-wblock->t.y1) + wblock->top_realline*window->fontheight;
  float gridded_abs_y = get_gridded_abs_y(window, abs_y);
  float gridded_y = gridded_abs_y - wblock->top_realline*window->fontheight + wblock->t.y1;
  
  return GetPlaceFromY(window,
                       wblock,
                       gridded_y
                       );
}


static double get_next_gridded_abs_y(struct Tracker_Windows *window, float abs_y){
  double grid = (double)root->grid_numerator / (double)root->grid_denominator;

  double abs_realline = (double)abs_y / window->fontheight;
  
  double rounded = round(abs_realline / grid) + 1.0;
    
  return rounded * grid * window->fontheight;
}

Place getNextPlaceInGridFromY(float y, int blocknum, int windownum) {
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if (wblock==NULL)
    return p_Create(0,0,1);

  // This is a hack, but it's a relatively clean one, and modifying GetReallineAndPlaceFromY is a quite major work (old code, hard to understand, it should be rewritten).
  float abs_y = (y-wblock->t.y1) + wblock->top_realline*window->fontheight;
  float gridded_abs_y = get_next_gridded_abs_y(window, abs_y);
  float gridded_y = gridded_abs_y - wblock->top_realline*window->fontheight + wblock->t.y1;
  
  return GetPlaceFromY(window,
                       wblock,
                       gridded_y
                       );
}

Place getPlaceFromRealline(int realline, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if (wblock==NULL)
    return p_Create(0,0,1);

  if (realline < 0 || realline > wblock->num_reallines){
    handleError("getPlaceFromRealline: Illegal realline %d\n", realline);
    return p_Create(0,0,1);
  }

  if (realline==wblock->num_reallines)
    return p_Absolute_Last_Pos(wblock->block);

  return wblock->reallines[realline]->l.p;
}

float getReallineY1(int realline, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if (wblock==NULL)
    return 0;

  if (realline < 0 || realline > wblock->num_reallines){
    handleError("getReallineY1: Illegal realline %d\n", realline);
    return 0.0;
  }

  float abs_y = get_realline_y1(window, realline);

  int top_realline = wblock->top_realline;
  float y = abs_y - ((float)top_realline*(float)window->fontheight) + wblock->t.y1;
  return y;
}
  
float getReallineY2(int realline, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if (wblock==NULL)
    return 0;

  if (realline < 0 || realline > wblock->num_reallines){
    handleError("getReallineY2: Illegal realline %d\n", realline);
    return 0.0;
  }

  float abs_y = get_realline_y2(window, realline);
  
  int top_realline = wblock->top_realline;
  float y = abs_y - ((float)top_realline*(float)window->fontheight) + wblock->t.y1;
  return y;
}

  
int getReallineFromY(float y, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if (wblock==NULL)
    return 0;

  return int( (y-wblock->t.y1) / (float)window->fontheight) + wblock->top_realline;
}

int getTopRealline(int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if (wblock==NULL)
    return 0;

  return wblock->top_realline;
}

int getBotRealline(int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if (wblock==NULL)
    return 0;

  return wblock->bot_realline;
}



// reltempo
///////////////////////////////////////////////////

float getReltempoSliderX1(void){
  return root->song->tracker_windows->wblock->reltempo.x1;
}
float getReltempoSliderY1(void){
  return root->song->tracker_windows->wblock->reltempo.y1;
}
float getReltempoSliderX2(void){
  return root->song->tracker_windows->wblock->reltempo.x2;
}
float getReltempoSliderY2(void){
  return root->song->tracker_windows->wblock->reltempo.y2;
}

double getReltempo(int blocknum, int windownum){
  struct WBlocks *wblock = getWBlockFromNum(windownum, blocknum);
  if (wblock==NULL)
    return 0.0;
  else
    return ATOMIC_DOUBLE_GET(wblock->block->reltempo);
}

void undoReltempo(int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if (wblock==NULL)
    return;
  
  ADD_UNDO(RelTempoSlider(window,wblock));
}

void setReltempo(double reltempo, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if (wblock==NULL)
    return;
  
  double new_reltempo = R_BOUNDARIES(
                                     (double)MINBLOCKRELTIME,
                                     reltempo,
                                     (double)MAXBLOCKRELTIME
  );
  
  ATOMIC_DOUBLE_SET(wblock->block->reltempo, new_reltempo);

  //update_statusbar(window);
  //DrawBlockRelTempo(window,wblock);

  SEQUENCER_block_changes_tempo_multiplier(wblock->block, new_reltempo);

  SEQUENCER_update(SEQUPDATE_TIME);

  window->must_redraw = true;
}

float getMinReltempo(void){
  return MINBLOCKRELTIME;
}

float getMaxReltempo(void){
  return MAXBLOCKRELTIME;
}

void addBlockMultiplierMidiLearn(void){
  BLOCKS_add_tempo_multiplier_midi_learn();
}

void removeBlockMultiplierMidiLearn(void){
  BLOCKS_remove_tempo_multiplier_midi_learn();
}

bool hasBlockMultiplierMidiLearn(void){
  return BLOCKS_has_tempo_multiplier_midi_learn();
}




// The track scrollbar (horizontal scrollbar)
///////////////////////////////////////////////////

float getTrackSliderX1(int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if(wblock==NULL)
    return 0;

  return wblock->t.x1;
}
float getTrackSliderY1(int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if(wblock==NULL)
    return 0;

  return wblock->reltempo.y1;
}
float getTrackSliderX2(int blocknum, int windownum){
  return getBottomSliderX2(root->song->tracker_windows);
}
float getTrackSliderY2(int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if(wblock==NULL)
    return 0;

  return wblock->reltempo.y2;
}

float getTrackSliderPos(int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if(wblock==NULL)
    return 0;

  int total_width = WTRACKS_getWidth(window, wblock);

  return scale(wblock->skew_x, 0, -total_width, 0, 1);
}

void setTrackSliderPos(float pos, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if(wblock==NULL)
    return;

  int visible_width = (int)floor(getTrackSliderX2(blocknum, windownum) - getTrackSliderX1(blocknum, windownum));
  int total_width = WTRACKS_getWidth(window, wblock);
  //printf("      total_width: %d\n", total_width);
  
  if (visible_width >= total_width){
    //    R_ASSERT_NON_RELEASE(wblock->skew_x == 0);
    wblock->skew_x = 0;
    return;
  }

  wblock->skew_x = (int)scale(pos, 0, 1, 0, -total_width);
  if (wblock->skew_x > 0)
    wblock->skew_x = 0;

  if (wblock->skew_x < -(total_width-visible_width))
    wblock->skew_x = -(total_width-visible_width);

  UpdateAllWTracksCoordinates(window,wblock);

  // Scroll right
  for(int safe = 0; safe < 10000 ; safe++){

    //struct WTracks *wtrack = wblock->wtrack;
  
    NInt track    = window->curr_track;
    int  subtrack = window->curr_track_sub;
    
    //int xb1 = GetXSubTrack_B1(wblock,track,subtrack)-1;
    int xb2 = GetXSubTrack_B2(wblock,track,subtrack)+1;

    if (xb2 > wblock->t.x1)
      break;

    if (window->wblock==wblock)
      cursorRight(window->l.num);

    if (track==window->curr_track && subtrack==window->curr_track_sub)
      break;
  }

  // Scroll left
  for(int safe = 0; safe < 10000 ; safe++){

    //struct WTracks *wtrack = wblock->wtrack;
  
    NInt track    = window->curr_track;

    if(track < 0)
      break;
    
    int  subtrack = window->curr_track_sub;
    
    int xb1 = GetXSubTrack_B1(wblock,track,subtrack)-1;
    //int xb2 = GetXSubTrack_B2(wblock,track,subtrack)+1;

    if (xb1 < wblock->t.x2)
      break;
    
    if (window->wblock==wblock)
      cursorLeft(window->l.num);

    if (track==window->curr_track && subtrack==window->curr_track_sub)
      break;
  }

  window->must_redraw=true;
}

void setTrackSliderIsMoving(bool is_moving, int windownum){
  struct Tracker_Windows *window = getWindowFromNum(windownum);
  if(window==NULL)
    return;

  window->track_slider_is_moving = is_moving;
  window->must_redraw=true;
}

bool getTrackSliderIsMoving(int windownum){
  struct Tracker_Windows *window = getWindowFromNum(windownum);
  if(window==NULL)
    return false;

  return window->track_slider_is_moving;
}



// The line scrollbar (vertical scrollbar)
///////////////////////////////////////////////////


float getEditorScrollbarX1(void){
  return get_scrollbar_x1(root->song->tracker_windows);
}
float getEditorScrollbarY1(void){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(-1, &window, -1);
  if(wblock==NULL)
    return 0.0;
  return wblock->t.y1 + get_scrollbar_y1(window, wblock);
}
float getEditorScrollbarX2(void){
  return get_scrollbar_x2(root->song->tracker_windows);
}
float getEditorScrollbarY2(void){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(-1, &window, -1);
  if(wblock==NULL)
    return 0.0;
  return wblock->t.y1 + get_scrollbar_y2(window, wblock);
}


float getEditorScrollbarScrollerX1(void){
  return getEditorScrollbarX1();
}

float getEditorScrollbarScrollerY1(int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if(wblock==NULL)
    return 0.0;

  float scrollbar_height = get_scrollbar_y2(window, wblock) - get_scrollbar_y1(window, wblock);
  
  return window->wblock->t.y1 + get_scrollbar_scroller_y1(wblock->curr_realline, wblock->num_reallines, scrollbar_height, get_scrollbar_scroller_height(window, wblock));
}

float getEditorScrollbarScrollerX2(void){
  return getEditorScrollbarX2();
}

float getEditorScrollbarScrollerY2(int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(blocknum, &window, windownum);
  if(wblock==NULL)
    return 0.0;

  return getEditorScrollbarScrollerY1(blocknum, windownum) + get_scrollbar_scroller_height(window, wblock);
}

void setEditorScrollbarIsMoving(bool is_moving, int windownum){
  struct Tracker_Windows *window = getWindowFromNum(windownum);
  if(window==NULL)
    return;

  window->scrollbar_is_moving = is_moving;
  window->must_redraw=true;
}


// track panning on/off
///////////////////////////////////////////////////

bool getTrackPanOnOff(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return 0.0f;

  return wtrack->track->panonoff;
}

void setTrackPanOnOff(bool onoff, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return;

  if (wtrack->track->panonoff == onoff)
    return;
  
  ADD_UNDO(TrackHeader(wblock->l.num, wtrack->l.num));
  
  wtrack->track->panonoff = onoff;
        
  window->must_redraw = true;
}

bool switchTrackPanOnOff(int tracknum, int blocknum, int windownum){
  bool ret = !getTrackPanOnOff(tracknum, blocknum, windownum);

  setTrackPanOnOff(ret, tracknum, blocknum, windownum);

  return ret;
}

// track volumening on/off
///////////////////////////////////////////////////

bool getTrackVolumeOnOff(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return 0.0f;

  return wtrack->track->volumeonoff;
}

void setTrackVolumeOnOff(bool onoff, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return;

  wtrack->track->volumeonoff = onoff;
  
  window->must_redraw = true;
}

bool switchTrackVolumeOnOff(int tracknum, int blocknum, int windownum){
  bool ret = !getTrackVolumeOnOff(tracknum, blocknum, windownum);

  setTrackVolumeOnOff(ret, tracknum, blocknum, windownum);

  return ret;
}


// track panning slider
///////////////////////////////////////////////////

float getTrackPan(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return 0.0f;

  return scale(wtrack->track->pan, -MAXTRACKPAN, MAXTRACKPAN, -1.0, 1.0);
}

void undoTrackPan(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return;

  ADD_UNDO(TrackHeader(wblock->l.num, wtrack->l.num));
}

// void setTrackPan(float pan, int tracknum, int blocknum, int windownum)
// was already (more correctly) implemented in api_various.c



// track volumening slider
///////////////////////////////////////////////////

float getTrackVolume(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return 0.0f;

  return scale(wtrack->track->volume, 0, MAXTRACKVOL, 0.0, 1.0);
}

void undoTrackVolume(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return;

  ADD_UNDO(TrackHeader(wblock->l.num, wtrack->l.num));
}

// void setTrackVolume(float volume, int tracknum, int blocknum, int windownum)
// was already (more correctly) implemented in api_various.c



// editor positions
///////////////////////////////////////////////////

float getEditorX1(int windownum){
  return 0;
}

float getEditorX2(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL)
    return -1;

  return window->width;
}

float getEditorY1(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL)
    return -1;
  return window->wblock->t.y1;
}

float getEditorY2(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL)
    return -1;
  return window->wblock->t.y2;
}

// block positions
///////////////////////////////////////////////////

float getBlockHeaderY2(int blocknum, int windownum){
  struct WBlocks *wblock = getWBlockFromNum(windownum, blocknum);
  return wblock==NULL ? 0 : wblock->t.y1;
}


// tracks positions
///////////////////////////////////////////////////

float getTrackX1(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if(wblock==NULL)
    return 0;

  if (tracknum < LEFTMOSTTRACK || tracknum >= wblock->block->num_tracks){
    handleError("getTrackX2: No track %d", tracknum);
    return 0.0f;
  }
  
  return WTRACK_getx1(window, wblock, tracknum);
}

float getTrackY1(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if(wblock==NULL)
    return 0;
  
  return wblock->a.y1;
  
  /*
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->y;
  */
}

float getTrackX2(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if(wblock==NULL)
    return 0;

  if (tracknum < LEFTMOSTTRACK || tracknum >= wblock->block->num_tracks){
    handleError("getTrackX2: No track %d", tracknum);
    return 0.0f;
  }
  
  return WTRACK_getx2(window, wblock, tracknum);
}

float getTrackY2(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if(wblock==NULL)
    return 0;
  
  return wblock->a.y2;
  /*
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->y2;
  */
}

float getBeatBarBorderX(int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if(wblock==NULL)
    return 0;

  return wblock->beats_x;
}

int getTempoVisualizerTracknum(void){
  return TEMPOCOLORTRACK;
}

int getBpmTracknum(void){
  return TEMPOTRACK;
}

int getLpbTracknum(void){
  return LPBTRACK;
}


int getSignatureTracknum(void){
  return SIGNATURETRACK;
}


int getLinenumTracknum(void){
  return LINENUMBTRACK;
}

int getBeatTracknum(void){
  return LINENUMBTRACK;
}

int getSwingTracknum(void){
  return SWINGTRACK;
}


int getTempoAutomationTracknum(void){
  return TEMPONODETRACK;
}



float getTrackPianorollX1(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->pianoroll_area.x;
}

float getTrackPianorollY1(int tracknum, int blocknum, int windownum){
  return getTopVisibleY(blocknum, windownum); //getBlockHeaderY2(blocknum, windownum);
}

float getTrackPianorollX2(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->pianoroll_area.x2;
}

float getTrackPianorollY2(int tracknum, int blocknum, int windownum){
  return getBotVisibleY(blocknum, windownum);
  /*
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->y2;
  */
}

float getTrackNotesX1(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->notearea.x;
}

float getTrackNotesY1(int tracknum, int blocknum, int windownum){
  return getBlockHeaderY2(blocknum, windownum);
}

float getTrackNotesX2(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->notearea.x2;
}

float getTrackNotesY2(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->y2;
}

float getTrackFxX1(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->fxarea.x;
}

float getTrackFxY1(int tracknum, int blocknum, int windownum){
  return getBlockHeaderY2(blocknum, windownum);
}

float getTrackFxX2(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->fxarea.x2;
}

float getTrackFxY2(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->y2;
}


/////

float getCenttextX1(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->centtextarea.x;
}

float getCenttextY1(int tracknum, int blocknum, int windownum){
  return getBlockHeaderY2(blocknum, windownum);
}

float getCenttextX2(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->centtextarea.x2;
}

float getCenttextY2(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->y2;
}

/////

float getSwingtextX1(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->swingtextarea.x;
}

float getSwingtextY1(int tracknum, int blocknum, int windownum){
  return getBlockHeaderY2(blocknum, windownum);
}

float getSwingtextX2(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->swingtextarea.x2;
}

float getSwingtextY2(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->y2;
}

/////

float getChancetextX1(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->chancetextarea.x;
}

float getChancetextY1(int tracknum, int blocknum, int windownum){
  return getBlockHeaderY2(blocknum, windownum);
}

float getChancetextX2(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->chancetextarea.x2;
}

float getChancetextY2(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->y2;
}

/////

float getVelocitytextX1(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->veltextarea.x;
}

float getVelocitytextY1(int tracknum, int blocknum, int windownum){
  return getBlockHeaderY2(blocknum, windownum);
}

float getVelocitytextX2(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->veltextarea.x2;
}

float getVelocitytextY2(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->y2;
}

/////

float getFxtextX1(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->fxtextarea.x;
}

float getFxtextY1(int tracknum, int blocknum, int windownum){
  return getBlockHeaderY2(blocknum, windownum);
}

float getFxtextX2(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->fxtextarea.x2;
}

float getFxtextY2(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0 : wtrack->y2;
}

int getFxtextFxnumFromX(float x, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return -1;

  if (wtrack->fxtext_on==false)
    return -1;

  if (FXTEXT_has(wtrack->track)==false)
    return -1;

  int column = 0;
  VECTOR_FOR_EACH(const struct FXs *, fxs, &wtrack->track->fxs){
    if(fxs->fx->is_enabled){
      float x2 = wtrack->fxtextarea.x + ((column+1) * WTRACK_fxtext_track_width(window->fontwidth));
      if (x < x2)
        return iterator666; //fxs->fx->effect_num;
      column++;
    }
  }END_VECTOR_FOR_EACH;

  return wtrack->track->fxs.num_elements-1;
  //struct FXs *fxs = VECTOR_last(&wtrack->track->fxs);
  //return fxs->fx->effect_num;
}

///


int getLeftmostTrackNum(void){
  return LEFTMOSTTRACK;
}

int getRelTempoTrackNum(void){
  return TEMPONODETRACK;
}
int getTempoTrackNum(void){
  return TEMPOTRACK;
}
int getLPBTrackNum(void){
  return LPBTRACK;
}
int getSignatureTrackNum(void){
  return SIGNATURETRACK;
}
int getSwingTrackNum(void){
  return SWINGTRACK;
}
int getTempoColorTrackNum(void){
  return TEMPOCOLORTRACK;
}
int getLinenumTrackNum(void){
  return LINENUMBTRACK;
}


// temponodearea
//////////////////////////////////////////////////

int getTemponodeAreaX1(void){
  return root->song->tracker_windows->wblock->temponodearea.x;
}
int getTemponodeAreaY1(void){
  return root->song->tracker_windows->wblock->t.y1;
}
int getTemponodeAreaX2(void){
  return root->song->tracker_windows->wblock->temponodearea.x2;
}
int getTemponodeAreaY2(void){
  return root->song->tracker_windows->wblock->t.y2;
}

float getTemponodeMax(int blocknum, int windownum){
  struct WBlocks *wblock = getWBlockFromNum(windownum, blocknum);
  if (wblock==NULL)
    return 0.0f;
  else
    return wblock->reltempomax;
}

static const struct Node *get_temponode(int boxnum){
  const vector_t *nodes = GetTempoNodes(root->song->tracker_windows, root->song->tracker_windows->wblock);
  return (const struct Node*)VECTOR_get2(nodes,
                                         boxnum,
                                         "temponode"
                                         );
}

float getTemponodeX(int num){
  const struct Node *nodeline = get_temponode(num);
  return nodeline==NULL ? 0 : nodeline->x;
}

float getTemponodeY(int num){
  const struct Node *nodeline = get_temponode(num);
  return nodeline==NULL ? 0 : nodeline->y-get_scroll_pos();
}

void setCurrentTemponode(int num, int blocknum){
  struct Blocks *block = blocknum==-1 ? root->song->tracker_windows->wblock->block : getBlockFromNum(blocknum);
  if (block==NULL) {
    handleError("setCurrentTemponode: No block %d",blocknum);
    return;
  }
  
  struct TempoNodes *temponode = (struct TempoNodes *)ListFindElement3_num(&block->temponodes->l, num);

  API_setCurrentNode(temponode==NULL ? NULL : &temponode->l);
}

void setIndicatorTemponode(int num, int blocknum){
  struct Blocks *block = blocknum==-1 ? root->song->tracker_windows->wblock->block : getBlockFromNum(blocknum);
  if (block==NULL) {
    handleError("setCurrentTemponode: No block %d",blocknum);
    return;
  }
  
  struct TempoNodes *temponode = (struct TempoNodes *)ListFindElement3_num(&block->temponodes->l, num);

  API_setIndicatorNode(temponode==NULL ? NULL : &temponode->l);
}



// pianoroll
//////////////////////////////////////////////////

int getPioanorollLowKey(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(-1, blocknum, tracknum);

  if (wtrack==NULL)
    return 0;

  return wtrack->pianoroll_lowkey;
}

int getPioanorollHighKey(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(-1, blocknum, tracknum);

  if (wtrack==NULL)
    return 127;

  return wtrack->pianoroll_highkey;
}


enum PianoNoteWhatToGet {
  PIANONOTE_INFO_X1,
  PIANONOTE_INFO_Y1,
  PIANONOTE_INFO_X2,
  PIANONOTE_INFO_Y2,
  PIANONOTE_INFO_VALUE
};

static float get_pianonote_info(enum PianoNoteWhatToGet what_to_get, int pianonotenum, dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  r::NotePtr note = getNoteFromNumA2(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (!note)
    return 0;

  if (what_to_get==PIANONOTE_INFO_VALUE){
    if (pianonotenum==0)
      return note->_val;

    const r::PitchTimeData::Reader reader(&note->_pitches);

    if (pianonotenum >= reader.size()+1) {

      if (pianonotenum == reader.size()+1) {
        if (pianonotenum==1 && equal_floats(note->d._pitch_end, 0))
          return note->_val;
        else
          return note->d._pitch_end;
      }
      
      handleError("There is no pianonote #%d in note %d in track #%d in block #%d",pianonotenum,(int)note->_id,tracknum,blocknum);
      return 0;
    }
    
    return reader.at_ref(pianonotenum-1)._val;
  }

  const r::PitchTimeData::Reader reader(&note->_pitches);
  
  //const struct NodeLine *nodeline0 = GetPianorollNodeLines(window, wblock, wtrack, note);
  const struct NodeLine2 *nodeline = GetPianorollNodeLines2(window, wblock, wtrack, note.get(), reader);

  int num = -1;

  while(nodeline != NULL){
    if (nodeline->is_node)
      num++;

    if (num==pianonotenum)
      break;
    
    nodeline=nodeline->next;
  }

  if (nodeline==NULL) {
    handleError("There is no pianonote #%d in note %d in track #%d in block #%d",pianonotenum,(int)note->_id,tracknum,blocknum);  
    return 0;
  }

  NodelineBox box = GetPianoNoteBox2(wtrack, nodeline);

  /*
  NodelineBox box0 = GetPianoNoteBox(wtrack, nodeline0);

  printf("box. x1: %d/%d. y1: %d/%d. x2: %d/%d. y2: %d/%d\n", (int)box.x1, (int)box0.x1, (int)box.y1, (int)box0.y1, (int)box.x2, (int)box0.x2, (int)box.y2, (int)box0.y2);
  */
  
  switch (what_to_get){
  case PIANONOTE_INFO_X1:
    return box.x1;
  case PIANONOTE_INFO_Y1:
    return wblock->t.y1 + box.y1 - get_scroll_pos();
  case PIANONOTE_INFO_X2:
    return box.x2;
  case PIANONOTE_INFO_Y2:
    return wblock->t.y1 + box.y2 - get_scroll_pos();
  case PIANONOTE_INFO_VALUE:
  default:
    handleError("Internal error");
    return 0;
  }
}


float getPianonoteX1(int num, dyn_t dynnote, int tracknum, int blocknum, int windownum){
  return get_pianonote_info(PIANONOTE_INFO_X1, num, dynnote, tracknum, blocknum, windownum);
}

float getPianonoteY1(int num, dyn_t dynnote, int tracknum, int blocknum, int windownum){
  return get_pianonote_info(PIANONOTE_INFO_Y1, num, dynnote, tracknum, blocknum, windownum);
}

float getPianonoteX2(int num, dyn_t dynnote, int tracknum, int blocknum, int windownum){
  return get_pianonote_info(PIANONOTE_INFO_X2, num, dynnote, tracknum, blocknum, windownum);
}

float getPianonoteY2(int num, dyn_t dynnote, int tracknum, int blocknum, int windownum){
  return get_pianonote_info(PIANONOTE_INFO_Y2, num, dynnote, tracknum, blocknum, windownum);
}

float getPianonoteValue(int num, dyn_t dynnote, int tracknum, int blocknum, int windownum){
  return get_pianonote_info(PIANONOTE_INFO_VALUE, num, dynnote, tracknum, blocknum, windownum);
}

int getNumPianonotes(dyn_t dynnote, int tracknum, int blocknum, int windownum){
#if 0
  struct Notes *note=getNoteFromNum(windownum,blocknum,tracknum,dynnote);
  if (note==NULL)
    return 0;
#else
  const r::NotePtr note=getNoteFromNum2(windownum,blocknum,tracknum,dynnote);
  if (note.get()==NULL)
    return 0;
#endif
  
  return 1 + r::PitchTimeData::Reader(&note->_pitches).size();
  //return 1 + ListFindNumElements3((struct ListHeader3*)note->pitches);
}

/*
static void MOVE_PLACE(Place *place, float diff){
  if (diff < 0)
    *place = p_Sub(*place, p_FromFloat(-diff));
  else
    *place = p_Add(*place, p_FromFloat(diff));
}
*/

/*
static void MOVE_PLACE2(Place *place, const Ratio diff){
  *place = ratio2place(place2ratio(*place) + diff);
}
*/

static void setPianoNoteValues(float value, int pianonotenum, r::NotePtr &note){

  // 1. Find delta
  //
  float old_value;
  
  if (pianonotenum==0) {
    
    old_value = note->_val;
    
  } else {

    /*
    struct Pitches *pitch = (struct Pitches*)ListFindElement3_num_r0((ListHeader3*)note->pitches, pianonotenum-1);
    if (pitch==NULL){
      handleError("There is no pianonote %d",pianonotenum);
      return;
    }

    old_value = pitch->note;
    */

    const r::PitchTimeData::Reader reader(&note->_pitches);

    if (pianonotenum-1 >= reader.size()){
      handleError("There is no pianonote %d",pianonotenum);
      return;
    }
      
    old_value = reader.at_ref(pianonotenum-1)._val;
  }

  
  float delta = value - old_value;

  // 2. Apply
  //
  //note->note + note->note - value
    
  note->_val = R_BOUNDARIES(1, note->_val + delta, 127);

  if (note->d._pitch_end > 0)
    note->d._pitch_end = R_BOUNDARIES(1, note->d._pitch_end + delta, 127);

  {
    r::PitchTimeData::Writer writer(&note->_pitches);
    for(r::Pitch &pitch : writer)
      pitch._val = R_BOUNDARIES(1, pitch._val + delta, 127);
  }
}

#if 0
static Place get_pianonote_place(int pianonotenum, struct Notes *note, const r::PitchTimeData::Reader &reader){
  if (pianonotenum==0)
    return note->l.p;

  /*
  struct Pitches *pitch = (struct Pitches*)ListFindElement3_num((ListHeader3*)note->pitches, pianonotenum-1);
  if (pitch==NULL){
    handleError("There is no pianonote %d",pianonotenum);
    return note->l.p;
  }

  return pitch->l.p;
  */
  
  if (pianonotenum-1 >= reader.size()){
    handleError("There is no pianonote %d",pianonotenum);
    return note->l.p;
  }
      
  return ratio2place(reader.at_ref(pianonotenum-1)._time);
}
#endif

static Ratio get_pianonote_ratio(int pianonotenum, r::NotePtr &note, const r::PitchTimeData::Reader &reader){
  if (pianonotenum==0)
    return note->get_time();

  /*
  struct Pitches *pitch = (struct Pitches*)ListFindElement3_num((ListHeader3*)note->pitches, pianonotenum-1);
  if (pitch==NULL){
    handleError("There is no pianonote %d",pianonotenum);
    return note->l.p;
  }

  return pitch->l.p;
  */
  
  if (pianonotenum-1 >= reader.size()){
    handleError("There is no pianonote %d",pianonotenum);
    return note->get_time();
  }
      
  return reader.at_ref(pianonotenum-1)._time;
}

Place getPianonotePlace(int pianonotenum, dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  r::NotePtr note = getNoteFromNumA2(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (note.get()==NULL)
    return p_Create(0,0,1);

  const r::PitchTimeData::Reader reader(&note->_pitches);
    
  if (reader.size()+1==pianonotenum)
    return ratio2place(note->d._end);
  
  return ratio2place(get_pianonote_ratio(pianonotenum, note, reader));
}

static int getPitchNumFromPianonoteNum(int pianonotenum, dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return 0;

  struct Tracks *track = wtrack->track;

  int ret = 0;

#if 1
  const r::NotePtr note=getNoteFromNum2(windownum,blocknum,tracknum,dynnote);
    
  const r::NoteTimeData::Reader reader(track->_notes2);
   
  for(const r::NotePtr &note2 : reader) {
    const r::PitchTimeData::Reader pitch_reader(&note2->_pitches);
      
    if (note2==note) {
      
      if (pianonotenum > 0) {
        if (pianonotenum-1 >= pitch_reader.size()) {
          handleError("There is no pianonote #%d in note %d in track #%d in block #%d",pianonotenum,(int)note->_id,tracknum,blocknum);
          return 0;
        }
      }

      return ret + pianonotenum;
    }

    ret++;

    ret += pitch_reader.size();

    if (note2->d._pitch_end > 0)
      ret++;
  }
  
#else
  
  struct Notes *note = getNoteFromNum(windownum, blocknum, tracknum, dynnote);

  struct Notes *note2 = track->notes;

  while(note2!=NULL){

    const r::PitchTimeData::Reader pitch_reader(note2->_pitches);
      
    if (note2==note) {
      
      if (pianonotenum > 0) {
        if (pianonotenum-1 >= pitch_reader.size()) {
          handleError("There is no pianonote #%d in note %d in track #%d in block #%d",pianonotenum,(int)note->id,tracknum,blocknum);
          return 0;
        }
        /*        
        struct Pitches *pitch = (struct Pitches*)ListFindElement3_num_r0((ListHeader3*)note->pitches, pianonotenum-1);
        if (pitch==NULL){
          handleError("There is no pianonote #%d in note %d in track #%d in block #%d",pianonotenum,(int)note->id,tracknum,blocknum);
          return 0;
        }
        */
      }

      return ret + pianonotenum;
    }

    ret++;

    //ret += ListFindNumElements3((struct ListHeader3*)note2->pitches);
    ret += pitch_reader.size(); //ListFindNumElements3((struct ListHeader3*)note2->pitches);

    if (note2->pitch_end > 0)
      ret++;

    note2 = NextNote(note2);
  }

#endif
  
  handleError("There is no pianonote %d in note %d in track %d in block %d",pianonotenum,(int)note->_id,tracknum,blocknum);
  return 0;
}

#if 0
static dyn_t moveNote(struct Blocks *block, struct Tracks *track, struct Notes *note, Ratio diff){
  Ratio old_start = place2ratio(note->l.p);

  if (RATIO_less_than_zero(old_start + diff))
    diff = -old_start;

  //printf("new_start 1: %f\n",old_start+diff);

  Ratio old_end = note->end;

  Ratio lastplace = place2ratio(p_Last_Pos(block));

  if ((old_end + diff) > lastplace)
    diff = lastplace - old_end;

  //printf("new_start 2: %f\n",old_start+diff);

  struct Notes *new_note = CopyNote(note); // Until notes have been converted to TimeData, making a copy seems to be the best way to ensure atomic operation since we can't use a Writer (neither constructor/destructor/adding are RT safe) while holding the player lock.

  //Float2Placement(new_start, &note->l.p);
  //Float2Placement(new_end, &note->end);
  MOVE_PLACE2(&new_note->l.p, diff);
  new_note->end += diff;

  new_note->id = note->id; // This is probably safe. 'note' should not be accessible afterwards.
    
  {
    const r::PitchTimeData::Reader pitch_reader(note->_pitches);
    const r::VelocityTimeData::Reader velocity_reader(note->_velocities);
    
    r::PitchTimeData::Writer pitch_writer(new_note->_pitches);
    r::VelocityTimeData::Writer velocity_writer(new_note->_velocities);
    
    for(r::Pitch pitch : pitch_reader){
      pitch._time += diff;
      pitch_writer.add(pitch);
    }
    
    for(r::Velocity velocity : velocity_reader){
      velocity._time += diff;
      velocity_writer.add(velocity);
    }
  }
  
  {
    SCOPED_PLAYER_LOCK_IF_PLAYING();
    
    ListRemoveElement3(&track->notes, &note->l);
    
    ListAddElement3_a(&track->notes, &new_note->l);

    NOTE_validate(block, track, new_note);
  }

  return GetNoteId(note);
}
#endif

static dyn_t moveNote2(struct Blocks *block, struct Tracks *track, r::NotePtr &note, Ratio diff){
  Ratio old_start = note->get_time();

  if (RATIO_less_than_zero(old_start + diff))
    diff = -old_start;

  //printf("new_start 1: %f\n",old_start+diff);

  Ratio old_end = note->d._end;

  Ratio lastplace = make_ratio(block->num_lines, 1);

  if ((old_end + diff) > lastplace)
    diff = lastplace - old_end;

  //printf("new_start 2: %f\n",old_start+diff);

  r::NoteTimeData::Writer writer(track->_notes2);
  
  r::ModifyNote new_note(writer, note, r::ModifyNote::Type::CAN_MODIFY_TIME);

  new_note->_time += diff;
  new_note->d._end += diff;

  // Can't do this now. We don't hold player lock while modifying anymore, so both player and gfx may use the old note.
  //new_note->id = note->id; // This is probably safe. 'note' should not be accessible afterwards.
    
  {
    const r::PitchTimeData::Reader pitch_reader(&note->_pitches);
    const r::VelocityTimeData::Reader velocity_reader(&note->_velocities);
    
    r::PitchTimeData::Writer pitch_writer(&new_note->_pitches);
    r::VelocityTimeData::Writer velocity_writer(&new_note->_velocities);
    
    for(r::Pitch pitch : pitch_reader){
      pitch._time += diff; // We modify a copy, not the original.
      pitch_writer.add(pitch);
    }
    
    for(r::Velocity velocity : velocity_reader){
      velocity._time += diff; // We modify a copy, not the original.
      velocity_writer.add(velocity);
    }
  }

  writer.sortit();
  
  return GetNoteId3(new_note.get());
}

dyn_t movePianonote(int pianonotenum, float value, Place place, dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  r::NotePtr note = getNoteFromNumA2(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (note.get()==NULL)
    return dynnote;

  struct Blocks *block = wblock->block;
  
  setPianoNoteValues(value, pianonotenum, note);

  window->must_redraw_editor = true;

  if (p_is_same_place(place))
    return dynnote;

  if (validate_place(place)==false)
    return dynnote;

  //float floatplace = GetfloatFromPlace(&place);
  
  struct Tracks *track = wtrack->track;

  /*
  Place old_place = get_pianonote_place(pianonotenum, note);
  float old_floatplace = GetfloatFromPlace(&old_place);
  float diff      = floatplace - old_floatplace;
  */

  const r::PitchTimeData::Reader reader(&note->_pitches);

  Ratio diff = place2ratio(place) - get_pianonote_ratio(pianonotenum, note, reader);
  
  return moveNote2(block, track, note, diff);
}

static int setPitchnum2(int num, float value, Place place, int tracknum, int blocknum, int windownum, bool replace_note_ends);
  
dyn_t movePianonoteStart(int pianonotenum, float value, Place place_arg, dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  r::NotePtr note = getNoteFromNumA2(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (note.get()==NULL)
    return dynnote;

  //struct Blocks *block = wblock->block;
  struct Tracks *track = wtrack->track;

  const r::PitchTimeData::Reader reader(&note->_pitches);

  if (reader.size() > 0) { //note->pitches!=NULL) {
    setPitchnum2(getPitchNumFromPianonoteNum(pianonotenum, dynnote, tracknum, blocknum, windownum),
                 value, place_arg,
                 tracknum, blocknum, windownum,
                 false
                 );
    return dynnote;
  }

  note->_val = R_BOUNDARIES(1, value, 127);
    
  window->must_redraw_editor = true;

  if (p_is_same_place(place_arg))
    return dynnote;

  if (validate_place(place_arg)==false)
    return dynnote;

  //float floatplace = GetfloatFromPlace(&place);
  Ratio place = place2ratio(place_arg);
  
  //const float mindiff = 0.001;
  const Ratio mindiff = make_ratio(1, 1024);
  
  //float lastplacefloat = GetfloatFromPlace(&note->end);
  const Ratio lastplace = note->d._end;
  if ( (place+mindiff) >= lastplace)
    place = lastplace - mindiff;

  {
    const r::VelocityTimeData::Reader reader(&note->_velocities);
    if (reader.size() > 0) {
      const Ratio firstvelplace = reader.at_first()._time;
      if ( (place+mindiff) >= firstvelplace)
        place = firstvelplace - mindiff;
    }
  }

  // (there are no pitches here)

#if 1
  {
    r::NoteTimeData::Writer writer(track->_notes2);

    r::ModifyNote new_note(writer, note, r::ModifyNote::Type::CAN_MODIFY_TIME);

    new_note->_time = place;

    writer.sortit();
    
    return GetNoteId3(new_note.get());
  }
  
#else

  const Place new_place = ratio2place(place);


  {
    SCOPED_PLAYER_LOCK_IF_PLAYING();
    
    ListRemoveElement3(&track->notes, &note->l);
    
    note->l.p = new_place;
    
    ListAddElement3_a(&track->notes, &note->l);

    NOTE_validate(block, track, note);

    return GetNoteId(note);
  }
#endif
}

static int getPitchnumLogtype_internal(int pitchnum, struct Tracks *track);
static void setPitchnumLogtype2(int logtype, int pitchnum, struct Tracks *track, int blocknum);
  
int getPianonoteLogtype(int pianonotenum, dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  r::NotePtr note = getNoteFromNumA2(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (note.get()==NULL)
    return LOGTYPE_LINEAR;

  struct Tracks *track = wtrack->track;

  int pitchnum = getPitchNumFromPianonoteNum(pianonotenum, dynnote, tracknum, blocknum, windownum);
  return getPitchnumLogtype_internal(pitchnum, track);
}

void setPianonoteLogtype(int logtype, int pianonotenum, dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;

  r::NotePtr note = getNoteFromNumA2(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (note.get()==NULL)
    return;

  struct Tracks *track = wtrack->track;

  window->must_redraw_editor=true;

  int pitchnum = getPitchNumFromPianonoteNum(pianonotenum, dynnote, tracknum, blocknum, windownum);
  //printf("pitchnum: %d. pianonotenum: %d. wtrack: %d. wblock: %d\n", pitchnum, pianonotenum, wtrack->l.num, wblock->l.num);
  setPitchnumLogtype2(logtype, pitchnum, track, blocknum);
}
  
dyn_t movePianonoteEnd(int pianonotenum, float value, Place place_arg, dyn_t dynnote_arg, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  r::NotePtr note = getNoteFromNumA2(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote_arg);
  if (note.get()==NULL)
    return dynnote_arg;

  const Place place = place_arg;
  const dyn_t dynnote = dynnote_arg;
  
  struct Blocks *block = wblock->block;
  struct Tracks *track = wtrack->track;
  
  const r::PitchTimeData::Reader reader(&note->_pitches);

  if (reader.size() > 0) { //note->pitches != NULL) {
    
    int pitchnum = getPitchNumFromPianonoteNum(pianonotenum, dynnote, tracknum, blocknum, windownum);
    int logtype  = getPitchnumLogtype_internal(pitchnum, track);    
             
    // 1. Change pitch value
    setPitchnum2(logtype==LOGTYPE_HOLD ? pitchnum : pitchnum + 1,
                 value, g_same_place,
                 tracknum, blocknum, windownum,
                 false
                 );

    // 2. Change place of the next pianonote
    setPitchnum2(pitchnum+1,
                 -1, place,
                 tracknum, blocknum, windownum,
                 false
                 );

    return dynnote;
    
  } else {
  
  
    window->must_redraw_editor=true;

    if (value > 0){
      if(note->d._pitch_end > 0 || reader.size() > 0) //note->pitches!=NULL)
        note->d._pitch_end = R_BOUNDARIES(1, value, 127);
      else
        note->_val = R_BOUNDARIES(1, value, 127);
    }

    if (p_is_same_place(place))
      return dynnote;
    
    if (validate_place(place)==false)
      return dynnote;

    bool floatplace_changed = false;
    float floatplace = GetfloatFromPlace(&place);
      
    const float mindiff = 0.001;
  
    float firstplacefloat = ratio2double(note->get_time());
    if (floatplace-mindiff <= firstplacefloat){
      floatplace = firstplacefloat + mindiff;
      floatplace_changed = true;
    }

    {
      const r::VelocityTimeData::Reader reader(&note->_velocities);
      if (reader.size() > 0) {
        const Ratio lastvelplace = reader.at_last()._time;
        float lastvelfloat = make_double_from_ratio(lastvelplace);
        if (floatplace-mindiff <= lastvelfloat){
          floatplace = lastvelfloat + mindiff;
          floatplace_changed = true;
        }
      }
    }

    // (there are no pitches here)

    {
      Place new_place = place;

      if (floatplace_changed)
        Float2Placement(floatplace, &new_place);

      Place endplace;
      PlaceSetLastPos(block,&endplace);
      
      if(PlaceGreaterThan(&new_place,&endplace))
        new_place = endplace;

      {
        //SCOPED_PLAYER_LOCK_IF_PLAYING();
        
        r::NoteTimeData::Writer writer(track->_notes2);

        r::ModifyNote new_note(writer, note, r::ModifyNote::Type::CAN_NOT_MODIFY_TIME);

        new_note->d._end = place2ratio(new_place);

        //NOTE_validate(block, track, note);

        return GetNoteId3(new_note.get());
      }
    }
  }
}

/*
dyn_t movePianonoteEnd(int pianonotenum, float value, Place place_arg, dyn_t dynnote_arg, int tracknum, int blocknum, int windownum){
  
}
*/

dyn_t addPianonote(float value, Place place, Place endplace, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return DYN_create_int(-1);

  struct Blocks *block = wblock->block;
  struct Tracks *track = wtrack->track;

  value = R_BOUNDARIES(1,value,127);

  Place lastplace;
  PlaceSetLastPos(block, &lastplace);

  if (p_Less_Than(place, p_Create(0,0,1)))
    place = p_Create(0,0,1);

  if (p_Greater_Or_Equal(endplace, lastplace))
    endplace = lastplace;
  
  if (p_Greater_Or_Equal(place, endplace)){
    //handleError("Illegal parameters for addPianonote. start: %f, end: %f",floatplace, endfloatplace);
    return DYN_create_int(-1);
  }

  ADD_UNDO(Notes(window,block,track,window->wblock->curr_realline));

  //printf("  Place: %d %d/%d\n",place.line,place.counter,place.dividor);
  int64_t id = InsertNote(wblock, wtrack, &place, &endplace, value, NOTE_get_velocity(track), true);
  
  window->must_redraw_editor = true;

  return GetNoteIdFromNoteId(id);
}

void deletePianonote(int pianonotenum, dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  r::NotePtr note = getNoteFromNumA2(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (!note)
    return;

  if (pianonotenum==0) {
    window->must_redraw_editor=true;

    printf("     DELETING SOMETHIGN\n");
    {
      const r::NoteTimeData::Reader reader(wtrack->track->_notes2);

      int i = 0;
      for(const r::NotePtr &note2 : reader)
        printf(" Bef %d: %p / %p. Id: %d. Place: %s\n", i, note.get(), note2.get(), (int)note2->_id, ratio_to_string(note2->get_time()));
    }
    
    {
      r::NoteTimeData::Writer writer(wtrack->track->_notes2);
      RemoveNote2(wblock->block, wtrack->track, writer, r::ModifyNote(writer, note).get_noteptr()); // We use ModifyNote here as a quick way to get reference to noteptr inside writer.
    }
    
    {
      const r::NoteTimeData::Reader reader(wtrack->track->_notes2);

      int i = 0;
      for(const r::NotePtr &note2 : reader)
        printf(" Aft %d: %p / %p. Id: %d. Place: %s\n", i, note.get(), note2.get(), (int)note2->_id, ratio_to_string(note2->get_time()));
    }
    
    return;
  }

  int pitchnum = getPitchNumFromPianonoteNum(pianonotenum, dynnote, tracknum,  blocknum, windownum);

  deletePitchnum(pitchnum, tracknum, blocknum);      
}


  
void setCurrentPianonote(int num, dyn_t dynnote, int tracknum){
  if (tracknum==-1)
    tracknum = currentTrack(-1,-1);

#if 0
  struct Notes *note=getNoteFromNum(-1, -1, tracknum, dynnote);
  if (note==NULL)
    return;
#else
  const r::NotePtr note=getNoteFromNum2(-1, -1, tracknum, dynnote);
  if (note.get()==NULL)
    return;
#endif
  
  if (
      g_current_piano_note.tracknum != tracknum ||
      g_current_piano_note.noteid != note->_id || 
      g_current_piano_note.pianonotenum != num
      )
    {  
      g_current_piano_note.tracknum = tracknum;
      g_current_piano_note.noteid = note->_id;
      g_current_piano_note.pianonotenum = num;
      root->song->tracker_windows->must_redraw_editor = true;
    }
}

dyn_t getCurrentPianonote(int tracknum){
  if (tracknum==-1)
    tracknum = currentTrack(-1,-1);
    
  if (tracknum != g_current_piano_note.tracknum)
    return g_dyn_false;

  if (g_current_piano_note.noteid < 0)
    return g_dyn_false;

  return DYN_create_string_from_chars(GetNoteIdAsCharString(g_current_piano_note.noteid));
}

void cancelCurrentPianonote(void){
  if (
      g_current_piano_note.tracknum != -1 ||
      g_current_piano_note.noteid != -1 ||
      g_current_piano_note.pianonotenum != -1
      )
    {
      g_current_piano_note.tracknum = -1;
      g_current_piano_note.noteid = -1;
      g_current_piano_note.pianonotenum = -1;
      root->song->tracker_windows->must_redraw_editor = true;
    }
}

void setCurrentPianoGhostNote(Place place_start, Place place_end, float value, int tracknum){
  if (tracknum==-1)
    tracknum = root->song->tracker_windows->wblock->wtrack->l.num;

  if (
      g_current_piano_ghost_note.tracknum != tracknum ||
      p_NOT_Equal(g_current_piano_ghost_note.start, place_start) ||
      p_NOT_Equal(g_current_piano_ghost_note.end, place_end) ||
      !equal_floats(g_current_piano_ghost_note.value, value)
      )
    {
      g_current_piano_ghost_note.tracknum = tracknum;
      g_current_piano_ghost_note.start = place_start;
      g_current_piano_ghost_note.end = place_end;
      g_current_piano_ghost_note.value = value;
      root->song->tracker_windows->must_redraw_editor = true;
    }
}

void cancelCurrentPianoGhostNote(void){

  if (
      g_current_piano_ghost_note.tracknum != -1
      )
    {
      g_current_piano_ghost_note.tracknum = -1;
      root->song->tracker_windows->must_redraw_editor = true;
    }
}

bool hasCurrentPianoGhostNoteStart(void){
  return g_current_piano_ghost_note.tracknum >= 0;
}
  
int getCurrentPianoGhostNoteTracknum(void){
  return g_current_piano_ghost_note.tracknum;
}

Place getCurrentPianoGhostNoteStart(void){
  if (!hasCurrentPianoGhostNoteStart())
    return p_Create(0,0,1);
  
  return g_current_piano_ghost_note.start;
}

Place getCurrentPianoGhostNoteEnd(void){
  if (!hasCurrentPianoGhostNoteStart())
    return p_Create(0,0,1);
  
  return g_current_piano_ghost_note.end;
}

float getCurrentPianoGhostNoteValue(void){
  return g_current_piano_ghost_note.value;
}

//static int addPitch2(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack, struct Notes *note, Place *place, float value);
static int addPitch3(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack, r::NotePtr &note, const Ratio &ratio, float value);

void addPianonotePitch(float value, Place place, dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  r::NotePtr note = getNoteFromNumA2(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (!note)
    return;

  if (equal_floats(note->d._pitch_end, 0)) {
    window->must_redraw_editor = true;
    note->d._pitch_end = note->_val;
  }

  addPitch3(window, wblock, wtrack, note, place2ratio(place), value);
}

void showPianorollEraser(float pitch1, float pitch2, Place place1, Place place2, int tracknum, int blocknum, int windownum){
  g_current_pianobar_rubber.pitch1 = pitch1;
  g_current_pianobar_rubber.pitch2 = pitch2;
  g_current_pianobar_rubber.place1 = place1;
  g_current_pianobar_rubber.place2 = place2;
  g_current_pianobar_rubber.tracknum = tracknum;
  g_current_pianobar_rubber.blocknum = blocknum;
  root->song->tracker_windows->must_redraw_editor = true;
}

void hidePianorollEraser(int windownum){
  if (g_current_pianobar_rubber.blocknum != -2){
    g_current_pianobar_rubber.blocknum = -2;
    root->song->tracker_windows->must_redraw_editor = true;
  }
}

void showPianorollSelectionRectangle(float pitch1, float pitch2, Place place1, Place place2, int tracknum, int blocknum, int windownum){
  g_current_pianobar_selection_rectangle.pitch1 = pitch1;
  g_current_pianobar_selection_rectangle.pitch2 = pitch2;
  g_current_pianobar_selection_rectangle.place1 = place1;
  g_current_pianobar_selection_rectangle.place2 = place2;
  g_current_pianobar_selection_rectangle.tracknum = tracknum;
  g_current_pianobar_selection_rectangle.blocknum = blocknum;
  root->song->tracker_windows->must_redraw_editor = true;
}

void hidePianorollSelectionRectangle(int windownum){
  if (g_current_pianobar_selection_rectangle.blocknum != -2){
    g_current_pianobar_selection_rectangle.blocknum = -2;
    root->song->tracker_windows->must_redraw_editor = true;
  }
}

void copySelectedPianonotes(int tracknum, int blocknum){
  S7CALL2(void_dyn_dyn, "FROM_C-copy-selected-pianonotes", DYN_create_int(tracknum), DYN_create_int(blocknum));
}

void cutSelectedPianonotes(int tracknum, int blocknum){
  S7CALL2(void_dyn_dyn, "FROM_C-cut-selected-pianonotes!", DYN_create_int(tracknum), DYN_create_int(blocknum));
}

void deleteSelectedPianonotes(int tracknum, int blocknum){
  S7CALL2(void_dyn_dyn, "FROM_C-delete-selected-pianonotes!", DYN_create_int(tracknum), DYN_create_int(blocknum));
}

void pastePianonotes(float pitch, Place start_place, int tracknum, int blocknum){
  S7EXTRA_GET_FUNC(func, "FROM_C-paste-pianonotes!");
  
  s7extra_applyFunc_void_varargs(func,
                                 DYN_create_float((double)pitch),
                                 DYN_create_place(start_place),
                                 DYN_create_int(tracknum),
                                 DYN_create_int(blocknum),
                                 g_uninitialized_dyn
                                 );
}



// Bars and beats in the editor
//////////////////////////////////////////////////

void setCurrentBar(int barnum, int blocknum){
  setCurrentBeat(barnum, -1, blocknum);
}

void setCurrentBeat(int barnum, int beatnum, int blocknum){
  if (blocknum==-1)
    blocknum = root->song->tracker_windows->wblock->l.num;

  if (barnum != g_current_bar_num || g_current_beat_num != beatnum || g_current_barbeat_block_num != blocknum){
    g_current_bar_num = barnum;
    g_current_beat_num = beatnum;
    g_current_barbeat_block_num = blocknum;
    root->song->tracker_windows->must_redraw_editor = true;
  }
}

dyn_t getCurrentBar(void){
  if (g_current_barbeat_block_num < 0 || g_current_beat_num >= 1)
    return g_dyn_false;

  hash_t *ret = HASH_create(2);
  HASH_put_int(ret, ":bar", g_current_bar_num);
  HASH_put_int(ret, ":blocknum", g_current_barbeat_block_num);

  return DYN_create_hash(ret);
}

dyn_t getCurrentBeat(void){
  if (g_current_barbeat_block_num < 0)
    return g_dyn_false;

  hash_t *ret = HASH_create(2);
  HASH_put_int(ret, ":bar", g_current_bar_num);
  HASH_put_int(ret, ":beat", g_current_beat_num);
  HASH_put_int(ret, ":blocknum", g_current_barbeat_block_num);

  return DYN_create_hash(ret);
}

void cancelCurrentBeat(void){
  if (g_current_barbeat_block_num != -1) {
    g_current_bar_num = -1;
    g_current_beat_num = -1;
    g_current_barbeat_block_num = -1;
    root->song->tracker_windows->must_redraw_editor = true;
  }
}

static dyn_t get_beat2(int barnum, int beatnum){
  hash_t *hash = HASH_create(2);
  
  HASH_put_int(hash, ":bar", barnum);      
  HASH_put_int(hash, ":beat", beatnum);
  
  return DYN_create_hash(hash);
}

static dyn_t get_beat(const WSignature_trss &wsignatures_trss, int realline){
  const WSignature &wsignature = wsignatures_trss.at(realline);

  if (wsignature.beat_num > 0)
    return get_beat2(wsignature.bar_num, wsignature.beat_num);
  else
    return g_dyn_false;
}



dyn_t getBeatAtRealline(int realline, int blocknum, int windownum){
  
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if(wblock==NULL)
    return g_dyn_false;

  if (realline < 0 || realline >= wblock->num_reallines){
    handleError("getBeatAtRealline: Illegal realline %d", realline);
    return g_dyn_false;
  }
  
  const WSignature_trss wsignatures_trss = WSignatures_get(window, wblock);

  R_ASSERT_RETURN_IF_FALSE2(wsignatures_trss.size()==wblock->num_reallines, g_dyn_false);
  
  return get_beat(wsignatures_trss, realline);
}
  

dyn_t getBeatAtPlace(Place place, int blocknum, int windownum){
  
  struct Tracker_Windows *window;
  const struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if(wblock==NULL)
    return g_dyn_false;

  const struct Blocks *block = wblock->block;

  if (place.line < 0 || p_Greater_Or_Equal(place, p_Absolute_Last_Pos(block))){
    handleError("getBeatAtPlace: Illegal place %s", PlaceToString(&place));
    return g_dyn_false;
  }

  const struct Beats *prev_beat = NULL;
  const struct Beats *beat = wblock->block->beats;

  while(beat != NULL){
    if (p_Greater_Than(beat->l.p, place))
      break;

    prev_beat = beat;
    beat = NextBeat(beat);
  }

  if (prev_beat != NULL)
    return get_beat2(prev_beat->bar_num, prev_beat->beat_num);
  else
    return g_dyn_false;
}
  

dynvec_t getBeats(int startline, int endline, int blocknum, int windownum){
  dynvec_t ret = {};
  
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if(wblock==NULL)
    return ret;

  if (endline == -1)
    endline = wblock->num_reallines;

  if (startline < 0 || endline > wblock->num_reallines || endline < startline){
    handleError("getBeats: Illegal startline/endline: %d / %d", startline, endline);
    return ret;
  }


  const WSignature_trss wsignatures_trss = WSignatures_get(window, wblock);

  R_ASSERT_RETURN_IF_FALSE2(wsignatures_trss.size()==wblock->num_reallines, ret);

  int size = endline-startline;
  
  ret = DYNVEC_create(size);

  for(int i=0;i<size;i++)
    DYNVEC_set(ret, i, get_beat(wsignatures_trss, i + startline));

  return ret;
}

Place getBeatStart(int barnum, int beatnum, int blocknum){
  Place ret = p_Create(0,0,1);

  struct Blocks *block = blocknum==-1 ? root->song->tracker_windows->wblock->block : getBlockFromNum(blocknum);

  if (block==NULL)
    return ret;

  if (beatnum==-1)
    beatnum = g_current_beat_num;

  if (barnum==-1)
    barnum = g_current_bar_num;

  if (barnum < 1){
    handleError("getBeatStart: Illegal barnum: %d\n", barnum);
    return ret;
  }

  if (beatnum < 1){
    handleError("getBeatStart: Illegal beatnum: %d\n", beatnum);
    return ret;
  }
  
  if (get_barbeat_start_and_end(block, barnum, beatnum, &ret, NULL)==false)
    handleError("getBeatStart: There is no barnum %d / beatnum %d.", barnum, beatnum);

  return ret;
}

Place getBeatEnd(int barnum, int beatnum, int blocknum){
  Place ret = p_Create(0,0,1);

  struct Blocks *block = blocknum==-1 ? root->song->tracker_windows->wblock->block : getBlockFromNum(blocknum);

  if (block==NULL)
    return ret;

  if (beatnum==-1)
    beatnum = g_current_beat_num;

  if (barnum==-1)
    barnum = g_current_bar_num;

  if (barnum < 1){
    handleError("getBeatEnd: Illegal barnum: %d\n", barnum);
    return ret;
  }

  if (beatnum < 1){
    handleError("getBeatEnd: Illegal beatnum: %d\n", beatnum);
    return ret;
  }
  
  if (get_barbeat_start_and_end(block, barnum, beatnum, NULL, &ret)==false)
    handleError("getBeatStart: There is no barnum %d / beatnum %d.", barnum, beatnum);

  return ret;
}

Place getBarStart(int barnum, int blocknum){
  Place ret = p_Create(0,0,1);

  struct Blocks *block = blocknum==-1 ? root->song->tracker_windows->wblock->block : getBlockFromNum(blocknum);

  if (block==NULL)
    return ret;

  if (barnum==-1)
    barnum = g_current_bar_num;

  if (barnum < 1){
    handleError("getBarStart: Illegal barnum: %d\n", barnum);
    return ret;
  }

  if (get_barbeat_start_and_end(block, barnum, -1, &ret, NULL)==false)
    handleError("getBarStart: There is no barnum %d", barnum);

  return ret;
}

Place getBarEnd(int barnum, int blocknum){
  Place ret = p_Create(0,0,1);

  struct Blocks *block = blocknum==-1 ? root->song->tracker_windows->wblock->block : getBlockFromNum(blocknum);

  if (block==NULL)
    return ret;

  if (barnum==-1)
    barnum = g_current_bar_num;

  if (barnum < 1){
    handleError("getBarEnd: Illegal barnum: %d\n", barnum);
    return ret;
  }

  if (get_barbeat_start_and_end(block, barnum, -1, NULL, &ret)==false)
    handleError("getBarEnd: There is no barnum %d", barnum);

  return ret;
}

bool hasBeat(int barnum, int beatnum, int blocknum){
  struct Blocks *block = blocknum==-1 ? root->song->tracker_windows->wblock->block : getBlockFromNum(blocknum);

  if (block==NULL)
    return false;

  if (beatnum==-1){

    if (g_current_barbeat_block_num==-1)
      return false;

    beatnum = g_current_beat_num;
  }

  if (barnum==-1){

    if (g_current_barbeat_block_num==-1)
      return false;

    barnum = g_current_bar_num;
  }

  if (barnum < 1){
    handleError("hasBeat: Illegal barnum: %d\n", barnum);
    return false;
  }

  if (beatnum < 1){
    handleError("hasBeat: Illegal beatnum: %d\n", beatnum);
    return false;
  }
  
  const struct Beats *beat = block->beats;

  while(beat != NULL){
    if (beat->bar_num==barnum){

      if (beat->beat_num == beatnum)
        return true;

      if (beat->beat_num > beatnum)
        return false;
    }

    if (beat->bar_num > barnum)
      return false;

    beat = NextBeat(beat);
  }

  return false;
}


// pitchnums
//////////////////////////////////////////////////

/*
static int getPitchNum(struct Tracks *track, struct Notes *note, int pitchnum2, bool is_end_pitch){
  int num = 0;
  struct Notes *note2 = track->notes;

  while(note2!=NULL){

    if (note==note2 && pitchnum2<0 && is_end_pitch==false)
      return num;

    num++;

    if (note==note2 && pitchnum2 >= 0)
      return num + pitchnum2;

    num += r::PitchTimeData::Reader(note2->_pitches).size();
    
    if (note2->pitch_end > 0){
      if (note==note2 && pitchnum2 < 0 && is_end_pitch==true)
        return num;
      
      num++;
    }

    if (note==note2) {
      handleError("getPitchNum: Could not find pitch in note.");
      return 0;
    }

    note2 = NextNote(note2);
  }

  handleError("getPitchNum: Could not find it");
  return 0;
}
*/

static int getPitchNum2(const struct Tracks *track, int64_t id, const int pitchnum2, const bool is_end_pitch){
  int num = 0;
  const r::NoteTimeData::Reader reader(track->_notes2);

  for(const r::NotePtr &note : reader){

    if (id==note->_id && pitchnum2<0 && is_end_pitch==false)
      return num;

    num++;

    if (id==note->_id && pitchnum2 >= 0)
      return num + pitchnum2;

    num += r::PitchTimeData::Reader(&note->_pitches).size();
    
    if (note->d._pitch_end > 0){
      if (id==note->_id && pitchnum2 < 0 && is_end_pitch==true)
        return num;
      
      num++;
    }

    if (id==note->_id) {
      handleError("getPitchNum: Could not find pitch in note.");
      return 0;
    }
  }

  handleError("getPitchNum: Could not find it");
  return 0;
}

int getNumPitchnums(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(-1, blocknum, tracknum);

  if (wtrack==NULL)
    return 0;

  struct Tracks *track = wtrack->track;
  
  int num = 0;

#if 1
  const r::NoteTimeData::Reader reader(track->_notes2);
  
  for(const r::NotePtr &note : reader){
    num++;

    num += r::PitchTimeData::Reader(&note->_pitches).size();

    if (note->d._pitch_end > 0)
      num++;
  }
  
#else
  struct Notes *notes = track->notes;
  
  while(notes!=NULL){

    num++;

    num += r::PitchTimeData::Reader(notes->_pitches).size();

    if (notes->pitch_end > 0)
      num++;
    
    notes = NextNote(notes);
  }
#endif
  
  return num;
}

void deletePitchnum(int pitchnum, int tracknum, int blocknum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(-1, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return;

  struct Blocks *block = wblock->block;
  struct Tracks *track = wtrack->track;

#if 1
  int num = 0;
  
  const r::NoteTimeData::Reader note_reader(wtrack->track->_notes2);
  
  for(const r::NotePtr &note : note_reader) {

    if (pitchnum==num) {
      r::NoteTimeData::Writer writer(wtrack->track->_notes2);
      RemoveNote2(block, track, writer, r::ModifyNote(writer, note).get_noteptr()); // We use ModifyNote here as a quick way to get reference to the noteptr in writer.
      goto gotit;
    }
    
    num++;

    const r::PitchTimeData::Reader reader(&note->_pitches);

    for(int n = 0 ; n < reader.size() ; n++) {
      if (pitchnum==num) {
        r::PitchTimeData::Writer writer(&note->_pitches);
        writer.remove_at_pos(n);
        goto gotit;
      }
      num++;
    }
        
    if (note->d._pitch_end > 0) {
      if (pitchnum==num){
        if (reader.size()==0)
          note->d._pitch_end = 0;
        else
          note->d._pitch_end = reader.at_last()._val;
        
        goto gotit;
      }
      num++;
    }
  }
#else
  int num = 0;
  struct Notes *notes = track->notes;
  
  while(notes!=NULL){

    if (pitchnum==num) {
      {
        SCOPED_PLAYER_LOCK_IF_PLAYING();
        RemoveNote(block, track, notes);
      }
      goto gotit;
    }
    
    num++;

    const r::PitchTimeData::Reader reader(notes->_pitches);

    for(int n = 0 ; n < reader.size() ; n++) {
      if (pitchnum==num) {
        r::PitchTimeData::Writer writer(notes->_pitches);
        writer.remove_at_pos(n);
        goto gotit;
      }
      num++;
    }
        
    if (notes->pitch_end > 0) {
      if (pitchnum==num){
        if (reader.size()==0)
          notes->pitch_end = 0;
        else
          notes->pitch_end = reader.at_last()._val;
        
        goto gotit;
      }
      num++;
    }
    
    notes = NextNote(notes);
  }
#endif
  
  handleError("no pitch %d in track %d in block %d\n",pitchnum,tracknum,blocknum);
  return;
  
 gotit:
  window->must_redraw_editor = true;
}


/*
static bool getPitch(int pitchnum, struct Pitches **pitch, struct Notes **note, bool *is_end_pitch, struct Tracks *track){
  int num = 0;
  struct Notes *notes = track->notes;
  
  *is_end_pitch = false;
  
  while(notes!=NULL){

    if(num==pitchnum) {
      *note = notes;
      *pitch = NULL;
      return true;
    }

    num++;
    
    struct Pitches *pitches = notes->pitches;
    while(pitches!=NULL){
      if(num==pitchnum) {
        *note = notes;
        *pitch = pitches;
        return true;
      }

      num++;
      pitches = NextPitch(pitches);
    }

    if (notes->pitch_end > 0) {
      if(num==pitchnum) {
        *note = notes;
        *is_end_pitch = true;
        *pitch = NULL;
        return true;
      }
      num++;
    }
    
    notes = NextNote(notes);
  }

  handleError("Pitch #%d in track #%d does not exist",pitchnum,track->l.num);
  return false;
}
*/

static bool getPitch2(int pitchnum, int &pitchnum2, r::NotePtr &note, bool *is_end_pitch, struct Tracks *track){
  int num = 0;
  *is_end_pitch = false;

#if 1
  const r::NoteTimeData::Reader reader(track->_notes2);
  
  for(const r::NotePtr &note2 : reader){
    
    if(num==pitchnum) {
      note = note2;
      pitchnum2 = -1;
      return true;
    }

    num++;

    pitchnum2 = 0;

    const r::PitchTimeData::Reader reader(&note2->_pitches);

    for(const r::Pitch &pitch : reader) {
      (void)pitch;
      
      if(num==pitchnum) {
        note = note2;
        return true;
      }

      num++;
      pitchnum2++;
    }
      
    
    if (note2->d._pitch_end > 0) {
      if(num==pitchnum) {
        note = note2;
        *is_end_pitch = true;
        return true;
      }
      num++;
    }
  }

#else
  struct Notes *notes = track->notes;
  
  while(notes!=NULL){

    if(num==pitchnum) {
      *note = notes;
      pitchnum2 = -1;
      return true;
    }

    num++;

    pitchnum2 = 0;

    const r::PitchTimeData::Reader reader(notes->_pitches);

    for(const r::Pitch &pitch : reader) {
      (void)pitch;
      
      if(num==pitchnum) {
        *note = notes;
        return true;
      }

      num++;
      pitchnum2++;
    }
      
    
    if (notes->pitch_end > 0) {
      if(num==pitchnum) {
        *note = notes;
        *is_end_pitch = true;
        return true;
      }
      num++;
    }
    
    notes = NextNote(notes);
  }
#endif
  
  handleError("Pitch #%d in track #%d does not exist",pitchnum,track->l.num);
  return false;
}

/*
Place getPitchStart(int pitchnum,  dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (note==NULL)
    return p_Create(0,0,1);

  bool is_end_pitch = false;
  struct Notes *note = NULL;
  struct Pitches *pitch = NULL;
  getPitch(pitchnum, &pitch, &note, &is_end_pitch, wtrack->track);

  if (is_end_pitch)
    return note->end;
  if (pitch==NULL)
    return note->l.p;
  else
    return pitch->l.p;
}
*/

static int getPitchnumLogtype_internal(int pitchnum, struct Tracks *track){
  bool is_end_pitch = false;

  r::NotePtr note;

  int pitchnum2;
  if (getPitch2(pitchnum, pitchnum2, note, &is_end_pitch, track)==false)
    return 0;

  if (is_end_pitch)
    return LOGTYPE_IRRELEVANT;
  else if (pitchnum2>=0){    
    r::PitchTimeData::Reader reader(&note->_pitches);
    R_ASSERT_RETURN_IF_FALSE2(pitchnum2 < reader.size(), 0);
    return reader.at_ref(pitchnum2)._logtype;
  }
  else
    return note->d._pitch_first_logtype;
}

/*
Place getPitchnumLogtype(int num,  int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (note==NULL)
    return p_Create(0,0,1);

  return getPitchnumLogtype_internal(num, wtrack->track);
}
*/

static void setPitchnumLogtype2(int logtype, int pitchnum, struct Tracks *track, int blocknum){
  bool is_end_pitch = false;
  int pitchnum2;

  r::NotePtr note;
  
  if (getPitch2(pitchnum, pitchnum2, note, &is_end_pitch, track)==false)
    return;

  if (is_end_pitch) {
    handleError("Can not set logtype of end pitch. pitchnum: %d, tracknum: %d, blocknum: %d. (pitchnum2: %d. num pitches in note: %d)",pitchnum,track->l.num, blocknum, pitchnum2, r::PitchTimeData::Reader(&note->_pitches).size());
    //abort();
    return;
  }
      
  if (pitchnum2 < 0)
    note->d._pitch_first_logtype = logtype;
  else {
    r::PitchTimeData::Writer writer(&note->_pitches);
    R_ASSERT_RETURN_IF_FALSE(pitchnum2 < writer.size());
    r::Pitch &pitch = writer.at_ref(pitchnum2);
    pitch._logtype = logtype;
  }
}


void setPitchnumLogtype(int logtype, int pitchnum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);

  if (wtrack==NULL)
    return;

  setPitchnumLogtype2(logtype, pitchnum, wtrack->track, blocknum);
}

int getNotenumForPitchnum(int pitchnum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);

  if (wtrack==NULL)
    return -1;

  struct Tracks *track = wtrack->track;
  
  bool is_end_pitch = false;
  int pitchnum2;

  r::NotePtr note;
  
  if (getPitch2(pitchnum, pitchnum2, note, &is_end_pitch, track)==false)
    return -1;

  R_ASSERT_RETURN_IF_FALSE2(note.get()!=NULL, -1);

  const r::NoteTimeData::Reader reader(wtrack->track->_notes2);

  int i = 0;
  for(const r::NotePtr &note2 : reader)
    if (note2==note)
      return i;
    else
      i++;

  return -1;
}



static int getReallineForPitch(const struct WBlocks *wblock, int pitchnum2, r::NotePtr &note, bool is_end_pitch){
  if (is_end_pitch){
    Place p = ratio2place(note->d._end);
    return find_realline_for_end_pitch(wblock, &p);
  } else if(pitchnum2 >= 0) {
    const r::PitchTimeData::Reader reader(&note->_pitches);
    R_ASSERT_RETURN_IF_FALSE2(pitchnum2 < reader.size(), 0);
    return FindReallineForRatio(wblock,0,reader.at_ref(pitchnum2)._time);
  }else
    return FindReallineForRatio(wblock,0,note->get_time());
}

enum PitchInfoWhatToGet {
  PITCH_INFO_Y1,
  PITCH_INFO_Y2,
  PITCH_INFO_VALUE,
};

static float getPitchInfo(enum PitchInfoWhatToGet what_to_get, int pitchnum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);

  if (wtrack==NULL)
    return 0;

  r::NotePtr note;
  
  int pitchnum2;
  bool is_end_pitch;
  
  if (getPitch2(pitchnum, pitchnum2, note, &is_end_pitch, wtrack->track)==false)
    return 0;
  
  switch (what_to_get){
  case PITCH_INFO_Y1:
    return get_mouse_realline_y1(window, getReallineForPitch(wblock, pitchnum2, note, is_end_pitch));
  case PITCH_INFO_Y2:
    return get_mouse_realline_y2(window, getReallineForPitch(wblock, pitchnum2, note, is_end_pitch));
  case PITCH_INFO_VALUE:
    {
      if (is_end_pitch)
        return note->d._pitch_end;
      else if (pitchnum2>=0){
        r::PitchTimeData::Reader reader(&note->_pitches);
        R_ASSERT_RETURN_IF_FALSE2(pitchnum2 < reader.size(), 0);
        return reader.at_ref(pitchnum2)._val;
      }
      else
        return note->_val;
    }
  }

  handleError("internal error (getPitchInfo)\n");
  return 0;
}

float getPitchnumY1(int pitchnum, int tracknum, int blocknum, int windownum){
  return getPitchInfo(PITCH_INFO_Y1, pitchnum, tracknum, blocknum, windownum);
}

float getPitchnumY2(int pitchnum, int tracknum, int blocknum, int windownum){
  return getPitchInfo(PITCH_INFO_Y2, pitchnum, tracknum, blocknum, windownum);
}

float getPitchnumX1(int pitchnum, int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0.0f : wtrack->notearea.x;
}

float getPitchnumX2(int pitchnum, int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  return wtrack==NULL ? 0.0f : wtrack->notearea.x2;
}

static struct Node2 *get_pitchnodeline(int pitchnum, int tracknum, int blocknum, int windownum, bool *is_end_pitch){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return NULL;

  r::NotePtr note;
  
  int pitchnum2;
  
  if (getPitch2(pitchnum, pitchnum2, note, is_end_pitch, wtrack->track)==false)
    return NULL;

  int note_pitchnum;

  if (pitchnum2 < 0)
    note_pitchnum = 0;
  else
    note_pitchnum = pitchnum2 + 1; //ListPosition3(&note->pitches->l, &pitch->l) + 1;


  const float track_pitch_min = wtrack->track->_notes2->_min_display_pitch;
  const float track_pitch_max = wtrack->track->_notes2->_max_display_pitch;

  const vector_t *nodes = GetPitchNodes3(window, wblock, wtrack, note, track_pitch_min, track_pitch_max);

  return (struct Node2*)nodes->elements[note_pitchnum];
}


float getPitchnumX(int num,  int tracknum, int blocknum, int windownum){
  bool is_end_pitch;
  struct Node2 *nodeline = get_pitchnodeline(num, tracknum, blocknum, windownum, &is_end_pitch);
  if (nodeline==NULL)
    return 0;

  return nodeline->x;
}

float getPitchnumY(int num, int tracknum, int blocknum, int windownum){
  bool is_end_pitch;
  struct Node2 *nodeline = get_pitchnodeline(num, tracknum, blocknum, windownum, &is_end_pitch);
  if (nodeline==NULL)
    return 0;

  return nodeline->y-get_scroll_pos();

  /*
  if (is_end_pitch) 
    return nodeline->y-get_scroll_pos();
  else
    return nodeline->y-get_scroll_pos();
  */
}


float getPitchnumValue(int pitchnum, int tracknum, int blocknum, int windownum){
  return getPitchInfo(PITCH_INFO_VALUE, pitchnum, tracknum, blocknum, windownum);
}

Place getPitchnumPlace(int pitchnum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);

  if (wtrack==NULL)
    return p_Create(0,0,1);

  struct Tracks *track = wtrack->track;
  
  bool is_end_pitch = false;

  r::NotePtr note;

  int pitchnum2;
  
  if (getPitch2(pitchnum, pitchnum2, note, &is_end_pitch, track)==false)
    return p_Create(0,0,1);

  R_ASSERT_RETURN_IF_FALSE2(note.get()!=NULL, p_Create(0,0,1));

  if (is_end_pitch)
    return ratio2place(note->d._end);
  else if (pitchnum2>=0){
    r::PitchTimeData::Reader reader(&note->_pitches);
    R_ASSERT_RETURN_IF_FALSE2(pitchnum2 < reader.size(), p_Create(0,0,1));
    return ratio2place(reader.at_ref(pitchnum2)._time);
  }else
    return ratio2place(note->get_time());
}

void setCurrentPitchnum(int num, int tracknum, int blocknum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(-1, &window, blocknum, &wblock, tracknum);
  if(wtrack==NULL)
    return;

  r::NotePtr note;
  
  bool is_end_pitch;
  int pitchnum2;
  if (getPitch2(num, pitchnum2, note, &is_end_pitch, wtrack->track)==false)
    return;

  if (pitchnum2 >= 0 && !is_end_pitch) {
    const r::PitchTimeData::Reader reader(&note->_pitches);
    R_ASSERT_RETURN_IF_FALSE(pitchnum2 < reader.size());
    API_setCurrentNode2(reader.at_ref(pitchnum2)._id);
  } else {
    API_setCurrentNode2(note->_id);
  }
}

void setIndicatorPitchnum(int num, int tracknum, int blocknum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(-1, &window, blocknum, &wblock, tracknum);
  if(wtrack==NULL)
    return;

  r::NotePtr note;
  
  bool is_end_pitch;
  int pitchnum2;

  if (getPitch2(num, pitchnum2, note, &is_end_pitch, wtrack->track)==false)
    return;

  API_setIndicatorNode2(note->_id);

  if(pitchnum2<0)
    g_indicator_pitch_num = 0;
  else
    g_indicator_pitch_num = pitchnum2 + 1;
}

static int setPitchnum2(int num, float value, Place place, int tracknum, int blocknum, int windownum, bool replace_note_ends){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);

  if (wtrack==NULL)
    return num;

  struct Blocks *block = wblock->block;  
  struct Tracks *track = wtrack->track;

  float clamped_value = R_BOUNDARIES(1,value,127);

  r::NotePtr note;

  bool is_end_pitch;

  int pitchnum2;
  
  if (getPitch2(num, pitchnum2, note, &is_end_pitch, track)==false)
    return num;

  window->must_redraw_editor = true;

  if (pitchnum2 >= 0 && !is_end_pitch) {

    R_ASSERT_NON_RELEASE(pitchnum2 >= 0);
    
    r::PitchTimeData::Writer writer(&note->_pitches);

    R_ASSERT_RETURN_IF_FALSE2(pitchnum2 < writer.size(), num);

    r::Pitch &pitch2 = writer.at_ref(pitchnum2);
    
    if (value > 0){
      pitch2._val = clamped_value;
    }
    
    if (!p_is_same_place(place)){
      
      if(place.line < 0){handleError("Negative place");return num;}

      {
        Ratio ratio = place2ratio(place);
        
        if (ratio < 0) {
          
          handleError("Position before start of block");
          
        } else if (ratio > wblock->block->num_lines) {
          
          handleError("Position after end of block");
          
        } else {
          
          writer.constraint_move(pitchnum2,
                                 R_BOUNDARIES(note->get_time(), place2ratio(place), note->d._end),
                                 wblock->block->num_lines
                                 );
        }
      }

    }
                        
  } else if (is_end_pitch){

    if (value > 0)
      note->d._pitch_end = clamped_value;
    
    if (!p_is_same_place(place)){
      int64_t id = MoveEndNote2(block, track, note, place2ratio(place), true);
      if (id < 0)
        return num;
      
      return getPitchNum2(track, id, -1, true);
    }
    
  } else {

    if (value > 0)
      note->_val = clamped_value; // Perhaps we should use writer when setting note value?

    if (place.line >= 0) {
      if(place.line < 0){handleError("Negative place");return num;}
      int64_t id = MoveNote2(block, track, note, place2ratio(place), replace_note_ends);
      if (id < 0)
        return num;
          
      return getPitchNum2(track, id, -1, false);
    }
  }

  return num;
}

int setPitchnum(int num, float value, Place place, int tracknum, int blocknum, int windownum){
  return setPitchnum2(num, value, place, tracknum, blocknum, windownum, true);
}
  
static r::NotePtr getNoteAtRatio(struct Tracks *track, const Ratio &ratio){
  const r::NoteTimeData::Reader reader(track->_notes2);

  for(const r::NotePtr &note : reader)
    if (ratio >= note->get_time() && ratio < note->d._end)
      return note;
  
  return r::NotePtr();
}

/*
static struct Notes *getNoteAtPlace2(struct Tracks *track, Place *place){
  
  struct Notes *note = track->notes;

  while(note != NULL){
    Place p = ratio2place(note->end);
    if (PlaceIsBetween3(place, &note->l.p, &p))
      return note;
    else
      note = NextNote(note);
  }

  return NULL;
}
*/

static int addNote4(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack, Place *place, float value){

  int64_t id = InsertNote(wblock, wtrack, place, NULL, value, NOTE_get_velocity(wtrack->track), false);

  window->must_redraw_editor = true;

  const r::NoteTimeData::Reader reader(wtrack->track->_notes2);

  return getPitchNum2(wtrack->track, id, -1, false);
}

/*
static int addPitch2(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack, struct Notes *note, Place *place, float value){

  if (validate_place(*place)==false)
    return -1;

  int pos = AddPitch(window, wblock, wtrack, note, place, value);

  if(pos < 0)
    return -1;

  //if (note->pitch_end==0)
  //  note->pitch_end = value;
  
  window->must_redraw_editor = true;

  return getPitchNum(wtrack->track, note, pos, false);
}
*/

static int addPitch3(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack, r::NotePtr &note, const Ratio &ratio, float value){

  if (validate_place(ratio2place(ratio))==false)
    return -1;

  int pos = AddPitch2(window, wblock, wtrack, note, ratio, value);

  if(pos < 0)
    return -1;

  //if (note->pitch_end==0)
  //  note->pitch_end = value;
  
  window->must_redraw_editor = true;

  return getPitchNum2(wtrack->track, note->_id, pos, false);
}

int addPitchnum(float value, Place place, int tracknum, int blocknum, int windownum){

  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);

  if (wtrack==NULL)
    return -1;

  if (validate_place2(place, wblock->block)==false)
    return -1;
  
  value = R_BOUNDARIES(1,value,127);

  ADD_UNDO(Notes(window,wblock->block,wtrack->track,window->wblock->curr_realline));

  int ret;
  

#if 1
  r::NotePtr note = getNoteAtRatio(wtrack->track, place2ratio(place));
  
  if (note){
    ret = addPitch3(window, wblock, wtrack, note, place2ratio(place), value);
  }else{
    ret = addNote4(window, wblock, wtrack, &place, value);
  }
#else
  struct Notes *note = getNoteAtPlace(wtrack->track, &place);

  if(note==NULL)
    ret = addNote4(window, wblock, wtrack, &place, value);
  else
    ret = addPitch2(window, wblock, wtrack, note, &place, value);
#endif

  if (ret==-1)
    UNDO_CANCEL_LAST_UNDO();

  //printf("\n\n\n\n ***** NUM: %d\n",ret);
  return ret;
}

int addPitchnumF(float value, float floatplace, int tracknum, int blocknum, int windownum){
  if (floatplace < 0){
    handleError("Place can not be negative: %f", (double)floatplace);
    return -1;
  }

  Place place;
  Float2Placement(floatplace, &place);
  return addPitchnum(value, place, tracknum, blocknum, windownum);
}

bool portamentoEnabled(dyn_t dynnote, int tracknum, int blocknum, int windownum){
#if 0
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note = getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (note==NULL)
    return false;

  //if (note->pitch_first_logtype==LOGTYPE_HOLD && note->pitches==NULL && note->pitch_end==note->note)
  //  return false;

  return note->pitch_end > 0;
#else
  const r::NotePtr note=getNoteFromNum2(windownum,blocknum,tracknum,dynnote);
  if (note.get()==NULL)
    return false;
  else
    return note->d._pitch_end > 0;
#endif
}

void setNoteEndPitch(float value, dyn_t dynnote, int tracknum, int blocknum, int windownum){
  r::NotePtr note=getNoteFromNum2(windownum,blocknum,tracknum,dynnote);
  if (!note)
    return;

  note->d._pitch_end = value;
}

void enablePortamento(dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;

  r::NotePtr note = getNoteFromNumA2(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (!note)
    return;

  if (equal_floats(note->d._pitch_end, 0)) {
    window->must_redraw_editor = true;
    note->d._pitch_end = note->_val;
  }
}

void disablePortamento(dyn_t dynnote, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;

  r::NotePtr note = getNoteFromNumA2(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
  if (!note)
    return;

  {
    r::PitchTimeData::Writer writer(&note->_pitches);
    writer.clear();
    note->d._pitch_end = 0;
  }
  
  window->must_redraw_editor = true;
}

// subtracks
///////////////////////////////////////////////////
static struct WTracks *getSubtrackWTrack(int subtracknum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return NULL;

  if (subtracknum>=GetNumSubtracks(wtrack)){
    handleError("No subtrack %d in track %d in block %d (only %d subtracks in this track)\n", subtracknum, tracknum, blocknum, GetNumSubtracks(wtrack));
    return 0;
  }

  return wtrack;
}

float getSubtrackX1(int subtracknum, int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getSubtrackWTrack(subtracknum, tracknum, blocknum, windownum);
  if (wtrack==NULL)
    return 0.0f;
  else
    return GetXSubTrack1(wtrack,subtracknum);
}

float getSubtrackX2(int subtracknum, int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getSubtrackWTrack(subtracknum, tracknum, blocknum, windownum);
  if (wtrack==NULL)
    return 0.0f;
  else
    return GetXSubTrack2(wtrack,subtracknum);
}



// fxs
//////////////////////////////////////////////////

void requestFX(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window=NULL;
  struct WTracks *wtrack;
  struct WBlocks *wblock;

  wtrack=getWTrackFromNumA(
                           windownum,
                           &window,
                           blocknum,
                           &wblock,
                           tracknum
                           );

  if(wtrack==NULL) return;

  //printf("x: %f, y: %f\n",tevent.x, tevent.y);
  
  AddFXNodeLineCurrPos(window, wblock, wtrack);
}

/*
void requestFXMousePos(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if(window==NULL)
    return;

  AddFXNodeLineCurrMousePos(window);
}
*/

static int get_effect_num(struct Patch *patch, const char *fx_name){
  int num_fxs = patch->instrument->getNumFxs(patch);
  for(int fxnum=0;fxnum<num_fxs;fxnum++)
    if (!strcmp(fx_name, patch->instrument->getFxName(patch, fxnum)))
      return fxnum;

  return -1;
}

int getFx(const char* fx_name, int tracknum, instrument_t instrument_id, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return -1;

  struct Tracks *track = wtrack->track;

  struct Patch *patch = track->patch;

  if(patch==NULL){
    handleError("Track %d has no assigned instrument",tracknum);
    return -1;
  }

  if (isLegalInstrument(instrument_id)){
    if (track->patch->instrument != get_audio_instrument()){
      RError("track->patch->instrument != get_audio_instrument()");
    } else {
      patch = getPatchFromNum(instrument_id);
    }
  }
  
  if (patch==NULL)
    return -1;

  int effect_num = get_effect_num(patch, fx_name);
  if (effect_num==-1){
    handleError("No effect \"%s\" for the instrument %s.", fx_name, patch->name);
    return -1;
  }

  int num = 0;

  VECTOR_FOR_EACH(struct FXs *, fxs, &wtrack->track->fxs){
    if (fxs->fx->effect_num == effect_num && fxs->fx->patch==patch)
      return num;    
    num++;
  }END_VECTOR_FOR_EACH;

  return -2;
}

int addFx(float value, Place place, const char* fx_name, int tracknum, instrument_t instrument_id, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return -1;

  if(value < 0 || value > 1){
    GFX_addMessage("addFx: Value must be between 0 and 1. Found %f", (double)value);
    value = R_BOUNDARIES(0, value, 1);
  }

  
  struct Tracks *track = wtrack->track;

  printf("\n\n    createFX track %d: %s. num_fx: %d\n\n", track->l.num, fx_name, track->fxs.num_elements);
  
  struct Patch *patch = track->patch;

  if(patch==NULL){
    handleError("Track %d has no assigned instrument",tracknum);
    return -1;
  }

  if (isLegalInstrument(instrument_id)){
    if (track->patch->instrument != get_audio_instrument()){
      handleError("An audio instrument is not assigned to track %d", track->l.num);
      return -1;
    } else {
      patch = getPatchFromNum(instrument_id);
    }
  }
  
  if (patch==NULL)
    return -1;

  int effect_num = get_effect_num(patch, fx_name);
  if (effect_num==-1){
    handleError("No effect \"%s\" for the instrument %s.", fx_name, patch->name);
    return -1;
  }

  VECTOR_FOR_EACH(struct FXs *, fxs, &wtrack->track->fxs){
    if (fxs->fx->effect_num == effect_num && fxs->fx->patch==patch){
      GFX_Message2(NULL, true, "addFX: Effect \"%s\" has already been added to track %d", fx_name, track->l.num);
      return -1;
    }
  }END_VECTOR_FOR_EACH;

  
  {
    struct FX *fx = patch->instrument->createFX(track, patch, effect_num);

    if (fx==NULL){
      printf("   FX returned NULL\n");
      return -1;
    }

    printf("  1. p.line: %d, p.c: %d, p.d: %d\n",place.line,place.counter,place.dividor);

    AddFXNodeLineCustomFxAndPos(window, wblock, wtrack, fx, &place, value);

    printf("  2. p.line: %d, p.c: %d, p.d: %d\n",place.line,place.counter,place.dividor);
        
    int num = 0;

    VECTOR_FOR_EACH(struct FXs *, fxs, &wtrack->track->fxs){
      if (fxs->fx == fx)
        return num;

      num++;
    }END_VECTOR_FOR_EACH;

#if defined(RELEASE)
    RError("Internal error: Newly created FX not found, even though it was just created");
#else
    abort();
#endif
    
    return -1;
  }
}

int addFxF(float value, float floatplace, const char* fx_name, int tracknum, instrument_t instrument_id, int blocknum, int windownum){
  if (floatplace < 0){
    handleError("Place can not be negative: %f", (double)floatplace);
    return -1;
  }

  Place place;
  Float2Placement(floatplace, &place);

  return addFx(value, place, fx_name, tracknum, instrument_id, blocknum, windownum);
}

static struct Node2 *get_fxnode(int fxnodenum, int fxnum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fxs = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fxs==NULL)
    return NULL;
  
  const vector_t *nodes = GetFxNodes(window, wblock, wtrack, fxs);
  return (struct Node2 *)VECTOR_get2(nodes, fxnodenum, "fx node");
}


float getFxnodeX(int num, int fxnum, int tracknum, int blocknum, int windownum){
  struct Node2 *node = get_fxnode(num, fxnum, tracknum, blocknum, windownum);
  return node==NULL ? 0 : node->x;
}

float getFxnodeY(int num, int fxnum, int tracknum, int blocknum, int windownum){
  struct Node2 *node = get_fxnode(num, fxnum, tracknum, blocknum, windownum);
  return node==NULL ? 0 : node->y-get_scroll_pos();
}

Place getFxnodePlace(int fxnodenum, int fxnum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fxs = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fxs==NULL)
    return p_Create(0,0,1);

  const r::FXTimeData::Reader reader(fxs->_fxnodes);
  if (!num_is_valid(reader, fxnodenum, "fx node"))
    return p_Create(0,0,1);
  
  const r::FXNode &fxnode = reader.at_ref(fxnodenum);
  return ratio2place(fxnode._time);  
}

float getFxnodeValue(int fxnodenum, int fxnum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fxs = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fxs==NULL)
    return 0.0f;

  int max = fxs->fx->max;
  int min = fxs->fx->min;

  const r::FXTimeData::Reader reader(fxs->_fxnodes);
  if (!num_is_valid(reader, fxnodenum, "fx node"))
    return 0.0f;
  
  const r::FXNode &fxnode = reader.at_ref(fxnodenum);
  return scale(fxnode._val, min, max, 0.0f, 1.0f);
}

int getFxnodeLogtype(int fxnodenum, int fxnum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fxs = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fxs==NULL)
    return 0.0f;

  const r::FXTimeData::Reader reader(fxs->_fxnodes);
  if (!num_is_valid(reader, fxnodenum, "fx node"))
    return 0.0f;
  
  const r::FXNode &fxnode = reader.at_ref(fxnodenum);
  return fxnode._logtype;
}

bool getFxEnabled(int fxnum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fxs = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fxs==NULL)
    return false;

  return fxs->fx->is_enabled;
}

void setFxEnabled(bool is_enabled, int fxnum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fxs = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fxs==NULL)
    return;

  {
    SCOPED_PLAYER_LOCK_IF_PLAYING();
    fxs->fx->is_enabled = is_enabled;
  }

  window->must_redraw = true; // fx must always use must_redraw, not must_redraw_editor, to update lower scrollbar and legalize cursor pos.
}

const char* getFxName(int fxnum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fxs = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fxs==NULL)
    return NULL;

  return fxs->fx->name;
}
  
instrument_t getFxInstrument(int fxnum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fxs = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fxs==NULL)
    return createIllegalInstrument();

  if (wtrack->track->patch==NULL){
    R_ASSERT_NON_RELEASE(false);
    return createIllegalInstrument();
  }

  //if (wtrack->track->patch == fxs->fx->patch)
  //  return createIllegalInstrument();
  
  return fxs->fx->patch->id;
}


const_char* getFxString(int fxnodenum, int fxnum, int tracknum, int blocknum, int windownum){   
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fxs = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fxs==NULL)
    return "<fx not found>";

  const r::FXTimeData::Reader reader(fxs->_fxnodes);
  if (!num_is_valid(reader, fxnodenum, "fx node"))
    return "<fxnode not found>";
  
  const r::FXNode &fxnode = reader.at_ref(fxnodenum);
  float val = fxnode._val;
  
  // Turned out this was a lot of work. Fix later, hopefully. No, it's hopeless. For VST, it can't be done without doing some type of hacking we don't want to do, and for the other types of sound plugins, it's extremely hairy.
  //return fx->getFXstring(fx, wtrack->track, val);

  // instead we just do this:
  struct FX *fx = fxs->fx;

  if (wtrack->track->patch->instrument==get_MIDI_instrument())
    return talloc_format("%s: %d", fx->name, (int)val);
  else if (fx->patch==wtrack->track->patch)
    return talloc_format("%s: %.01f%%", fx->name, scale_double((double)val, (double)fx->min, (double)fx->max, 0, 100));
  else
    return talloc_format("%s (%s): %.01f%%", fx->name, fx->patch->name, scale_double((double)val, (double)fx->min, (double)fx->max, 0, 100));
}

int getNumFxs(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum, tracknum);
  if (wtrack == NULL)
    return 0;

  return wtrack->track->fxs.num_elements;
}

int getNumFxnodes(int fxnum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fxs = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fxs==NULL)
    return 0;

#if 1  
  const r::FXTimeData::Reader reader(fxs->_fxnodes);
  return reader.size();
#else  
  return ListFindNumElements3((struct ListHeader3*)fxs->fxnodelines);
#endif
  
}

float getFxMinValue(int fxnum, int tracknum, int blocknum, int windownum){
  return 0.0f;
#if 0
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fx = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fx==NULL)
    return 0;

  return fx->fx->min;
#endif
}

float getFxMaxValue(int fxnum, int tracknum, int blocknum, int windownum){
  return 1.0f;
#if 0
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fx = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fx==NULL)
    return 1;

  return fx->fx->max;
#endif
}

int addFxnode(float value, Place place, int fxnum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fxs = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fxs==NULL)
    return -1;

  R_ASSERT(value >= 0.0f);
  R_ASSERT(value <= 1.0f);

  if (PlaceLessThan(&place, PlaceGetFirstPos())){
    handleError("addFxnode: placement before top of block for fx #%d. (%s)", fxnum, PlaceToString(&place));
    place = *PlaceGetFirstPos();
  }

  Place lastplace = p_Absolute_Last_Pos(wblock->block);

  if (PlaceGreaterThan(&place, &lastplace)) {    
    handleError("addFxnode: placement after fx end for fx #%d (%s). num_lines: #%d", fxnum, PlaceToString(&place), wblock->block->num_lines);
    place = lastplace;
  }

  ADD_UNDO(FXs(window, wblock->block, wtrack->track, wblock->curr_realline));

  int max = fxs->fx->max;
  int min = fxs->fx->min;

  int ret = AddFXNodeLine(
                          window,
                          wblock,
                          wtrack,
                          fxs,
                          (int)scale(value, 0,1, min, max),
                          &place
                          );

  if (ret==-1){
    //handleError("addFxnode: Can not create new fx with the same position as another fx");
    //printf("   UNDOING CANCEL\n");
    if (Undo_Is_Open()==false)
      UNDO_CANCEL_LAST_UNDO();

    return -1;
  }

  window->must_redraw = true; // fx must always use must_redraw, not must_redraw_editor, to update lower scrollbar and legalize cursor pos.

  return ret;
}

int addFxnodeF(float value, float floatplace, int fxnum, int tracknum, int blocknum, int windownum){
  if (floatplace < 0){
    handleError("Place can not be negative: %f", (double)floatplace);
    return -1;
  }

  Place place;
  Float2Placement(floatplace, &place);

  return addFxnode(value, place, fxnum, tracknum, blocknum, windownum);
}
  
void setFxnode(int fxnodenum, float value, Place place, int fxnum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fxs = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fxs==NULL)
    return;

  if(value < 0.0f || value > 1.0f){
    handleError("setFxnode: Value must be between 0 and 1, not %f. fxnodenum: %d, fxnum: %d, tracknum: %d, blocknum: %d", (double)value, fxnodenum, fxnum, tracknum, blocknum);
    return;
  }
  
  int fxmax = fxs->fx->max;
  int fxmin = fxs->fx->min;
  
  r::FXTimeData::Writer writer(fxs->_fxnodes);
  if (!num_is_valid(writer, fxnodenum, "fx node"))
    return;

  writer.at_ref(fxnodenum)._val = (int)scale(value, 0.0f, 1.0f, fxmin, fxmax);
  
  if (!p_is_same_place(place)){

    Ratio ratio = place2ratio(place);
    
    if (ratio < 0) {
      
      handleError("Position before start of block");

    } else if (ratio > wblock->block->num_lines) {
      
      handleError("Position after end of block");

    } else {

      //printf("   Time: %f. Place: %f\n", (double)ratio.num/(double)ratio.den, p_getDouble(place));
      writer.constraint_move(fxnodenum, ratio, wblock->block->num_lines);
      
    }
  }
  
  window->must_redraw_editor = true;
}

void setFxnodeF(int fxnodenum, float value, float floatplace, int fxnum, int tracknum, int blocknum, int windownum){
  Place place;

  if (floatplace >= 0.0f)
    Float2Placement(floatplace, &place);
  else {
    R_ASSERT_NON_RELEASE(false); // Don't know if this is a legal situation.
    place = g_same_place;
  }
  
  return setFxnode(fxnodenum, value, place, fxnum, tracknum, blocknum, windownum);
}

void setFxnodeLogtype(int logtype, int fxnodenum, int fxnum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fxs = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fxs==NULL)
    return;

  r::FXTimeData::Writer writer(fxs->_fxnodes);
  if (!num_is_valid(writer, fxnodenum, "fx node"))
    return;

  ADD_UNDO(FXs(window, wblock->block, wtrack->track, wblock->curr_realline));
  
  r::FXNode &fxnode = writer.at_ref(fxnodenum);

  fxnode._logtype = logtype;
  
  window->must_redraw_editor = true;
}

void deleteFxnode(int fxnodenum, int fxnum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fxs = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fxs==NULL)
    return;

  const vector_t *nodes = GetFxNodes(window, wblock, wtrack, fxs);
  if (fxnodenum < 0 || fxnodenum>=nodes->num_elements) {
    handleError("There is no fx node %d for fx %d in track %d in block %d",fxnodenum, fxnum, tracknum, blocknum);
    return;
  }

  ADD_UNDO(FXs(window, wblock->block, wtrack->track, wblock->curr_realline));

  DeleteFxNode(window, wtrack, fxs, fxnodenum); // DeleteFxNode locks player / stops playing, and so forth.
}


void setCurrentFxnode(int fxnodenum, int fxnum, int tracknum, int blocknum, int windownum){
 struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fx = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fx==NULL)
    return;

  const vector_t *nodes = GetFxNodes(window, wblock, wtrack, fx);
  if (fxnodenum < 0 || fxnodenum>=nodes->num_elements) {
    handleError("There is no fx node %d for fx %d in track %d in block %d",fxnodenum, fxnum, tracknum, blocknum);
    return;
  }

  struct Node2 *node = (struct Node2 *)nodes->elements[fxnodenum];
  API_setCurrentNode2(node->id);
}

void setIndicatorFxnode(int fxnodenum, int fxnum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fx = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fx==NULL)
    return;

  const vector_t *nodes = GetFxNodes(window, wblock, wtrack, fx);
  if (fxnodenum < 0 || fxnodenum>=nodes->num_elements) {
    handleError("There is no fx node %d for fx %d in track %d in block %d",fxnodenum, fxnum, tracknum, blocknum);
    return;
  }

  struct Node2 *node = (struct Node2 *)nodes->elements[fxnodenum];
  API_setIndicatorNode2(node->id);
}

void setNoMouseFx(int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if(wblock==NULL)
    return;
  
  if (wblock->mouse_fxs != NULL){
    wblock->mouse_fxs = NULL;
    window->must_redraw_editor = true;
    //printf("no mouse fx dirty\n");
  }
}

void setMouseFx(int fxnum, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct FXs *fxs = getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
  if (fxs==NULL)
    return;
  else if (wblock->mouse_fxs != fxs){
    wblock->mouse_fxs = fxs;
    window->must_redraw_editor = true;
    //printf("mouse fx dirty\n");
  }
}

int getMouseFx(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return -1;

  struct FXs *fxs = wblock->mouse_fxs;

  VECTOR_FOR_EACH(struct FXs *, fxs2, &wtrack->track->fxs){
    if(fxs==fxs2)
      return iterator666;
  }END_VECTOR_FOR_EACH;

  return -1;
}

void undoFxs(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return;

  ADD_UNDO(FXs(window, wblock->block, wtrack->track, wblock->curr_realline));
}


////////////////////////
// fxs, clipboard



void clearTrackFX(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
  if (wtrack==NULL)
    return;

  struct Tracks *track = wtrack->track;

  ADD_UNDO(FXs(window, wblock->block, track, wblock->curr_realline));
  
  PC_Pause();{
    VECTOR_clean(&track->fxs);
  }PC_StopPause(window);

  window->must_redraw = true; // trigger call to UpdateWBlockCoordinates and ValidateCursorPos.
}


//////////////////////////////////////////////////
// track widths
//////////////////////////////////////////////////

float getTrackWidth(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
    
  if (tracknum==-1){
    wblock = getWBlockFromNumA(windownum, &window, blocknum);
    if (wblock==NULL)
      return 0.0f;
    return wblock->temponodearea.width;
  } else {
    struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
    if (wtrack==NULL)
      return 0.0f;
    return wtrack->fxwidth;
  }
}

void undoTrackWidth(void){
  struct Tracker_Windows *window=getWindowFromNum(-1);
  if(window==NULL)
    return;
  
  ADD_UNDO(Block_CurrPos(window)); // can be optimized a lot
}

void setTrackWidth (float new_width, int tracknum, int blocknum, int windownum){
  if (new_width < 2) {
#if 0
    handleError("Can not set width smaller than 2");
    return;
#else
    new_width = 2;
#endif
  }

  struct Tracker_Windows *window;
  struct WBlocks *wblock;
          
  if (tracknum==-1){
    wblock = getWBlockFromNumA(windownum, &window, blocknum);
    if (wblock==NULL)
      return;
    wblock->temponodearea.width = (int)new_width;
  } else {
    struct WTracks *wtrack = getWTrackFromNumA(windownum, &window, blocknum, &wblock, tracknum);
    if (wtrack==NULL)
      return;
    //printf("new width: %d, old: %d\n",(int)new_width,wtrack->fxwidth);
    wtrack->fxwidth = (int)new_width;
  }

  //UpdateWBlockCoordinates(window,wblock);
  //GL_create(window);
  
  window->must_redraw=true;
}



// ctrl / shift keys
//////////////////////////////////////////////////

extern struct TEvent tevent;

bool controlPressed(void){
  return ControlPressed(); //AnyCtrl(tevent.keyswitch);
}

/*
bool control2Pressed(void){
  return Control2Pressed();
}
*/

bool horizontalModifierPressed(void){
  return HorizontalModifierPressed();
}

bool verticalModifierPressed(void){
  return VerticalModifierPressed();
}

bool shiftPressed(void){
  return ShiftPressed(); //AnyShift(tevent.keyswitch);
}

/*
// Doesn't work to check right extra
bool extraPressed(void){
  return AnyExtra(tevent.keyswitch);
}
*/

bool metaPressed(void){
  //return LeftExtra(tevent.keyswitch);
  return MetaPressed(); // osx: ctrl.
}

bool altPressed(void){
  return AltPressed();
}

/*
bool alt2Pressed(void){
#ifdef FOR_MACOSX
  return MetaPressed(); // osx: ctrl.
#else
  return AltPressed();
#endif
}
*/


// mouse pointer
//////////////////////////////////////////////////

/*
enum MousePointerType {
  MP_NORMAL,
  MP_BLANK,
  MP_DIAGONAL,
  MP_HORIZONTAL,  
};
*/

void setNormalMousePointer(int64_t guinum){
  if (guinum<0){
    handleError("guinum for cursor must be 0 or larger");
    return;
  }
  //printf("a) api_mouse: Set NORMAL  mouse %d\n", (int)guinum);
    SetNormalPointer(guinum);
}
void setPointingMousePointer(int64_t guinum){
  if (guinum<0){
    handleError("guinum for cursor must be 0 or larger");
    return;
  }
  //printf("b) api_mouse: Set POINTING mouse %d\n", (int)guinum);
    SetPointingPointer(guinum);
}
void setOpenHandMousePointer(int64_t guinum){
  if (guinum<0){
    handleError("guinum for cursor must be 0 or larger");
    return;
  }
  //printf("c) api_mouse: Set OPEN mouse %d\n", (int)guinum);
  SetOpenHandPointer(guinum);
}
void setClosedHandMousePointer(int64_t guinum){
  if (guinum<0){
    handleError("guinum for cursor must be 0 or larger");
    return;
  }
  SetClosedHandPointer(guinum);
}
void setBlankMousePointer(int64_t guinum){
  if (guinum<0){
    handleError("guinum for cursor must be 0 or larger");
    return;
  }
    SetBlankPointer(guinum);
}
void setDiagonalResizeMousePointer(int64_t guinum){
  if (guinum<0){
    handleError("guinum for cursor must be 0 or larger");
    return;
  }
    SetDiagResizePointer(guinum);
}
void setHorizontalResizeMousePointer(int64_t guinum){
  if (guinum<0){
    handleError("guinum for cursor must be 0 or larger");
    return;
  }
  SetHorizResizePointer(guinum);
}
void setHorizontalSplitMousePointer(int64_t guinum){
  if (guinum<0){
    handleError("guinum for cursor must be 0 or larger");
    return;
  }
  SetHorizSplitPointer(guinum);
}
void setVerticalResizeMousePointer(int64_t guinum){
  if (guinum<0){
    handleError("guinum for cursor must be 0 or larger");
    return;
  }
  SetVerticalResizePointer(guinum);
}

void setVerticalSplitMousePointer(int64_t guinum){
  if (guinum<0){
    handleError("guinum for cursor must be 0 or larger");
    return;
  }
  SetVerticalSplitPointer(guinum);
}

void moveMousePointer(float x, float y, int windownum){
  struct Tracker_Windows *window = getWindowFromNum(windownum);
  if (window!=NULL)
    MovePointer(window, x, y);
}

void moveAbsMousePointer(float x, float y, int windownum){
  struct Tracker_Windows *window = getWindowFromNum(windownum);
  if (window!=NULL)
    MoveAbsPointer(window, x, y);
}

bool isLeftMousePressed(void){
  return GetMouseButtons() & (2<<TR_LEFTMOUSEDOWN);
}
bool isRightMousePressed(void){
  return GetMouseButtons() & (2<<TR_RIGHTMOUSEDOWN);
}
bool isMiddleMousePressed(void){
  return GetMouseButtons() & (2<<TR_MIDDLEMOUSEDOWN);
}

double getDoubleClickInterval(void){
  return GetDoubleClickInterval();
}

float getMousePointerX(int windownum){
  
  if (windownum==-2){

    if (sequencerInFullMode() || sequencerInWindow())
      return GetPointerPos(NULL).x + 10000;
    else
      windownum=-1;
  }
  
  struct Tracker_Windows *window = getWindowFromNum(windownum);
  if (window==NULL)
    return 0;
  
  return GetPointerPos(window).x;
}

float getMousePointerY(int windownum){
  if (windownum==-2){

    if (sequencerInFullMode() || sequencerInWindow())
      return GetPointerPos(NULL).y + 10000;
    else
      windownum=-1;
    
  }

  struct Tracker_Windows *window = getWindowFromNum(windownum);
  if (window==NULL)
    return 0;
  
  WPoint ret = GetPointerPos(window);
  return ret.y;
}

float getAbsMousePointerX(int windownum){
  struct Tracker_Windows *window = getWindowFromNum(windownum);
  if (window==NULL)
    return 0;

  WPoint ret = GetAbsPointerPos(window);
  return ret.x;
}
float getAbsMousePointerY(int windownum){
  struct Tracker_Windows *window = getWindowFromNum(windownum);
  if (window==NULL)
    return 0;

  WPoint ret = GetAbsPointerPos(window);
  return ret.y;
}

int getScreenWidth(int windownum){
  struct Tracker_Windows *window = getWindowFromNum(windownum);
  if (window==NULL)
    return 0;

  Area ret = GetScreenSize(window);
  return ret.x;
}
int getScreenHeight(int windownum){
  struct Tracker_Windows *window = getWindowFromNum(windownum);
  if (window==NULL)
    return 0;

  Area ret = GetScreenSize(window);
  return ret.x2;
}

bool mousePointerInMixer(void){
  return MW_has_mouse_pointer();
}

float g_mouse_dx = 0;
float g_mouse_dy = 0;

float getDeltaMouseX(void){
  float ret = g_mouse_dx;
  g_mouse_dx = 0;
  return ret;
}

float getDeltaMouseY(void){
  float ret = g_mouse_dy;
  g_mouse_dy = 0;
  return ret;
}

bool hasDeltaMouse(void){
#if defined(FOR_WINDOWS)
  return W_HasDeltaMouse();
#else
  return false;
#endif
}

bool canMovePointer(void){
#if defined(FOR_WINDOWS)
  return W_CanMovePointer();
#else
  return true;
#endif
}

float getGlobalMousePointerX(void){
  WPoint ret = GetPointerPos(NULL);
  return ret.x;
}
float getGlobalMousePointerY(void){
  WPoint ret = GetPointerPos(NULL);
  return ret.y;
}

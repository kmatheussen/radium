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

#define __STDC_FORMAT_MACROS

#include "../common/includepython.h"

#include <unistd.h>

#include <gc.h>

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wshorten-64-to-32"
#include <QVector> // Shortening warning in the QVector header. Temporarily turned off by the surrounding pragmas.
#pragma clang diagnostic pop
#include <QLinkedList>
#include <QThread>
#include <QUuid>
#include <QClipboard>

#include "../bin/packages/s7/s7.h"

#include "../common/nsmtracker.h"
#include "../common/list_proc.h"
#include "../common/ratio_funcs.h"
#include "../common/placement_proc.h"
#include "../common/velocities_proc.h"
#include "../common/tempos_proc.h"
#include "../common/swing_proc.h"
#include "../common/Signature_proc.h"
#include "../common/LPB_proc.h"
#include "../common/temponodes_proc.h"
#include "../common/fxlines_proc.h"
#include "../common/notes_proc.h"
#include "../common/pitches_proc.h"
#include "../common/wblocks_proc.h"
#include "../common/disk.h"
#include "../common/disk_save_proc.h"
#include "../common/disk_load_proc.h"
#include "../common/lines_proc.h"
#include "../common/reallines_insert_proc.h"
#include "../common/block_properties_proc.h"
#include "../common/track_insert_proc.h"
#include "../common/tracks_proc.h"
#include "../common/wtracks_proc.h"
#include "../common/undo_trackheader_proc.h"
#include "../common/block_insert_proc.h"
#include "../common/block_delete_proc.h"
#include "../common/block_split_proc.h"
#include "../common/eventreciever_proc.h"
#include "../common/visual_proc.h"
#include "../common/OS_settings_proc.h"
#include "../common/OS_visual_input.h"
#include "../common/settings_proc.h"
#include "../common/player_proc.h"
#include "../common/player_pause_proc.h"
#include "../common/undo_blocks_proc.h"
#include "../common/time_proc.h"
#include "../common/sequencer_proc.h"
#include "../common/patch_proc.h"
#include "../common/OS_string_proc.h"
#include "../common/Dynvec_proc.h"
#include "../common/PriorityQueue.hpp"
#include "../common/undo_tempos_proc.h"
#include "../common/undo_temponodes_proc.h"
#include "../common/undo_lpbs_proc.h"
#include "../common/undo_swings_proc.h"
#include "../common/undo_signatures_proc.h"
#include "../common/undo_notes_proc.h"
#include "../common/undo_notesandfxs_proc.h"
#include "../common/undo_fxs_proc.h"
#include "../common/swingtext_proc.h"
#include "../common/fxtext_proc.h"
#include "../common/chancetext_proc.h"
#include "../common/centtext_proc.h"
#include "../common/veltext_proc.h"

#include "../embedded_scheme/scheme_proc.h"
#include "../embedded_scheme/s7extra_proc.h"

#include "../OpenGL/Widget_proc.h"
#include "../OpenGL/Render_proc.h"

#include "../audio/SoundProducer_proc.h"
#include "../audio/Mixer_proc.h"
#include "../audio/Faust_plugins_proc.h"
#include "../audio/SampleReader_proc.h"
#include "../audio/SoundfileSaver_proc.h"

#include "../midi/midi_i_input_proc.h"

#include "../mixergui/QM_MixerWidget.h"
#include "../crashreporter/crashreporter_proc.h"
#include "../Qt/Qt_comment_dialog_proc.h"
#include "../Qt/EditorWidget.h"
#include "../Qt/KeyboardFocusFrame.hpp"
#include "../Qt/Qt_PopupMenu_proc.h"
#include "../Qt/Qt_Menues_proc.h"

#ifdef _AMIGA
#include "Amiga_colors_proc.h"
#endif

#include "api_common_proc.h"

#include "api_various_proc.h"
#include "radium_proc.h"



void editorWindowToFront(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
	GFX_EditorWindowToFront(window);
}
void playListWindowToFront(void){
	GFX_PlayListWindowToFront();
}
void instrumentWindowToFront(void){
	GFX_InstrumentWindowToFront();
}
void setMixerRotate(float rotate){
  MW_set_rotate(rotate);
}

bool mainMixerIsModular(void){
  return MW_modular_mixer_is_visible();
}

void setMainMixerIsModular(bool show_modular){
  MW_set_modular_mixer_type(show_modular);
}

void switchMainMixerIsModular(void){
  setMainMixerIsModular(!mainMixerIsModular());
}

void setMainMixerInWindow(bool show_window){
  MW_set_window_mode(show_window);
}
  
bool mainMixerIsInWindow(void){
  return MW_is_in_window_mode();
}
  
bool switchMixerIsInWindow(void){
  bool ret = !MW_is_in_window_mode();
  setMainMixerInWindow(ret);
  return ret;
}

void setShowCpuUsageInMixer(bool showit){
  if (showit != getShowCpuUsageInMixer()){
    ATOMIC_SET(g_show_cpu_usage_in_mixer, showit);
    MW_update_all_chips();
    MW_update_checkboxes();
  }
}

bool getShowCpuUsageInMixer(void){
  return ATOMIC_GET(g_show_cpu_usage_in_mixer);
}

bool switchShowCpuUsageInMixer(void){
  bool ret = !getShowCpuUsageInMixer();
  setShowCpuUsageInMixer(ret);
  return ret;
}

void setVisibleMixerConnections(bool val){
  MW_set_connections_visibility(val);
  MW_update_checkboxes();
}

bool getVisibleMixerConnections(void){
  return MW_get_connections_visibility();
}

bool switchVisibleMixerConnections(void){
  bool ret = !getVisibleMixerConnections();
  setVisibleMixerConnections(ret);
  return ret;
}

void setVisibleMixerBusConnections(bool val){
  MW_set_bus_connections_visibility(val);
  MW_update_checkboxes();
}

bool getVisibleMixerBusConnections(void){
  return MW_get_bus_connections_visibility();
}

bool switchVisibleMixerBusConnections(void){
  bool ret = !getVisibleMixerBusConnections();
  setVisibleMixerBusConnections(ret);
  return ret;
}

static bool g_showInstrumentWidgetWhenDoubleClickingSoundObject = false;

bool showInstrumentWidgetWhenDoubleClickingSoundObject(void){
  static bool has_inited = false;

  if (has_inited==false){
    g_showInstrumentWidgetWhenDoubleClickingSoundObject = SETTINGS_read_bool("show_instrument_widget_when_double_clicking_sound_object", false);
    has_inited = true;
  }

  return g_showInstrumentWidgetWhenDoubleClickingSoundObject;
}

void setShowInstrumentWidgetWhenDoubleClickingSoundObject(bool val){
  g_showInstrumentWidgetWhenDoubleClickingSoundObject = val;
  SETTINGS_write_bool("show_instrument_widget_when_double_clicking_sound_object", val);
}


static bool g_showPlaylistDuringStartup = true;

bool showPlaylistDuringStartup(void){
  static bool has_inited = false;

  if (has_inited==false){
    g_showPlaylistDuringStartup = SETTINGS_read_bool("show_playlist_during_startup", g_showPlaylistDuringStartup);
    has_inited = true;
  }

  return g_showPlaylistDuringStartup;
}

void setShowPlaylistDuringStartup(bool val){
  g_showPlaylistDuringStartup = val;
  SETTINGS_write_bool("show_playlist_during_startup", val);
}


static bool g_showMixerStripDuringStartup = true;

bool showMixerStripDuringStartup(void){
  static bool has_inited = false;

  if (has_inited==false){
    g_showMixerStripDuringStartup = SETTINGS_read_bool("show_mixer_strip_during_startup", true);
    has_inited = true;
  }

  return g_showMixerStripDuringStartup;
}

void setShowMixerStripDuringStartup(bool val){
  g_showMixerStripDuringStartup = val;
  SETTINGS_write_bool("show_mixer_strip_during_startup", val);
}


static bool g_instrumentWidgetIsInMixer = false;

bool instrumentWidgetIsInMixer(void){
  static bool has_inited = false;

  if (has_inited==false){
    g_instrumentWidgetIsInMixer = SETTINGS_read_bool("position_instrument_widget_in_mixer", false);
    has_inited = true;
  }

  return g_instrumentWidgetIsInMixer;
}

void setInstrumentWidgetInMixer(bool val){
  g_instrumentWidgetIsInMixer = val;
  SETTINGS_write_bool("position_instrument_widget_in_mixer", val);

  MW_set_instrument_in_mixer(val);
}

bool switchInstrumentWidgetInMixer(void){
  bool ret = !instrumentWidgetIsInMixer();
  setInstrumentWidgetInMixer(ret);
  return ret;
}



static bool g_showMixerStripOnLeftSide = true;

bool showMixerStripOnLeftSide(void){
  static bool has_inited = false;

  if (has_inited==false){
    g_showMixerStripOnLeftSide = SETTINGS_read_bool("show_mixer_strip_on_left_side", true);
    has_inited = true;
  }

  return g_showMixerStripOnLeftSide;
}

void setShowMixerStripOnLeftSide(bool val){
  g_showMixerStripOnLeftSide = val;
  SETTINGS_write_bool("show_mixer_strip_on_left_side", val);
}


static bool g_sequencerWindowIsChildOfMainWindow = true;

bool sequencerWindowIsChildOfMainWindow(void){
  static bool has_inited = false;

  if (has_inited==false){
    g_sequencerWindowIsChildOfMainWindow = SETTINGS_read_bool("sequencer_window_is_child_of_main_window", g_sequencerWindowIsChildOfMainWindow);
    has_inited = true;
  }

  return g_sequencerWindowIsChildOfMainWindow;
}

void setSequencerWindowIsChildOfMainWindow(bool val){
  g_sequencerWindowIsChildOfMainWindow = val;
  SETTINGS_write_bool("sequencer_window_is_child_of_main_window", val);
}


static bool g_mixerWindowIsChildOfMainWindow = true;

bool mixerWindowIsChildOfMainWindow(void){
  static bool has_inited = false;

  if (has_inited==false){
    g_mixerWindowIsChildOfMainWindow = SETTINGS_read_bool("mixer_window_is_child_of_main_window", g_mixerWindowIsChildOfMainWindow);
    has_inited = true;
  }

  return g_mixerWindowIsChildOfMainWindow;
}

void setMixerWindowIsChildOfMainWindow(bool val){
  g_mixerWindowIsChildOfMainWindow = val;
  SETTINGS_write_bool("mixer_window_is_child_of_main_window", val);
}


void toggleCurrWindowFullScreen(void){
  GFX_toggleCurrWindowFullScreen();
}

void toggleFullScreen(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;

#if 0
  if(GFX_InstrumentWindowIsVisible()==true)
    GFX_SetMinimalInstrumentWindow();
#endif

  GFX_toggleFullScreen(window);

#if 0  
  if(GFX_InstrumentWindowIsVisible()==true)
    GFX_SetMinimalInstrumentWindow();
#endif
}

void showHideEditor(int windownum){
  //struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  GFX_showHideEditor();
}

bool editorIsVisible(int windownum){
  return GFX_EditorIsVisible();
}

void showEditor(void){
  GFX_ShowEditor();
}
  
void hideEditor(void){
  GFX_HideEditor();
}
  
void setEditorKeyboardFocus(bool setit){
  FOCUSFRAMES_set_focus(radium::KeyboardFocusFrameType::EDITOR, setit);
}

void setMixerKeyboardFocus(bool setit){
  FOCUSFRAMES_set_focus(radium::KeyboardFocusFrameType::MIXER, setit);
}

void setSequencerKeyboardFocus(bool setit){
  FOCUSFRAMES_set_focus(radium::KeyboardFocusFrameType::SEQUENCER, setit);
}

void setBestGuessKeyboardFocus(void){
  FOCUSFRAMES_set_focus_best_guess();
}

bool editorHasKeyboardFocus(void){
  return FOCUSFRAMES_has_focus(radium::KeyboardFocusFrameType::EDITOR);
}

bool mixerHasKeyboardFocus(void){
  return FOCUSFRAMES_has_focus(radium::KeyboardFocusFrameType::MIXER);
}

bool sequencerHasKeyboardFocus(void){
  return FOCUSFRAMES_has_focus(radium::KeyboardFocusFrameType::SEQUENCER);
}


void showSequencer(void){
  GFX_ShowSequencer();
}

void hideSequencer(void){
  GFX_HideSequencer();
}

bool sequencerIsVisible(void){
  return GFX_SequencerIsVisible();
}

void showHideSequencer(void){
  if(sequencerIsVisible())
    hideSequencer();
  else
    showSequencer();
}

void setSequencerInWindow(bool doit){
  S7CALL2(void_bool, "FROM-C-sequencer-set-gui-in-window!", doit);
}

bool sequencerInWindow(void){
  return S7CALL2(bool_void, "FROM-C-sequencer-gui-in-window");
}

void showHideSequencerInWindow(void){
  setSequencerInWindow(!sequencerInWindow());    
}

void setSequencerInFullMode(bool doit){
  S7CALL2(void_bool, "FROM_C-show-sequencer-in-full-mode!", doit);
}

bool sequencerInFullMode(void){
  return !upperPartOfMainWindowIsVisible();
}

bool switchSequencerInFullMode(void){
  bool ret = !sequencerInFullMode();
  setSequencerInFullMode(ret);

  return ret;
}

void switchSequencerPlaylistConfiguration(void){
  bool seq  = GFX_SequencerIsVisible();
  bool play = GFX_PlaylistWindowIsVisible();

  int state = (seq ? 2 : 0) + (play ? 1 : 0);

  switch(state){
    case 0: // 00 -> 01
      GFX_PlayListWindowToFront();
      break;
    case 1: // 01 -> 10
      GFX_ShowSequencer();
      GFX_PlayListWindowToBack();
      break;
    case 2: // 10 -> 11
      GFX_PlayListWindowToFront();
      break;
    case 3: // 11 -> 00
      GFX_HideSequencer();
      GFX_PlayListWindowToBack();
      break;
  }
}

void showHideMixerWidget(void){
  GFX_showHideMixerWidget();
}

void showHideInstrumentWidget(int windownum){
  //struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  //GFX_showHideInstrumentWidget(window);
  if(GFX_InstrumentWindowIsVisible())
    GFX_InstrumentWindowToBack();
  else{
    GFX_InstrumentWindowToFront();
  }
}

void showHideEditWidget(void){
  //struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  //GFX_showHideInstrumentWidget(window);
  if(editGuiIsVisible())
    hideEditGui();
  else{
    showEditGui();
  }
}

void hideUpperPartOfMainWindow(void){
  struct Tracker_Windows *window = root->song->tracker_windows;
  EditorWidget *editor = static_cast<EditorWidget*>(window->os_visual.widget);
  editor->xsplitter->hide();
}

void showUpperPartOfMainWindow(void){
  struct Tracker_Windows *window = root->song->tracker_windows;
  EditorWidget *editor = static_cast<EditorWidget*>(window->os_visual.widget);
  editor->xsplitter->show();
}

bool upperPartOfMainWindowIsVisible(void){
  struct Tracker_Windows *window = root->song->tracker_windows;
  EditorWidget *editor = static_cast<EditorWidget*>(window->os_visual.widget);
  return editor->xsplitter->isVisible();
}

void showHideUpperPartOfMainWindow(void){
  if (upperPartOfMainWindowIsVisible())
    hideUpperPartOfMainWindow();
  else
    showUpperPartOfMainWindow();
}


static int g_max_submenues = 200;

int getMaxSubmenuEntries(void){
  static bool has_inited = false;

  if (has_inited==false){
    g_max_submenues = SETTINGS_read_int32("max_submenu_entries", g_max_submenues);
    has_inited = true;
  }

  return g_max_submenues;
}

void setMaxSubmenuEntries(int val){
  if (val != g_max_submenues){
    g_max_submenues = val;
    SETTINGS_write_int("max_submenu_entries", val);
  }
}


static float g_tab_bar_height = 1.5;

float getTabBarHeight(void){
  static bool has_inited = false;

  if (has_inited==false){
    g_tab_bar_height = SETTINGS_read_double("tab_bar_height", g_tab_bar_height);
    has_inited = true;
  }

  return g_tab_bar_height;
}

void setTabBarHeight(float new_val){
  g_tab_bar_height = new_val;
  SETTINGS_write_double("tab_bar_height", new_val);
}


#if 0
void toggleInstrumentWidgetOnly(void){
  //if(GFX_
}
#endif

void showHidePlaylist(int windownum){  
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  GFX_showHidePlaylist(window);
}

void showHideMixerStrip(int windownum){  
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  GFX_showHideMixerStrip(window);
}

void showHideMenuBar(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  if (GFX_MenuVisible(window))
    GFX_HideMenu(window);
  else
    GFX_ShowMenu(window);
}

void hideMenuBar(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  GFX_HideMenu(window);
}

void showMenuBar(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  GFX_ShowMenu(window);
}


// Switch between:
// 1. Editor alone
// 2. Editor + instrument
// 3. Mixer + instruments
void switchWindowConfiguration(void){
  printf("editor: %d\ninstrument: %d\nmixer: %d\n",GFX_EditorIsVisible(), GFX_InstrumentWindowIsVisible(), GFX_MixerIsVisible());

  // From 1 to 2
  if(GFX_EditorIsVisible()==true && GFX_InstrumentWindowIsVisible()==false && GFX_MixerIsVisible()==false){
    GFX_InstrumentWindowToFront();
    //GFX_SetMinimalInstrumentWindow();
    return;
  }

  // From 2 to 3
  if(GFX_EditorIsVisible()==true && GFX_InstrumentWindowIsVisible()==true && GFX_MixerIsVisible()==false){
    GFX_HideEditor();
    GFX_ShowMixer();
    //GFX_SetMinimalInstrumentWindow();
    return;
  }

  // To 1.
  GFX_ShowEditor();
  GFX_InstrumentWindowToBack();
  GFX_HideMixer();
}

void enableMetronome(bool onoff){
  ATOMIC_SET(root->clickonoff, onoff);
  if (!onoff)
    PATCH_silence_click_instruments();
}

bool metronomeEnabled(void){
  return ATOMIC_GET_RELAXED(root->clickonoff);
}

bool switchMetronome(void){
  bool new_value = !metronomeEnabled();
  enableMetronome(new_value);
  return new_value;
}

void enablePlayCursor(bool onoff){
  ATOMIC_SET(root->play_cursor_onoff, onoff);
  
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return;
  window->must_redraw = true;
}

bool playCursorEnabled(void){
  return ATOMIC_GET(root->play_cursor_onoff);
}

void switchPlayCursorOnOff(void){
  return enablePlayCursor(!playCursorEnabled());
}

bool editorFollowsPlayCursor(void){
  return ATOMIC_GET(root->editor_follows_play_cursor_onoff);
}

void enableEditorFollowsPlayCursor(bool onoff){
  ATOMIC_SET(root->editor_follows_play_cursor_onoff, onoff);
  
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return;
  window->must_redraw = true;
}

bool switchEditorFollowsPlayCursor(void){
  bool ret = !editorFollowsPlayCursor();
  enableEditorFollowsPlayCursor(ret);
  return ret;
}

void insertReallines(int toinsert,int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  InsertRealLines_CurrPos(window,toinsert);
}


namespace radium{
  struct GeneralTranspose{
    const bool all_tracks;
    const bool in_range;
    const bool for_entry;
    const bool is_down;
    const bool big_step;
    const bool range_or_all_tracks;

    Place y1,y2;
    
    GeneralTranspose(bool all_tracks,
                     bool in_range,
                     bool for_entry,
                     bool is_down,
                     bool big_step)
      : all_tracks(all_tracks)
      , in_range(in_range)
      , for_entry(for_entry)
      , is_down(is_down)
      , big_step(big_step)
      , range_or_all_tracks(all_tracks || in_range)
    {
      struct Tracker_Windows *window=root->song->tracker_windows;
      struct WBlocks *wblock=window->wblock;
        
      if (for_entry){
        
        int realline = wblock->curr_realline;
        if (realline < 0 || realline >= wblock->num_reallines){
          EVENTLOG_add_event(strdup(talloc_format("curr_realline: %d (%d)", wblock->curr_realline, wblock->num_reallines)));
          R_ASSERT(false);
          return;
        }

        y1 = wblock->reallines[realline]->l.p;
        y2 = realline==wblock->num_reallines-1 ? p_Absolute_Last_Pos(wblock->block) : wblock->reallines[realline+1]->l.p;
        
      } else if (in_range) {

        y1 = wblock->rangey1;
        y2 = wblock->rangey2;
        
      } else {

        y1 = PlaceFirstPos;
        y2 = p_Absolute_Last_Pos(wblock->block);
      }
    }
    
  };
}

static bool TransposeSwing(Swing *swing, radium::GeneralTranspose &gt){
  int step = gt.big_step ? 10 : 1;
  if (gt.is_down)
    step *= -1;

  int new_weight = R_BOUNDARIES(1, swing->weight + step, 99);

  if (new_weight==swing->weight)
    return false;
  
  swing->weight = new_weight;

  return true;
}

static bool TransposeSignature(Signatures *signature, radium::GeneralTranspose &gt){
  int step = gt.big_step ? 4 : 1;
  if (gt.is_down)
    step *= -1;

  int new_numerator = R_BOUNDARIES(1, signature->signature.numerator + step, 99);

  if (new_numerator==signature->signature.numerator)
    return false;

  signature->signature.numerator = new_numerator;

  return true;
}

static bool TransposeLPB(LPBs *lpb, radium::GeneralTranspose &gt){
  int step = gt.big_step ? 4 : 1;
  if (gt.is_down)
    step *= -1;

  int new_lpb = R_BOUNDARIES(1, lpb->lpb + step, 99);

  if (new_lpb==lpb->lpb)
    return false;

  lpb->lpb = new_lpb;

  return true;
}

static bool TransposeBPM(Tempos *bpm, radium::GeneralTranspose &gt){
  int step = gt.big_step ? 10 : 1;
  if (gt.is_down)
    step *= -1;

  int new_bpm = R_BOUNDARIES(1, bpm->tempo + step, 999);

  if (new_bpm==bpm->tempo)
    return false;

  bpm->tempo = new_bpm;

  return true;
}

static bool TransposeTemponode(struct WBlocks *wblock, TempoNodes *temponode, radium::GeneralTranspose &gt){
  float step = gt.big_step ? wblock->reltempomax / 5.0f : wblock->reltempomax / 20.0f;
  if (gt.is_down)
    step *= -1;

  float new_reltempo = R_BOUNDARIES(-99.999, temponode->reltempo + step, 99.999);

  //printf("       Step: %f. old: %f. new: %f\n", step, temponode->reltempo, new_reltempo);
  
  if (equal_floats(new_reltempo, temponode->reltempo))
    return false;

  temponode->reltempo = new_reltempo;

  adjust_reltempomax(wblock, new_reltempo);

  return true;
}
 
static bool TransposeFxNode(struct FX *fx, struct FXNodeLines *fxnode, radium::GeneralTranspose &gt){
  int step = gt.big_step ? 0x10 : 0x1;
  if (gt.is_down)
    step *= -1;
  
  int old_value = round(scale_double(fxnode->val, fx->min, fx->max, 0, 0x100));
  if (old_value==0x100)
    old_value=0xff;
  
  int new_value = R_BOUNDARIES(0, old_value + step, 0xff);
  
  //printf("       Step: %f. old: %f. new: %f\n", step, temponode->reltempo, new_reltempo);
  
  if (old_value==new_value)
    return false;
  
  safe_int_write(&fxnode->val, round(scale_double(new_value, 0, 0x100, fx->min, fx->max)));
  
  return true;
}

namespace{

  struct Pitches2{
    ListHeader3 l;
    float *note;
    int *chance;
    int *velocity;
  };
  
  Pitches2 *get_pitches2_from_track(struct Tracks *track){
    struct Pitches2 *pitches2s = NULL;
    
    struct Notes *notes = track->notes;
    
    while(notes!=NULL){
      {
        Pitches2 *pitches2 = (struct Pitches2*)talloc(sizeof(Pitches2));
        
        pitches2->l.p = notes->l.p;
        pitches2->note = &notes->note;
        pitches2->chance = &notes->chance;
        pitches2->velocity = &notes->velocity;
        
        ListAddElement3(&pitches2s, &pitches2->l);
      }
      
      struct Pitches *pitches = notes->pitches;
      while(pitches != NULL){
        Pitches2 *pitches2 = (struct Pitches2*)talloc(sizeof(Pitches2));
      
        pitches2->l.p = pitches->l.p;
        pitches2->note = &pitches->note;
        pitches2->chance = &pitches->chance;

        ListAddElement3(&pitches2s, &pitches2->l);
        
        pitches = NextPitch(pitches);
      }

      {
        Pitches2 *pitches2 = (struct Pitches2*)talloc(sizeof(Pitches2));
        
        pitches2->l.p = notes->end;
        if (notes->pitches != NULL && notes->pitch_end > 0.005)
          pitches2->note = &notes->pitch_end;
        pitches2->velocity = &notes->velocity_end;
        
        ListAddElement3(&pitches2s, &pitches2->l);
      }
      
      notes = NextNote(notes);
    }

    return pitches2s;
  }
}

static bool TransposeChance(Pitches2 *pitches2, radium::GeneralTranspose &gt){
  if (pitches2->chance==NULL)
    return false;
  
  int step = gt.big_step ? 0x10 : 0x1;
  if (gt.is_down)
    step *= -1;
  
  int old_value = *pitches2->chance;
  
  int new_value = R_BOUNDARIES(0, old_value + step, 0xff);
  
  //printf("       Step: %f. old: %f. new: %f\n", step, temponode->reltempo, new_reltempo);
  
  if (old_value==new_value)
    return false;
  
  safe_int_write(pitches2->chance, new_value);
  
  return true;
}
 
static bool TransposePitch2(Pitches2 *pitches2, radium::GeneralTranspose &gt, double small_step, double big_step){
  if (pitches2->note==NULL)
    return false;
  
  double step = gt.big_step ? big_step : small_step;
  if (gt.is_down)
    step *= -1;

  double old_value = *pitches2->note;
  double new_value = R_BOUNDARIES(0.01, old_value + step, 127.99);
  
  //printf("       Step: %f. old: %f. new: %f\n", step, temponode->reltempo, new_reltempo);
  
  if (equal_floats(old_value, new_value))
    return false;

  safe_float_write(pitches2->note, new_value);
  
  return true;
}

static bool TransposeCent(Pitches2 *pitches2, radium::GeneralTranspose &gt){
  return TransposePitch2(pitches2, gt, 0.01, 0.1);
}

static bool TransposePitch(Pitches2 *pitches2, radium::GeneralTranspose &gt){
  return TransposePitch2(pitches2, gt, 1, 12);
}
 
static bool TransposeVelocity(Pitches2 *pitches2, radium::GeneralTranspose &gt){
  if (pitches2->velocity==NULL)
    return false;
  
  int step = gt.big_step ? 0x10 : 0x1;
  if (gt.is_down)
    step *= -1;

  int old_value = round(scale_double(*pitches2->velocity, 0, MAX_VELOCITY, 0, 0x100));
  int new_value = R_BOUNDARIES(0, scale_double(old_value + step, 0, 0x100, 0, MAX_VELOCITY), MAX_VELOCITY);
  
  //printf("       Step: %f. old: %f. new: %f\n", step, temponode->reltempo, new_reltempo);
  
  if (old_value==new_value)
    return false;

  safe_int_write(pitches2->velocity, new_value);
  
  return true;
}
 

template <class T>
static bool general_transform_list2(radium::GeneralTranspose &gt,
                                    T *list,
                                    std::function<bool(T*, radium::GeneralTranspose&)> transformer
                                    )
{
  bool ret = false;
  
  while(list!=NULL){
    //printf("list->l.p: %s. gt.y1: %s\n", PlaceToString(&list->l.p), PlaceToString(&gt.y1));
    if (p_Less_Than(list->l.p, gt.y1))
      goto next;
    if (p_Greater_Or_Equal(list->l.p, gt.y2))
      break;
    
    if (transformer(list, gt))
      ret = true;

  next:
    list = (T*)list->l.next;
  }

  return ret;
}


template <class T>
static bool general_transform_list(struct WBlocks *wblock, struct WTracks *wtrack,
                                   radium::GeneralTranspose &gt,
                                   std::function<T*(Blocks*, Tracks*)> get_T,
                                   std::function<bool(T*, radium::GeneralTranspose&)> transformer,
                                   std::function<void(Tracks*)> make_undo
                                   ){
  
  struct Blocks *block = wblock->block;

  bool ret = false;

  if (!gt.range_or_all_tracks || wtrack==NULL) {

    struct Tracks *track = wtrack==NULL ? NULL : wtrack->track;

    make_undo(track);
    
    if (general_transform_list2(gt, get_T(block, track), transformer))
      ret = true;
      
  } else {
  
    if (gt.in_range && !wblock->isranged)
      return false;

    radium::ScopedUndo scoped_undo;
    
    struct Tracks *track = block->tracks;
    
    while (track != NULL) {
      
      if (gt.in_range) {
        
        if (track->l.num < wblock->rangex1)
          goto next;
        
        if (track->l.num > wblock->rangex2)
          break;
      }

      make_undo(track);
      
      if (general_transform_list2(gt, get_T(block, track), transformer))
        ret = true;
      
    next:
      track = NextTrack(track);
    }
      
  }
    
    
  if(ret)
    root->song->tracker_windows->must_redraw = true;
  else
    UNDO_CANCEL_LAST_UNDO();

  return ret;
}


static void general_transpose(radium::GeneralTranspose gt){
  struct Tracker_Windows *window=root->song->tracker_windows;
  struct WBlocks *wblock=window->wblock;
  struct Blocks *block=wblock->block;

  switch(window->curr_track){
      
    case SWINGTRACK:
        
      if(general_transform_list<Swing>(wblock, NULL, gt,
                                       [](auto *block, auto *track){return block->swings;},
                                       TransposeSwing,
                                       [window](auto *track){ADD_UNDO(Swings_CurrPos(window, NULL));}))
        TIME_block_swings_have_changed(block);
        
      break;
        
    case SIGNATURETRACK:
      PC_Pause();{
        if(general_transform_list<Signatures>(wblock, NULL, gt,
                                              [](auto *block, auto *track){return block->signatures;},
                                              TransposeSignature,
                                              [window](auto *track){ADD_UNDO(Signatures_CurrPos(window));}))
          TIME_block_signatures_have_changed(block); // 
      }PC_StopPause(NULL);
      
      break;
        
    case LPBTRACK:
      PC_Pause();{
        if(general_transform_list<LPBs>(wblock, NULL, gt,
                                        [](auto *block, auto *track){return block->lpbs;},
                                        TransposeLPB,
                                        [window](auto *track){ADD_UNDO(LPBs_CurrPos(window));}))
          TIME_block_LPBs_have_changed(block);
          
      }PC_StopPause(NULL);
      break;
        
    case TEMPOTRACK:
      if(general_transform_list<Tempos>(wblock, NULL, gt,
                                        [](auto *block, auto *track){return block->tempos;},
                                        TransposeBPM,
                                        [window](auto *track){ADD_UNDO(Tempos_CurrPos(window));}))
        TIME_block_tempos_have_changed(block);
      break;
        
    case TEMPONODETRACK:
      if(general_transform_list<TempoNodes>(wblock, NULL, gt,
                                            [](auto *block, auto *track){return block->temponodes;},
                                            [wblock](auto *temponode, auto &gt){return TransposeTemponode(wblock, temponode, gt);},
                                            [window](auto *track){ADD_UNDO(TempoNodes_CurrPos(window));}))
        TIME_block_tempos_have_changed(block);
      break;
        
    default:

      R_ASSERT(window->curr_track >= 0);

      struct WTracks *wtrack = wblock->wtrack;
      //struct Tracks *track = wtrack->track;
      struct FXs *fxs;
        
      if (SWINGTEXT_subsubtrack(window, wtrack) >= 0){
          
        if (general_transform_list<Swing>(wblock, wtrack, gt,
                                          [](auto *block, auto *track){return track->swings;},
                                          TransposeSwing,
                                          [window](auto *track){ADD_UNDO(Swings_CurrPos(window, track));}))
          TIME_block_swings_have_changed(block);
          
      } else if (FXTEXT_subsubtrack(window, wtrack, &fxs) >= 0){      
          
        general_transform_list<FXNodeLines>(wblock, wtrack, gt,
                                            [fxs](auto *block, auto *track){return fxs->fxnodelines;},
                                            [fxs, wblock](auto *node, auto &gt){return TransposeFxNode(fxs->fx, node, gt);},
                                            [window, wblock](auto *track){ADD_UNDO(FXs(window, wblock->block, track, wblock->curr_realline));});
          
      } else if (CHANCETEXT_subsubtrack(window, wtrack) >= 0){      
          
        general_transform_list<Pitches2>(wblock, wtrack, gt,
                                         [](auto *block, auto *track){return get_pitches2_from_track(track);},
                                         TransposeChance,
                                         [window, wblock](auto *track){ADD_UNDO(Notes(window, wblock->block, track, wblock->curr_realline));});
          
      } else if (CENTTEXT_subsubtrack(window, wtrack) >= 0){      
          
        general_transform_list<Pitches2>(wblock, wtrack, gt,
                                         [](auto *block, auto *track){return get_pitches2_from_track(track);},
                                         TransposeCent,
                                         [window, wblock](auto *track){ADD_UNDO(Notes(window, wblock->block, track, wblock->curr_realline));});
          
      } else if (VELTEXT_subsubtrack(window, wtrack) >= 0){      
          
        general_transform_list<Pitches2>(wblock, wtrack, gt,
                                         [](auto *block, auto *track){return get_pitches2_from_track(track);},
                                         TransposeVelocity,
                                         [window, wblock](auto *track){ADD_UNDO(Notes(window, wblock->block, track, wblock->curr_realline));});
          
      } else {
          
        general_transform_list<Pitches2>(wblock, wtrack, gt,
                                         [](auto *block, auto *track){return get_pitches2_from_track(track);},
                                         TransposePitch,
                                         [window, wblock](auto *track){ADD_UNDO(Notes(window, wblock->block, track, wblock->curr_realline));});
          
      }
    
      break;
  }
}

void generalTransposeEntryDown(bool big_step){
  general_transpose(radium::GeneralTranspose(false, false, true, true, big_step));
}
                        
void generalTransposeEntryUp(bool big_step){
  general_transpose(radium::GeneralTranspose(false, false, true, false, big_step));
}
                        
void generalTransposeTrackDown(bool big_step){
  general_transpose(radium::GeneralTranspose(false, false, false, true, big_step));
}
                        
void generalTransposeTrackUp(bool big_step){
  general_transpose(radium::GeneralTranspose(false, false, false, false, big_step));
}
                        
void generalTransposeRangeDown(bool big_step){
  general_transpose(radium::GeneralTranspose(false, true, false, true, big_step));
}
                        
void generalTransposeRangeUp(bool big_step){
  general_transpose(radium::GeneralTranspose(false, true, false, false, big_step));
}
                        
void generalTransposeBlockDown(bool big_step){
  general_transpose(radium::GeneralTranspose(true, false, false, true, big_step));
}
                        
void generalTransposeBlockUp(bool big_step){
  general_transpose(radium::GeneralTranspose(true, false, false, false, big_step));
}
                        

extern int g_downscroll;

void generalDelete(bool scroll_down, int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;

  int downscroll = g_downscroll;
  if (!scroll_down)
    g_downscroll = 0;

  switch(window->curr_track){
  case SWINGTRACK:
    RemoveSwingCurrPos(window);
    break;
  case SIGNATURETRACK:
    RemoveSignaturesCurrPos(window);
    break;
  case LPBTRACK:
    RemoveLPBsCurrPos(window);
    break;
  case TEMPOTRACK:
    RemoveTemposCurrPos(window);
    break;
  case TEMPONODETRACK:
    RemoveAllTempoNodesOnReallineCurrPos(window);
    break;
  default:
    if(window->curr_track_sub>=0) StopVelocityCurrPos(window,0);
    else RemoveNoteCurrPos(window);
  }

  g_downscroll = downscroll;
}

void insertLines(int toinsert,int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  
  Ratio lz_ratio;

  {
    dyn_t lz = getLineZoomBlockRatio(window->wblocks->l.num, windownum);
    
    if (lz.type==INT_TYPE)
      lz_ratio = make_ratio(lz.int_number, 1);
    
    else if (lz.type==RATIO_TYPE)
      lz_ratio = *lz.ratio;
    
    else {
      R_ASSERT(false);
      return;
    }
  }

  Ratio toinsert_ratio = make_ratio(toinsert,1) / lz_ratio;
  InsertLines_CurrPos(window,toinsert_ratio);
}

void generalReturn(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;

  switch(window->curr_track){
  case SIGNATURETRACK:
    SetSignatureCurrPos(window);
    break;
  case LPBTRACK:
    SetLPBCurrPos(window);
    break;
  case TEMPOTRACK:
    SetTempoCurrPos(window);
    break;
  case TEMPONODETRACK:
    AddTempoNodeCurrPos(window,(float) -0.0f);
    break;
  default:
    printf("curr_track: %d, sub_track: %d\n",window->curr_track,window->curr_track_sub);
    if(window->curr_track>=0){
      if (window->curr_track_sub>=0)
        AddVelocityCurrPos(window);
      else if (window->curr_track_sub==-1)
        EditNoteCurrPos(window);
    }
    break;
  }  
}

int appendBlock(void){
  struct Tracker_Windows *window=getWindowFromNum(-1);
  return AppendWBlock(window)->l.num;
}

void appendTrack(int blocknum){
  struct Tracker_Windows *window=getWindowFromNum(-1);
  struct WBlocks *wblock = getWBlockFromNum(-1, blocknum);if(wblock==NULL) return;
  AppendWTrack_CurrPos(window,wblock);
}

void swapTracks(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  SwapTrack_CurrPos(window);
}

void makeTrackMonophonic(int tracknum, int blocknum, int windownum){
  struct WTracks *wtrack = getWTrackFromNum(windownum, blocknum,tracknum);
  if(wtrack==NULL)
    return;
  
  TRACK_make_monophonic_destructively(wtrack->track);
}

void splitTrackIntoMonophonicTracks(int tracknum, int blocknum, int windownum){
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

  if (TRACK_split_into_monophonic_tracks(window, wblock, wtrack)==false)
    GFX_Message2(NULL, true, "Track #%d is already monophonic", tracknum);
}
  
void splitBlock(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  BLOCK_Split_CurrPos(window);
}


// Warning, must be called via python (does not update graphics or handle undo/redo)
void setNumTracks(int numtracks, int blocknum, int windownum){
  struct Tracker_Windows *window=NULL;
  struct WBlocks *wblock = getWBlockFromNumA(
                                             windownum,
                                             &window,
                                             blocknum
                                             );
  if(wblock==NULL) return;

  {
    radium::PlayerPause player_pause;
    Block_Set_num_tracks(wblock->block, numtracks);
  }
  
  //MinimizeBlock_CurrPos(window);
  wblock->block->is_dirty = true;
}


// Warning, must be called via python (does not update graphics or handle undo/redo)
void setNumLines(int numlines, int blocknum, int windownum){
  struct Tracker_Windows *window=NULL;
  struct WBlocks *wblock = getWBlockFromNumA(
                                             windownum,
                                             &window,
                                             blocknum
                                             );
  if(wblock==NULL) return;

  Block_Set_num_lines(wblock->block, numlines);
  wblock->block->is_dirty = true;
}

void changeTrackNoteLength(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack=getWTrackFromNumA(
                                           windownum,
                                           &window,
                                           blocknum,
                                           &wblock,
                                           tracknum
                                           );
  if(wtrack==NULL) return;

  SetNoteLength(window,wtrack,wtrack->notelength=wtrack->notelength==3?2:3);
  window->must_redraw = true;
}

void setTrackNoteLength(int notelength, int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack=getWTrackFromNumA(
                                           windownum,
                                           &window,
                                           blocknum,
                                           &wblock,
                                           tracknum
                                           );
  if(wtrack==NULL) return;

  if (notelength!=2 && notelength!=3){
    handleError("setTrackNoteLength: notelength must be 2 or 3. (%d)", notelength);
    return;
  }

  SetNoteLength(window,wtrack,notelength);
  window->must_redraw = true;
}

int getTrackNoteLength(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack=getWTrackFromNumA(
                                           windownum,
                                           &window,
                                           blocknum,
                                           &wblock,
                                           tracknum
                                           );
  if(wtrack==NULL) return 3;

  return wtrack->notelength;
}

void changeBlockNoteLength(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  ChangeNoteLength_Block_CurrPos(window);
}

void changeTrackNoteAreaWidth(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack=getWTrackFromNumA(
                                           windownum,
                                           &window,
                                           blocknum,
                                           &wblock,
                                           tracknum
                                           );
  if(wtrack==NULL) return;

  if (wtrack->is_wide){
    wtrack->fxwidth = wtrack->non_wide_fx_width;
  } else {
    wtrack->non_wide_fx_width = wtrack->fxwidth;
    wtrack->fxwidth *= 2;
    if (wtrack->fxwidth < 100)
      wtrack->fxwidth = 100;
  }
  
  wtrack->is_wide = !wtrack->is_wide;
  
    
  window->must_redraw = true;
}

bool trackNoteAreaWidthIsWide(int tracknum, int blocknum, int windownum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack=getWTrackFromNumA(
                                           windownum,
                                           &window,
                                           blocknum,
                                           &wblock,
                                           tracknum
                                           );
  if(wtrack==NULL) return false;

  return wtrack->is_wide;
    
  window->must_redraw = true;
}

void setTrackNoteAreaWidth(bool is_wide, int tracknum, int blocknum, int windownum){
  if (is_wide != trackNoteAreaWidthIsWide(tracknum, blocknum, windownum))
    changeTrackNoteAreaWidth(tracknum, blocknum, windownum);
}

void changeBlockNoteAreaWidth(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  ChangeNoteAreaWidth_Block_CurrPos(window);
}

void minimizeTrack(int windownum, int blocknum, int tracknum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack=getWTrackFromNumA(
                                           windownum,
                                           &window,
                                           blocknum,
                                           &wblock,
                                           tracknum
                                           );
  if(wtrack==NULL) return;
  MinimizeTrack_CurrPos(window, wblock, wtrack);
}

void minimizeBlockTracks(int windownum, int blocknum){
  struct Tracker_Windows *window=NULL;
  struct WBlocks *wblock;

  wblock=getWBlockFromNumA(
                           windownum,
                           &window,
                           blocknum
                           );

  if(wblock==NULL) return;
  
  MinimizeBlock_CurrPos(window, wblock);
}

extern bool doquit;

void quit(bool ignore_nsm){
  
  if (!ignore_nsm && nsmIsActive()){

    if(Undo_are_you_sure_questionmark()==false)
      return;

    evalScheme("(FROM_C-nsm-quit)");
    return;
  }

  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return;
  doquit=Quit(window);
  if(doquit==true) printf("doquit is really true.\n");
}

void saveSoundfile(void){
  SOUNDFILESAVERGUI_open();
}

void openCommentDialog(void){
  COMMENTDIALOG_open();
}

void openSongPropertiesDialog(void){
  SONGPROPERTIES_open();
}

void openPreferencesDialog(void){
  PREFERENCES_open();
}

void openMIDIPreferencesDialog(void){
  PREFERENCES_open_MIDI();
}

void openSequencerPreferencesDialog(void){
  PREFERENCES_open_sequencer();
}

void openToolsDialog(void){
  TOOLS_open();
}

void openPluginManager(void){
  evalScheme("(pmg-start (ra:create-new-instrument-conf) (lambda (descr) (create-instrument (ra:create-new-instrument-conf) descr)))");
}

void openMidiLearnPreferencesDialog(void){
  MIDILEARN_PREFS_open();
}

void openAboutWindow(void){
  float length = getSongLength();
  int minutes = length / 60;
  int seconds = length - (minutes*60);
  int s2      = (length - floorf(length)) * 100.0f;
  
  double vblank = GL_get_vblank();
  
  GFX_addMessage(
              "<center><b>Radium " RADIUM_VERSION "</b></center>"
              "<p>"
              "OpenGL vendor: \"%s\"<br>"
              "OpenGL renderer: \"%s\"<br>"
              "OpenGL version: \"%s\"<br>"
              "OpenGL flags: %x<br>"
              "Qt version: \"%s\"<br>"
              "C/C++ compiler version: " __VERSION__ "<br>"
              "S7 version: " S7_VERSION " / " S7_DATE
              "<p>"
              "<A href=\"http://users.notam02.no/~kjetism/radium/development.php\">Credits</A>"
              "<p>"
              "Jack samplerate: %d<br>"
              "Monitor refresh rate: %s<br>"
              "Control port: %d"
              "<p>"
              "Song length: %02d : %02d : %02d"
              "<p>"
              "Radium needs more demo songs. If you provide one which is suitable, you will get a free lifetime subscription. More information <A href=\"http://users.notam02.no/~kjetism/radium/songs.php\">here</A>."
              ,
              ATOMIC_GET(GE_vendor_string)==NULL ? "(null)" : ATOMIC_GET(GE_vendor_string),
              ATOMIC_GET(GE_renderer_string)==NULL ? "(null)" : ATOMIC_GET(GE_renderer_string),
              ATOMIC_GET(GE_version_string)==NULL ? "(null)" : ATOMIC_GET(GE_version_string),
              ATOMIC_GET(GE_opengl_version_flags),
              GFX_qVersion(),
              (int)MIXER_get_sample_rate(),
              vblank < 0 ? "Refresh rate not detected" : talloc_format("%.2f", 1000.0 / vblank),
              SCHEME_get_webserver_port(),
              minutes, seconds, s2
              );
}

/*
const_char *getProgramPath(void){
  return (char*)OS_get_program_path();
}

const_char *appendPaths(const_char* path1, const_char* path2){
  return talloc_format("%s%s%s", path1, OS_get_directory_separator(), path2);
}
*/

filepath_t getConfPath(filepath_t filename){
  return OS_get_conf_filename(filename);
}

bool hasConfPath(filepath_t filename){
  return OS_has_conf_filename(filename);
}

filepath_t getKeybindingsConfPath(void){
  return OS_get_keybindings_conf_filename();
}

filepath_t getMenuesConfPath(void){
  return OS_get_menues_conf_filename();
}

static const char *g_embedded_audio_files_path = NULL;
filepath_t getEmbeddedAudioFilesPath(void){
  if (g_embedded_audio_files_path==NULL)
    g_embedded_audio_files_path = SETTINGS_read_string("embedded_audio_files_path", "%home%/.radium/embedded_audio_files");
  
  return make_filepath(g_embedded_audio_files_path);
}

void setEmbeddedAudioFilesPath(filepath_t new_path){
  g_embedded_audio_files_path = STRING_get_chars(new_path.id);
  SETTINGS_write_string("embedded_audio_files_path", g_embedded_audio_files_path);
}

bool save(bool ignore_nsm){

  if (!ignore_nsm && nsmIsActive()){
    evalScheme("(FROM_C-nsm-save)");
    return true;
  }
  
  return Save(root);
}

bool saveAs(filepath_t filename, bool with_embedded_samples, bool ignore_nsm){
  if (!ignore_nsm && nsmIsActive()){

    if(Undo_NSM_are_you_sure_questionmark()==false)
      return false;

    evalScheme("(FROM_C-nsm-save-as)");
    
    return true;
  }
  
  if (with_embedded_samples)
    return SaveWithEmbeddedSamples(filename, root);
  else
    return SaveWithoutEmbeddedSamples(filename, root);
}

bool exportSong(filepath_t filename){
  if (isIllegalFilepath(filename)){
    const wchar_t *song_path = SETTINGS_read_wchars("filerequester_song_path", NULL);    
    const filepath_t wdir = song_path==NULL ? createIllegalFilepath() : make_filepath(song_path);
    filename = GFX_GetSaveFileName(root->song->tracker_windows, NULL, " Select file to export", wdir, "*.rad", NULL, true);

    if(isIllegalFilepath(filename))
      return false;
  }
  
  return Export_Song(filename, root);
}

bool saveWithEmbeddedSamples(bool ignore_nsm){
  if (!ignore_nsm && nsmIsActive()){
    GFX_addMessage("Option not supported under NSM");
    return false;
  }
  
  return SaveWithEmbeddedSamples(createIllegalFilepath(), root);
}

bool saveWithoutEmbeddedSamples(bool ignore_nsm){
  if (!ignore_nsm && nsmIsActive()){
    GFX_addMessage("Option not supported under NSM");
    return false;
  }

  return SaveWithoutEmbeddedSamples(createIllegalFilepath(), root);
}


extern bool isloaded;

bool load(bool ignore_nsm){

  if (!ignore_nsm && nsmIsActive()){
    
    if(Undo_NSM_are_you_sure_questionmark()==false)
      return false;
    
    evalScheme("(FROM_C-nsm-open)");
    
    return true;
  }
  
  if( Load_CurrPos(getWindowFromNum(-1))){
    
    isloaded=true;
    return true;
    
  } else {

    return false;
    
  }
}

bool loadSong(filepath_t filename){
  if( LoadSong_CurrPos(getWindowFromNum(-1),filename)){
    
    isloaded=true;
    return true;
    
  } else {
    
    return false;
    
  }
}

static bool ask_clear_or_import(void){
  if (Undo_num_undos_since_last_save()==0 && hasSession()==false)
    return true;
  
  vector_t v = {};
  int yes = VECTOR_push_back(&v, "Yes");
  VECTOR_push_back(&v, "No");

  bool ret = yes==GFX_Message(&v, "Are you sure? Song will be cleared, and all undo data will be deleted.");

  if (ret)
    resetUndo();

  return ret;
}

bool importSong(filepath_t filename){
  if (ask_clear_or_import()==false)
    return false;

  const filepath_t filename_org = dc.filename;
  
  if (!LoadSong_CurrPos(getWindowFromNum(-1),filename))
    return false;

  dc.filename = filename_org;

  if (isLegalFilepath(dc.filename))
    GFX_SetWindowTitle(getWindowFromNum(-1), talloc_wformat(L"** %S **", dc.filename.id));
  else
    GFX_SetWindowTitle(root->song->tracker_windows, STRING_create("Radium - New song."));

  return true;
}

bool hasSession(void){
  return isLegalFilepath(dc.filename);
}

void newSong(bool ignore_nsm){

  if (!ignore_nsm && nsmIsActive()){

    if(Undo_NSM_are_you_sure_questionmark()==false)
      return;

    evalScheme("(FROM_C-nsm-new-song)");
    return;
  }
  
  NewSong_CurrPos(getWindowFromNum(-1));
}

void clearSong(void){
  if (ask_clear_or_import()==false)
    return;
  
  const filepath_t filename = dc.filename;

  NewSong_CurrPos(getWindowFromNum(-1));
  
  dc.filename = filename;

  if (isLegalFilepath(dc.filename))
    GFX_SetWindowTitle(getWindowFromNum(-1), talloc_wformat(L"** %S **", dc.filename.id));
}

void importMidi(void){
  static bool imported=false;
  if(imported==false){
    PyRun_SimpleString("import import_midi");
    imported=true;
  }
  PyRun_SimpleString("import_midi.import_midi()");
}

void internal_updateAllBlockGraphics(void){
  GL_create_all(root->song->tracker_windows);
}

static void import_importmod_file(void){
  static bool imported=false;
  if(imported==false){
    PyRun_SimpleString("import import_mod");
    imported=true;
  }else
    PyRun_SimpleString("import_mod=reload(import_mod)"); // Avoid having to restart radium if code is changed. Practical during development. No practical impact on performance either.
  
  GL_create_all(root->song->tracker_windows);
}

void requestImportMod(void){
  //import_importmod_file();
  //PyRun_SimpleString("import_mod.import_mod()");
  //SCHEME_eval("(let () (load \"import_mod.scm\" (curlet)) (load-protracker-module))");
  SCHEME_eval("(my-require 'import_mod.scm)");
  SCHEME_eval("(async-load-protracker-module)");
}

void importXM(void){
  import_importmod_file();
  PyRun_SimpleString("import_mod.import_xm()");
  GL_create_all(root->song->tracker_windows);
}

void insertTracks(int numtracks,int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  InsertTracks_CurrPos(window,(NInt)numtracks);
}

static void insertOrDeleteTrack(int tracknum, int blocknum, int windownum, int num_to_insert){
  struct Tracker_Windows *window=NULL;
  struct WBlocks *wblock;

  wblock=getWBlockFromNumA(
                           windownum,
                           &window,
                           blocknum
                           );

  if(wblock==NULL) return;

  if (tracknum==-1)
    tracknum = wblock->wtrack->l.num;

  ADD_UNDO(Block_CurrPos(window));

  InsertTracks(window,
               wblock,
               tracknum,
               num_to_insert
               );

  window->must_redraw = true;
}

void insertTrack(int tracknum, int blocknum, int windownum){
  insertOrDeleteTrack(tracknum, blocknum, windownum, 1);
}

void deleteTracks(int numtracks,int windownum){
  insertTracks(-numtracks,windownum);
}

void deleteTrack(int tracknum, int blocknum, int windownum){
  insertOrDeleteTrack(tracknum, blocknum, windownum, -1);
}

void deleteBlock(int blocknum, int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  DeleteBlock_CurrPos(window, blocknum);
}

int insertBlock(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return -1;
  return InsertBlock_CurrPos(window)->l.num;
}

int getNumTracks(int blocknum){
  struct WBlocks *wblock = getWBlockFromNum(-1, blocknum);
  if(wblock==NULL) return 0;

  return wblock->block->num_tracks;
}

int getNumLines(int blocknum){
  struct WBlocks *wblock = getWBlockFromNum(-1, blocknum);
  if(wblock==NULL) return 0;

  return wblock->block->num_lines;
}

int getNumReallines(int blocknum, int windownum){
  struct WBlocks *wblock = getWBlockFromNum(windownum, blocknum);
  if(wblock==NULL) return 0;

  return wblock->num_reallines;
}

const char *getBlockName(int blocknum){
  struct WBlocks *wblock = getWBlockFromNum(-1, blocknum);
  if(wblock==NULL) return "";

  return wblock->block->name;
}

void setBlockName(const_char* new_name, int blocknum){
  struct WBlocks *wblock = getWBlockFromNum(-1, blocknum);
  if(wblock==NULL) return;
  
  Block_set_name(wblock->block, new_name);
}

int getNumBlocks(void){
  return root->song->num_blocks;
}

void selectBlock(int blocknum, int windownum){
  struct Tracker_Windows *window=NULL;
  struct WBlocks *wblock = getWBlockFromNumA(
                                             windownum,
                                             &window,
                                             blocknum
                                             );
  if(wblock==NULL) return;

  if (window->wblock==wblock)
    return;
  
  PC_PauseNoMessage();{

     // Note: We stop the player to be able to play the block if already playing.
    // We don't stop to avoid race conditions (player don't have to be stopped to avoid race condition for doing any of the things in here).
    
    if(wblock->curr_realline == wblock->num_reallines-1)
      wblock->curr_realline = 0;

    SelectWBlock(window, wblock, false);
  }PC_StopPause_ForcePlayBlock(NULL);
}

void setBlockColor(const_char *colorname, int blocknum, int windownum){
  struct Tracker_Windows *window=NULL;
  struct WBlocks *wblock = getWBlockFromNumA(
                                             windownum,
                                             &window,
                                             blocknum
                                             );
  if(wblock==NULL) return;

  unsigned int color = GFX_get_color_from_colorname(colorname);
  wblock->block->color = color;
  g_editor_blocks_generation++;

  SEQUENCER_update(SEQUPDATE_TIME|SEQUPDATE_PLAYLIST|SEQUPDATE_BLOCKLIST);
}

const char *getBlockColor(int blocknum, int windownum, bool displayed_color){
  struct Tracker_Windows *window=NULL;
  struct WBlocks *wblock = getWBlockFromNumA(
                                             windownum,
                                             &window,
                                             blocknum
                                             );
  if(wblock==NULL) return "";

  if(displayed_color)
    return talloc_strdup(get_displayed_block_color(wblock->block).name(QColor::HexArgb).toUtf8());
  else
    return GFX_get_colorname_from_color(wblock->block->color);
}

const_char* getAudiofileColor(filepath_t w_audiofilename, bool displayed_color){
  if (isIllegalFilepath(w_audiofilename)){
    handleError("getAudiofileColor: illegal sample name for argument");
    return "";
  }

  if(displayed_color)
    return talloc_strdup(get_displayed_audiofile_color(w_audiofilename).name(QColor::HexArgb).toUtf8());
  else
    return GFX_get_colorname_from_color(SAMPLEREADER_get_sample_color(w_audiofilename));
}

void setAudiofileColor(const_char* colorname, filepath_t w_audiofilename){
  if (isIllegalFilepath(w_audiofilename)){
    handleError("setAudiofileColor: illegal sample name for argument 2");
    return;
  }

  unsigned int color = GFX_get_color_from_colorname(colorname);

  SAMPLEREADER_set_sample_color(w_audiofilename, color);

  g_sample_reader_filenames_generation++; // update audio file browser in the right part of sequencer.
  SEQUENCER_update(SEQUPDATE_TIME|SEQUPDATE_PLAYLIST|SEQUPDATE_BLOCKLIST);
}


void showBlocklistGui(void){
  S7CALL2(void_void, "FROM_C-create-blocks-table-gui");
}

void showInstrumentListGui(void){
  S7CALL2(void_void, "FROM_C-create-instruments-table-gui");
}

void setTrackNoteShowType(int type,int tracknum,int blocknum,int windownum){
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

  wtrack->noteshowtype=type;

#if !USE_OPENGL
  if(window->wblock==wblock){
    DrawUpWTrack(window,wblock,wtrack);
  }
#endif
}

void setTrackVolume(float volume,int tracknum,int blocknum,int windownum){
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

  if(volume<=0.0f)
    wtrack->track->volume = 0;
  else if(volume>=1.0f)
    wtrack->track->volume = MAXTRACKPAN;
  else
    wtrack->track->volume = (int)(volume * (float)MAXTRACKVOL);

  window->must_redraw_editor = true;

  EditorWidget *editor = static_cast<EditorWidget*>(window->os_visual.widget);
  editor->header_widget->update(wtrack->x, 0, wtrack->x2 - wtrack->x + 2, editor->header_widget->height());
}

void setTrackPan(float pan,int tracknum,int blocknum,int windownum){
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

  if(pan<=-1.0f)
    wtrack->track->pan = -MAXTRACKPAN;
  else if(pan>=1.0f)
    wtrack->track->pan = MAXTRACKPAN;
  else
    wtrack->track->pan = (int)(pan * (float)MAXTRACKPAN);

  window->must_redraw_editor = true;

  EditorWidget *editor = static_cast<EditorWidget*>(window->os_visual.widget);
  editor->header_widget->update(wtrack->x, 0, wtrack->x2 - wtrack->x + 2, editor->header_widget->height());
}

void switchTrackNoteShowType(int tracknum,int blocknum,int windownum){
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

  wtrack->noteshowtype++;
  if(wtrack->noteshowtype>MAXTYPE) wtrack->noteshowtype=0;

#if !USE_OPENGL
  if(window->wblock==wblock){
    DrawUpWTrack(window,wblock,wtrack);
  }
#endif
}

void setBlockNoteShowType(int type,int blocknum,int windownum){
  struct Tracker_Windows *window=NULL;
  struct WTracks *wtrack;
  struct WBlocks *wblock;

  wblock=getWBlockFromNumA(
	windownum,
	&window,
	blocknum
	);

  if(wblock==NULL) return;

  wtrack=wblock->wtracks;
  while(wtrack!=NULL){
    wtrack->noteshowtype=type;
    wtrack=NextWTrack(wtrack);
  }

#if !USE_OPENGL
  if(window->wblock==wblock){
    DrawUpAllWTracks(window,wblock,NULL);
  }
#endif
}

void switchBlockNoteShowType(int blocknum,int windownum){
  struct Tracker_Windows *window=NULL;
  struct WTracks *wtrack;
  struct WBlocks *wblock;

  wtrack=getWTrackFromNumA(
	windownum,
	&window,
	blocknum,
	&wblock,
	0
	);

  if(wtrack==NULL) return;

  int type = wtrack->noteshowtype+1;
  if(type>MAXTYPE)
    type = 0;

  setBlockNoteShowType(type, blocknum, windownum);
}

// track midi channel
int getTrackMidiChannel(int tracknum, int blocknum, int windownum){
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

  if(wtrack==NULL) return 0;

  return ATOMIC_GET(wtrack->track->midi_channel);
}

void setTrackMidiChannel(int midi_channel, int tracknum, int blocknum, int windownum){
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

  if (midi_channel < 0 || midi_channel > 15){
    handleError("midi_channel must be between 0 and 15. Found %d", midi_channel);
    return;
  }
  

  ADD_UNDO(TrackHeader(wblock->l.num, wtrack->l.num));
  ATOMIC_SET(wtrack->track->midi_channel, midi_channel);
}


// swingtext (subtrack)

void showSwingtext(bool showit, int tracknum, int blocknum, int windownum){
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

  wtrack->swingtext_on = showit;

  UpdateAllWBlockCoordinates(window);
  window->must_redraw = true;
}

bool swingtextVisible(int tracknum,int blocknum,int windownum){
  struct WTracks *wtrack = getWTrackFromNum(-1, blocknum, tracknum);

  if (wtrack==NULL)
    return false;

  return wtrack->swingtext_on;
}

void showHideSwingtext(int tracknum,int blocknum,int windownum){
  showSwingtext(!swingtextVisible(tracknum, blocknum, windownum),
              tracknum, blocknum, windownum);
}

void showHideSwingtextInBlock(int blocknum,int windownum){
  struct Tracker_Windows *window=NULL;
  struct WTracks *wtrack;
  struct WBlocks *wblock;

  wtrack=getWTrackFromNumA(
	windownum,
	&window,
	blocknum,
	&wblock,
	-1
	);

  if(wtrack==NULL) return;

  bool on = !wtrack->swingtext_on;

  wtrack = wblock->wtracks;
  while(wtrack!=NULL){
    wtrack->swingtext_on = on;
    wtrack = NextWTrack(wtrack);
  }
  
  UpdateAllWBlockCoordinates(window);
  window->must_redraw = true;
}

void setSwingEnabled(bool val, int blocknum, int windownum){
  struct Tracker_Windows *window;
  const struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if(wblock==NULL) return;

  wblock->block->swing_enabled = val;
  TIME_block_swings_have_changed(wblock->block);

  window->must_redraw = true;
}

  
bool getSwingEnabled(int blocknum, int windownum){
  const struct WBlocks *wblock = getWBlockFromNum(windownum, blocknum);
  if(wblock==NULL) return false;

  return wblock->block->swing_enabled;
}

bool switchSwingEnabled(int blocknum, int windownum){

  struct Tracker_Windows *window;
  const struct WBlocks *wblock = getWBlockFromNumA(windownum, &window, blocknum);
  if(wblock==NULL) return false;

  bool ret = !wblock->block->swing_enabled;
  
  wblock->block->swing_enabled = ret;

  TIME_block_swings_have_changed(wblock->block);

  window->must_redraw = true;
  
  return ret;
}

  

// centtext

bool centtextCanBeTurnedOff(int tracknum, int blocknum, int windownum){
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

  if(wtrack==NULL) return true;

  if (wtrack->notesonoff==0)
    return true;
  
  int num_cents_subtracks = wtrack->centtext_on ? 2 : 0;

  if (WTRACK_num_non_polyphonic_subtracks(wtrack)==num_cents_subtracks)
    return true;
  
  struct Notes *note = wtrack->track->notes;
  while(note!=NULL){
    if (!equal_floats(note->note, floorf(note->note)))
      return false;
    
    struct Pitches *pitch = note->pitches;
    while(pitch != NULL){
      if (!equal_floats(pitch->note, floorf(pitch->note)))
        return false;
      pitch = NextPitch(pitch);
    }
    
    note = NextNote(note);
  }

  return true;
}

void showCenttext(bool showit, int tracknum, int blocknum, int windownum){
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

  wtrack->centtext_on = showit;

  UpdateAllWBlockCoordinates(window);
  window->must_redraw = true;
}

bool centtextVisible(int tracknum,int blocknum,int windownum){
  struct WTracks *wtrack = getWTrackFromNum(-1, blocknum, tracknum);

  if (wtrack==NULL)
    return false;

  return wtrack->centtext_on;
}

void showHideCenttext(int tracknum,int blocknum,int windownum){
  showCenttext(!centtextVisible(tracknum, blocknum, windownum),
              tracknum, blocknum, windownum);
}

void showHideCenttextInBlock(int blocknum,int windownum){
  struct Tracker_Windows *window=NULL;
  struct WTracks *wtrack;
  struct WBlocks *wblock;

  wtrack=getWTrackFromNumA(
	windownum,
	&window,
	blocknum,
	&wblock,
	-1
	);

  if(wtrack==NULL) return;

  bool on = !wtrack->centtext_on;

  wtrack = wblock->wtracks;
  while(wtrack!=NULL){
    wtrack->centtext_on = on;
    wtrack = NextWTrack(wtrack);
  }
  
  UpdateAllWBlockCoordinates(window);
  window->must_redraw = true;
}

// chancetext

void showChancetext(bool showit, int tracknum, int blocknum, int windownum){
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

  wtrack->chancetext_on = showit;

  UpdateAllWBlockCoordinates(window);
  window->must_redraw = true;
}

bool chancetextVisible(int tracknum,int blocknum,int windownum){
  struct WTracks *wtrack = getWTrackFromNum(-1, blocknum, tracknum);

  if (wtrack==NULL)
    return false;

  return wtrack->chancetext_on;
}

void showHideChancetext(int tracknum,int blocknum,int windownum){
  showChancetext(!chancetextVisible(tracknum, blocknum, windownum),
              tracknum, blocknum, windownum);
}

void showHideChancetextInBlock(int blocknum,int windownum){
  struct Tracker_Windows *window=NULL;
  struct WTracks *wtrack;
  struct WBlocks *wblock;

  wtrack=getWTrackFromNumA(
	windownum,
	&window,
	blocknum,
	&wblock,
	-1
	);

  if(wtrack==NULL) return;

  bool on = !wtrack->chancetext_on;

  wtrack = wblock->wtracks;
  while(wtrack!=NULL){
    wtrack->chancetext_on = on;
    wtrack = NextWTrack(wtrack);
  }
  
  UpdateAllWBlockCoordinates(window);
  window->must_redraw = true;
}

// veltext

void showVeltext(bool showit, int tracknum, int blocknum, int windownum){
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

  wtrack->veltext_on = showit;

  UpdateAllWBlockCoordinates(window);
  window->must_redraw = true;
}

bool veltextVisible(int tracknum,int blocknum,int windownum){
  struct WTracks *wtrack = getWTrackFromNum(-1, blocknum, tracknum);

  if (wtrack==NULL)
    return false;

  return wtrack->veltext_on;
}

void showHideVeltext(int tracknum,int blocknum,int windownum){
  showVeltext(!veltextVisible(tracknum, blocknum, windownum),
              tracknum, blocknum, windownum);
}

void showHideVeltextInBlock(int blocknum,int windownum){
  struct Tracker_Windows *window=NULL;
  struct WTracks *wtrack;
  struct WBlocks *wblock;

  wtrack=getWTrackFromNumA(
	windownum,
	&window,
	blocknum,
	&wblock,
	-1
	);

  if(wtrack==NULL) return;

  bool on = !wtrack->veltext_on;

  wtrack = wblock->wtracks;
  while(wtrack!=NULL){
    wtrack->veltext_on = on;
    wtrack = NextWTrack(wtrack);
  }
  
  UpdateAllWBlockCoordinates(window);
  window->must_redraw = true;
}

void showFxtext(bool showit, int tracknum, int blocknum, int windownum){
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

  wtrack->fxtext_on = showit;

  UpdateAllWBlockCoordinates(window);
  window->must_redraw = true;
}

bool fxtextVisible(int tracknum,int blocknum,int windownum){
  struct WTracks *wtrack = getWTrackFromNum(-1, blocknum, tracknum);

  if (wtrack==NULL)
    return false;

  return wtrack->fxtext_on;
}

void showHideFxtext(int tracknum,int blocknum,int windownum){
  showFxtext(!fxtextVisible(tracknum, blocknum, windownum),
              tracknum, blocknum, windownum);
}

void showHideFxtextInBlock(int blocknum,int windownum){
  struct Tracker_Windows *window=NULL;
  struct WTracks *wtrack;
  struct WBlocks *wblock;

  wtrack=getWTrackFromNumA(
	windownum,
	&window,
	blocknum,
	&wblock,
	-1
	);

  if(wtrack==NULL) return;

  bool on = !wtrack->fxtext_on;

  wtrack = wblock->wtracks;
  while(wtrack!=NULL){
    wtrack->fxtext_on = on;
    wtrack = NextWTrack(wtrack);
  }
  
  UpdateAllWBlockCoordinates(window);
  window->must_redraw = true;
}

void showPianoroll(bool showit, int tracknum,int blocknum,int windownum){
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

  wtrack->pianoroll_on = showit;

  UpdateAllWBlockCoordinates(window);
  window->must_redraw = true;
}

bool pianorollVisible(int tracknum,int blocknum,int windownum){
  struct WTracks *wtrack = getWTrackFromNum(-1, blocknum, tracknum);

  if (wtrack==NULL)
    return false;

  return wtrack->pianoroll_on;
}

void showHidePianoroll(int tracknum,int blocknum,int windownum){
  showPianoroll(!pianorollVisible(tracknum, blocknum, windownum),
                tracknum, blocknum, windownum);
}

void showHidePianorollInBlock(int blocknum,int windownum){
  struct Tracker_Windows *window=NULL;
  struct WTracks *wtrack;
  struct WBlocks *wblock;

  wtrack=getWTrackFromNumA(
	windownum,
	&window,
	blocknum,
	&wblock,
	-1
	);

  if(wtrack==NULL) return;

  bool on = !wtrack->pianoroll_on;

  wtrack = wblock->wtracks;
  while(wtrack!=NULL){
    wtrack->pianoroll_on = on;
    wtrack = NextWTrack(wtrack);
  }
  
  UpdateAllWBlockCoordinates(window);
  window->must_redraw = true;
}

void showNoteTrack(bool showit, int tracknum,int blocknum,int windownum){
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

  if (showit)
    wtrack->notesonoff = 1;
  else
    wtrack->notesonoff = 0;
      
  UpdateAllWBlockCoordinates(window);
  window->must_redraw = true;
}


bool noteTrackVisible(int tracknum,int blocknum,int windownum){
  struct WTracks *wtrack = getWTrackFromNum(-1, blocknum, tracknum);

  if (wtrack==NULL)
    return false;

  return wtrack->notesonoff==1;
}


void showHideNoteTrack(int tracknum,int blocknum,int windownum){
  bool is_visible = noteTrackVisible(tracknum, blocknum, windownum);
  showNoteTrack(!is_visible, tracknum, blocknum, windownum);
}

void showHideNoteTracksInBlock(int blocknum,int windownum){
  struct Tracker_Windows *window=NULL;
  struct WTracks *wtrack;
  struct WBlocks *wblock;

  wtrack=getWTrackFromNumA(
	windownum,
	&window,
	blocknum,
	&wblock,
	-1
	);

  if(wtrack==NULL) return;

  int on;
  if (wtrack->notesonoff==0)
    on = 1;
  else
    on = 0;

  wtrack = wblock->wtracks;
  while(wtrack!=NULL){
    wtrack->notesonoff = on;
    wtrack = NextWTrack(wtrack);
  }
  
  UpdateAllWBlockCoordinates(window);
  window->must_redraw = true;
}

void setSignatureTrackVisible(bool showit, int windownum){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return;

  if (window->show_signature_track == showit)
    return;
  
  window->show_signature_track = showit;

  if (!window->show_signature_track && window->curr_track==SIGNATURETRACK)
    ATOMIC_WRITE(window->curr_track, 0);

  UpdateAllWBlockCoordinates(window);
  window->must_redraw = true;
}

void showHideSignatureTrack(int windownum){
  setSignatureTrackVisible(!signatureTrackVisible(windownum), windownum);
}

// swingtext (global)
void setSwingTrackVisible(bool setit, int windownum){
  
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return;
  
  if (window->show_swing_track == setit)
    return;
  
  window->show_swing_track = setit;
  
  if (!window->show_swing_track && window->curr_track==SWINGTRACK)
    ATOMIC_WRITE(window->curr_track, 0);
  
  UpdateAllWBlockCoordinates(window);
  window->must_redraw = true;
}

void showHideSwingTrack(int windownum){
  setSwingTrackVisible(!swingTrackVisible(windownum), windownum);
}

void setLpbTrackVisible(bool showit, int windownum){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return;
  
  if(showit==window->show_lpb_track)
    return;
  
  window->show_lpb_track = showit;

  if (!window->show_lpb_track && window->curr_track==LPBTRACK)
    ATOMIC_WRITE(window->curr_track, 0);

  UpdateAllWBlockCoordinates(window);
  window->must_redraw = true;
}

void showHideLPBTrack(int windownum){
  setLpbTrackVisible(!lpbTrackVisible(windownum), windownum);
}

bool bpmTrackVisible(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return false;
  return window->show_bpm_track;
}

void setBpmTrackVisible(bool setit, int windownum){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return;

  if (setit==window->show_bpm_track)
    return;
  
  window->show_bpm_track = setit;

  if (!window->show_bpm_track && window->curr_track==TEMPOTRACK)
    ATOMIC_WRITE(window->curr_track, 0);

  UpdateAllWBlockCoordinates(window);
  window->must_redraw = true;
}

void showHideBPMTrack(int windownum){
  setBpmTrackVisible(!bpmTrackVisible(windownum), windownum);
}

bool reltempoTrackVisible(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return false;
  return window->show_reltempo_track;
}

void setReltempoTrackVisible(bool setit, int windownum){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return;

  if (setit==window->show_reltempo_track)
    return;
  
  window->show_reltempo_track = setit;

  if (!window->show_reltempo_track && window->curr_track==TEMPONODETRACK)
    ATOMIC_WRITE(window->curr_track, 0);

  UpdateAllWBlockCoordinates(window);
  window->must_redraw = true;
}

void showHideReltempoTrack(int windownum){
  setReltempoTrackVisible(!reltempoTrackVisible(windownum), windownum);
}

bool swingTrackVisible(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return false;
  return window->show_swing_track;
}

bool signatureTrackVisible(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return false;
  return window->show_signature_track;
}

bool lpbTrackVisible(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return false;
  return window->show_lpb_track;
}


static bool g_show_linenumbers = false;

bool linenumbersVisible(void){
  static bool has_inited = false;

  if (has_inited==false){
    g_show_linenumbers = SETTINGS_read_bool("show_linenumbers", false);
    has_inited = true;
  }

  return g_show_linenumbers;
}

void setLinenumbersVisible(bool doit){
  g_show_linenumbers = doit;
  SETTINGS_write_bool("show_linenumbers", doit);
  root->song->tracker_windows->must_redraw = true;
}

void showHideLinenumbers(void){
  setLinenumbersVisible(!linenumbersVisible());
}

// Various

static bool g_cpu_friendly_audio_meter_updates = false;

bool useCPUFriendlyAudiometerUpdates(void){
  static bool has_inited = false;

  if (has_inited==false){
    g_cpu_friendly_audio_meter_updates = SETTINGS_read_bool("cpu_friendly_audio_meter_updates", g_cpu_friendly_audio_meter_updates);
    has_inited = true;
  }

  return g_cpu_friendly_audio_meter_updates;
}

void setUseCPUFriendlyAudiometerUpdates(bool val){
  g_cpu_friendly_audio_meter_updates = val;
  SETTINGS_write_bool("g_cpu_friendly_audio_meter_updates", val);
}


bool recordAccuratelyFromMidi(void){
  return MIDI_get_record_accurately();
}

void setRecordAccuratelyFromMidi(bool accurately){
  MIDI_set_record_accurately(accurately);
}

bool switchRecordAccuratelyFromMidi(void){
  bool ret = !recordAccuratelyFromMidi();
  setRecordAccuratelyFromMidi(ret);
  return ret;
}


// Disk

static bool g_stop_playing_when_saving_song = false;

bool doStopPlayingWhenSavingSong(void){
  static bool has_inited = false;

  if (has_inited==false){
    g_stop_playing_when_saving_song = SETTINGS_read_bool("stop_playing_when_saving_song", g_stop_playing_when_saving_song);
    has_inited = true;
  }

  return g_stop_playing_when_saving_song;
}

void setStopPlayingWhenSavingSong(bool val){
  g_stop_playing_when_saving_song = val;
  SETTINGS_write_bool("stop_playing_when_saving_song", val);
}


// audio file save folder

static bool g_save_recorded_audio_files_in_browser_path = false;

bool saveRecordedAudioFilesInBrowserPath(void){
  static bool has_inited = false;

  if (has_inited==false){
    g_save_recorded_audio_files_in_browser_path = SETTINGS_read_bool("save_recorded_audio_files_in_browser_path", false);
    has_inited = true;
  }

  return g_save_recorded_audio_files_in_browser_path;
}

void setSaveRecordedAudioFilesInBrowserPath(bool val){
  if (val != g_save_recorded_audio_files_in_browser_path){
    g_save_recorded_audio_files_in_browser_path = val;
    SETTINGS_write_bool("save_recorded_audio_files_in_browser_path", val);
  }
}

  
// autobackup

static bool g_save_backup_while_playing = true;

bool doSaveBackupWhilePlaying(void){
  static bool has_inited = false;

  if (has_inited==false){
    g_save_backup_while_playing = SETTINGS_read_bool("save_backup_while_playing", true);
    has_inited = true;
  }

  return g_save_backup_while_playing;
}

void setSaveBackupWhilePlaying(bool val){
  g_save_backup_while_playing = val;
  SETTINGS_write_bool("save_backup_while_playing", val);
}


static bool g_do_autobackups = false;

bool doAutoBackups(void){
  static bool has_inited = false;

  if (has_inited==false){
    g_do_autobackups = SETTINGS_read_bool("do_auto_backups", true);
    has_inited = true;
  }

  return g_do_autobackups;
}

void setDoAutoBackups(bool doit){
  g_do_autobackups = doit;
  SETTINGS_write_bool("do_auto_backups", doit);
}

static int g_autobackup_interval_minutes = false;

int autobackupIntervalInMinutes(void){
  static bool has_inited = false;

  if (has_inited==false){
    g_autobackup_interval_minutes = SETTINGS_read_int32("autobackup_interval_minutes", 1);
    has_inited = true;
  }

  return g_autobackup_interval_minutes;
}

void setAutobackupIntervalInMinutes(int interval){
  g_autobackup_interval_minutes = interval;
  SETTINGS_write_int("autobackup_interval_minutes", interval);
}


// sequencer recording

static bool g_do_auto_delete_sequencer_recordings = true;

bool doAutoDeleteSequencerRecordings(void){
  static bool has_inited = false;

  if (has_inited==false){
    g_do_auto_delete_sequencer_recordings = SETTINGS_read_bool("auto_delete_sequencer_recordings", g_do_auto_delete_sequencer_recordings);
    has_inited = true;
  }

  return g_do_auto_delete_sequencer_recordings;
}

void setDoAutoDeleteSequencerRecordings(bool doit){
  g_do_auto_delete_sequencer_recordings = doit;
  SETTINGS_write_bool("auto_delete_sequencer_recordings", doit);
}


static int g_unused_recording_takes_treatment = URTT_ASK;

int unusedRecordingTakesTreatment(void){
  static bool has_inited = false;

  if (has_inited==false){
    g_unused_recording_takes_treatment = SETTINGS_read_int32("unused_recording_takes_treatment", g_unused_recording_takes_treatment);
    has_inited = true;
  }

  return g_unused_recording_takes_treatment;
}

void setUnusedRecordingTakesTreatment(int treatment){
  if (treatment < 0 || treatment > 2){
    handleError("setUnusedRecordingTakesTreatment: Treatment must be 0, 1, or 2. Found %d", treatment);
    return;
  }
  g_unused_recording_takes_treatment = treatment;
  SETTINGS_write_int("unused_recording_takes_treatment", treatment);
}



// main menu
void addMenuMenu(const char* name, const_char* command){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return;
  GFX_AddMenuMenu(window, name, command);
}

void addMenuMenu2(const char* name, dynvec_t strings, func_t* callback){
  vector_t vec = VECTOR_create(strings.num_elements);

  for(int i=0;i<strings.num_elements;i++){
    if (strings.elements[i].type != STRING_TYPE){
      handleError("addMenuMenu2: Element #%d is not a string. Found: %s", i, DYN_type_name(strings.elements[i].type));
      return;
    }
    vec.elements[i] = STRING_get_chars(strings.elements[i].string);
  }

  QMenu *menu = GFX_create_qmenu(vec, callback);
  
  GFX_AddMenuMenu(name, menu);
}

void goPreviousMenuLevel(void){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return;
  GFX_GoPreviousMenuLevel(window);
}

void addMenuItem(const char* name, const_char* command){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return;
  GFX_AddMenuItem(window, name, command);
}

void addCheckableMenuItem(const_char* name, const_char* command, int checkval){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return;
  GFX_AddCheckableMenuItem(window, name, command, checkval==1?true:false);
}

void addMenuSeparator(void){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return;
  GFX_AddMenuSeparator(window);
}


int64_t setStatusbarText(const_char* text){
  return GFX_SetStatusBar(text);
}

void removeStatusbarText(int64_t id){
  GFX_RemoveStatusbarText(id);
}

int getWebserverPort(void){
  return SCHEME_get_webserver_port();
}

void eraseEstimatedVblank(void){
#if USE_QT5
  printf("eraseEstimatedVblank() is not used in Qt5\n");
#else
  GL_erase_estimated_vblank();
#endif
}

dyn_t evalSchemeWithReturn(const_char *code){
  return SCHEME_eval_withreturn(code);
}

void evalScheme(const_char *code){
#if !defined(RELEASE)
  QString code2(code);
  code2 = code2.simplified();
  //printf("0 -%s\n", code2.toUtf8().constData());
  
  if(code2.at(0) == QChar('(')){
    //printf("1\n");
    code2.remove(0,1);
    code2 = code2.trimmed();
    int pos_space = code2.indexOf(' ');
    int pos_rp = code2.indexOf(')');
    int pos;
    if (pos_space==-1)
      pos=pos_rp;
    else if (pos_rp==-1)
      pos=pos_space;
    else
      pos=R_MIN(pos_rp, pos_space);

    //printf("2: %d\n", pos);
    
    if (pos > 0){
      code2 = code2.left(pos);
      code2 = code2.trimmed();
      //printf("   CODE2: -%s-\n", code2.toUtf8().constData());
      S7CALL2(void_charpointer,"FROM-C-assert-that-function-can-be-called-from-C",code2.toUtf8().constData());
    }
  }
#endif
  SCHEME_eval(code);
}

static bool g_error_check_eval_scheme = false;

bool errorCheckEvalScheme(void){
#if !defined(RELEASE)
  //return false;
  return true;
#endif
  return g_error_check_eval_scheme;
}

void setErrorCheckEvalScheme(bool error_check){
  g_error_check_eval_scheme = error_check;
}

void evalPython(const_char *code){
  PyRun_SimpleString(code);
}

static dyn_t g_keybindings_from_keys = g_uninitialized_dyn;
static dyn_t g_keybindings_from_command = g_uninitialized_dyn;

void addKeybinding(const_char* command, const_char* keys){ // keys must be sorted

  hash_t *r_keybindings_from_keys;
  hash_t *r_keybindings_from_command;
  
  if (g_keybindings_from_keys.type==UNINITIALIZED_TYPE){

    R_ASSERT(false);
    
    r_keybindings_from_keys = HASH_create(512);
    g_keybindings_from_keys = DYN_create_hash(r_keybindings_from_keys);

    r_keybindings_from_command = HASH_create(512);
    g_keybindings_from_command = DYN_create_hash(r_keybindings_from_command);

  } else {

    r_keybindings_from_keys = g_keybindings_from_keys.hash;
    r_keybindings_from_command = g_keybindings_from_command.hash;
    
  }

  dyn_t dyn_keys = DYN_create_string_from_chars(keys);
  
  // Add to key->command hash table
  {

    // First remove old keybinding->command, if stored.
    if (HASH_has_key(r_keybindings_from_keys, keys)){
      const_char *old_command = HASH_get_chars(r_keybindings_from_keys, keys);
      if (HASH_has_key(r_keybindings_from_command, old_command)){
        dyn_t dyn = HASH_get_dyn(r_keybindings_from_command, old_command);
        DYNVEC_remove_element_and_keep_order(*dyn.array, dyn_keys);
      }
    }

    // One of the few places where we overwrite old element in hash_t
    if (HASH_has_key(r_keybindings_from_keys, keys))
      HASH_remove(r_keybindings_from_keys, keys);
    
    HASH_put_chars(r_keybindings_from_keys, keys, command);
  }

  // Add to command->keys hash table
  {

    if (!HASH_has_key(r_keybindings_from_command, command)){
      
      dynvec_t vec = {};
      HASH_put_dyn(r_keybindings_from_command, command, DYN_create_array(vec));
      
    }

    dyn_t dyn = HASH_get_dyn(r_keybindings_from_command, command);
    DYNVEC_push_back(dyn.array, dyn_keys);

  }
  
}
                   
dyn_t getKeybindingsFromKeys(void){

  if (g_keybindings_from_keys.type!=UNINITIALIZED_TYPE)
    return g_keybindings_from_keys;

  return g_keybindings_from_keys;
}

dyn_t getKeybindingsFromCommands(void){

  if (g_keybindings_from_command.type!=UNINITIALIZED_TYPE)
    return g_keybindings_from_command;

  return g_keybindings_from_command;
}


dynvec_t getKeybindingsFromCommand(const_char *command){

  dynvec_t empty_ret = {};

  if (g_keybindings_from_command.type==UNINITIALIZED_TYPE){
    R_ASSERT(false);
    return empty_ret;
  }

  hash_t *keybindings = getKeybindingsFromCommands().hash;
  
  R_ASSERT_RETURN_IF_FALSE2(g_keybindings_from_command.type==HASH_TYPE, empty_ret);
      
  if (HASH_has_key(keybindings,command))
    return HASH_get_array(keybindings, command);
  else{
    return empty_ret;
  }
}

const_char* getKeybindingFromKeys(const_char *keys){
  hash_t *keybindings = getKeybindingsFromKeys().hash;
  
  if (g_keybindings_from_keys.type==UNINITIALIZED_TYPE){
    R_ASSERT(false);
    return "";
  }

  R_ASSERT_RETURN_IF_FALSE2(g_keybindings_from_keys.type==HASH_TYPE, "");

  if (HASH_has_key(keybindings,keys))
    return HASH_get_chars(keybindings, keys);
  else
    return "";
}

void reloadKeybindings(void){
  bool first_time = g_keybindings_from_command.type==UNINITIALIZED_TYPE;
    
  g_keybindings_from_keys = DYN_create_hash(HASH_create(512));
  g_keybindings_from_command = DYN_create_hash(HASH_create(512));
  
  evalPython("keybindingsparser.parse_and_show_errors()");

  if(!first_time)
    S7CALL2(void_void, "FROM_C-keybindings-have-been-reloaded");
}

void addKeybindingToConfFile(const_char* keybinding, const_char* funcname, dynvec_t arguments){

  const char *command = funcname;
  for(int i=0;i<arguments.num_elements;i++){
    dyn_t argument = arguments.elements[i];
    if (argument.type != STRING_TYPE){
      handleError("setKeybinding: Expected string in argument[%d], found %s", i, DYN_type_name(argument.type));
      return;
    }
    command = talloc_format("%s %S", command, argument.string);
  }

  const char *pythoncommand = talloc_format("keybindings_changer.FROM_C_insert_new_keybinding_into_conf_file(\"%s\", \'%s\')", keybinding, command);
  printf("Evaling -%s\n", pythoncommand);

  static bool has_imported = false;
  if (has_imported==false){
    evalPython("import keybindings_changer");
    has_imported = true;
  }
  
  evalPython(pythoncommand);  
}


void removeKeybindingFromConfFile(const_char* keybinding, const_char* funcname, dynvec_t arguments){

  const char *command = funcname;
  for(int i=0;i<arguments.num_elements;i++){
    dyn_t argument = arguments.elements[i];
    if (argument.type != STRING_TYPE){
      handleError("setKeybinding: Expected string in argument[%d], found %s", i, DYN_type_name(argument.type));
      return;
    }
    command = talloc_format("%s %S", command, argument.string);
  }

  const char *pythoncommand = talloc_format("keybindings_changer.FROM_C_remove_keybinding_from_conf_file(\"%s\", \'%s\')", keybinding, command);
  printf("Evaling -%s\n", pythoncommand);

  static bool has_imported = false;
  if (has_imported==false){
    evalPython("import keybindings_changer");
    has_imported = true;
  }
  
  evalPython(pythoncommand);  
}


#define INCLUDE_KEY_EVENT_NAMES 1
#include "../common/keyboard_sub_ids.h"

static const_char* getKeyName(int keynum){
  if(keynum <= 0 || keynum > EVENT_DASMAX){
    handleError("getKeyName: No key %d", keynum);
    return "";
  }

  return g_key_event_names[keynum];
}


static radium::ProtectedS7Extra<func_t*> g_grab_callback = radium::ProtectedS7Extra<func_t*>("grab_callback");

void API_has_grabbed_keybinding(int key, int *qualifiers, int len_qualifiers){
  R_ASSERT_RETURN_IF_FALSE(g_grab_callback.v != NULL);

  const char *keybinding = getKeyName(key);

  for(int i=0;i<len_qualifiers;i++)
    keybinding = talloc_format("%s %s", keybinding, getKeyName(qualifiers[i]));

  S7CALL(void_charpointer,g_grab_callback.v,keybinding);

  g_grab_callback.set(NULL);
}

void grabKeybinding(func_t *callback){
  if (g_grab_next_eventreceiver_key==true){
    R_ASSERT(g_grab_callback.v != NULL);
    handleError("grabKeybinding: Already grabbing keybinding");
    return;
  }

  R_ASSERT_RETURN_IF_FALSE(g_grab_callback.v == NULL);

  g_grab_callback.set(callback);
  g_grab_next_eventreceiver_key = true;
}

void cancelGrabKeybinding(void){
  g_grab_next_eventreceiver_key = false;
  g_grab_callback.set(NULL);
}

const_char* getQualifierName(const_char *qualifier){
#if FOR_LINUX
  const char *g_left_meta = "Left Meta";
  const char *g_right_meta = "Right Meta";
#elif FOR_WINDOWS
  const char *g_left_meta = "Left Win";
  const char *g_right_meta = "Right Win";
#elif FOR_MACOSX
  const char *g_left_meta = "Left Cmd";
  const char *g_right_meta = "Right Cmd";
#endif

#ifdef C
#  define _RADIUM_OLD_C C
#  undef C
#endif

#define C(a,b) if (!strcmp(a,qualifier)) return b;

    C("CTRL_L","Left Ctrl");
    C("CTRL_R","Right Ctrl");
    C("CTRL","Ctrl");
    
    C("CAPS","Caps Lock");
    
    C("SHIFT_L","Left Shift");
    C("SHIFT_R","Right Shift");
    C("SHIFT","Shift");
    
    C("ALT_L","Left Alt");
#if FOR_MACOSX
    C("ALT_R","Right Alt");
#else
    C("ALT_R","AltGr");
#endif
    C("ALT", "Alt");
    
    C("EXTRA_L", g_left_meta);
    C("EXTRA_R", g_right_meta);

    C("MOUSE_MIXERSTRIPS", "Mouse in mixer strips");
    C("MOUSE_SEQUENCER", "Mouse in sequencer");
    C("MOUSE_MIXER", "Mouse in mixer");
    C("MOUSE_EDITOR", "Mouse in editor");

    C("FOCUS_MIXERSTRIPS", "Mixer strips has keyboard focus");
    C("FOCUS_SEQUENCER", "Sequencer has keyboard focus");
    C("FOCUS_MIXER", "Mixer has keyboard focus");
    C("FOCUS_EDITOR", "Editor has keyboard focus");

#undef C
#ifdef _RADIUM_OLD_C
#  define C _RADIUM_OLD_C
#endif
    
    return "";
}

static int getKeynumFromKeyname(QString key){
  for(int i=0;i<EVENT_DASMAX;i++){
    if (key == QString(g_key_event_names[i]))
      return i;
  }

  return -1;
}

extern struct TEvent tevent;
void sendKeyEvent(const_char *c_keybinding){

  QStringList keybinding = QString(c_keybinding).split(" ");

  if (keybinding.length()==0){
    handleError("sendKeyEvent: keybinding \"%s\" is not valid", c_keybinding);
    return;
  }
  
  int key = getKeynumFromKeyname(keybinding.at(0));
  if (key<0){
    handleError("sendKeyEvent: Unknown key \"%s\"", keybinding.at(0).toUtf8().constData());
    return;
  }

  QStringList qualifiers(keybinding);
  qualifiers.removeFirst();
  qualifiers.sort();
  
  if (qualifiers.contains("UP"))
    tevent.ID=TR_KEYBOARDUP;
  else
    tevent.ID=TR_KEYBOARD;

  uint32_t keyswitch = 0;
  
  for(QString qualifier : qualifiers){
    int num = getKeynumFromKeyname(qualifier);
    if (num < 0){
      handleError("sendKeyEvent: Unknown qualifier \"%s\"", qualifier.toUtf8().constData());
      return;
    }
    keyswitch |= (1 << num);
  }

  uint32_t keyswitch_before = tevent.keyswitch;
  
  tevent.keyswitch=keyswitch;
  tevent.SubID=key;

  EventReciever(&tevent,root->song->tracker_windows);

  tevent.keyswitch = keyswitch_before;
}


bool isFullVersion(void){
  return FULL_VERSION==1;
}

int radiumMajorVersion(void){
  static int val = QString(RADIUM_VERSION).split(".")[0].toInt(); // [NO_STATIC_ARRAY_WARNING]
  return val;
}

int radiumMinorVersion(void){
  static int val = QString(RADIUM_VERSION).split(".")[1].toInt(); // [NO_STATIC_ARRAY_WARNING]
  return val;
}

int radiumRevisionVersion(void){
  static int val = QString(RADIUM_VERSION).split(".")[2].toInt(); // [NO_STATIC_ARRAY_WARNING]
  return val;
}


static bool g_vst_gui_always_on_top = true;

bool vstGuiIsAlwaysOnTop(void){
  static bool has_inited = false;

  if (has_inited==false){
    g_vst_gui_always_on_top = SETTINGS_read_bool("vst_gui_always_on_top", g_vst_gui_always_on_top);
    has_inited = true;
  }

  return g_vst_gui_always_on_top;
}

void setVstGuiAlwaysOnTop(bool doit){
  if (doit != g_vst_gui_always_on_top) {
    g_vst_gui_always_on_top = doit;
    SETTINGS_write_bool("vst_gui_always_on_top", doit);
    PREFERENCES_update();
  }
}



static DEFINE_ATOMIC(bool, g_show_virtual_midi_keyboard_below_native_guis) = true;

// Can be called from the juce message thread as well as the main thread.
bool showVirtualMidiKeyboardBelowNativeGUIs(void){
  static bool has_inited = false;

  if (has_inited==false){
    ATOMIC_SET(g_show_virtual_midi_keyboard_below_native_guis, SETTINGS_read_bool("show_virtual_midi_keyboard_below_native_guis", ATOMIC_GET(g_show_virtual_midi_keyboard_below_native_guis)));
    has_inited = true;
  }

  return ATOMIC_GET(g_show_virtual_midi_keyboard_below_native_guis);
}

void setShowVirtualMidiKeyboardBelowNativeGUIs(bool doit){
  if (doit != ATOMIC_GET(g_show_virtual_midi_keyboard_below_native_guis)) {
    ATOMIC_SET(g_show_virtual_midi_keyboard_below_native_guis, doit);
    SETTINGS_write_bool("show_virtual_midi_keyboard_below_native_guis", doit);
    PREFERENCES_update();
  }
}



static bool g_modal_windows;

bool doModalWindows(void){
  static bool has_inited = false;

  if (has_inited==false){
    g_modal_windows = SETTINGS_read_bool("modal_windows", GL_should_do_modal_windows());
    has_inited = true;
  }

  return g_modal_windows;
}

void setModalWindows(bool doit){
  g_modal_windows = doit;
  SETTINGS_write_bool("modal_windows", doit);
}

//

static DEFINE_ATOMIC(int, g_high_cpu_protection_opengl_protection) = -1;

bool doHighCpuOpenGlProtection(void){  
  int g = ATOMIC_GET(g_high_cpu_protection_opengl_protection);
  
  if (g == -1){
    g = SETTINGS_read_bool("high_cpu_protection_opengl_protection", true) ? 1 : 0;
    ATOMIC_SET(g_high_cpu_protection_opengl_protection, g);
  }

  return g==1 ? true : false;
}

void setHighCpuOpenGlProtection(bool doit){
  ATOMIC_SET(g_high_cpu_protection_opengl_protection, doit ? 1 : 0);
  SETTINGS_write_bool("high_cpu_protection_opengl_protection", doit);
}

//

static DEFINE_ATOMIC(int, g_lock_juce_when_swapping_opengl) = -1;

bool doLockJuceWhenSwappingOpenGL(void){
  int g = ATOMIC_GET(g_lock_juce_when_swapping_opengl);
  
  if (g == -1){
    g = bool_to_int(SETTINGS_read_bool("lock_juce_when_swapping_opengl", false));

    ATOMIC_SET(g_lock_juce_when_swapping_opengl, g);
    if (g==1){
      if (SETTINGS_read_bool("show_lock_juce_when_swapping_opengl_warning", true)) {
        vector_t v = {};
        VECTOR_push_back(&v,"OK");
        int dont_show = VECTOR_push_back(&v,"Don't show this message again.");

        int result = GFX_Message2(&v, true, "Warning: the \"Don't run plugin GUI code when swapping OpenGL\" option is probably not necessary anymore.\n\nIt might also reduce graphical performance.");

        if (result==dont_show)
          SETTINGS_write_bool("show_lock_juce_when_swapping_opengl_warning", false);
      }
    }
  }

  return int_to_bool(g);
}

void setLockJuceWhenSwappingOpenGL(bool doit){
  ATOMIC_SET(g_lock_juce_when_swapping_opengl, bool_to_int(doit));
  SETTINGS_write_bool("lock_juce_when_swapping_opengl", doit);
}



static bool g_native_file_requesters;

bool useNativeFileRequesters(void){
#if FOR_WINDOWS

  return true;  // Workaround. non-native QFileDialog freezes on windows.

#else

  static bool has_inited = false;

  if (has_inited==false){
    bool default_value = false;
    g_native_file_requesters = SETTINGS_read_bool("native_file_requesters", default_value);
    has_inited = true;
  }

  return g_native_file_requesters;
#endif
}

void setUseNativeFileRequesters(bool doit){
  g_native_file_requesters = doit;
  SETTINGS_write_bool("native_file_requesters", doit);
}



static float g_instrument_brightness = 0.531000018119812;

static void update_all_instrument_colors_in_editor(void){
  root->song->tracker_windows->must_redraw_editor=true;
  SEQUENCER_update(SEQUPDATE_TIMING);
}

static void update_all_instrument_colors(void){
  root->song->tracker_windows->must_redraw=true;
  redrawMixerStrips(false);
  MW_update_all_chips();
  SEQUENCER_update(SEQUPDATE_HEADERS);
}

float getInstrumentBrightness(void){
  static bool has_inited = false;

  if (has_inited==false){
    g_instrument_brightness = SETTINGS_read_double("color_instrument_brightness_v2", g_instrument_brightness);
    has_inited = true;
  }

  return g_instrument_brightness;
}

void setInstrumentBrightness(float val){
  if (!equal_floats(val, g_instrument_brightness)){
    g_instrument_brightness = val;
    update_all_instrument_colors();
    SETTINGS_write_double("color_instrument_brightness_v2", val);
  }
}


static float g_instrument_brightness_in_editor = 0.5;

float getInstrumentBrightnessInEditor(void){
  static bool has_inited = false;

  if (has_inited==false){
    g_instrument_brightness_in_editor = SETTINGS_read_double("color_instrument_brightness_in_editor_v2", g_instrument_brightness_in_editor);
    has_inited = true;
  }

  return g_instrument_brightness_in_editor;
}

void setInstrumentBrightnessInEditor(float val){
  if (!equal_floats(val, g_instrument_brightness_in_editor)){
    g_instrument_brightness_in_editor = val;
    update_all_instrument_colors_in_editor();
    SETTINGS_write_double("color_instrument_brightness_in_editor_v2", val);
  }
}


static float g_instrument_saturation = 0.4339999854564667;

float getInstrumentSaturation(void){
  static bool has_inited = false;

  if (has_inited==false){
    g_instrument_saturation = SETTINGS_read_double("color_instrument_saturation_v2", g_instrument_saturation);
    has_inited = true;
  }

  return g_instrument_saturation;
}

void setInstrumentSaturation(float val){
  if (!equal_floats(val, g_instrument_saturation)){
    g_instrument_saturation = val;
    update_all_instrument_colors();
    SETTINGS_write_double("color_instrument_saturation_v2", val);
  }
}


static float g_instrument_saturation_in_editor = 0.363999992609024;

float getInstrumentSaturationInEditor(void){
  static bool has_inited = false;

  if (has_inited==false){
    g_instrument_saturation_in_editor = SETTINGS_read_double("color_instrument_saturation_in_editor_v2", g_instrument_saturation_in_editor);
    has_inited = true;
  }

  return g_instrument_saturation_in_editor;
}

void setInstrumentSaturationInEditor(float val){
  if (!equal_floats(val, g_instrument_saturation_in_editor)){
    g_instrument_saturation_in_editor = val;
    update_all_instrument_colors_in_editor();
    SETTINGS_write_double("color_instrument_saturation_in_editor_v2", val);
  }
}


static float g_block_brightness = 0.5;

static void update_all_block_colors(void){
  SEQUENCER_update(SEQUPDATE_EVERYTHING);
}

float getBlockBrightness(void){
  static bool has_inited = false;

  if (has_inited==false){
    g_block_brightness = SETTINGS_read_double("color_block_brightness_v2", g_block_brightness);
    has_inited = true;
  }

  return g_block_brightness;
}

void setBlockBrightness(float val){
  if (!equal_floats(val, g_block_brightness)){
    g_block_brightness = val;
    update_all_block_colors();
    SETTINGS_write_double("color_block_brightness_v2", val);
  }
}


static float g_block_saturation = 1.0;

float getBlockSaturation(void){
  static bool has_inited = false;

  if (has_inited==false){
    g_block_saturation = SETTINGS_read_double("color_block_saturation_v2", g_block_saturation);
    has_inited = true;
  }

  return g_block_saturation;
}

void setBlockSaturation(float val){
  if (!equal_floats(val, g_block_saturation)){
    g_block_saturation = val;
    update_all_block_colors();
    SETTINGS_write_double("color_block_saturation_v2", val);
  }
}



void printMixerTree(void){
  SP_print_tree();
}

void testCrashreporter(void){
  //R_ASSERT(false);
  //return;
#if !defined(RELEASE)
  abort(); // The crash below usually doesn't work in non-release mode since we usually compile with fsanitize=address
#endif
  int *ai=NULL;
  ai[0] = 50;
}

extern bool g_test_crashreporter_in_audio_thread;
void testCrashreporterInAudioThread(void){
  g_test_crashreporter_in_audio_thread = true;
}

void testErrorMessage(void){
  SYSTEM_show_error_message("Error message seems to work");
}

void startAutotestingMode(void){
  g_user_interaction_enabled = false;
}

void stopAutotestingMode(void){
  g_user_interaction_enabled = true;
}

bool isInAutotestingMode(void){
  return g_user_interaction_enabled==false;
}

int64_t getGcMemoryUsage(void){
  return GC_get_heap_size();
}

void disableSchemeHistory(void){
  s7extra_disable_history();
}
void enableSchemeHistory(void){
  s7extra_enable_history();
}


// FAUST

static int g_faust_optimization_level;

int getFaustOptimizationLevel(void){
  static bool has_inited = false;

  if (has_inited==false){
    int default_value = 4;
    g_faust_optimization_level = SETTINGS_read_int32("faust_optimization_level", default_value);
    has_inited = true;
  }

  return g_faust_optimization_level;
}

void setFaustOptimizationLevel(int level){
  g_faust_optimization_level = level;
  SETTINGS_write_int("faust_optimization_level", level);
}


static const char *g_faust_gui_style;

const char *getFaustGuiStyle(void){
  static bool has_inited = false;

  if (has_inited==false){
    const char *default_value = "Blue";
    g_faust_gui_style = SETTINGS_read_string("faust_gui_style", default_value);
    has_inited = true;
  }

  return g_faust_gui_style;
}

void setFaustGuiStyle(const char *style){
  g_faust_gui_style = talloc_strdup(style);
  SETTINGS_write_string("faust_gui_style", style);
  FAUST_change_qtguistyle(style);
}


// PLAYLIST

/*
void setPlaylistLength(int len){
  BL_setLength(len);
}

void setPlaylistBlock(int pos, int blocknum){
  struct Blocks *block = getBlockFromNum(blocknum);
  if (block==NULL)
    return;

  BL_setBlock(pos, block);
}
*/

/*
static double get_block_length(struct Blocks *block){
  double time = getBlockSTimeLength(block);

  time /= ATOMIC_DOUBLE_GET(block->reltempo);

  return time / (double)MIXER_get_sample_rate();
}
*/

int64_t getBlockLength(int blocknum, int windownum){
  const struct WBlocks *wblock = getWBlockFromNum(windownum, blocknum);
  if(wblock==NULL) return 1; // return 1.0 instead of 0.0 to avoid divide by zero errors.

  return getBlockSTimeLength(wblock->block); //get_block_length(wblock->block);
}

double getSongLength(void){
  return SONG_get_length();
}

int64_t getSongLengthInFrames(void){
  return SONG_get_length() * MIXER_get_sample_rate();
}

int getSampleRate(void){
  return MIXER_get_sample_rate();
}

int64_t g_editor_blocks_generation = 0;

// This number increases every time a block is added or removed, tracks are added or removed, block is renamed, or block duration changes.
int64_t getEditorBlocksGeneration(void){
  return g_editor_blocks_generation;
}

int getLogtypeHold(void){
  return LOGTYPE_HOLD;
}

int getLogtypeLinear(void){
  return LOGTYPE_LINEAR;
}

Place getCursorPlace(int blocknum, int windownum){
  struct WBlocks *wblock = getWBlockFromNum(windownum, blocknum);if(wblock==NULL) return p_Create(0,0,1);

  return wblock->reallines[wblock->curr_realline]->l.p;
}

int getCursorTrack(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  if (window==NULL)
    return 0;
  return window->curr_track;
}

int getHighestLegalPlaceDenominator(void){
  return MAX_UINT32;
}

int getNumeratorFromRatioString(const_char* s){
  return STATIC_RATIO_from_string(STRING_create(s)).numerator;
}

int getDenominatorFromRatioString(const_char* s){
  return STATIC_RATIO_from_string(STRING_create(s)).denominator;
}

dyn_t getRatioFromString(const_char* s){
  return DYN_create_ratio(make_ratio_from_static_ratio(STATIC_RATIO_from_string(STRING_create(s))));
}

const_char* getStringFromRatio(dyn_t ratio){
  if (!DYN_is_liberal_ratio(ratio)){
    handleError("getStringFromRatio: Expected number or string. Found %s", DYN_type_name(ratio.type));
    return "";
  }

  return STRING_get_chars(STATIC_RATIO_as_string(DYN_get_static_ratio(ratio)));
}


const_char *toBase64(const char *s){
  return STRING_get_chars(STRING_toBase64(STRING_create(s)));
}

const_char *fromBase64(const char *s){
  return STRING_get_chars(STRING_fromBase64(STRING_create(s)));
}

const_char *appendBase64Strings(const_char* w1, const_char* w2){
  return qstring_to_w(w_to_qstring(w1) + w_to_qstring(w2));
}

int getStringLength(const_char* s){
  return QString(s).length();
}

const_char *createUuid(void){
  return talloc_strdup(QUuid::createUuid().toString().toUtf8().constData()); 
}

void msleep(int ms){
  ASSERT_NON_RT_NON_RELEASE();

#if defined(FOR_LINUX)
  
  // Not sure, but QThread::msleep may use a lot of CPU on linux since QTimer uses a lot of CPU on linux (think buzy-waits to be more accurate even when using the default "coarse timer". (The "very coarse timer" doesn't work at all on linux.)).
  if (ms < 1000)
    usleep(1000*ms);
  else
    QThread::msleep(ms);
  
#else
  
  QThread::msleep(ms);
  
#endif
  //usleep(1000*ms); // usleep only works in the range 0->1000000
}

double getMs(void){
  return TIME_get_ms();
}

int64_t getPid(void){
  return getpid();
}

extern const char *g_argv0;
const_char* getArgv0(void){
  return g_argv0;
}

bool releaseMode(void){
#if defined(RELEASE)
  return true;
#else
  return false;
#endif
}

bool optimizedBuild(void){
#ifdef __OPTIMIZE__
  return true;
#else
  return false;
#endif
}


const_char *getOsName(void){
#if defined(FOR_LINUX)
  return "linux";
#elif defined(FOR_MACOSX)
  return "macosx";
#elif defined(FOR_WINDOWS)
  return "windows";
#else
#error "unknown"
#endif
}

const_char* getProgramLog(void){
  return EVENTLOG_get();
}

void addToProgramLog(const_char* text){
  EVENTLOG_add_event(talloc_strdup(text));
}

void copyWtextToClipboard(const_char* wtext){
  QGuiApplication::clipboard()->setText(w_to_qstring(wtext));
}
const_char* getClipboardWtext(void){
  return qstring_to_w(QGuiApplication::clipboard()->text());
}

void copyTextToClipboard(const_char* text){
  QGuiApplication::clipboard()->setText(text);
}
const_char* getClipboardText(void){
  return talloc_strdup(QGuiApplication::clipboard()->text().toUtf8().constData());
}

void copyFilepathToClipboard(filepath_t filepath){
  if (isIllegalFilepath(filepath)){
    handleError("copyFilePathToClipboard: illegal filepath");
    return;
  }

  QGuiApplication::clipboard()->setText(STRING_get_qstring(filepath.id));
}

filepath_t getClipboardFilepath(void){
  return make_filepath(QGuiApplication::clipboard()->text());
}

// Scheduler
////////////////////////////

#define MAX_SCHEDULED_CALLBACKS 8192

#define STDFUNC 0


namespace{
#if STDFUNC
  static std::function<void(int,bool)> g_empty_callback3;
#endif
  
  struct ScheduledEvent{
    ScheduledEvent *next = NULL;

    double priority = 0.0;
    radium::ProtectedS7Extra<func_t*> _callback = radium::ProtectedS7Extra<func_t*>("ra:schedule");
#if STDFUNC
    std::function<void(void)> _callback3;
#endif
    bool stop_me = false;
    
    ScheduledEvent()
    {
    }

#if STDFUNC
    void call_before_usage(double daspriority, func_t *callback, std::function<void(void)> callback3){
#else
      void call_before_usage(double daspriority, func_t *callback){
#endif
      R_ASSERT(_callback.v==NULL);
      priority = daspriority;
      _callback.set(callback);
#if STDFUNC
      _callback3 = callback3;
#endif
    }

    void call_after_usage(void){
      _callback.set(NULL);
#if STDFUNC
      _callback3 = g_empty_callback3;
#endif
    }
  };
}

static ScheduledEvent *g_unused_events = NULL;
static radium::PriorityQueue<ScheduledEvent> g_scheduled_events(MAX_SCHEDULED_CALLBACKS);

static void release_event(ScheduledEvent *event){
  R_ASSERT(event->next==NULL);

  event->call_after_usage();
  event->next = g_unused_events;
  g_unused_events = event;
}

static void schedule(ScheduledEvent *event){
#if !defined(RELEASE)
  static int largest_size = 0;
  if (g_scheduled_events.size() > largest_size){
    largest_size = g_scheduled_events.size();
    printf("   Soft scheduler: Num used events: %d\n", g_scheduled_events.size());
  }
#endif
  
  if (g_scheduled_events.add(event)==false){
    handleError("Can not schedule event. Queue is full.");
    release_event(event);
  }
}

static ScheduledEvent *get_free_event(void){
  ScheduledEvent *event;

  if (g_unused_events!=NULL){
    event = g_unused_events;
    g_unused_events = event->next;
    event->next = NULL;
  } else
    event = new ScheduledEvent();

  return event;
}

#if STDFUNC
static void schedule_internal(double ms, func_t *callback, std::function<void(void)> callback3)
#else
static void schedule_internal(double ms, func_t *callback)
#endif
{
  ScheduledEvent *event = get_free_event();

  double priority = TIME_get_ms() + ms;

#if STDFUNC
  event->call_before_usage(priority, callback, g_empty_callback3);
#else
  event->call_before_usage(priority, callback);
#endif
  
  schedule(event);  
}

#if STDFUNC
void API_schedule(double ms, std::function<void(void)> callback3){
  schedule_internal(ms, NULL, callback3);
}
#endif

void schedule(double ms, func_t *callback){
#if STDFUNC
  schedule_internal(ms, callback, g_empty_callback3);
#else
  schedule_internal(ms, callback);
#endif  
}

void removeSchedule(func_t *callback){
  for(auto *event : g_scheduled_events){
    if (event->_callback.v==callback){
      event->call_after_usage();
      event->stop_me = true;
      return;
    }
  }
  handleError("removeSchedule: Callback not found");
}

// Called every 5 ms.
void API_call_very_often(void){
  double time = TIME_get_ms();

  while(true){
    ScheduledEvent *event = g_scheduled_events.get_first_event();
    if (event==NULL)
      break;

    if (event->priority > time)
      break;

    g_scheduled_events.remove_first_event();

    if (event->stop_me==true){
      event->stop_me = false;
      release_event(event);
      break;
    }

    bool has_new_ms = false;
    double new_ms = 0;
    
    if (event->_callback.v != NULL) {
      
      dyn_t ret = S7CALL(dyn_void, event->_callback.v);
      if (DYN_is_number(ret)){
        new_ms = DYN_get_double_from_number(ret);
        has_new_ms = true;
      } else {
#if 0 //!defined(RELEASE)
        if(ret.type != DynType::BOOL_TYPE)
          abort();
        if(ret.bool_number != false)
          abort();
#endif        
      }
      
#if STDFUNC      
    } else if(event->_callback3 != NULL){
      
      event->_callback3();
#endif
      
    } else {
      
      R_ASSERT(false);
    }
    
    if (has_new_ms){
      event->priority = TIME_get_ms() + new_ms;
      schedule(event);
    } else {
      release_event(event);
    }

    // Should not do this. Although it's not supposed to happen, We MIGHT have been called from scheme here if inside a custom Qt exec() call.
    // (In addition, I don't think it's necessary to call this function to have the correct scheme history printed anyway)
    //    throwExceptionIfError();
  }
}

#undef STDFUNC



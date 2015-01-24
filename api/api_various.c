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


#include "Python.h"
#include "radium_proc.h"

#include "../common/nsmtracker.h"
#include "../common/list_proc.h"
#include "../common/velocities_proc.h"
#include "../common/tempos_proc.h"
#include "../common/LPB_proc.h"
#include "../common/temponodes_proc.h"
#include "../common/fxlines_proc.h"
#include "../common/notes_proc.h"
#include "../common/pitches_proc.h"
#include "../common/wblocks_proc.h"
#include "../common/disk_save_proc.h"
#include "../common/disk_load_proc.h"
#include "../common/lines_proc.h"
#include "../common/reallines_insert_proc.h"
#include "../common/block_properties_proc.h"
#include "../common/track_insert_proc.h"
#include "../common/wtracks_proc.h"
#include "../common/block_insert_proc.h"
#include "../common/block_delete_proc.h"
#include "../common/block_split_proc.h"
#include "../common/eventreciever_proc.h"
#include "../common/visual_proc.h"
#include "../common/OS_settings_proc.h"
#include "../embedded_scheme/scheme_proc.h"
#include "../OpenGL/Widget_proc.h"

#ifdef _AMIGA
#include "Amiga_colors_proc.h"
#endif

#include "api_common_proc.h"

extern struct Root *root;


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
void helpWindowToFront(void){
	GFX_HelpWindowToFront();
}

void maximizeEditorWindow(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  GFX_MaximizeEditorWindow(window);
}
void minimizeEditorWindow(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  GFX_MinimizeEditorWindow(window);
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

void showHideMixerWidget(void){
  GFX_showHideMixerWidget();
}

void showHideInstrumentWidget(int windownum){
  //struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  //GFX_showHideInstrumentWidget(window);
  if(GFX_InstrumentWindowIsVisible())
    GFX_InstrumentWindowToBack();
  else
    GFX_InstrumentWindowToFront();
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
  // From 1 to 2
  printf("editor: %d\ninstrument: %d\nmixer: %d\n",GFX_EditorIsVisible(), GFX_InstrumentWindowIsVisible(), GFX_MixerIsVisible());

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

void insertReallines(int toinsert,int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  InsertRealLines_CurrPos(window,toinsert);
}

void generalDelete(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  switch(window->curr_track){
  case TEMPOTRACK:
    RemoveTemposCurrPos(window);
    break;
  case LPBTRACK:
    RemoveLPBsCurrPos(window);
    break;
  case TEMPONODETRACK:
    RemoveAllTempoNodesOnReallineCurrPos(window);
    break;
  default:
    if(window->curr_track_sub>=0) StopVelocityCurrPos(window,0);
    else RemoveNoteCurrPos(window);
  }
}

void insertLines(int toinsert,int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  InsertLines_CurrPos(window,toinsert);
}

void generalReturn(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;

  switch(window->curr_track){
  case TEMPOTRACK:
    SetTempoCurrPos(window);
    break;
  case LPBTRACK:
    SetLPBCurrPos(window);
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

void appendBlock(void){
  struct Tracker_Windows *window=getWindowFromNum(-1);
  AppendWBlock(window);
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

  Block_Set_num_tracks(wblock->block, numtracks);
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

void changeTrackNoteLength(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  ChangeNoteLength_CurrPos(window);
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

  wtrack->is_wide = !wtrack->is_wide;
  window->must_redraw = true;
}

void changeBlockNoteAreaWidth(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  ChangeNoteAreaWidth_Block_CurrPos(window);
}

void minimizeTrack(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  MinimizeTrack_CurrPos(window);
}

void minimizeBlockTracks(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  MinimizeBlock_CurrPos(window);
}

extern bool doquit;

void quit(void){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return;
  doquit=Quit(window);
  if(doquit==true) printf("doquit is really true.\n");
}

extern void SOUNDFILESAVERGUI_open(void);
void saveSoundfile(void){
  SOUNDFILESAVERGUI_open();
}

extern void COMMENTDIALOG_open(void);
void openCommentDialog(void){
  COMMENTDIALOG_open();
}

char *getProgramPath(void){
  return (char*)OS_get_program_path();
}

char *getConfPath(char *filename){
  return (char*)OS_get_conf_filename2(filename);
}

char *getKeybindingsConfPath(void){
  return (char*)OS_get_keybindings_conf_filename2();
}

char *getMenuesConfPath(void){
  return (char*)OS_get_menues_conf_filename2();
}

void saveAs(void){
  SaveAs(root);
}


void save(void){
  Save(root);
}

extern bool isloaded;

void load(void){
  if( Load_CurrPos(getWindowFromNum(-1))){
    isloaded=true;
  }
}

void loadSong(char *filename){
  if( LoadSong_CurrPos(getWindowFromNum(-1),filename)){
    isloaded=true;
  }
}

void newSong(void){
  NewSong_CurrPos(getWindowFromNum(-1));
}

void importMidi(void){
  static bool imported=false;
  if(imported==false){
    PyRun_SimpleString("import import_midi");
    imported=true;
  }
  PyRun_SimpleString("import_midi.import_midi()");
}

void insertTracks(int numtracks,int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  InsertTracks_CurrPos(window,(NInt)numtracks);
}

void deleteTracks(int numtracks,int windownum){
  insertTracks(-numtracks,windownum);
}

void deleteBlock(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  DeleteBlock_CurrPos(window);
}

void insertBlock(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  InsertBlock_CurrPos(window);
}

int getNumTracks(int blocknum){
  struct WBlocks *wblock = getWBlockFromNum(-1, blocknum);
  if(wblock==NULL) return 0;

  return wblock->block->num_tracks;
}

int getNumBlocks(void){
  return root->song->num_blocks;
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

  wblock->block->is_dirty = true;
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

  wblock->block->is_dirty = true;
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

void showHideLPBTrack(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return;

  window->show_lpb_track = !window->show_lpb_track;

  if (!window->show_lpb_track && window->curr_track==LPBTRACK)
    window->curr_track = 0;

  UpdateAllWBlockCoordinates(window);
}

void showHideBPMTrack(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return;

  window->show_bpm_track = !window->show_bpm_track;

  if (!window->show_bpm_track && window->curr_track==TEMPOTRACK)
    window->curr_track = 0;

  UpdateAllWBlockCoordinates(window);
}

void showHideReltempoTrack(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return;

  window->show_reltempo_track = !window->show_reltempo_track;

  if (!window->show_reltempo_track && window->curr_track==TEMPONODETRACK)
    window->curr_track = 0;

  UpdateAllWBlockCoordinates(window);
}

bool lpbTrackVisible(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return false;
  return window->show_lpb_track;
}

bool bpmTrackVisible(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return false;
  return window->show_bpm_track;
}

bool reltempoTrackVisible(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return false;
  return window->show_reltempo_track;
}


void addMenuMenu(char* name, char* command){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return;
  GFX_AddMenuMenu(window, name, command);
}

void goPreviousMenuLevel(void){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return;
  GFX_GoPreviousMenuLevel(window);
}

void addMenuItem(char* name, char* command){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return;
  GFX_AddMenuItem(window, name, command);
}

void addCheckableMenuItem(char* name, char* command, int checkval){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return;
  GFX_AddCheckableMenuItem(window, name, command, checkval==1?true:false);
}

void addMenuSeparator(void){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return;
  GFX_AddMenuSeparator(window);
}

void setStatusbarText(char* text, int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  static bool is_empty = false;
  if (text[0]=='\0') {
    if (!is_empty) {
      GFX_SetStatusBar(window,text);
      is_empty = true;
    }
    
  } else {

    GFX_SetStatusBar(window,text);
    is_empty = false;
    
  }
  
}

int getWebserverPort(void){
  return SCHEME_get_webserver_port();
}

void eraseEstimatedVblank(void){
  GL_erase_estimated_vblank();
}

void evalScheme(char *code){
  SCHEME_eval(code);
}

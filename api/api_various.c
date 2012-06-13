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
#include "../common/wblocks_proc.h"
#include "../common/disk_save_proc.h"
#include "../common/disk_load_proc.h"
#include "../common/lines_proc.h"
#include "../common/reallines_insert_proc.h"
#include "../common/block_properties_proc.h"
#include "../common/track_insert_proc.h"
#include "../common/wtracks_proc.h"
#include "../common/window_config_proc.h"
#include "../common/block_insert_proc.h"
#include "../common/block_delete_proc.h"
#include "../common/block_split_proc.h"
#include "../common/eventreciever_proc.h"
#include "../common/gfx_wtracks_proc.h"
#include "../common/visual_proc.h"

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

void addFX(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  AddFXNodeLineCurrPos(window);
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
    if(window->curr_track>=0 && window->curr_track_sub>=0) AddVelocityCurrPos(window);
    break;
  }  
}

void appendBlock(void){
  AppendWBlock(getWindowFromNum(-1));
}

void swapTracks(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  SwapTrack_CurrPos(window);
}

void splitBlock(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  BLOCK_Split_CurrPos(window);
}


void configColors(int windownum){
#ifdef _AMIGA
  Amiga_ConfigColors();
#else
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  GFX_ConfigColors(window);
#endif
}

void configWindow(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  Window_config(window);
}

void configBlock(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  Block_Properties_CurrPos(window);
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
  wblock->is_dirty = true;
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
  wblock->is_dirty = true;
}

void changeTrackNoteLength(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  ChangeNoteLength_CurrPos(window);
}

void changeBlockNoteLength(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  ChangeNoteLength_Block_CurrPos(window);
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

  if(window->wblock==wblock){
    DrawUpWTrack(window,wblock,wtrack);
  }
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

  if(window->wblock==wblock){
    DrawUpWTrack(window,wblock,wtrack);
  }
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

  if(window->wblock==wblock){
    DrawUpAllWTracks(window,wblock);
  }
  
}



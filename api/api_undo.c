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

#include "../common/nsmtracker.h"
#include "../common/undo.h"
#include "../common/undo_blocks_proc.h"
#include "../common/undo_tracks_proc.h"

#include "api_common_proc.h"



void setMaxUndos(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  SetMaxUndos(window);
}

void redo(void){
  Redo();
}

void undo(void){
  Undo();
}

void resetUndo(void){
  ResetUndo();
}

void startIgnoringUndo(void){
  Undo_start_ignoring_undo_operations();
}

void stopIgnoringUndo(void){
  Undo_stop_ignoring_undo_operations();
}

void addUndoBlock(void){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return;
  Undo_Block_CurrPos(window);
}

void addUndoTrack(void){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return;
  Undo_Track_CurrPos(window);
}

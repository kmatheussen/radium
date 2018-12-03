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

#include <QSet>

#include "../common/nsmtracker.h"
#include "../common/vector_proc.h"
#include "../common/undo.h"
#include "../common/undo_blocks_proc.h"
#include "../common/undo_tracks_proc.h"

#include "../embedded_scheme/s7extra_proc.h"

#include "api_common_proc.h"

#include "api_undo_proc.h"



void setMaxUndos(int windownum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);if(window==NULL) return;
  SetMaxUndos(window);
}

const_char *getUndoHistory(void){
  const char *ret = "\n";
  vector_t history = Undo_get_history();
  VECTOR_FOR_EACH(const char *, line, &history){
    ret = talloc_format("%s%s\n", ret, line);
  }END_VECTOR_FOR_EACH;

  return ret;
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

void cancelLastUndo(void){
  if (Undo_num_undos()==0){
    handleError("cancelLastUndo: No undos");
    return;
  }
  UNDO_CANCEL_LAST_UNDO();
}

void addUndoBlock(int blocknum){
  if (blocknum<0 || blocknum>=root->song->num_blocks){
    handleError("addUndoBlock: There is no block #%d", blocknum);
    return;
  }
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return;
  ADD_UNDO(Block2(blocknum));
}

void addUndoTrack(void){
  struct Tracker_Windows *window=getWindowFromNum(-1);if(window==NULL) return;
  ADD_UNDO(Track_CurrPos(window->wblock->l.num, window->wblock->wtrack->l.num));
}

bool askAreYouSureSongHasChanged(void){
  return Undo_are_you_sure_questionmark();
}

void openUndo(void){
  UNDO_OPEN_REC();
}

void closeUndo(void){
  UNDO_CLOSE();
}

static radium::ProtectedS7FuncVector g_undo_callbacks(true);

static int64_t g_undoredo_generation = 0;

void addUndoRedoCallback(func_t* callback){

  g_undoredo_generation++;
  
  if(g_undo_callbacks.push_back(callback)==false)
    handleError("addUndoRedoCallback: Callback is already registered");
}

void removeUndoRedoCallback(func_t* callback){

  g_undoredo_generation++;

  int num_removed = g_undo_callbacks.removeAll(callback);
  
  if (num_removed == 0)
    handleError("removeUndoRedoCallback: Callback not found.");
}

void API_call_me_right_after_undoing_or_redoing(void){
  QVector<func_t*> to_remove; // Need to do it like this since the callback could both remove and add undoredo callbacks.

  g_undo_callbacks.safe_for_all(true, [&to_remove](func_t *callback){
      
      if (S7CALL(bool_void, callback)==false)
        to_remove.push_back(callback);
      
      return true;
      
    });
  
  // Don't want to call removeUndoRedoCallback for the entries since a callback could have removed a previous callback, and we don't want to show error if that happens.
  for(func_t *callback : to_remove)
    g_undo_callbacks.removeAll(callback);
}

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

#ifndef RADIUM_COMMON_UNDO_H
#define RADIUM_COMMON_UNDO_H

#define UNDOPREFIX U_N_D_O_

#define CALL_ADD_UNDO_FUNC(call) \
  UNDOPREFIX ## call;
  
#define ADD_UNDO(call)                                                  \
  do{                                                                   \
    if (Undo_Currently_Adding_Undo())                                   \
      RError("Can not add undo while adding undo (something is very spaghetti)"); \
    else if (Undo_Is_Currently_Undoing()==false){                       \
      Undo_Start_Adding_Undo(LOC());                                    \
      CALL_ADD_UNDO_FUNC(call);                                         \
      Undo_End_Adding_Undo();                                           \
    }                                                                   \
  }while(0)

#define ADD_UNDO_FUNC(sign)                     \
  UNDOPREFIX ## sign

typedef void *(*UndoFunction)(
                              struct Tracker_Windows *window,
                              struct WBlocks *wblock,
                              struct WTracks *wtrack,
                              int realline,
                              void *pointer
                              );

extern LANGSPEC int Undo_num_undos_since_last_save(void);
extern LANGSPEC void Undo_saved_song(void);
extern LANGSPEC int Undo_num_undos(void);

extern LANGSPEC void Undo_Open_rec(void); // Allow creating new level, even when level already is > 0.
extern LANGSPEC void Undo_Open(void);
extern LANGSPEC bool Undo_Close(void);
extern LANGSPEC bool Undo_Is_Currently_Undoing(void);
extern LANGSPEC bool Undo_Is_Open(void);
extern LANGSPEC void Undo_CancelLastUndo(void);
extern LANGSPEC void Undo_ReopenLast(void);
extern LANGSPEC UndoFunction Undo_get_last_function(void);

extern LANGSPEC bool Undo_Currently_Adding_Undo(void);
extern LANGSPEC void Undo_Start_Adding_Undo(source_pos_t source_pos);
extern LANGSPEC void Undo_End_Adding_Undo(void);


extern LANGSPEC vector_t Undo_get_history(void);

extern LANGSPEC void Undo_Add(
                              int windownum,
                              int blocknum,
                              int tracknum,
                              int realline,
                              void *pointer,
                              UndoFunction undo_function,
                              const char *info
                              );

extern LANGSPEC void Undo_Add_dont_stop_playing(
                                                int windownum,
                                                int blocknum,
                                                int tracknum,
                                                int realline,
                                                void *pointer,
                                                UndoFunction undo_function,
                                                const char *info
                                                );

extern LANGSPEC void Undo_start_ignoring_undo_operations(void);
extern LANGSPEC void Undo_stop_ignoring_undo_operations(void);
extern LANGSPEC bool Undo_Is_Currently_Ignoring(void);


#if 0
extern void Undo_New(
	NInt windownum,
	NInt blocknum,
	NInt tracknum,
	int realline,
	void *pointer,
        UndoFunction undo_function
);
#endif

extern LANGSPEC void ResetUndo(void);
extern LANGSPEC bool Undo_are_you_sure_questionmark(void);
extern LANGSPEC void Undo(void);
extern LANGSPEC void Redo(void);
extern LANGSPEC void SetMaxUndos(struct Tracker_Windows *window);

extern int64_t g_curr_undo_generation;


#ifdef __cplusplus

namespace radium{
  
  struct ScopedUndo{
  
    ScopedUndo(){
      Undo_Open_rec();
    }
    
    ~ScopedUndo(){
      Undo_Close();
    }
  };

}

#endif

#endif

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
  UNDOPREFIX ## call
  
#define ADD_UNDO(call)                                                  \
  do{                                                                   \
    if (Undo_Currently_Adding_Undo())                                   \
      RError("Can not add undo while adding undo (something is very spaghetti)"); \
    else if (Undo_Is_Currently_Undoing()==false && Undo_Is_Currently_Ignoring()==false){ \
      Undo_Start_Adding_Undo(LOC());                                    \
      CALL_ADD_UNDO_FUNC(call);                                         \
      Undo_End_Adding_Undo();                                           \
    }                                                                   \
  }while(0)

#define ADD_UNDO_FUNC(sign)                       UNDOPREFIX ## sign

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

extern LANGSPEC void Das_Undo_Open_rec(void); // Allow creating new level, even when level already is > 0.
extern LANGSPEC void Das_Undo_Open(void); // Don't call directly
extern LANGSPEC bool Das_Undo_Close(void); // Don't call directly

#define UNDO_OPEN()                                                     \
  do{                                                                   \
    EVENTLOG_add_event(CR_FORMATEVENT("Undo: Open"));                          \
    Das_Undo_Open();                                                    \
  }while(0)

#define UNDO_OPEN_REC()                                                     \
  do{                                                                   \
    EVENTLOG_add_event(CR_FORMATEVENT("Undo: Open Rec"));                          \
    Das_Undo_Open_rec();                                                \
  }while(0)

#define UNDO_CLOSE()                                                     \
  (EVENTLOG_add_event(CR_FORMATEVENT("Undo: Close")),                          \
   Das_Undo_Close())                                                        \

#undef TOSTRING
#undef STRINGIFY

extern LANGSPEC bool Undo_Is_Currently_Undoing(void);
extern LANGSPEC bool Undo_Is_Open(void);
extern LANGSPEC void Das_Undo_CancelLastUndo(void); // don't call directly
extern LANGSPEC void Das_Undo_ReopenLast(void); // don't call directly
extern LANGSPEC UndoFunction Undo_get_last_function(void);

#define UNDO_CANCEL_LAST_UNDO()                                                     \
  do{                                                                   \
    EVENTLOG_add_event(CR_FORMATEVENT("Undo: Cancel last"));                          \
    Das_Undo_CancelLastUndo();                                                    \
  }while(0)

#if 1
// Used in Qt/Qt_faust_plugin_widget_callbacks.h. Fix: destroys redo data (should be fixed by replacing CurrUndo->next=NULL with CurrUndo->next=CurrUndo->last_next).
#define UNDO_REOPEN_LAST()                                                     \
  do{                                                                   \
    EVENTLOG_add_event(CR_FORMATEVENT("Undo: Reopen last"));                          \
    Das_Undo_ReopenLast();                                                \
  }while(0)
#endif

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
extern LANGSPEC bool Undo_NSM_are_you_sure_questionmark(void);
extern LANGSPEC bool Undo_are_you_sure_questionmark(void);
extern LANGSPEC void Undo(void);
extern LANGSPEC void Redo(void);
extern LANGSPEC bool CanRedo(void);
extern LANGSPEC void SetMaxUndos(struct Tracker_Windows *window);

typedef void (*UndoUnavailableCallback)(void*);

extern LANGSPEC void UNDO_add_callback_when_curr_entry_becomes_unavailable(UndoUnavailableCallback callback, void *data, int delay); // if delay is 0, callback will be called immediately. if delay is 1, callback will be called one undo addition later.

extern int64_t g_curr_undo_generation;

extern LANGSPEC void ADD_UNDO_FUNC(Dummy(void));
  


#ifdef __cplusplus

namespace radium{
  
  struct ScopedUndo{
    bool doit;
  
    ScopedUndo(bool doit = true)
      : doit(doit)
    {
      if(doit)
        UNDO_OPEN_REC();
    }
    
    ~ScopedUndo(){
      if(doit)
        UNDO_CLOSE();
    }
  };

  struct ScopedIgnoreUndo{
    bool doit;

    ScopedIgnoreUndo(bool doit = true)
      : doit(doit)
    {
      if (doit)
        Undo_start_ignoring_undo_operations();
    }
    
    ~ScopedIgnoreUndo(){
      if (doit)
        Undo_stop_ignoring_undo_operations();
    }
  };

  struct UndoOnlyIfNeeded{
    bool has_made_undo = false;
    
    bool should_I_make_undo_questionmark(void){
      if (has_made_undo==false){
        has_made_undo = true;
        return true;
      } else {
        return false;
      }
    }

    UndoOnlyIfNeeded(){}
    
    UndoOnlyIfNeeded(const UndoOnlyIfNeeded&) = delete;
    UndoOnlyIfNeeded& operator=(const UndoOnlyIfNeeded&) = delete;
  };

}

void UNDO_functions(const char *name, std::function<void(void)> redo, std::function<void(void)> undo);

#endif

#endif

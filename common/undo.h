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

extern LANGSPEC void Undo_Open(void);
extern LANGSPEC bool Undo_Close(void);
extern LANGSPEC bool Undo_Is_Open(void);
extern LANGSPEC void Undo_CancelLastUndo(void);
extern LANGSPEC UndoFunction Undo_get_last_function(void);

extern LANGSPEC void Undo_Add(
                              int windownum,
                              int blocknum,
                              int tracknum,
                              int realline,
                              void *pointer,
                              UndoFunction undo_function);

extern LANGSPEC void Undo_Add_dont_stop_playing(
                                                int windownum,
                                                int blocknum,
                                                int tracknum,
                                                int realline,
                                                void *pointer,
                                                UndoFunction undo_function
                                                );

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
extern LANGSPEC bool Undo_are_you_shure_questionmark(void);
extern LANGSPEC void Undo(void);
extern LANGSPEC void Redo(void);
extern LANGSPEC void SetMaxUndos(struct Tracker_Windows *window);


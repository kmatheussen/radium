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

#ifndef RADIUM_COMMON_NOTES_PROC_H
#define RADIUM_COMMON_NOTES_PROC_H

extern LANGSPEC int GetNoteSubtrack(const struct WTracks *wtrack, struct Notes *note);
extern LANGSPEC int GetNumSubtracks(const struct WTracks *wtrack);

#ifdef __cplusplus
extern void StopAllNotesAtPlace(
                                struct Blocks *block,
                                struct Tracks *track,
                                const Place *placement
                                );

extern const r::NotePtr GetCurrNote(struct Tracker_Windows *window);

#endif

//extern LANGSPEC void NOTE_init(struct Notes *note);
extern LANGSPEC struct Notes *NewNote(void);
extern LANGSPEC struct Notes *CopyNote(const struct Notes *old_note);

#if __cplusplus
extern r::NotePtr NewNote2(const Ratio &time,
                    float notenum,
                    int velocity);

// Helper function while transitioning from "Notes" to "r::Note".
r::NotePtr NewNoteFromOldNote(const struct Notes *note);
#endif

extern LANGSPEC bool NOTES_sorted_by_pitch_questionmark(struct Notes *notes);
extern LANGSPEC struct Notes *NOTES_sort_by_pitch(struct Notes *notes);

extern LANGSPEC void NOTE_validate(const struct Blocks *block, struct Tracks *track, struct Notes *note);

extern LANGSPEC struct Notes *InsertGfxNote(struct WBlocks *wblock,
                                            struct WTracks *wtrack,
                                            const Place *placement, 
                                            const Place *end_placement,
                                            float notenum,
                                            int velocity
                                            );

extern LANGSPEC struct Notes *InsertNote(
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	const Place *placement,
        const Place *end_placement,
	float notenum,
	int velocity,
	bool polyphonic
);

extern LANGSPEC int NOTE_get_velocity(struct Tracks *track);

// returns true if it may have been scroll-playing
extern LANGSPEC bool InsertNoteCurrPos(struct Tracker_Windows *window,float notenum,bool polyphonic, float velocity);

extern LANGSPEC void LengthenNotesTo(
                     struct Blocks *block,
                     struct Tracks *track,
                     const Place *placement
                     );
extern LANGSPEC void ReplaceNoteEnds(
                    struct Blocks *block,
                    struct Tracks *track,
                    const Place *old_placement,
                    const Place *new_placement,
                    int subtrack
                    );

#if __cplusplus
void ReplaceNoteEnds2(
                      struct Blocks *block,
                      struct Tracks *track,
                      r::NoteTimeData::Writer &writer,
                      const Ratio &old_ratio,
                      const Ratio &new_ratio,
                      int polyphony_num
                      );
#endif

extern LANGSPEC void CutNoteAt(const struct Blocks *block, const struct Tracks *track,struct Notes *note, const Place *place);
  
extern LANGSPEC void RemoveNote(struct Blocks *block,
                struct Tracks *track,
                const struct Notes *note
                );
#if __cplusplus
void RemoveNote2(
                 struct Blocks *block,
                 struct Tracks *track,
                 const r::NotePtr &note
                 );
#endif

extern LANGSPEC void RemoveNoteCurrPos(struct Tracker_Windows *window);

extern LANGSPEC struct Notes *FindPrevNoteOnSameSubTrack(const struct Tracks *track, const struct Notes *note);

#if __cplusplus
r::NotePtr FindPrevNoteOnSameSubTrack2(const radium::Vector<r::NotePtr> &notes, const r::NotePtr &note);
r::NotePtr FindNextNoteOnSameSubtrack2(const radium::Vector<r::NotePtr> &notes, const r::NotePtr &note);
#endif

extern LANGSPEC struct Notes *FindNoteOnSubTrack(
                                        const struct WTracks *wtrack,
                                        int subtrack,
                                        const Place *placement
);

#if __cplusplus
r::NotePtr FindNoteOnSubTrack2(const struct WTracks *wtrack,
                               int subtrack,
                               const Ratio &ratio
                               );
#endif

extern LANGSPEC struct Notes *FindNextNoteOnSameSubtrack(struct Notes *note);

extern LANGSPEC struct Notes *FindNextNote(
                                           struct Tracks *track,
                                           const Place *placement
                                           );

extern LANGSPEC struct Notes *FindNote(
                       struct Tracks *track,
                       const Place *placement
                       );
extern LANGSPEC vector_t FindAllNotes(
                                      struct Tracks *track,
                                      const Place *placement
                                      );

extern LANGSPEC struct Notes *FindNoteCurrPos(struct Tracker_Windows *window);
extern LANGSPEC vector_t FindAllNotesCurrPos(struct Tracker_Windows *window);

extern LANGSPEC const char *notetext_from_notenum(float notenumf);
extern LANGSPEC float notenum_from_notetext(const char *notetext);

extern LANGSPEC void EditNoteCurrPos(struct Tracker_Windows *window);

extern LANGSPEC void StopVelocityCurrPos(struct Tracker_Windows *window,int noend);

#if __cplusplus
#include "placement_proc.h"
#include "ratio_funcs.h"
static inline bool note_continues_next_block(const struct Blocks *block, const struct Notes *note){
  return note->noend==1 && note->end >= place2ratio(p_Last_Pos(block));
}
static inline bool note_continues_next_block2(const struct Blocks *block, const r::Note *note){
  return note->d._noend==1 && note->d._end >= place2ratio(p_Last_Pos(block));
}
#endif

#if __cplusplus
void RT_notes_called_each_block(struct SeqTrack *seqtrack,
                                const int play_id,
                                const struct SeqBlock *seqblock,
                                const struct Tracks *track,
                                const int64_t seqtime_start,
                                const int64_t seqtime_end,
                                const r::RatioPeriod &period
                                );
#endif

  
#endif

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


extern LANGSPEC void SetNotePolyphonyAttributes(struct Tracks *track);
extern LANGSPEC int GetNoteSubtrack(const struct WTracks *wtrack, struct Notes *note);
extern LANGSPEC int GetNumSubtracks(const struct WTracks *wtrack);

struct Notes *GetCurrNote(struct Tracker_Windows *window);

#define NOTE_ID_RESOLUTION 256 // i.e. 256 id's per note.
static inline int64_t NotenumId(float notenum){
  int64_t n = notenum*NOTE_ID_RESOLUTION;
  return n*NUM_PATCH_VOICES;
}

void NOTE_init(struct Notes *note);
struct Notes *NewNote(void);

bool NOTES_sorted_by_pitch_questionmark(struct Notes *notes);
struct Notes *NOTES_sort_by_pitch(struct Notes *notes);

extern LANGSPEC void NOTE_validate(const struct Blocks *block, struct Tracks *track, struct Notes *note);

extern struct Notes *InsertNote(
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	Place *placement,
        Place *end_placement,
	float notenum,
	int velocity,
	bool polyphonic
);

int NOTE_get_velocity(struct Tracks *track);

extern void InsertNoteCurrPos(struct Tracker_Windows *window,float notenum,bool polyphonic, float velocity);

void LengthenNotesTo(
                     struct Blocks *block,
                     struct Tracks *track,
                     Place *placement
                     );
void ReplaceNoteEnds(
                    struct Blocks *block,
                    struct Tracks *track,
                    Place *old_placement,
                    Place *new_placement,
                    int subtrack
                    );

void CutNoteAt(struct Blocks *block, struct Tracks *track,struct Notes *note, Place *place);
  
void RemoveNote(struct Blocks *block,
                struct Tracks *track,
                struct Notes *note
                );

extern void RemoveNoteCurrPos(struct Tracker_Windows *window);

extern struct Notes *FindPrevNoteOnSameSubTrack(struct Tracks *track, struct Notes *note);

extern struct Notes *FindNoteOnSubTrack(
                                        const struct WTracks *wtrack,
                                        int subtrack,
                                        Place *placement
);

struct Notes *FindNextNoteOnSameSubtrack(struct Notes *note);

struct Notes *FindNote(
                       struct Tracks *track,
                       Place *placement
                       );

struct Notes *FindNoteCurrPos(struct Tracker_Windows *window);

char *notetext_from_notenum(float notenumf);
float notenum_from_notetext(char *notetext);

void EditNoteCurrPos(struct Tracker_Windows *window);

extern void StopVelocityCurrPos(struct Tracker_Windows *window,int noend);

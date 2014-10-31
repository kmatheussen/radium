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


#ifndef TRACKER_INCLUDE

#include "nsmtracker.h"

struct Notes *GetCurrNote(struct Tracker_Windows *window);

#define NOTE_ID_RESOLUTION 256 // i.e. 256 id's per note.
static inline int64_t NotenumId(float notenum){
  int64_t n = notenum*NOTE_ID_RESOLUTION;
  return n*NUM_PATCH_VOICES;
}

struct Notes *NewNote(void);

extern struct Notes *InsertNote(
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	Place *placement,
        Place *end_placement,
	float notenum,
	int velocity,
	int override
);

int NOTE_get_velocity(struct Tracks *track);

extern void InsertNoteCurrPos(struct Tracker_Windows *window,int notenum,int override);

void LengthenNotesTo(
                     struct Blocks *block,
                     struct Tracks *track,
                     Place *placement
                     );
void ReplaceNoteEnds(
                    struct Blocks *block,
                    struct Tracks *track,
                    Place *old_placement,
                    Place *new_placement
                    );

void RemoveNote(struct Blocks *block,
                struct Tracks *track,
                struct Notes *note
                );

extern void RemoveNoteCurrPos(struct Tracker_Windows *window);

extern struct Notes *FindNoteOnSubTrack(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int subtrack,
	int realline,
	Place *placement
);

extern void StopVelocityCurrPos(struct Tracker_Windows *window,int noend);

#endif

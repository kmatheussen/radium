#ifndef _RADIUM_COMMON_SEQBLOCK_STRETCHPOINTS_PROC_H
#define _RADIUM_COMMON_SEQBLOCK_STRETCHPOINTS_PROC_H

// Stretch points for audio seqblocks. See the comment for SeqBlockStretchPoint
// in nsmtracker.h for an explanation.
//
// All functions are safe to call from any thread. The read functions (get_*) never
// allocate, and are safe to call from the audio thread.

// Returns true if the seqblock has any stretch points.
extern LANGSPEC bool SEQBLOCK_STRETCHPOINTS_has(const struct SeqBlock *seqblock);

// Returns the number of stretch points, or 0 if the seqblock has none.
extern LANGSPEC int SEQBLOCK_STRETCHPOINTS_get_num(const struct SeqBlock *seqblock);

// Returns false if there are no stretch points, or if index is out of range.
extern LANGSPEC bool SEQBLOCK_STRETCHPOINTS_get(const struct SeqBlock *seqblock, int index, int64_t *seqtime, int64_t *sample_pos);

// Maps a position in the block (frames, relative to seqblock->t.time) to a position in the
// seqblock interior domain (same units as seqblock->t.interior_start/interior_end).
// If there are no stretch points, the plain linear mapping is used.
extern LANGSPEC int64_t SEQBLOCK_STRETCHPOINTS_get_sample_pos(const struct SeqBlock *seqblock, int64_t seqtime);

// The stretch factor (duration / sample duration, i.e. the same format as seqblock->t.stretch,
// but without the seqblock->t.speed factor) of the segment containing 'seqtime'.
// Returns seqblock->t.stretch if there are no stretch points.
extern LANGSPEC double SEQBLOCK_STRETCHPOINTS_get_stretch(const struct SeqBlock *seqblock, int64_t seqtime);

// The smallest stretch factor of any segment. Useful for finding out how much data is needed
// to be prepared in advance. Returns seqblock->t.stretch if there are no stretch points.
extern LANGSPEC double SEQBLOCK_STRETCHPOINTS_get_min_stretch(const struct SeqBlock *seqblock);

// Replaces the stretch point list for <seqblock>. The points are sanitized: points that are
// not strictly increasing in both seqtime and sample_pos are dropped, and the first and last
// points are forced to (0, interior_start) and (time2-time, interior_end).
// Passing num_points < 2 removes all stretch points.
// Must be called from the main thread (it's not necessary to hold the player lock, but allowed).
extern LANGSPEC void SEQBLOCK_STRETCHPOINTS_set(struct SeqBlock *seqblock, const struct SeqBlockStretchPoint *points, int num_points);

// Removes all stretch points.
extern LANGSPEC void SEQBLOCK_STRETCHPOINTS_clear(struct SeqBlock *seqblock);

// Scales all stretch point times when the duration of the seqblock changes for reasons
// that are not initiated by the stretch point editor (e.g. tempo changes).
extern LANGSPEC void SEQBLOCK_STRETCHPOINTS_duration_changed(struct SeqBlock *seqblock, int64_t old_duration, int64_t new_duration);

#endif

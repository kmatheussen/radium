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

#ifndef _RADIUM_API_API_COMMON_PROC_H
#define _RADIUM_API_API_COMMON_PROC_H

#ifdef __cplusplus
extern "C" {
#endif

int
#ifdef _AMIGA
__stdargs __saveds
#endif
radium_main(const char *arg);

extern void initradium(void);

extern const char *pullErrorMessage(void);
extern void printExceptionIfError(void);
extern void throwExceptionIfError(void); // Warning, is likely to cause a longjmp!
extern void clearErrorMessage(void); // Should be called before running code that might call handleError followed by throwExceptionIfError.


#ifdef RADIUM_COMMON_NSMTRACKER_H

#include "../common/placement_proc.h"
  
static inline void *VECTOR_get2_r0(const vector_t *v, int num, const char *type){
  if (num < 0){
    handleError("Can not use negative index for VECTOR_get. name: %s index: %d (size: %d)",type,num,v->num_elements);
    return NULL;
  }
  if (num>=v->num_elements)
    return NULL;
  
  return v->elements[num];
}
                         
static inline void *VECTOR_get2(const vector_t *v, int num, const char *type){
  void *ret = VECTOR_get2_r0(v, num, type);

  if (ret==NULL && num>=0){
    handleError("There is no %s %d (size: %d)",type,num,v->num_elements);
    return NULL;
  }
  
  return v->elements[num];
}

static inline bool validate_place(const Place place){
  if (place.line < 0 || place.counter >= place.dividor || place.dividor > MAX_UINT32){
    handleError("Place %d + %d/%d is not valid", place.line, place.counter, place.dividor);
    return false;
  }
  
  return true;
}

static inline bool validate_place2(const Place place, const struct Blocks *block){
  if (validate_place(place)==false)
    return false;
  
  if (p_Greater_Or_Equal(place, p_Last_Pos(block))){
    handleError("Place %s is placed after block end", p_ToString(place));
    return false;
  }

  return true;
}
#endif


extern struct Tracker_Windows *getWindowFromNum(int windownum);


extern struct WBlocks *getWBlockFromNum(int windownum,int wblocknum);


extern struct WBlocks *getWBlockFromNumA(
	int windownum,
	struct Tracker_Windows **window,
	int blocknum
);

extern struct Blocks *getBlockFromNum(int blocknum);

extern struct Tracks *getTrackFromNum(int blocknum,int tracknum);

extern struct WTracks *getWTrackFromNum(
	int windownum,
	int blocknum,
	int wtracknum
);


extern struct WTracks *getWTrackFromNumA(
	int windownum,
	struct Tracker_Windows **window,
	int wblocknum,
	struct WBlocks **wblock,
	int wtracknum
);

extern struct Notes *getNoteFromNum(int windownum,int blocknum,int tracknum,dyn_t note);
extern struct Notes *getNoteFromNumA(int windownum,struct Tracker_Windows **window, int blocknum, struct WBlocks **wblock, int tracknum, struct WTracks **wtrack, dyn_t dynnote);

extern struct Pitches *getPitchFromNumA(int windownum,struct Tracker_Windows **window, int blocknum, struct WBlocks **wblock, int tracknum, struct WTracks **wtrack, dyn_t dynnote, struct Notes **note, int pitchnum);
extern struct Pitches *getPitchFromNum(int windownum,int blocknum,int tracknum,dyn_t dynnote,int pitchnum);

extern struct Velocities *getVelocityFromNumA(int windownum,struct Tracker_Windows **window, int blocknum, struct WBlocks **wblock, int tracknum, struct WTracks **wtrack, dyn_t dynnote, struct Notes **note, int velocitynum);
extern struct Velocities *getVelocityFromNum(int windownum,int blocknum,int tracknum,dyn_t dynnote,int velocitynum);

extern struct Signatures *getSignatureFromNumA(int windownum,struct Tracker_Windows **window, int blocknum, struct WBlocks **wblock, int num);
extern struct Signatures *getSignatureFromNum(int windownum,int blocknum,int num);

extern struct LPBs *getLPBFromNumA(int windownum,struct Tracker_Windows **window, int blocknum, struct WBlocks **wblock, int num);
extern struct LPBs *getLPBFromNum(int windownum,int blocknum,int num);

extern struct BPMs *getBPMFromNumA(int windownum,struct Tracker_Windows **window, int blocknum, struct WBlocks **wblock, int bpmnum);
extern struct BPMs *getBPMFromNum(int windownum,int blocknum,int bpmnum);

extern struct FXs *getFXsFromNumA(int windownum,struct Tracker_Windows **window, int blocknum, struct WBlocks **wblock, int tracknum, struct WTracks **wtrack, int fxnum);
extern struct FXs *getFXsFromNum(int windownum,int blocknum,int tracknum,int fxnum);

extern struct Patch *getPatchFromNum(int64_t instrument_id);
extern struct Patch *getAudioPatchFromNum(int64_t instrument_id);

extern struct SeqTrack *getSeqtrackFromNum(int seqtracknum);
extern struct SeqTrack *getAudioSeqtrackFromNum(int seqtracknum);
extern struct SeqTrack *getBlockSeqtrackFromNum(int seqtracknum);

extern struct SeqBlock *getSeqblockFromNum(int seqblocknum, int seqtracknum);
extern struct SeqBlock *getSeqblockFromNumA(int seqblocknum, int seqtracknum, struct SeqTrack **seqtrack, bool use_gfx_if_possible);
extern struct SeqBlock *getAudioSeqblockFromNum(int seqblocknum, int seqtracknum);
extern struct SeqBlock *getAudioSeqblockFromNumA(int seqblocknum, int seqtracknum, struct SeqTrack **seqtrack);
extern struct SeqBlock *getSeqblockFromIdA(int64_t seqblock_id, struct SeqTrack **seqtrack);
extern struct SeqBlock *getSeqblockFromId(int64_t seqblock_id);
extern struct SeqBlock *getAudioSeqblockFromIdA(int64_t seqblockid, struct SeqTrack **seqtrack);
extern struct SeqBlock *getAudioSeqblockFromId(int64_t seqblock_id);

extern struct SeqBlock *getGfxSeqblockFromNumA(int seqblocknum, int seqtracknum, struct SeqTrack **seqtrack);
extern struct SeqBlock *getGfxSeqblockFromNum(int seqblocknum, int seqtracknum);
extern struct SeqBlock *getGfxGfxSeqblockFromNumA(int seqblocknum, int seqtracknum, struct SeqTrack **seqtrack);

extern const char* GetNoteIdAsCharString(int64_t note_id);
extern dyn_t GetNoteIdFromNoteId(int64_t note_id);
extern dyn_t GetNoteId(struct Notes *note);

extern void MoveEndNote(struct Blocks *block, struct Tracks *track, struct Notes *note, const Place *place, bool last_legal_may_be_next_note);
extern dyn_t MoveNote(struct Blocks *block, struct Tracks *track, struct Notes *note, Place *place, bool replace_note_ends);
  
#ifdef __cplusplus
}

#include "../common/OS_string_proc.h"

static inline const wchar_t *w_path_to_path(const char *w_path){
  return STRING_fromBase64(STRING_create(w_path));
}

static inline const wchar_t *w_path_to_path(const wchar_t *w_path){
  return STRING_fromBase64(w_path);
}

static inline QString w_to_qstring(const char* w_path){
  return STRING_get_qstring(w_path_to_path(w_path));
}

static inline const char* qstring_to_w(const QString path){
  return talloc_strdup(path.toUtf8().toBase64().constData());
}

static inline const char* path_to_w_path(const wchar_t *path){
  const QString path2 = STRING_get_qstring(path);
  const QByteArray path2_base64 = path2.toUtf8().toBase64();
  return talloc_strdup(path2_base64.constData());
}

#endif

#endif

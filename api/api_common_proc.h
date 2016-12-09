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




extern LANGSPEC struct Tracker_Windows *getWindowFromNum(int windownum);


extern LANGSPEC struct WBlocks *getWBlockFromNum(int windownum,int wblocknum);


extern LANGSPEC struct WBlocks *getWBlockFromNumA(
	int windownum,
	struct Tracker_Windows **window,
	int blocknum
);

extern LANGSPEC struct Blocks *getBlockFromNum(int blocknum);

extern LANGSPEC struct Tracks *getTrackFromNum(int blocknum,int tracknum);

extern LANGSPEC struct WTracks *getWTrackFromNum(
	int windownum,
	int blocknum,
	int wtracknum
);


extern LANGSPEC struct WTracks *getWTrackFromNumA(
	int windownum,
	struct Tracker_Windows **window,
	int wblocknum,
	struct WBlocks **wblock,
	int wtracknum
);

extern LANGSPEC struct Notes *getNoteFromNum(int windownum,int blocknum,int tracknum,int notenum);
extern LANGSPEC struct Notes *getNoteFromNumA(int windownum,struct Tracker_Windows **window, int blocknum, struct WBlocks **wblock, int tracknum, struct WTracks **wtrack, int notenum);

extern LANGSPEC struct Pitches *getPitchFromNumA(int windownum,struct Tracker_Windows **window, int blocknum, struct WBlocks **wblock, int tracknum, struct WTracks **wtrack, int notenum, struct Notes **note, int pitchnum);
extern LANGSPEC struct Pitches *getPitchFromNum(int windownum,int blocknum,int tracknum,int notenum,int pitchnum);

extern LANGSPEC struct Velocities *getVelocityFromNumA(int windownum,struct Tracker_Windows **window, int blocknum, struct WBlocks **wblock, int tracknum, struct WTracks **wtrack, int notenum, struct Notes **note, int velocitynum);
extern LANGSPEC struct Velocities *getVelocityFromNum(int windownum,int blocknum,int tracknum,int notenum,int velocitynum);

extern LANGSPEC struct Signatures *getSignatureFromNumA(int windownum,struct Tracker_Windows **window, int blocknum, struct WBlocks **wblock, int num);
extern LANGSPEC struct Signatures *getSignatureFromNum(int windownum,int blocknum,int num);

extern LANGSPEC struct LPBs *getLPBFromNumA(int windownum,struct Tracker_Windows **window, int blocknum, struct WBlocks **wblock, int num);
extern LANGSPEC struct LPBs *getLPBFromNum(int windownum,int blocknum,int num);

extern LANGSPEC struct BPMs *getBPMFromNumA(int windownum,struct Tracker_Windows **window, int blocknum, struct WBlocks **wblock, int bpmnum);
extern LANGSPEC struct BPMs *getBPMFromNum(int windownum,int blocknum,int bpmnum);

extern LANGSPEC struct FXs *getFXsFromNumA(int windownum,struct Tracker_Windows **window, int blocknum, struct WBlocks **wblock, int tracknum, struct WTracks **wtrack, int fxnum);
extern LANGSPEC struct FXs *getFXsFromNum(int windownum,int blocknum,int tracknum,int fxnum);

extern LANGSPEC struct Patch *getPatchFromNum(int64_t instrument_id);
extern LANGSPEC struct Patch *getAudioPatchFromNum(int64_t instrument_id);

extern LANGSPEC struct SeqTrack *getSeqtrackFromNum(int seqtracknum);
extern LANGSPEC struct SeqBlock *getSeqblockFromNum(int seqblocknum, int seqtracknum);
extern LANGSPEC struct SeqBlock *getSeqblockFromNumA(int seqblocknum, int seqtracknum, struct SeqTrack **seqtrack);
extern LANGSPEC struct SeqBlock *getGfxGfxSeqblockFromNumA(int seqblocknum, int seqtracknum, struct SeqTrack **seqtrack);

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

extern void PC_RemoveFirst(void);
extern const struct Blocks *PC_GetPlayBlock(int numfromcurrent);
extern bool PC_GetNextNoteAfterCurrentBlock(NInt tracknum, int *playlistaddpos, struct Notes **note, struct Tracks **track, const struct Blocks **block);

extern void PC_InsertElement(struct PEventQueue *peq, int addplaypos, STime addtime);
extern void PC_InsertElement_latencycompencated(struct PEventQueue *peq, int addplaypos, STime addtime);
extern void PC_InsertElement2(struct PEventQueue *peq, int addplaypos, const Place *p);
extern void PC_InsertElement2_latencycompencated(struct PEventQueue *peq, int addplaypos, const Place *p);
extern void PC_InsertElement_a(struct PEventQueue *peq, int addplaypos, STime addtime);
extern void PC_InsertElement2_a(struct PEventQueue *peq, int addplaypos, const Place *p);
extern void PC_InsertElement2_a_latencycompencated(struct PEventQueue *peq, int addplaypos, const Place *p);

extern void PC_ReturnElements(void);
extern void PC_ReturnElements_fromPlayPos(int playpos);

extern void PC_GoNextBlock(void);

extern bool PC_isPlayingBlock(void);
extern bool PC_isPlayingSong(void);

extern STime PC_TimeToRelBlockStart(STime time);

#endif


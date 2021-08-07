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


#ifndef _RADIUM_COMMON_OS_BS_EDIT_PROC_H
#define _RADIUM_COMMON_OS_BS_EDIT_PROC_H

extern LANGSPEC void BS_call_very_often(void);
extern LANGSPEC void BS_resizewindow(void);
extern LANGSPEC void BS_UpdateBlockList(void);
extern LANGSPEC void BS_UpdatePlayList(void);
extern LANGSPEC void BS_UpdatePlayListForceUpdate(void); // must be called instead of BS_UpdatePlayList if name of a block might have been changed.
extern LANGSPEC void BS_SelectBlock(struct Blocks *block);
extern LANGSPEC void BS_SelectBlocklistPos(int pos);
extern LANGSPEC void BS_SelectPlaylistPos(int pos, bool change_song_pos_too);
void BS_SelectPlaylistPosFromSeqblock(struct SeqBlock *seqblock, bool change_song_pos_too);
extern LANGSPEC struct SeqBlock *BS_GetPrevPlaylistBlock(void);
extern LANGSPEC struct SeqBlock *BS_GetNextPlaylistBlock(void);  
extern LANGSPEC struct SeqBlock *BS_GetSeqBlockFromPos(int pos);
extern LANGSPEC struct Blocks *BS_GetBlockFromPos(int pos);
extern LANGSPEC int BS_GetCurrBlocklistPos(void);
extern LANGSPEC int BS_GetCurrPlaylistPos(void);

#endif



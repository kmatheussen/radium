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


#ifndef _RADIUM_COMMON_TEMPOS_PROC_H
#define _RADIUM_COMMON_TEMPOS_PROC_H


extern LANGSPEC struct WBPMs *WBPMs_get(
                                        const struct Tracker_Windows *window,
                                        const struct WBlocks *wblock
                                        );

#if USE_QT4
QVector<Tempos*> BPMs_get(const struct WBlocks *wblock, int realline);
#endif

extern LANGSPEC struct Tempos *SetTempo(
	struct Blocks *block,
	const Place *place,
	int newtempo,
        int logtype
);

extern LANGSPEC void SetTempos(
                               struct Blocks *block,
                               struct Tempos *tempos
                               );

extern LANGSPEC void SetTempoCurrPos(struct Tracker_Windows *window);

extern LANGSPEC void RemoveTempo(struct Blocks *block,const struct Tempos *tempo);
extern LANGSPEC void RemoveTemposCurrPos(struct Tracker_Windows *window);

#endif

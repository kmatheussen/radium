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


extern LANGSPEC struct WLPBs *WLPBs_get(
                                        const struct Tracker_Windows *window,
                                        const struct WBlocks *wblock
                                        );

#ifdef USE_QT4
QVector<LPBs*> LPBs_get(const struct WBlocks *wblock, int realline);
#endif
  
extern LANGSPEC struct LPBs *SetLPB(
                                    struct Blocks *block,
                                    const Place *place,
                                    int newlpb
                                    );
extern LANGSPEC void SetLPBCurrPos(struct Tracker_Windows *window);

extern LANGSPEC void RemoveLPB(struct Blocks *block,struct LPBs *lpb);
extern LANGSPEC void RemoveLPBsCurrPos(struct Tracker_Windows *window);


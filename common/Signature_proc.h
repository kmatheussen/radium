/* Copyright 2000-2015 Kjetil S. Matheussen

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

#if USE_QT4
const WSignature_trss WSignatures_get(
                                      const struct Tracker_Windows *window,
                                      const struct WBlocks *wblock
                                      );
#endif

extern LANGSPEC struct Signatures *SetSignature(
                                                struct Blocks *block,
                                                const Place *place,
                                                StaticRatio ratio
                                                );
extern LANGSPEC void SetSignatureCurrPos(struct Tracker_Windows *window);

extern LANGSPEC void RemoveSignaturesCurrPos(struct Tracker_Windows *window);


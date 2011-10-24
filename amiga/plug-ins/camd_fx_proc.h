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

extern void CAMD_treatFX(struct FX *fx,int val,struct Tracks *track,int skip);
extern void CAMD_closeFX(struct FX *fx,struct Tracks *track);

extern bool CAMDSetTreatFX(struct FX *fx,struct CAMD_FX *camd_fx);

extern int CAMDgetFX(struct Tracker_Windows *window,struct Tracks *track,struct FX *fx);

extern void *CAMD_CopyInstrumentData(struct Tracks *track);

#endif


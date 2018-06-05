/* Copyright 2012 Kjetil S. Matheussen

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


#ifndef _RADIUM_AUDIO_UNDO_AUDIO_EFFECT_PROC_H
#define _RADIUM_AUDIO_UNDO_AUDIO_EFFECT_PROC_H

extern LANGSPEC void ADD_UNDO_FUNC(AudioEffect_CurrPos(struct Patch *patch, int effect_num));
extern LANGSPEC void ADD_UNDO_FUNC(AudioEffect_CurrPos2(struct Patch *patch, int effect_num, float value));
extern LANGSPEC void ADD_UNDO_FUNC(OnlySystemEffects(struct Patch *patch));
  
#endif

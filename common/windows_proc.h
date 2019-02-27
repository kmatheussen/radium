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

#ifndef _RADIUM_COMMON_WINDOWS_PROC_H
#define _RADIUM_COMMON_WINDOWS_PROC_H


extern LANGSPEC void UpdateTrackerWindow(struct Tracker_Windows *window);

extern LANGSPEC void DrawUpTrackerWindow(struct Tracker_Windows *window);

extern LANGSPEC int OpenTrackerWindow(int x,int y,int width,int height);

extern LANGSPEC int CloseTrackerWindow(NInt place);

extern LANGSPEC void CloseAllTrackerWindows(void);

extern LANGSPEC void checkIfWBlocksAreDirty(void);

extern LANGSPEC void ValidateCursorPos(struct Tracker_Windows *window);

static inline int getBottomSliderX2(struct Tracker_Windows *window){
  return window->width-1;
}

#ifdef USE_QT4

#include "../Qt/EditorWidget.h"

#endif


#endif

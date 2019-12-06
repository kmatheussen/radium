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

#ifndef _RADIUM_COMMON_WBLOCKS_PROC_H
#define _RADIUM_COMMON_WBLOCKS_PROC_H

static inline float get_scrollbar_x1(const struct Tracker_Windows *window){
  return 0.0;
}

// Returns y coordinate for the opengl widget, not the editor widget.
static inline float get_scrollbar_y1(const struct Tracker_Windows *window, const struct WBlocks *wblock){
  return 2.0;
}

static inline float get_scrollbar_x2(const struct Tracker_Windows *window){
  return window->leftslider.width;
}

// Returns y coordinate for the opengl widget, not the editor widget.
static inline float get_scrollbar_y2(const struct Tracker_Windows *window, const struct WBlocks *wblock){
  return wblock->t.y2 - wblock->t.y1;
}

// Is also called from the OpenGL thread, where we don't have access to a Tracker_Windows instance.
// Returns y coordinate for the opengl widget, not the editor widget.
static inline float get_scrollbar_scroller_y1(float realline, float num_reallines, float scrollbar_height, float scrollbar_scroller_height){
  return scale(realline,
               0, num_reallines,
               2.5, (scrollbar_height - scrollbar_scroller_height)
               );
}

static inline float get_scrollbar_scroller_height(const struct Tracker_Windows *window, const struct WBlocks *wblock){
  return  (float)(wblock->t.y2 - wblock->t.y1 - 6)
    * (float)wblock->num_visiblelines
    / (float)(wblock->num_reallines + wblock->num_visiblelines - 2);
}



extern LANGSPEC void CloseWBlock(struct Tracker_Windows *window, NInt blocknum);
extern LANGSPEC void CloseAllWBlocks(struct Tracker_Windows *window);

extern LANGSPEC bool WBlock_legalizeStartEndReallines(const struct WBlocks *wblock,int *start_realline,int *end_realline);

extern LANGSPEC void UpdateWBlockCoordinates(
	struct Tracker_Windows *window,
	struct WBlocks *wblock
);

extern LANGSPEC void SetWBlock_Top_And_Bot_Realline(
                                                    const struct Tracker_Windows *window,
                                                    struct WBlocks *wblock
);

extern LANGSPEC void UpdateAllWBlockCoordinates(
	struct Tracker_Windows *window
);

extern LANGSPEC void UpdateWBlockWidths(struct Tracker_Windows *window,struct WBlocks *wblock);
extern LANGSPEC void UpdateAllWBlockWidths(struct Tracker_Windows *window);

extern LANGSPEC void SelectWBlock(struct Tracker_Windows *window,struct WBlocks *wblock);

extern LANGSPEC void SelectPrevWBlock(struct Tracker_Windows *window);

extern LANGSPEC void SelectNextWBlock(struct Tracker_Windows *window);

extern LANGSPEC void SelectPrevPlaylistWBlock(struct Tracker_Windows *window, bool change_song_pos_too);
extern LANGSPEC void SelectNextPlaylistWBlock(struct Tracker_Windows *window, bool change_song_pos_too);

extern LANGSPEC void NewWBlock(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct Blocks *block
);

extern LANGSPEC void UpdateWBlocks(struct Tracker_Windows *window);

extern LANGSPEC struct Blocks *AppendWBlock(struct Tracker_Windows *window);

extern LANGSPEC void AppendWBlock_spes(struct Tracker_Windows *window,int num_lines,NInt num_tracks);

#endif

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


#include "placement_proc.h"


static inline void set_p1_and_p2(
                                 struct WBlocks *wblock,
                                 int start_realline,
                                 int end_realline,
                                 Place *p1,
                                 Place *p2
                                 )
{
  R_ASSERT_RETURN_IF_FALSE(start_realline>=0 && start_realline < wblock->num_reallines);
  R_ASSERT_RETURN_IF_FALSE(start_realline>=0 && end_realline <= wblock->num_reallines);
  
  PlaceCopy(p1, &wblock->reallines[start_realline]->l.p);
           
  if (end_realline>=wblock->num_reallines-1)
    PlaceSetLastPos(wblock->block, p2);
  else
    PlaceCopy(p2, &wblock->reallines[end_realline+1]->l.p);
}



extern LANGSPEC float FindReallineForF(
                                       const struct WBlocks *wblock,
                                       float reallineF, // start search from here. Use 0 to search all.
                                       const Place *place
                                       );

extern LANGSPEC int FindRealLineFor(
                                    const struct WBlocks *wblock,
                                    int realline,
                                    const Place *place
                                    );

extern LANGSPEC int FindRealLineForNote(
                                        const struct WBlocks *wblock,
                                        int realline,
                                        const struct Notes *note
);

extern LANGSPEC int FindRealLineForEndNote(
                                           const struct WBlocks *wblock,
                                           int realline,
                                           const struct Notes *note
);

extern LANGSPEC int FindSubRealLine(
                                    const struct Tracker_Windows *window,
                                    const struct WBlocks *wblock,
                                    int realline,
                                    const Place *p
);

static inline int get_realline_y1(const struct Tracker_Windows *window, int realline){
  return window->fontheight*realline;
}

static inline int get_realline_y2(const struct Tracker_Windows *window, int realline){
  return window->fontheight*(realline+1);
}

static inline float get_realline_y(const struct Tracker_Windows *window, float reallineF){
  return (float)window->fontheight*reallineF;
}

#ifdef __cplusplus
static inline int FindReallineForRatio(const struct WBlocks *wblock, int realline, const Ratio &ratio){
  Place place = ratio2place(ratio);
  return FindRealLineFor(wblock, realline, &place);
}
static inline float FindReallineForRatioF(const struct WBlocks *wblock, int realline, const Ratio &ratio){
  Place place = ratio2place(ratio);
  return FindReallineForF(wblock, (float)realline, &place);
}
#endif

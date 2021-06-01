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

#pragma once

extern LANGSPEC struct FXs *FXs_create(void);

extern LANGSPEC void FX_min_max_have_changed_for_patch(struct Patch *patch, NInt fxnum, float old_min, float old_max, float new_min, float new_max);

extern LANGSPEC int AddFXNodeLine(
                                  struct Tracker_Windows *window,
                                  struct WBlocks *wblock,
                                  struct WTracks *wtrack,
                                  struct FXs *fxs,
                                  int val,
                                  const Place *p1
                                  );

extern LANGSPEC void AddFXNodeLineCustomFxAndPos(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack, struct FX *fx, const Place *p, float val);
  
extern LANGSPEC void AddFXNodeLineCurrMousePos(struct Tracker_Windows *window);

extern LANGSPEC void AddFXNodeLineCurrPos(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack);

//extern LANGSPEC void DeleteFxNodeLine(struct Tracker_Windows *window, struct WTracks *wtrack, struct FXs *fxs, struct FXNodeLines *fxnodeline);
#if __cplusplus
extern void DeleteFxNodes(struct Tracker_Windows *window, struct WTracks *wtrack, struct FXs *fxs, const std::vector<int> &fxnodenums);
extern void DeleteFxNode(struct Tracker_Windows *window, struct WTracks *wtrack, struct FXs *fxs, int fxnodenum);

#if defined(_RADIUM_COMMON_RATIO_FUNCS_H)
extern void RT_fxline_called_each_block(struct SeqTrack *seqtrack,
                                        const int play_id,
                                        const struct SeqBlock *seqblock,
                                        const struct Tracks *track,                                 
                                        const int64_t seqtime_start,
                                        const int64_t seqtime_end,
                                        const r::RatioPeriod &track_period
                                        );
#endif

#endif

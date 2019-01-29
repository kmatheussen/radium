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


#ifndef RADIUM_COMMON_TIME_PROC_H
#define RADIUM_COMMON_TIME_PROC_H

#include <math.h>

#include "placement_proc.h"

static inline bool has_same_tempo(double tempo1, double tempo2){
  return fabs(tempo1 - tempo2) < 0.003; // approx.
}


// Can be called from any thread.
extern LANGSPEC double Place2STime_from_times2(
                                               const struct STimes *stimes,
                                               double place_as_float
                                               );

// Can be called from any thread.
static inline STime Place2STime_from_times(int num_lines, const struct STimes *times, const Place *p){
  
  if(0==p->counter)
    return times[p->line].time;

  if (p->line >= num_lines){
    R_ASSERT_NON_RELEASE(false);
    return times[num_lines].time;
  }

  double y = GetDoubleFromPlace(p);
  return Place2STime_from_times2(times, y);
}

// Can be called from any thread.
static inline STime Place2STime(
	const struct Blocks *block,
	const Place *p
){
  return Place2STime_from_times(block->num_time_lines, block->times, p);
}

extern LANGSPEC double STime2Place_f(
                                    const struct Blocks *block,
                                    double time
                                    );

extern LANGSPEC Place STime2Place(
                                  const struct Blocks *block,
                                  STime time
                                  );

// Can be called from any thread.
static inline STime getBlockSTimeLength(const struct Blocks *block){                       
  if (block==NULL){
#if !defined(RELEASE)
    abort();
#else
    return 48000; // This should never happen, and it has never happened.
#endif
  }

#if !defined(RELEASE)
  if (THREADING_is_main_thread()){
    if (block->num_lines != block->num_time_lines) // 'num_time_lines' is the actual allocated number of lines.
      RWarning("block->num_lines != block->num_time_lines: %d != %d",block->num_lines, block->num_time_lines);

    R_ASSERT(block->times[block->num_time_lines].time == block->length);
  }
#endif
  
  return block->length;
}

static inline bool isSTimeInBlock(const struct Blocks *block,STime time){
  STime block_length = getBlockSTimeLength(block);
  if(time > block_length)
    return false;
  else
    return true;
}



extern LANGSPEC void TIME_block_tempos_have_changed(struct Blocks *block);
extern LANGSPEC void TIME_block_LPBs_have_changed(struct Blocks *block);
extern LANGSPEC void TIME_block_signatures_have_changed(struct Blocks *block);
extern LANGSPEC void TIME_block_num_lines_have_changed(struct Blocks *block);
extern LANGSPEC void TIME_block_num_tracks_have_changed(struct Blocks *block);
extern LANGSPEC void TIME_block_swings_have_changed(struct Blocks *block);
extern LANGSPEC void TIME_global_tempos_have_changed(void);
extern LANGSPEC void TIME_global_LPB_has_changed(void);
extern LANGSPEC void TIME_global_signature_has_changed(void);
extern LANGSPEC void TIME_everything_has_changed2(const struct Root *root, struct Song *song);
extern LANGSPEC void TIME_everything_has_changed(void);
extern LANGSPEC void TIME_everything_in_block_has_changed2(struct Blocks *block, const struct Root *root, const struct Song *song);
extern LANGSPEC void TIME_everything_in_block_has_changed(struct Blocks *block);
  
#endif


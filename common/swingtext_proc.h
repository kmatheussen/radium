
#ifndef _RADIUM_COMMON_SWINGTEXT_PROC_H
#define _RADIUM_COMMON_SWINGTEXT_PROC_H

#include "placement_proc.h"


extern LANGSPEC int SWINGTEXT_subsubtrack(const struct Tracker_Windows *window, const struct WTracks *wtrack);
extern LANGSPEC bool SWINGTEXT_keypress(struct Tracker_Windows *window, struct WBlocks *wblock, struct WTracks *wtrack, int realline, const Place *place, int key); // wtrack must be NULL if window->curr_track < 0

static inline bool swingtext_fits_reallines(struct WBlocks *wblock, const dynvec_t *barswings){
  R_ASSERT_RETURN_IF_FALSE2(wblock->block->beats!=NULL, false);
  R_ASSERT_RETURN_IF_FALSE2(wblock->block->filledout_swings.type==ARRAY_TYPE, false);
  R_ASSERT_RETURN_IF_FALSE2(wblock->reallines!=NULL, false);
  
  const struct Beats *beats = wblock->block->beats;
  
  //const dynvec_t *barswings = wblock->block->filledout_swings.array;

  int realline = 0;
  
  for(int i = 0; i < barswings->num_elements ; i++){
    R_ASSERT_RETURN_IF_FALSE2(barswings->elements[i].type==HASH_TYPE, false);
    
    const hash_t *barswing = barswings->elements[i].hash;
    
    int barnum = HASH_get_int32(barswing, ":barnum");
    const dynvec_t *swings = HASH_get_dyn(barswing, ":swings").array;

    while(beats!=NULL && beats->bar_num-1 < barnum)
      beats = NextBeat(beats);

    R_ASSERT_RETURN_IF_FALSE2(beats!=NULL, false);

    for(int swingnum = 0 ; swingnum < swings->num_elements ; swingnum++){
      R_ASSERT_RETURN_IF_FALSE2(swings->elements[swingnum].type==ARRAY_TYPE, false);
      
      const dynvec_t *swing = swings->elements[swingnum].array;
      
      const Place place = p_Add(beats->l.p, DYN_get_place(swing->elements[0]));
        
      Place realline_place2 = realline==wblock->num_reallines-1 ? p_Last_Pos(wblock->block) :  wblock->reallines[realline+1]->l.p;

      if (p_Greater_Or_Equal(place, realline_place2)){

        do{
          realline++;
          realline_place2 = realline==wblock->num_reallines-1 ? p_Last_Pos(wblock->block) :  wblock->reallines[realline+1]->l.p;
        }while(p_Greater_Or_Equal(place, realline_place2));
      }
      
      const Place realline_place1 = wblock->reallines[realline]->l.p;

      if (!p_Equal(place, realline_place1))
        return false;
    }
  }

  return true;
}


#endif


extern LANGSPEC struct Beats *Beats_get(struct Blocks *block, StaticRatio default_signature, int default_lpb);

static inline bool get_barbeat_start_and_end(const struct Blocks *block, int barnum, int beatnum, Place *start, Place *end){

  bool find_beat_only = true;
    
  if (beatnum < 1){
    find_beat_only = false;
    beatnum = 1;
  }
  
  bool found_start = false;
  
  const struct Beats *beat = block->beats;

  while(beat != NULL){
    
    if (beat->bar_num > barnum)
      break;

    if (beat->bar_num==barnum && beat->beat_num==beatnum){

      if (found_start==false) {

        if (start != NULL)
          *start = beat->l.p;

        if (end==NULL)
          return true;

        found_start = true;
        
        if (find_beat_only) {

          beat = NextBeat(beat);

          R_ASSERT_RETURN_IF_FALSE2(end!=NULL, false);

          if (beat==NULL)
            *end = p_Absolute_Last_Pos(block);
          else
            *end = beat->l.p;

          return true;
          
        } else {

          barnum++;
          
        }
        
      } else {

        R_ASSERT_RETURN_IF_FALSE2(end!=NULL, false);
        *end = beat->l.p;
        return true;
        
      }
    }

    beat = NextBeat(beat);
  }

  if (found_start){
    R_ASSERT_RETURN_IF_FALSE2(end!=NULL, false);
    *end = p_Absolute_Last_Pos(block);
    return true;
  }

  return false;
}




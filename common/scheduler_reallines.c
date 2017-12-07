
#include "nsmtracker.h"
#include "list_proc.h"
#include "../audio/Pd_plugin_proc.h"
#include "../OpenGL/Widget_proc.h"
#include "player_pause_proc.h"
#include "placement_proc.h"
#include "realline_calc_proc.h"

#include "../audio/Mixer_proc.h"

#include "scheduler_proc.h"



static int64_t RT_scheduled_realline(struct SeqTrack *seqtrack, int64_t time, union SuperType *args){
  const struct SeqBlock *seqblock = args[0].const_pointer;
  struct WBlocks *wblock = args[1].pointer;
  int realline = args[2].int32_num;

  //printf("%d\n", realline);
  
#ifdef WITH_PD
  bool inserted_pd_realline = false;
  int64_t org_time = time;
  const Place *org_pos = NULL;

  if (realline < wblock->num_reallines) // number of reallines can change while playing.
    org_pos = &wblock->reallines[realline]->l.p;
#endif

  // Do thing with the current realline
  {
    if (!ATOMIC_GET(root->play_cursor_onoff)){
      
      // Set current realline in main thread (main thread picks up till_curr_realline and sets curr_realline afterwards)          
      ATOMIC_SET(wblock->till_curr_realline, realline);
      
      // Set current realline in opengl thread
      //printf("PEQ: set realline %d\n",realline);
      GE_set_curr_realline(realline);

    }

    PC_Pause_set_pos(wblock->l.num, realline);
  }

  
  // Schedule next realline
  //
  
  realline++;

  if(pc->playtype==PLAYRANGE){ // This never happens. Instead playtype is PLAYBLOCK, and pc->is_playing_range is true;
    R_ASSERT(false);
    /*
    if(realline>=wblock->rangey2){
      realline=wblock->rangey1;
    }

    // sanity checks to avoid crash. May happen if editing reallines while playing.
    if (realline>=wblock->num_reallines) // If outside range, first try to set realline to rangey1
      realline = wblock->rangey1;

    if (realline>=wblock->num_reallines) // that didnt work, set realline to 0
      realline = 0;
    */
  } else if (pc->playtype==PLAYBLOCK && pc->is_playing_range == true){

    if (realline>=wblock->num_reallines || p_Greater_Than(wblock->reallines[realline]->l.p, wblock->rangey2)){

      ATOMIC_SET(pc->player_state, PLAYER_STATE_STOPPING);
      
      //PC_ReturnElements();

      return DONT_RESCHEDULE;
    }
          
  }else if(realline>=wblock->num_reallines) {

    return DONT_RESCHEDULE;
    
  }

    
#ifdef WITH_PD
  if (org_pos != NULL)
    if(inserted_pd_realline==false)
      RT_PD_set_realline(org_time, time, org_pos);
#endif

  {
    args[2].int32_num = realline;
    Place next_place = wblock->reallines[realline]->l.p;
    
    return get_seqblock_place_time(seqblock, next_place);
  }
}



void RT_schedule_reallines_in_block(struct SeqTrack *seqtrack, const struct SeqBlock *seqblock, const Place place){
  R_ASSERT_RETURN_IF_FALSE(seqblock->block != NULL);

  struct WBlocks *wblock=(struct WBlocks *)ListFindElement1(&root->song->tracker_windows->wblocks->l,seqblock->block->l.num);

  int realline=FindRealLineFor(wblock,0,&place);

  if(realline>=wblock->num_reallines)
    return;

  const int num_args = 3;
    
  union SuperType args[num_args];

  args[0].const_pointer = seqblock;
  args[1].pointer = wblock;
  args[2].int32_num = realline;

  Place realline_place = wblock->reallines[realline]->l.p;
  int64_t time = get_seqblock_place_time(seqblock, realline_place);

  SCHEDULER_add_event(seqtrack, time, RT_scheduled_realline, &args[0], num_args, SCHEDULER_INIT_PRIORITY);
}

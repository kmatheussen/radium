
#include "nsmtracker.h"
#include "list_proc.h"
#include "../audio/Pd_plugin_proc.h"
#include "../OpenGL/Widget_proc.h"
#include "player_pause_proc.h"
#include "placement_proc.h"
#include "realline_calc_proc.h"

#include "../audio/Mixer_proc.h"

#include "scheduler_proc.h"

static void setit(const struct SeqTrack *seqtrack, struct WBlocks *wblock, int realline){
  if(seqtrack != RT_get_curr_seqtrack())
    return;

  if (!ATOMIC_GET(root->play_cursor_onoff)){
    
    // Set current realline in main thread (main thread picks up till_curr_realline and sets curr_realline afterwards)
    //printf("   Setting till_curr_realline to %d\n", realline);
    ATOMIC_SET(wblock->till_curr_realline, realline);
    
    // Set current realline in opengl thread
    //printf("PEQ: set realline %d\n",realline);
    GE_set_curr_realline(realline);
    
  }
  
  PC_Pause_set_pos(wblock->l.num, realline);
}


static int64_t RT_scheduled_realline(struct SeqTrack *seqtrack, int64_t time, union SuperType *args){
  const struct SeqBlock *seqblock = args[0].const_pointer;
  struct WBlocks *wblock = args[1].pointer;
  int realline = args[2].int32_num;

  int64_t counter = args[3].int_num;

  //printf("%d. counter: %d / seqblock->counter: %d. Num reallines: %d\n", realline, (int)counter, (int)seqblock->curr_scheduled_realline_counter, wblock->num_reallines);


  if (seqblock->curr_scheduled_realline_counter > counter){ // I.e. this event have been replaced because the number of reallines was changed while playing.
    //printf("      stop1: %d / %d\n", (int)counter, (int)seqblock->curr_scheduled_realline_counter);
    return DONT_RESCHEDULE;
  }

  const int num_reallines = wblock->num_reallines;

  /*
  if (num_reallines != wblock->num_reallines){ // Happens when changing LZ value.
    realline = get_curr_realline_for_seqtrack(seqtrack);

    num_reallines = wblock->num_reallines;
    args[3].int32_num = num_reallines;
  }
  */
  
#ifdef WITH_PD
  bool inserted_pd_realline = false;
  int64_t org_time = time;
  const Place *org_pos = NULL;

  if (realline < num_reallines) // number of reallines can change while playing.
    org_pos = &wblock->reallines[realline]->l.p;
#endif

  // Do thing with the current realline
  setit(seqtrack, wblock, realline);

  
  // Schedule next realline
  //

  const int next_realline = realline+1;

  if(pc->playtype==PLAYRANGE){ // This never happens. Instead playtype is PLAYBLOCK, and pc->is_playing_range is true;
    R_ASSERT(false);
    /*
    if(next_realline>=wblock->rangey2){
      next_realline=wblock->rangey1;
    }

    // sanity checks to avoid crash. May happen if editing next_reallines while playing.
    if (next_realline>=wblock->num_next_reallines) // If outside range, first try to set next_realline to rangey1
      next_realline = wblock->rangey1;

    if (next_realline>=wblock->num_next_reallines) // that didn't work, set next_realline to 0
      next_realline = 0;
    */
  } else if (pc->playtype==PLAYBLOCK && pc->is_playing_range == true){

    if (next_realline>=num_reallines || p_Greater_Than(wblock->reallines[next_realline]->l.p, wblock->rangey2)){

      ATOMIC_SET(pc->player_state, PLAYER_STATE_STOPPING);
      
      //PC_ReturnElements();

      return DONT_RESCHEDULE;
    }
          
  }else if(next_realline>=num_reallines) {

    return DONT_RESCHEDULE;
    
  }

    
#ifdef WITH_PD
  if (org_pos != NULL)
    if(inserted_pd_realline==false)
      RT_PD_set_realline(org_time, time, org_pos);
#endif

  {
    args[2].int32_num = next_realline;
    Place next_place = wblock->reallines[next_realline]->l.p;

    //printf("       next_place: %d + %d/%d\n", next_place.line, next_place.counter, next_place.dividor);
    return get_seqblock_place_time2(seqblock, wblock->wtrack->track, next_place);
  }
}


static void RT_schedule_reallines_in_block2(struct SeqTrack *seqtrack, struct SeqBlock *seqblock, struct WBlocks *wblock, int realline){
  R_ASSERT_RETURN_IF_FALSE(seqblock->block != NULL);

  if(realline>=wblock->num_reallines)
    return;

  const int num_args = 4;
    
  union SuperType args[num_args];

  args[0].const_pointer = seqblock;
  args[1].pointer = wblock;
  args[2].int32_num = realline;
  args[3].int_num = ++seqblock->curr_scheduled_realline_counter;

  Place realline_place = wblock->reallines[realline]->l.p;
  int64_t time = get_seqblock_place_time2(seqblock, wblock->wtrack->track, realline_place);

  SCHEDULER_add_event(seqtrack, time, RT_scheduled_realline, &args[0], num_args, SCHEDULER_INIT_PRIORITY);
}

void RT_schedule_reallines_in_block(struct SeqTrack *seqtrack, struct SeqBlock *seqblock, const Place place){
  struct WBlocks *wblock = (struct WBlocks *)ListFindElement1(&root->song->tracker_windows->wblocks->l,seqblock->block->l.num);
  int realline=FindRealLineFor(wblock,0,&place);

  RT_schedule_reallines_in_block2(seqtrack, seqblock, wblock, realline);
}
  
static void reschedule_reallines_because_num_reallines_have_changed_in_wblock3(struct SeqTrack *seqtrack, struct SeqBlock *seqblock, struct WBlocks *wblock, int64_t curr_seqtrack_time){  
  R_ASSERT_NON_RELEASE(seqblock->block != NULL);

  struct Blocks *block = wblock->block;

  if (seqblock->block == block && curr_seqtrack_time < seqblock->t.time2) {
    
    PLAYER_lock();{
      
      curr_seqtrack_time = seqtrack->start_time; // Get accurate seqtrack time, and also update it.
      
      if (curr_seqtrack_time >= seqblock->t.time) {
        if (curr_seqtrack_time < seqblock->t.time2) {
          
          STime stime = seqtime_to_blocktime(seqblock, curr_seqtrack_time - seqblock->t.time);
          Place place = STime2Place2(block, stime, wblock->wtrack->track);
          
          int realline=FindRealLineFor(wblock,0,&place);

          setit(seqtrack, wblock, realline);
            
          realline++;
          
          //printf("    Rescheduling block %d. Realline: %d. Place: %d + %d/%d\n", wblock->l.num, realline, place.line,place.counter,place.dividor);
          RT_schedule_reallines_in_block2(seqtrack, seqblock, wblock, realline);
        }
      }
      
      
    }PLAYER_unlock();
  }
  
}

static void reschedule_reallines_because_num_reallines_have_changed_in_wblock2(struct SeqTrack *seqtrack, struct WBlocks *wblock){

  int64_t curr_seqtrack_time = ATOMIC_DOUBLE_GET_RELAXED(seqtrack->start_time_nonrealtime);
  //printf("curr_seqtrack_time: %f\n",  (double)curr_seqtrack_time / (double)pc->pfreq);
  
  VECTOR_FOR_EACH(struct SeqBlock *seqblock, &seqtrack->seqblocks){

    reschedule_reallines_because_num_reallines_have_changed_in_wblock3(seqtrack, seqblock, wblock, curr_seqtrack_time);
      
  }END_VECTOR_FOR_EACH;
  
}

// It should be safe to call this function at any time, and more often than necessary.
void reschedule_reallines_because_num_reallines_have_changed_in_wblock(struct WBlocks *wblock){
  if (!is_playing())
    return;

  
  // I.e. when pressing left shift + up/down while playing.
  
  
  if(pc->playtype==PLAYSONG){

    VECTOR_FOR_EACH(struct SeqTrack *seqtrack, &root->song->seqtracks){
      if (seqtrack->for_audiofiles==false)
        reschedule_reallines_because_num_reallines_have_changed_in_wblock2(seqtrack, wblock);
    }END_VECTOR_FOR_EACH;
    
  } else {

    struct SeqTrack *curr_seqtrack = root->song->block_seqtrack;
    
    int64_t curr_seqtrack_time = ATOMIC_DOUBLE_GET_RELAXED(curr_seqtrack->start_time_nonrealtime);
    
    reschedule_reallines_because_num_reallines_have_changed_in_wblock3(curr_seqtrack, &g_block_seqtrack_seqblock, wblock, curr_seqtrack_time);
    
  }
}

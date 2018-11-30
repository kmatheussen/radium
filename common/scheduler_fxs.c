
#include "nsmtracker.h"
#include "placement_proc.h"
#include "patch_proc.h"


#include "scheduler_proc.h"



#define g_num_fx_args 7

static void RT_schedule_fxnodeline(
                                   struct SeqTrack *seqtrack,
                                   const struct SeqBlock *seqblock,
                                   const struct Tracks *track,
                                   struct FX *fx,
                                   const struct FXNodeLines *fxnodeline1,
                                   Place start_place
                                   );

static int64_t RT_scheduled_fx(struct SeqTrack *seqtrack, int64_t time, union SuperType *args){
  const struct SeqBlock    *seqblock    = args[0].const_pointer;
  const struct Tracks      *track       = args[1].const_pointer;
  struct FX                *fx          = args[2].pointer;
  const struct FXNodeLines *fxnodeline1 = args[3].const_pointer;
  int64_t                   time1       = args[4].int_num;
  int64_t                   time2       = args[5].int_num;
  int                       last_val    = args[6].int32_num;

  const struct FXNodeLines *fxnodeline2 = NextFXNodeLine(fxnodeline1);
  if (fxnodeline2==NULL) // Can happen if deleting a nodeline while playing.
    return DONT_RESCHEDULE;
  
  // May happen if removing patch from track while playing, not sure. Doesn't hurt to have this check.
  if (track->patch==NULL)
    return DONT_RESCHEDULE;

  R_ASSERT_RETURN_IF_FALSE2(fx->patch!=NULL, DONT_RESCHEDULE);
  
  int val1 = fxnodeline1->val;
  int val2 = fxnodeline2->val;
  
  int x;
  FX_when when;
  
  if (time==time1) {
    
    when = FX_start;
    x = val1;
    
  } else if (time==time2) {
    
    when = FX_end;
    x = val2;
    
  } else {
    
    when = FX_middle;
    x = scale_double(time, time1, time2, val1, val2);
    
  }

  bool enabled = track->onoff==1 || root->song->mute_editor_automation_when_track_is_muted==false;
    
  if(enabled)
    if (when!=FX_middle || x != last_val){   // Note: We don't check if last value was similar when sending out FX_end. FX_start and FX_end is always sent to the instrument.

      //printf("   scheduler_fxs.c. Sending out %d at %d (%d)\n",x,fxnodeline1->l.p.line, (int)time);
      
      RT_FX_treat_fx(seqtrack, fx, x, time, 0, when);
      /*
        float *slider_automation_value = ATOMIC_GET(fx->slider_automation_value);
        if(slider_automation_value!=NULL)
        safe_float_write(slider_automation_value, scale_double(x,fx->min,fx->max,0.0f,1.0f));
        
        enum ColorNums *slider_automation_color = ATOMIC_GET(fx->slider_automation_color);    
        if(slider_automation_color!=NULL)
        __atomic_store_n(slider_automation_color, fx->color, __ATOMIC_SEQ_CST);
      */
    }
  
  if (time >= time2 /* || enabled==false */ ) { // If we check "when==FX_end" instead, we go into an infinte loop if time==time1==time2.
    
    RT_schedule_fxnodeline(seqtrack, seqblock, track, fx, fxnodeline2, fxnodeline2->l.p);
    return DONT_RESCHEDULE;
    
  } else {

    args[6].int32_num = x;

    if (fxnodeline1->logtype == LOGTYPE_HOLD)
      return time2;
    else if (enabled==false)
      return R_MIN(time + 25*pc->pfreq/1000, time2); // 25ms.
    else
      return R_MIN(time + RADIUM_BLOCKSIZE, time2);
  }
    
}



static void RT_schedule_fxnodeline(
                                   struct SeqTrack *seqtrack,
                                   const struct SeqBlock *seqblock,
                                   const struct Tracks *track,
                                   struct FX *fx,
                                   const struct FXNodeLines *fxnodeline1,
                                   Place start_place
                                   )
{
  R_ASSERT_RETURN_IF_FALSE(fxnodeline1!=NULL);

  const struct FXNodeLines *fxnodeline2 = NextFXNodeLine(fxnodeline1); // Might be NULL.
  if (fxnodeline2==NULL)
    return;
  
  int64_t time1 = get_seqblock_place_time2(seqblock, track, fxnodeline1->l.p);
  int64_t time2 = get_seqblock_place_time2(seqblock, track, fxnodeline2->l.p);

  int64_t time;
  if (PlaceEqual(&start_place, &fxnodeline1->l.p)) // This test is not really necessary, get_seqblock_place_time should always return the same value for the same place. But with this check there is no need to think about the possibility of it to fail.
    time = time1;
  else
    time = get_seqblock_place_time2(seqblock, track, start_place);
  
  union SuperType args[g_num_fx_args];
  args[0].const_pointer = seqblock;
  args[1].const_pointer = track;
  args[2].pointer       = fx;
  args[3].const_pointer = fxnodeline1;
  args[4].int_num       = time1;
  args[5].int_num       = time2;
  args[6].int32_num     = INT32_MIN;
  
  //printf(" Scheduling FX at %d. seqblock->time: %d\n",(int)time, (int)seqblock->time);
  SCHEDULER_add_event(seqtrack, time, RT_scheduled_fx, &args[0], g_num_fx_args, SCHEDULER_FX_PRIORITY);
}


void RT_schedule_fxs_newblock(struct SeqTrack *seqtrack,
                              const struct SeqBlock *seqblock,
                              int64_t start_time,
                              Place start_place)
{
  struct Tracks *track=seqblock->block->tracks;
  
  while(track!=NULL){

    int tracknum = track->l.num;

    bool doit = seqblock->track_is_disabled==NULL  // i.e. playing block
      || tracknum >= MAX_DISABLED_SEQBLOCK_TRACKS
      || !seqblock->track_is_disabled[tracknum];

    if (doit){
      
      VECTOR_FOR_EACH(struct FXs *fxs, &track->fxs){

        if (fxs->fx->is_enabled) {
          
          struct FXNodeLines *fxnodeline1 = fxs->fxnodelines;
          
          if (PlaceGreaterOrEqual(&fxnodeline1->l.p, &start_place)){
            
            RT_schedule_fxnodeline(seqtrack, seqblock, track, fxs->fx, fxnodeline1, fxnodeline1->l.p);
            
          } else {
            
            struct FXNodeLines *fxnodeline2 = NextFXNodeLine(fxnodeline1);
            
            while(fxnodeline2 != NULL){
              if (PlaceGreaterOrEqual(&start_place, &fxnodeline1->l.p) && PlaceLessThan(&start_place, &fxnodeline2->l.p)) {
                RT_schedule_fxnodeline(seqtrack, seqblock, track, fxs->fx, fxnodeline1, start_place);
                break;
              }
              fxnodeline1 = fxnodeline2;
              fxnodeline2 = NextFXNodeLine(fxnodeline2);
            }
            
          }

        }
        
      }END_VECTOR_FOR_EACH;
    }
    
    track=NextTrack(track);   
  }

}



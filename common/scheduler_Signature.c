
#include "nsmtracker.h"
#include "placement_proc.h"


#include "scheduler_proc.h"



// Called from ../audio/Juce_plugins.cpp and ../audio/Mixer.cpp
//
StaticRatio RT_Signature_get_current_Signature(const struct SeqTrack *seqtrack){
  if (is_playing())
    return seqtrack->signature_iterator.signature_value;
  else {
    if (root==NULL){ // When does this happen?
#if !defined(RELEASE)
      abort();
#endif
      return make_static_ratio(4,4);
    }else {
      StaticRatio signature = root->signature;
      if (signature.denominator<=0 || signature.denominator<=0)
        signature = make_static_ratio(4,4); // Happens during startup, and maybe when loading song.
      return signature;
    }
  }
}




static int64_t RT_scheduled_Signature(struct SeqTrack *seqtrack, int64_t time, union SuperType *args){

  const struct SeqBlock *seqblock = args[0].const_pointer;

  Signature_Iterator *iterator = &seqtrack->signature_iterator;

  const struct Signatures *signature = iterator->next_signature;
  
  iterator->signature_value = signature->signature;
  iterator->next_signature  = NextSignature(signature);

  //printf("   SIG %d/%d\n",iterator->signature_value.numerator, iterator->signature_value.denominator);
  
  // Schedule next signature
  if (iterator->next_signature != NULL)
    return get_seqblock_place_time(seqblock, iterator->next_signature->l.p);
  else
    return DONT_RESCHEDULE;
}




void RT_schedule_Signature_newblock(struct SeqTrack *seqtrack,
                                    const struct SeqBlock *seqblock,
                                    const Place start_place)
{
  R_ASSERT_RETURN_IF_FALSE(seqblock->block != NULL);
  
  Signature_Iterator *iterator = &seqtrack->signature_iterator;
  memset(iterator, 0, sizeof(Signature_Iterator));
  iterator->signature_value = root->signature;
  //printf("   SIG Init %d/%d\n",iterator->signature_value.numerator, iterator->signature_value.denominator);
  
  const struct Blocks *block = seqblock->block;
    
  const struct Signatures *next_signature = block->signatures;

  if(next_signature==NULL)
    return;

  // spool forward to the 'signature' that is used by 'start_place'
  //
  while(PlaceGreaterThan(&start_place, &next_signature->l.p)){
    iterator->signature_value = next_signature->signature;
    //printf("   SIG Init %d/%d\n",iterator->signature_value.numerator, iterator->signature_value.denominator);
    next_signature = NextSignature(next_signature);
    if (next_signature==NULL)
      return;
  }

  iterator->next_signature = next_signature;
  
  {
    int num_args = 1;
    union SuperType args[num_args];
    args[0].const_pointer = seqblock;
    
    int64_t time = get_seqblock_place_time(seqblock, next_signature->l.p);
    
    SCHEDULER_add_event(seqtrack, time, RT_scheduled_Signature, &args[0], num_args, SCHEDULER_SIGNATURE_PRIORITY);
  }
}

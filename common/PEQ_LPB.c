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

#include <string.h>

#include "nsmtracker.h"
#include "playerclass.h"
#include "time_proc.h"
#include "PEQmempool_proc.h"
#include "PEQcommon_proc.h"
#include "placement_proc.h"

#include "PEQ_LPB_proc.h"

extern PlayerClass *pc;

extern struct Root *root;

typedef struct {
  struct LPBs *lpb;

  double num_beats_played_so_far;

  STime time1;
  STime time2;

  Place place1;
  Place place2;
  
  int lpb_value;
  double num_beats_between_time1_and_time2;
} LPB_iterator;


static LPB_iterator g_lpb_iterator = {0};


// Returns number of beats played so far.
//
// Only called from the juce plugin host process method, which should be called from the scheduler. This means that PlayerNewLPB should always have been called before this function, if playing.
double RT_LPB_get_beat_position(void){
  LPB_iterator *iterator = &g_lpb_iterator;

  
  if (pc->isplaying==false)    
    return 0.0f;

  STime time = pc->start_time - pc->seqtime;

  R_ASSERT(pc->end_time-pc->seqtime >= iterator->time1);
  R_ASSERT(time < iterator->time2);
  
  //if (time < iterator->time1)
  //  time = iterator->time1;

  // Note: time (i.e. pc->start_time) may be lower than time1. Handle that situation correct is necessary to get correct timing.
  
  return  
    iterator->num_beats_played_so_far +
    scale_double(time,
                 iterator->time1, iterator->time2,
                 0.0, iterator->num_beats_between_time1_and_time2
                 );
}

static void print_lpb_iterator_status(const struct Blocks *block){
#if 0
  LPB_iterator *iterator = &g_lpb_iterator;
  printf("\n\ntime: %d -> %d (%d)\nlpb_value: %d\n",(int)iterator->time1,(int)iterator->time2,block==NULL?-1:(int)getBlockSTimeLength(block),iterator->lpb_value);
  printf("num_beats_so_far: %f", iterator->num_beats_played_so_far);
  printf("num_beats_between: %f\n\n",iterator->num_beats_between_time1_and_time2);
  printf("beat_position: %f\n\n",RT_LPB_get_beat_position());
#endif
}


static void Spool_LPB_iterator_to_place(LPB_iterator *iterator, Place *place){
}

#define SetAbsoluteLastPlace(place, block) do{        \
    Place *p = place;                                 \
    p->line = block->num_lines;                       \
    p->counter = 0;                                   \
    p->dividor = 1;                                   \
  }while(0)

static void InitPEQ_LPB_new_block(const struct Blocks *block, LPB_iterator *iterator){
  struct LPBs *lpb = block->lpbs;

  iterator->time1 = 0;
  PlaceSetFirstPos(&iterator->place1);
    
  if (lpb==NULL) {

    iterator->lpb_value = root->lpb;
    iterator->time2 = getBlockSTimeLength(block);
    SetAbsoluteLastPlace(&iterator->place2, block);
      
  } else if (PlaceIsFirstPos(&lpb->l.p)){

    iterator->lpb_value = lpb->lpb;

    lpb = NextLPB(lpb);

    if (lpb==NULL) {
      iterator->time2 = getBlockSTimeLength(block);
      SetAbsoluteLastPlace(&iterator->place2, block);
    } else {
      iterator->time2 = Place2STime(block, &lpb->l.p);
      iterator->place2 = lpb->l.p;
    }
    
  } else {

    iterator->lpb_value = root->lpb;
    iterator->time2 = Place2STime(block, &lpb->l.p);
    iterator->place2 = lpb->l.p;
  }

  iterator->lpb = lpb;
}


static void PlayerNextLPB_Block(struct PEventQueue *peq,int doit);
static void PlayerNextLPB(struct PEventQueue *peq, int doit);

static void InsertNextLPB_PEQ(struct PEventQueue *peq, LPB_iterator *iterator){
  if (iterator->lpb==NULL) {

    peq->TreatMe=PlayerNextLPB_Block;
    peq->block = PC_GetPlayBlock(1);
    
    if (peq->block == NULL)
      ReturnPEQelement(peq);
    
    else {
      
      PC_InsertElement_a( // need to use the "_a" version so that the block has a chance to update first. (there should be an "_aa" version for the block. Now fx/etc. can be called before lpb block change)
                         peq,
                         1,
                         0
                          );
    }
    
  } else {

    peq->TreatMe=PlayerNextLPB;
    
    PC_InsertElement(
                     peq,
                     0,
                     iterator->time2
                     );

  }

  iterator->num_beats_played_so_far += iterator->num_beats_between_time1_and_time2;
  
  iterator->num_beats_between_time1_and_time2 = (GetfloatFromPlace(&iterator->place2) - GetfloatFromPlace(&iterator->place1)) / (double)iterator->lpb_value;
  
  print_lpb_iterator_status(peq->block);
}

void InitPEQ_LPB(struct Blocks *block,Place *place){
  LPB_iterator *iterator = &g_lpb_iterator;
  memset(iterator, 0, sizeof(LPB_iterator));
  
  InitPEQ_LPB_new_block(block, iterator);
    
  Spool_LPB_iterator_to_place(iterator, place);

  struct PEventQueue *peq = GetPEQelement();
  
  peq->TreatMe=PlayerNextLPB;
  peq->block=block;

  InsertNextLPB_PEQ(peq, iterator);
}

static void PlayerNextLPB_Block(struct PEventQueue *peq,int doit){
  LPB_iterator *iterator = &g_lpb_iterator;

  //printf("PlayerNextBlock %d (%d)\n",(int)peq->l.time,(int)pc->start_time);
  
  InitPEQ_LPB_new_block(peq->block, iterator);
  InsertNextLPB_PEQ(peq, iterator);
}

static void PlayerNextLPB(struct PEventQueue *peq,int doit){
  //printf("PlayerNext %d (%d)\n",(int)peq->l.time,(int)pc->start_time);
    
  LPB_iterator *iterator = &g_lpb_iterator;

  struct LPBs *lpb = iterator->lpb;
  R_ASSERT(lpb!=NULL);
  
  iterator->time1 = iterator->time2;
  iterator->place1 = iterator->place2;
  
  iterator->lpb_value = lpb->lpb;

  lpb = NextLPB(lpb);

  if (lpb==NULL) {
    iterator->time2 = getBlockSTimeLength(peq->block);
    SetAbsoluteLastPlace(&iterator->place2, peq->block);
  } else {
    iterator->time2 = Place2STime(peq->block, &lpb->l.p);
    iterator->place2 = lpb->l.p;
  }

  iterator->lpb = lpb;
  
  InsertNextLPB_PEQ(peq, iterator);
}


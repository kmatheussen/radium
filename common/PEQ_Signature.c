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

#include "PEQ_Signature_proc.h"

extern PlayerClass *pc;

extern struct Root *root;

static Ratio g_signature_value = {4,4};
static struct Signatures *g_signature = NULL;

Ratio RT_Signature_get_current_Signature(void){
  if (ATOMIC_GET(pc->isplaying))
    return g_signature_value;
  else {
    if (root==NULL || root->song==NULL || root->song->tracker_windows==NULL || root->song->tracker_windows->wblock==NULL || root->song->tracker_windows->wblock->block==NULL)
      return ratio(4,4);
    else
      return root->signature;
  }
}


static void InitPEQ_Signature_new_block(const struct Blocks *block){
  g_signature = block->signatures;
      
  if (g_signature!=NULL && PlaceIsFirstPos(&g_signature->l.p))
    g_signature_value = g_signature->signature;
  else
    g_signature_value = root->signature;
}


static void PlayerNextSignature_Block(struct PEventQueue *peq,int doit);
static void PlayerNextSignature(struct PEventQueue *peq, int doit);

static void InsertNextSignature_PEQ(struct PEventQueue *peq){
  if (g_signature==NULL) {

    peq->TreatMe=PlayerNextSignature_Block;
    peq->block = PC_GetPlayBlock(1);
    
    if (peq->block == NULL)
      ReturnPEQelement(peq);
    
    else {
      
      PC_InsertElement_a( // need to use the "_a" version so that the block has a chance to update first. (there should be an "_aa" version for the block. Now fx/etc. can be called before signature block change)
                         peq,
                         1,
                         0
                          );
    }
    
  } else {

    peq->TreatMe=PlayerNextSignature;
    
    PC_InsertElement2(
                     peq,
                     0,
                     &g_signature->l.p
                     );

  }
}

void InitPEQ_Signature(struct Blocks *block,Place *place){
  InitPEQ_Signature_new_block(block);
    
  struct PEventQueue *peq = GetPEQelement();
  
  peq->TreatMe=PlayerNextSignature;
  peq->block=block;

  InsertNextSignature_PEQ(peq);
}

static void PlayerNextSignature_Block(struct PEventQueue *peq,int doit){
  //printf("PlayerNextBlock %d (%d)\n",(int)peq->l.time,(int)pc->start_time);
  
  InitPEQ_Signature_new_block(peq->block);
  InsertNextSignature_PEQ(peq);
}

static void PlayerNextSignature(struct PEventQueue *peq,int doit){
  R_ASSERT_RETURN_IF_FALSE(g_signature != NULL);

  //printf("Player Next signature\n");
  
  g_signature_value = g_signature->signature;

  g_signature = NextSignature(g_signature);

  InsertNextSignature_PEQ(peq);
}


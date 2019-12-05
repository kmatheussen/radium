
#include "nsmtracker_proc.h"
#include "blts_proc.h"

Blt *bltsroot=NULL;

Blt *Blt_getBlt(){
  Blt *ret;
  if(bltsroot==NULL){
    return talloc_atomic(sizeof(struct Blts));
  }

  ret=bltsroot;
  bltsroot=ret->next;

  return ret;
}

void Blt_retBlt(Blt *blt){
  blt->next=bltsroot;
  bltsroot=blt;
}


/* Take away unnecesarry lines and tracks from blt. */

#define BLTCONT() curr=curr->next;continue

bool Blt_squize(struct Tracker_Windows *window,Blt *blt){
  Blt *curr=window->blts,*prev,*temp,*new;

  /* Check if any of the tboxes totally covers the blt and return false if. */
  while(curr!=NULL){
    if(TBoxInsideTBox(blt,curr)==true) return false;
    curr=curr->next;
  }

  /* Check if blt totally covers any other tboxes, and remove them. */
  curr=window->blts;
  prev=NULL;
  while(curr!=NULL){
    if(TBoxInsideTBox(blt,curr)==true){
      temp=curr->next;
      Blt_ret(curr);
      if(prev==NULL){
	window->blts=temp;
      }else{
	prev->next=temp;
      }
      curr=temp;
    }else{
      prev=curr;
      curr=curr->next;
    }
  }

  /* Check if blt overlap any tboxes partly, and return true if not. */
  curr=window->blts;
  while(curr!=NULL){
    if(TBoxOverlapTBox(curr,blt)==true) break;
    curr=curr->next;
  }
  if(curr==NULL) return true;

  /* We now know that blt overlap curr partly.*/

  /* First lets see if the overlap only covers the y direction. */
  if(TBoxOnlyOverLapBox_Y(curr,blt)==true){
    TBoxSquize_Y(curr,blt);
    temp=curr->next;
    Blt_ret(curr);
    if(prev==NULL){
      window->blts=temp;
    }else{
      prev->next=temp;
    }
    return Blt_Squize(window,blt);
  }

  /* No, then lets see if the overlap only covers the x direction. */
  if(TBoxOnlyOverLapBox_X(curr,blt)==true){
    TBoxSquize_Y(curr,blt);
    temp=curr->next;
    Blt_ret(curr);
    if(prev==NULL){
      window->blts=temp;
    }else{
      prev->next=temp;
    }
    return Blt_Squize(window,blt);
  }

  /* Not an only x or y overlap, so a new blt has to be made. */
  new=Blt_get();
  TBoxMake2Into3(curr,blt,new);

  return true;
}

void Blt_Mark(
	      struct *Tracker_Windows *window,
	      NInt starttrack,
	      NInt endtrack,
	      int startrealline,
	      int endrealline
	      )
{
  Blt *blt=Blt_getBlt();

  blt->starttrack=starttrack;
  blt->endtrack=endtrack;
  blt->startrealline=startrealline;
  blt->endrealline=endrealline;

  Blt_squize(window,blt);

  if(blt->starttrack==blt->endtrack || blt->startrealline==blt->endrealline){
    Blt_retBlt(blt);
    return;
  }

  blt->next=window->blts;
  window->blts=blt;
}

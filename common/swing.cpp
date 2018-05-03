/* Copyright 2017 Kjetil S. Matheussen

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


#include "nsmtracker.h"
#include "list_proc.h"
#include "placement_proc.h"
#include "realline_calc_proc.h"
#include "undo_swings_proc.h"
#include "time_proc.h"

#include "../api/api_timing_proc.h"

#include "swing_proc.h"


QVector<Swing*> Swings_get(const struct WBlocks *wblock, const struct Tracks *track, int realline){
  QVector<Swing*> ret;
  
  struct Swing *swing=track==NULL ? wblock->block->swings : track->swings;
  int realline2 = 0;
  
  while(swing!=NULL){
    realline2=FindRealLineFor(wblock,realline2,&swing->l.p);
    if(realline2>realline)
      break;
    if(realline2==realline)
      ret.push_back(swing);
    swing = NextSwing(swing);
  }
  
  return ret;
}


#if 0 // Add auto-swings in scheme instead.
// If a bar has any swing value, make sure the bar start also have a swing value.
static void add_missing_swing_in_bar(const struct Blocks *block, const struct Beats *bar, const struct Swing *swing, QVector<Swing*> &new_swings){
  const Place bar_start = bar->l.p;

  const struct Beats *next_bar = bar;
  while(next_bar!=NULL && next_bar->bar_num==bar->bar_num)
    next_bar=NextBeat(next_bar);

  const Place bar_end = next_bar==NULL ? p_Absolute_Last_Pos(block) : next_bar->l.p;

  bool has_swing_start = false;

  while(swing!=NULL){
    if (p_Equal(swing->l.p, bar_start)){

      R_ASSERT(has_swing_start==false);
      has_swing_start = true;
      
    } else if (p_Greater_Or_Equal(swing->l.p, bar_end)){

      R_ASSERT_RETURN_IF_FALSE(next_bar!=NULL);
      add_missing_swing_in_bar(block, next_bar, swing, new_swings);
      return;
      
    } else if (has_swing_start==false) {
      struct Swing *new_swing = (struct Swing*)talloc(sizeof(struct Swing));
      new_swing->l.p = bar_start;
      new_swing->weight = 4;
      new_swing->logtype = LOGTYPE_HOLD;
      new_swings.push_back(new_swing);

      has_swing_start = true;
    }

    swing = NextSwing(swing);
  }

}
#endif

static void legalize_swings(struct Blocks *block, struct Swing **swings){
  /*
  QVector<Swing*> new_swings;

  add_missing_swing_in_bar(block, block->beats, *swings, new_swings);

  for(auto *swing : new_swings)
    ListAddElement3(swings, &swing->l);
  */
}

// TODO: Automatically add swing to first beat in bar, if it doesn't exist.
void AddSwing(struct Blocks *block, struct Tracks *track, const Place place, int weight, int logtype){
  struct Swing **swings = track==NULL ? &block->swings : &track->swings;
  
  struct Swing *swing = (struct Swing*)ListFindElement3(LCAST(*swings),&place);

  bool already_there = swing!=NULL && p_Equal(swing->l.p, place);

  //printf("Adding place %s. Old: %s. Already_there: %d\n", PlaceToString(&place), swing==NULL ? "NULL" : PlaceToString(&swing->l.p), already_there);

  if (!already_there){
    swing = (struct Swing*)talloc(sizeof(struct Swing));
    swing->l.p = place;
  }
  
  swing->weight = weight;
  swing->logtype = logtype;

  if (!already_there) {
    ListAddElement3(swings, &swing->l);
    legalize_swings(block, swings);
  }

  TIME_block_swings_have_changed(block);
}

void RemoveSwing(struct Blocks *block,struct Tracks *track, struct Swing *swing){
  struct Swing **swings = track==NULL ? &block->swings : &track->swings;
  
  ListRemoveElement3(swings,&swing->l);
  legalize_swings(block, swings);

  TIME_block_swings_have_changed(block);
}

static void RemoveSwings(struct Blocks *block,struct Tracks *track,Place *p1,Place *p2){
  struct Swing **swings = track==NULL ? &block->swings : &track->swings;
  
  ListRemoveElements3(swings,p1,p2);
  legalize_swings(block, swings);

  TIME_block_swings_have_changed(block);
}


void RemoveSwingCurrPos(struct Tracker_Windows *window){
  
	struct WBlocks *wblock=window->wblock;
	int curr_realline=wblock->curr_realline;

	Place p1,p2;

	ADD_UNDO(Swings_CurrPos(window));

	PlaceSetReallinePlace(wblock,curr_realline,&p1);
	PlaceSetReallinePlace(wblock,curr_realline+1,&p2);

	RemoveSwings(wblock->block,NULL,&p1,&p2);

        window->must_redraw_editor = true;
}

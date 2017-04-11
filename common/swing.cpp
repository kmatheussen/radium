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


QVector<Swing*> Swings_get(const struct WBlocks *wblock, int realline){
  QVector<Swing*> ret;
  
  struct Swing *swing=wblock->block->swings;
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

void UpdateSwings(struct Blocks *block){
  block->filledout_swings = API_createFilledoutSwings(block);
  UpdateSTimes(block);
}

void AddSwing(struct Blocks *block, const Place place, int weight, int logtype){
  struct Swing *swing = (struct Swing*)ListFindElement3(&block->swings->l,&place);

  bool already_there = swing!=NULL && p_Equal(swing->l.p, place);

  if (!already_there){
    swing = (struct Swing*)talloc(sizeof(struct Swing));
    swing->l.p = place;
  }
  
  swing->weight = weight;
  swing->logtype = logtype;

  if (!already_there)
    ListAddElement3(&block->swings, &swing->l);

  UpdateSwings(block);
  //updatewhat?();
}

void RemoveSwing(struct Blocks *block,struct Swing *swing){
  ListRemoveElement3(&block->swings,&swing->l);

  UpdateSwings(block);
  //updatewhat?();
}

static void RemoveSwings(struct Blocks *block,Place *p1,Place *p2){
  ListRemoveElements3(&block->swings,p1,p2);

  UpdateSwings(block);
  //updatewhat?();
}


void RemoveSwingCurrPos(struct Tracker_Windows *window){
	struct WBlocks *wblock=window->wblock;
	int curr_realline=wblock->curr_realline;

	Place p1,p2;

	ADD_UNDO(Swings_CurrPos(window));

	PlaceSetReallinePlace(wblock,curr_realline,&p1);
	PlaceSetReallinePlace(wblock,curr_realline+1,&p2);

	RemoveSwings(wblock->block,&p1,&p2);

        window->must_redraw_editor = true;
}

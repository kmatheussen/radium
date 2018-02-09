/* Copyright 2012 Kjetil S. Matheussen

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



#include "../common/nsmtracker.h"
#include "../common/undo.h"

#include "../audio/SoundPlugin.h"

#include "QM_chip.h"

#include "undo_chip_position_proc.h"


extern struct Root *root;


struct Undo_ChipPos{
  const struct Patch *patch;
  float x;
  float y;
};

static void *Undo_Do_ChipPos(
                                 struct Tracker_Windows *window,
                                 struct WBlocks *wblock,
                                 struct WTracks *wtrack,
                                 int realline,
                                 void *pointer
                                 );

static void Undo_ChipPos(
                             struct Tracker_Windows *window,
                             struct WBlocks *wblock,
                             const struct Patch *patch
                             )
{
  struct Undo_ChipPos *undo_ae=talloc(sizeof(struct Undo_ChipPos));
  
  undo_ae->patch = patch;
  undo_ae->x = CHIP_get_pos_x(patch);
  undo_ae->y = CHIP_get_pos_y(patch);


  printf("********* Storing chipos undo. value: %f\n",undo_ae->x);

  Undo_Add_dont_stop_playing(
                             window->l.num,
                             wblock->l.num,
                             wblock->wtrack->l.num,
                             wblock->curr_realline,
                             undo_ae,
                             Undo_Do_ChipPos,
                             talloc_format("Chip position %s: %f %f",patch->name, undo_ae->x, undo_ae->y)
                             );
}

void ADD_UNDO_FUNC(ChipPos_CurrPos(const struct Patch *patch)){
  struct Tracker_Windows *window = root->song->tracker_windows;

  Undo_ChipPos(window,window->wblock, patch);
}

static void *Undo_Do_ChipPos(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
){

  struct Undo_ChipPos *undo_ae=pointer;

  float new_x = CHIP_get_pos_x(undo_ae->patch);
  float new_y = CHIP_get_pos_y(undo_ae->patch);

  //printf("Calling Undo_do for %d. Current value: %f. Now setting it back to %f\n",undo_ae->effect_num,new_value,undo_ae->value);

  CHIP_set_pos(undo_ae->patch,undo_ae->x,undo_ae->y);

  undo_ae->x = new_x;
  undo_ae->y = new_y;

  return undo_ae;
}


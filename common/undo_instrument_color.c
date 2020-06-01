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
#include "../common/patch_proc.h"
#include "../common/OS_Player_proc.h"

#include "../api/api_instruments_proc.h"
#include "../api/api_proc.h"

#include "undo_instrument_color_proc.h"


extern struct Root *root;

struct Undo_InstrumentColor{
  instrument_t instrument_id;

  const char *color;
};


static void *Undo_Do_InstrumentColor(
                                 struct Tracker_Windows *window,
                                 struct WBlocks *wblock,
                                 struct WTracks *wtrack,
                                 int realline,
                                 void *pointer
                                 );

static void Undo_InstrumentColor(
                                 struct Tracker_Windows *window,
                                 struct WBlocks *wblock,
                                 instrument_t instrument_id
                                 )
{
  struct Undo_InstrumentColor *undo_ae=talloc(sizeof(struct Undo_InstrumentColor));

  if (!isLegalInstrument(instrument_id)){
    struct Patch *patch = PATCH_get_current();
    R_ASSERT_RETURN_IF_FALSE(patch!=NULL);
    instrument_id = patch->id;
  }
      
  undo_ae->instrument_id = instrument_id;

  undo_ae->color = getInstrumentColor(instrument_id, true);


  //printf("********* Storing eff undo. value: %f %d\n",undo_ae->value,plugin->comp.is_on);

  Undo_Add_dont_stop_playing(
                             window->l.num,
                             wblock->l.num,
                             wblock->wtrack->l.num,
                             wblock->curr_realline,
                             undo_ae,
                             Undo_Do_InstrumentColor,
                             talloc_format("Undo instrument color %d -> %s",(int)instrument_id.id, undo_ae->color)
                             );

}

void ADD_UNDO_FUNC(InstrumentColor(instrument_t instrument_id)){
  struct Tracker_Windows *window = root->song->tracker_windows;
  //printf("Undo_InstrumentColor_CurrPos\n");
  Undo_InstrumentColor(window,window->wblock, instrument_id);
}

static void *Undo_Do_InstrumentColor(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
){

  struct Undo_InstrumentColor *undo_ae=pointer;

  const char *now_color = getInstrumentColor(undo_ae->instrument_id, true);
  
  API_setInstrumentColor(undo_ae->color, undo_ae->instrument_id, false);

  undo_ae->color = now_color;

  return undo_ae;
}


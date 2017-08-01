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
#include "../common/OS_Player_proc.h"

#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"

#include "../api/api_proc.h"

#include "undo_connection_enabled_proc.h"


extern struct Root *root;

struct Undo_ConnectionEnabled{
  struct Patch *source;
  struct Patch *target;

  bool enabled;
};


static void *Undo_Do_ConnectionEnabled(
                                 struct Tracker_Windows *window,
                                 struct WBlocks *wblock,
                                 struct WTracks *wtrack,
                                 int realline,
                                 void *pointer
                                 );

static void Undo_ConnectionEnabled(
                                       struct Tracker_Windows *window,
                                       struct WBlocks *wblock,
                                       struct Patch *source,
                                       struct Patch *target
                                       )
{
  struct Undo_ConnectionEnabled *undo_ae=talloc(sizeof(struct Undo_ConnectionEnabled));
  
  undo_ae->source = source;
  undo_ae->target = target;

  undo_ae->enabled = getConnectionEnabled(source->id, target->id);


  //printf("********* Storing eff undo. value: %f %d\n",undo_ae->value,plugin->comp.is_on);

  Undo_Add_dont_stop_playing(
                             window->l.num,
                             wblock->l.num,
                             wblock->wtrack->l.num,
                             wblock->curr_realline,
                             undo_ae,
                             Undo_Do_ConnectionEnabled,
                             talloc_format("Undo connection enabled %s -> %s",source->name, target->name)
                             );

}

void ADD_UNDO_FUNC(ConnectionEnabled_CurrPos(struct Patch *source, struct Patch *target)){
  struct Tracker_Windows *window = root->song->tracker_windows;
  //printf("Undo_ConnectionEnabled_CurrPos\n");
  Undo_ConnectionEnabled(window,window->wblock, source, target);
}

static void *Undo_Do_ConnectionEnabled(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	void *pointer
){

  struct Undo_ConnectionEnabled *undo_ae=pointer;

  bool now_enabled = getConnectionEnabled(undo_ae->source->id, undo_ae->target->id);

  setConnectionEnabled(undo_ae->source->id, undo_ae->target->id, undo_ae->enabled, true);

  undo_ae->enabled = now_enabled;

  return undo_ae;
}


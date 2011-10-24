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


#include <stdio.h>
#include <stdlib.h>

#include "nsmtracker.h"
#include "list_proc.h"
#include "notes_proc.h"
#include "nodelines_proc.h"

#include "tracks_proc.h"




extern struct Root *root;

void CloseTrack(struct Blocks *block, NInt tracknum){
	struct Tracks *temp=(struct Tracks *)ListFindElement1(&block->tracks->l,tracknum);

	ListRemoveElement1(&block->tracks,&temp->l);
}


void NewTrack(struct Blocks *block,struct Tracks *track){
	track->onoff=1;
	track->trackname="NTN";
	track->instrument=root->def_instrument;
	track->volume=800;
	track->panonoff=false;
	track->volumeonoff=true;
	ListAddElement1(&block->tracks,&track->l);
	(*track->instrument->SelectTrackInstrument)(track,track->instrument);
}

void AppendTrack(struct Blocks *block){
	int tracknum=ListFindFirstFreePlace1(&block->tracks->l);

	struct Tracks *temp=talloc(sizeof(struct Tracks));

	debug("tracks.c,AppendTrack,begin. temp:%x\n",temp);

	temp->l.num=tracknum;
	NewTrack(block,temp);
}



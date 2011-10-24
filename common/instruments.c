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


#include "nsmtracker.h"
#include "list_proc.h"

#include "instruments_proc.h"



extern struct Root *root;

void CloseInstrument(NInt instrumentnum){
	struct Instruments *temp=(struct Instruments *)ListFindElement1(
		&root->song->instruments->l,
		instrumentnum
	);
	if(temp==NULL) return;

	(*temp->CloseInstrument)(temp);

	ListRemoveElement1(&root->song->instruments,&temp->l);
}

void CloseAllInstruments(void){
	if(root==NULL) return;
	if(root->song==NULL) return;

	while(root->song->instruments!=NULL)
		CloseInstrument(root->song->instruments->l.num);
}



extern int InitInstrumentPlugIn(struct Instruments *instrument);


/* This function must be made more general later. */
bool OpenInstrument(void){
	struct Instruments *instrument=talloc(sizeof(struct Instruments));
	instrument->l.num=ListFindFirstFreePlace1(&root->song->instruments->l);

	if(InitInstrumentPlugIn(instrument)==INSTRUMENT_FAILED) return false;

	ListAddElement1(&root->song->instruments,&instrument->l);
	root->def_instrument=instrument;

	return true;
}




void StopAllInstruments(void){
	struct Instruments *instrument=root->song->instruments;

	while(instrument!=NULL){
		(*instrument->StopPlaying)(instrument);
		instrument=NextInstrument(instrument);
	}

}


void InitAllInstrumentsForPlaySongFromStart(void){
	struct Instruments *instrument=root->song->instruments;

	while(instrument!=NULL){
		if(instrument->PlayFromStartHook==NULL){
			RError("Error in function 'InitAllInstrumentsForPlaySongFromStart' in file 'instruments.c'\n");
			return;
		}
		(*instrument->PlayFromStartHook)(instrument);
		instrument=NextInstrument(instrument);
	}
}


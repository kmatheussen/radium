/* Copyright 2001 Kjetil S. Matheussen

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
#include "../common/placement_proc.h"
#include "../common/block_properties_proc.h"
#include "../common/undo_blocks_proc.h"
#include "../common/undo_notes_proc.h"
#include "../common/notes_proc.h"
#include "../common/player_proc.h"
#include "../common/wtracks_proc.h"
#include "../common/list_proc.h"
#include "../common/notes_legalize_proc.h"
#include "../common/windows_proc.h"


#include "ad_noteadd_proc.h"


extern struct Root *root;


//int AD_noteAdds_trackCmp(const struct NoteAdds_track *n1,const struct NoteAdds_track *n2){
int AD_noteAdds_trackCmp(const void *n1,const void *n2){
	if(((struct NoteAdds_track *)n1)->place < ((struct NoteAdds_track *)n2)->place) return -1;
	if(((struct NoteAdds_track *)n1)->place > ((struct NoteAdds_track *)n2)->place) return 1;
	return 0;
}

float AD_NoteAdds_findmax_sort(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct NoteAdds_track_do *nats_do
){
	struct NoteAdds_track *nats=nats_do->nats;
	struct NoteAdds_track *nat;
	int lokke;
	float maxplace=-1.0;

	if(nats_do->sort==1){
		qsort(
			nats,
			nats_do->num_nats,
			sizeof(struct NoteAdds_track),
			AD_noteAdds_trackCmp
		);

		nat=&nats[nats_do->num_nats-1];
		maxplace=nat->place;
		if(nat->endplace>=0.0f){
			maxplace=nat->endplace;
		}
	}else{
		for(lokke=0;lokke<nats_do->num_nats;lokke++){
			nat=&nats[lokke];
			if(maxplace<nat->place){
				maxplace=nat->place;
			}
			if(maxplace<nat->endplace){
				maxplace=nat->endplace;
			}
		}
	}
	return maxplace;
}


void AD_installNoteAdds_track(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int num_nats,
	struct NoteAdds_track *nats,
	float startplace
){

	struct NoteAdds_track *nat;
	struct NoteAdds_track *next=NULL;
	int lokke;
	struct Notes *note;

	int standardvel=root->standardvel;

	int maxvel=MAX_VELOCITY;


	for(lokke=0;lokke<num_nats;lokke++){
		nat=&nats[lokke];
		if(lokke<num_nats-1){
			next=&nats[lokke+1];
		}

		note=NewNote();

		Float2Placement((float)(nat->place+startplace),&note->l.p);

		if(nat->endplace<0.0f){
			if(lokke<num_nats-1){
				Float2Placement((float)(next->place+startplace),&note->end);		
			}else{
				PlaceSetLastPos(wblock->block,&note->end);
				note->noend=1;
			}
		}else{
			Float2Placement((float)(nat->endplace+startplace),&note->end);		
		}

		if(nat->volume<0.0f){
			note->velocity_end=note->velocity=standardvel;
		}else{
			note->velocity_end=note->velocity=R_MIN(maxvel,maxvel*nat->volume);
		}

		note->note=R_BOUNDARIES(nat->notenum,1,127);

		ListAddElement3(&wtrack->track->notes,&note->l);
	}

	LegalizeNotes(wblock->block,wtrack->track);
}


void AD_installNoteAdds_track_do(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct NoteAdds_track_do *nats_do
){
	struct WTracks *wtrack;
	Place lastplace;
	bool undo=false;
	Place maxp;
	int num_lines=wblock->block->num_lines;
	NInt num_tracks=wblock->block->num_tracks;
	bool dochangeblockproperties=false;

	float maxplace=AD_NoteAdds_findmax_sort(window,wblock,nats_do);

	PlayStop();

	if(nats_do->startplace<0.0f){
		nats_do->startplace=GetfloatFromPlace(&wblock->reallines[wblock->curr_realline]->l.p);
	}

	Float2Placement((float)(maxplace+nats_do->startplace),&maxp);

	PlaceSetLastPos(wblock->block,&lastplace);

	if(nats_do->tracknum>=wblock->block->num_tracks){
		undo=true;
		dochangeblockproperties=true;
		num_tracks=nats_do->tracknum+1;
	}

	if(PlaceGreaterOrEqual(&maxp,&lastplace)){
		if(undo==false){
			Undo_Block(window,wblock,wblock->wtrack,wblock->curr_realline);
		}
		num_lines=maxp.line+1;
		dochangeblockproperties=true;
	}

	if(dochangeblockproperties==true){
		Undo_Block(window,wblock,wblock->wtrack,wblock->curr_realline);
		Block_Properties(
			wblock->block,
			num_tracks,
			num_lines
		);
		undo=true;
	}

	if(nats_do->tracknum==-1){
		wtrack=wblock->wtrack;
	}else{
		wtrack=ListFindElement1_num(&wblock->wtracks->l,nats_do->tracknum);
	}

	if(undo==false){
		Undo_Notes(window,wblock->block,wtrack->track,wblock->curr_realline);
	}

	AD_installNoteAdds_track(
		window,
		wblock,
		wtrack,
		nats_do->num_nats,
		nats_do->nats,
		nats_do->startplace
	);

        #if 0
	if(dochangeblockproperties==true){
		UpdateSomeTrackReallines(window,wblock,wtrack->l.num,wtrack->l.num);
		DrawUpTrackerWindow(window);
	}else{
		UpdateAndClearSomeTrackReallinesAndGfxWTracks(
			window,
			wblock,
			wtrack->l.num,
			wtrack->l.num
		);
	}
        #endif
}


void AD_insertNoteAdds_block_do(
	struct Tracker_Windows *window,
	struct NoteAdds_block *nab
){

	struct WTracks *wtrack;
	Place lastplace;
	Place maxp;
	int num_lines;
	NInt num_tracks;
	bool dochangeblockproperties=false;
	NInt maxtrack=-1,itemp;
	int lokke;
	float maxplace,ftemp;

	struct WBlocks *wblock;

	if(nab->blocknum==-1){
		wblock=window->wblock;
	}else{
		wblock=(struct WBlocks *)ListFindElement1_num(&window->wblocks->l,nab->blocknum);
		if(wblock==NULL) return;
	}

	num_lines=wblock->block->num_lines;
	num_tracks=wblock->block->num_tracks;
	dochangeblockproperties=false;

	maxplace=-1;

	PlayStop();

	for(lokke=0;lokke<nab->num_nats_do;lokke++){
		ftemp=AD_NoteAdds_findmax_sort(window,wblock,nab->nats_do[lokke]);
		if(nab->nats_do[lokke]->startplace<0.0f){
			nab->nats_do[lokke]->startplace=GetfloatFromPlace(&wblock->reallines[wblock->curr_realline]->l.p);
		}
		maxplace=R_MAX(ftemp+nab->nats_do[lokke]->startplace,maxplace);
		itemp=nab->nats_do[lokke]->tracknum;
		if(itemp==-1){
			itemp=wblock->wtrack->l.num;
		}
		maxtrack=R_MAX(maxtrack,itemp);
	}

	Undo_Block(window,wblock,wblock->wtrack,wblock->curr_realline);

	Float2Placement((float)(maxplace),&maxp);

	PlaceSetLastPos(wblock->block,&lastplace);

	if(maxtrack>=wblock->block->num_tracks){
		dochangeblockproperties=true;
		num_tracks=maxtrack+1;
	}

	if(PlaceGreaterOrEqual(&maxp,&lastplace)){
		num_lines=maxp.line+1;
		dochangeblockproperties=true;
	}

	if(dochangeblockproperties==true){
		Undo_Block(window,wblock,wblock->wtrack,wblock->curr_realline);
		Block_Properties(
			wblock->block,
			num_tracks,
			num_lines
		);
	}

	for(lokke=0;lokke<nab->num_nats_do;lokke++){
		if(nab->nats_do[lokke]->tracknum==-1){
			wtrack=wblock->wtrack;
		}else{
			wtrack=ListFindElement1_num(&wblock->wtracks->l,nab->nats_do[lokke]->tracknum);
		}
		AD_installNoteAdds_track(
			window,
			wblock,
			wtrack,
			nab->nats_do[lokke]->num_nats,
			nab->nats_do[lokke]->nats,
			nab->nats_do[lokke]->startplace
		);
	}
#if 0
	if(dochangeblockproperties==true){
		UpdateSomeTrackReallines(window,wblock,0,maxtrack);
		DrawUpTrackerWindow(window);
	}else{
		UpdateAndClearSomeTrackReallinesAndGfxWTracks(
			window,
			wblock,
			0,
			maxtrack
		);
	}
#endif
}








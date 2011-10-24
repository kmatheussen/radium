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
#include "playerclass.h"
#include "PEQcommon_proc.h"
#include "placement_proc.h"
#include "PEQmempool_proc.h"
#include "PEQvelocities_proc.h"
#include "time_proc.h"
#include "list_proc.h"

#include "PEQnotes_proc.h"




extern struct Root *root;

void PE_StartNote(STime time,struct PEventQueue *peq,int doit);
void PE_StopNote(STime time,struct PEventQueue *peq,int doit);


void InitPEQendnote(
	struct Blocks *block,
	struct Tracks *track,
	struct Notes *note,
	int playlistaddpos
){
	NInt tracknum=track->l.num;
	struct PEventQueue *peq=GetPEQelement();
	int orgplaylistaddpos=playlistaddpos;

	peq->TreatMe=PE_StopNote;
	peq->block=block;
	peq->track=track;
	peq->note=note;

	if(
		note->noend==1 &&
		note->end.line==block->num_lines-1 &&
		note->end.counter==MAX_UINT32-1 &&
		note->end.dividor==MAX_UINT32
	){
		if(PC_isPlayingBlock()) playlistaddpos=0;

		for(;;){
			playlistaddpos++;
			block=PC_GetPlayBlock(playlistaddpos);
			if(block==NULL){
				ReturnPEQelement(peq);
				return;
			}
			track=ListFindElement1_r0(&block->tracks->l,tracknum);
			if(track!=NULL){
				if(track->notes!=NULL){
					if(track->stops!=NULL){
						PC_InsertElement2(peq,playlistaddpos,PlaceMin(&track->stops->l.p,&track->notes->l.p));
						break;
					}else{
						PC_InsertElement2(peq,playlistaddpos,&track->notes->l.p);
						break;
					}
				}else{
					if(track->stops!=NULL){
						PC_InsertElement2(peq,playlistaddpos,&track->stops->l.p);
						break;
					}
				}
			}
		}
	}else{
		PC_InsertElement2(peq,playlistaddpos,&note->end);
	}

//A small hack here.
	peq->playpos=orgplaylistaddpos;
}


void InitPEQnote(
	struct Blocks *block,
	struct Tracks *track,
	struct Notes *note,
	int playlistaddpos
){
	struct PEventQueue *peq=GetPEQelement();
	peq->TreatMe=PE_StartNote;
	peq->block=block;
	peq->track=track;
	peq->note=note;

	PC_InsertElement2_a(peq,playlistaddpos,&note->l.p);


	/* Note-stop. */

	if(track->onoff==1){
		InitPEQendnote(block,track,note,playlistaddpos);

		InitPEQvelocities(block,track,note,playlistaddpos);
	}
}

void InitPEQnotesBlock(
	PEQ_UsedTracks **UsedTracks,
	struct Blocks *block,
	Place *p,
	int playlistaddpos
){
	PEQ_UsedTracks *peq_usedtrack;

	struct Notes *note;
	struct Tracks *track=block->tracks;

	while(track!=NULL){
		if(
			ListFindElement1_r0(*UsedTracks,track->l.num) == NULL
		){
			note=track->notes;
			if(note!=NULL){

				while(PlaceLessThan(&note->l.p,p)){
					note=NextNote(note);
					if(note==NULL) break;
				}

				if(note!=NULL){
					peq_usedtrack=talloc(sizeof(PEQ_UsedTracks));
					peq_usedtrack->num=track->l.num;
					ListAddElement1(UsedTracks,peq_usedtrack);
					InitPEQnote(block,track,note,playlistaddpos);
				}
			}
		}
		track=NextTrack(track);
	}
}



void InitAllPEQnotes(
	struct Blocks *block,
	Place *p
){
	Place *firstplace=PlaceGetFirstPos();
	int playlistaddpos=0;
	struct Blocks *playlistblock;

	PEQ_UsedTracks *UsedTracks=NULL;


	InitPEQnotesBlock(&UsedTracks,block,p,0);

	if( PC_isPlayingSong() ){

		for(;;){
			playlistaddpos++;
			playlistblock=PC_GetPlayBlock(playlistaddpos);
			if(playlistblock==NULL) break;
			InitPEQnotesBlock(&UsedTracks,playlistblock,firstplace,playlistaddpos);
		}
	}


}


void PEQ_FindNextNoteAddPlayPos(struct PEventQueue *peq){
	struct Blocks *block;
	struct Tracks *track;
	struct Notes *note;
	NInt tracknum=peq->track->l.num;
	int playlistaddpos=0;

	for(;;){
		playlistaddpos++;
		block=PC_GetPlayBlock(playlistaddpos);
		if(block==NULL){
			ReturnPEQelement(peq);
			return;
		}
		track=ListFindElement1_r0(&block->tracks->l,tracknum);
		if(track!=NULL){
			note=track->notes;
			if(note!=NULL){
				peq->block=block;
				peq->track=track;
				peq->note=note;

				PC_InsertElement2_a(peq,playlistaddpos,&note->l.p);

				if(track->onoff==1){
					InitPEQendnote(block,track,note,playlistaddpos);
					InitPEQvelocities(block,track,note,playlistaddpos);
				}

				return;
			}
		}
	}

}


void PEQ_FindNextNote(struct PEventQueue *peq){

	struct Notes *note=NextNote(peq->note);

	if(note!=NULL){
		peq->note=note;
		PC_InsertElement2_a(peq,0,&note->l.p);
		if(peq->track->onoff==1){
			InitPEQendnote(peq->block,peq->track,note,0);
			InitPEQvelocities(peq->block,peq->track,note,0);
		}
	}else{
		PEQ_FindNextNoteAddPlayPos(peq);
	}
}

void PE_StartNote(STime time,struct PEventQueue *peq,int doit){

	struct Notes *note=peq->note;

	if(doit && peq->track->onoff==1 && peq->track->patch!=NULL){
		(*peq->track->patch->playnote)(note->note,note->velocity,peq->track,note);
	}

	PEQ_FindNextNote(peq);

	return;
}


void PE_StopNote(STime time,struct PEventQueue *peq,int doit){

	if(doit && peq->track->patch!=NULL){
//		Pdebug("Stop note: %d, vel: %d\n",peq->note->note,peq->note->velocity_end);
		(*peq->track->patch->stopnote)(
			peq->note->note,
			peq->note->velocity_end,
			peq->track,
			peq->note
		);
	}

	ReturnPEQelement(peq);
	return;
}






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

#include "Python.h"

#include "../common/nsmtracker.h"
#include "../common/list_proc.h"
#include "../common/placement_proc.h"
#include "../common/velocities_proc.h"
#include "../common/windows_proc.h"
#include "../common/wtracks_proc.h"
#include "../common/notes_proc.h"
#include "../advanced/ad_noteadd_proc.h"

#include "api_common_proc.h"
#include "api_support_proc.h"




extern struct Root *root;


void incNoteVolume(int incvolume,int windownum){
	struct Tracker_Windows *window=getWindowFromNum(windownum);
	if(window==NULL) return;

	IncreaseVelocityCurrPos(window,incvolume);
}

int getNoteNote(int windownum,int blocknum,int tracknum,int notenum){
	struct Notes *note=getNoteFromNumA(windownum,blocknum,tracknum,notenum);

	if(note==NULL) return -1;

  return note->note;
}

void putNoteNote(int notenote,int windownum,int blocknum,int tracknum,int notenum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  struct Notes *note=getNoteFromNumA(windownum,blocknum,tracknum,notenum);

  if(window==NULL) return;
  if(note==NULL) return;

  note->note=notenote;

  UpdateAndClearSomeTrackReallinesAndGfxWTracks(
						window,
						window->wblock,
						0,
						window->wblock->block->num_tracks-1
						);
}

int getNoteVolume(int windownum,int blocknum,int tracknum,int notenum){
	struct Notes *note=getNoteFromNumA(windownum,blocknum,tracknum,notenum);

	if(note==NULL) return -1;

  return note->velocity;
}

float getNotePlace(int windownum,int blocknum,int tracknum,int notenum){
	struct Notes *note=getNoteFromNumA(windownum,blocknum,tracknum,notenum);

	if(note==NULL) return -1.0f;

  return GetfloatFromPlace(&note->l.p);
}

float getNoteEndPlace(int windownum,int blocknum,int tracknum,int notenum){
	struct Notes *note=getNoteFromNumA(windownum,blocknum,tracknum,notenum);

	if(note==NULL) return -1.0f;

  return GetfloatFromPlace(&note->end);
}


int getNumNotes(int blocknum,int tracknum,int windownum){
	struct WTracks *wtrack=getWTrackFromNum(windownum,blocknum,tracknum);

	if(wtrack==NULL) return 0;
	if(wtrack->track->notes==NULL) return 0;

	return ListFindNumElements3(&wtrack->track->notes->l);
}

void setNoteEndPlace(int line,int counter,int dividor,int windownum,int blocknum,int tracknum,int notenum){
  struct Tracker_Windows *window=getWindowFromNum(windownum);
  struct Notes *note=getNoteFromNumA(windownum,blocknum,tracknum,notenum);

  if(window==NULL) return;
  if(note==NULL) return;

  PlaceCopy(&note->end, PlaceCreate(line,counter,dividor));
}

#include "../common/trackreallines_proc.h"
#include "../common/gfx_wtracks_proc.h"
//#include "../common/placement_proc.h"
#include "../common/wtracks_proc.h"

int addNote(int notenum,int velocity,
            int line,int counter,int dividor,
            int end_line,int end_counter,int end_dividor, 
            int windownum, int blocknum, int tracknum)
{
  struct WBlocks *wblock=getWBlockFromNum(windownum,blocknum);
  struct WTracks *wtrack=getWTrackFromNum(windownum,blocknum,tracknum);
  if(wblock==NULL || wtrack==NULL) {
    RError("unknown wblock(%p) or wtrack(%p) %d/%d/%d\n",wblock,wtrack,windownum,blocknum,tracknum);
    return -1;
  }

  struct Notes *note = InsertNote(wblock,
                                  wtrack,
                                  PlaceCreate(line,counter,dividor),
                                  end_line==-1 ? NULL : PlaceCreate(end_line,end_counter,end_dividor),
                                  notenum,
                                  velocity,
                                  0);
  int pos = 0;
  {
    struct Notes *notes = wtrack->track->notes;
    while(notes!=note) {
      notes=NextNote(notes);
      pos++;
    }
  }

  wblock->is_dirty = true;

  return pos;
}

void addNoteAdds(
	PyObject *noteadds,
	int windownum,
	int blocknum,
	int tracknum,
	float startplace,
	int sort
){
	struct Tracker_Windows *window;
	struct WBlocks *wblock;

	PyObject **pyobjects;
	int num_pyobjects;

	struct NoteAdds_track *nats;
	struct NoteAdds_track_do *nat_do=NULL;
	struct NoteAdds_block *nab;
	int num_nats;
	int lokke;

	const int attrformat[]={1,0,1,1};
	char *attrnames[]={"place","notenum","volume","endplace"};

//	printf("tracknum: %d\n",tracknum);

	wblock=getWBlockFromNumA(
		windownum,
		&window,
		blocknum
	);
	if(wblock==NULL){
		printf("wblock==NULL\n");
		return;
	}

	pyobjects=PYR_getPYOArray(&num_pyobjects,noteadds);
	if(num_pyobjects==-1){
		printf("Somethings wrong 1\n");
		return;
	}
	if(num_pyobjects==0) return;


	nab=talloc(sizeof(struct NoteAdds_block));
	nab->blocknum=blocknum;
	nab->num_nats_do=num_pyobjects;
	nab->nats_do=talloc(sizeof(struct NoteAdds_track_do *)*num_pyobjects);

	for(lokke=0;lokke<num_pyobjects;lokke++){
		nats=PYR_getObjArray(&num_nats,pyobjects[lokke],4,attrformat,attrnames);

		if(num_nats==-1){
			printf("Somethings wrong 2\n");
			return;
		}

		nat_do=talloc(sizeof(struct NoteAdds_track_do));
		nat_do->tracknum=tracknum;
		nat_do->num_nats=num_nats;
		nat_do->nats=nats;
		nat_do->startplace=startplace;
		nat_do->sort=sort;

		nab->nats_do[lokke]=nat_do;
	}

//	printf("num_pyobjects: %d\n",num_pyobjects);
	if(num_pyobjects==1){
		AD_installNoteAdds_track_do(
			window,
			wblock,
			nat_do
		);
	}else{
		AD_insertNoteAdds_block_do(window,nab);
	}
}



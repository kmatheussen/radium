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




/*******************************************************************

   OVERVIEW

	Graphical datas for the notes and velocities. Not that elegant
	programmed as the fxlines and temponodelines, because it was
	programmed long before those ones.

*******************************************************************/

#include <assert.h>
#include <string.h>

#include "nsmtracker.h"
#include "vector_proc.h"
#include "list_proc.h"
#include "localzooms_proc.h"
#include "windows_proc.h"
#include "wtracks_proc.h"
#include "subtrack_proc.h"
#include "placement_proc.h"
#include "realline_calc_proc.h"
#include "velocities_proc.h"
#include "gfx_subtrack_proc.h"
#include "gfx_wtracks_proc.h"
#include "nodelines_proc.h"
#include "nodeboxes_proc.h"
#include "nodelines.h"
#include "fxlines_proc.h"
#include "instruments_proc.h"
#include "patch_proc.h"
#include "time_proc.h"
#include "tracks_proc.h"
#include "pitchnodes_proc.h"

#include "trackreallines_proc.h"


extern struct Root *root;


struct TrackReallineNodeInfo{
	struct WTracks *wtrack;
	void *pointer;
	struct Velocities *velocity;
	int type;
	int subtype;
};


/*******************************************************************
  FUNCTION
    This one both free old- and allocate new trackreallines.
*******************************************************************/
void NewTrackRealLines(
	struct WBlocks *wblock,
	struct WTracks *wtrack
){
  wtrack->trackreallines=talloc(wblock->num_reallines * sizeof(struct TrackRealline));
  wtrack->wpitches=talloc(sizeof(WPitches *) * wblock->num_reallines);
}


static void AddTrackReallineElement(
	struct WTracks *wtrack,
	struct TrackReallineElements *element,
	int realline
){
	element->next=wtrack->trackreallines[realline].trackreallineelements;
	wtrack->trackreallines[realline].trackreallineelements=element;
}


/**************************************************************************
  FUNCTION
    Adds a trackreallineelement to the trackline.
**************************************************************************/
static void InsertTRLElementS(
	struct WTracks *wtrack,
        struct Notes *note,
	int realline,
	int type,int subtype,
        float y1,float y2,
	float x1,float x2,
	void *pointer
){
	struct TrackReallineElements *element = talloc(sizeof(struct TrackReallineElements));
        element->note=note;
	element->type=type;
	element->subtype=subtype;
	element->y1=y1;
	element->y2=y2;
	element->x1=x1;
	element->x2=x2;
	element->pointer=pointer;

	AddTrackReallineElement(wtrack,element,realline);
}



struct TrackReallineElements *TRE_startroot=NULL;

static void InsertTRLElement_start(
	struct TrackReallineElements *element,
	int realline
){
	element->type=realline;
	element->next=TRE_startroot;
	TRE_startroot=element;
}

void InsertStartTREElements(
	struct WTracks *wtrack
){
	struct TrackReallineElements *element=TRE_startroot,*temp;
	int realline;

	while(element!=NULL){
		temp=element->next;
		realline=element->type;
		element->type=TRE_REALSTARTSTOP;
		AddTrackReallineElement(wtrack,element,realline);	
		element=temp;
	}
	TRE_startroot=NULL;
}


/**************************************************************************
  FUNCTION
    Adds a trackreallineelement to the trackline. Does allso ensure that
    all datas are within the trackline area.
**************************************************************************/
static void InsertTRLElement(
	struct Tracker_Windows *window,
	struct WTracks *wtrack,
	int subtrack,
	int realline,
	int type,int subtype,float y1,float y2,
	int x1,int x2,
	void *pointer,
        void *note
){
	struct TrackReallineElements *element = talloc(sizeof(struct TrackReallineElements));

	//	y1=min(window->fontheight,max(0,y1));
	//	y2=min(window->fontheight-1,max(0,y2));

	x1=SubtrackBoundaries(wtrack,subtrack,x1);
	x2=SubtrackBoundaries(wtrack,subtrack,x2);

	//	if(y1==y2 && x1==x2) return;
	
	element->type=type;
	element->subtype=subtype;
	element->y1=y1;
	element->y2=y2;

	element->x1=x1;
	element->x2=x2;
	element->pointer=pointer;
        element->note=note;

	if(type==TRE_REALSTARTSTOP){
		InsertTRLElement_start(element,realline);
	}else{
		AddTrackReallineElement(wtrack,element,realline);
	}
}



/**************************************************************************
***************************************************************************
***************************************************************************
***************************************************************************
           Start of the AddTrackRealLineNote Part
***************************************************************************
***************************************************************************
***************************************************************************
***************************************************************************/


/**************************************************************************
  FUNCTION
    Adds a trackreallineelement to the trackline. Does allso ensure that
    all datas are within the trackline area.
**************************************************************************/
static void NodeLineCallBack(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
        struct WTracks *wtrack,
	void *extrainfo,
	int firstlast,
	int realline,
	float y1,float y2,
	float x1,float x2
){
	struct TrackReallineNodeInfo *nodeinfo=(struct TrackReallineNodeInfo *)extrainfo;
	struct Notes *note=(struct Notes *)nodeinfo->pointer;

	InsertTRLElementS(
		nodeinfo->wtrack,
                note,
		realline,
		nodeinfo->type,
		nodeinfo->subtype,
		y1,y2,x1,x2,
		nodeinfo->pointer
	);

	if(firstlast==NODELINE_FIRST || firstlast==NODELINE_FIRSTANDLAST){
	  InsertTRLElementS(
			    nodeinfo->wtrack,
                            note,
			    realline,
			    TRE_VELLINESTART,
			    nodeinfo->subtype,
			    y1,(float)GetNodeSize(window,wblock,wtrack),x1,(float)GetNodeSize(window,wblock,wtrack),
			    nodeinfo->pointer
			    );
	  InsertTRLElementS(
			    nodeinfo->wtrack,
                            note,
			    realline,
			    TRE_REALSTARTSTOP,
			    nodeinfo->subtype,
			    y1,0.0f,0.0f,0.0f,
			    nodeinfo->pointer
			    );

	}

	if(
		note->noend==1 &&
		note->end.line==wblock->block->num_lines-1 &&
		note->end.counter==MAX_UINT32-1 &&
		note->end.dividor==MAX_UINT32
	)return;

	if(firstlast==NODELINE_LAST || firstlast==NODELINE_FIRSTANDLAST){
          InsertTRLElementS(
                            nodeinfo->wtrack,
                            note,
                            realline,
                            TRE_VELLINEEND,
                            nodeinfo->subtype,
                            y2,(float)(GetNodeSize(window,wblock,wtrack)*2),x2,(float)GetNodeSize(window,wblock,wtrack),
                            nodeinfo->pointer
                            );
	  InsertTRLElementS(
			    nodeinfo->wtrack,
                            note,
			    realline,
			    TRE_REALSTARTSTOP,
			    nodeinfo->subtype,
			    y2,0.0f,0.0f,0.0f,
			    nodeinfo->pointer
			    );
	}
}

static void NodeLineCallBack_last(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
        struct WTracks *wtrack,
	void *extrainfo,
	int firstlast,
	int realline,
	float y1,float y2,
	float x1,float x2
){
	struct TrackReallineNodeInfo *nodeinfo=(struct TrackReallineNodeInfo *)extrainfo;
	struct Notes *note=(struct Notes *)nodeinfo->pointer;

	InsertTRLElementS(
		nodeinfo->wtrack,
                note,
		realline,
		nodeinfo->type,
		nodeinfo->subtype,
		y1,y2,x1,x2,
		nodeinfo->pointer
	);


	if(firstlast==NODELINE_FIRST || firstlast==NODELINE_FIRSTANDLAST){
	  InsertTRLElementS(
			    nodeinfo->wtrack,
                            note,
			    realline,
			    TRE_VELLINENODE,
			    nodeinfo->subtype,
			    y1,y2,x1,x2,
			    nodeinfo->velocity
			    );
	}

	if(
		note->noend==1 &&
		note->end.line==wblock->block->num_lines-1 &&
		note->end.counter==MAX_UINT32-1 &&
		note->end.dividor==MAX_UINT32
	)return;

	if(firstlast==NODELINE_LAST || firstlast==NODELINE_FIRSTANDLAST){
	  InsertTRLElementS(
			    nodeinfo->wtrack,
                            note,
			    realline,
			    TRE_VELLINEEND,
			    nodeinfo->subtype,
			    y2,(float)GetNodeSize(window,wblock,wtrack),x2,(float)GetNodeSize(window,wblock,wtrack),
			    nodeinfo->pointer
			    );
	  InsertTRLElementS(
			    nodeinfo->wtrack,
                            note,
			    realline,
			    TRE_REALSTARTSTOP,
			    nodeinfo->subtype,
			    y2,0.0f,0.0f,0.0f,
			    nodeinfo->pointer
			    );
	}
}

static void NodeLineCallBack_first(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
        struct WTracks *wtrack,
	void *extrainfo,
	int firstlast,
	int realline,
	float y1,float y2,
	float x1,float x2
){
	struct TrackReallineNodeInfo *nodeinfo=(struct TrackReallineNodeInfo *)extrainfo;
	struct Notes *note=(struct Notes *)nodeinfo->pointer;

	InsertTRLElementS(
		nodeinfo->wtrack,
                note,
		realline,
		nodeinfo->type,
		nodeinfo->subtype,
		y1,y2,x1,x2,
		nodeinfo->pointer
	);

	if(firstlast==NODELINE_FIRST || firstlast==NODELINE_FIRSTANDLAST){
	  InsertTRLElementS(
			    nodeinfo->wtrack,
                            note,
			    realline,
			    TRE_VELLINESTART,
			    nodeinfo->subtype,
			    y1,(float)(GetNodeSize(window,wblock,wtrack)*2),x1,(float)GetNodeSize(window,wblock,wtrack),
			    nodeinfo->pointer
			    );
	  InsertTRLElementS(
			    nodeinfo->wtrack,
                            note,
			    realline,
			    TRE_REALSTARTSTOP,
			    nodeinfo->subtype,
			    y1,0.0f,0.0f,0.0f,
			    nodeinfo->pointer
			    );
	}
}

void NodeLineCallBack_vel(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
        struct WTracks *wtrack,
	void *extrainfo,
	int firstlast,
	int realline,
	float y1,float y2,
	float x1,float x2
){
	struct TrackReallineNodeInfo *nodeinfo=(struct TrackReallineNodeInfo *)extrainfo;
	struct Notes *note=(struct Notes *)nodeinfo->pointer;

	InsertTRLElementS(
		nodeinfo->wtrack,
                note,
		realline,
		TRE_VELLINE,
		nodeinfo->subtype,
		y1,y2,x1,x2,
		nodeinfo->pointer
	);

	if(firstlast==NODELINE_FIRST || firstlast==NODELINE_FIRSTANDLAST){
	  InsertTRLElementS(
			    nodeinfo->wtrack,
                            note,
			    realline,
			    TRE_VELLINENODE,
			    nodeinfo->subtype,
			    y1,y2,x1,x2,
			    nodeinfo->velocity
			    );
	}
}



static void AddTrackReallineNote(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	struct Notes *note
){
	struct TrackReallineNodeInfo nodeinfo;

	struct Velocities *velocity,*prev;

	int realline=FindRealLineForNote(wblock,note->Tline,note);
	int subtrack=FindFirstFreeSubTrack(wtrack,realline,&note->l.p);
        note->subtrack = subtrack;

	float maxx = MAX_VELOCITY;

        assert(note!=NULL);
	InsertTRLElement(
		window,
		wtrack,
		subtrack,
		realline,
		TRE_THISNOTELINES,
		subtrack,
		0.0f,0.0f,0,0,					/* Will be filled in in the OrganizeThisNoteLines part. */
		note,
                note
	);

	nodeinfo.wtrack=wtrack;
	nodeinfo.pointer=note;
	nodeinfo.type=TRE_VELLINE;
	nodeinfo.subtype=subtrack;

	velocity=note->velocities;
	if(velocity!=NULL){
		MakeNodeLines(
			window,
			wblock,
                        wtrack,
			&note->l.p,&velocity->l.p,
			(float)note->velocity,(float)velocity->velocity,
			0.0f,maxx,
			&nodeinfo,
			&NodeLineCallBack_first
		);
		prev=velocity;
		velocity=NextVelocity(velocity);
		while(velocity!=NULL){
			nodeinfo.velocity=prev;
			MakeNodeLines(
				window,
				wblock,
                                wtrack,
				&prev->l.p,&velocity->l.p,
				(float)prev->velocity,(float)velocity->velocity,
				0.0f,maxx,
				&nodeinfo,
				&NodeLineCallBack_vel
			);
			prev=velocity;
			velocity=NextVelocity(velocity);
		}
		nodeinfo.velocity=prev;
		MakeNodeLines(
			window,
			wblock,
                        wtrack,
			&prev->l.p,&note->end,
			(float)prev->velocity,(float)note->velocity_end,
			0.0f,maxx,
			&nodeinfo,
			&NodeLineCallBack_last
		);
	}else{
		MakeNodeLines(
			window,
			wblock,
                        wtrack,
			&note->l.p,&note->end,
			(float)note->velocity,(float)note->velocity_end,
			0.0f,maxx,
			&nodeinfo,
			&NodeLineCallBack
		);
	}
}



/**************************************************************************
***************************************************************************
***************************************************************************
***************************************************************************
           End of the AddTrackRealLineNote Part
***************************************************************************
***************************************************************************
***************************************************************************
***************************************************************************/



/**************************************************************************
***************************************************************************
***************************************************************************
***************************************************************************
	       Start of the OrganizeThisNoteLines part
***************************************************************************
***************************************************************************
***************************************************************************
***************************************************************************/


/*** NOTES ****************************************************************
    About the following two procedures: There might be bugs here! I'm not
    at all too shure if they allways function correct.
***************************************************************************/
int FindNumberOfNotesOnTrackRealline(struct TrackReallineElements *element){
	int ret=0;
	while(element!=NULL){
		if(element->type==TRE_THISNOTELINES)
                  ret++;
		element=element->next;
	}
	return ret;
}


int FindNumberOfFreeReallinesDown(struct WBlocks *wblock,struct WTracks *wtrack,int realline){
	int done=0;
	int free=realline+1;
	while(0==done && free<wblock->num_reallines){
		done=FindNumberOfNotesOnTrackRealline(wtrack->trackreallines[free].trackreallineelements);
		free++;
	}
	return R_MAX(0,free-realline-2);  /* The use of 'max' is just a hack here 
	                                   I did to get it to work. I don't know
	                                   if this allways works as I haven't
	                                   thought this function thru. ...After
	                                   thinking a bit more, I think it is correct.*/
}

/*** END NOTES ****/


/************************************************************************
  FUNCTION
    Set x1,y1,x2,y2 values for the TRE_THISNOTELINES trackrealline
    element 'element'.
************************************************************************/
void InsertCoordinatesForThisNoteLines(
	struct Tracker_Windows *window,
	struct WTracks *wtrack,
	int realline,
	struct TrackReallineElements *element
){
	int velend=0;
	struct TrackReallineElements *temp=wtrack->trackreallines[realline].trackreallineelements;

   /****************************************************************
     First find a suitable TRE_VELLINE element related to this note.
	 ****************************************************************/
	for(;;){
		if(temp==NULL){
			RError("Error in function 'InsertCoordinatesForThisNoteLines' in file 'trackreallines.c %x'\n",wtrack->trackreallines[realline].trackreallineelements);
			temp=wtrack->trackreallines[realline].trackreallineelements;
			break;
		}
		if(temp->pointer==element->pointer){
			if(temp->type==TRE_VELLINE &&
				(1==velend || (
					temp->y2 >= 0.5f &&
					temp->y1 <= 0.5f
			)))break;

			if(
				0==velend &&
				(temp->type==TRE_VELLINEEND || temp->type==TRE_VELLINESTART)
			){
				temp=wtrack->trackreallines[realline].trackreallineelements;
				velend=1;
				continue;
			}
		}
		temp=temp->next;
	}


	/****************************************************************
	  Then use the coordinates for that TRE_VELLINE element to
	  calculate the coordinates for the TRE_THISNOTELINES element.
	 ****************************************************************/
	element->x1=0;
	element->y1=0.5f;

	element->x2=(temp->x1+temp->x2)/2;
	element->y2=(temp->y1+temp->y2)/2;
	element->subtype=temp->subtype;

	wtrack->trackreallines[realline].note=((struct Notes *)(element->pointer))->note;
	wtrack->trackreallines[realline].dasnote=(struct Notes *)element->pointer;
}


int NoteReachesDownToRealline(
	struct WBlocks *wblock,
	struct Notes *note,
	int realline
){
	struct LocalZooms *myrealline=wblock->reallines[realline];
	float a1=GetfloatFromLineCounterDividor(&myrealline->l.p);
	float a2=GetfloatFromLineCounterDividor(&note->end);

	if(a2>a1) return 1;
	return 0;
}


/****************************************************************
  FUNCTION
    Spread 'to_insert' number of notes placed on the realline to reallines
    below.  From the AddTrackReallineNote procedure, the latest note
    is put first and the first note is placed latest in the list,
    which makes the function relatively simple. The function does
    not work that good, and I have to admit I don't quite understand
    how it works any longer either, so it should be replaced with a new
    routine.
****************************************************************/
void InsertNotesFromRealline(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int realline,
	int to_insert,
	int of_notes
){
	struct TrackReallineElements *mylist=NULL;
	struct TrackReallineElements *element=wtrack->trackreallines[realline].trackreallineelements;
	struct TrackReallineElements *prev=NULL;
	int lokke;
	int mul=0;

	/* First handle as many elements as there are free lines down. This
	   is done by removing all such elements from the list and temporarily
	   put it in the new list 'mylist'.
	*/
	for(lokke=1;lokke<=to_insert;lokke++){

		/* Find element. */
		while(element->type!=TRE_THISNOTELINES){
			prev=element;
			element=element->next;
		}

		/* Temporarily remove it from the list */
		if(prev!=NULL){
			prev->next=element->next;
		}else{
			wtrack->trackreallines[realline].trackreallineelements=element->next;
		}

		/* Handle it. */
		if(
			NoteReachesDownToRealline(
				wblock,
				(struct Notes *)element->pointer,
				realline+to_insert-lokke+1
			)==1
		){
			AddTrackReallineElement(wtrack,element,realline+to_insert-lokke+1);
			InsertCoordinatesForThisNoteLines(window,wtrack,realline+to_insert-lokke+1,element);
		}else{
			element->next=mylist;
			mylist=element;
			InsertCoordinatesForThisNoteLines(window,wtrack,realline,element);
			mul=1;
		}

		/* Insert the element into the list again if it was okey. */
		if(prev!=NULL){
			element=prev->next;
		}else{
			element=wtrack->trackreallines[realline].trackreallineelements;
		}
	}


	/* Then handle the elements that has to be placed on the realline. */

	element=wtrack->trackreallines[realline].trackreallineelements;
	if(of_notes>to_insert+1){
		while(element!=NULL){
			while(element!=NULL && element->type!=TRE_THISNOTELINES) element=element->next;
			if(element!=NULL){
				InsertCoordinatesForThisNoteLines(window,wtrack,realline,element);
				element=element->next;
			}
		}
		mul=1;
	}else{
		while(element->type!=TRE_THISNOTELINES) element=element->next;
		wtrack->trackreallines[realline].note=((struct Notes *)(element->pointer))->note;
                wtrack->trackreallines[realline].dasnote=(struct Notes *)element->pointer;
		InsertCoordinatesForThisNoteLines(window,wtrack,realline,element);
	}


	/* Now inserting the mylists elements back into the trackreallineelements list. */


	element=wtrack->trackreallines[realline].trackreallineelements;
	while(element->next!=NULL)element=element->next;
	element->next=mylist;


	/* Set note to NOTE_MUL if mul. */

	if(1==mul){
		wtrack->trackreallines[realline].note=NOTE_MUL;
                wtrack->trackreallines[realline].dasnote=NULL;
	}


}

/****************************************************************
  FUNCTION
    Organizes the TRE_THISNOTELINES TRLEElements.
    Does ao. set the trackreallines->note attributes.
****************************************************************/
void OrganizeThisNoteLines(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack
){
	int realline;
	int num_notes,num_freereallinesdown;
	struct TrackRealline *trackreallines=wtrack->trackreallines;
	struct Notes *note;
	struct TrackReallineElements *temp;

	for(realline=0;realline<wblock->num_reallines;realline++){
		num_notes=FindNumberOfNotesOnTrackRealline(trackreallines[realline].trackreallineelements);
		if(num_notes>0){
			if(num_notes>1){
				num_freereallinesdown=FindNumberOfFreeReallinesDown(wblock,wtrack,realline);
				if(num_freereallinesdown>=num_notes-1){
					InsertNotesFromRealline(window,wblock,wtrack,realline,num_notes-1,num_notes);
				}else{
					InsertNotesFromRealline(window,wblock,wtrack,realline,num_freereallinesdown,num_notes);
				}
				realline+=num_freereallinesdown;
			}else{
				temp=wtrack->trackreallines[realline].trackreallineelements;
				while(temp->type!=TRE_THISNOTELINES) temp=temp->next;
				note=temp->pointer;
				trackreallines[realline].note=note->note;
                                trackreallines[realline].dasnote=note;
				InsertCoordinatesForThisNoteLines(window,wtrack,realline,temp);
			}
		}
	}
}



/**************************************************************************
***************************************************************************
***************************************************************************
***************************************************************************
	       End of the OrganizeThisNoteLines part
***************************************************************************
***************************************************************************
***************************************************************************
***************************************************************************/


void AddStopsElements(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack
){
	int realline = 0;
	struct Stops *stop=wtrack->track->stops;

	while(stop!=NULL){
		realline=FindRealLineFor(wblock,R_MAX(stop->Tline,realline),&stop->l.p);

		int subrealline=FindSubRealLine(window,wblock,realline,&stop->l.p);
		InsertTRLElementS(
			wtrack,
                        NULL,
			realline,
			TRE_STOPLINE,0,
			(float)subrealline,(float)subrealline,0.0f,(float)(wtrack->fxwidth-2),
			stop
		);

		if(wtrack->trackreallines[realline].note!=0){
			wtrack->trackreallines[realline].note=NOTE_MUL;
		}else{
			wtrack->trackreallines[realline].note=NOTE_STP;
		}
                wtrack->trackreallines[realline].dasnote=NULL;
		stop=NextStop(stop);
	}

}

void AddPitchElements(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack
){

  float min_pitch = 10000.0f;
  float max_pitch = -10000.0f;

  TRACK_get_min_and_max_pitches(wtrack->track,&min_pitch, &max_pitch);

  struct Notes *note=wtrack->track->notes;

  while(note!=NULL){
    struct Pitches *pitch=note->pitches;
    int realline = 0;

    while(pitch!=NULL){

      realline=FindRealLineFor(wblock,R_MAX(pitch->Tline,realline),&pitch->l.p);
      int subrealline=FindSubRealLine(window,wblock,realline,&pitch->l.p);

      InsertTRLElementS(
			wtrack,
                        note,
			realline,
                        TRE_THISPITCHLINES,0,
			(float)subrealline,(float)subrealline,0.0f,(float)(wtrack->fxwidth-2),
			pitch
                        );
      
      if(wtrack->trackreallines[realline].note!=0){
        wtrack->trackreallines[realline].note=NOTE_MUL;
	wtrack->trackreallines[realline].dasnote=NULL;
      }else{
        wtrack->trackreallines[realline].note=NOTE_PITCH_START + pitch->note;
      }

      pitch=NextPitch(pitch);
    }

    UpdateWPitches(window, wblock, wtrack, note, min_pitch, max_pitch);

    note=NextNote(note);
  }
}

static void create_peaks(
                         struct Tracker_Windows *window,
                         struct WBlocks *wblock,
                         struct WTracks *wtrack)
{
  struct Blocks *block = wblock->block;

  int realline;
  for(realline=0;realline<wblock->num_reallines;realline++){
    struct TrackRealline *trackrealline= &wtrack->trackreallines[realline];
    struct TrackReallineElements *element;
    for(element=trackrealline->trackreallineelements;element!=NULL;element=element->next){
      if(element->type==TRE_VELLINE){
        
        if(TRACK_has_peaks(wtrack->track)==true && (element->y2-element->y1)>0.001){

          struct Patch *patch = wtrack->track->patch;

          struct Notes *note=element->pointer;
          float note_time = Place2STime(block, &note->l.p);

          element->num_peaks = ((element->y2-element->y1)*NUM_PEAKS_PER_LINE)+1;
          if(element->num_peaks<0){
            RError("element->num_peaks<0: %d",element->num_peaks);
            element->num_peaks=0;
          }

          if(element->num_peaks>0){

            element->num_peaks++; // We're generating coordinates for a polygon, and it needs to stretch to the start of the next node.

            float time1 = Place2STime(block, &wblock->reallines[realline]->l.p) - note_time;
            float time2;

            if(realline<wblock->num_reallines-1){
              time2 = Place2STime(block, &wblock->reallines[realline+1]->l.p);
            }else{
              time2 = block->times[block->num_lines].time;
            }
            time2 = time2 - note_time;

            const int num_channels=PATCH_get_peaks(patch, 0,
                                                   -1,
                                                   wtrack->track,
                                                   0,0,
                                                   NULL,NULL);
            int ch;

            for(ch=0;ch<num_channels;ch++){
              APoint *peaks=talloc_atomic(element->num_peaks*2*sizeof(APoint));
              
              element->peaks[ch] = peaks;

              int i;
              for(i=0;i<element->num_peaks;i++){
                float y1 = scale(i,0,element->num_peaks-1,element->y1,element->y2); // -1 is here since we generate 2 points for each node.
                float y2 = scale(i+1,0,element->num_peaks-1,element->y1,element->y2);

                float track_volume =  wtrack->track->volumeonoff ? (float)wtrack->track->volume / MAXTRACKVOL : 1.0f;
                float velocity = scale(y2,element->y1,element->y2,element->x1,element->x2);

                float min,max;
                PATCH_get_peaks(patch, 
                                note->note,
                                ch,
                                wtrack->track,
                                scale(y1,0,1,time1,time2) / block->reltempo,
                                scale(y2,0,1,time1,time2) / block->reltempo,
                                &min,
                                &max);
                
                float bound_x1 = scale(ch,0,num_channels,0.0f,velocity);
                float bound_x2 = scale(ch+1,0,num_channels,0.0f,velocity);

                peaks[i].x = R_MAX(bound_x1, scale(min*track_volume,-1,1, bound_x1, bound_x2));
                peaks[i].y = y1;
                peaks[element->num_peaks*2 - 1 - i].x = R_MIN(bound_x2, scale(max*track_volume,-1,1, bound_x1, bound_x2));
                peaks[element->num_peaks*2 - 1 - i].y = y1; //max
              }
            }
          }
        }

        element->velocity_polygon=talloc_atomic(2*2*sizeof(APoint));
          
        element->velocity_polygon[0].x = 0.0f;
        element->velocity_polygon[0].y = element->y1;
          
        element->velocity_polygon[1].x = 0.0f;
        element->velocity_polygon[1].y = element->y2;
        
        element->velocity_polygon[2].x = element->x2;
        element->velocity_polygon[2].y = element->y2;
        
        element->velocity_polygon[3].x = element->x1;
        element->velocity_polygon[3].y = element->y1;
      }
    }
  }
}

/****************************************************************
  FUNCTION
    Building up all from the ground, is what really happens.
****************************************************************/
void UpdateTrackReallines(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack
){
	struct Notes *note=wtrack->track->notes;

	SetNum_Vel(wtrack);

	/*
	if(wtrack->num_vel>(wtrack->fxwidth/(window->fontwidth*2))){
		wtrack->fxwidth=window->fontwidth*wtrack->num_vel*2;
		if(window->wblock==wblock){
			UpdateAllWTracksCoordinates(window,wblock);
			UpdateFXNodeLines(window,wblock,wtrack);
			window->must_redraw = true;
		}
	}
	*/

	NewTrackRealLines(wblock,wtrack);

	while(note!=NULL){
		AddTrackReallineNote(window,wblock,wtrack,note);
		note=NextNote(note);
	}
	InsertStartTREElements(wtrack);

	OrganizeThisNoteLines(window,wblock,wtrack);

	AddStopsElements(window,wblock,wtrack);

        AddPitchElements(window,wblock,wtrack);

        create_peaks(window,wblock,wtrack);
        
	if(window->curr_track==wtrack->l.num)
		if(window->curr_track_sub >= wtrack->num_vel)
			window->curr_track_sub=wtrack->num_vel-1;

}


void UpdateAllTrackReallines(
                             struct Tracker_Windows *window,
                             struct WBlocks *wblock
){
	struct WTracks *wtrack=wblock->wtracks;

	while(wtrack!=NULL){
		UpdateTrackReallines(
			window,
			wblock,
			wtrack
		);
		wtrack=NextWTrack(wtrack);
	}
}

void TRACKREALLINES_update_peak_tracks(struct Tracker_Windows *window, struct Patch *patch){
  struct WBlocks *wblock=window->wblocks;

  while(wblock!=NULL){

    struct WTracks *wtrack=wblock->wtracks;

    while(wtrack!=NULL){
      struct Tracks *track=wtrack->track;

      if(track->patch==patch || patch==NULL)
        if(TRACK_has_peaks(wtrack->track))
          UpdateTrackReallines(
                               window,
                               wblock,
                               wtrack
                               );

      wtrack=NextWTrack(wtrack);
    }

    wblock=NextWBlock(wblock);
  }
}

static bool g_peaks_are_dirty;

void TRACKREALLINES_call_very_often(struct Tracker_Windows *window){
  if(g_peaks_are_dirty==false)
    return;
  else
    g_peaks_are_dirty=false;

  struct WBlocks *wblock=window->wblocks;

  struct Instruments *instrument = get_all_instruments();
  while(instrument!=NULL){
    VECTOR_FOR_EACH(struct Patch *patch,&instrument->patches){
      if(patch->peaks_are_dirty==true){
        patch->peaks_are_dirty=false;
        TRACKREALLINES_update_peak_tracks(window,patch);
        DrawUpAllPeakWTracks(window,wblock,patch);
      }
    }END_VECTOR_FOR_EACH;
    instrument = NextInstrument(instrument);
  }
}

void RT_TRACKREALLINES_schedule_update_peak_tracks(struct Patch *patch){
  patch->peaks_are_dirty = true;
  g_peaks_are_dirty = true;
}

void UpdateSomeTrackReallines(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	NInt starttrack,
	NInt endtrack
){
	NInt lokke;

	struct WTracks *wtrack=ListFindElement1(&wblock->wtracks->l,starttrack);
	if(wtrack==NULL) return;

	for(lokke=0;lokke<=endtrack-starttrack;lokke++){
		UpdateTrackReallines(window,wblock,wtrack);
		wtrack=NextWTrack(wtrack);
		if(wtrack==NULL) break;
	}

}


/* The next one is used to speed up trackwidth changes.
   Should only be used when minimizing.
*/
void TR_scaleTrackReallines(
	struct Tracker_Windows *window,
	struct WBlocks *wblock,
	struct WTracks *wtrack,
	int oldwidth
	)
{
  struct TrackRealline *tr=wtrack->trackreallines;
  struct TrackReallineElements *tre;
  int num_reallines=wblock->num_reallines;
  int lokke;
  int width=wtrack->fxwidth;

  for(lokke=0;lokke<num_reallines;lokke++){
    tre=tr[lokke].trackreallineelements;
    while(tre!=NULL){
      tre->x1=(tre->x1*width)/oldwidth;
      tre->x2=(tre->x2*width)/oldwidth;
      tre=tre->next;
    }
  }

}







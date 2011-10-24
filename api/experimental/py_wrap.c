/*    Wrapper code for python interface. All functions here should be
      crash-proof. No matter what you put in, radium should in theory
      not crash. Python are only allowed to call _these_ functions in Radium.
*/


#include "../common/nsmtracker.h"
#include "../common/list_proc.h"
#include "../common/placement_proc.h"

#define FROM_SWIG_C
#include "swig.i"

extern struct Root *root;




/*******************************************
*******************************************/

struct Tracker_Windows *getWindowFromNum(int windownum){
	if(windownum==-1) return root->song->tracker_windows;
	return (struct Tracker_Windows *)ListFindElement1_r0(&root->song->tracker_windows->l,(NInt)windownum);
}


struct WBlocks *SWIG_getWBlock(int blocknum){
  if(blocknum==-1) return root->song->tracker_windows->wblock;
  return (struct WBlocks *)ListFindElement1_r0(&root->song->tracker_windows->wblocks->l,(NInt)blocknum);
}

struct Blocks *SWIG_getBlock(int blocknum){
  if(blocknum==-1) return root->song->tracker_windows->wblock->block;
  return (struct Blocks *)ListFindElement1_r0(&root->song->blocks->l,(NInt)blocknum);
}

struct Tracks *SWIG_getTrack(int blocknum,int tracknum){
  struct WBlocks *wblock=SWIG_getWBlock(blocknum);
  if(wblock==NULL) return NULL;
  if(tracknum==-1) return wblock->wtrack->track;
  return (struct Tracks *)ListFindElement1_r0(&wblock->block->tracks->l,(NInt)tracknum);
}

struct Notes *SWIG_getNote(int blocknum,int tracknum,int notenum){
  struct Tracks *track=SWIG_getTrack(blocknum,tracknum);
  if(track==NULL) return NULL;
  return (struct Notes *)ListFindElement3_num(&track->notes->l,(NInt)notenum);
}

int RA_getNumNotes(int blocknum,int tracknum){
  struct Tracks *track=SWIG_getTrack(blocknum,tracknum);
  if(track==NULL || track->notes==NULL) return 0;
  return ListFindNumElements3(&track->notes->l);
}

int RA_getNoteNote(int blocknum,int tracknum,int notenum){
  struct Notes *note=SWIG_getNote(blocknum,tracknum,notenum);
  if(note==NULL) return 0;
  return note->note;
}


float RA_getNoteVel(int blocknum,int tracknum,int notenum){
  struct Tracks *track=SWIG_getTrack(blocknum,tracknum);
  struct Notes *note=SWIG_getNote(blocknum,tracknum,notenum);
  if(note==NULL) return 0;

  return (float)(
		 (float)note->velocity
		 /
		 (float)((*track->instrument->getMaxVelocity)(track))
		 );
}

float RA_getNoteFloatStart(int blocknum,int tracknum,int notenum){
  struct Notes *note=SWIG_getNote(blocknum,tracknum,notenum);
  if(note==NULL) return 0;
  return GetfloatFromPlace(&note->l.p);
}

float RA_getNoteFloatEnd(int blocknum,int tracknum,int notenum){
  struct Notes *note=SWIG_getNote(blocknum,tracknum,notenum);
  if(note==NULL) return 0;
  return GetfloatFromPlace(&note->end);
}

void RA_addNote_FloatPlace(
			   int blocknum,
			   int tracknum,
			   float start,
			   int notenum,
			   float volume,
			   float end)
{
  struct Blocks *block=SWIG_getBlock(blocknum);
  struct Tracks *track=SWIG_getTrack(blocknum,tracknum);
  struct Notes *note;
  Place p2;
  if(track==NULL) return;

  note=talloc(sizeof(struct Notes));

  Float2Placement(start,&note->l.p);
  Float2Placement(end,&note->end);
  PlaceSetLastPos(block,&p2);

  if(PlaceGreaterOrEqual(&note->l.p,&p2)) return;
  if(PlaceGreaterOrEqual(&note->end,&p2)){
    PlaceCopy(&note->end,&p2);
  }

  note->note=notenum;
  note->velocity=boundaries(
			    volume*(*track->instrument->getMaxVelocity)(track),
			    0,
			    (*track->instrument->getMaxVelocity)(track)
			    );

  ListAddElement3(&track->notes,&note->l);
}


void RA_removeNotes(int blocknum,int tracknum){
  struct Tracks *track=SWIG_getTrack(blocknum,tracknum);
  if(track==NULL) return;
  track->notes=NULL;
}




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


#include "../common/includepython.h"

#include "../common/nsmtracker.h"
#include "../common/list_proc.h"
#include "../common/vector_proc.h"
#include "../common/placement_proc.h"
#include "../common/notes_proc.h"
#include "../common/instruments_proc.h"
#include "../common/patch_proc.h"
#include "../common/visual_proc.h"
#include "../embedded_scheme/scheme_proc.h"
#include "../embedded_scheme/s7extra_proc.h"

#include "api_gui_proc.h"

#include "api_common_proc.h"


extern struct Root *root;



/* Hmm, well, okey, I put the init_radium function here. */


PyObject *gotkeyFunc=NULL;

int
#ifdef _AMIGA
__stdargs __saveds
#endif
radium_main(char *arg);

void init_radium(char *arg,PyObject *gkf){
	static bool started=false;

	if(started==true) return;

	started=true;

	if(!PyCallable_Check(gkf)){
		fprintf(stderr,"gotKey is definitely not gotKey. Check start.py\n");
		return;
	}

	Py_XINCREF(gkf);

	gotkeyFunc=gkf;

	radium_main(arg);

	Py_XDECREF(gkf);
}



/*******************************************
	Common operations
*******************************************/

static const char *g_error_message = NULL;

const char *pullErrorMessage(void){
  const char *message = g_error_message;
  g_error_message = NULL;
  return message;
}

static void printException(const char *message){
  fprintf(stderr, "SCHEME error. Message: \"%s\"\n", message);
  //s7extra_callFunc2_void_void("display-ow!"); // No point. ow! uses the errorlet, but we haven't thrown exception yet.
}

void printExceptionIfError(void){
  const char *message = pullErrorMessage();
  if (message != NULL)
    printException(message);
}

// Warning, is likely to cause a longjmp!
void throwExceptionIfError(void){
  const char *message = pullErrorMessage();
  if (message != NULL){
    printException(message);
    SCHEME_throw("radium-api", message);
  }
}

void clearErrorMessage(void){
  const char *message = pullErrorMessage();
  if(message != NULL){
    printf("********** Warning, the error \"%s\" was not handled by throwing an error.\n", message);
    message = NULL;
  }
}

void handleError_internal(const char *fmt,...){
  if (g_error_message != NULL)
    return;

  char message[1000];
  va_list argp;
  
  va_start(argp,fmt);
  vsnprintf(message,998,fmt,argp);
  va_end(argp);

  printException(message);

  printf("HISTORY:\n%s\n",SCHEME_get_history());
                     

  vector_t v = {0};

  int ok = VECTOR_push_back(&v, "Ok");
  (void)ok;
  int continue_ = VECTOR_push_back(&v, "Continue");

  int ret = GFX_Message(&v, message);

  // We don't want to throw here since the api code is not written with that in mind. Instead, we throw in 'throwExceptionIfError' above, which is called when exiting an api call.
  if (ret!=continue_)
    g_error_message = talloc_strdup(message);  
}

struct Tracker_Windows *getWindowFromNum(int windownum){
	if(windownum==-1) return root->song->tracker_windows;
	struct Tracker_Windows *ret = ListFindElement1_r0(&root->song->tracker_windows->l,(NInt)windownum);
        if (ret==NULL)
          handleError("Window #%d does not exist", windownum);
        return ret;
}


struct WBlocks *getWBlockFromNum(int windownum,int wblocknum){
	struct Tracker_Windows *window=getWindowFromNum(windownum);
	if(window==NULL) return NULL;

	if(wblocknum==-1) return window->wblock;
	struct WBlocks *ret = ListFindElement1_num(&window->wblocks->l,(NInt)wblocknum);
        if (ret==NULL)
          handleError("WBlock #%d does not exist", wblocknum);
        return ret;
}


struct WBlocks *getWBlockFromNumA(
	int windownum,
	struct Tracker_Windows **window,
	int blocknum
){
	*window=getWindowFromNum(windownum);
	if(*window==NULL) return NULL;

	if(blocknum==-1) return (*window)->wblock;
	struct WBlocks *ret = ListFindElement1_num(&(*window)->wblocks->l,(NInt)blocknum);
        if (ret==NULL)
          handleError("WBlock #%d does not exist", blocknum);
        return ret;
}

struct Blocks *getBlockFromNum(int blocknum){
  struct Blocks *ret = ListFindElement1_r0(&root->song->blocks->l,(NInt)blocknum);
  if (ret==NULL)
    handleError("Block #%d does not exist",blocknum);
  return ret;
}


struct Tracks *getTrackFromNum(int blocknum,int tracknum){
	struct Blocks *block=getBlockFromNum(blocknum);
	if(block==NULL) return NULL;

	struct Tracks *ret = ListFindElement1_num_r0(&block->tracks->l,(NInt)tracknum);
        if (ret==NULL)
          handleError("Track #%d in Block #%d does not exist",tracknum,blocknum);
        
        return ret;
}

struct WTracks *getWTrackFromNum(
	int windownum,
	int wblocknum,
	int wtracknum
){
	struct WBlocks *wblock=getWBlockFromNum(windownum,wblocknum);
	if(wblock==NULL) return NULL;
	if(wtracknum==-1) return wblock->wtrack;
	struct WTracks *ret = ListFindElement1_num_r0(&wblock->wtracks->l,(NInt)wtracknum);
        if (ret==NULL)
          handleError("WTrack #%d in WBlock #%d in window #%d does not exist",wtracknum, wblocknum, windownum);
        return ret;
}

struct WTracks *getWTrackFromNumA(
	int windownum,
	struct Tracker_Windows **window,
	int wblocknum,
	struct WBlocks **wblock,
	int wtracknum
){
	if(wblock==NULL){
		handleError("Warning, might be a program-error, wblock==NULL at function getWTrackFromNumA in file api/api_common.c\n");
		return NULL;
	}

      	(*wblock)=getWBlockFromNumA(windownum,window,wblocknum);
        if ((*wblock)==NULL)
          return NULL;
          
	if(wtracknum==-1) return (*wblock)->wtrack;
//	printf("So far.. %d,%d\n",wblocknum,wtracknum);
	struct WTracks *ret = ListFindElement1_num_r0(&(*wblock)->wtracks->l,(NInt)wtracknum);
        if (ret==NULL)
          handleError("WTrack #%d in WBlock %d does not exist",wtracknum, wblocknum);
        return ret;
}

                             
struct Notes *getNoteFromNumA(int windownum,struct Tracker_Windows **window, int blocknum, struct WBlocks **wblock, int tracknum, struct WTracks **wtrack, int notenum){
  (*wtrack) = getWTrackFromNumA(windownum, window, blocknum, wblock, tracknum);
  if ((*wtrack)==NULL)
    return NULL;

  if(notenum==-1){
    
    struct Notes *note = GetCurrNote(*window);
    if (note==NULL) {
      handleError("Note #%d in track #%d in block #%d in window #%d does not exist",notenum,tracknum,blocknum,windownum);
      return NULL;
    }else
      return note;
    
  }else{
    
    struct Tracks *track = (*wtrack)->track;
    struct Notes *ret = ListFindElement3_num_r0(&track->notes->l,(NInt)notenum);
    if (ret==NULL)
      handleError("Note #%d in track #%d in block #%d does not exist",notenum,tracknum,blocknum);
    
    return ret;
  }
}

struct Notes *getNoteFromNum(int windownum,int blocknum,int tracknum,int notenum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  return getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, notenum);
}

struct Pitches *getPitchFromNumA(int windownum,struct Tracker_Windows **window, int blocknum, struct WBlocks **wblock, int tracknum, struct WTracks **wtrack, int notenum, struct Notes **note, int pitchnum){
  (*note) = getNoteFromNumA(windownum, window, blocknum, wblock, tracknum, wtrack, notenum);
  if ((*note)==NULL)
    return NULL;

  if (pitchnum==0)
    return NULL; // pitch 0 is the note itself.

  int num_pitches = ListFindNumElements3(&(*note)->pitches->l);
  if (pitchnum==num_pitches+1)
    return NULL; // last pitch
      
  struct Pitches *pitch = ListFindElement3_num_r0(&(*note)->pitches->l, pitchnum-1);
  if (pitch==NULL){
    handleError("There is no pitch %d in note %d in track %d in block %d",pitchnum,notenum,tracknum,blocknum);
    return NULL;
  }

  return pitch;
}

struct Pitches *getPitchFromNum(int windownum,int blocknum,int tracknum,int notenum,int pitchnum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note;
  return getPitchFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, notenum, &note, pitchnum);
}


struct Velocities *getVelocityFromNumA(int windownum,struct Tracker_Windows **window, int blocknum, struct WBlocks **wblock, int tracknum, struct WTracks **wtrack, int notenum, struct Notes **note, int velocitynum){
  (*note) = getNoteFromNumA(windownum, window, blocknum, wblock, tracknum, wtrack, notenum);
  if ((*note)==NULL)
    return NULL;

  if (velocitynum==0)
    return NULL; // velocity 0 is the note itself.

  int num_velocities = ListFindNumElements3(&(*note)->velocities->l);
  if (velocitynum==num_velocities+1)
    return NULL; // last velocity
      
  struct Velocities *velocity = ListFindElement3_num_r0(&(*note)->velocities->l, velocitynum-1);
  if (velocity==NULL){
    handleError("There is no velocity %d in note %d in track %d in block %d",velocitynum,notenum,tracknum,blocknum);
    return NULL;
  }

  return velocity;
}

struct Velocities *getVelocityFromNum(int windownum,int blocknum,int tracknum,int notenum,int velocitynum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note;
  return getVelocityFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, notenum, &note, velocitynum);
}


struct Signatures *getSignatureFromNumA(int windownum,struct Tracker_Windows **window, int blocknum, struct WBlocks **wblock, int num){
  (*wblock) = getWBlockFromNumA(windownum, window, blocknum);
  if ((*wblock)==NULL)
    return NULL;

  struct Blocks *block = (*wblock)->block;
  
  struct Signatures *ret = ListFindElement3_num_r0(&block->signatures->l,(NInt)num);
  if (ret==NULL)
      handleError("Signature #%d in block #%d does not exist",num,blocknum);
    
  return ret;
}

struct Signatures *getSignatureFromNum(int windownum,int blocknum,int num){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  return getSignatureFromNumA(windownum, &window, blocknum, &wblock, num);
}

struct LPBs *getLPBFromNumA(int windownum,struct Tracker_Windows **window, int blocknum, struct WBlocks **wblock, int num){
  (*wblock) = getWBlockFromNumA(windownum, window, blocknum);
  if ((*wblock)==NULL)
    return NULL;

  struct Blocks *block = (*wblock)->block;
  
  struct LPBs *ret = ListFindElement3_num_r0(&block->signatures->l,(NInt)num);
  if (ret==NULL)
      handleError("LPB #%d in block #%d does not exist",num,blocknum);
    
  return ret;
}

struct LPBs *getLPBFromNum(int windownum,int blocknum,int num){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  return getLPBFromNumA(windownum, &window, blocknum, &wblock, num);
}

struct BPMs *getBPMFromNumA(int windownum,struct Tracker_Windows **window, int blocknum, struct WBlocks **wblock, int bpmnum){
  (*wblock) = getWBlockFromNumA(windownum, window, blocknum);
  if ((*wblock)==NULL)
    return NULL;

  struct Blocks *block = (*wblock)->block;
  
  struct BPMs *ret = ListFindElement3_num_r0(&block->tempos->l,(NInt)bpmnum);
  if (ret==NULL)
      handleError("BPM #%d in block #%d does not exist",bpmnum,blocknum);
    
  return ret;
}

struct BPMs *getBPMFromNum(int windownum,int blocknum,int bpmnum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  return getBPMFromNumA(windownum, &window, blocknum, &wblock, bpmnum);
}

struct FXs *getFXsFromNumA(int windownum,struct Tracker_Windows **window, int blocknum, struct WBlocks **wblock, int tracknum, struct WTracks **wtrack, int fxnum){
  (*wtrack) = getWTrackFromNumA(windownum, window, blocknum, wblock, tracknum);
  if ((*wtrack)==NULL)
    return NULL;

  struct Tracks *track = (*wtrack)->track;
  struct FXs *ret = fxnum < 0 ? NULL : VECTOR_get_r0(&track->fxs,fxnum,"fxs");
  if (ret==NULL)
    handleError("FX #%d in track #%d in block #%d does not exist",fxnum,tracknum,blocknum);
  
  return ret;
}

struct FXs *getFXsFromNum(int windownum,int blocknum,int tracknum,int fxnum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  return getFXsFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, fxnum);
}

struct Patch *getPatchFromNum(int64_t instrument_id){
  struct Patch *patch = PATCH_get_from_id(instrument_id);
  if(patch==NULL)
    handleError("instrument %d not found", (int)instrument_id);
  
  return patch;
}

struct Patch *getAudioPatchFromNum(int64_t instrument_id){ // TODO: Rename to getAudioPatchFromId
  struct Patch *patch = PATCH_get_from_id(instrument_id);
  if(patch==NULL) {
    handleError("instrument %d not found", (int)instrument_id);
    return NULL;
  }
  
  if (patch->instrument != get_audio_instrument()) {
    handleError("instrument %d is not an audio instrument", (int)instrument_id);
    return NULL;
  }

  return patch;
}

struct SeqTrack *getSeqtrackFromNum(int seqtracknum){
  if (seqtracknum < 0 || seqtracknum >= root->song->seqtracks.num_elements){
    handleError("Sequencer track %d not found", seqtracknum);
    return NULL;
  }
  
  return root->song->seqtracks.elements[seqtracknum];
}

struct SeqBlock *getSeqblockFromNum(int seqblocknum, int seqtracknum){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return NULL;

  if (seqblocknum < 0 || seqblocknum >= seqtrack->seqblocks.num_elements){
    handleError("Sequencer block #%d not found in sequencer track %d", seqblocknum, seqtracknum);
    return NULL;
  }

  return seqtrack->seqblocks.elements[seqblocknum];
}

struct SeqBlock *getSeqblockFromNumA(int seqblocknum, int seqtracknum, struct SeqTrack **seqtrack){
  (*seqtrack) = getSeqtrackFromNum(seqtracknum);
  if ((*seqtrack)==NULL)
    return NULL;

  if (seqblocknum < 0 || seqblocknum >= (*seqtrack)->seqblocks.num_elements){
    handleError("Sequencer block #%d not found in sequencer track %d", seqblocknum, seqtracknum);
    return NULL;
  }

  return (*seqtrack)->seqblocks.elements[seqblocknum];
}

struct SeqBlock *getGfxGfxSeqblockFromNumA(int seqblocknum, int seqtracknum, struct SeqTrack **seqtrack){
  (*seqtrack) = getSeqtrackFromNum(seqtracknum);
  if ((*seqtrack)==NULL)
    return NULL;

  if (seqblocknum < 0 || seqblocknum >= (*seqtrack)->gfx_gfx_seqblocks.num_elements){
    handleError("Sequencer gfx block #%d not found in sequencer track %d", seqblocknum, seqtracknum);
    return NULL;
  }

  return (*seqtrack)->gfx_gfx_seqblocks.elements[seqblocknum];
}


// NOTES

static const Place *getPrevLegalNotePlace(const struct Tracks *track, const struct Notes *note){
  const Place *end = PlaceGetFirstPos(); // small bug here, cant move pitch to first position, only almost to first position.

  struct Notes *prev = FindPrevNoteOnSameSubTrack(track, note);
  //printf("prev: %p. next(prev): %p, note: %p, next(note): %p\n",prev,prev!=NULL?NextNote(prev):NULL,note,NextNote(note));
  
  if (prev != NULL) {
    end = &prev->l.p;
    if (prev->velocities!=NULL)
      end = ListLastPlace3(&prev->velocities->l);
    if (prev->pitches!=NULL)
      end = PlaceMax(end, ListLastPlace3(&prev->pitches->l));
  }
  
  return end;
}

static const Place *getNextLegalNotePlace(const struct Notes *note){
  const Place *end = &note->end;

  if (note->velocities != NULL)
    end = PlaceMin(end, &note->velocities->l.p);

  if (note->pitches != NULL)
    end = PlaceMin(end, &note->pitches->l.p);

  return end;
}


void MoveEndNote(struct Blocks *block, struct Tracks *track, struct Notes *note, const Place *place, bool last_legal_may_be_next_note){
  Place firstLegal, lastLegal;

  if (last_legal_may_be_next_note && !ControlPressed()){
    
    struct Notes *next = FindNextNoteOnSameSubtrack(note);
  
    if (next!=NULL)
      PlaceCopy(&lastLegal, &next->l.p);
    else
      PlaceSetLastPos(block, &lastLegal);

  }else{
    
    PlaceSetLastPos(block, &lastLegal);
    
  }

  
  const Place *last_pitch = ListLastPlace3(&note->pitches->l);
  const Place *last_velocity = ListLastPlace3(&note->velocities->l);
  const Place *startPlace = &note->l.p;

  if (last_pitch==NULL)
    last_pitch = startPlace;
  if (last_velocity==NULL)
    last_velocity = startPlace;

  const Place *firstLegalConst = PlaceMax(last_pitch, last_velocity);
  PlaceFromLimit(&firstLegal, firstLegalConst);

  {
    SCOPED_PLAYER_LOCK_IF_PLAYING();
    note->end = *PlaceBetween(&firstLegal, place, &lastLegal);
    NOTE_validate(block, track, note);
  }
    
  R_ASSERT(PlaceLessOrEqual(&note->end, &lastLegal));
}

int MoveNote(struct Blocks *block, struct Tracks *track, struct Notes *note, Place *place, bool replace_note_ends){
  Place old_place = note->l.p;

  if (!PlaceEqual(&old_place, place)) {

    //printf("MoveNote. old: %f, new: %f\n", GetfloatFromPlace(&old_place), GetfloatFromPlace(place));
         
    if (PlaceLessThan(place, &old_place)) {
      const Place *prev_legal = getPrevLegalNotePlace(track, note);
      //printf("prev_legal: %f\n",GetfloatFromPlace(prev_legal));
      if (PlaceLessOrEqual(place, prev_legal))
        PlaceFromLimit(place, prev_legal);
    } else {
      const Place *next_legal = getNextLegalNotePlace(note);
      if (PlaceGreaterOrEqual(place, next_legal))
        PlaceTilLimit(place, next_legal);
    }
    
    {
      SCOPED_PLAYER_LOCK_IF_PLAYING();
      
      ListRemoveElement3(&track->notes, &note->l);
      note->l.p = *place;
      ListAddElement3_a(&track->notes, &note->l);
      if (replace_note_ends && !ControlPressed())
        ReplaceNoteEnds(block, track, &old_place, place, note->polyphony_num);
      NOTE_validate(block, track, note);
    }

  }
  
  return ListPosition3(&track->notes->l, &note->l);
}


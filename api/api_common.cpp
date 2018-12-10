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

#define __STDC_FORMAT_MACROS 1

#include "../common/includepython.h"

#include <inttypes.h>

#include <QHash>
#include <QPair>


#include "../common/nsmtracker.h"
#include "../common/list_proc.h"
#include "../common/vector_proc.h"
#include "../common/placement_proc.h"
#include "../common/notes_proc.h"
#include "../common/instruments_proc.h"
#include "../common/patch_proc.h"
#include "../common/visual_proc.h"
#include "../common/seqtrack_proc.h"

#include "../Qt/helpers.h"

#include "../embedded_scheme/scheme_proc.h"
#include "../embedded_scheme/s7extra_proc.h"


#include "api_gui_proc.h"

#include "radium_proc.h"
#include "api_common_proc.h"


extern struct Root *root;



/* Hmm, well, okey, I put the init_radium function here. */


PyObject *gotkeyFunc=NULL;

int
#ifdef _AMIGA
__stdargs __saveds
#endif
radium_main(const char *arg);

void init_radium(const char *arg,PyObject *gkf){
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

bool g_is_going_to_call_throwExceptionIfError = false;

const char *pullErrorMessage(void){
  //const char *old = g_error_message;
  const char *message = g_error_message;
  g_error_message = NULL;
  //printf("pullErrorMessage: g: %p. Content: -%s-. Returned: %p. Content: -%s-\n", old, old, message, message);
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
  R_ASSERT_NON_RELEASE(g_scheme_nested_level > 0);

  R_ASSERT_NON_RELEASE(g_is_going_to_call_throwExceptionIfError==true);

  const char *message = pullErrorMessage();
  if (message != NULL){
    printException(message);
    SCHEME_throw("radium-api", message);
  }

  g_is_going_to_call_throwExceptionIfError = false;
}

void clearErrorMessage(void){
  const char *message = pullErrorMessage();
  if(message != NULL){
    printf("********** Warning, the error \"%s\" was not handled by throwing an error.\n", message);
    message = NULL;
  }
}

#define SHOW_DIALOG_WHEN_ERROR 0 // Turned out to be useless. Gives the user an option to select "Continue" instead of throwing a scheme exception, but I can't remember any time where that option would be useful. In addition, while the dialog was open, other scheme code could run in the mean time, which could cause very complex bugs (the same code that caused the bug could be called again for instance).

// We don't throw scheme exception here since the api code is not written with that in mind.
// Instead, we set the variable "g_error_message" and then the function 'throwExceptionIfError', which is called from a safe point, throws exception instead.
void handleError_internal(const char *fmt,...){
  /*
    It does happen when called from one of the s7extra_callFunc* functions, and the return value is wrong type.
    
    if (g_is_going_to_call_throwExceptionIfError==false){
      R_ASSERT(false); // Should not happen. A function has probably been called with the wrong 'error_type' argument.
      return;
    }
  */
  
  if (g_error_message != NULL)
    return;

  char message[1000];
  va_list argp;
  
  va_start(argp,fmt);
  vsnprintf(message,998,fmt,argp);
  va_end(argp);

  g_error_message = talloc_strdup(message); // Set this value before calling SCHEME_get_history nad GFX_Message, so that we won't get into an infinite loop.
    
  printException(message);

  const char *backtrace = SCHEME_get_history();
  puts(backtrace);

  GFX_addMessage(message);
  GFX_addMessage(backtrace);

  if (g_is_going_to_call_throwExceptionIfError==false)
    g_error_message = NULL; // If not, the next scheme function to run, whatever it might be, will throw exception.
  
  
    
#if SHOW_DIALOG_WHEN_ERROR
  
  static double last_time = 0;

  if (safe_to_run_exec() && (TIME_get_ms() - last_time) > 0) {

    vector_t v = {};
    
    int stop = VECTOR_push_back(&v, "Stop");
    (void)stop;
    int continue_ = VECTOR_push_back(&v, "Continue");
    int silent = VECTOR_push_back(&v, "Silent! (continue for 10 seconds)");

    int ret = GFX_Message(&v, "%s", message);

    g_error_message = NULL;
  
    if (ret==continue_ || ret==silent)
      g_error_message = NULL;

    last_time = TIME_get_ms();
    
    if (ret==silent)
      last_time += 10000;
    
  }
#endif
  
}

struct Tracker_Windows *getWindowFromNum(int windownum){
	if(windownum==-1) return root->song->tracker_windows;
	struct Tracker_Windows *ret = (struct Tracker_Windows *)ListFindElement1_r0(&root->song->tracker_windows->l,(NInt)windownum);
        if (ret==NULL)
          handleError("Window #%d does not exist", windownum);
        return ret;
}


struct WBlocks *getWBlockFromNum(int windownum,int wblocknum){
	struct Tracker_Windows *window=getWindowFromNum(windownum);
	if(window==NULL) return NULL;

	if(wblocknum==-1) return window->wblock;
	struct WBlocks *ret = (struct WBlocks *)ListFindElement1_num(&window->wblocks->l,(NInt)wblocknum);
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
	struct WBlocks *ret = (struct WBlocks *)ListFindElement1_num(&(*window)->wblocks->l,(NInt)blocknum);
        if (ret==NULL)
          handleError("WBlock #%d does not exist", blocknum);
        return ret;
}

struct Blocks *getBlockFromNum(int blocknum){
  struct Blocks *ret = (struct Blocks *)ListFindElement1_r0(&root->song->blocks->l,(NInt)blocknum);
  if (ret==NULL)
    handleError("Block #%d does not exist",blocknum);
  return ret;
}


struct Tracks *getTrackFromNum(int blocknum,int tracknum){
	struct Blocks *block=getBlockFromNum(blocknum);
	if(block==NULL) return NULL;

	struct Tracks *ret = (struct Tracks *)ListFindElement1_num_r0(&block->tracks->l,(NInt)tracknum);
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
	struct WTracks *ret = (struct WTracks *)ListFindElement1_num_r0(&wblock->wtracks->l,(NInt)wtracknum);
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
	struct WTracks *ret = (struct WTracks *)ListFindElement1_num_r0(&(*wblock)->wtracks->l,(NInt)wtracknum);
        if (ret==NULL)
          handleError("WTrack #%d in WBlock %d does not exist",wtracknum, wblocknum);
        return ret;
}

static struct Notes *getCurrNote(int windownum, struct Tracker_Windows **window, int blocknum, int tracknum){
  struct Notes *note = GetCurrNote(*window);
  if (note==NULL) {
    handleError("Current note in track #%d in block #%d in window #%d does not exist",tracknum,blocknum,windownum);
    return NULL;
  }else
    return note;
}
                             
struct Notes *getNoteFromNumA(int windownum,struct Tracker_Windows **window, int blocknum, struct WBlocks **wblock, int tracknum, struct WTracks **wtrack, dyn_t dynnote){
  (*wtrack) = getWTrackFromNumA(windownum, window, blocknum, wblock, tracknum);
  if ((*wtrack)==NULL)
    return NULL;

  if(dynnote.type==UNINITIALIZED_TYPE){

    return getCurrNote(windownum, window, blocknum, tracknum);
    
  } else {
    
    struct Tracks *track = (*wtrack)->track;

    if (dynnote.type==INT_TYPE){
      int notenum = (int)dynnote.int_number;
      if (notenum==-1)
        return getCurrNote(windownum, window, blocknum, tracknum);

      struct Notes *ret = (struct Notes *)ListFindElement3_num_r0(&track->notes->l,notenum);
      if (ret==NULL)
        handleError("Note #%d in track #%d in block #%d does not exist",notenum,tracknum,blocknum);
      return ret;

    } else if (dynnote.type==STRING_TYPE){

      const char *chars = STRING_get_chars(dynnote.string);
      if (chars[0]==0)
        return getCurrNote(windownum, window, blocknum, tracknum);

      int64_t note_id = atoll(chars);
      if (note_id==-1)
        return getCurrNote(windownum, window, blocknum, tracknum);
      if (note_id < 0){
        handleError("Illegal note id: \"%s\"", chars);
        return NULL;
      }

      struct Notes *note = track->notes;
      while(note != NULL){
        if (note->id==note_id)
          return note;
        note = NextNote(note);
      }

      handleError("Note with id %s in track #%d in block #%d note found",chars,tracknum,blocknum);
      return NULL;
      

    } else {
      handleError("Expected number or string for note, got %s", DYN_type_name(dynnote.type));
      return NULL;
    }

  }
}

struct Notes *getNoteFromNum(int windownum,int blocknum,int tracknum,dyn_t dynnote){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  return getNoteFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
}

struct Pitches *getPitchFromNumA(int windownum,struct Tracker_Windows **window, int blocknum, struct WBlocks **wblock, int tracknum, struct WTracks **wtrack, dyn_t dynnote, struct Notes **note, int pitchnum){
  (*note) = getNoteFromNumA(windownum, window, blocknum, wblock, tracknum, wtrack, dynnote);
  if ((*note)==NULL)
    return NULL;

  if (pitchnum==0)
    return NULL; // pitch 0 is the note itself.

  int num_pitches = ListFindNumElements3((struct ListHeader3*)(*note)->pitches);
  if (pitchnum==num_pitches+1)
    return NULL; // last pitch
      
  struct Pitches *pitch = (struct Pitches *)ListFindElement3_num_r0(&(*note)->pitches->l, pitchnum-1);
  if (pitch==NULL){
    handleError("There is no pitch #%d in note %d in track #%d in block #%d",pitchnum,(int)(*note)->id,tracknum,blocknum);
    return NULL;
  }

  return pitch;
}

struct Pitches *getPitchFromNum(int windownum,int blocknum,int tracknum,dyn_t dynnote,int pitchnum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note;
  return getPitchFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote, &note, pitchnum);
}


struct Velocities *getVelocityFromNumA(int windownum,struct Tracker_Windows **window, int blocknum, struct WBlocks **wblock, int tracknum, struct WTracks **wtrack, dyn_t dynnote, struct Notes **note, int velocitynum){
  (*note) = getNoteFromNumA(windownum, window, blocknum, wblock, tracknum, wtrack, dynnote);
  if ((*note)==NULL)
    return NULL;

  if (velocitynum==0)
    return NULL; // velocity 0 is the note itself.

  int num_velocities = ListFindNumElements3((struct ListHeader3*)(*note)->velocities);
  if (velocitynum==num_velocities+1)
    return NULL; // last velocity
      
  struct Velocities *velocity = (struct Velocities *)ListFindElement3_num_r0(&(*note)->velocities->l, velocitynum-1);
  if (velocity==NULL){
    handleError("There is no velocity #%d in note with id \"%d\" in track #%d in block #%d",velocitynum,(int)(*note)->id,tracknum,blocknum);
    return NULL;
  }

  return velocity;
}

struct Velocities *getVelocityFromNum(int windownum,int blocknum,int tracknum,dyn_t dynnote,int velocitynum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note;
  return getVelocityFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote, &note, velocitynum);
}


struct Signatures *getSignatureFromNumA(int windownum,struct Tracker_Windows **window, int blocknum, struct WBlocks **wblock, int num){
  (*wblock) = getWBlockFromNumA(windownum, window, blocknum);
  if ((*wblock)==NULL)
    return NULL;

  struct Blocks *block = (*wblock)->block;
  
  struct Signatures *ret = (struct Signatures *)ListFindElement3_num_r0(&block->signatures->l,(NInt)num);
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
  
  struct LPBs *ret = (struct LPBs *)ListFindElement3_num_r0(&block->signatures->l,(NInt)num);
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
  
  struct BPMs *ret = (struct BPMs *)ListFindElement3_num_r0(&block->tempos->l,(NInt)bpmnum);
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
  struct FXs *ret = fxnum < 0 ? NULL : (struct FXs *)VECTOR_get2(&track->fxs,fxnum,"fxs");
  if (ret==NULL)
    return NULL;
  
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

struct SeqTrack *getSeqtrackFromNum_R0(int seqtracknum){
  if (seqtracknum < 0 || seqtracknum >= root->song->seqtracks.num_elements)
    return NULL;
  
  return (struct SeqTrack *)root->song->seqtracks.elements[seqtracknum];
}

struct SeqTrack *getSeqtrackFromNum(int seqtracknum){
  auto *ret = getSeqtrackFromNum_R0(seqtracknum);
  
  if (ret==NULL)
    handleError("Sequencer track %d not found", seqtracknum);
  
  return ret;
}

struct SeqTrack *getAudioSeqtrackFromNum(int seqtracknum){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return NULL;

  if (seqtrack->for_audiofiles==false){
    handleError("Sequencer track %d is not for audio files", seqtracknum);
    return NULL;
  }
    
  return seqtrack;
}

struct SeqTrack *getBlockSeqtrackFromNum(int seqtracknum){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return NULL;

  if (seqtrack->for_audiofiles==true){
    handleError("Sequencer track %d is not for editor blocks", seqtracknum);
    return NULL;
  }
    
  return seqtrack;
}

struct SeqBlock *getSeqblockFromNum(int seqblocknum, int seqtracknum){
  struct SeqTrack *seqtrack = getSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return NULL;

  if (seqblocknum < 0 || seqblocknum >= seqtrack->seqblocks.num_elements){
    handleError("Seqblock #%d not found in seqtrack %d", seqblocknum, seqtracknum);
    return NULL;
  }

  return (struct SeqBlock *)seqtrack->seqblocks.elements[seqblocknum];
}

struct SeqBlock *getSeqblockFromNumA_R0(int seqblocknum, int seqtracknum, struct SeqTrack **seqtrack, bool use_gfx_if_possible){
  (*seqtrack) = getSeqtrackFromNum_R0(seqtracknum);
  if ((*seqtrack)==NULL){
    return NULL;
  }

  if (seqblocknum < 0 || seqblocknum >= gfx_seqblocks2(*seqtrack, use_gfx_if_possible)->num_elements){
    return NULL;
  }

  return (struct SeqBlock *)gfx_seqblocks2(*seqtrack, use_gfx_if_possible)->elements[seqblocknum];
}

struct SeqBlock *getSeqblockFromNumA(int seqblocknum, int seqtracknum, struct SeqTrack **seqtrack, bool use_gfx_if_possible){
  auto *ret = getSeqblockFromNumA_R0(seqblocknum, seqtracknum, seqtrack, use_gfx_if_possible);
  
  if(ret==NULL)
    handleError("Seqblock #%d not found in seqtrack %d", seqblocknum, seqtracknum);
  
  return ret;
}

struct SeqBlock *getAudioSeqblockFromNum(int seqblocknum, int seqtracknum){
  struct SeqTrack *seqtrack = getAudioSeqtrackFromNum(seqtracknum);
  if (seqtrack==NULL)
    return NULL;

  if (seqblocknum < 0 || seqblocknum >= seqtrack->seqblocks.num_elements){
    handleError("Seqblock #%d not found in seqtrack %d", seqblocknum, seqtracknum);
    return NULL;
  }

  struct SeqBlock *seqblock = (struct SeqBlock *)seqtrack->seqblocks.elements[seqblocknum];
  if (seqblock->block!=NULL){
    handleError("Seqblock #%d in seqtrack %d is not an audio file", seqblocknum, seqtracknum);
    return NULL;
  }

  return seqblock;
}

struct SeqBlock *getAudioSeqblockFromNumA(int seqblocknum, int seqtracknum, struct SeqTrack **seqtrack){
  (*seqtrack) = getAudioSeqtrackFromNum(seqtracknum);
  if ((*seqtrack)==NULL)
    return NULL;

  if (seqblocknum < 0 || seqblocknum >= (*seqtrack)->seqblocks.num_elements){
    handleError("Seqblock #%d not found in seqtrack %d", seqblocknum, seqtracknum);
    return NULL;
  }

  struct SeqBlock *seqblock = (struct SeqBlock *)(*seqtrack)->seqblocks.elements[seqblocknum];
  if (seqblock->block!=NULL){
    handleError("Seqblock #%d in seqtrack %d is not an audio file", seqblocknum, seqtracknum);
    return NULL;
  }

  return seqblock;
}


static QHash<const int64_t, QPair<int,int>> g_seqblock_ids_hash;

static void add_to_ids_hash(int64_t seqblock_id, int seqtracknum, int seqblocknum){

#if !defined(RELEASE)
  printf("add_to_ids_hash[%d]: <%d,%d>. Contains: %d. size: %d\n", (int)seqblock_id, seqtracknum, seqblocknum, g_seqblock_ids_hash.contains(seqblock_id), g_seqblock_ids_hash.size());
#endif
  
  // First check if we should clear the hash table if it hasn grown a lot.
  // Currently, this is very unlikely to happen, but maybe something changes in the future.
  if (g_seqblock_ids_hash.size() >= 10000){    
    printf(" CLEARING: %d\n", g_seqblock_ids_hash.size());
    R_ASSERT_NON_RELEASE(false);
    g_seqblock_ids_hash.clear();
  }

  g_seqblock_ids_hash[seqblock_id] = QPair<int, int>(seqtracknum, seqblocknum);
}

static struct SeqBlock *get_seqblock_from_id_a(int64_t seqblock_id, struct SeqTrack **seqtrack, bool use_gfx, int &seqblocknum, int &seqtracknum, bool throw_error = true){
  const auto &pair = g_seqblock_ids_hash[seqblock_id];
  seqtracknum = pair.first;
  seqblocknum = pair.second;

  struct SeqBlock *seqblock = getSeqblockFromNumA_R0(seqblocknum, seqtracknum, seqtrack, use_gfx);

  if (seqblock==NULL || seqblock->id != seqblock_id){

    VECTOR_FOR_EACH(struct SeqTrack *, seqtrack2, &root->song->seqtracks){
      seqtracknum = iterator666;
      
      VECTOR_FOR_EACH(struct SeqBlock *, seqblock, use_gfx ? gfx_seqblocks(seqtrack2) : &seqtrack2->seqblocks){
        seqblocknum = iterator666;
        
        if (seqblock->id == seqblock_id){
          
          *seqtrack = seqtrack2;

          add_to_ids_hash(seqblock_id, seqtracknum, seqblocknum);

          return seqblock;
          
        }
      }END_VECTOR_FOR_EACH;
    }END_VECTOR_FOR_EACH;

    if(throw_error)
      handleError("Sequencer block with id #%d not found", (int)seqblock_id);
    
    return NULL;
    
  } else {
  
    return seqblock;

  }
}

extern struct SeqBlock *getSeqblockFromIdB(int64_t seqblock_id, struct SeqTrack **seqtrack, int &seqblocknum, int &seqtracknum, bool throw_error){
  return get_seqblock_from_id_a(seqblock_id, seqtrack, false, seqblocknum, seqtracknum, throw_error);
}

extern struct SeqBlock *getGfxSeqblockFromIdB(int64_t seqblock_id, struct SeqTrack **seqtrack, int &seqblocknum, int &seqtracknum, bool throw_error){
  return get_seqblock_from_id_a(seqblock_id, seqtrack, true, seqblocknum, seqtracknum, throw_error);
}

struct SeqBlock *getSeqblockFromIdA(int64_t seqblock_id, struct SeqTrack **seqtrack){
  int seqblocknum, seqtracknum;
  return get_seqblock_from_id_a(seqblock_id, seqtrack, false, seqblocknum, seqtracknum);
}
struct SeqBlock *getGfxSeqblockFromIdA(int64_t seqblock_id, struct SeqTrack **seqtrack){
  int seqblocknum, seqtracknum;
  return get_seqblock_from_id_a(seqblock_id, seqtrack, true, seqblocknum, seqtracknum);
}
                                      
struct SeqBlock *getSeqblockFromId(int64_t seqblock_id){
  struct SeqTrack *seqtrack;
  return getSeqblockFromIdA(seqblock_id, &seqtrack);
}

struct SeqBlock *getGfxSeqblockFromId(int64_t seqblock_id){
  struct SeqTrack *seqtrack;
  return getGfxSeqblockFromIdA(seqblock_id, &seqtrack);
}

static struct SeqBlock *get_audio_seqblock_from_id_a(int64_t seqblock_id, struct SeqTrack **seqtrack, bool use_gfx){
  int seqblocknum, seqtracknum;
    
  struct SeqBlock *seqblock = get_seqblock_from_id_a(seqblock_id, seqtrack, use_gfx, seqblocknum, seqtracknum);
  if (seqblock==NULL)
    return NULL;

  if (seqblock->block!=NULL){
    handleError("Sequencer block with id #%d (seqblock #%d in seqtrack #%d) is not an audio file", (int)seqblock_id, seqblocknum, seqtracknum);
    return NULL;
  }

  return seqblock;
}

struct SeqBlock *getAudioSeqblockFromIdA(int64_t seqblock_id, struct SeqTrack **seqtrack){
  return get_audio_seqblock_from_id_a(seqblock_id, seqtrack, false);
}

struct SeqBlock *getAudioGfxSeqblockFromIdA(int64_t seqblock_id, struct SeqTrack **seqtrack){
  return get_audio_seqblock_from_id_a(seqblock_id, seqtrack, true);
}

struct SeqBlock *getAudioSeqblockFromId(int64_t seqblock_id){
  struct SeqTrack *seqtrack;
  return getAudioSeqblockFromIdA(seqblock_id, &seqtrack);
}

struct SeqBlock *getAudioGfxSeqblockFromId(int64_t seqblock_id){
  struct SeqTrack *seqtrack;
  return getAudioGfxSeqblockFromIdA(seqblock_id, &seqtrack);
}


struct SeqBlock *getGfxSeqblockFromNumA(int seqblocknum, int seqtracknum, struct SeqTrack **seqtrack){
  (*seqtrack) = getSeqtrackFromNum(seqtracknum);
  if ((*seqtrack)==NULL)
    return NULL;

  const vector_t *seqblocks = gfx_seqblocks(*seqtrack);
  
  if (seqblocknum < 0 || seqblocknum >= seqblocks->num_elements){
    handleError("Sequencer gfx block #%d not found in sequencer track %d", seqblocknum, seqtracknum);
    return NULL;
  }

  return (struct SeqBlock *)seqblocks->elements[seqblocknum];
}

struct SeqBlock *getGfxSeqblockFromNum(int seqblocknum, int seqtracknum){  
  struct SeqTrack *seqtrack;
  return getGfxSeqblockFromNumA(seqblocknum, seqtracknum, &seqtrack);
}


struct SeqBlock *getGfxGfxSeqblockFromNumA(int seqblocknum, int seqtracknum, struct SeqTrack **seqtrack){
  (*seqtrack) = getSeqtrackFromNum(seqtracknum);
  if ((*seqtrack)==NULL)
    return NULL;

  if (seqblocknum < 0 || seqblocknum >= (*seqtrack)->gfx_gfx_seqblocks.num_elements){
    handleError("Sequencer gfx gfx block #%d not found in sequencer track %d", seqblocknum, seqtracknum);
    return NULL;
  }

  return (struct SeqBlock *)(*seqtrack)->gfx_gfx_seqblocks.elements[seqblocknum];
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


const char* GetNoteIdAsCharString(int64_t note_id){
  return talloc_format("%" PRId64, note_id);
}

dyn_t GetNoteIdFromNoteId(int64_t note_id){
  return DYN_create_string_from_chars(GetNoteIdAsCharString(note_id));
}

dyn_t GetNoteId(struct Notes *note){
  return GetNoteIdFromNoteId(note->id);
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

  
  const Place *last_pitch = ListLastPlace3((struct ListHeader3*)note->pitches);
  const Place *last_velocity = ListLastPlace3((struct ListHeader3*)note->velocities);
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

dyn_t MoveNote(struct Blocks *block, struct Tracks *track, struct Notes *note, Place *place, bool replace_note_ends){
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

  return GetNoteId(note);
}


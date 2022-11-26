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
#include "../common/TimeData.hpp"
#include "../common/list_proc.h"
#include "../common/vector_proc.h"
#include "../common/placement_proc.h"
#include "../common/notes_proc.h"
#include "../common/instruments_proc.h"
#include "../common/patch_proc.h"
#include "../common/visual_proc.h"
#include "../common/sequencer_proc.h"

#include "../Qt/helpers.h"

#include "../embedded_scheme/scheme_proc.h"
#include "../embedded_scheme/s7extra_proc.h"


#include "api_gui_proc.h"

#include "radium_proc.h"
#include "api_common_proc.h"


const char *g_last_api_entry_func_name = NULL;

bool g_ignore_s_is_calling = false; // debugging (radium_s7_wrap.c)
bool g_endless_recursion = false;


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
  R_ASSERT_NON_RELEASE(g_is_going_to_call_throwExceptionIfError==true);

  static int s_counter = 0;

  if (((s_counter++) % 1024)==0) {

    //printf("\n\n\nn\n\n\n--------------------------------Checking=============================!\n\n\n\n\n\n");
    //getchar();
    
    s_counter = 1;
    
    if (g_empty_dynvec.num_elements != 0){
      R_ASSERT_NON_RELEASE(false);
      dynvec_t *dynvec = (dynvec_t*)&g_empty_dynvec;
      dynvec->num_elements = 0;
      R_ASSERT(false);
    }
    
    if (g_empty_dynvec_dyn.type != ARRAY_TYPE){
      dyn_t *dyn = (dyn_t*)&g_empty_dynvec_dyn;
      dyn->type = ARRAY_TYPE;
      R_ASSERT(false);
    }
    
    if (g_empty_dynvec_dyn.array != &g_empty_dynvec){
      dyn_t *dyn = (dyn_t*)&g_empty_dynvec_dyn;
      dyn->array = (dynvec_t*)&g_empty_dynvec;
      R_ASSERT(false);
    }
    
    if (g_uninitialized_dyn.type != UNINITIALIZED_TYPE) {
      dyn_t *dyn = (dyn_t*)&g_uninitialized_dyn;
      dyn->type = UNINITIALIZED_TYPE;
      R_ASSERT(false);
    }
    
    if (g_dyn_false.type != BOOL_TYPE) {
      dyn_t *dyn = (dyn_t*)&g_dyn_false;
      dyn->type = BOOL_TYPE;
      R_ASSERT(false);
    }
    
    if (g_dyn_true.type != BOOL_TYPE) {
      dyn_t *dyn = (dyn_t*)&g_dyn_true;
      dyn->type = BOOL_TYPE;
      R_ASSERT(false);
    }
    
    R_ASSERT(g_dyn_false.bool_number==false);
    R_ASSERT(g_dyn_true.bool_number==true);
    
    R_ASSERT(g_dyn_minus_one.type==INT_TYPE);
    R_ASSERT(g_dyn_minus_one.int_number==-1);
    
  }
  
  if(g_scheme_nested_level==0){
    R_ASSERT_NON_RELEASE(false);
    return;
  }

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
#if !defined(RELEASE)
    printf("********************* Press return to continue ***********************\n");
    getchar();
    //abort();
#endif
    message = NULL;
  }
}

#define SHOW_DIALOG_WHEN_ERROR 0 // Turned out to be useless. Gives the user an option to select "Continue" instead of throwing a scheme exception, but I can't remember any time where that option would be useful. In addition, while the dialog was open, other scheme code could run in the mean time, which could cause very complex bugs (the same code that caused the bug could be called again for instance).

int64_t g_num_calls_to_handleError = 0;

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
  R_ASSERT_RETURN_IF_FALSE(THREADING_is_main_thread());
    
  bool is_called_from_scheme = g_scheme_nested_level > 0;
  
  g_num_calls_to_handleError++;

  if (g_error_message != NULL && is_called_from_scheme)
    return;

  bool is_going_to_call_throwExceptionIfError = g_is_going_to_call_throwExceptionIfError;
    
  char message[1000];
  va_list argp;
  
  va_start(argp,fmt);
  vsnprintf(message,998,fmt,argp);
  va_end(argp);

  if (is_called_from_scheme)
    g_error_message = talloc_format("<br>\nLast API entry: %s.<br>\nError: %s", g_last_api_entry_func_name, getHtmlFromText(message));
  //g_error_message = talloc_strdup(message); // Set this value before calling SCHEME_get_history and GFX_Message, so that we won't get into an infinite loop.
    
  printException(message);


  if (!is_called_from_scheme) {

    const char *backtrace = SCHEME_get_history();
    puts(backtrace);

    const char *message2 = V_strdup(talloc_format("%s:%s\n", message, backtrace));
    
    QTimer::singleShot(3,[message2]{
        GFX_addMessage("%s", message2);
        V_free((void*)message2);
      });
  }
  
  if (is_going_to_call_throwExceptionIfError==false)
    g_error_message = NULL; // If not, the next scheme function to run, whatever it might be, will throw exception.
  
  R_ASSERT_NON_RELEASE(is_going_to_call_throwExceptionIfError == g_is_going_to_call_throwExceptionIfError);
  
    
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
#ifdef DO_LATER
  struct Notes *note = GetCurrNote(*window);
#else
  struct Notes *note = NULL;
#endif
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


static const r::NotePtr getCurrNote2(int windownum, struct Tracker_Windows **window, int blocknum, int tracknum){
  const r::NotePtr note = GetCurrNote(*window);
  
  if (!note)
    handleError("Current note in track #%d in block #%d in window #%d does not exist",tracknum,blocknum,windownum);

  return note;
}

const r::NotePtr getNoteFromNumA2(int windownum, struct Tracker_Windows **window,
                                  int blocknum, struct WBlocks **wblock,
                                  int tracknum,
                                  const r::NoteTimeData::Reader &reader,
                                  dyn_t dynnote){
  if (dynnote.type==INT_TYPE){
    int notenum = (int)dynnote.int_number;
    if (notenum==-1)
      return getCurrNote2(windownum, window, blocknum, tracknum);
    
    if (reader.size() <= notenum){
      handleError("Note #%d in track #%d in block #%d does not exist",notenum,tracknum,blocknum);
      return r::NotePtr();
    }
    
    return reader.at_ref(notenum);
    
  } else if (dynnote.type==STRING_TYPE){
    
    const char *chars = STRING_get_chars(dynnote.string);
    if (chars[0]==0)
      return getCurrNote2(windownum, window, blocknum, tracknum);
    
    int64_t note_id = atoll(chars);
    if (note_id==-1)
      return getCurrNote2(windownum, window, blocknum, tracknum);
    if (note_id < 0){
      handleError("Illegal note id: \"%s\"", chars);
      return r::NotePtr();
    }
    
    for(const r::NotePtr &note : reader)
      if (note->_id==note_id)
        return note;
    
    handleError("Note with id %s in track #%d in block #%d note found",chars,tracknum,blocknum);
    return r::NotePtr();
     
  } else {
    handleError("Expected number or string for note, got %s", DYN_type_name(dynnote.type));
    return r::NotePtr();
  }
}
  
const r::NotePtr getNoteFromNumA2(int windownum,struct Tracker_Windows **window,
                                  int blocknum, struct WBlocks **wblock, int tracknum,
                                  struct WTracks **wtrack,
                                  dyn_t dynnote){
  
  ASSERT_IS_NONRT_MAIN_THREAD_NON_RELEASE(); // Must provide NoteTimeData::ReaderWriter& to use it outside the main thread (to ensure it's not deleted while using it).
    
  (*wtrack) = getWTrackFromNumA(windownum, window, blocknum, wblock, tracknum);
  if ((*wtrack)==NULL)
    return r::NotePtr();

  if(dynnote.type==UNINITIALIZED_TYPE){

    return getCurrNote2(windownum, window, blocknum, tracknum);
    
  } else {
    
    struct Tracks *track = (*wtrack)->track;

    const r::NoteTimeData::Reader reader(track->_notes2);
     
    return getNoteFromNumA2(windownum, window, blocknum, wblock, tracknum, reader, dynnote);
  }
}

const r::NotePtr getNoteFromNum2(int windownum,int blocknum,int tracknum,dyn_t dynnote){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  return getNoteFromNumA2(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote);
}


r::Pitch getPitchFromNumA(int windownum,
                          struct Tracker_Windows **window,
                          int blocknum,
                          struct WBlocks **wblock,
                          int tracknum,
                          struct WTracks **wtrack,
                          dyn_t dynnote,
                          struct Notes **retnote,
                          int pitchnum)
{
  *retnote = NULL;
  
  struct Notes *note = getNoteFromNumA(windownum, window, blocknum, wblock, tracknum, wtrack, dynnote);
  if (note==NULL)
    return r::Pitch(make_ratio(0,1),1,1);
  
  const r::PitchTimeData::Reader reader(note->_pitches);
  
  int num_pitches = reader.size() + 2;

  if (pitchnum < 0 || pitchnum >= num_pitches){
    handleError("There is no pitch #%d in note with id \"%d\" in track #%d in block #%d",pitchnum,(int)note->id,tracknum,blocknum);
    return r::Pitch(make_ratio(0,1),1,1);
  }

  *retnote = note;

  if (pitchnum==0)
    return r::Pitch(place2ratio(note->l.p), note->note, note->pitch_first_logtype, note->chance);

  if (pitchnum==num_pitches-1)
    return r::Pitch(note->end, note->pitch_end);
  
  return reader.at(pitchnum-1);
}

r::Pitch getPitchFromNumA2(int windownum,
                           struct Tracker_Windows **window,
                           int blocknum,
                           struct WBlocks **wblock,
                           int tracknum,
                           struct WTracks **wtrack,
                           dyn_t dynnote,
                           r::NotePtr &note,
                           int pitchnum)
{
  note = getNoteFromNumA2(windownum, window, blocknum, wblock, tracknum, wtrack, dynnote);
  if (!note)
    return r::Pitch(make_ratio(0,1),1,1);
  
  const r::PitchTimeData::Reader reader(&note->_pitches);
  
  int num_pitches = reader.size() + 2;

  if (pitchnum < 0 || pitchnum >= num_pitches){
    handleError("There is no pitch #%d in note with id \"%d\" in track #%d in block #%d",pitchnum,(int)note->_id,tracknum,blocknum);
    return r::Pitch(make_ratio(0,1),1,1);
  }

  if (pitchnum==0)
    return r::Pitch(note->get_time(), note->_val, note->d._pitch_first_logtype, note->d._chance);

  if (pitchnum==num_pitches-1)
    return r::Pitch(note->d._end, note->d._pitch_end);
  
  return reader.at(pitchnum-1);
}

 /*
struct Pitches *getPitchFromNum(int windownum,int blocknum,int tracknum,dyn_t dynnote,int pitchnum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note;
  return getPitchFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote, &note, pitchnum);
}
 */

r::Velocity getVelocityFromNumA(int windownum,
                                struct Tracker_Windows **window,
                                int blocknum,
                                struct WBlocks **wblock,
                                int tracknum,
                                struct WTracks **wtrack,
                                dyn_t dynnote,
                                struct Notes **retnote,
                                int velocitynum)
{
  *retnote = NULL;
  
  struct Notes *note = getNoteFromNumA(windownum, window, blocknum, wblock, tracknum, wtrack, dynnote);
  if (note==NULL)
    return r::Velocity(make_ratio(0,1),1,1);
  
  const r::VelocityTimeData::Reader reader(note->_velocities);
  
  int num_velocities = reader.size() + 2;

  if (velocitynum < 0 || velocitynum >= num_velocities){
    handleError("There is no velocity #%d in note with id \"%d\" in track #%d in block #%d",velocitynum,(int)note->id,tracknum,blocknum);
    return r::Velocity(make_ratio(0,1),1,1);
  }

  *retnote = note;

  if (velocitynum==0)
    return r::Velocity(place2ratio(note->l.p), note->velocity, note->velocity_first_logtype);

  if (velocitynum==num_velocities-1)
    return r::Velocity(note->end, note->velocity_end);
  
  return reader.at(velocitynum-1);
}

r::Velocity getVelocityFromNumA2(int windownum,
                                 struct Tracker_Windows **window,
                                 int blocknum,
                                 struct WBlocks **wblock,
                                 int tracknum,
                                 struct WTracks **wtrack,
                                 dyn_t dynnote,
                                 r::NotePtr &note,
                                 int velocitynum)
{
  note = getNoteFromNumA2(windownum, window, blocknum, wblock, tracknum, wtrack, dynnote);
  
  if (!note)
    return r::Velocity(make_ratio(0,1),1,1);
  
  const r::VelocityTimeData::Reader reader(&note->_velocities);
  
  int num_velocities = reader.size() + 2;

  if (velocitynum < 0 || velocitynum >= num_velocities){
    handleError("There is no velocity #%d in note with id \"%d\" in track #%d in block #%d",velocitynum,(int)note->_id,tracknum,blocknum);
    return r::Velocity(make_ratio(0,1),1,1);
  }

  if (velocitynum==0)
    return r::Velocity(note->get_time(), note->d._velocity, note->d._velocity_first_logtype);

  if (velocitynum==num_velocities-1)
    return r::Velocity(note->d._end, note->d._velocity_end);
  
  return reader.at(velocitynum-1);
}

/*
r::Velocity getVelocityFromNum(int windownum,int blocknum,int tracknum,dyn_t dynnote,int velocitynum){
  struct Tracker_Windows *window;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  struct Notes *note;
  return getVelocityFromNumA(windownum, &window, blocknum, &wblock, tracknum, &wtrack, dynnote, &note, velocitynum);
}
*/

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

struct Patch *getPatchFromNum(instrument_t instrument_id){
  struct Patch *patch = instrument_id==make_instrument(-1) ? PATCH_get_current() : PATCH_get_from_id(instrument_id);
  if(patch==NULL)
    handleError("instrument %d not found", (int)instrument_id.id);
  
  return patch;
}

struct Patch *getAudioPatchFromNum(instrument_t instrument_id){ // TODO: Rename to getAudioPatchFromId
  struct Patch *patch = instrument_id==make_instrument(-1) ? PATCH_get_current() : PATCH_get_from_id(instrument_id);
  if(patch==NULL) {
    handleError("instrument %d not found", (int)instrument_id.id);
    return NULL;
  }
  
  if (patch->instrument != get_audio_instrument()) {
    handleError("instrument %d is not an audio instrument", (int)instrument_id.id);
    return NULL;
  }

  if (patch->patchdata==NULL){
    R_ASSERT_NON_RELEASE(false);
    return NULL;
  }
  
  return patch;
}

struct SeqTrack *getSeqtrackFromNum_R0(int seqtracknum){
  if (seqtracknum==-1)
    return SEQUENCER_get_curr_seqtrack();
  
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
  if (seqblock_id==-1)
    seqblock_id = g_curr_seqblock_id;
  
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

struct SeqBlock *getSeqblockFromIdA(int64_t seqblock_id, struct SeqTrack **seqtrack, bool throw_error){
  int seqblocknum, seqtracknum;
  return get_seqblock_from_id_a(seqblock_id, seqtrack, false, seqblocknum, seqtracknum, throw_error);
}
struct SeqBlock *getGfxSeqblockFromIdA(int64_t seqblock_id, struct SeqTrack **seqtrack){
  int seqblocknum, seqtracknum;
  return get_seqblock_from_id_a(seqblock_id, seqtrack, true, seqblocknum, seqtracknum);
}
                                      
struct SeqBlock *getSeqblockFromId(int64_t seqblock_id){
  struct SeqTrack *seqtrack;
  return getSeqblockFromIdA(seqblock_id, &seqtrack);
}
                                      
struct SeqBlock *getEditorSeqblockFromId(int64_t seqblock_id){
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock = getSeqblockFromIdA(seqblock_id, &seqtrack);
  
  if (seqblock==NULL)
    return NULL;
  
  if (seqblock->block==NULL){
    handleError("Seqblock with id #%d is not an editor seqblock", (int)seqblock_id);
    return NULL;
  }

  return seqblock;
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

static const Place getPrevLegalNotePlace(const struct Tracks *track, const struct Notes *note){
  Place end = *PlaceGetFirstPos(); // small bug here, cant move pitch to first position, only almost to first position.

  struct Notes *prev = FindPrevNoteOnSameSubTrack(track, note);
  //printf("prev: %p. next(prev): %p, note: %p, next(note): %p\n",prev,prev!=NULL?NextNote(prev):NULL,note,NextNote(note));
  
  if (prev != NULL) {
    end = prev->l.p;

    {
      const r::VelocityTimeData::Reader reader(note->_velocities);
      
      if (reader.size() > 0)
        end = ratio2place(reader.at_last()._time);
    }

    {
      const r::PitchTimeData::Reader reader(note->_pitches);

      if (reader.size() > 0){
        Place p = ratio2place(reader.at_last()._time);
        end = *PlaceMax(&end, &p);
      }
    }
  }
  
  return end;
}

static const Ratio getPrevLegalNoteRatio(const radium::Vector<r::NotePtr> &notes, const r::NotePtr &note){
  Ratio end = make_ratio(0,1);

  const r::NotePtr prev = FindPrevNoteOnSameSubTrack2(notes, note);
  //printf("prev: %p. next(prev): %p, note: %p, next(note): %p\n",prev,prev!=NULL?NextNote(prev):NULL,note,NextNote(note));
  
  if (prev) {
    end = prev->get_time();

    {
      const r::VelocityTimeData::Reader reader(&note->_velocities);
      
      if (reader.size() > 0)
        end = reader.at_last()._time;
    }

    {
      const r::PitchTimeData::Reader reader(&note->_pitches);

      if (reader.size() > 0)
        end = R_MAX(end, reader.at_last()._time);
    }
  }
  
  return end;
}

static const Place getNextLegalNotePlace(const struct Notes *note){
  Place end = ratio2place(note->end);

  {
    const r::VelocityTimeData::Reader reader(note->_velocities);
    
    if (reader.size() > 0) {
      Place first_vel = ratio2place(reader.at_first()._time);    
      end = *PlaceMin(&end, &first_vel);
    }
  }

  {
    const r::PitchTimeData::Reader reader(note->_pitches);
    
    if (reader.size() > 0) {
      Place first_vel = ratio2place(reader.at_first()._time);    
      end = *PlaceMin(&end, &first_vel);
    }
  }

  return end;
}

static const Ratio getNextLegalNoteRatio(const r::NotePtr &note){
  Ratio end = note->d._end;

  {
    const r::VelocityTimeData::Reader reader(&note->_velocities);
    
    if (reader.size() > 0)
      end = R_MIN(reader.at_first()._time, end);
  }

  {
    const r::PitchTimeData::Reader reader(&note->_pitches);
    
    if (reader.size() > 0)
      end = R_MIN(reader.at_first()._time, end);
  }

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

  Place das_last_velocity;
  const r::VelocityTimeData::Reader velocity_reader(note->_velocities);
  int num_velocities = velocity_reader.size();
  
  Place das_last_pitch;
  const r::PitchTimeData::Reader pitch_reader(note->_pitches);
  int num_pitches = pitch_reader.size();
  
  const Place *startPlace = &note->l.p;

  const Place *last_pitch = NULL;
  if (num_pitches==0)
    last_pitch = startPlace;
  else {
    das_last_pitch = ratio2place(pitch_reader.at_last()._time);
    last_pitch = &das_last_pitch;
  }

  const Place *last_velocity = NULL; //ListLastPlace3((struct ListHeader3*)note->velocities);
  if (num_velocities==0)
    last_velocity = startPlace;
  else {
    das_last_velocity = ratio2place(velocity_reader.at_last()._time);
    last_velocity = &das_last_velocity;
  }
  
  const Place *firstLegalConst = PlaceMax(last_pitch, last_velocity);
  PlaceFromLimit(&firstLegal, firstLegalConst);

  {
    SCOPED_PLAYER_LOCK_IF_PLAYING();
    Place new_end = *PlaceBetween(&firstLegal, place, &lastLegal);
    note->end = place2ratio(new_end);
    NOTE_validate(block, track, note);
  }
    
  R_ASSERT(note->end <= place2ratio(lastLegal));
}

int64_t MoveEndNote2(struct Blocks *block, struct Tracks *track, r::NotePtr &note, const Ratio &place, bool last_legal_may_be_next_note){

  r::NoteTimeData::Writer writer(track->_notes2);

  if (!writer.contains(note)){
    R_ASSERT(false); // at least at the time of writing, this would probably be an error.
    return -1;
  }

  Ratio lastLegal = make_ratio(block->num_lines, 1);

  if (last_legal_may_be_next_note && !ControlPressed()){

    r::NotePtr next = FindNextNoteOnSameSubtrack2(writer.get_vector(), note);
  
    if (next)
      lastLegal = next->get_time();
  }

  Ratio startPlace = note->get_time();

  Ratio last_pitch;

  {
    const r::PitchTimeData::Reader pitch_reader(&note->_pitches);
    int num_pitches = pitch_reader.size();
    
    if (num_pitches==0)
      last_pitch = startPlace;
    else
      last_pitch = pitch_reader.at_last()._time;
  }

  Ratio last_velocity;

  {
    const r::VelocityTimeData::Reader velocity_reader(&note->_velocities);
    int num_velocities = velocity_reader.size();
    
    if (num_velocities==0)
      last_velocity = startPlace;
    else
      last_velocity = velocity_reader.at_last()._time;
  }

  Ratio firstLegal = R_MAX(last_pitch, last_velocity);

  r::ModifyNote new_note(note);

  if (place < firstLegal)
    new_note->d._end = firstLegal;
  else if (place > lastLegal)
    new_note->d._end = lastLegal;
  else
    new_note->d._end = place;

  return new_note->_id;
}

dyn_t MoveNote(struct Blocks *block, struct Tracks *track, struct Notes *note, Place *place, bool replace_note_ends){
  Place old_place = note->l.p;

  if (!PlaceEqual(&old_place, place)) {

    //printf("MoveNote. old: %f, new: %f\n", GetfloatFromPlace(&old_place), GetfloatFromPlace(place));
         
    if (PlaceLessThan(place, &old_place)) {
      const Place prev_legal = getPrevLegalNotePlace(track, note);
      //printf("prev_legal: %f\n",GetfloatFromPlace(prev_legal));
      if (PlaceLessOrEqual(place, &prev_legal))
        PlaceFromLimit(place, &prev_legal);
    } else {
      const Place next_legal = getNextLegalNotePlace(note);
      if (PlaceGreaterOrEqual(place, &next_legal))
        PlaceTilLimit(place, &next_legal);
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

int64_t MoveNote2(struct Blocks *block, struct Tracks *track, r::NotePtr &note, Ratio ratio, bool replace_note_ends){

  r::NoteTimeData::Writer writer(track->_notes2);
  
  Ratio old_start = note->get_time();

  if (old_start == ratio)    
    return note->_id;


  //printf("MoveNote. old: %f, new: %f\n", GetfloatFromPlace(&old_place), GetfloatFromPlace(place));
         
  if (ratio < old_start) {
    
    const Ratio prev_legal = getPrevLegalNoteRatio(writer.get_vector(), note);
    //printf("prev_legal: %f\n",GetfloatFromPlace(prev_legal));
    if (ratio <= prev_legal)
      ratio = prev_legal;
    
  } else {
    
    const Ratio next_legal = getNextLegalNoteRatio(note);
    if (ratio >= next_legal)
      ratio = next_legal;
    
  }

#if 0
  {
    SCOPED_PLAYER_LOCK_IF_PLAYING();
    
    ListRemoveElement3(&track->notes, &note->l);
    note->l.p = *place;
    
    ListAddElement3_a(&track->notes, &note->l);
    
    if (replace_note_ends && !ControlPressed())
      ReplaceNoteEnds(block, track, &old_place, place, note->polyphony_num);
    
    NOTE_validate(block, track, note);
  }
  
#else

  /*
  if (!writer.removeElement(note)){
    R_ASSERT(false); // at least at the time of writing, this would probably be an error.
    return GetNoteId2(note);
  }
  */

  int64_t ret;
  
  {
    fprintf(stderr, "   About to make new note:\n");
    r::ModifyNote new_note(writer, note, r::ModifyNote::Type::CAN_MODIFY_TIME);
    fprintf(stderr, "   .... 1. New note: %p\n", new_note.get());
    new_note->set_time(ratio);
    fprintf(stderr, "   .... 2. New note: %p\n", new_note.get());
    ret = new_note->_id;
    fprintf(stderr, "api_common.cpp: Exit. Note: %p. New note: %p.. Id: %d\n", note.get(), new_note.get(), int(ret));
  }

  for(const r::NotePtr &note : writer){
    printf("   NOTE AFT: %d. %p -> %p\n", (int)note.get()->_id, &note, note.get());
  }
  
  writer.sortit();
  
  if (replace_note_ends && !ControlPressed())
    ReplaceNoteEnds2(block, track, writer, old_start, ratio, note->d._polyphony_num);

  return ret;
  
#endif
}


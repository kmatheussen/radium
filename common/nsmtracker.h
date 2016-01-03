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


#ifndef __SSE2__
#error "SSE2 is missing (i.e. -msse2 is lacking)"
#endif

#ifndef __SSE2_MATH__
#error "SSE2 math is missing (i.e. -fpmath=sse is lacking)"
#endif

#if __tune_corei7__
#error "Compiled with -mtune=native or -mtune=corei7"
#endif
 
#ifdef RELEASE
#ifndef __OPTIMIZE__
#error "Missing -O2 or -O3 compiler option"
#endif
#endif

#ifndef DEBUG
#  error "Missing DEBUG option. Edit the Makefile."
#endif

#ifdef FOR_WINDOWS
#if FOR_WINDOWS
#else
#error "oops"
#endif
#endif

#ifdef FOR_LINUX
#if FOR_LINUX
#else
#error "oops"
#endif
#endif

#ifdef FOR_MACOSX
#if FOR_MACOSX
#else
#error "oops"
#endif
#endif



/******************************************************************
  Main header file for the tracker. Each struct often has a source-
  file with the same, or nearly the same, name.

  Note, the file OS_Visual.h is OS spesific
  and must be put into the OS directory it belongs te. The other
  OS_*_proc.h files are not.
******************************************************************/

#ifndef TRACKER_DEFINE
#define TRACKER_DEFINE 1

#ifdef __cplusplus
#  define LANGSPEC "C"
#else
#  define LANGSPEC
#endif

#if !USE_GTK_VISUAL && !USE_GTK_REQTYPE && !USE_GTK_MENU
#  define GTK_IS_USED 0
#else
#  define GTK_IS_USED 1
#endif

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <stdint.h>
#include <stdbool.h>


#include "debug_proc.h"
#include "memory_proc.h"
#include "nsmtracker_events.h"
#include "OS_error_proc.h"
#include "OS_Semaphores.h"
#include "../crashreporter/crashreporter_proc.h"


/* Unfortunately, AmigaOS has one absolute address that is legal to
   read from; 4, which often makes MuForce not to report reading
   from the wrong address (radiums fault, not MuFurces). By 
   inserting SDB at all 4 offsets, we never read from 4 if a pointer is NULL.
*/
#ifdef SYSBASEDEBUG
#  define SDB int sysbasedebug;
#else
#  define SDB
#endif

/* Next two lines must be placed in its own OS_depend file later. */
typedef uint32_t uint_32;	/* A type that should be 64 bits, but may be 32 if 64 is _really_ slow or 64 is not available. */
                           /* uint_32 is allways loaded/saved as 64, and converted to 64 bit if uint_32 is 32 bit. */
#define MAX_UINT32 65534  /* Sqr(max(uint_32))-1 (rounded down)*/

//typedef int32_t STime;		/* Time can be negative. */
typedef int64_t STime;		/* Time can be negative. */
//typedef STime NInt;
typedef int32_t NInt;
//#define PFREQ (48000*8)			/* Subseconds for STime */ /* Replaced by samplerate */

//#define LATENCY (PFREQ/200)
#define LATENCY 0 // Dont need this when the player is called from the jack thread.


// Must be a multiply of 64 because of pd, which uses a block size of 64. 64 seems to work fine.
#define RADIUM_BLOCKSIZE 64


#define MINBLOCKRELTIME 0.001f
#define MAXBLOCKRELTIME 6.0f

// Higher than 255 is no point.
#define MAX_BRIGHTNESS 63

#if !USE_OPENGL
enum{
  PAINT_DIRECTLY = 0,
  PAINT_BUFFER = 1
};
#else
enum{
  PAINT_DIRECTLY = 0,
  PAINT_BUFFER = PAINT_DIRECTLY
};
#endif



#include <OS_Visual.h>


#define R_MAX(a,b) (((a)>(b))?(a):(b))
#define R_MIN(a,b) (((a)<(b))?(a):(b))
#define R_ABS(a) ((a)<0?(-(a)):(a))
#define R_BOUNDARIES(a,b,c) (R_MIN(R_MAX((a),(b)),(c)))

#define R_ASSERT(a)                                                     \
  do{                                                                   \
    if(!(a))                                                            \
      CRASHREPORTER_send_assert_message(CT_ERROR, "Assert failed: \"" # a "\". %s: " __FILE__":%d", __FUNCTION__, __LINE__); \
  }while(0)

#define R_ASSERT_RETURN_IF_FALSE2(a,b)                                  \
  do{                                                                   \
    if(!(a)) {                                                          \
      CRASHREPORTER_send_assert_message(CT_ERROR, "Assert failed: \"" # a "\". %s: " __FILE__":%d", __FUNCTION__, __LINE__); \
      return b;                                                         \
    }                                                                   \
  }while(0)

#define R_ASSERT_RETURN_IF_FALSE(a) R_ASSERT_RETURN_IF_FALSE2(a,)

#if defined(RELEASE)
  #define R_ASSERT_NON_RELEASE(a)
#else
  #define R_ASSERT_NON_RELEASE(a) R_ASSERT(a)
#endif


static inline int64_t scale_int64(int64_t x, int64_t x1, int64_t x2, int64_t y1, int64_t y2){
  return y1 + ( ((x-x1)*(y2-y1))
                /
                (x2-x1)
                );
}

static inline double scale_double(double x, double x1, double x2, double y1, double y2){
  return y1 + ( ((x-x1)*(y2-y1))
                /
                (x2-x1)
                );
}

static inline float scale(float x, float x1, float x2, float y1, float y2){
  return y1 + ( ((x-x1)*(y2-y1))
                /
                (x2-x1)
                );
}



/*********************************************************************
	colors.h
*********************************************************************/

enum ColorNums {
  ILLEGAL_COLOR_NUM = -1,
  
  START_CONFIG_COLOR_NUM = 0,

  LOW_EDITOR_BACKGROUND_COLOR_NUM = START_CONFIG_COLOR_NUM, // 0
  HIGH_EDITOR_BACKGROUND_COLOR_NUM,         // 15

  TEXT_COLOR_NUM, 
  BAR_TEXT_COLOR_NUM,
  INSTRUMENT_NAME_COLOR_NUM,
  PORTAMENTO_NOTE_TEXT_COLOR_NUM,
  PORTAMENTO_END_NOTE_TEXT_COLOR_NUM,
  
  LOW_BACKGROUND_COLOR_NUM,
  HIGH_BACKGROUND_COLOR_NUM,                 // 11
  EDITOR_SLIDERS_COLOR_NUM,
  BUTTONS_COLOR_NUM,                          // 13
  PEAKS_COLOR_NUM,
  PEAKS_0DB_COLOR_NUM,
  PEAKS_4DB_COLOR_NUM,
  NOTE_EVENT_INDICATOR_COLOR_NUM,
  NOTE_EVENT_INDICATOR_BORDER_COLOR_NUM,
  SLIDER1_COLOR_NUM,
  SLIDER2_COLOR_NUM,
  SLIDER_DISABLED_COLOR_NUM,
  
  WAVEFORM_COLOR_NUM,
  VELOCITY1_COLOR_NUM,
  VELOCITY2_COLOR_NUM,
  TRACK_SEPARATOR1_COLOR_NUM,
  TRACK_SEPARATOR2_COLOR_NUM,
  RANGE_COLOR_NUM,  
  PITCH_LINE_COLOR_NUM,

  CURSOR_EDIT_ON_COLOR_NUM,                       // 7
  CURSOR_EDIT_OFF_COLOR_NUM,                       // 7
  PLAY_CURSOR_COLOR_NUM,
  
  AUTOMATION_INDICATOR_COLOR_NUM,
  AUTOMATION1_COLOR_NUM,
  AUTOMATION2_COLOR_NUM,
  AUTOMATION3_COLOR_NUM,
  AUTOMATION4_COLOR_NUM,
  AUTOMATION5_COLOR_NUM,
  AUTOMATION6_COLOR_NUM,
  AUTOMATION7_COLOR_NUM,
  AUTOMATION8_COLOR_NUM,
  TEMPOGRAPH_COLOR_NUM,

  PIANONOTE_COLOR_NUM,
  PIANOROLL_OCTAVE_COLOR_NUM,
  PIANOROLL_NOTE_NAME_COLOR_NUM,
  PIANOROLL_NOTE_BORDER_COLOR_NUM,
  
  SOUNDFONT_COLOR_NUM,
  SOUNDFILE_COLOR_NUM,
  CURRENT_SOUNDFILE_COLOR_NUM,

  ZOOMLINE_TEXT_COLOR_NUM1,
  ZOOMLINE_TEXT_COLOR_NUM2,
  ZOOMLINE_TEXT_COLOR_NUM3,
  ZOOMLINE_TEXT_COLOR_NUM4,
  ZOOMLINE_TEXT_COLOR_NUM5,
  ZOOMLINE_TEXT_COLOR_NUM6,
  ZOOMLINE_TEXT_COLOR_NUM7,

  END_CONFIG_COLOR_NUM,

  BLACK_COLOR_NUM = 500,
  WHITE_COLOR_NUM,
  RED_COLOR_NUM,

  END_ALL_COLOR_NUMS,
};




/*********************************************************************
	placement.h
*********************************************************************/



struct Placement{
	int line;
	SDB
	uint_32 counter;
	uint_32 dividor;
};
typedef struct Placement Place;


/*********************************************************************
	list.h
*********************************************************************/

struct ListHeader1{
	struct ListHeader1 *next;
   SDB
	NInt num;
};

struct ListHeader3{
	struct ListHeader3 *next;
   SDB
	Place p;
};

#define Tline l.p.line
#define Tcounter l.p.counter
#define Tdividor l.p.dividor


struct ListHeaderP{
	struct ListHeaderP *next;
   SDB
	STime time;
};



/*********************************************************************
        playerclass
*********************************************************************/

#include "playerclass.h"
extern PlayerClass *pc;



/*********************************************************************
	hashmap.h
*********************************************************************/

struct _hash_t;
typedef struct _hash_t hash_t;



/*********************************************************************
	vector.h
*********************************************************************/

typedef struct{
  int num_elements;
  int num_elements_allocated;
  void **elements;
} vector_t;


/*********************************************************************
	ratio.h
*********************************************************************/

typedef struct {
  int numerator;
  int denominator;
} Ratio;

static inline Ratio ratio(int numerator, int denominator) {
  Ratio ratio = {numerator, denominator};
  return ratio;
}

static inline char *ratio_to_string(Ratio ratio){
  return talloc_format("%d/%d", ratio.numerator, ratio.denominator);
}


/*********************************************************************
	quantitize.h
*********************************************************************/

typedef struct{
  Ratio quant;
  bool quantitize_start;
  bool quantitize_end;
  bool keep_note_length;
  int type;
} quantitize_options_t;




/*********************************************************************
	velocities.h
*********************************************************************/

#define MAX_VELOCITY (1<<16)

struct Velocities{
	struct ListHeader3 l;

	int velocity;
};
#define NextVelocity(a) ((struct Velocities *)((a)->l.next))

static inline float VELOCITY_get(int velocity){
  return scale(velocity,0,MAX_VELOCITY,0,1);
}

/*********************************************************************
	pitches.h
*********************************************************************/

struct Pitches{
	struct ListHeader3 l;

	float note;
};
#define NextPitch(a) ((struct Pitches *)((a)->l.next))

typedef struct TrackReallineElements WPitches;
enum{
  TRE_PITCHLINE,
  TRE_PITCHNODE
};


/*********************************************************************
	notes.h
*********************************************************************/

#define NOTE_END_NORMAL 128
enum{
  NOTE_MUL=NOTE_END_NORMAL,
  NOTE_STP,
  NOTE_MUR
};


struct Notes{
	struct ListHeader3 l;

	float note;
	int velocity;

	Place end;
	
	struct Velocities *velocities;
	int velocity_end;

	float pitch_end;
  
	struct Velocities first_velocity; // used by nodelines
	struct Velocities last_velocity; // used by nodelines

	struct Pitches *pitches;

	int subtrack;

	int noend;

        int64_t id;
};
#define NextNote(a) ((struct Notes *)((a)->l.next))



/*********************************************************************
	patch.h
*********************************************************************/

struct Tracks;

struct Instruments;

union SuperType{
  void *pointer;
  const void *const_pointer;
  int64_t int_num;
  uint32_t uint32_num;
  double float_num;
};

enum TimeFormat{
  TIME_IN_BEATS = 0,
  TIME_IN_MS = 1,
  TIME_IN_S = 2
};

struct PatchVoice{
  bool is_on;
  int transpose;
  float volume;
  float start;
  float length;
  enum TimeFormat time_format;
};

#define NUM_PATCH_VOICES 6
#define MAX_NUM_EVENT_RECEIVERS 64
#define MAX_NOTE_INTENCITY 20

#define MAX_PLAYING_PATCH_NOTES (1024*32)

typedef struct{
  float note_num;
  int64_t note_id;
  float pan;
} PatchPlayingNote;

static inline PatchPlayingNote NewPatchPlayingNote(float note_num, int64_t note_id,float pan){
  PatchPlayingNote ret;
  ret.note_num=note_num;
  ret.note_id=note_id;
  ret.pan=pan;
  return ret;
}


// Note that Patch objects are stored directly in undo/redo (not copied), so it must not be freed, reused for other purposes, or othervice manipulated when not available.
struct Patch{
  int id;
  bool is_usable; // If pasting a track with this patch, this flag tells whether the patch can be used on the new track.
  
  const char *name;

  int colornum;

  STime last_time; // player lock must be held when setting this value.

  void (*playnote)(struct Patch *patch,float notenum,int64_t note_id,float velocity,STime time,float pan);
  void (*changevelocity)(struct Patch *patch,float notenum,int64_t note_id,float velocity,STime time);
  void (*changepitch)(struct Patch *patch,float notenum,int64_t note_id,float pitch,STime time);
  void (*sendrawmidimessage)(struct Patch *patch,uint32_t msg,STime time); // note on, note off, and polyphonic aftertouch are/should not be sent using sendmidimessage. sysex is not supported either.
  void (*stopnote)(struct Patch *patch,float notenum,int64_t note_id,STime time);
  void (*closePatch)(struct Patch *patch);
  
  struct Instruments *instrument;

  void *patchdata;		// Free use by the instrument plug-in.

  void (*changeTrackPan)(int newpan,const struct Tracks *track);

  struct PatchVoice voices[NUM_PATCH_VOICES];

  int num_currently_playing_voices;
  PatchPlayingNote playing_voices[MAX_PLAYING_PATCH_NOTES];      /* To keep track of how many times a voice has to be turned off. */

  int num_currently_playing_notes;
  PatchPlayingNote playing_notes[MAX_PLAYING_PATCH_NOTES];  /* To keep track of which notes are playing. (Useful to avoid hanging notes when turning on and off voices)*/

  bool peaks_are_dirty; /* Can be set to true by any thread. */

  bool forward_events; /* If true, all events that comes in, are also sent out to the receivers. True by default. */
  int num_event_receivers;
  struct Patch *event_receivers[MAX_NUM_EVENT_RECEIVERS];

  volatile int visual_note_intencity; // Used by the mixer to keep track of how bright the note indicator should light up.

  volatile bool widget_needs_to_be_updated;
};
#define PATCH_FAILED 0
#define PATCH_SUCCESS 1
#define NextPatch(a) ((struct Patch *)((a)->l.next))

static inline void Patch_addPlayingVoice(struct Patch *patch, float note_num, int64_t note_id, float pan){
#if 0
  printf("Adding note with id %d\n",(int)note_id);
  if(note_id==52428)
    abort();
#endif

  if(patch->num_currently_playing_voices==MAX_PLAYING_PATCH_NOTES)
    printf("Error. Reached max number of voices there's room for in a patch. Hanging notes are likely to happen.\n");
  else    
    patch->playing_voices[patch->num_currently_playing_voices++] = NewPatchPlayingNote(note_num, note_id, pan);
}


static inline void Patch_removePlayingVoice(struct Patch *patch, int64_t note_id){
  int i;
  for(i=0;i<patch->num_currently_playing_voices;i++){
    if(patch->playing_voices[i].note_id==note_id){
      patch->playing_voices[i] = patch->playing_voices[patch->num_currently_playing_voices-1];
      patch->num_currently_playing_voices--;
      return;
    }
  }
  if (pc->isplaying){
    printf("Warning. Unable to find voice with note_id %d when removing playing note. Num playing: %d\n",(int)note_id,patch->num_currently_playing_voices);
    //abort();
  }
#if 0
  for(i=0;i<patch->num_currently_playing_voices;i++)
    printf("id: %d\n",(int)patch->playing_voices[i].note_id);
  if(patch->num_currently_playing_voices > 3)
    abort();
#endif
}

static inline void Patch_addPlayingNote(struct Patch *patch, float note_num, int64_t note_id, float pan){
  if(patch->num_currently_playing_notes==MAX_PLAYING_PATCH_NOTES)
    printf("Error. Reached max number of notes there's room for in a patch. Hanging notes are likely to happen.\n");
  else    
    patch->playing_notes[patch->num_currently_playing_notes++] = NewPatchPlayingNote(note_num, note_id, pan);
}

static inline void Patch_removePlayingNote(struct Patch *patch, int64_t note_id){
  int i;
  for(i=0;i<patch->num_currently_playing_notes;i++){
    if(patch->playing_notes[i].note_id==note_id){
      patch->playing_notes[i] = patch->playing_notes[patch->num_currently_playing_notes-1];
      patch->num_currently_playing_notes--;
      return;
    }
  }
  if (pc->isplaying)
    printf("Warning. Unable to find note with note_id %d when removing playing note\n",(int)note_id);
}


/*********************************************************************
	fx.h
*********************************************************************/

typedef enum {
  FX_start = 0,
  FX_middle = 1,
  FX_end = 2,
  FX_single = 3
} FX_when;

struct FX{
  //	struct ListHeader1 l; // The next field in 'l' is not used. FX objects are stored one by one in the FXs object.
	int num;
	const char *name;
	enum ColorNums color;
	void (*configureFX)(struct FX *fx,struct Tracks *track);
	int min;
	int max;

        struct Patch *patch;
  
  	int effect_num; // Set by the instrument plugin.

	// Having pointers to variables in sliders is a bit strange, but letting sliders reference FX instead would cause bookkeeping of live and not alive FX objects.
	// Not getting that bookkeeping correct would mean crashes that could be difficult to track.
	// This, on the other hand, is safe, since sliders are always alive as long as the Patch is alive, and the patch always outlives an FX object.
	// (The refactor to let Patch own FX hasn't been done yet. It didn't make sense when there were only MIDI instrument, but now it's too complicated to let FX live independently.
        //  However, when an instrument is deleted, all tracks are scanned, and FX are deleted when a patch is deleted. Same when changing patch for a track.)
	float *slider_automation_value; // Pointer to the float value showing automation in slider. Value is scaled between 0-1. May be NULL.
	enum ColorNums   *slider_automation_color; // Pointer to the integer holding color number for showing automation in slider. May be NULL.

        void (*treatFX)(struct FX *fx,int val,STime time,int skip, FX_when when);

	void (*closeFX)(struct FX *fx,const struct Tracks *track);
	void *fxdata;	//Free use for the instrument plug-in.
	void (*SaveFX)(struct FX *fx,const struct Tracks *track);

  //void (*setFXstring)(struct FX *fx,struct Tracks *track, char *string);
};
#define FX_FAILED 0
#define FX_SUCCESS 1


/*********************************************************************
	instruments.h
*********************************************************************/

// These constants are not only used internally, but they are also saved to disk.
enum{
  NO_INSTRUMENT_TYPE = 0,
  MIDI_INSTRUMENT_TYPE,
  AUDIO_INSTRUMENT_TYPE
};

struct Tracker_Windows;
struct Instruments{
	struct ListHeader1 l;

	const char *instrumentname;

        vector_t patches; // Not safe to traverse from player thread.

        //int (*getMaxVelocity)(const struct Patch *patch);

	int (*getFX)(struct Tracker_Windows *window,const struct Tracks *track,struct FX *fx);
	int (*getPatch)(struct Tracker_Windows *window,ReqType reqtype,const struct Tracks *track,struct Patch *patch);
	//void (*treatSpecialCommand)(char *command,struct Tracks *track);
	void (*CloseInstrument)(struct Instruments *instrument);
	void (*StopPlaying)(struct Instruments *instrument);
	void (*PP_Update)(struct Instruments *instrument,struct Patch *patch);
	void *(*CopyInstrumentData)(const struct Tracks *track);		//Necesarry for undo.

	void (*PlayFromStartHook)(struct Instruments *instrument);

	void *(*LoadFX)(struct FX *fx,const struct Tracks *track);

	void (*handle_fx_when_theres_a_new_patch_for_track)(struct Tracks *track, struct Patch *old_patch, struct Patch *new_patch);
        void (*remove_patch)(struct Patch *patch);

	void (*setPatchData)(struct Patch *patch, char *key, char *value);
	char *(*getPatchData)(struct Patch *patch, char *key);
};
#define INSTRUMENT_FAILED 0
#define INSTRUMENT_SUCCESS 1
#define NextInstrument(a) ((struct Instruments *)((a)->l.next))



/*********************************************************************
	stops.h
*********************************************************************/


struct Stops{
	struct ListHeader3 l;
};
#define NextStop(a) ((struct Stops *)((a)->l.next))



/*********************************************************************
	fxnodelines.h
*********************************************************************/


struct FXNodeLines{
	struct ListHeader3 l;
	int val;
};
#define NextFXNodeLine(a) ((struct FXNodeLines *)((a)->l.next))


struct FXs{
	struct ListHeader1 l;	/* l.num=fxnum */
	struct FX *fx;
	struct FXNodeLines *fxnodelines;
};
#define NextFX(a) ((struct FXs *)((a)->l.next))



/*********************************************************************
	tracks.h
*********************************************************************/
struct Tracks{
	struct ListHeader1 l;

	struct Notes *notes;
	struct Stops *stops;
	int onoff;

        int num_subtracks;
  
	const char *trackname;
	struct Patch *patch;
	struct FXs *fxs;

        void *midi_instrumentdata;			/* Used by the midi instrument. */

	int pan;
	int volume;

	bool panonoff;
        bool volumeonoff;                      /* The volume-button on/off, not track on/off. (i.e. if off, volume=1.0, not 0.0) */

        volatile bool is_recording;
};
#define NextTrack(a) ((struct Tracks *)((a)->l.next))

#define MAXTRACKVOL 1000
#define MAXTRACKPAN 1000

static inline float TRACK_get_pan(const struct Tracks *track){
  if(track->panonoff)
    return scale(track->pan, -MAXTRACKPAN, MAXTRACKPAN, -1.0, 1.0);
  else
    return 0.0f;
}

static inline float TRACK_get_volume(const struct Tracks *track){
  if(track->volumeonoff)
    return scale(track->volume, 0, MAXTRACKVOL, 0, 1);
  else
    return 1.0f;
}

static inline float TRACK_get_velocity(const struct Tracks *track, int velocity){
  return TRACK_get_volume(track) * VELOCITY_get(velocity);
}


/*********************************************************************
	area.h
*********************************************************************/



typedef struct{
	int x,x2;
}Area;

typedef struct{
	int y,y2;
}YArea;

typedef struct{
	int width;
	int x,x2;
}WArea;

typedef struct{
  float x,y;
}WPoint;




/*********************************************************************
	peaks.h
*********************************************************************/

#define NUM_PEAKS_PER_LINE 8
typedef struct{
  float x,y;
} APoint;



/*********************************************************************
	trackreallines2.h
*********************************************************************/

typedef struct{
  Place p;
  struct Notes *note;
  struct Pitches *pitch;
  struct Stops *stop;
  bool is_end_pitch;
} TrackRealline2;



/*********************************************************************
	nodelinens.h
*********************************************************************/

struct NodeLine{
  struct NodeLine *next;

  float x1,y1;
  float x2,y2;

  const struct ListHeader3 *element1;
  const struct ListHeader3 *element2;

  bool is_node;
};

static inline const struct NodeLine *Nodeline_n(const struct NodeLine *nodeline, int n){
  while(n>0 && nodeline!=NULL) {
    nodeline = nodeline->next;
    n--;
  }
  return nodeline;
}

struct Node{
  float x, y;
  const struct ListHeader3 *element;
};

struct MinMax{
  float min;
  float max;
};

typedef struct {
  float x1,y1;
  float x2,y2;
} NodelineBox;




/*********************************************************************
	tbox.h
*********************************************************************/
struct TBoxstruct{
  int x1,y1;
  int x2,y2;
};
typedef struct TBoxstruct TBox;


/*********************************************************************
	wtracks.h
*********************************************************************/

#define WTRACKS_SPACE 2


struct WTracks{
	struct ListHeader1 l;
//	l.num=wtracknum;

	int x,y,x2,y2;						/* GFX area. */

	int notesonoff;					/* notearea and placementarea on/off. */
	int notelength;					/* Number of characters the notes is. Usually 2 or 3. */
        int notewidth;
	Area notearea;						/* These are all parts of the GFX area. */
	int fxonoff;						/* FX area on/off */
	int fxwidth;						/* is fxarea.x2-fxarea.x */
	Area fxarea;

        bool is_wide;

        bool pianoroll_on;
        int pianoroll_lowkey;
        int pianoroll_highkey;

        int pianoroll_width; // not necessary
        Area pianoroll_area;
  
  //int num_vel;						/* Max number of velocity lines showed simultaniously. (I.e the number of subtracks)*/

	struct Tracks *track;			/* Only referenced. wtracknum=track->tracknum */

	TBox pan;
	TBox volume;

	TBox panonoff;
	TBox volumeonoff;

  int noteshowtype;
};
#define NextWTrack(a) ((struct WTracks *)((a)->l.next))

#define TEXTTYPE 0
#define GFXTYPE1 1
#define MAXTYPE 1

struct CurrentPianoNote{
  int tracknum;
  int notenum;
  int pianonotenum;
};

static inline const NodelineBox GetPianoNoteBox(const struct WTracks *wtrack, const struct NodeLine *nodeline){
  const float gfx_width  = wtrack->pianoroll_area.x2 - wtrack->pianoroll_area.x;
  const float notespan   = wtrack->pianoroll_highkey - wtrack->pianoroll_lowkey;
  const float note_width = gfx_width / notespan;

  float x_min = R_MIN(nodeline->x1, nodeline->x2);
  float x_max = R_MAX(nodeline->x1, nodeline->x2);

  float y_min = R_MIN(nodeline->y1, nodeline->y2);
  float y_max = R_MAX(nodeline->y1, nodeline->y2);

  const struct NodeLine *next = nodeline->next;
  
  while(next!=NULL){
    if (next->is_node)
      break;

    x_min = R_MIN(x_min, R_MIN(next->x1, next->x2));
    x_max = R_MAX(x_max, R_MAX(next->x1, next->x2));

    y_min = R_MIN(y_min, R_MIN(next->y1, next->y2));
    y_max = R_MAX(y_max, R_MAX(next->y1, next->y2));

    next = next->next;
  }

  NodelineBox nodelineBox;

  nodelineBox.x1 = x_min-note_width/2.0;
  nodelineBox.y1 = y_min;
  nodelineBox.x2 = x_max+note_width/2.0;
  nodelineBox.y2 = y_max;

  return nodelineBox;
}


/*********************************************************************
	Signature.h
*********************************************************************/

struct Signatures{
  struct ListHeader3 l;
  Ratio signature;
};
#define NextSignature(a) (struct Signatures *)((a)->l.next)

struct WSignatures{
  Ratio signature;
  int bar_num;
  int beat_num;   // In a 4/4 measure, this value is either 0, 1, 2 or 3, or 4. (0 means that there is no beat placed on this realline)
  int type;	  /* 0=normal, 1=below positioned, 2=mul. */
  vector_t how_much_below;  /* If type is 1 or 2, these values contains how much below (between 0 and 1) */
};
#define SIGNATURE_NORMAL 0
#define SIGNATURE_BELOW 1
#define SIGNATURE_MUL 2

static inline bool WSIGNATURE_is_measure_change(const struct WSignatures *signature){
  return signature->signature.numerator != 0 && signature->beat_num==1;
}

static inline bool WSIGNATURE_is_first_beat(const struct WSignatures *signature){
  return signature->beat_num==1;
}



/*********************************************************************
	Beats.h
*********************************************************************/

struct Beats{
  struct ListHeader3 l;
  Ratio signature; // Current signature for this beat.
  int bar_num;
  int beat_num;  // For instance, in a 4/4 measure, this value is either 1, 2 or 3, or 4.
};
#define NextBeat(a) (struct Beats *)((a)->l.next)
  

/*********************************************************************
	lpb.h
*********************************************************************/


struct LPBs{
	struct ListHeader3 l;
	int lpb;
};
#define NextLPB(a) (struct LPBs *)((a)->l.next)

struct WLPBs{
	int lpb;
	int type;					/* 0=normal, 1=below positioned, 2=mul. */
};
#define LPB_NORMAL 0
#define LPB_BELOW 1
#define LPB_MUL 2



/*********************************************************************
	tempos.h (i.e. BPM)
*********************************************************************/


struct Tempos{
	struct ListHeader3 l;
	int tempo;
};
#define NextTempo(a) (struct Tempos *)((a)->l.next)
#define NextBPM(a) NextTempo(a)

struct WTempos{
	int tempo;
	int type;							/* 0=normal, 1=below positioned, 2=mul. */
};
/* Types */
#define TEMPO_NORMAL 0
#define TEMPO_BELOW 1
#define TEMPO_MUL 2

// Todo: Rename all Tempos to BPMs. First step:
#define BPMs Tempos
#define WBPMs WTempos


/*********************************************************************
	temponodes.h
*********************************************************************/



struct TempoNodes{
	struct ListHeader3 l;
	float reltempo;
};
#define NextTempoNode(a) ((struct TempoNodes *)((a)->l.next))



/*********************************************************************
	time.h
*********************************************************************/

struct STimeChanges{
	struct ListHeader3 l;
	STime time;

	float tempo1;			// tempo (tempo*lpb) at this->l.p
	float rel;				// reltempo for this->l.p
	float deltarel;		// rel+deltarel is reltempo for this->l.next->l.p
};
#define NextSTimeChange(a) (struct STimeChanges *)((a)->l.next)


struct STimes{									/* One element for each line. */
	STime time;							/* Start-time for the line. */
   SDB
	const struct STimeChanges *timechanges;
        //bool is_beat; // true if this line starts a new beat
};


/*********************************************************************
	blocks.h
*********************************************************************/


struct Blocks{
	struct ListHeader1 l;

	char *name;

	NInt num_tracks;
	int num_lines;

	struct Tracks *tracks;
	struct Beats        *beats;
	struct Signatures   *signatures;
  	struct LPBs   *lpbs;
	struct Tempos *tempos;
	struct TempoNodes *temponodes;
	struct TempoNodes *lasttemponode;
  
	const struct STimes *times;			/* Pointer to array. Last element (times[num_lines]) is the playtime of the block. */

	volatile float reltempo;					/* factor that the tempo is multiplied with when playing this block. */

  // This variable is checked after each keyboard or menu event. If true, trackreallines, wtracks, etc. will be updated.
  bool is_dirty; 
};
#define NextBlock(a) (struct Blocks *)((a)->l.next)



/*********************************************************************
	localzooms.h
*********************************************************************/


struct LocalZooms{
	struct ListHeader3 l;
//	struct LocalZooms *next;		/* Next on the same level. */
//	int line;							/* The same type of line that note has. */
//	uint_32 counter;					/* Same type of counter that line has. */
//	uint_32 dividor;					/* Same type of dividor that line has. */

	int zoomline;						/* The linetype that is showed in the editor. */
	int level;
	int realline;

	struct LocalZooms *uplevel;	/* Contains 'num_newlines' # of elements. */

  bool autogenerated;
};
#define NextLocalZoom(a) ((struct LocalZooms *)((a)->l.next))



/*********************************************************************
	wblocks.h
*********************************************************************/


struct WBlocks{
	struct ListHeader1 l;

	int tempotrackonoff;				/* 1=on, 0=off */
	int temponodetrackonoff;		/* 1=on, 0=off */

	TBox a; // everything
//	int x,y,x2,y2;						/* GFX area. */

	TBox t;
//	int tx,ty,tx2,ty2;				/* lines, nodes, etc. GFX area. */

        TBox bottombar;                                /* reltempo slider and track slider */

	//WArea zoomlevelarea;
        WArea linenumarea;
        int linenumbers_max_num_characters;
        int bars_max_num_characters;
        int beats_x; int beats_max_num_characters;
        int zoomlines_x; int zoomlines_max_num_characters;

  //        WArea 
  //WArea zoomlinearea;
        WArea tempocolorarea;
  //WArea signatureTypearea;
	WArea signaturearea;
	WArea lpbTypearea;
	WArea lpbarea;
	WArea tempoTypearea; // When one character signals whether the tempo is down "d", or multi "m"
	WArea tempoarea;
	WArea temponodearea;

	YArea linearea;

	int maxwtracksize;					/* The size of the widest wtrack. */

	int num_visiblelines;

        volatile int till_curr_realline;       // Set by the player thread. Read by the main thread.
  
        int top_realline; // only access from main thread.
        int curr_realline; // only access from main thread.
        int bot_realline; // only access from main thread.

        int mouse_track; // The track the mouse is currently above. -1 if not on a track.
        struct Notes *mouse_note; // The note the mouse is currently above. NULL if mouse is not above a note.

        struct FXs *mouse_fxs; // The fxs the mouse is currently above. NULL if mouse is not above an fx.
  
	struct Blocks *block;			/* Only referenced. wblocknum=block->blocknum */

	struct LocalZooms *localzooms;    /* These two variables (localzooms and reallines) contain the same elements, but 'localzooms' is organized as a tree, while 'reallines' is organized as an array. (roughly)*/
	struct LocalZooms **reallines;   // Used by the player. Must be protected by PLAYER_lock
	int num_reallines;               // Same here. Must be protected by PLAYER_lock.

        int num_expand_lines;

	struct WTracks *wtracks;
	struct WTracks *wtrack;			/* Current track. Only referenced. */

	char *title;						/* Window title. */

	NInt left_track;					/* The leftmost visible track. */
	int left_subtrack;
	NInt right_track;					/* The rightmost visible track. */
	int right_subtrack;

        //struct WTempos *wtempos;
	float reltempomax;

	bool isranged;
	NInt rangex1;
	NInt rangex2;
	NInt rangey1;
	NInt rangey2;

	bool isgfxdatahere;

	TBox reltempo; // API: (x1 y1 x2 y2) getWBlockFromNum(-1,-1)
};
#define NextWBlock(a) (struct WBlocks *)((a)->l.next)



/*********************************************************************
	slider.h
*********************************************************************/


struct Slider{
	int show;
	int width;
	int x,x2;
	int lx,lx2;
};


/*********************************************************************
       blts.h
********************************************************************/

typedef struct{

  /* Used by Blt_blt Blt_mark */
  bool blt_do;
  int x1;int x2;
  int startrealline;
  int endrealline;

  /* Used by Blt_blt and Blt_marktrackheader */
  bool blt_doheader;
  NInt starttrack;
  NInt endtrack;

  /* Used by Blt_clearNotUsedVisible and Blt_markVisible */
  bool clear_do;
  int v_x1;int v_x2;
  int v_y1;int v_y2;
}Blt;


/*********************************************************************
       Mixer.hpp
********************************************************************/

struct SoundProducer;

typedef struct {
  struct SoundProducer *bus1;
  struct SoundProducer *bus2;
} Buses;


/*********************************************************************
	windows.h
*********************************************************************/

struct Tracker_Windows{
	struct ListHeader1 l;

	struct OS_visual os_visual;
	int x,y;								/* Where window is placed. (for amiga: screen-pos)*/
	int width,height;					/* Size of area to use. */
	char *fontname;
	int fontID;							/* System spesific. For amiga: fontsize. */
	int fontTags;						/* System spesific. For amiga: nothing. */
	int fontwidth,fontheight;		/* Proportional fonts not so very allowed. */
	int systemfontheight;
  
	NInt curr_track;
	int curr_track_sub;				/* -1=note, 0,1,2,...,n=vel */
	NInt curr_block;

	int maxwtracksize;					/* The size of the widest wtrack for all wblocks. */

	struct Slider bottomslider;
	struct Slider leftslider;
	struct Slider rightslider;

	bool playalong;					/* If true, this window allso shows whats being played
											   if any other window is playing. default=true. */

	struct WBlocks *wblock;			/* Current wblock. Only referenced. */
	struct WBlocks *wblocks;

  //	struct TEventFIFO *TELroot;
  //	struct TEventFIFO *TELlast;
  //	uint32_t event_treat;		/* Chooses which event type(s) to treat. (0=all)*/
  //	int dontbuffer;

  //	struct MouseAction curraction;
  //	struct MouseAction prevaction;

	int org_fontheight;
#ifdef _AMIGA
	char *h_fontname;
	int h_fontID;							/* System spesific. For amiga: fontsize. */
	int h_fontTags;						/* System spesific. For amiga: nothing. */
	int h_fontwidth;
#endif

  bool show_signature_track;
  bool show_lpb_track;
  bool show_bpm_track;
  bool show_reltempo_track;

  int num_pixmapdefs;
  int *pixmapdefs;
  int *pixmapdefs_calc;

  Blt blt;

#ifdef USE_GFX_OP_QUEUE
  void *op_queue;
#endif

  bool must_redraw;
  bool must_redraw_editor; // Same as must_redraw, but only redraws the editor.

  bool redraw_has_been_scheduled;
};
#define NextWindow(a) (struct Tracker_Windows *)((a)->l.next)

/* curr_track types */
#define TEMPONODETRACK -1
#define LINENUMBTRACK -2
#define SIGNATURETRACK -3
#define LPBTRACK -4
#define TEMPOTRACK -5
#define TEMPOCOLORTRACK -6
#define LEFTMOSTTRACK TEMPOCOLORTRACK
#define NOTRACK -10000

/*********************************************************************
	song.h
*********************************************************************/


struct Song{
	struct Tracker_Windows *tracker_windows;
	struct Blocks *blocks;
	struct Blocks **playlist;			/* This variable is just temporarily. Later, the playlist will be much more advanced. */

	NInt num_blocks;
	int length;								/* Playlist length. */
	char *songname;

	NInt maxtracks;						/* The highest number of tracks in a block. (changed when exceeding) */

	hash_t *mixerwidget_state; // Only used during loading.
	hash_t *instrument_widget_order_state; // Only used during loading.
};



/*********************************************************************
	root.h
*********************************************************************/


struct Root{
	struct Song *song;
	
	int curr_playlist;
	volatile NInt curr_block;

	volatile bool setfirstpos;

	int tempo;			/* Standard tempo. */
	int lpb;			/* Standard lpb. */
	Ratio signature;		/* Standard signature. */

        quantitize_options_t quantitize_options;
  
        int grid_numerator;
        int grid_denominator;

	int keyoct;
        int min_standardvel;
        int standardvel;

	volatile bool editonoff;
        volatile bool play_cursor_onoff;
        volatile bool editor_follows_play_cursor_onoff;
        volatile bool clickonoff;
};

extern struct Root *root;




/*************************************************
 Structures for the advanced functions.
 (especially made for extension language support.)
 *************************************************/

struct NoteAdds_track{
	float place;					// A placement type represented as float
	int notenum;
	float volume;					// 0.0 is off, 1.0 is max, -0.0 - -2.0 is default
	float endplace;				// The end place. A value -0.0 - -2.0 means no spesified end-place (end-place is set to the same as the start-place of the next note in the array).
};

struct NoteAdds_track_do{
	NInt tracknum;
	int num_nats;
	struct NoteAdds_track *nats;
	float startplace;
	int sort;
};

struct NoteAdds_block{
	NInt blocknum;
	int num_nats_do;
	struct NoteAdds_track_do **nats_do;
};


#endif












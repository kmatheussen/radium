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


#ifndef DEBUG
#  error "Missing DEBUG option. Edit the Makefile."
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


struct Notes{
	struct ListHeader3 l;

	float note;
	int velocity;

	Place end;
	
	struct Velocities *velocities;
	int velocity_end;

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

  const char *name;

  int colornum;

  STime last_time; // player lock must be held when setting this value.

  void (*playnote)(struct Patch *patch,float notenum,int64_t note_id,float velocity,STime time,float pan);
  void (*changevelocity)(struct Patch *patch,float notenum,int64_t note_id,float velocity,STime time);
  void (*changepitch)(struct Patch *patch,float notenum,int64_t note_id,float pitch,STime time);
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
  if (pc->isplaying)
    printf("Warning. Unable to find voice with note_id %d when removing playing note. Num playing: %d\n",(int)note_id,patch->num_currently_playing_voices);
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
	int color;
	void (*configureFX)(struct FX *fx,struct Tracks *track);
	int min;
	int max;

  	int effect_num; // Set by the instrument plugin.

	// Having pointers to variables in sliders is a bit strange, but letting sliders reference FX instead would cause bookkeeping of live and not alive FX objects.
	// Not getting that bookkeeping correct would mean crashes that could be difficult to track.
	// This, on the other hand, is safe, since sliders are always alive as long as the Patch is alive, and the patch always outlives an FX object.
	// (The refactor to let Patch own FX hasn't been done yet. It didn't make sense when there were only MIDI instrument, but now it's too complicated to let FX live independently.
        //  However, when an instrument is deleted, all tracks are scanned, and FX are deleted when a patch is deleted. Same when changing patch for a track.)
	float *slider_automation_value; // Pointer to the float value showing automation in slider. Value is scaled between 0-1. May be NULL.
	int   *slider_automation_color; // Pointer to the integer holding color number for showing automation in slider. May be NULL.

	void (*treatFX)(struct FX *fx,int val,const struct Tracks *track,STime time,int skip, FX_when when);

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

	const char *trackname;
	struct Patch *patch;
	struct FXs *fxs;

        void *midi_instrumentdata;			/* Used by the midi instrument. */

	int pan;
	int volume;

	bool panonoff;
        bool volumeonoff;                      /* The volume-button on/off, not track on/off. (i.e. if off, volume=1.0, not 0.0) */
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


/*********************************************************************
	trackreallines.h
*********************************************************************/

#define NUM_PEAKS_PER_LINE 8
typedef struct{
  float x,y;
} APoint;

#define TRE_Max INT16_MAX
struct TrackReallineElements{
  struct TrackReallineElements *next;

  struct Notes *note;

  int type;
  int subtype;
  float y1,y2;
  float x1,x2;
  void *pointer;

  APoint *velocity_polygon;

  int num_peaks;
  APoint *peaks[2];
};

/************* Types: */
enum{
  TRE_THISNOTELINES,
  TRE_THISPITCHLINES,
  TRE_VELLINECENTS,
  TRE_VELLINENODE,
  TRE_VELLINE,
  TRE_VELLINESTART,
  TRE_VELLINEEND,
  TRE_STOPLINE,
  TRE_REALSTARTSTOP
};

/* Subtype for 0-0x40 is
   the same as subtrack for the note.
*/


struct TrackRealline{
  struct Notes *dasnote;
  float note;										/* Is 0 if no note. */
  struct TrackReallineElements *trackreallineelements;
};
#define NOTE_END_NORMAL 128
enum{
  NOTE_MUL=NOTE_END_NORMAL,
  NOTE_STP,
  NOTE_MUR,
  NOTE_PITCH_START
};


/*********************************************************************
	OpenGL/Render.h
*********************************************************************/

struct NodeLine{
  struct NodeLine *next;

  float x1,y1;
  float x2,y2;

  const struct ListHeader3 *element1;
  const struct ListHeader3 *element2;

  bool is_node;
};

struct Node{
  float x, y;
  const struct ListHeader3 *element;
};


/*********************************************************************
	wfxnodes.h
*********************************************************************/


typedef struct TrackReallineElements WFXNodes;
/*
struct WFXNodes{
	struct WFXNodes *next;
   SDB
	unsigned char type;
	unsigned char subtype;						// not used.
	unsigned char y1,y2;
	unsigned short x1,x2;
	void *pointer;									// Only referenced.
};
*/
/************* Types: */
#define TRE_FXNODE 0x50
#define TRE_FXLINE 0x60


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


	int num_vel;						/* Max number of velocity lines showed simultaniously. (I.e the number of subtracks)*/

	struct Tracks *track;			/* Only referenced. wtracknum=track->tracknum */

	struct TrackRealline *trackreallines;
	WFXNodes **wfxnodes;
	WPitches **wpitches;

        vector_t velocity_nodes; // contains vector of vectors of Node's. (element 1 contains velocities for note 1, element 2 contains velocities for note 2, etc.)
  
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

	bool is_beat;

	struct LPBs *LPB;			/* Only referenced. */
};
#define LPB_NORMAL 0
#define LPB_BELOW 1
#define LPB_MUL 2



/*********************************************************************
	tempos.h
*********************************************************************/


struct Tempos{
	struct ListHeader3 l;
	int tempo;
};
#define NextTempo(a) (struct Tempos *)((a)->l.next)

struct WTempos{
	int tempo;
	int type;							/* 0=normal, 1=below positioned, 2=mul. */
   SDB
	struct Tempos *Tempo;			/* Only referenced. */
};
/* Types */
#define TEMPO_NORMAL 0
#define TEMPO_BELOW 1
#define TEMPO_MUL 2



/*********************************************************************
	temponodes.h
*********************************************************************/



struct TempoNodes{
	struct ListHeader3 l;
	float reltempo;
};
#define NextTempoNode(a) ((struct TempoNodes *)((a)->l.next))

typedef struct TrackReallineElements WTempoNodes;
/*
struct WTempoNodes{
	struct WTempoNodes *next;
	unsigned char type;
	unsigned char subtype;			// Currently not used.
	unsigned char y1,y2;
	unsigned short x1,x2;
	
	void *temponode;	// Only referenced.
};
*/
/* Types */
#define TEMPONODE_NODE 0
#define TEMPONODE_LINE 1


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

	//WArea zoomlevelarea;
	WArea linenumarea;
	WArea zoomlinearea;
        WArea tempocolorarea;
	WArea lpbTypearea;
	WArea lpbarea;
	WArea tempoTypearea; // When one character signals whether the tempo is down "d", or multi "m"
	WArea tempoarea;
	WArea temponodearea;

	YArea linearea;

	int maxwtracksize;					/* The size of the widest wtrack. */

	int num_visiblelines;

	int top_realline;

	int curr_realline;
	int till_curr_realline;				/* The player-routine sets this on. */

	int bot_realline;

        int mouse_track; // The track the mouse is currently above. -1 if not on a track.
        struct Notes *mouse_note; // The note the mouse is currently above. NULL if mouse is not above a note.

	struct Blocks *block;			/* Only referenced. wblocknum=block->blocknum */

	struct LocalZooms *localzooms;
	struct LocalZooms **reallines;
	int num_reallines;
	int num_reallines_last;

	struct WTracks *wtracks;
	struct WTracks *wtrack;			/* Current track. Only referenced. */

	char *title;						/* Window title. */

	NInt left_track;					/* The leftmost visible track. */
	int left_subtrack;
	NInt right_track;					/* The rightmost visible track. */
	int right_subtrack;

	struct WTempos *wtempos;
	WTempoNodes **wtemponodes;
        vector_t *reltempo_nodes; // contains vector of Node's
	struct WLPBs *wlpbs;
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
	windows.h
*********************************************************************/
#include "mouse.h"

struct Tracker_Windows{
	struct ListHeader1 l;

	struct OS_visual os_visual;
	int x,y;								/* Where window is placed. (for amiga: screen-pos)*/
	int width,height;					/* Size of area to use. */
	char *fontname;
	int fontID;							/* System spesific. For amiga: fontsize. */
	int fontTags;						/* System spesific. For amiga: nothing. */
	int fontwidth,fontheight;		/* Proportional fonts not so very allowed. */

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

	struct TEventFIFO *TELroot;
	struct TEventFIFO *TELlast;
	uint32_t event_treat;		/* Chooses which event type(s) to treat. (0=all)*/
	int dontbuffer;

	struct MouseAction curraction;
	struct MouseAction prevaction;

	int org_fontheight;
#ifdef _AMIGA
	char *h_fontname;
	int h_fontID;							/* System spesific. For amiga: fontsize. */
	int h_fontTags;						/* System spesific. For amiga: nothing. */
	int h_fontwidth;
#endif

  int num_pixmapdefs;
  int *pixmapdefs;
  int *pixmapdefs_calc;

  Blt blt;

#ifdef USE_GFX_OP_QUEUE
  void *op_queue;
#endif
  bool must_redraw;
};
#define NextWindow(a) (struct Tracker_Windows *)((a)->l.next)

/* curr_track types */
#define TEMPONODETRACK -1
#define TEMPOTRACK -2
#define LPBTRACK -3
#define LINENUMBTRACK -4
#define TEMPOCOLORTRACK -5
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
	NInt curr_block;

	bool setfirstpos;

	int tempo;			/* Standard tempo. */
	int lpb;				/* Standard lpb. */

	float quantitize;
	int keyoct;
        int min_standardvel;
        int standardvel;

	bool editonoff;
	bool scrollplayonoff;
};




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












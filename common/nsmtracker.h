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

  Note, the files OS_Visual.h and OS_Semaphores.h are OS spesific
  and must be put into their respective directories. The
  OS_*_proc.h files are not.
******************************************************************/

#ifndef TRACKER_DEFINE
#define TRACKER_DEFINE 1

#ifdef __cplusplus
#  define LANGSPEC "C"
#else
#  define LANGSPEC
#endif


#include <limits.h>
#include <stdint.h>
#include <stdbool.h>
#include "debug_proc.h"
#include "memory_proc.h"
#include "nsmtracker_events.h"
#include "OS_error_proc.h"
#include <OS_Semaphores.h>


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

typedef int32_t STime;		/* Time can be negative. */
typedef STime NInt;
#define PFREQ 48000			/* Subseconds for STime */
//#define PFREQ 1200

#define MAXBLOCKRELTIME 6.0f
#define MINBLOCKRELTIME 0.001f

#include <OS_Visual.h>



#ifdef max
#undef max
#endif

#define max(a,b) (((a)>(b))?(a):(b))

#ifdef min
#undef min
#endif

#define min(a,b) (((a)<(b))?(a):(b))

#ifdef abs
#undef abs
#endif

#define abs(a) ((a)<0?(-(a)):(a))

#define boundaries(a,b,c) (min(max((a),(b)),(c)))


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
	velocities.h
*********************************************************************/



struct Velocities{
	struct ListHeader3 l;

	int velocity;
};
#define NextVelocity(a) ((struct Velocities *)((a)->l.next))



/*********************************************************************
	notes.h
*********************************************************************/


struct Notes{
	struct ListHeader3 l;

	int note;
	int cents;
	int velocity;

	Place end;
	int velocity_end;
	
	struct Velocities *velocities;

	int noend;
};
#define NextNote(a) ((struct Notes *)((a)->l.next))



/*********************************************************************
	patch.h
*********************************************************************/

struct Tracks;

struct Patch{
	struct ListHeader1 l;
	char *name;
	int minvel;
	int maxvel;
	int standardvel;
	void (*playnote)(int notenum,int velocity,struct Tracks *track,struct Notes *note);
	void (*changevelocity)(int velocity,struct Tracks *track,struct Notes *note);
	void (*stopnote)(int notenum,int velocity,struct Tracks *track,struct Notes *note);
	void (*closePatch)(void);

	void *patchdata;		// Free use by the instrument plug-in.

	void (*changeTrackPan)(int newpan,struct Tracks *track);
};
#define PATCH_FAILED 0
#define PATCH_SUCCESS 1
#define NextPatch(a) ((struct Patch *)((a)->l.next))

/*********************************************************************
	fx.h
*********************************************************************/


struct FX{
	struct ListHeader1 l;
	char *name;
	int color;
	void (*configureFX)(struct FX *fx,struct Tracks *track);
	int min;
	int max;
	void (*treatFX)(struct FX *fx,int val,struct Tracks *track,int skip);
	void (*closeFX)(struct FX *fx,struct Tracks *track);
	void *fxdata;	//Free use for the instrument plug-in.
	void (*SaveFX)(struct FX *fx,struct Tracks *track);
};
#define FX_FAILED 0
#define FX_SUCCESS 1


/*********************************************************************
	instruments.h
*********************************************************************/

struct Tracker_Windows;
struct Instruments{
	struct ListHeader1 l;

	struct Patch *patches;

	char *instrumentname;
	int (*getMaxVelocity)(struct Tracks *track);
	int (*getFX)(struct Tracker_Windows *window,struct Tracks *track,struct FX *fx);
	int (*getPatch)(struct Tracker_Windows *window,ReqType reqtype,struct Tracks *track,struct Patch *patch);
	void (*treatSpecialCommand)(char *command,struct Tracks *track);
	void (*CloseInstrument)(struct Instruments *instrument);
	void (*SelectTrackInstrument)(struct Tracks *track,struct Instruments *instrument);
	void (*StopPlaying)(struct Instruments *instrument);
	int (*getStandardVelocity)(struct Tracks *track);
	void (*PP_Update)(struct Instruments *instrument,struct Patch *patch);
	void *(*CopyInstrumentData)(struct Tracks *track);		//Necesarry for undo.

	void (*PlayFromStartHook)(struct Instruments *instrument);

	void *(*LoadFX)(struct FX *fx,struct Tracks *track);

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
	int onoff;							/* 1=on, 0=off */

	char *trackname;
	struct Instruments *instrument; /* Only referenced. */
	struct Patch *patch;
	struct FXs *fxs;

	void *instrumentdata;			/* Free use for the instrument plug-in. */

	int pan;
	int volume;

	bool panonoff;
        bool volumeonoff;                      /* The volume-button on/off, not track on/off. */
};
#define NextTrack(a) (struct Tracks *)((a)->l.next)

#define MAXTRACKVOL 1000
#define MAXTRACKPAN 1000

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


#define TRE_Max INT16_MAX
struct TrackReallineElements{
	struct TrackReallineElements *next;
   SDB
	int type;										/* Dont use number higher than TRE_Max, because of TRE_GC. */
	int subtype;
	float y1,y2;
	float x1,x2;
	void *pointer;									/* References only, TREs are allocated atomic. */
};
/************* Types: */
#define TRE_THISNOTELINES 0
#define TRE_VELLINECENTS 0x5
#define TRE_VELLINENODE 0x10
#define TRE_VELLINE 0x20
#define TRE_VELLINESTART 0x30
#define TRE_VELLINEEND 0x40
#define TRE_STOPLINE 0x90
#define TRE_REALSTARTSTOP 0xa0

/* Subtype for 0-0x40 is
   the same as subtrack for the note.
*/


struct TrackRealline{
	int note;										/* Is 0 if no note. */
   SDB
	struct TrackReallineElements *trackreallineelements;
};
#define NOTE_MUL 128
#define NOTE_STP 129
#define NOTE_MUR 130

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
	int x1,y1,x2,y2;
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

	int l_num_vel;						/* Last drawn number of velocity lines showed sim. */
	int num_vel;						/* Max number of velocity lines showed simultaniously. */

	struct Tracks *track;			/* Only referenced. wtracknum=track->tracknum */

	struct TrackRealline *trackreallines;
	WFXNodes **wfxnodes;

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
	STime time;									/* Start-time for the line. */
   SDB
	struct STimeChanges *timechanges;
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

	struct STimes *times;			/* Pointer to array. Last element (times[num_lines]) is the playtime of the block. */

	float reltempo;					/* factor that the tempo is multiplied with when playing this block. */

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

	TBox a;
//	int x,y,x2,y2;						/* GFX area. */

	TBox t;
//	int tx,ty,tx2,ty2;				/* lines, nodes, etc. GFX area. */

	WArea zoomlevelarea;
	WArea linenumarea;
        WArea tempocolorarea;
	WArea lpbTypearea;
	WArea lpbarea;
	WArea tempoTypearea;
	WArea tempoarea;
	WArea temponodearea;

	YArea linearea;

	int maxwtracksize;					/* The size of the widest wtrack. */

	int num_visiblelines;

	int top_realline;

	int curr_realline;
	int till_curr_realline;				/* The player-routine sets this on. */

	int bot_realline;

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
	struct WLPBs *wlpbs;
	float reltempomax;

	bool isranged;
	NInt rangex1;
	NInt rangex2;
	NInt rangey1;
	NInt rangey2;

	bool isgfxdatahere;

	TBox reltempo;

  // This variable is checked after each keyboard or menu event. If true, trackreallines, wtracks, etc. will be updated.
  bool is_dirty; 

  //tempocolor stuff
  bool tc_onoff;
  int tc_numcolors;
  int tc_maxtime;
  int tc_mintime;
  int tc_type;
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

	struct OS_visual *os_visual;
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
	TBox resizebox;

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
	char *h_fontname;
	int h_fontID;							/* System spesific. For amiga: fontsize. */
	int h_fontTags;						/* System spesific. For amiga: nothing. */
	int h_fontwidth;

	bool textborder;

	int minnodesize;

  int num_pixmapdefs;
  int *pixmapdefs;
  int *pixmapdefs_calc;

  Blt blt;
};
#define NextWindow(a) (struct Tracker_Windows *)((a)->l.next)

/* curr_track types */
#define TEMPONODETRACK -1
#define TEMPOTRACK -2
#define LPBTRACK -3
#define LINENUMBTRACK -4
#define TEMPOCOLORTRACK -5


/*********************************************************************
	song.h
*********************************************************************/


struct Song{
	struct Tracker_Windows *tracker_windows;
	struct Blocks *blocks;
	struct Blocks **playlist;			/* This variable is just temporarily. Later, the playlist will be much more advanced. */
	struct Instruments *instruments;

	NInt num_blocks;
	int length;								/* Playlist length. */
	char *songname;

	NInt maxtracks;						/* The highest number of tracks in a block. (changed when exceeding) */
};



/*********************************************************************
	root.h
*********************************************************************/


struct Root{
	struct Song *song;
	struct Instruments *def_instrument; /* Default instrument. */

	
	int curr_playlist;
	NInt curr_block;

	bool setfirstpos;

	int clearall;

	int tempo;			/* Standard tempo. */
	int lpb;				/* Standard lpb. */

	float quantitize;
	int keyoct;
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












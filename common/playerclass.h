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



#ifndef TRACKER_PLAYERCLASS
#define TRACKER_PLAYERCLASS



#if 0
/*********************************************************************
 This is an oop struct. 'TreatMe' is a virtual procedure. Because
 of simplicity (GC_malloc can not be used from the player-thread, see
 PEQmempool.c), subclasses are not used.
**********************************************************************/
struct PEventQueue{
	struct ListHeaderP l;

	void (*TreatMe)(struct PEventQueue *peq, int doit);

// Used by various functions.
        const struct SeqTrack *seqtrack;
	const struct Tracker_Windows *window;
        struct WBlocks *wblock; // not const.
	const struct Blocks *block;
	const struct Tracks *track;
	struct Notes *note;

// Used by PEQrealline
	int realline;

// Used by PEQline
	int line;


// Used by PEQvelocities
	const struct Velocities *velocity;
	const struct Velocities *nextvelocity;

// Used by PEQpitches
	struct Pitches *pitch;
	struct Pitches *nextpitch;


// Used by PEQfxs
	const struct FXs *fxs;
	const struct FXNodeLines *fxnodeline;
	const struct FXNodeLines *nextfxnodeline;


// Used by PEQvelocities and PEQfxs
	STime time1;
	STime time2;
};
#define NextPEventQueue(a) ((struct PEventQueue *)(a->l.next))


// Peq-types: (return-value from the PEQ_GetType function).

#define PEQT_NEWBLOCK 0
#define PEQT_FIRSTFX 1
#define PEQT_FX 2
#define PEQT_STARTNOTE 3
#define PEQT_STOPNOTE 4
#define PEQT_NEWREALLINE 5
#define PEQT_VELFROMSTART 6
#define PEQT_VEL 7
#define PEQT_VELTOEND 8
#define PEQT_VELFROMSTARTTOEND 9

#endif


typedef struct{
  int playtype;
  
  int64_t abstime;
  struct SeqTrack *seqtrack;
  struct SeqBlock *seqblock;
  Place place; // Is only valid if seqtrack!=NULL (and therefore also seqblock)
} player_start_data_t;


typedef enum {
  PLAYER_STATE_PROGRAM_NOT_READY = 0,
  PLAYER_STATE_STARTING_TO_PLAY,
  PLAYER_STATE_PLAYING,
  PLAYER_STATE_STOPPING,
  PLAYER_STATE_STOPPED,
  PLAYER_STATE_ENDING
} Player_State;

typedef struct{

//private

//	struct PEventQueue *peq;	// Player events.

        int pfreq; // player frequency. i.e. sample rate. TODO: Get rid of this one. Use MIXER_getsamplerate instead.

        volatile double start_time; // During current call to peq->treatMe
        volatile double end_time;   // During current call to peq->treatMe

        DEFINE_ATOMIC(double, start_time_f);       // double version of start_time.
        double end_time_f;         // double version of end_time.

        DEFINE_ATOMIC(STime, song_abstime); // Used by the cursor position in the sequencer. Absolute time, i.e. not seqtime.
	DEFINE_ATOMIC(STime, abstime_since_starting_to_play);	// Absolute time, i.e. not seqtime.

        STime reltime; // The argument for PlayerTask. Will usually contain the audio blocksize. Necessary for calculating delta time. (I'm 99% this value is always equal to RADIUM_BLOCKSIZE, i.e. 64)

        DEFINE_ATOMIC(Player_State, player_state);

        /*
	DEFINE_ATOMIC (bool, isplaying);
	DEFINE_ATOMIC (bool, initplaying);
        DEFINE_ATOMIC (bool, has_stopped);
        */
        
        bool is_treating_editor_events; // Used by "SCHEDULER_add_event" to determine whether to run events (which belongs to the current block) NOW, or schedule it.
        //DEFINE_ATOMIC(bool, playertask_has_been_called); // if true, we can be sure that the timing values are valid.
        
	int playtype;

        // These two are used when looping a range.
        bool is_playing_range;
        STime range_duration;
        
	//struct Blocks *block;		// The block now playing. Must be read and written atomically, except when we are reading in the player thread. Can only be written to in the player thread.

	STime pausetime;
	bool nowpausing;

        DEFINE_ATOMIC(int, play_id); // A counter. Increased each time the program starts playing, and stops playing.

}PlayerClass;

extern PlayerClass *pc;


// Should only be used if it's not important if the variables that are initialized when starting to play, have actually been initialized.
// If that is important, then ATOMIC_GET(pc->player_state)==PLAYER_STATE_PLAYING must be used instead.
static inline bool is_playing(void){
  if (pc==NULL)
    return false;
  Player_State state = ATOMIC_GET(pc->player_state);
  return state==PLAYER_STATE_STARTING_TO_PLAY || state==PLAYER_STATE_PLAYING;
}

static inline void init_player_state(void){
  ATOMIC_SET(pc->player_state, PLAYER_STATE_STOPPED);
}


//playtypes:
#define PLAYSONG 0
#define PLAYBLOCK 1
#define PLAYRANGE 2 // Must never be set. We set pc->is_playing_range to true instead.
#define PLAYBLOCK_NONLOOP 3

//typedef struct ListHeader1 PEQ_UsedTracks;

#endif


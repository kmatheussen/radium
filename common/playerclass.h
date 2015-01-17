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




/*********************************************************************
 This is an oop struct. 'TreatMe' is a virtual procedure. Because
 of simplicity (GC_malloc can not be used from the player-thread, see
 PEQmempool.c), subclasses are not used.
**********************************************************************/
struct PEventQueue{
	struct ListHeaderP l;

	void (*TreatMe)(struct PEventQueue *peq, int doit);

// Used by all
	int playpos;				// Position in the playlist this event was generated from.


// Used by various functions.
	const struct Tracker_Windows *window;
        struct WBlocks *wblock; // not const.
	const struct Blocks *block;
	const struct Tracks *track;
	const struct Notes *note;


// Used by PEQrealline
	int realline;

// Used by PEQline
	int line;


// Used by PEQvelocities
	const struct Velocities *velocity;
	const struct Velocities *nextvelocity;

// Used by PEQpitches
	const struct Pitches *pitch;
	const struct Pitches *nextpitch;


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


typedef struct{

//private

	struct PEventQueue *peq;	// Player events.

        int pfreq; // player frequency. i.e. sample rate.

        volatile STime start_time; // During current call to peq->treatMe
        volatile STime end_time;   // During current call to peq->treatMe

	volatile STime therealtime;	// Shows the real time, not taking the block->reltempo variable into consideration. Only used by PEQ_clock and PTask2MTask.c.

        STime reltime; // The argument for PlayerTask. Will usually contain the audio blocksize. Necessary for calculating delta time.

	STime seqtime;		/* Time being played at the top of the block that now is playing. */

	volatile bool isplaying;
	volatile bool initplaying;

        volatile bool playertask_has_been_called; // if true, we can be sure that the timing values are valid.
        
	int playtype;

	struct Blocks *block;		// The block now playing.

	volatile int playpos;				// Number of blocks currently being played. Not the same as root->curr_playlist.
	STime pausetime;
	bool nowpausing;

}PlayerClass;

//playtypes:
#define PLAYSONG 0
#define PLAYBLOCK 1
#define PLAYRANGE 2
#define PLAYBLOCK_NONLOOP 3

typedef struct ListHeader1 PEQ_UsedTracks;

#endif


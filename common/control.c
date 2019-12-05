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









#include "nsmtracker.h"
//#include "PEQmempool_proc.h"
//#include "PEQ_type_proc.h"
#include "playerclass.h"
#include "song_proc.h"
#include "patch_proc.h"
#include "instruments_proc.h"
#include "OS_endprogram_proc.h"
#include "input.h"
//#include "PEQ_clock_proc.h"
#include "../audio/Mixer_proc.h"
#include "../midi/midi_i_input_proc.h"
#include "scheduler_proc.h"
#include "quantitize_proc.h"

#include "control_proc.h"

struct Root *root=NULL;


extern PlayerClass *pc;

vector_t g_symbols = {0};

extern void RADIUM_ensure_bin_packages_gc_is_used(void);

bool InitProgram(void){
//	GC_INIT();
  bool ret;

#if !defined(FOR_MACOSX)
  RADIUM_ensure_bin_packages_gc_is_used();
#endif
  
  printf("Initializing...\n");

	printf("...Error handler\n");
	Error_init();

	printf("...Memory handler\n");
	init_memory();

	root=tralloc(sizeof(struct Root));

	if(root==NULL){
		fprintf(stderr,"Not enough memory\n");
		return false;
	}

	root->keyoct=36;
        root->quantitize_options = Quantitize_get_default_options();
        root->grid_numerator=1;
        root->grid_denominator=1;
        root->min_standardvel=MAX_VELOCITY*40/100;
	root->standardvel=MAX_VELOCITY*80/100;
        ATOMIC_SET(root->editonoff, true);
        ATOMIC_SET(root->play_cursor_onoff, false);
        ATOMIC_SET(root->editor_follows_play_cursor_onoff, false);

	root->song=SONG_create();
          
	pc=tralloc(sizeof(PlayerClass));

	if(root->song==NULL || pc==NULL){
		fprintf(stderr,"Not enough memory\n");
		return false;
	}

        pc->pfreq = 48000; // Default value. Should be overridden in MIXER_start().

/*
	if( ( ! InitPEQmempool(1000) )  || ( ! Input_Init(4000) ) ){	// 1000 and 4000 are hardcoded values. Not good.
		return false;
	}
*/

        printf("...Midi\n");
        MIDI_input_init();

        SCHEDULER_init();

        PATCH_init();

	printf("...Sound\n");
        if(MIXER_start()==false){
          fprintf(stderr,"Could not open Sound\n");
          return false;
        }

	printf("...Player 1/2\n");

#if 0
	if( ( ! InitPEQmempool() )   ){	// 1000 and 4000 are hardcoded values. Not good.
		return false;
	}
#endif

#if 0
	printf("...Clock handler\n");

	if( ! InitClock() ) return false;
#endif
        
	printf("...Player 2/2\n");

#if 0
	PEQ_GetType_Init();
#endif
        
	printf("...Instrument\n");

	if( OpenInstruments()==false ){
          return false;
        }

	printf("...Kebang\n");

	ret=NewSong();

#if !USE_OPENGL
	printf("...Blitting\n");

	if(ret==true){
	  Blt_blt(root->song->tracker_windows);
	}
#endif

	printf("Initialization finished.\n");
	return ret;
}


void EndProgram(void){

	OS_EndProgram();

	printf("Closing down os independent stuff...\n");

	printf("...Instruments\n");
	//CloseAllInstruments(); // Removed since it's still used in the audio thread, which is still alive.

	printf("...Song\n");
	ClearSong();

#if 0
	printf("...Clock handler\n");
	CloseClock();
#endif
        
	printf("...Error handler\n");
	Error_uninit();

//	Input_uninit();

	printf("Finished closing down OS independent stuff.\n\n");

	OS_EndProgram2();

	fflush(stdout);
	fflush(stdin);
}

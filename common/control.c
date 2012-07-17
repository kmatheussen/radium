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
#include "PEQmempool_proc.h"
#include "PEQ_type_proc.h"
#include "playerclass.h"
#include "song_proc.h"
#include "instruments_proc.h"
#include "OS_endprogram_proc.h"
#include "input.h"
#include "PEQ_clock_proc.h"
#include "blts_proc.h"

#include "control_proc.h"

struct Root *root=NULL;


extern PlayerClass *pc;

extern void RADIUM_ensure_bin_packages_gc_is_used();

bool InitProgram(void){
//	GC_INIT();
  bool ret;

  RADIUM_ensure_bin_packages_gc_is_used();

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

	root->keyoct=48;
	root->quantitize=0.5f;
	root->standardvel=100;
	root->scrollplayonoff=true;

	root->song=talloc(sizeof(struct Song));

	pc=tralloc(sizeof(PlayerClass));

	if(root->song==NULL || pc==NULL){
		fprintf(stderr,"Not enough memory\n");
		return false;
	}


/*
	if( ( ! InitPEQmempool(1000) )  || ( ! Input_Init(4000) ) ){	// 1000 and 4000 are hardcoded values. Not good.
		return false;
	}
*/

	printf("...Player 1/2\n");

	if( ( ! InitPEQmempool(1000) )   ){	// 1000 and 4000 are hardcoded values. Not good.
		return false;
	}


	printf("...Clock handler\n");

	if( ! InitClock() ) return false;

	printf("...Player 2/2\n");

	PEQ_GetType_Init();

	printf("...Instrument\n");

	if( ! OpenInstrument() ) return false;

	printf("...Kebang\n");

	ret=NewSong();

	printf("...Blitting\n");

	if(ret==true){
	  Blt_blt(root->song->tracker_windows);
	}

	printf("Initialization finished.\n");

	return ret;
}


void EndProgram(void){

	OS_EndProgram();

	printf("Closing down os independent stuff...\n");

	printf("...Instruments\n");
	CloseAllInstruments();

	printf("...Song\n");
	ClearSong();

	printf("...Clock handler\n");
	CloseClock();

	printf("...Error handler\n");
	Error_uninit();

//	Input_uninit();

	printf("Finished closing down OS independent stuff.\n\n");

	OS_EndProgram2();

	fflush(stdout);
	fflush(stdin);

}

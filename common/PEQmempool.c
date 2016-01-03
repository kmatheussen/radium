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



/******************* overview ***************************************
  PEventQueue elements is used by the player-task. This one should
  be as fast as possible, and since it quite often have to allocate
  new PEventeQueue elements, its best to have a pool for this.
  (besides, the amiga-port of gc doesn't (officially) support
  multi-threading, so this is somewhat necesarry for the amiga-port.)
*********************************************************************/

#include <string.h>

#include "nsmtracker.h"
#include "visual_proc.h"

#include "playerclass.h"
#include "OS_Player_proc.h"

#include "t_gc_proc.h"

#include "PEQmempool_proc.h"

#define SAFETY_BUFFER 500
#define INITIAL_NUM_ELEMENTS 2000

extern PlayerClass *pc;

static struct PEventQueue peqrootelements[INITIAL_NUM_ELEMENTS] = {0}; // store as a static variable so the gc can scan it.

static struct PEventQueue *peqroot = NULL;

static int num_elements_used = 0;

bool InitPEQmempool(void){
	int lokke;

        R_ASSERT(num_elements_used==0);
        
        memset(peqrootelements, 0, INITIAL_NUM_ELEMENTS*sizeof(struct PEventQueue)); // So the garbage collector can free unused memory.

        peqroot = NULL;
        
	for(lokke=0;lokke<INITIAL_NUM_ELEMENTS;lokke++){
          struct PEventQueue *temp = &peqrootelements[lokke];

          temp->l.next = &peqroot->l;
          
          peqroot=temp;
	}

	return true;
}


struct PEventQueue *GetPEQelement(void){

	struct PEventQueue *temp=peqroot;

	if(temp==NULL){ // This should REALLY not happen.

          //temp=GC_malloc(sizeof(struct PEventQueue));
          temp=malloc(sizeof(struct PEventQueue));
          if(temp==NULL){
            RT_message("Error. Out of memory on a very bad place. Now probably crashing.\n");
          }
          
	} else {

          peqroot=NextPEventQueue(temp);

        }

        ++num_elements_used;
        //printf("   +NUM ELEMENTS: %d\n",num_elements_used);

        if(num_elements_used > INITIAL_NUM_ELEMENTS-SAFETY_BUFFER){
          RT_message("<p>Stopping player.</p><p>We are at risk of running out of memory for the player.</p>This is a bug in Radium, and is not supposed to happen.</p><p>Please report this bug.</p>");
          RT_request_to_stop_playing();
        }
        
	return temp;

}

void ReturnPEQelement(struct PEventQueue *element){
  //memset(element, 0, sizeof(struct PEventQueue));

  --num_elements_used;

  //printf("   -NUM ELEMENTS: %d\n",num_elements_used);
          
  element->l.next= &peqroot->l;
  peqroot=element;
}

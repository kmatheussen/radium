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



#include "nsmtracker.h"
#include "playerclass.h"

#include "t_gc_proc.h"

#include "PEQmempool_proc.h"


extern PlayerClass *pc;


struct PEventQueue *peqroot;

bool InitPEQmempool(int num_elements){
	struct PEventQueue *temp;
	int lokke;

	for(lokke=0;lokke<num_elements;lokke++){
		temp=malloc(sizeof(struct PEventQueue));		// Its not supposed to ever be freed, and its not supposed to hold pointers to be scanned. Therefore 'malloc'. GC provide a special function for this: 'GC_malloc_atomic_uncollectable', but it "roughly" (word used in documetation of GC) does the same as malloc anyway.
		if(temp==NULL){
			RError("Out of memory\n");
			return false;
		}
		temp->l.next= &peqroot->l;

		peqroot=temp;
	}
	return true;
}

//void *my_fastcalloc(size_t size);

struct PEventQueue *GetPEQelement(void){
	struct PEventQueue *temp=peqroot;
	peqroot=NextPEventQueue(temp);

	if(temp==NULL){
		RError("Warning. Peqmempool empty. (Set it higher, it must be very low now!)\n");
		pc->isplaying=false;
		temp=calloc(1,sizeof(struct PEventQueue));
		if(temp==NULL){
			RError("Error. Out of memory on a very bad place. Now probably crashing.\n");
		}
	}

	return temp;

}

void ReturnPEQelement(struct PEventQueue *element){
	element->l.next= &peqroot->l;
	peqroot=element;
}




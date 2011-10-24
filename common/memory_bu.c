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


#include <stdlib.h>
#include <stdio.h>
#include <proto/exec.h>
#include <exec/memory.h>
#include "t_gc_proc.h"
#include "nsmtracker.h"
#include "visual_proc.h"
#include "trackreallineelements_proc.h"
#include "control_proc.h"

#include "memory_proc.h"



extern struct Root *root;

size_t tmemorysize=32000;		/* Hardcoded, must be unhardcoded later. */
int tmemoryisused=1;

void *tmemory;

int init_memory(void){
	tmemory=GC_malloc_atomic(tmemorysize);
	tmemoryisused=0;

#ifdef MEMORY_DEBUG
	GC_exclude_static_roots(&GC_AM,&GC_AM+1);
#endif

	return (int) tmemory;
}

#ifdef MEMORY_DEBUG

struct GC_AllocedMemoryHeader{
	struct GC_AllocedMemoryHeader *next;
	ULONG size;
};

struct GC_AllocedMemoryHeader *GC_AM=NULL;


ULONG MEMF_TRACKER = MEMF_FAST | MEMF_CLEAR;


void uinit_memory(void){
	struct GC_AllocedMemoryHeader *gc_am=GC_AM;
	struct GC_AllocedMemoryHeader *temp;

	while(gc_am!=NULL){
		temp=gc_am->next;
		FreeMem(gc_am,gc_am->size);
		gc_am=temp;
	}
}

#endif

void tfree(void *element){
#ifdef MEMORY_DEBUG
	struct GC_AllocedMemoryHeader *gc_am=GC_AM;
	struct GC_AllocedMemoryHeader *temp=NULL;
#endif

	if(element==NULL){
		RError("Warning, attempting to free NULL\n");
		return;
	}

#ifdef MEMORY_DEBUG

//	fprintf(stderr,"about to free\n");

	while(gc_am!=(void *)(((char *)(element))-sizeof(struct GC_AllocedMemoryHeader))){
		temp=gc_am;
		gc_am=gc_am->next;
		if(gc_am==NULL){
			RError("Error. Element %x, was not in allocated list. Can't free\n",element);
			return;
		}
	}

	if(temp==NULL){
		GC_AM=gc_am->next;
	}else{
		temp->next=gc_am->next;
	}

	FreeMem(gc_am,gc_am->size);

//	fprintf(stderr,"freeing successful\n");

#endif

#ifndef MEMORY_DEBUG
	GC_free(element);
#endif

}


/*******************************************************************
  This function is called from gc.lib to allocate memory.
  It uses amigaOS's own AllocMem to allocate to be shure
  to get the right type. (checking the result of calloc
  could allso have been done, but size is often near 1MB
  in size, and sas/c doesn't free memory before the next
  free). By using AllocMem, I allso allocate 8 bytes more
  to keep track of all allocated memory and its size, which
  is put before the return-value.

  This is amiga-spesific, and this file is not
  supposed to be an OS-spesific source-file. So something
  has to be done here before porting.

  The reason for doing all this, is not to allocate CHIP-mem
  before all not-viewed gfx-data is freed or returned to GCs mem-pool.
  A MAJOR speed-up, in other words.
*******************************************************************/
/*
void *my_fastcalloc(size_t size){
	struct GC_AllocedMemoryHeader *gc_am;

//	fprintf(stderr,"\n\nGC: trying to allocate: %d bytes\n",size);
	gc_am=AllocMem((ULONG)(size + sizeof(struct GC_AllocedMemoryHeader)),MEMF_TRACKER);

	if(gc_am==NULL) return NULL;

//	fprintf(stderr,"\n\nGC: have allocated: %d bytes\n",size);

//	if(size==8) fprintf(stderr,"Allocating 8 now.\n");

	gc_am->next=GC_AM;
	gc_am->size=size + sizeof(struct GC_AllocedMemoryHeader);
	GC_AM=gc_am;


	return ((char *)gc_am)+sizeof(struct GC_AllocedMemoryHeader);
}

*/

void ShutDownYepp(void){

	EndProgram();

	exit(1);
}

size_t allocated=0;


void *tracker_alloc_clean(size_t size,void *(*AllocFunction)(size_t size2)){
#ifndef MEMORY_DEBUG
	return (*AllocFunction)(size);
#endif
#ifdef MEMORY_DEBUG
	allocated+=size;
	return my_fastcalloc(size);		// For debugging. (wrong use of GC allocated memory can be very difficult to trace)
#endif

}

void *tracker_alloc(size_t size,void *(*AllocFunction)(size_t size2)){
	void *ret;

#ifndef MEMORY_DEBUG
	ret=(*AllocFunction)(size);
#endif
#ifdef MEMORY_DEBUG
	ret=my_fastcalloc(size);		// For debugging. (wrong use of GC_malloced memory could be very difficult to trace)
#endif

	allocated+=size;

	if(ret!=NULL) return ret;

#ifndef MEMORY_DEBUG
	ret=(*AllocFunction)(size);
#endif
#ifdef MEMORY_DEBUG
	ret=my_fastcalloc(size);		// For debugging. (wrong use of GC_malloced memory could be very difficult to trace)
#endif

	if(ret!=NULL) return ret;

/*
	if(MEMF_TRACKER==(MEMF_FAST | MEMF_CLEAR)){
		RError("\n\n\n\n\nIMPORTANT!!!!\n\n");
		RError("You have now no more fastmem left. Beware that the program might quit anytime.\n");
		RError("And secondly, the program will now run slower because it has to access chip-mem.\n");
		RError("So, what you should do is; save your song, quit, and start the program again. And perhaps... buy some more memory?\n");
		RError("\nNote. This is not a bug or bad programming, but something that makes your program generally faster");
		RError("and become much smaller in size.\n\n\n\n");

		MEMF_TRACKER = MEMF_ANY | MEMF_CLEAR;

		return tracker_alloc(size,AllocFunction);
	}
*/

	return NULL;
}

void *tralloc(size_t size){
	return tracker_alloc(size,GC_malloc);
}

void *tralloc_atomic(size_t size){
	return tracker_alloc(size,GC_malloc_atomic);
}



/************************************************************
   FUNCTION
     Does never return NULL.
************************************************************/
void *talloc(size_t size){
	void *ret;

	if(size>tmemorysize){
		RError("\n\n\nYour out-of-memory-buffer is too low!!!\n");
		RError("Save and quit, and set the out-of memory-buffer\n");
		RError("to at least %d bytes.\n\n\n",size);
	}

	ret=tracker_alloc(size,GC_malloc);

	if(ret!=NULL) return ret;

	RError("\n\n\n NEARLY OUT MEMORY!!!! SAVE AND QUIT IMMIDIATELY!!!!\n\n\n");

	if(size>tmemorysize){
		RError("\n\n\n OUT OF MEMORY, AND YOUR OUT-OF-MEMORY-BUFFER IS TOO LOW!!! Later, SET THE OUT-OF MEMORY-BUFFER to at least %d bytes.\n",size);
	}

	if(tmemoryisused==0){
		tmemoryisused=1;
		GC_free(tmemory);
		tmemory=NULL;
	}

#ifndef MEMORY_DEBUG
	ret=GC_malloc(size);
#endif
#ifdef MEMORY_DEBUG
	ret=my_fastcalloc(size);		// For debugging. (wrong use of GC_malloced memory could be very difficult to trace)
#endif
	if(ret!=NULL) return ret;


	RError("\n\n\n OUT OF MEMORY!!!! NOW QUITTING.\n\n\n");
	ShutDownYepp();
}


void *talloc_atomic(size_t size){
	void *ret;

	ret=tracker_alloc(size,GC_malloc_atomic);

	if(ret!=NULL) return ret;

	if(tmemoryisused==0){
		tmemoryisused=1;
		GC_free(tmemory);
		tmemory=NULL;
	}

#ifndef MEMORY_DEBUG
	ret=GC_malloc_atomic(size);
#endif
#ifdef MEMORY_DEBUG
	ret=my_fastcalloc(size);		// For debugging. (wrong use of GC_malloced memory could be very difficult to trace)
#endif
	if(ret!=NULL) return ret;


	RError("\n\n\n OUT OF MEMORY!!!! NOW QUITTING (atomic allocator).\n\n\n");
	ShutDownYepp();
}


void *talloc_atomic_uncollectable(size_t size){
	void *ret;

	ret=tracker_alloc(size,GC_malloc_atomic_uncollectable);

	if(ret!=NULL) return ret;

	if(tmemoryisused==0){
		tmemoryisused=1;
		GC_free(tmemory);
		tmemory=NULL;
	}

#ifndef MEMORY_DEBUG
 	ret=GC_malloc_atomic_uncollectable(size);
#endif
#ifdef MEMORY_DEBUG
	ret=my_fastcalloc(size);		// For debugging. (wrong use of GC_malloced memory could be very difficult to trace)
#endif
	if(ret!=NULL) return ret;


	RError("\n\n\n OUT OF MEMORY!!!! NOW QUITTING (atomic_uncollectable allocator).\n\n\n");
	ShutDownYepp();
}






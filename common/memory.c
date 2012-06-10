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
#include <string.h>

#include "t_gc_proc.h"
#include "nsmtracker.h"
#include "visual_proc.h"
#include "trackreallineelements_proc.h"
#include "control_proc.h"
#include "OS_memory_proc.h"

#include "memory_proc.h"



extern struct Root *root;

size_t tmemorysize=32000;		/* Hardcoded, must be unhardcoded later. */
int tmemoryisused=1;

void *tmemory;


int init_memory(void){
	tmemory=GC_malloc_atomic(tmemorysize);
	tmemoryisused=0;

	return (int) tmemory;
}


void tfree(void *element){
#ifdef MEMORY_DEBUG

	if(element==NULL){
		RError("Warning, attempting to free NULL\n");
		return;
	}

	OS_tfree(element);

#else

	GC_free(element);

#endif

}



void ShutDownYepp(void){

	EndProgram();

	exit(1);
}

size_t allocated=0;


void *tracker_alloc_clean(size_t size,void *(*AllocFunction)(size_t size2)){

#ifndef MEMORY_DEBUG
#	ifdef _AMIGA
		return (*GC_amiga_allocwrapper_do)(size,AllocFunction);
#	else
		return (*AllocFunction)(size);
#	endif
#else
	allocated+=size;
	return OS_getmem(size);		// For debugging. Wrong use of GC allocated memory can be very difficult to trace.
#endif

}

void *tracker_alloc(size_t size,void *(*AllocFunction)(size_t size2)){
	allocated+=size;

#ifndef MEMORY_DEBUG
#	ifdef _AMIGA
		return (*GC_amiga_allocwrapper_do)(size,AllocFunction);
#	else
		return (*AllocFunction)(size);
#	endif
#else
	return OS_getmem(size);		// For debugging. (wrong use of GC_malloced memory could be very difficult to trace)
#endif

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
     Clears memory.
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

	RError("\n\n\n NEARLY OUT MEMORY. SAVE AND QUIT IMMIDIATELY!\n\n\n");

	if(size>tmemorysize){
		RError("\n\n\n OUT OF MEMORY, AND YOUR OUT-OF-MEMORY-BUFFER IS TOO LOW! Later, SET THE OUT-OF MEMORY-BUFFER to at least %d bytes.\n",size);
	}

	if(tmemoryisused==0){
		tmemoryisused=1;
		GC_free(tmemory);
		tmemory=NULL;
	}

#ifndef MEMORY_DEBUG
	ret=GC_malloc(size);
#else
	ret=OS_getmem(size);		// For debugging. (wrong use of GC_malloced memory could be very difficult to trace)
#endif
	if(ret!=NULL) return ret;


	RError("\n\n\n OUT OF MEMORY! NOW QUITTING.\n\n\n");
	ShutDownYepp();
	return NULL;
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
#else
	ret=OS_getmem(size);		// For debugging. (wrong use of GC_malloced memory could be very difficult to trace)
#endif
	if(ret!=NULL) return ret;


	RError("\n\n\n OUT OF MEMORY! NOW QUITTING (atomic allocator).\n\n\n");
	ShutDownYepp();

	return NULL;
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
#else
	ret=OS_getmem(size);		// For debugging. (wrong use of GC_malloced memory could be very difficult to trace)
#endif
	if(ret!=NULL) return ret;


	RError("\n\n\n OUT OF MEMORY! NOW QUITTING (atomic_uncollectable allocator).\n\n\n");
	ShutDownYepp();
	return NULL;
}

char *talloc_strdup(char *input) {
  char *ret = talloc_atomic(strlen(input) + 1);
  sprintf(ret,"%s",input);
  return ret;
}

char *talloc_numberstring(int number){
  char s[1000];
  sprintf(s,"%d",number);
  return talloc_strdup(s);
}

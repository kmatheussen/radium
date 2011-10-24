#include <stdlib.h>
#include <proto/exec.h>
#include <exec/memory.h>
#include "nsmtracker.h"

#include "../common/OS_memory_proc.h"



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

void OS_tfree(void *element){
	struct GC_AllocedMemoryHeader *gc_am=GC_AM;
	struct GC_AllocedMemoryHeader *temp=NULL;

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
}


void *OS_getmem(size_t size){
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


void Amiga_out_of_fast(void){
		RError("\n\n\n\n\nIMPORTANT!\n\n");
		RError("You have now no more fastmem left. Beware that the program might quit anytime.\n");
		RError("And secondly, that the program will now run slower.\n");
		RError("So, what you should do is; save your song, quit, and start the program again. And probably: buy some more memory.\n");
}

extern void GC_amiga_set_toany(void (*func)(void));

void Amiga_memory_config(void){
//	printf("asefawefaewepoaierergnøaoiaroianaoririgarg\n");
	GC_amiga_set_toany(Amiga_out_of_fast);
}


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



/* A conservative garbitch collector. It is awfully slow, but it seems
   to work. GC_register_data_segments(void) and getstackbase(void) is ripped
   from Hans Boehms concervative garbage collector for c and c++. These
   two routines are from the amiga-spesific part of it, so it is not
   made by Hans Boehm, but [fill in names]. The difference between HBCGCFCAC
   and this one is: HBCGCFCAC is superior. (there is at least 7 years of
   development behind it, while this has something like 2 days. :))))) ).
   But this one does one thing that HBCGCFCAC doesnt: Free memory. If you
   have virtual memory, there is really not any big point in freing, but
   if not, it may be.

   It works by checking registers, stack and data-memory from the program,
   checking for pointers. Freeing previously allocated memory not reachable
   from the program anymore.
*/


#ifdef TRACKER_GB

#include "nsmtracker.h"

#include <stdio.h>
#include <stdlib.h>

#define __USE_SYSBASE 1	/* 4.5.2000 */
# include <exec/exec.h> /* 4.5.2000 */
# include <proto/exec.h>
# include <proto/dos.h>
# include <dos/dosextens.h>
# include <workbench/startup.h>
# include <dos.h>

struct t_memheader{
	struct t_memheader *next;
	size_t size;
	unsigned int row;
};
typedef struct t_memheader Hlist;

Hlist *last=NULL;
#define NUM_ROWS 1001
Hlist **rows;

int num_allocated_last=0;
int num_all=0;

//struct Atomic{
	char *top,*bot;
//};

//struct Atomic *atomic;

struct S_mem{
	struct S_mem *next;
	void *mem;
	size_t size;
};

struct S_mem *sroot=NULL;
void *stackbase;

extern struct Root *root;

unsigned int hasj(Hlist *element){
	unsigned int row=(unsigned int) element;
	row=(row>>2)%NUM_ROWS;
	return row;
}

void InsertHlist(Hlist *element){
	unsigned int row=hasj(element);
	element->row=row;

	element->next=rows[row];
	rows[row]=element;
}

void InsertHlist2(Hlist *element){
	element->next=last;
	last=element;
}

void InsertHlist3(Hlist *element){
	unsigned int row=element->row;

	element->next=rows[row];
	rows[row]=element;
}



void ABORT(char *s){
	fprintf(stderr,"%s",s);
	exit(2);
}

void *getstackbase(void){
    struct Process *proc = (struct Process*)SysBase->ThisTask;
 
    /* Reference: Amiga Guru Book Pages: 42,567,574 */

	if(
		proc->pr_Task.tc_Node.ln_Type==NT_PROCESS &&
		proc->pr_CLI != NULL
	){
		return (char *)proc->pr_ReturnAddr + sizeof(ULONG);
	}else{
		return (char *)proc->pr_Task.tc_SPUpper;
	}
}

void GC_add_roots_inner(
	char *a,
	char *b
){
	struct S_mem *temp;
	temp=calloc(1,sizeof(struct S_mem));
	temp->next=sroot;
	temp->mem=a;
	temp->size=b-a;
	sroot=temp;
	return;
}

void GC_register_data_segments(void){
	struct Process	*proc;
	struct CommandLineInterface *cli;
	BPTR myseglist;
	ULONG *data;
 
	int num;


	proc= (struct Process*)SysBase->ThisTask;

	/* Reference: Amiga Guru Book Pages: 538ff,565,573 and XOper.asm */

	if (proc->pr_Task.tc_Node.ln_Type==NT_PROCESS) {

		if (proc->pr_CLI == NULL) {
			myseglist = proc->pr_SegList;

		} else {
	    /* ProcLoaded	'Loaded as a command: '*/
	    cli = BADDR(proc->pr_CLI);
	    myseglist = cli->cli_Module;
		}

	}else{
		ABORT("Not a Process.");
 	}

	if (myseglist == NULL) {
	    ABORT("Arrrgh.. can't find segments, aborting");
 	}


	/* xoper hunks Shell Process */

	num=0;
	for(
		data = (ULONG *)BADDR(myseglist);
		data != NULL;
		data = (ULONG *)BADDR(data[0])
	){
		if(
			((ULONG) GC_register_data_segments < (ULONG) &data[1]) ||
	      ((ULONG) GC_register_data_segments > (ULONG) &data[1] + data[-1])
		){
	      GC_add_roots_inner(
				(char *)&data[1],
				((char *)&data[1]) + data[-1]
			);
		}

		++num;
	}
}

void GC_INIT(void){
	stackbase=getstackbase();
	printf("gcinit!!1\n\nroot:%x\n",root);
	GC_register_data_segments();
//	atomic=calloc(1,sizeof(struct Atomic));

/*
	GC_add_roots_inner((char *)root,(char *)(((char *)root)+sizeof(struct Root)));
*/

	rows=calloc(1,sizeof(void *)*NUM_ROWS);
}




Hlist *GetMemHeader(void *mem){
	char *a=(char *)mem;
	a -= sizeof(struct t_memheader);
	return (struct t_memheader *)a;
}

void *GetMemVoid(struct t_memheader *t_mem){
	char *a=(char *)t_mem;
	a += sizeof(struct t_memheader);
	return (void *)a;
}

int bass=0;

Hlist *CheckAddress(void *mem){
	Hlist *hmem=GetMemHeader(mem);
	unsigned int frow=hasj(hmem);
	Hlist *t_mem,*prev=NULL;
	t_mem=rows[frow];

	while(t_mem!=NULL){
		if(t_mem==hmem){
			if(prev==NULL)
				rows[frow]=t_mem->next;
			else
				prev->next=t_mem->next;
			InsertHlist2(t_mem);
			return t_mem;
		}
		prev=t_mem;
		t_mem=t_mem->next;
	}
	return NULL;
}

Hlist *CheckPointer(void *mem){
	char **cmem=(char **)mem;
	return CheckAddress(cmem[0]);
}

void MarkAndSweep(void *mem,size_t size){
	char *a,*c;
	Hlist *b;

	bass++;

	a=(char *)mem;
	c=a+size-4;

	while(a<=c){
		b=CheckPointer(a);
		if(b!=NULL){
			MarkAndSweep(GetMemVoid(b),b->size);
			a+=4;
		}else{
			a+=2;
		}
	}

}


void *getSP(void){
	return (void *)getreg(REG_A7);
}

int tfree=0;

void FreeMemRoot(void){
	Hlist *element;
	Hlist *temp=NULL;
	int lokke;
	unsigned int mi,ma,u=0;
	for(lokke=0;lokke<NUM_ROWS;lokke++){
		element=rows[lokke];
		rows[lokke]=NULL;
		while(element!=NULL){
			if(u==0)mi=ma=element->row;
			mi=min(mi,element->row);
			ma=max(mi,element->row);
			tfree++;
			temp=element->next;
			free(element);
			element=temp;
		}
	}
	printf("max: %d, min: %d\n",ma,mi);
}

void InsertUsedObjects(void){
	Hlist *temp;
	while(last!=NULL){
		temp=last->next;

//		InsertHlist3(last);
		last->next=rows[last->row];
		rows[last->row]=last;

		last=temp;
	}
}

void CollectGarbage(void){
	struct S_mem *s_mem=sroot;
	char *sb=(char *)stackbase;
	char *sp=getSP();
	struct t_memheader *b;
	bass=0;

	return;

	fprintf(stderr,"Registers!\n");
#define RegCheck(a) b=CheckAddress((void *)getreg(a)); \
	if(b!=NULL) {MarkAndSweep(GetMemVoid(b),b->size);bass--;}

	RegCheck(REG_D2);
	RegCheck(REG_D3);
	RegCheck(REG_D4);
	RegCheck(REG_D5);
	RegCheck(REG_D6);
	RegCheck(REG_D7);
	RegCheck(REG_A2);
	RegCheck(REG_A3);
	RegCheck(REG_A4);
	RegCheck(REG_A5);
	RegCheck(REG_A6);

	printf("stackbot: %x, stack: %x, stacksize: %d\n",sb,sp,sb-sp);
	fprintf(stderr,"Stack!\n");
	MarkAndSweep(sp,(size_t)(sb-sp));
	bass--;

	fprintf(stderr,"Garbage!\n");

	printf("root:%x\n",root);

	while(s_mem!=NULL){
		printf("mem:%x,size:%d\n",s_mem->mem,s_mem->size);
		MarkAndSweep(s_mem->mem,s_mem->size);
		bass--;
		s_mem=s_mem->next;
	}
	
	fprintf(stderr,"Freeing!\n");

	FreeMemRoot();
	InsertUsedObjects();


	fprintf(stderr,"bass: %d,FREE: %d\n",bass,tfree);
}


int first=0;

void *GC_malloc(size_t size){
	Hlist *t_mem;
	void *ret;
	num_allocated_last+=size;

	num_all++;

	return calloc(1,size);

	if(num_allocated_last>200000){
		CollectGarbage();
		printf("num_all: %d\n",num_all);
		num_all-=tfree;
		tfree=0;
		num_allocated_last=0;
	}


	t_mem=calloc(1,size+sizeof(Hlist));

	if((char *)t_mem <= (char *)SysBase->MaxLocMem){	/* Don't want to touch chip-mem or NULL. */
		free(t_mem);
		CollectGarbage();
		t_mem=calloc(1,size+sizeof(Hlist));
		if((char *)t_mem <= (char *)SysBase->MaxLocMem){
			free(t_mem);
			return NULL; /* We _really_ don't want chip-mem!. */
		}
	}

	t_mem->size=size;
	InsertHlist(t_mem);

	ret=GetMemVoid(t_mem);

	return ret;
}

void *GC_malloc_atomic(size_t size){
	return GC_malloc(size);
}


/* Will probably not work very good: */
void GC_free(void *mem){
	mem=NULL;
	CollectGarbage();
}

void GC_exclude_static_roots(void *a,void *b){
	return;
}

void GC_gcollect(void){
	CollectGarbage();
}


#endif




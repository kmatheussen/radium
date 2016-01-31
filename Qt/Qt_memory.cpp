/* Copyright 2003 Kjetil S. Matheussen

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



#include <new>
#include <stdlib.h>
#include <stdio.h>

#include "../common/nsmtracker.h"

#include "../common/OS_memory_proc.h"



#if 0

#include <gc.h>

// Segfault, probably some Qt root which is missing.

void* operator new(size_t size){
  static int num=1;
  printf("new: %d. size: %d\n",num++,(int)size);
  //return malloc(size);
  return GC_malloc(size);
}

void operator delete (void* mem){
  //printf("delete %p\n",mem);
  //free(mem);
}

void* operator new[](size_t size){
  //return malloc(size);
  return GC_malloc(size);
}

void operator delete[](void* mem)
{
  //free(mem);
}
#endif





void  OS_tfree(void *mem){
  V_free(mem);
}

// Used instead of GC_alloc when debugging memory, or GC_alloc returns NULL.
void *OS_getmem(size_t size){
  void *ret = V_calloc(1,size);
  if(ret==NULL)
    abort();
  return ret;
}


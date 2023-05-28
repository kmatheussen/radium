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
#include <unistd.h>
#include <stdarg.h>


#include "t_gc_proc.h"
#include "nsmtracker.h"
#include "visual_proc.h"
#include "control_proc.h"
#include "OS_memory_proc.h"
#include "OS_Player_proc.h"
#include "threading_lowlevel.h"
#include "memory_proc.h"


extern struct Root *root;

#if 0
static int tmemorysize=32000;		/* Hardcoded, must be unhardcoded later. */
static int tmemoryisused=1;

static void *tmemory;
#endif

static LockType ts_gc_lock;

void Threadsafe_GC_disable(void){
  LOCK_LOCK(ts_gc_lock); // Is this really enough? We don't lock other GC_* calls, and I don't think bdw-gc is compiled with thread support. Can't remember seeing any psan hits though.
  GC_disable();
  LOCK_UNLOCK(ts_gc_lock);
}

void Threadsafe_GC_enable(void){
  LOCK_LOCK(ts_gc_lock);
  GC_enable();
  LOCK_UNLOCK(ts_gc_lock);
}

void init_memory(void){
  LOCK_INITIALIZE(ts_gc_lock);
  
#ifndef DISABLE_BDWGC
  //	tmemory=GC_malloc_atomic(tmemorysize);
  //	tmemoryisused=0;
#endif
	//return (int) tmemory;
        return;
}

#if !defined(RELEASE)
#ifndef DISABLE_BDWGC
static void dummy_finalizer(void *actual_mem_start, void *user_data){
}
#endif    
#endif

void tfree(void *element){
  ASSERT_IS_NONRT_MAIN_THREAD_NON_RELEASE();

#if !defined(RELEASE)
#ifndef DISABLE_BDWGC
    // Check that element has no finalizer.
  GC_finalization_proc old = NULL;
  GC_register_finalizer(element, dummy_finalizer, NULL, &old, NULL);
  if (old!=NULL)
    abort();
#endif    
#endif
    
#ifdef DISABLE_BDWGC

	if(element==NULL){
		RError("Warning, attempting to free NULL\n");
		return;
	}

	OS_tfree(element);

#else

#  if !defined(VALIDATE_MEM)
	GC_free(element);
#  else
        V_free_it(GC_free, element);
#  endif

#endif

}



static void ShutDownYepp(void) {

	EndProgram();

	exit(1);
}

int64_t allocated=0;


#if defined(VALIDATE_MEM)

#  ifndef DISABLE_BDWGC
static void dummyfree(void *data){
}

static void gcfinalizer(void *actual_mem_start, void *user_data){

  ASSERT_IS_NONRT_MAIN_THREAD_NON_RELEASE()
  
  //printf("gcfinalizer called for %p\n",actual_mem_start);
  V_free_actual_mem_real_start(dummyfree, actual_mem_start);
}
#  endif

#endif

static void *tracker_alloc__(int size,void *(*AllocFunction)(size_t size2), const char *filename, int linenumber) __attribute__((malloc)) __attribute__((returns_nonnull));

static void *tracker_alloc__(int size,void *(*AllocFunction)(size_t size2), const char *filename, int linenumber){
	allocated+=size;

        if (THREADING_is_RT())
          RError("Calling GC-alloc while holding player lock. %s: %d", filename, linenumber);

        if (THREADING_is_main_thread()==false){
          RError("Calling GC-alloc from non-main_thread. %s: %d", filename, linenumber);
          return calloc(1, size); // thread-safe fallback.
        }
        
#ifndef DISABLE_BDWGC
#	ifdef _AMIGA
		return (*GC_amiga_allocwrapper_do)(size,AllocFunction);
#	else
#          if !defined(VALIDATE_MEM)
                void *ret = AllocFunction(size);
#          else                
		void *ret = V_alloc(AllocFunction,size,filename,linenumber);
                void *actual_mem_start = V_allocated_mem_real_start(ret);
#if defined(RELEASE)
                #error "oh no"
#endif
                GC_register_finalizer_ignore_self(actual_mem_start, gcfinalizer, NULL, NULL, NULL);
#          endif
#	endif


        return ret;

#else
	return OS_getmem(size);		// For debugging. (wrong use of GC_malloced memory could be very difficult to trace)
#endif



}

void *tralloc__(int size, const char *filename, int linenumber){
  return tracker_alloc__(size,GC_malloc, filename, linenumber);
}

void *tralloc_atomic__(int size, const char *filename, int linenumber){
  return tracker_alloc__(size,GC_malloc_atomic, filename, linenumber);
}


/************************************************************
   FUNCTION
     Does never return NULL.
     Clears memory.
************************************************************/
void *talloc__(int size, const char *filename, int linenumber){ ///, const char *filename, int linenumber){
	void *ret;

	ret=tracker_alloc__(size,GC_malloc, filename, linenumber);

	if(ret!=NULL) return ret;

        RWarning("Out of memory. Trying to get memory a different way. You should save and quit.");

        return calloc(1,size);
        /*
        ret=calloc(1,size);
	if(ret!=NULL) return ret;


	RWarning("\n\n\n Didn't succeed. OUT OF MEMORY. Have to quit\n\n\n");
	ShutDownYepp();
	return NULL;
        */
}

void *talloc_atomic__(int size, const char *filename, int linenumber){
	void *ret;

        //return GC_malloc_atomic(size);
        
	ret=tracker_alloc__(size,GC_malloc_atomic, filename, linenumber);

	if(ret!=NULL) goto gotit;

#if 0
	if(tmemoryisused==0){
		tmemoryisused=1;
		GC_free(tmemory);
		tmemory=NULL;
	}

#  ifndef DISABLE_BDWGC
	ret=GC_malloc_atomic(size);
#  else
	ret=OS_getmem(size);		// For debugging. (wrong use of GC_malloced memory could be very difficult to trace)
#  endif

#endif
        RWarning("Out of memory. I'll try to continue by allocating a different way, but you should save and quit now.\n");

        ret = calloc(1,size);
        
	if(ret!=NULL) goto gotit;


        RWarning("Out of memory. Quitting\n");
	ShutDownYepp();

 gotit:
        
#if !defined(RELEASE)
        
        // Fill with random data. Very often GC_malloc_atomic returns zeros, but not always.
        
        if (ret != NULL){
          unsigned char *chars = (unsigned char *)ret;
          for(int i=0;i<size;i++){
            chars[i] = (unsigned char)(rand() % 256);
          }
        }
#endif
        
	return ret;
}


void *talloc_atomic_uncollectable__(int size, const char *filename, int linenumber){
	void *ret;

	ret=tracker_alloc__(size,GC_malloc_atomic_uncollectable, filename, linenumber);

	if(ret!=NULL) return ret;

#if 0
	if(tmemoryisused==0){
		tmemoryisused=1;
		GC_free(tmemory);
		tmemory=NULL;
	}

#ifndef DISABLE_BDWGC
 	ret=GC_malloc_atomic_uncollectable(size);
#else
	ret=OS_getmem(size);		// For debugging. (wrong use of GC_malloced memory could be very difficult to trace)
#endif

#endif

        RWarning("Out of memory. I'll try to continue by allocating a different way, but you should save and quit now.\n");
        return calloc(1,size);

        /*
	if(ret!=NULL) return ret;


        RWarning("Didn't succeed. Out of memory. Quitting. (atomic_uncollectable allocator)\n");
	ShutDownYepp();
	return NULL;
        */
}

void *talloc_atomic_clean__(int size, const char *filename, int linenumber){
  void *ret = talloc_atomic__(size, filename, linenumber);
  memset(ret, 0, size);
  return ret;
}

void *talloc_realloc__(void *v, int new_size, const char *filename, int linenumber){
#ifdef DISABLE_BDWGC
  # if defined(VALIDATE_MEM)
    return V_realloc(v,new_size);
  # else
    return realloc(v,new_size);
  # endif
#else
  #if !defined(VALIDATE_MEM)
    return GC_realloc(v,new_size);
  #else
    {
      MemoryAllocator allocator = V_get_MemoryAllocator(v);
      int old_size = V_get_size(v);
      
      void *new_mem = tracker_alloc__(new_size, allocator, filename, linenumber);
      memcpy(new_mem, v, R_MIN((int)new_size, old_size));
      
      return new_mem;
    }
  #endif
#endif
}

char *talloc_strdup__(const char *input, const char *filename, int linenumber) {
  if(input==NULL) // when does this happen?
    return NULL;
  int len = strlen(input);
  char *ret = (char*)talloc_atomic__(len + 1, filename, linenumber);
  strcpy(ret, input);
  return ret;
}

wchar_t *talloc_wcsdup__(const wchar_t *input, const char *filename, int linenumber) {
  if(input==NULL) // when does this happen?
    return NULL;
  
  wchar_t *ret = (wchar_t*)talloc_atomic__(sizeof(wchar_t) * ((int)wcslen(input) + 1), filename, linenumber);
  
  return wcscpy(ret, input);
}

char *talloc_numberstring__(int number, const char *filename, int linenumber){
  char s[1000];
  sprintf(s,"%d",number);
  return talloc_strdup__(s, filename, linenumber);
}

char *talloc_floatstring__(float number, const char *filename, int linenumber){
  char s[1000];
  snprintf(s,999,"%f",number);
  return talloc_strdup__(s, filename, linenumber);
}


char *talloc_format_internal(const char *fmt,...){
  int size = 64;
  char *ret = (char*)talloc_atomic__(size,__FILE__,__LINE__);

  for(;;){
    va_list argp;

    va_start(argp,fmt);
    int len = vsnprintf(ret,size-2,fmt,argp);
    va_end(argp);

    if (len <= 0) {
      size = size * 2;
      ret = (char*)talloc_realloc__(ret, size,__FILE__,__LINE__);
    } else if (len >= size-3) {
      size = len + 16;
      ret = (char*)talloc_realloc__(ret, size,__FILE__,__LINE__);
    } else
      break;
  }

#if defined(VALIDATE_MEM)
  V_validate(ret);
#endif
  
  return ret;
}

wchar_t *talloc_wformat_internal(const wchar_t *fmt,...){
  int size = 64;
  wchar_t *ret = (wchar_t*)talloc_atomic__(size*sizeof(wchar_t),__FILE__,__LINE__);

  for(;;){
    va_list argp;

    va_start(argp,fmt);
    int len = vswprintf(ret,size-2,fmt,argp);
    va_end(argp);

    if (len <= 0) {
      size = size * 2;
      ret = (wchar_t*)talloc_realloc__(ret, size*sizeof(wchar_t),__FILE__,__LINE__);
    } else if (len >= size-3) {
      size = len + 16;
      ret = (wchar_t*)talloc_realloc__(ret, size*sizeof(wchar_t),__FILE__,__LINE__);
    } else
      break;
  }

#if defined(VALIDATE_MEM)
  V_validate(ret);
#endif
  
  return ret;
}

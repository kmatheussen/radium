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

#ifndef RADIUM_COMMON_MEMORY_PROC_H
#define RADIUM_COMMON_MEMORY_PROC_H


#include <stdlib.h>
#include <string.h>
#include <wchar.h>

#include "memory_talloc_wcsdup_proc.h"

extern LANGSPEC void Threadsafe_GC_disable(void);
extern LANGSPEC void Threadsafe_GC_enable(void);

extern LANGSPEC void init_memory(void);

extern LANGSPEC void tfree(void *element);

//extern LANGSPEC void *tracker_alloc_clean(int size,void *(*AllocFunction)(int size2));

//extern LANGSPEC void *tracker_alloc__(int size,void *(*AllocFunction)(size_t size2), const char *filename, int linenumber) __attribute__((malloc)) __attribute__((returns_nonnull));

extern LANGSPEC void *tralloc__(int size, const char *filename, int linenumber) __attribute__((malloc)) __attribute__((returns_nonnull));

extern LANGSPEC void *tralloc_atomic__(int size, const char *filename, int linenumber) __attribute__((malloc)) __attribute__((returns_nonnull));

extern LANGSPEC void *talloc__(int size, const char *filename, int linenumber) __attribute__((malloc)) __attribute__((returns_nonnull));

extern LANGSPEC void *talloc_atomic__(int size, const char *filename, int linenumber) __attribute__((malloc)) __attribute__((returns_nonnull));

extern LANGSPEC void *talloc_atomic_uncollectable__(int size, const char *filename, int linenumber) __attribute__((malloc)) __attribute__((returns_nonnull));

extern LANGSPEC void *talloc_realloc__(void *v, int new_size, const char *filename, int linenumber) __attribute__((malloc)) __attribute__((returns_nonnull));

extern LANGSPEC void *talloc_atomic_clean__(int size, const char *filename, int linenumber) __attribute__((malloc)) __attribute__((returns_nonnull));

extern LANGSPEC char *talloc_strdup__(const char *input, const char *filename, int linenumber) __attribute__((malloc)); // __attribute__((returns_nonnull));

extern LANGSPEC char *talloc_numberstring__(int number, const char *filename, int linenumber) __attribute__((malloc)) __attribute__((returns_nonnull));

extern LANGSPEC char *talloc_floatstring__(float number, const char *filename, int linenumber) __attribute__((malloc)) __attribute__((returns_nonnull));



#define tracker_alloc(a,b) tracker_alloc__(a,b,__FILE__,__LINE__) 

#define tralloc(a) tralloc__(a,__FILE__,__LINE__) 

#define tralloc_atomic(a) tralloc_atomic__(a,__FILE__,__LINE__)

#define talloc(a) talloc__(a,__FILE__,__LINE__)

#define talloc_atomic(a) talloc_atomic__(a,__FILE__,__LINE__)

#define talloc_atomic_uncollectable(a) talloc_atomic_uncollectable__(a,__FILE__,__LINE__)

#define talloc_realloc(a,b) talloc_realloc__(a,b,__FILE__,__LINE__)

#define talloc_atomic_clean(a) talloc_atomic_clean__(a,__FILE__,__LINE__)

#define talloc_strdup(a) talloc_strdup__(a,__FILE__,__LINE__)

#define talloc_numberstring(a) talloc_numberstring__(a,__FILE__,__LINE__)

#define talloc_floatstring(a) talloc_floatstring__(a,__FILE__,__LINE__)

#define tcopy2(mem, size) memcpy(talloc(size), (void*)mem, size)
#define tcopy2_atomic(mem, size) memcpy(talloc_atomic(size), (void*)mem, size)

#define tcopy(mem) ({ typeof(mem) memTempo = (mem) ; tcopy2(memTempo, sizeof(typeof(*(memTempo)))); })
#define tcopy_atomic(mem) ({ typeof(mem) memTempo = (mem) ; tcopy2_atomic(memTempo, sizeof(typeof(*(memTempo)))); })

extern LANGSPEC char *talloc_format_internal(const char *fmt,...) FORMAT_ATTRIBUTE(1,2) __attribute__((malloc)) __attribute__((returns_nonnull));

#define talloc_format(FMT, ...) ((void)donothing(0 && printf(FMT,  __VA_ARGS__)), talloc_format_internal(FMT,   __VA_ARGS__)) // Add a "printf" call to make the C compiler show warning/error if using wrong arguments for FMT. (the printf call will never be called, or even compiled into the program, but the error runs on it)

extern LANGSPEC wchar_t *talloc_wformat_internal(const wchar_t *fmt,...);

#define talloc_wformat(FMT, ...) ((void)donothing(0 && wprintf(FMT,  __VA_ARGS__)), talloc_wformat_internal(FMT,   __VA_ARGS__)) // Add a "printf" call to make the C compiler show warning/error if using wrong arguments for FMT. (the printf call will never be called, or even compiled into the program, but the error runs on it)


#endif

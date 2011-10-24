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

#ifndef TRACKER_INCLUDE

extern int init_memory(void);

extern void tfree(void *element);

extern LANGSPEC void *tracker_alloc_clean(size_t size,void *(*AllocFunction)(size_t size2));

extern LANGSPEC void *tracker_alloc(size_t size,void *(*AllocFunction)(size_t size2));

extern LANGSPEC void *tralloc(size_t size);

extern LANGSPEC void *tralloc_atomic(size_t size);

extern LANGSPEC void *talloc(size_t size);

extern LANGSPEC void *talloc_atomic(size_t size);

extern LANGSPEC void *talloc_atomic_uncollectable(size_t size);

#endif


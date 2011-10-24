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


#ifdef TRACKER_GB
#include <stdlib.h>
extern void *GC_malloc(size_t size);
extern void *GC_malloc_atomic(size_t size);
extern void GC_free(void *mem);
extern void GC_exclude_static_roots(void *a,void *b);
extern void GC_gcollect(void);
extern void GC_INIT(void);
#endif

#ifndef TRACKER_GB
#include <gc.h>
#endif


/* Copyright 2012 Kjetil S. Matheussen

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


#include <string.h>

#define GFX_DONTSHRINK

#include "nsmtracker.h"

#include "visual_op_queue_proc.h"

#include "visual_proc.h"
#include "gfx_point_proc.h"

#include "gfx_op_queue_proc.h"


enum{
#  define OP_ENUMS
#  include "gfx_op_queue_generated.c"
#  undef OP_ENUMS
};


typedef struct{
  int type;
  int i1;
  union{
    int i2;
    unsigned int u2;
    char s2[64];
  };
  int i3;
  int i4;
  union{
    int i5;
    bool b5;
  };
  int i6;
  union{
    int i7;
    APoint *p7;
  };
  int i8;
#if 0
  // this would be nice
#ifdef DEBUG
  char *line;
  char *file;
#endif
#endif
} queue_element_t;

typedef struct{
  queue_element_t *elements;
  int pos;
  int size;
} queue_t;

void GFX_play_op_queue(struct Tracker_Windows *window){
  queue_t *queue = window->op_queue;
  int i;

#if 0
  if(queue->pos>0)
    fprintf(stderr,"queue size: %d\n",queue->pos);
#endif

  for(i=0;i<queue->pos;i++){
    queue_element_t *el = &queue->elements[i];

    if(false // Ugly, but this is exactly what happened in Qt/Qt_visual.cpp, and there hasn't been problems with it. (now there are) (but the problem is not here, see gfx_point.c) Point drawing primitive seems to work perfectly, so there's probably something wrong with the tempocolor track drawing.
       || el->type==ENUM_GFX_C2V_bitBlt
       || el->type==ENUM_GFX_C_DrawCursor
       || el->type==ENUM_GFX_P2V_bitBlt 
       || el->type==ENUM_GFX_BitBlt
       )
      GFX_BouncePoints(window);

    switch(el->type){
#     define OP_CASES
#     include "gfx_op_queue_generated.c"
#     undef OP_CASES
    default:
      RError("Unknown type %d",el->type);
    }
  }

  queue->pos = 0;
}

void GFX_create_op_queue(struct Tracker_Windows *window){
  window->op_queue = talloc(sizeof(queue_t));
}

void GFX_clear_op_queue(struct Tracker_Windows *window){
  queue_t *queue = window->op_queue;
  queue->pos = 0;
}

int GFX_get_op_queue_size(struct Tracker_Windows *window){
  queue_t *queue = window->op_queue;
  return queue->pos;
}

#define START_QUEUE_SIZE (4096*8)
//#define START_QUEUE_SIZE 16 // For debugging

static void grow_queue(queue_t *queue){

  if(queue->size==0){

    queue->elements = talloc_atomic(sizeof(queue_element_t)*START_QUEUE_SIZE);
    memset(queue->elements, 0, sizeof(queue_element_t)*START_QUEUE_SIZE);  // needed for oversized strings to terminate
    queue->size = START_QUEUE_SIZE;

  } else {

    queue->elements = talloc_realloc(queue->elements,sizeof(queue_element_t)*queue->size*2);
    memset(queue->elements+queue->size, 0, sizeof(queue_element_t)*queue->size);  // needed for oversized strings to terminate
    queue->size *= 2;

  }
}

extern bool g_allowed_to_grow_queue;

static queue_element_t *get_next_element(queue_t *queue){
#if !defined(RELEASE)
  if (g_allowed_to_grow_queue==false)
    abort();
#endif
  
  if(queue->pos==queue->size)
    grow_queue(queue);
  return &queue->elements[queue->pos++];
}


#define OP_FUNCS
#include "gfx_op_queue_generated.c"
#undef OP_FUNCS



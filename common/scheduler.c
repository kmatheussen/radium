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

#include "nsmtracker.h"
#include "playerclass.h"

#include "scheduler_proc.h"


#define QUEUE_SIZE 2048
#define MAX_ARGS 4

extern PlayerClass *pc;


typedef struct _event_t{
  int64_t time;
  union SuperType args[MAX_ARGS];
  SchedulerCallback callback;
  struct _event_t *next;
} event_t;



static int g_current_time = 0;

static event_t g_event0 = {0};

static event_t *g_free_events = NULL;

static event_t *g_queue[QUEUE_SIZE];
static int g_queue_size = 0;



static event_t *get_free_event(void){
  event_t *ret = g_free_events;
  g_free_events = g_free_events->next;
  ret->next = NULL;
  return ret;
}

static event_t *get_first_event(void){
  return g_queue[1];
}

/*
static double scale(double x, double x1, double x2, double y1, double y2){
  return y1 + ( ((x-x1)*(y2-y1))
                /
                (x2-x1)
                );
}
*/

// SCHEDULER plugs into SEQ, and since they have different timer formats, we need to convert.
//
// The difference is that SCHEDULER doesn't reset timer when starting to play. The SCHEDULER also
// increases time even when not playing.
//
// Since we know the start and end time of the current block for both SCHEDULER and SEQ, the conversion is simple.

static int64_t scheduler_to_seq_time(int64_t scheduler_time){
  //int64_t block_length = pc->end_time - pc->start_time;
  //return scale(scheduler_time,g_current_time,g_current_time + block_length, pc->start_time, pc->end_time);
  //
  //... is the same as:
  return pc->start_time + (scheduler_time - g_current_time);
}

static int64_t seq_to_scheduler_time(int64_t seq_time){

  //int64_t block_length = pc->end_time - pc->start_time;
  //return scale(seq_time, pc->start_time, pc->end_time, g_current_time, g_current_time + block_length);
  //
  //... is the same as:
  return g_current_time + (seq_time - pc->start_time);
}

void SCHEDULER_add_event(int64_t seq_time, SchedulerCallback callback, const union SuperType *args, int num_args, enum SchedulerPriority priority){
  int64_t time = seq_to_scheduler_time(seq_time);

#if 0
  printf("|||||||||| Adding event at seq_time %d, scheduler_time %d. g_current_time: %d, pc->start_time: %d\n",
         (int)seq_time,(int)time,
         (int)g_current_time,(int)pc->start_time);
#endif

  if(g_queue_size > QUEUE_SIZE-2){
    printf("SCHEDULER: queue full. Skipping.\n");
    return;
  }
  if(num_args>MAX_ARGS){
    printf("Max 4 args allowed for scheduler...\n");
    return;
  }

  // Add priority bit.
  time = time * 2;
  time = time + priority;

  if(priority==SCHEDULER_ADD_BEFORE_SAME_TIME){
    //printf("stop sched.\n");
  }

  event_t *event = get_free_event();
  event->callback=callback;
  event->time = time;

  g_queue_size++;

  int i = g_queue_size;
  int new_i = i >> 1;

  while(g_queue[new_i]->time > time){
    g_queue[i] = g_queue[new_i];
    i = new_i;
    new_i = new_i >> 1;
  }
  g_queue[i] = event;


  int argnum;
  for(argnum=0;argnum<num_args;argnum++)
    event->args[argnum] = args[argnum];
}

static void remove_first_event(void){
  event_t *first = get_first_event();

  event_t *last = g_queue[g_queue_size];
  int64_t last_time = last->time;

  int i = 1;
  int child = 2;

  g_queue_size--;

  while((child <= g_queue_size)){
    if(child != g_queue_size
       && g_queue[child+1]->time < g_queue[child]->time)
      child++;

    if(last_time <= g_queue[child]->time)
      break;

    g_queue[i] = g_queue[child];
    i          = child;
    child      = child * 2;
  }
  g_queue[i] = last;

  g_queue[g_queue_size+1] = NULL; // for the gc

  memset(first,0,sizeof(event_t)); // for the gc
  first->next = g_free_events;
  g_free_events = first;
}

void SCHEDULER_called_per_block(int64_t reltime){
  int64_t end_time = g_current_time + reltime;

  while(g_queue_size>0){
    event_t *event = get_first_event();
    int64_t event_time = event->time/2;  // remove priority bit.
    if(event_time < end_time){
      event->callback(scheduler_to_seq_time(event_time), &event->args[0]);
      remove_first_event();
    }else
      break;
  }

  g_current_time = end_time;
}

// Must be called when deleting a patch or track.
void SCHEDULER_clear(void){
}

void SCHEDULER_init(void){
  int i;
  for(i=0;i<QUEUE_SIZE;i++){
    event_t *event = talloc(sizeof(event_t));
    event->next = g_free_events;
    g_free_events = event;
  }
  g_event0.time = -100000;
  g_queue[0] = &g_event0;
}

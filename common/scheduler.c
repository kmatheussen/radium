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
#include "threading.h"
#include "OS_Player_proc.h"

#include "scheduler_proc.h"


#define QUEUE_SIZE 2048
#define MAX_ARGS 7

extern PlayerClass *pc;


typedef struct _event_t{
  int64_t time;
  int64_t seq_time;
  union SuperType args[MAX_ARGS];
  SchedulerCallback callback;
  struct _event_t *next;
} event_t;



static int64_t g_current_time = 0;

static event_t g_event0 = {0};

static event_t g_all_events[QUEUE_SIZE] = {{0}}; // stored as a static variable so the gc can easily reach the data.

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


// SCHEDULER plugs into SEQ, and since they have different timer formats, we need to convert.
//
// The difference is that SCHEDULER doesn't reset timer when starting to play. The SCHEDULER also
// increases time even when not playing.
//
// Since we know the start and end time of the current block for both SCHEDULER and SEQ, the conversion is simple.

#if 0
static int64_t scheduler_to_seq_time(int64_t scheduler_time){
  //int64_t block_length = pc->end_time - pc->start_time;
  //return scale(scheduler_time,g_current_time,g_current_time + block_length, pc->start_time, pc->end_time);
  //
  //... is the same as:
  return ((int64_t)pc->start_time) + (scheduler_time - g_current_time);
}
#endif

// Warning: It's quite complicated to get the same value back as you started with when convert betwween seq time and scheduler time.
static int64_t seq_to_scheduler_time(int64_t seq_time){

#if 0
  int64_t block_length = pc->end_time - pc->start_time;
  return scale(seq_time,
               pc->start_time, pc->end_time,
               g_current_time, g_current_time + block_length
               );
#else
  //... is the same as:
  return g_current_time + (seq_time - ((int64_t)pc->start_time)); // (optimization)
#endif
}

static void schedule_event(event_t *event){
  int64_t time = event->time;
  
  g_queue_size++;

  int i = g_queue_size;
  int new_i = i >> 1;

  while(time <= g_queue[new_i]->time){ // '<=' (instead of '<') means that the event will be inserted after events with the same time.
    g_queue[i] = g_queue[new_i];
    i = new_i;
    new_i = new_i >> 1;
  }
  
  g_queue[i] = event;
}

void SCHEDULER_add_event(int64_t seq_time, SchedulerCallback callback, const union SuperType *args, int num_args, enum SchedulerPriority priority){
  R_ASSERT(PLAYER_current_thread_has_lock());
  
  // An event created by an RT_process function needs to run right away.
  // If not it won't be run until the next audio block since SCHEDULER_called_per_block
  // has already been called for this audio block.
  // (Q: why not do this for events genereated by the editor too?
  //  A: Because priority would be lost. Effects could run after notes, "note on" events could run before "note off" events, and so forth)
  if (false==pc->is_treating_editor_events && seq_time < pc->end_time) {
    callback(seq_time, args);
    return;
  }
  
  int64_t time = seq_to_scheduler_time(seq_time);
  
  //R_ASSERT(seq_time==scheduler_to_seq_time(time));
           
  if(time < 0){
#if !defined(RELEASE)
    fprintf(stderr,"time<0: %d. seq_time: %d\n",(int)time,(int)seq_time);
    abort();
#endif
    return;
  }
  
  //args=NULL; // test crashreporter
  
#if 0
  printf("|||||||||| Adding event at seq_time %d, scheduler_time %d. g_current_time: %d, pc->start_time: %d\n",
         (int)seq_time,(int)time,
         (int)g_current_time,(int)pc->start_time);
#endif

  if(g_queue_size > QUEUE_SIZE-2){
    printf("SCHEDULER: queue full. Skipping.\n"); // Can happen if playing very fast. Should perhaps use RT_message instead.
    return;
  }
  
#if !defined(RELEASE)
  if(num_args>MAX_ARGS){
    fprintf(stderr, "Max %d args allowed for scheduler...\n", MAX_ARGS);
    abort();
  }
#endif
  
  // Add priority bit.
  time = time << SCHEDULER_NUM_PRIORITY_BITS;
  time = time + priority;

  
  event_t *event = get_free_event();
  event->callback=callback;
  event->time = time;
  event->seq_time = seq_time;

  memcpy(event->args, args, sizeof(union SuperType)*num_args);

  schedule_event(event);
}

static void remove_first_event(void){
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
}

static void release_event(event_t *event){
  memset(event,0,sizeof(event_t)); // for the gc
  event->next = g_free_events;
  g_free_events = event;
}

int SCHEDULER_called_per_block(int64_t reltime){
  int64_t end_time = g_current_time + reltime;
  //printf("  called_per_block. end_time: %d. pc->start_time: %f\n",(int)end_time, pc->start_time);
  
  while(g_queue_size>0){
    event_t *event = get_first_event();
    int64_t event_time = event->time >> SCHEDULER_NUM_PRIORITY_BITS;  // remove priority bits.
    //printf("  SCHEDULER: sched: %d - seq: %d.  First event: %d. pc->start_time: %d, pc->end_time: %d\n",(int)end_time, (int)scheduler_to_seq_time(end_time), (int)scheduler_to_seq_time(event_time),(int)pc->start_time, (int)pc->end_time);
    if(event_time < end_time){
      remove_first_event();
      {
        event->callback(event->seq_time, &event->args[0]); // note that the callback can also schedule new events
      }
      release_event(event);
    }else
      break;
  }

  g_current_time = end_time;

  
  return g_queue_size;
}

// * Must be called when deleting a patch or track. (why?)
// * Can't there be hanging notes, or other undefined behaviors, when the event callback is not called?
// Returns true if everything was cleared.
bool SCHEDULER_clear(void){
  //printf("TODO: Implermnet SCHEDULER_clear\n");

  const int max_to_remove = 20; // 2048/20 = 102.4. 102.4 * 64 frames / (48000 frames / seconds) = 0.1365 seconds. I.e. we should never wait more than approx. 0.14 seconds for all events to be cleared.
  int num_removed = 0;
  
  while(g_queue_size>0 && num_removed < max_to_remove){
    event_t *event = get_first_event();
    remove_first_event();
    release_event(event);
    num_removed++;
  }

  if (g_queue_size > 0)
    return false;
  else
    return true;
}

int SCHEDULER_num_events(void){
  return g_queue_size;
}

void SCHEDULER_init(void){
  int i;
  for(i=0;i<QUEUE_SIZE;i++){
    event_t *event = &g_all_events[i]; //talloc(sizeof(event_t));
    event->next = g_free_events;
    g_free_events = event;
  }
  g_event0.time = -100000;
  g_queue[0] = &g_event0;
}

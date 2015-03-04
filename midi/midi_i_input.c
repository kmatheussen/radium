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






#include "nsmtracker.h"

#include "midi_i_plugin.h"
#include "midi_i_plugin_proc.h"
#include "../common/notes_proc.h"
#include "../common/blts_proc.h"
#include "../common/OS_Ptask2Mtask_proc.h"
#include "../common/player_proc.h"
#include "../common/patch_proc.h"
#include "../common/placement_proc.h"

#include "midi_i_input_proc.h"

extern struct Root *root;

static volatile int g_cc=0,g_data1,g_data2;

static struct Patch *g_through_patch = NULL;

// TODO: This isn't always working properly. Going to change rtmidi API.

extern const char *NotesTexts3[131];


#define PACK_MIDI_MSG(a,b,c) ( (a&0xf0)<<16 | b<<8 | c)

typedef struct _midi_event_t{
  struct _midi_event_t *next;
  struct WBlocks *wblock;
  struct WTracks *wtrack;
  STime blocktime;
  uint32_t msg;
} midi_event_t;

static midi_event_t *g_midi_events = NULL;
static midi_event_t *g_recorded_midi_events = NULL;
static midi_event_t *g_last_recorded_midi_event = NULL;

static midi_event_t *get_midi_event(void){
  
  if (g_midi_events==NULL) {
    
    midi_event_t *midi_events = calloc(1024, sizeof(midi_event_t));
    int i;
    for(i=1;i<1023;i++)
      midi_events[i].next = &midi_events[i+1];
    
    g_midi_events = &midi_events[1];
    
    return &midi_events[0];
    
  }else {

    midi_event_t *ret = g_midi_events;
    g_midi_events = ret->next;
    return ret;

  }
}

static void record_midi_event(int cc, int data1, int data2){
  
  midi_event_t *midi_event = get_midi_event();

  midi_event->next = NULL;
  
  midi_event->wblock    = root->song->tracker_windows->wblock;
  midi_event->wtrack    = midi_event->wblock->wtrack;
  midi_event->blocktime = pc->start_time - pc->seqtime;
  midi_event->msg       = PACK_MIDI_MSG(cc,data1,data2);

  if (g_recorded_midi_events==NULL)
    g_recorded_midi_events = midi_event;
  else
    g_last_recorded_midi_event->next = midi_event;

  g_last_recorded_midi_event = midi_event;
}

static Place time_to_place(const struct Blocks *block, STime time){
  int line1,line2;

  int line=1;
  while(block->times[line].time < time)
    line++;

  line2 = line;
  line1 = line-1;

  R_ASSERT(line2>0);
  R_ASSERT(line2<=block->num_lines);
  
  STime time1 = block->times[line1].time;
  STime time2 = block->times[line2].time;
  
  Place place;

  float place_f = scale(time,time1,time2,line1,line2); // todo: may be inaccurate

  Float2Placement(place_f, &place);

  Place lastplace;
  PlaceSetLastPos(block, &lastplace);
  
  if (PlaceGreaterOrEqual(&place, &lastplace))
    PlaceTilLimit(&place,&lastplace);

  Place *firstplace = PlaceGetFirstPos();
    
  if (PlaceLessThan(&place,firstplace))
    place = *firstplace;
  
  return place;
}

static midi_event_t *find_midievent_end_note(midi_event_t *midi_event, int notenum_to_find){
  while(midi_event!=NULL){
    if (midi_event->wblock!=NULL) {
      
      uint32_t msg = midi_event->msg;
      int cc = msg>>16;
      int notenum = (msg>>8)&0xff;
      int volume = msg&0xff;
      
      if (cc==0x80 || volume==0){
        if (notenum==notenum_to_find)
          return midi_event;
      }
    }
    
    midi_event = midi_event->next;
  }

  return NULL;
}

void MIDI_insert_recorded_midi_events(void){
  midi_event_t *midi_event = g_recorded_midi_events;

  while(midi_event != NULL){
    midi_event_t *next = midi_event->next;

    if (midi_event->wblock!=NULL) {
      
      const struct Blocks *block = midi_event->wblock->block;

      STime time = midi_event->blocktime;
      uint32_t msg = midi_event->msg;
      
      printf("%d: %x\n",(int)time,msg);
      
      int cc = msg>>16;
      int notenum = (msg>>8)&0xff;
      int volume = msg&0xff;
      
      // add note
      if (cc==0x90 && volume>0) {
        
        Place place = time_to_place(block,time);
        Place endplace;
        Place *endplace_p;
        
        midi_event_t *midi_event_endnote = find_midievent_end_note(next,notenum);
        if (midi_event_endnote!=NULL){
          midi_event_endnote->wblock = NULL; // only use it once
          endplace = time_to_place(block,midi_event_endnote->blocktime);
          endplace_p = &endplace;
        }else
          endplace_p = NULL;
        
        endplace.line++;
        InsertNote(midi_event->wblock,
                   midi_event->wtrack,
                   &place,
                   endplace_p,
                   notenum,
                   (float)volume * MAX_VELOCITY / 127.0f,
                   true
                 );
      }
      
    }

    
    // remove event
    midi_event->next = g_midi_events;
    g_midi_events = midi_event;

    
    // iterate next
    midi_event = next;
  }

  g_recorded_midi_events = NULL;
  g_last_recorded_midi_event = NULL;
}

void MIDI_InputMessageHasBeenReceived(int cc,int data1,int data2){
  //printf("got new message. on/off:%d. Message: %x,%x,%x\n",(int)root->editonoff,cc,data1,data2);
  //static int num=0;
  //num++;

  if(cc>=0xf0) // Too much drama
    return;

  bool is_playing = pc->isplaying;
  
  if(cc>=0x80 && cc<0xa0){
    if (is_playing && root->editonoff)
      record_midi_event(cc,data1,data2);
    
    struct Patch *patch = g_through_patch;
    if(patch!=NULL){
      //printf("%d: got note %s (0x%x 0x%x)\n",num-1,NotesTexts3[data1],cc,data2);
      if(data2>0 && cc>=0x90)
        PATCH_play_note(patch,data1,-1,scale(data2,0,127,0,1),0.5);
      else
        PATCH_stop_note(patch,data1,-1);
    }
  }

  if (is_playing)
    return;
  
  if(g_cc!=0) // Too quick.
    return;

  // should be a memory barrier here somewhere.

  if((cc&0xf0)==0x90 && data2!=0) {
    g_cc = cc;
    g_data1 = data1;
    g_data2 = data2;

    Ptask2Mtask();
  }
}

// This is safe. A patch is never deleted.
void MIDI_SetThroughPatch(struct Patch *patch){
  //printf("Sat new patch %p\n",patch);
  if(patch!=NULL)
    g_through_patch=patch;
}


void MIDI_HandleInputMessage(void){
  //int cc = g_cc;
  int data1 = g_data1;
  //int data2 = g_data2;

  // should be a memory barrier here somewhere.

  if(g_cc==0)
    return;

  g_cc = 0;

  if( ! root->editonoff)
    return;

  InsertNoteCurrPos(root->song->tracker_windows,data1,false);
  root->song->tracker_windows->must_redraw = true;
}

void MIDI_input_init(void){
  midi_event_t *midi_event = get_midi_event();
  midi_event->next = g_midi_events;
  g_midi_events = midi_event;
}

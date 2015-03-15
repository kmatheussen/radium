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
#include "midi_proc.h"

#include "../common/notes_proc.h"
#include "../common/blts_proc.h"
#include "../common/OS_Ptask2Mtask_proc.h"
#include "../common/player_proc.h"
#include "../common/patch_proc.h"
#include "../common/placement_proc.h"
#include "../common/time_proc.h"
#include "../common/hashmap_proc.h"
#include "../common/undo.h"
#include "../common/undo_notes_proc.h"
#include "../audio/Mixer_proc.h"
#include "../common/OS_Player_proc.h"

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

  if (root==NULL || root->song==NULL || root->song->tracker_windows==NULL || root->song->tracker_windows->wblock==NULL)
    return;

  midi_event_t *midi_event = get_midi_event();

  midi_event->next = NULL;
  
  midi_event->wblock    = root->song->tracker_windows->wblock;
  midi_event->wtrack    = midi_event->wblock->wtrack;
  midi_event->blocktime = MIXER_get_accurate_radium_time() - pc->seqtime;
  midi_event->msg       = PACK_MIDI_MSG(cc,data1,data2);

  midi_event->wtrack->track->is_recording = true;

  //printf("Rec %d: %x, %x, %x\n",(int)midi_event->blocktime,cc,data1,data2);

  if (g_recorded_midi_events==NULL)
    g_recorded_midi_events = midi_event;
  else
    g_last_recorded_midi_event->next = midi_event;

  g_last_recorded_midi_event = midi_event;
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

  if (midi_event==NULL)
    return;

  hash_t *track_set = HASH_create(8);
  
  Undo_Open();{
    
    while(midi_event != NULL){
      midi_event_t *next = midi_event->next;

      if (midi_event->wblock!=NULL) {

        struct Blocks *block = midi_event->wblock->block;
        struct Tracks *track = midi_event->wtrack->track;
        track->is_recording = false;
        
        char *key = talloc_format("%x",midi_event->wtrack);
        if (HASH_has_key(track_set, key)==false){

          Undo_Notes(root->song->tracker_windows,
                     block,
                     track,
                     midi_event->wblock->curr_realline
                     );
          HASH_put_int(track_set, key, 1);
        }
        

        STime time = midi_event->blocktime;
        uint32_t msg = midi_event->msg;
      
        printf("%d: %x\n",(int)time,msg);
      
        int cc = msg>>16;
        int notenum = (msg>>8)&0xff;
        int volume = msg&0xff;
      
        // add note
        if (cc==0x90 && volume>0) {
        
          Place place = STime2Place(block,time);
          Place endplace;
          Place *endplace_p;
        
          midi_event_t *midi_event_endnote = find_midievent_end_note(next,notenum);
          if (midi_event_endnote!=NULL){
            midi_event_endnote->wblock = NULL; // only use it once
            endplace = STime2Place(block,midi_event_endnote->blocktime);
            endplace_p = &endplace;
          }else
            endplace_p = NULL;
        
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
    
  }Undo_Close();

  
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
  }

  struct Patch *patch = g_through_patch;
  if(patch!=NULL){

    uint32_t msg = MIDI_msg_pack3(cc, data1, data2);
    int len = MIDI_msg_len(msg);
    
    if (len>=1 && len<=3) {

      uint8_t data[3] = {cc, data1, data2};

      PLAYER_lock(); {
        RT_MIDI_send_msg_to_patch(patch, data, len, -1); // This is scary. This is a third thread, and the code is not made with third threads in mind. (for instance, if RT_MIDI_send_msg_to_patch calls STime2Place at the same time as block->times is updated, we can get a crash. Besides, if it takes time to get the player lock, the midi input timing for the events can be screwed up.
      } PLAYER_unlock();
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

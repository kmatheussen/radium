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



#include "../weakjack/weak_libjack.h"

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
#include "../common/settings_proc.h"
#include "../audio/Mixer_proc.h"
#include "../common/OS_Player_proc.h"

#include "midi_i_input_proc.h"

static volatile uint32_t g_msg = 0;

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
    
    midi_event_t *midi_events = V_calloc(1024, sizeof(midi_event_t));
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

static void record_midi_event(uint32_t msg){

  if (root==NULL || root->song==NULL || root->song->tracker_windows==NULL || root->song->tracker_windows->wblock==NULL)
    return;

  midi_event_t *midi_event = get_midi_event();

  midi_event->next = NULL;
  
  midi_event->wblock    = root->song->tracker_windows->wblock;
  midi_event->wtrack    = midi_event->wblock->wtrack;
  midi_event->blocktime = MIXER_get_accurate_radium_time() - pc->seqtime;
  midi_event->msg       = msg;

  midi_event->wtrack->track->is_recording = true;

  //printf("Rec %d: %x, %x, %x\n",(int)midi_event->blocktime,cc,data1,data2);

  if (g_recorded_midi_events==NULL)
    g_recorded_midi_events = midi_event;
  else
    g_last_recorded_midi_event->next = midi_event;

  g_last_recorded_midi_event = midi_event;
}


static midi_event_t *find_midievent_end_note(midi_event_t *midi_event, int notenum_to_find, STime starttime_of_note){
  while(midi_event!=NULL){
    if (midi_event->wblock!=NULL) {
      
      uint32_t msg = midi_event->msg;
      int cc = msg>>16;
      int notenum = (msg>>8)&0xff;
      int volume = msg&0xff;
      
      if (cc==0x80 || volume==0){
        if (notenum==notenum_to_find && midi_event->blocktime > starttime_of_note)
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
            
        int cc = (msg>>16)&0xf0; // remove channel
        int notenum = (msg>>8)&0xff;
        int volume = msg&0xff;

        // add note
        if (cc==0x90 && volume>0) {
        
          Place place = STime2Place(block,time);
          Place endplace;
          Place *endplace_p;
        
          midi_event_t *midi_event_endnote = find_midievent_end_note(next,notenum,time);
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

typedef struct {
  int32_t deltatime;
  uint32_t msg;
} play_buffer_event_t;

static jack_ringbuffer_t *g_play_buffer;

static void add_event_to_play_buffer(int cc,int data1,int data2){
  play_buffer_event_t event;

  event.deltatime = 0;
  event.msg = PACK_MIDI_MSG(cc,data1,data2);

  jack_ringbuffer_write(g_play_buffer, (char*)&event, sizeof(play_buffer_event_t));
}

void RT_MIDI_handle_play_buffer(void){
  struct Patch *patch = g_through_patch;
  
  while (jack_ringbuffer_read_space(g_play_buffer) >= sizeof(play_buffer_event_t)) {
    play_buffer_event_t event;
    jack_ringbuffer_read(g_play_buffer, (char*)&event, sizeof(play_buffer_event_t));

      if(patch!=NULL){

        uint32_t msg = event.msg;
        
        uint8_t data[3] = {MIDI_msg_byte1(msg), MIDI_msg_byte2(msg), MIDI_msg_byte3(msg)};
          
        RT_MIDI_send_msg_to_patch(patch, data, 3, -1);
      }
  }
}


static bool g_record_accurately_while_playing = true;

bool MIDI_get_record_accurately(void){
  return g_record_accurately_while_playing;
}

void MIDI_set_record_accurately(bool accurately){
  SETTINGS_write_bool("record_midi_accurately", accurately);
  g_record_accurately_while_playing = accurately;
}

static bool g_record_velocity = true;

bool MIDI_get_record_velocity(void){
  return g_record_velocity;
}

void MIDI_set_record_velocity(bool doit){
  printf("doit: %d\n",doit);
  SETTINGS_write_bool("always_record_midi_velocity", doit);
  g_record_velocity = doit;
}

void MIDI_InputMessageHasBeenReceived(int cc,int data1,int data2){
  //printf("got new message. on/off:%d. Message: %x,%x,%x\n",(int)root->editonoff,cc,data1,data2);
  //static int num=0;
  //num++;

  if(cc>=0xf0) // Too much drama
    return;

  bool is_playing = pc->isplaying;

  uint32_t msg = MIDI_msg_pack3(cc, data1, data2);
  int len = MIDI_msg_len(msg);
  if (len<1 || len>3)
    return;
  
  if(g_through_patch!=NULL)
    add_event_to_play_buffer(cc, data1, data2);
  
  if (g_record_accurately_while_playing && is_playing) {
    
    if(cc>=0x80 && cc<0xa0)
      if (root->editonoff)
        record_midi_event(msg);

  } else {

    if (g_msg == 0) { // if g_msg!=0, we are playing too quickly;

      // should probably be a memory barrier here somewhere.
      
      if((cc&0xf0)==0x90 && data2!=0)
        g_msg = msg;
    }
  }
}

// This is safe. A patch is never deleted.
void MIDI_SetThroughPatch(struct Patch *patch){
  //printf("Sat new patch %p\n",patch);
  if(patch!=NULL)
    g_through_patch=patch;
}


void MIDI_HandleInputMessage(void){
  // should be a memory barrier here somewhere.

  uint32_t msg = g_msg;
  
  if (msg!=0) {

    g_msg = 0;

    if(root->editonoff) {
      float velocity = -1.0f;
      if (g_record_velocity)
        velocity = (float)MIDI_msg_byte3(msg) / 127.0;
      //printf("velocity: %f, byte3: %d\n",velocity,MIDI_msg_byte3(msg));
      InsertNoteCurrPos(root->song->tracker_windows,MIDI_msg_byte2(msg), false, velocity);
      root->song->tracker_windows->must_redraw = true;
    }
  }
}

void MIDI_input_init(void){
  MIDI_set_record_accurately(SETTINGS_read_bool("record_midi_accurately", true));
  MIDI_set_record_velocity(SETTINGS_read_bool("always_record_midi_velocity", false));
  
  midi_event_t *midi_event = get_midi_event();
  
  midi_event->next = g_midi_events;
  
  g_midi_events = midi_event;
  
  g_play_buffer = jack_ringbuffer_create(8000*sizeof(play_buffer_event_t));
}

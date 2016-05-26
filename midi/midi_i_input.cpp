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


#include <unistd.h>


#include <boost/version.hpp>
#if (BOOST_VERSION < 100000) || ((BOOST_VERSION / 100 % 1000) < 58)
  #error "Boost too old. Need at least 1.58.\n Quick fix: cd $HOME ; wget http://downloads.sourceforge.net/project/boost/boost/1.60.0/boost_1_60_0.tar.bz2 ; tar xvjf boost_1_60_0.tar.bz2 (that's it!)"
#endif
#include <boost/lockfree/queue.hpp>



#include "nsmtracker.h"

#include "midi_i_plugin.h"
#include "midi_i_plugin_proc.h"
#include "midi_proc.h"

#include "../common/list_proc.h"
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
#include "../common/visual_proc.h"
#include "../common/Mutex.hpp"
#include "../common/Queue.hpp"
#include "../common/Vector.hpp"

#include "midi_i_input_proc.h"

static DEFINE_ATOMIC(uint32_t, g_msg) = 0;

static DEFINE_ATOMIC(struct Patch *, g_through_patch) = NULL;

extern const char *NotesTexts3[131];




/*********************************************************
 *********************************************************
 **          Configuration                              **
 *********************************************************
 *********************************************************/

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





/*********************************************************
 *********************************************************
 **          Record MIDI                                **
 *********************************************************
 *********************************************************/


#define PACK_MIDI_MSG(a,b,c) ( (a&0xf0)<<16 | b<<8 | c)

typedef struct _midi_event_t{
  time_position_t timepos;
  uint32_t msg;
} midi_event_t;


static radium::Mutex g_midi_event_mutex;
static radium::Vector<midi_event_t> g_recorded_midi_events;

static radium::Queue<midi_event_t, 8000> g_midi_event_queue;

  
// Called from a MIDI input thread. Only called if playing
static void record_midi_event(uint32_t msg){
  midi_event_t midi_event;

  if (MIXER_fill_in_time_position(&midi_event.timepos) == true){ // <-- MIXER_fill_in_time_position returns false if the event was recorded after song end, or program is initializing, or there was an error.
    
    midi_event.msg       = msg;

    if (!g_midi_event_queue.tryPut(midi_event))
      RT_message("Midi recording buffer full.\nUnless your computer was almost halting because of high CPU usage, please report this incident.");

  }
}

// Runs in its own thread
static void *recording_queue_pull_thread(void*){
  while(true){
    
    midi_event_t event = g_midi_event_queue.get();
    
    {
      radium::ScopedMutex lock(&g_midi_event_mutex);

      if (ATOMIC_GET(root->song_state_is_locked) == true){ // not totally tsan proof
        struct Blocks *block = (struct Blocks*)ListFindElement1_r0(&root->song->blocks->l, event.timepos.blocknum);
        if (block != NULL){          
          struct Tracks *track = (struct Tracks*)ListFindElement1_r0(&block->tracks->l, event.timepos.tracknum);
          if (track != NULL){
            if (ATOMIC_GET(track->is_recording) == false){
              GFX_ScheduleEditorRedraw();
              ATOMIC_SET(track->is_recording, true);
            }
          }
        }
      }

      g_recorded_midi_events.add(event);
    }
    
  }

  return NULL;
}


static bool find_and_remove_midievent_end_note(int blocknum, int pos, int notenum_to_find, STime starttime_of_note, midi_event_t &midi_event){
  for(int i = pos ; i < g_recorded_midi_events.size(); i++) {

    midi_event = g_recorded_midi_events[i];
    
    if (midi_event.timepos.blocknum != blocknum)
      return false;

    uint32_t msg     = midi_event.msg;
    int      cc      = MIDI_msg_byte1(msg);
    int      notenum = MIDI_msg_byte2(msg);
    int      volume  = MIDI_msg_byte3(msg);
    
    if (cc==0x80 || volume==0){
      if (notenum==notenum_to_find && midi_event.timepos.blocktime > starttime_of_note) {
        g_recorded_midi_events.remove_pos(i, true);
        return true;
      }
    }
  }

  return false;
}


// Called from the main thread after the player has stopped
void MIDI_insert_recorded_midi_events(void){
  while(g_midi_event_queue.size() > 0) // Wait til the recording_queue_pull_thread is finished draining the queue.
    usleep(1000*5);

  ATOMIC_SET(root->song_state_is_locked, false);

  printf("MIDI_insert_recorded_midi_events called %d\n", g_recorded_midi_events.size());
         
  {
    radium::ScopedMutex lock(&g_midi_event_mutex);
    if (g_recorded_midi_events.size() == 0)
      return;
  }

  
  usleep(1000*20); // Wait a little bit more for the last event to be transfered into g_recorded_midi_events. (no big deal if we lose it though, CPU is probably so buzy if that happens that the user should expect not everything working as it should. It's also only in theory that we could lose the last event since the transfer only takes some nanoseconds, while here we wait 20 milliseconds.)


  {
    radium::ScopedMutex lock(&g_midi_event_mutex);
    radium::ScopedUndo scoped_undo;
    
    hash_t *track_set = HASH_create(8);

    for(int i = 0 ; i < g_recorded_midi_events.size(); i++) { // events can be removed from g_recorded_midi_events inside this loop
      
      auto midi_event = g_recorded_midi_events[i];

      //printf("%d / %d: %x\n",midi_event.timepos.blocknum, midi_event.timepos.tracknum, midi_event.msg);
        
      if (midi_event.timepos.tracknum >= 0) {
          
        struct WBlocks *wblock = (struct WBlocks*)ListFindElement1(&root->song->tracker_windows->wblocks->l, midi_event.timepos.blocknum);
        R_ASSERT_RETURN_IF_FALSE(wblock!=NULL);
          
        struct WTracks *wtrack = (struct WTracks*)ListFindElement1(&wblock->wtracks->l, midi_event.timepos.tracknum);
        R_ASSERT_RETURN_IF_FALSE(wtrack!=NULL);
                    
        struct Blocks *block = wblock->block;
        struct Tracks *track = wtrack->track;

        // GFX
        
        ATOMIC_SET(track->is_recording, false);


        // UNDO
        
        char *key = (char*)talloc_format("%x",track);
        if (HASH_has_key(track_set, key)==false){

          ADD_UNDO(Notes(root->song->tracker_windows,
                         block,
                         track,
                         wblock->curr_realline
                         ));
          HASH_put_int(track_set, key, 1);
        }


        // ADD NOTE / STOP

        STime time = midi_event.timepos.blocktime;
        uint32_t msg = midi_event.msg;
        
        Place place = STime2Place(block,time);
        
        int cc      = MIDI_msg_byte1_remove_channel(msg);
        int notenum = MIDI_msg_byte2(msg);
        int volume  = MIDI_msg_byte3(msg);
        
        // add note
        if (cc==0x90 && volume>0) {
          
          Place endplace;
          Place *endplace_p = NULL;
          
          midi_event_t midi_event_endnote;
          if (find_and_remove_midievent_end_note(midi_event.timepos.blocknum, i, notenum, time, midi_event_endnote) == true){
            endplace = STime2Place(block,midi_event_endnote.timepos.blocktime);
            endplace_p = &endplace;
          }
          
          InsertNote(wblock,
                     wtrack,
                     &place,
                     endplace_p,
                     notenum,
                     (float)volume * MAX_VELOCITY / 127.0f,
                     true
                     );
          }
        
        // add stp. (happens if note was started in block a, and stopped in block b)
        if (cc==0x80 || (cc==0x90 && volume==0)){
          
          struct Stops *stop=(struct Stops*)talloc(sizeof(struct Stops));
          PlaceCopy(&stop->l.p,&place);
          
          ListAddElement3(&track->stops,&stop->l);
        }
      }
      
    }

    g_recorded_midi_events.clear();

  } // end mutex and undo scope

}


/*********************************************************
 *********************************************************
 **          Send MIDI input to current patch           **
 *********************************************************
 *********************************************************/

typedef struct {
  int32_t deltatime;
  uint32_t msg;
} play_buffer_event_t;

static boost::lockfree::queue<play_buffer_event_t, boost::lockfree::capacity<8000> > g_play_buffer;

// Called from a MIDI input thread
static void add_event_to_play_buffer(int cc,int data1,int data2){
  play_buffer_event_t event;

  event.deltatime = 0;
  event.msg = PACK_MIDI_MSG(cc,data1,data2);

  if (!g_play_buffer.bounded_push(event))
    RT_message("Midi play buffer full.\nMost likely the player can not keep up because it uses too much CPU.\nIf that is not the case, please report this incident.");
}

// Called from the player thread
void RT_MIDI_handle_play_buffer(void){
  struct Patch *patch = ATOMIC_GET(g_through_patch);

  play_buffer_event_t event;

  while(g_play_buffer.pop(event)==true){

    if(patch!=NULL){
      
      uint32_t msg = event.msg;
      
      uint8_t data[3] = {(uint8_t)MIDI_msg_byte1(msg), (uint8_t)MIDI_msg_byte2(msg), (uint8_t)MIDI_msg_byte3(msg)};
      
      RT_MIDI_send_msg_to_patch((struct Patch*)patch, data, 3, -1);
    }
    
  }
}


// This is safe. A patch is never deleted.
void MIDI_SetThroughPatch(struct Patch *patch){
  //printf("Sat new patch %p\n",patch);
  if(patch!=NULL)
    ATOMIC_SET(g_through_patch, patch);
}





/*********************************************************
 *********************************************************
 **       Got MIDI from the outside. Entry point.       **
 *********************************************************
 *********************************************************/


void MIDI_InputMessageHasBeenReceived(int cc,int data1,int data2){
  //printf("got new message. on/off:%d. Message: %x,%x,%x\n",(int)root->editonoff,cc,data1,data2);
  //static int num=0;
  //num++;

  if(cc==0xf0 || cc==0xf7) // sysex not supported
    return;

  bool isplaying = is_playing();

  uint32_t msg = MIDI_msg_pack3(cc, data1, data2);
  int len = MIDI_msg_len(msg);
  if (len<1 || len>3)
    return;
  
  if(ATOMIC_GET(g_through_patch)!=NULL)
    add_event_to_play_buffer(cc, data1, data2);
  
  if (g_record_accurately_while_playing && isplaying) {
    
    if(cc>=0x80 && cc<0xa0)
      if (ATOMIC_GET(root->editonoff))
        record_midi_event(msg);

  } else {

    if((cc&0xf0)==0x90 && data2!=0)
      if (ATOMIC_COMPARE_AND_SET_UINT32(g_msg, 0, msg)==false) {
        // printf("Playing to fast. Skipping note %u from MIDI input.\n",msg); // don't want to print in realtime thread
      }
  }
}



/*************************************************************
**************************************************************
 **  Insert MIDI received from the outside into the editor  **
 *************************************************************
 *************************************************************/


// called very often
void MIDI_HandleInputMessage(void){
  // should be a memory barrier here somewhere.

  uint32_t msg = ATOMIC_GET(g_msg); // Hmm, should have an ATOMIC_COMPAREFALSE_AND_SET function. (doesn't matter though, it would just look better)
  
  if (msg!=0) {

    ATOMIC_SET(g_msg, 0);

    if(ATOMIC_GET(root->editonoff)){
      float velocity = -1.0f;
      if (g_record_velocity)
        velocity = (float)MIDI_msg_byte3(msg) / 127.0;
      //printf("velocity: %f, byte3: %d\n",velocity,MIDI_msg_byte3(msg));
      InsertNoteCurrPos(root->song->tracker_windows,MIDI_msg_byte2(msg), false, velocity);
      root->song->tracker_windows->must_redraw = true;
    }
  }
}



/*************************************************************
**************************************************************
 **                     Initialization                      **
 *************************************************************
 *************************************************************/

void MIDI_input_init(void){
  radium::ScopedMutex lock(&g_midi_event_mutex);
    
  MIDI_set_record_accurately(SETTINGS_read_bool("record_midi_accurately", true));
  MIDI_set_record_velocity(SETTINGS_read_bool("always_record_midi_velocity", false));
  
  static pthread_t recording_pull_thread;
  pthread_create(&recording_pull_thread, NULL, recording_queue_pull_thread, NULL);
}

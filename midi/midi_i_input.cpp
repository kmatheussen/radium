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
  #error "Boost too old. Need at least 1.58.\n Quick fix: cd $HOME ; wget http://downloads.sourceforge.net/project/boost/boost/1.63.0/boost_1_63_0.tar.bz2 ; tar xvjf boost_1_63_0.tar.bz2 (that's it!)"
#endif
#include <boost/lockfree/queue.hpp>

#include <QThread>
#include <QSet>


#include "../api/api_proc.h"

#include "nsmtracker.h"

#include "../common/list_proc.h"
#include "../common/hashmap_proc.h"
#include "../common/notes_proc.h"
#include "../common/blts_proc.h"
#include "../common/OS_Ptask2Mtask_proc.h"
#include "../common/player_proc.h"
#include "../common/patch_proc.h"
#include "../common/tracks_proc.h"
#include "../common/instruments_proc.h"
#include "../common/placement_proc.h"
#include "../common/time_proc.h"
#include "../common/hashmap_proc.h"
#include "../common/undo.h"
#include "../common/undo_notes_proc.h"
#include "../common/undo_fxs_proc.h"
#include "../common/settings_proc.h"
#include "../common/OS_Player_proc.h"
#include "../common/visual_proc.h"
#include "../common/Mutex.hpp"
#include "../common/QueueStack.hpp"
#include "../common/Vector.hpp"
#include "../common/clipboard_range_copy_proc.h"

#include "../crashreporter/crashreporter_proc.h"

#include "../audio/Mixer_proc.h"
#include "../audio/SoundPlugin.h"
#include "../audio/SoundPlugin_proc.h"

#include "midi_instrument.h"
#include "midi_instrument_proc.h"
#include "midi_proc.h"

#include "midi_i_input_proc.h"


static DEFINE_ATOMIC(uint32_t, g_msg) = 0;

DEFINE_ATOMIC(struct Patch *, g_through_patch) = NULL;

extern const char *NotesTexts3[131];

static radium::Mutex g_midi_learns_mutex;
static radium::Vector<radium::MidiLearn*> g_midi_learns;

static bool msg_is_fx(const uint32_t msg){
  int cc0 = MIDI_msg_byte1_remove_channel(msg);
  if (cc0==0xb0 || cc0==0xe0)
    return true;
  else
    return false;
}

static bool msg_is_note(const uint32_t msg){
  int cc0 = MIDI_msg_byte1_remove_channel(msg);
  if (cc0 == 0x80 || cc0==0x90)
    return true;
  else
    return false;
}

static bool msg_is_note_on(const uint32_t msg){
  int cc0 = MIDI_msg_byte1_remove_channel(msg);
  int data3 = MIDI_msg_byte3(msg);
  if (cc0 == 0x90 && data3>0)
    return true;
  else
    return false;
}

static bool msg_is_note_off(const uint32_t msg){
  int      cc0     = MIDI_msg_byte1_remove_channel(msg);
  int      volume  = MIDI_msg_byte3(msg);

  if (cc0==0x80 || (cc0=0x90 && volume==0))
    return true;
  else
    return false;
}

static float get_msg_fx_value(uint32_t msg){
  int d1 = MIDI_msg_byte1(msg);
  int d2 = MIDI_msg_byte2(msg);
  int d3 = MIDI_msg_byte3(msg);

  if (d1 < 0xc0) {

    // cc
    
    return d3 / 127.0;
      
    
  } else {

    // pitch bend

      int val = d3<<7 | d2;
      //printf("     d2: %x, d3: %x, %x\n", d2,d3, d3<<7 | d2);
      
      if (val < 0x2000)
        return scale(val,
                     0, 0x2000,
                     0, 0.5
                     );
      else
        return scale(val,
                     0x2000,   0x3fff,
                     0.5,      1.0
                     );
  }
}



/*********************************************************
 *********************************************************
 **          Configuration                              **
 *********************************************************
 *********************************************************/

static bool g_record_accurately_while_playing = false;

bool MIDI_get_record_accurately(void){
  return g_record_accurately_while_playing;
}

void MIDI_set_record_accurately(bool accurately){
  {
    // Very interesting! We get an error about not being able to acquire realtime priority with the line below in Macos. I guess it's a problem during startup only. Uncommented for now then, as it's not very important.
    //radium::PlayerLock lock;
    
    g_record_accurately_while_playing = accurately;
  }
  SETTINGS_write_bool("record_midi_accurately", accurately);
}

static bool g_record_velocity = true;

bool MIDI_get_record_velocity(void){
  return g_record_velocity;
}

void MIDI_set_record_velocity(bool doit){
  {
    // Very interesting! We get an error about not being able to acquire realtime priority with the line below in Macos.
    //radium::PlayerLock lock;
    
    g_record_velocity = doit;
  }
  printf("doit: %d\n",doit);
  SETTINGS_write_bool("always_record_midi_velocity", doit);
}





/*********************************************************
 *********************************************************
 **          Record MIDI                                **
 *********************************************************
 *********************************************************/


typedef struct _midi_event_t{
  time_position_t timepos;
  uint32_t msg;
  SoundPlugin *plugin;
  int effect_num;
} midi_event_t;


static radium::Mutex g_midi_event_mutex;
static radium::Vector<midi_event_t> g_recorded_midi_events;

static radium::Queue<midi_event_t, 8000> *g_midi_event_queue; // 8000 is not the maximum size that can be recorded totally, but the maximum size that can be recorded until the recording queue pull thread has a chance to pick it up. In other words: 8000 should be plenty enough as long as the CPU is not too busy.

  
// Called from a MIDI input thread. Only called if playing
static void record_midi_event(const symbol_t *port_name, const uint32_t msg){

  bool is_fx = msg_is_fx(msg);
  bool is_note = msg_is_note(msg);

#if defined(RELEASE)
  if (!is_fx && !is_note)
    return;
#endif
  
  time_position_t timepos;
  
  if (MIXER_fill_in_time_position(&timepos) == false)
    return; // <-- MIXER_fill_in_time_position returns false if the event was recorded after song end, or program is initializing, or there was an error.
      

  if (is_fx){
    
    radium::ScopedMutex lock(g_midi_learns_mutex); // <- Fix. Timing can be slightly inaccurate if adding/removing midi learn while recording, since MIDI_add/remove_midi_learn obtains the player lock while holding the g_midi_learns_mutex.
    
    for (auto midi_learn : g_midi_learns) {
      
      if (midi_learn->RT_matching(port_name, msg)){
        
        midi_event_t midi_event;
        midi_event.timepos = timepos;
        midi_event.msg = msg;
        
        if (midi_learn->RT_get_automation_recording_data(&midi_event.plugin, &midi_event.effect_num)){
          
          // Send event to the pull thread
          if (!g_midi_event_queue->tryPut(midi_event))
            RT_message("Midi recording buffer full while recording FX.\nUnless your computer was almost halting because of high CPU usage, "
                       "or your MIDI input and output ports are connected recursively, please report this incident.");
        }
        
      }
    }
    
  } else if (is_note){
    
    midi_event_t midi_event;
    midi_event.timepos = timepos;
    midi_event.msg       = msg;
    
    // Send event to the pull thread
    if (!g_midi_event_queue->tryPut(midi_event))
      RT_message("Midi recording buffer full while recording Note.\nUnless your computer was almost halting because of high CPU usage, "
                 "or your MIDI input and output ports are connected recursively, please report this incident.");
  }
}

// A minor hack. The sliders just add pitch wheel midi events to the g_midi_event_queue queue when dragging a recording slider.
void MIDI_add_automation_recording_event(SoundPlugin *plugin, int effect_num, float value){
  midi_event_t midi_event;

  if (is_playing()==false)
    return;

  if (value < 0){
    printf("   Warning: value < 0: %f\n",value);
    value = 0;
  }else if (value > 1.0){
    printf("   Warning: value > 1: %f\n",value);
    value = 1;
  }
  
  if (MIXER_fill_in_time_position(&midi_event.timepos) == true){ // <-- MIXER_fill_in_time_position returns false if the event was recorded after song end, or program is initializing, or there was an error.
    int val;

    if (value < 0.5)
      val = scale(value, 0, 0.5, 0, 0x2000);
    else
      val = scale(value, 0.0, 1.0, 0x2000, 0x3fff);

    // not sure if this is necessary.
    if (val < 0)
      val = 0;
    else if (val > 0x3fff)
      val = 0x3fff;

    midi_event.msg = (0xe0 << 16) | ((val&127) << 8) | (val >> 7);

    midi_event.plugin = plugin;
    midi_event.effect_num = effect_num;
    
    if (!g_midi_event_queue->tryPut(midi_event))
      RT_message("Midi recording buffer full.\nUnless your computer was almost halting because of high CPU usage, "
                 "or your MIDI input and output ports are connected recursively, please report this incident.");
  }
}

namespace{
struct RecordingQueuePullThread : public QThread {

  RecordingQueuePullThread(const RecordingQueuePullThread&) = delete;
  RecordingQueuePullThread& operator=(const RecordingQueuePullThread&) = delete;

  DEFINE_ATOMIC(bool, is_running) = true;
  
public:

  RecordingQueuePullThread(){
  }
  
  ~RecordingQueuePullThread(){
    if(g_midi_event_queue==NULL)
      return;
    
    ATOMIC_SET(is_running, false);
    midi_event_t event = {};
    g_midi_event_queue->put(event);
    wait(5000);
  }
  
  void run() override {

    while(true){
    
      midi_event_t event = g_midi_event_queue->get();
      if(ATOMIC_GET(is_running)==false)
        return;
            
      {
        radium::ScopedMutex lock(g_midi_event_mutex);
        
        // Schedule "Seq" painting
        if (ATOMIC_GET(root->song_state_is_locked) == true){ // not totally tsan proof, but we use the _r0 versions of ListFindElement below, so it's pretty safe.
          struct Blocks *block = (struct Blocks*)ListFindElement1_r0(&root->song->blocks->l, event.timepos.blocknum);
          if (block != NULL){          
            struct Tracks *track = (struct Tracks*)ListFindElement1_r0(&block->tracks->l, event.timepos.tracknum);
            if (track != NULL){
              if (ATOMIC_GET(track->is_recording) == false){
                GFX_ScheduleEditorRedrawIfCurrentBlockIsVisible();
                ATOMIC_SET(track->is_recording, true);
              }
            }
          }
        }

        // Send event to the main thread
        g_recorded_midi_events.push_back(event);
      }
    
    }
  }

  //  return NULL;
};

}

static RecordingQueuePullThread g_recording_queue_pull_thread; // Must be placed below the definition of 'g_midi_learns' since the destructor of 'g_recording_queue_pull_thread' must be called before the destructor of 'g_midi_learns'.



static midi_event_t *find_midievent_end_note(std::vector<midi_event_t> &midi_events, int blocknum, int pos, int notenum_to_find, STime starttime_of_note){

  for(midi_event_t &midi_event : midi_events) {

    if (midi_event.timepos.blocknum != blocknum)
      return NULL;

    uint32_t msg     = midi_event.msg;
    int      notenum = MIDI_msg_byte2(msg);
    
    if (msg_is_note_off(msg)){
      if (notenum==notenum_to_find && midi_event.timepos.blocktime > starttime_of_note)
        return &midi_event;
    }
  }

  return NULL;
}


// Happens if note was started in block a, and stopped in block b.
static void add_recorded_stp(struct Blocks *block, struct Tracks *track, const STime time){
        
  Place place = STime2Place(block,time);
        
  struct Stops *stop=(struct Stops*)talloc(sizeof(struct Stops));
  PlaceCopy(&stop->l.p,&place);
          
  ListAddElement3_ns(&track->stops,&stop->l);
}


static void add_recorded_note(std::vector<midi_event_t> &midi_events, struct WBlocks *wblock, struct Blocks *block, struct WTracks *wtrack, const int recorded_midi_events_pos, const STime time, const uint32_t msg, bool is_gfx){
        
  Place place = STime2Place(block,time);
  int notenum = MIDI_msg_byte2(msg);
  int volume  = MIDI_msg_byte3(msg);
        
  Place endplace;
  Place *endplace_p = NULL; // if NULL, the note doesn't stop in this block.
          
  midi_event_t *midi_event_endnote = find_midievent_end_note(midi_events, block->l.num, recorded_midi_events_pos+1, notenum, time);
  if (midi_event_endnote != NULL) {
    endplace = STime2Place(block,midi_event_endnote->timepos.blocktime);
    endplace_p = &endplace;
    if(!is_gfx)
      midi_event_endnote->msg = 0; // don't use this event again later.
  }

  if (is_gfx)
    InsertGfxNote(wblock,
                  wtrack,
                  &place,
                  endplace_p,
                  notenum,
                  (float)volume * MAX_VELOCITY / 127.0f
                  );
  else
    InsertNote(wblock,
               wtrack,
               &place,
               endplace_p,
               notenum,
               (float)volume * MAX_VELOCITY / 127.0f,
               true
               );
}


static void add_recorded_fx(std::vector<midi_event_t> &midi_events, struct Tracker_Windows *window, struct WBlocks *wblock, struct Blocks *block, struct WTracks *wtrack, const int midi_events_pos, const midi_event_t &first_event){

  printf("Add recorded fx %s / %d. %x\n",first_event.plugin->patch->name, first_event.effect_num, first_event.msg);

  int blocknum = wblock->l.num;
  int tracknum = wtrack->l.num;

  struct Tracks *track = wtrack->track;
  const struct Patch *track_patch = track->patch;

  SoundPlugin *plugin = first_event.plugin;
  const int effect_num = first_event.effect_num;
  struct Patch *patch = (struct Patch*)plugin->patch;

  if (patch==NULL)
    return;

  if (track_patch==NULL) {
    setInstrumentForTrack(patch->id, tracknum, blocknum, -1);
    track_patch = patch;
  }
  
  if (track_patch->instrument != get_audio_instrument())
    return;

  Place place = STime2Place(block,first_event.timepos.blocktime);
  float value = get_msg_fx_value(first_event.msg);
  const char *effect_name = PLUGIN_get_effect_name(plugin, effect_num);

  bool next_node_must_be_set = false;
  
  int fxnum = getFx(effect_name, tracknum, patch->id, blocknum, -1);
  if (fxnum==-1)
    return;

  ADD_UNDO(FXs(window, block, track, wblock->curr_realline));

  Undo_start_ignoring_undo_operations();
  
  if (fxnum==-2){
    fxnum = addFx(value, place, effect_name, tracknum, patch->id, blocknum, -1);
    setFxnodeLogtype(LOGTYPE_HOLD, 0, fxnum, tracknum, blocknum, -1);
    next_node_must_be_set = true;
  }else{
    int nodenum = addFxnode(value, place, fxnum, tracknum, blocknum, -1);
    setFxnodeLogtype(LOGTYPE_HOLD, nodenum, fxnum, tracknum, blocknum, -1);
  }
  
  for(int i = midi_events_pos+1 ; i < (int)midi_events.size(); i++) {
    
    midi_event_t &midi_event = midi_events.at(i);

    if (midi_event.timepos.blocknum != blocknum)
      break;
    
    if (midi_event.timepos.tracknum != tracknum)
      continue;

    if (!msg_is_fx(midi_event.msg))
      continue;
    
    if (midi_event.plugin != plugin)
      continue;

    if (midi_event.effect_num != effect_num)
      continue;

    Place place = STime2Place(block,midi_event.timepos.blocktime);
    float value = get_msg_fx_value(midi_event.msg);

    int nodenum;

    if (next_node_must_be_set){
      nodenum = 1;
      setFxnode(nodenum, value, place, fxnum, tracknum, blocknum, -1);
      next_node_must_be_set = false;
    } else {
      nodenum = addFxnode(value, place, fxnum, tracknum, blocknum, -1);
    }

    if (nodenum != -1)
      setFxnodeLogtype(LOGTYPE_HOLD, nodenum, fxnum, tracknum, blocknum, -1);
          
    midi_event.msg = 0; // don't use again later.
  }

  Undo_stop_ignoring_undo_operations();
}


static bool insert_recorded_midi_events(bool is_gfx){

  bool ret = false;

  bool do_split = doSplitIntoMonophonicTracksAfterRecordingFromMidi();
    
  QVector<struct WBlocks*> blocks_for_tracks_to_split;
  QVector<struct WTracks*> tracks_to_split;
  QSet<struct Tracks*> track_set;

  struct Tracker_Windows *window = root->song->tracker_windows;

  std::vector<midi_event_t> midi_events;
  
  {
    radium::ScopedMutex lock(g_midi_event_mutex);
    midi_events = g_recorded_midi_events.to_std_vector();
    if (!is_gfx)
      g_recorded_midi_events.clear();
  }

  {
    radium::ScopedUndo scoped_undo(!is_gfx);

    for(int i = 0 ; i < (int)midi_events.size(); i++) {
      
      auto midi_event = midi_events[i];

      //printf("%d / %d: %x\n",midi_event.timepos.blocknum, midi_event.timepos.tracknum, midi_event.msg);
        
      if (midi_event.timepos.tracknum >= 0 && midi_event.msg > 0) {

        // Find block and track
        //
        struct WBlocks *wblock = (struct WBlocks*)ListFindElement1(&window->wblocks->l, midi_event.timepos.blocknum);
        R_ASSERT_RETURN_IF_FALSE2(wblock!=NULL, true);

        struct WTracks *wtrack;

        if (midi_event.timepos.tracknum < 0)
          wtrack = wblock->wtracks;
        if (midi_event.timepos.tracknum >= wblock->block->num_tracks)
          wtrack = (struct WTracks*)ListLast1(&wblock->wtracks->l);
        else {
          wtrack = (struct WTracks*)ListFindElement1(&wblock->wtracks->l, midi_event.timepos.tracknum);
          R_ASSERT_RETURN_IF_FALSE2(wtrack!=NULL, true);
        }
                    
        struct Blocks *block = wblock->block;
        struct Tracks *track = wtrack->track;

        const uint32_t msg = midi_event.msg;
        const STime time = midi_event.timepos.blocktime;


        // Update GFX
        //
        if(!is_gfx)
          ATOMIC_SET(track->is_recording, false);


        // Do things that should only be done once for each track.
        // 
        if (!is_gfx || msg_is_note_on(msg)){

          if (track_set.contains(track)==false){
            
            if (is_gfx){

              // GFX

              Place p1,p2;

              PlaceSetFirstPos(&p1);
              PlaceSetLastPos(wblock->block,&p2);

              track->gfx_notes = NULL;
              CopyRange_notes(&track->gfx_notes,track->notes,&p1,&p2);

            } else {

              // !GFX

              track->gfx_notes = NULL;
              
              if (do_split){
                blocks_for_tracks_to_split.push_back(wblock);
                tracks_to_split.push_back(wtrack);
              }
              
              ADD_UNDO(Notes(window,
                             block,
                             track,
                             wblock->curr_realline
                             ));
              
            }

            track_set << track;

          }
          
        }

        // Add Data
        //
        if (!is_gfx && msg_is_note_off(msg)){

          // note off messages are normally treated in the call to add_recorded_note below, but if the note off message was received while playing a different block than the note on message, we treat it here.
          add_recorded_stp(block, track, time);
          ret = true;

        } else if (msg_is_note_on(msg)) {

          add_recorded_note(midi_events, wblock, block, wtrack, i, time, msg, is_gfx);
          ret = true;

        } else if (!is_gfx && msg_is_fx(msg)) {

          add_recorded_fx(midi_events, window, wblock, block, wtrack, i, midi_event);
          ret = true;

        }
      }

    }

  } // end of mutex and undo scope



  if (!is_gfx) {    
    if (do_split)
      for(int i = 0 ; i < tracks_to_split.size() ; i++){
        struct WBlocks *wblock = blocks_for_tracks_to_split[i];
        struct WTracks *wtrack = tracks_to_split[i];
          
        TRACK_split_into_monophonic_tracks(window, wblock, wtrack);
      }
  }


  return ret;
}


static int g_last_recorded_midi_events_size = 0;

// Called from the main thread after the player has stopped.
bool MIDI_insert_recorded_midi_events(void){
  ATOMIC_SET(root->song_state_is_locked, false);

  g_last_recorded_midi_events_size = 0;
  
  while(g_midi_event_queue->size() > 0) // Wait til the recording_queue_pull_thread is finished draining the queue.
    msleep(5);
  
  {
    radium::ScopedMutex lock(g_midi_event_mutex);
    //printf("MIDI_insert_recorded_midi_events called. Num events recorded: %d\n", g_recorded_midi_events.size());
    if (g_recorded_midi_events.size() == 0){
      return false;
    }
  }

  
  msleep(20); // Wait a little bit more for the last event to be transfered into g_recorded_midi_events. (no big deal if we lose it though, CPU is probably so buzy if that happens that the user should expect not everything working as it should. It's also only in theory that we could lose the last event since the transfer only takes some nanoseconds, while here we wait 20 milliseconds.)


  return insert_recorded_midi_events(false);
}

// Called regularly from the main thread.
bool MIDI_insert_recorded_midi_gfx_events(void){
  if(is_playing_relaxed()==false)
    return false;

  int recorded_midi_events_size = g_recorded_midi_events.size_relaxed();

  if (g_last_recorded_midi_events_size == recorded_midi_events_size)
    return false;

  g_last_recorded_midi_events_size = recorded_midi_events_size;

  //printf("\n\n\n ********************* Inserting from MIDI. size: %d ****************\n\n\n", recorded_midi_events_size);

  return insert_recorded_midi_events(true);
}

/*********************************************************
 *********************************************************
 **                  MIDI Learn                         **
 *********************************************************
 *********************************************************/

void MIDI_add_midi_learn(radium::MidiLearn *midi_learn){
  g_midi_learns.ensure_there_is_room_for_more_without_having_to_allocate_memory(1);

  {
    radium::ScopedMutex lock(g_midi_learns_mutex); // obtain this lock first to avoid priority inversion
    PLAYER_lock();{
      g_midi_learns.push_back(midi_learn);
    }PLAYER_unlock();
  }
  
  g_midi_learns.post_add();

  MIDILEARN_PREFS_add(midi_learn);
}

void MIDI_remove_midi_learn(radium::MidiLearn *midi_learn, bool show_error_if_not_here){
  R_ASSERT(show_error_if_not_here==false);
  
  MIDILEARN_PREFS_remove(midi_learn);
  
  for(auto midi_learn2 : g_midi_learns)
    if (midi_learn == midi_learn2) {
      radium::ScopedMutex lock(g_midi_learns_mutex); // obtain this lock first to avoid priority inversion
      PLAYER_lock();{
        g_midi_learns.remove(midi_learn2);
      }PLAYER_unlock();
      return;
    }

  if (show_error_if_not_here==false)
    RError("MIDI_remove_midi_learn: midi_learn not found");
}



/****************************************************************************
*****************************************************************************
 **          Send MIDI input to midi learn patch / current patch           **
 ****************************************************************************
 ****************************************************************************/

hash_t* radium::MidiLearn::create_state(void){
  hash_t *state = HASH_create(5);
  HASH_put_bool(state, "is_enabled", ATOMIC_GET(is_enabled));
  HASH_put_bool(state, "is_learning", ATOMIC_GET(is_learning));
  const symbol_t *port_name_symbol = ATOMIC_GET(port_name);
  HASH_put_chars(state, "port_name", port_name_symbol==NULL ? "" : port_name_symbol->name);
  HASH_put_int(state, "byte1", ATOMIC_GET(byte1));
  HASH_put_int(state, "byte2", ATOMIC_GET(byte2));
  return state;
}

void radium::MidiLearn::init_from_state(hash_t *state){
  ATOMIC_SET(byte2, HASH_get_int32(state, "byte2"));
  ATOMIC_SET(byte1, HASH_get_int32(state, "byte1"));
  ATOMIC_SET(port_name, get_symbol(HASH_get_chars(state, "port_name")));
  ATOMIC_SET(is_learning, HASH_get_bool(state, "is_learning"));
  ATOMIC_SET(is_enabled, HASH_get_bool(state, "is_enabled"));
}

bool radium::MidiLearn::RT_matching(const symbol_t *port_name, uint32_t msg){

  if (ATOMIC_GET(is_enabled)==false)
    return false;
  
  int d1 = MIDI_msg_byte1(msg);
  int d2 = MIDI_msg_byte2(msg);

  if (d1 < 0xb0)
    return false;

  if (d1 >= 0xc0 && d1 < 0xe0)
    return false;

  if (d1 >= 0xf0)
    return false;

  if (ATOMIC_GET(is_learning)){
    ATOMIC_SET(byte1, d1);
    ATOMIC_SET(byte2, d2);
    ATOMIC_SET(is_learning, false);
    ATOMIC_SET(this->port_name, port_name);
  }

  //printf("Got msg %x. byte1: %x, byte2: %x\n", msg, byte1, byte2);

  if(ATOMIC_GET(this->port_name) != port_name)
    return false;

  if (d1 < 0xc0) {

    // cc
    
    if(d1==ATOMIC_GET(byte1) && d2==ATOMIC_GET(byte2))
      return true;

  } else {
    
    // pitch bend

    if (d1==ATOMIC_GET(byte1))
      return true;
  }

  return false;
}

bool radium::MidiLearn::RT_maybe_use(const symbol_t *port_name, uint32_t msg){  
  if (RT_matching(port_name, msg)==false)
    return false;

  float value = get_msg_fx_value(msg);

  RT_callback(value);
  
  return true;
}

typedef struct {
  const symbol_t *port_name;
  int32_t deltatime;
  uint32_t msg;
} play_buffer_event_t;

static boost::lockfree::queue<play_buffer_event_t, boost::lockfree::capacity<8000> > g_play_buffer;

void radium::MidiLearn::RT_maybe_use_forall(int64_t instrument_id, const symbol_t *port_name, uint32_t msg){
  for (auto midi_learn : g_midi_learns) {
    bool may_use = false;

    if (instrument_id == -1)
      may_use = true;
    else {
      int64_t id2 = midi_learn->RT_get_instrument_id();
      if (id2==-1 || id2==instrument_id)
        may_use = true;
    }

    if (may_use)
      midi_learn->RT_maybe_use(port_name, msg);
  }
}

// Called from the player thread
void RT_MIDI_handle_play_buffer(void){

  play_buffer_event_t event;

  bool has_set_midi_receive_time = false;

  struct Instruments *midi_instrument = get_MIDI_instrument();
  struct Instruments *audio_instrument = get_audio_instrument();

  if (midi_instrument==NULL || audio_instrument==NULL){ // happens during init
    R_ASSERT(midi_instrument==NULL && audio_instrument==NULL);
    return;
  }
  
  struct Patch *playing_patches[midi_instrument->patches.num_elements + audio_instrument->patches.num_elements + 1]; // add 1 to avoid ubsan hit if there are no instruments.

  bool has_inited = false;
  int num_playing_patches = 0;

  struct SeqTrack *seqtrack = NULL; // set to NULL to silence false compiler warning.
  struct Patch *through_patch = NULL; // set to NULL to silence false compiler warning.
  double seqtrack_starttime = 0.0; // set to 0.0 to silence false compiler warning.

  while(g_play_buffer.pop(event)==true){

    uint32_t msg = event.msg;

    radium::MidiLearn::RT_maybe_use_forall(-1, event.port_name, msg);

    if (has_inited == false){
      
      seqtrack = RT_get_curr_seqtrack();
      seqtrack_starttime = seqtrack->start_time;

      if(ATOMIC_GET(g_send_midi_input_to_current_instrument))
        through_patch = ATOMIC_GET(g_through_patch);
      else
        through_patch = NULL;

      VECTOR_FOR_EACH(struct Patch *, patch, &midi_instrument->patches){
        if(ATOMIC_GET(patch->always_receive_midi_input)){
          playing_patches[num_playing_patches] = patch;
          num_playing_patches++;
        }
      }END_VECTOR_FOR_EACH;

      VECTOR_FOR_EACH(struct Patch *, patch, &audio_instrument->patches){
        if(ATOMIC_GET(patch->always_receive_midi_input)){
          playing_patches[num_playing_patches] = patch;
          num_playing_patches++;
          
          struct SoundPlugin *plugin = static_cast<struct SoundPlugin*>(patch->patchdata);
          if (plugin==NULL){
            R_ASSERT_NON_RELEASE(false);
          } else {
            PLUGIN_touch(plugin);
          }
        }
      }END_VECTOR_FOR_EACH;

      has_inited = true;
    }
    
    if(through_patch!=NULL || num_playing_patches > 0){

      uint8_t byte1 = MIDI_msg_byte1(msg);
      if (ATOMIC_GET(g_use_track_channel_for_midi_input)){
        byte1 &= 0xf0;
        byte1 |= ATOMIC_GET(g_curr_midi_channel);
      }
      uint8_t byte[3] = {byte1, (uint8_t)MIDI_msg_byte2(msg), (uint8_t)MIDI_msg_byte3(msg)};

      if (has_set_midi_receive_time==false){
        g_last_midi_receive_time = TIME_get_ms();
        has_set_midi_receive_time = true;
      }

      if (through_patch != NULL)
        RT_MIDI_send_msg_to_patch(seqtrack, (struct Patch*)through_patch, byte, 3, seqtrack_starttime);

      for(int i=0;i<num_playing_patches;i++)
        RT_MIDI_send_msg_to_patch(seqtrack, playing_patches[i], byte, 3, seqtrack_starttime);
    }
    
  }
}

// Called from a MIDI input thread
static void add_event_to_play_buffer(const symbol_t *port_name, uint32_t msg){
  play_buffer_event_t event;

  event.port_name = port_name;
  event.deltatime = 0;
  event.msg = msg;

  if (!g_play_buffer.bounded_push(event))
    RT_message("Midi play buffer full.\nMost likely the player can not keep up because it uses too much CPU.\nIf that is not the case, please report this incident.");
}

// This function is called with NULL as argument when the current trought-patch instrument is made inactive, and when loading song.
void MIDI_SetThroughPatch(struct Patch *patch){
  //printf("Sat new patch %p\n",patch);
  ATOMIC_SET(g_through_patch, patch);
}

struct Patch *MIDI_GetThroughPatch(void){
  return ATOMIC_GET(g_through_patch);
}


/*********************************************************
 *********************************************************
 **       Got MIDI from the outside. Entry point.       **
 *********************************************************
 *********************************************************/

// Can be called from any thread
void MIDI_InputMessageHasBeenReceived(const symbol_t *port_name, int cc,int data1,int data2){
  //printf("got new message. on/off:%d. Message: %x,%x,%x\n",(int)root->editonoff,cc,data1,data2);
  //static int num=0;
  //num++;

  if(cc==0xf0 || cc==0xf7) // sysex not supported
    return;
  
  if (MIXER_is_saving())
    return;
  
  bool isplaying = is_playing();

  switch(cc){
    case 0xfa:
      RT_request_to_start_playing_block();
      return;
    case 0xfb:
      RT_request_to_continue_playing_block();
      return;
    case 0xfc:
      RT_request_to_stop_playing();
      return;
  }
  
  uint32_t msg = MIDI_msg_pack3(cc, data1, data2);
  int len = MIDI_msg_len(msg);
  if (len<1 || len>3)
    return;

  add_event_to_play_buffer(port_name, msg);

  if (g_record_accurately_while_playing && isplaying) {
    
    if (ATOMIC_GET(root->editonoff))
      record_midi_event(port_name, msg);

  } else {

    if(msg_is_note_on(msg))
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


// called very often from the main thread
void MIDI_HandleInputMessage(void){
  // should be a memory barrier here somewhere.

  uint32_t msg = ATOMIC_GET(g_msg); // Hmm, should have an ATOMIC_COMPAREFALSE_AND_SET function. (doesn't matter though, it would just look better)
  
  if (msg!=0) {

    ATOMIC_SET(g_msg, 0);

    if(ATOMIC_GET(root->editonoff)){
      float velocity = -1.0f;
      if (g_record_velocity)
        velocity = (float)MIDI_msg_byte3(msg) / 127.0;
      //printf("velocity: %f, byte3: %d, shift: %d\n",velocity,MIDI_msg_byte3(msg),shiftPressed());
      InsertNoteCurrPos(root->song->tracker_windows,MIDI_msg_byte2(msg), shiftPressed(), velocity);
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
  
  // Load config from disk
  {
    doUseTrackChannelForMidiInput();
    isSendingMidiInputToCurrentInstrument();

    MIDI_set_record_accurately(SETTINGS_read_bool("record_midi_accurately", true));
    MIDI_set_record_velocity(SETTINGS_read_bool("always_record_midi_velocity", false));
  }

  {
    radium::ScopedMutex lock(g_midi_event_mutex); // Should have a comment here why we need to lock this mutex, if it is really necessary to lock it.
    
    g_midi_event_queue = new radium::Queue<midi_event_t, 8000>;
    
    g_recording_queue_pull_thread.start();
  }
}

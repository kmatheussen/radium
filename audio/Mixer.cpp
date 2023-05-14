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


#include <unistd.h>
#include <stdio.h>
#include <stdint.h>
#include <math.h>

#include "../weakjack/weak_libjack.h"

#include <QString>
#include <QStringList>
#include <QTime>
#include <QThread>
#include <QMouseEvent>

#include "../common/nsmtracker.h"
#include "../Qt/helpers.h"

#include "../common/time_proc.h"
#include "../common/Mutex.hpp"
#include "../common/Time.hpp"
#include "../common/visual_proc.h"
#include "../common/player_proc.h"
#include "../common/playerclass.h"
#include "../common/song_tempo_automation_proc.h"
#include "../common/Semaphores.hpp"
#include "../common/stacktoucher_proc.h"
#include "../common/OS_Player_proc.h"
#include "../common/threading_lowlevel.h"
#include "../common/scheduler_proc.h"
//#include "../common/PEQ_LPB_proc.h"
#include "../common/OS_visual_input.h"
#include "../common/sequencer_proc.h"
#include "../common/sequencer_timing_proc.h"
#include "../common/settings_proc.h"

#include "../midi/midi_i_input_proc.h"

#include "Jack_plugin_proc.h"
#include "SoundfileSaver_proc.h"
#include "get_windows_commandlines_proc.h"
#include "KillJackd_proc.h"

#include "SoundProducer_proc.h"
#include "SoundPluginRegistry_proc.h"
#include "SoundPlugin_proc.h"
#include "MultiCore_proc.h"
#include "CpuUsage.hpp"
#include "SampleRecorder_proc.h"
#include "Juce_plugins_proc.h"
#include "Modulator_plugin_proc.h"
#include "AudioBuffer.hpp"
#include "SendReceive_plugins_proc.h"
#include "Sampler_plugin_proc.h"

#include "Mixer_proc.h"

volatile bool g_test_crashreporter_in_audio_thread = false;

extern PlayerClass *pc;

static RSemaphore *g_saving_sound_has_started = NULL;

// these four variables can only be written to in the audio thread.
static volatile int g_num_allocated_click_plugins = 0;
static volatile int g_num_click_plugins = 0;
static SoundPlugin **g_click_plugins = NULL;
static Patch **g_click_patches = NULL; // only written to in RT_MIXER_get_all_click_patches.

int g_audio_block_size = 64;

DEFINE_ATOMIC(int, g_num_jack_xruns) = 0;


#ifdef MEMORY_DEBUG

static radium::Mutex debug_mutex;
static radium::CondWait debug_wait;

void PLAYER_memory_debug_wake_up(void){
  debug_wait.notify_one();
}
#endif

DEFINE_ATOMIC(bool, g_currently_processing_dsp) = false;
DEFINE_ATOMIC(double, g_curr_song_tempo_automation_tempo) = 1.0;


jack_client_t *g_jack_client = NULL;
static int g_jack_client_priority;
struct CpuUsage g_cpu_usage;

DEFINE_ATOMIC(int64_t, g_last_mixer_time) = 0;

namespace{
  struct Mixer;
}
static Mixer *g_mixer = NULL;


void THREADING_acquire_player_thread_priority2(radium_thread_t thread){
#if 1
  static bool has_shown_warning = false;

  if (g_jack_client==NULL) {

    if (!JUCE_audio_set_audio_thread_priority(thread)) {
      if (has_shown_warning==false) {
        has_shown_warning=true;
#if defined(FOR_LINUX)
        RT_message("Error: Unable to set real time priority. You might want to check your system configuration.");
#else
        RT_message("Error: Unable to set real time priority.");
#endif
      }
    }
    
  } else {
    
    int err = jack_acquire_real_time_scheduling(thread, g_jack_client_priority);
    
    if (err != 0 && has_shown_warning==false) {
#if 0 //def FOR_MACOSX
      printf("jack_acquire_real_time_scheduling(GET_CURRENT_THREAD(), g_jack_client_priority); failed: %d\n", err);
#endif
      has_shown_warning=true;
      RT_message("Unable to set real time priority. Error code: %d. (EPERM: %d) (errno: %d) (strerror: \"%s\")\n"
                 "\n"
                 "You should:\n"
                 "\n"
                 "1. Quit Radium\n"
                 "2. Stop Jack\n"
                 "3. Start Jack\n"
                 "4. Start Radium\n"
#if defined(FOR_LINUX)
                 "\n"
                 "On Linux, you might also want to check your system configuration."
#endif
                 "\n\n"
                 , err,
                 EPERM,
                 errno,
                 strerror(errno)
                 );
      
    }
  }
  
#endif
}

void THREADING_acquire_player_thread_priority(void){
  THREADING_acquire_player_thread_priority2(GET_CURRENT_THREAD());
}

void THREADING_drop_player_thread_priority2(radium_thread_t thread){
  bool success;
  
  if (g_jack_client==NULL)
    success = JUCE_audio_set_normal_thread_priority(thread);
  else    
    success = (jack_drop_real_time_scheduling(thread)==0);

  if (!success)
    RT_message("Error: Unable to drop real time priority.");  
}

void THREADING_drop_player_thread_priority(void){
  THREADING_drop_player_thread_priority2(GET_CURRENT_THREAD());
}

#if 0
static void PLAYER_drop_same_priority(void){
  THREADING_drop_player_thread_priority();
}
#endif


static void check_jackd_arguments(void){
#if defined(FOR_WINDOWS)
  
  QString mandatory= "The \"-S\" parameter is mandatory to make the jack server work correctly.\n\n"
    "The jackd argument line can be set in QJackCtl (\"Jack Control\") under Setup -> Settings -> Server Prefix. The line should look like this:\n\n"
    "jackd -S";
  
  bool found_jack = false;
  bool found_sync_flag=false;
  
  vector_t *command_lines = get_windows_command_lines();

  for(int command_line_num=0;command_line_num<command_lines->num_elements;command_line_num++){
    
    const char *command_line = (const char *)command_lines->elements[command_line_num];
    QString line(command_line);

    line = line.trimmed();

    printf("Got line: \"%s\" %s\n",line.toUtf8().constData(),command_line);

    if(line.startsWith("jackd") || line.startsWith("jackd.exe") || line.contains("/jackd.exe ") || line.contains("\\jackd.exe ") || line.contains("\\jackd ") || line.contains("/jackd.exe\"") || line.contains("\\jackd.exe\"")) {

      found_jack = true;

      QStringList elements = line.split(" ", Qt::SkipEmptyParts);

      for(int i=0;i<elements.size();i++){
        QString element = elements.at(i);
        if(element=="-S")
          found_sync_flag=true;
      }
    }
  }

  if(found_jack==false){

#if !defined(RELEASE)
    ScopedQPointer<MyQMessageBox> msgBox(MyQMessageBox::create(true));
    msgBox->setIcon(QMessageBox::Warning);
    msgBox->setText("Unable to find jack process command line arguments.");
    msgBox->setInformativeText("Please make sure the -S flag was added to the jackd argument line. If not, glitches in sound will occur.\n ");
    msgBox->setInformativeText(mandatory);
    msgBox->setStandardButtons(QMessageBox::Ok);
    safeExec(msgBox, false);
#endif
    
  } else if(found_sync_flag==false){

    ScopedQPointer<MyQMessageBox> msgBox(MyQMessageBox::create(true));
    msgBox->setIcon(QMessageBox::Critical);
    msgBox->setText("The -S parameter was not set for Jack.");
    msgBox->setInformativeText(mandatory);
    
    msgBox->setStandardButtons(QMessageBox::Ok);
    safeExec(msgBox, false);
  }


#endif
}

#if !USE_SPINLOCK_FOR_PLAYER_LOCK && (defined(FOR_LINUX) || defined(FOR_MACOSX))
static pthread_mutexattr_t player_lock_mutexattr; // Why is this one global?
#endif
static LockType player_lock;
static LockType player_runner_lock;

// The main thread waits for this semaphore before it can obtain the player lock.
// This to make sure the player lock is obtained right after the player thread releases the lock (and not, for instance, right before the player thread tries to obtain the lock)
// (currently not used since it too long time to stop player, which is bad when we want to pause the player as quickly as possible, and probably other situations).
static radium::Semaphore player_lock_semaphore;

static __thread bool g_current_thread_has_player_lock = false;
static bool g_someone_has_player_lock = false;
static __thread bool g_current_thread_has_player_runner_lock = false;



#if 0
// sometimes, this code is VERY useful
extern "C" int backtrace(void**,int);
extern "C" char** backtrace_symbols(void**,int);

static void print_backtrace(void){
#define NUM_LINES 100
      
      void *buffer[NUM_LINES];
      char **strings;
      int nptrs = backtrace(buffer, NUM_LINES);
      
      strings = backtrace_symbols(buffer, nptrs);

      for(int i=0;i<nptrs;i++){
        printf("%d: %s\n",i,strings[i]);
      }
}
#endif

static bool g_player_locks_initialized = false;


static void lock_player(void){
  R_ASSERT(!PLAYER_current_thread_has_lock()); // the player lock is reentrant, just in case, but reentrancy is not supposed to be used.

  R_ASSERT_NON_RELEASE(g_player_locks_initialized==true);
  
  LOCK_LOCK(player_lock);
  THREADING_inc_RT();
  g_current_thread_has_player_lock = true;
  g_someone_has_player_lock = true;
}

static void unlock_player(void){
  R_ASSERT_RETURN_IF_FALSE(PLAYER_current_thread_has_lock());

  R_ASSERT_NON_RELEASE(g_player_locks_initialized==true);
  g_someone_has_player_lock = false;
  g_current_thread_has_player_lock = false;
  THREADING_dec_RT();
  LOCK_UNLOCK(player_lock);
}

#if defined(FOR_MACOSX)
  #define MAX_LOCK_DURATION_MS 0.15
  #define MAX_LOCK_DURATION_TO_REPORT_ABOUT_MS 0.9 // Set this value lower to get more messages about spending too much time holding the player lock.
#else
  #define MAX_LOCK_DURATION_MS 0.05
  #define MAX_LOCK_DURATION_TO_REPORT_ABOUT_MS 0.3 // Set this value lower to get more messages about spending too much time holding the player lock.
#endif

static bool g_signalled_someone = false;

static DEFINE_ATOMIC(bool, g_player_wants_player_lock) = false;

static void RT_lock_player(){
  R_ASSERT(THREADING_is_player_thread());

  ATOMIC_SET(g_player_wants_player_lock, true);

#if !defined(RELEASE)
  
  double start = TIME_get_ms();{
    lock_player();
  }double dur = TIME_get_ms() - start;

  if (dur > MAX_LOCK_DURATION_TO_REPORT_ABOUT_MS) {
    //if (elapsed > 1)
    //  abort(); // That's really bad. Need a backtrace. (No, the relevant backtrace is gone now. TODO: fix. When obtaining lock we should store __FILE__ and __LINE__.)
    RT_message("RT_lock_player: Waiting longer than %fms to get lock: %fms", MAX_LOCK_DURATION_TO_REPORT_ABOUT_MS, dur);
  }
  
#else
  
  lock_player();
  
#endif

  ATOMIC_SET(g_player_wants_player_lock, false);
    
  g_signalled_someone = player_lock_semaphore.signalIfAnyoneIsWaiting();
}

static void RT_unlock_player(){
  R_ASSERT(THREADING_is_player_thread());
  if (!g_signalled_someone)
    player_lock_semaphore.signalIfAnyoneIsWaiting();
  unlock_player();
}

namespace{
class PlayerLock_Local{

  PlayerLock_Local(const PlayerLock_Local&) = delete;
  PlayerLock_Local& operator=(const PlayerLock_Local&) = delete;

public:

  const bool _enable;
  
  PlayerLock_Local(const bool enable = true)
    : _enable(enable)
  {
    if (enable)
      RT_lock_player();
  }

  ~PlayerLock_Local(){
    if (_enable)
      RT_unlock_player();
  }
};
}

#if !defined(FOR_LINUX) || USE_SPINLOCK_FOR_PLAYER_LOCK
static priority_t g_priority_used_before_obtaining_PLAYER_lock; // Protected by the player lock
#endif

static radium::Time g_player_lock_timer; // Calling 'restart' is protected by the player lock. ('elapsed' is thread safe)


// Set to 0 since it takes a very long time to stop playing if we wait for player. TODO: Set to 1 if block size is very low.
#define DO_WAIT_FOR_PLAYER_TO_FINISH_BEFORE_ACQUIRING_PLAYER_LOCK 0

#if DO_WAIT_FOR_PLAYER_TO_FINISH_BEFORE_ACQUIRING_PLAYER_LOCK
#define MAYBE_WAIT_FOR_PLAYER_TO_FINISH() player_lock_semaphore.wait(); // Make sure we don't try to hold the lock until right after the player is finished with it.
#else
#define MAYBE_WAIT_FOR_PLAYER_TO_FINISH() /* */
#endif

// Only called from PLAYER_maybe_pause_lock_a_little_bit and PLAYER_lock.
static void lock_player_from_nonrt_thread(void){

  // I.e. if it's less than 10ms since we released the lock, we sleep a little bit to make sure the player thread isn't blocked by us.
  //if(g_player_lock_timer.RT_elapsed() < 10)
  //  msleep(10);
  
#if defined(FOR_LINUX) && !USE_SPINLOCK_FOR_PLAYER_LOCK

  MAYBE_WAIT_FOR_PLAYER_TO_FINISH();

  // we use mutex with the PTHREAD_PRIO_INHERIT on linux, so we don't have to acquire same thread priority as the player.
  
  lock_player();
  //print_backtrace();

#else
  
  priority_t priority = THREADING_get_priority();

  // Manually avoid priority inversion
  //
  // OSX: From the source code of OSX, it seems like OSX doesn't support PTHREAD_PRIO_INHERIT.
  //
  // Windows: It's a bit unclear how well priority inheritance works on this platform.
  // Probably we don't have to acquire same priority, but we do it anyway to be sure.
  //
  if (g_mixer != NULL) // On MacOS we get an error message if trying to obtain player priority before mixer has started.
    THREADING_acquire_player_thread_priority();

  MAYBE_WAIT_FOR_PLAYER_TO_FINISH();

  lock_player();

  g_priority_used_before_obtaining_PLAYER_lock = priority;
  
#endif
    
  g_player_lock_timer.restart();
}

// Only called from PLAYER_maybe_pause_lock_a_little_bit and PLAYER_unlock
static void unlock_player_from_nonrt_thread(int iteration){

#if !defined(RELEASE) && !defined(RADIUM_USES_TSAN)
  float elapsed = g_player_lock_timer.elapsed();
#endif

  //g_player_lock_timer.restart();

    
#if defined(FOR_LINUX) && !USE_SPINLOCK_FOR_PLAYER_LOCK // we use mutex with the PTHREAD_PRIO_INHERIT on linux
  
  unlock_player();
    
#else
  
  priority_t priority = g_priority_used_before_obtaining_PLAYER_lock; // 
  
  unlock_player();

  //PLAYER_drop_same_priority();
  THREADING_set_priority(priority);
  
#endif

#if !defined(RELEASE) && !defined(RADIUM_USES_TSAN)
  //printf("Elapsed: %f. (%d)\n", elapsed, iteration);
  if(elapsed > MAX_LOCK_DURATION_TO_REPORT_ABOUT_MS){  // The lock is realtime safe, but we can't hold it a long time.

    if (THREADING_is_main_thread()){
      addMessage(talloc_format("Warning: Holding player lock (%d) for more than %fms: %fms.<br>\n<pre>%s</pre>\n",
                               iteration,
                               MAX_LOCK_DURATION_TO_REPORT_ABOUT_MS, elapsed,
                               JUCE_get_backtrace()));
    } else {
      printf("Warning (non-main thread): Holding player lock (%d) for more than %fms: %fms.<br>\n<pre>%s</pre>\n",
             iteration,
             MAX_LOCK_DURATION_TO_REPORT_ABOUT_MS, elapsed,
             JUCE_get_backtrace()
             );
    }
  }
#endif
}

void PLAYER_maybe_pause_lock_a_little_bit(int iteration){
  
  R_ASSERT_NON_RELEASE(!THREADING_is_player_or_runner_thread());
  R_ASSERT_NON_RELEASE(PLAYER_current_thread_has_lock());

  bool player_wants_lock = ATOMIC_GET(g_player_wants_player_lock);
#if defined(RELEASE)
  float elapsed = player_wants_lock ? 0.0 : g_player_lock_timer.elapsed();
#else
  float elapsed = g_player_lock_timer.elapsed();
#endif
    
  if (player_wants_lock || elapsed > MAX_LOCK_DURATION_MS){
    unlock_player_from_nonrt_thread(iteration);
#if !defined(RELEASE)
    printf("   Player lock pausing. Player wants lock: %d. Elapsed: %f.\n", player_wants_lock, elapsed);
#endif
#if !DO_WAIT_FOR_PLAYER_TO_FINISH_BEFORE_ACQUIRING_PLAYER_LOCK
    msleep(1);
#endif
    lock_player_from_nonrt_thread();
  }
}

void PLAYER_lock(void){

  R_ASSERT(!THREADING_is_player_or_runner_thread());

  //static int num=0; printf("  %d: ========================================= PLAYER_LOCK  \n", num++);
  
#if 0 //!defined(RELEASE)
  printf("  PLAYER_LOCK  \n");
  if (ATOMIC_GET(root->editonoff)==false){
    static int downcount = 1;
    fprintf(stderr," Aborting since edit is turned off. (this is a debug feature and not a bug!). count down: %d\n", downcount);
    if (downcount==0)
      abort();
    downcount--;
  }
  /*
  printf("   >> Obtaining player lock\n");
  if (is_playing())
    abort();
  */
#endif

  lock_player_from_nonrt_thread();
}

void PLAYER_unlock(void){
  R_ASSERT(!THREADING_is_player_or_runner_thread());

  unlock_player_from_nonrt_thread(-1);
}

void RT_PLAYER_runner_lock(void){
  R_ASSERT_NON_RELEASE(THREADING_is_player_or_runner_thread() || g_current_thread_has_player_lock);
  
  LOCK_LOCK(player_runner_lock);
  g_current_thread_has_player_runner_lock = true;
}

void RT_PLAYER_runner_unlock(void){
  R_ASSERT_NON_RELEASE(THREADING_is_player_or_runner_thread() || g_current_thread_has_player_lock);
  
  g_current_thread_has_player_runner_lock = false;
  LOCK_UNLOCK(player_runner_lock);
}

bool PLAYER_current_thread_has_lock(void){
  return g_current_thread_has_player_lock || g_current_thread_has_player_runner_lock;
}

bool PLAYER_someone_has_player_lock(void){
  return g_someone_has_player_lock;
}


void THREADING_init_player_locks(void){

  R_ASSERT(g_player_locks_initialized==false);
    
  LOCK_INITIALIZE(player_runner_lock); // Don't have to do anything special. The player_runner_lock is always called from a realtime thread, and never recursively.

#if defined(FOR_WINDOWS) || USE_SPINLOCK_FOR_PLAYER_LOCK
  
   LOCK_INITIALIZE(player_lock);

#else
   
  int s1 = pthread_mutexattr_init(&player_lock_mutexattr);
  if (s1!=0)
    GFX_Message(NULL, "pthread_mutexattr_init failed: %d\n",s1);

  int s2 = pthread_mutexattr_settype(&player_lock_mutexattr, PTHREAD_MUTEX_RECURSIVE);
  if (s2!=0)
    GFX_Message(NULL, "pthread_mutexattr_settype failed: %d\n",s2);

#ifdef FOR_LINUX
  int s3 = pthread_mutexattr_setprotocol(&player_lock_mutexattr, PTHREAD_PRIO_INHERIT);  // Regarding macosx, I don't know whether it supports PTHREAD_PRIO_INHERIT, so we just boost priority manually instead on that platform.
  if (s3!=0)
    GFX_Message(NULL, "pthread_mutexattr_setprotocol failed: %d\n",s3);
#endif
  
  int s4 = pthread_mutex_init(&player_lock, &player_lock_mutexattr);
  if (s4!=0)
    GFX_Message(NULL, "pthread_mutex_init failed: %d\n",s4);

#endif

  g_player_locks_initialized = true;
}


/*
static void RT_MIXER_check_if_someone_has_solo(void);
static void RT_MIXER_check_if_at_least_two_soundproducers_are_selected(void);
*/

int g_soundcardblock_delta_time = 0;

int g_soundcardblock_size = 0; // Player and runner threads.
DEFINE_ATOMIC(int, g_soundcardblock_size) = 0; // Any thread

static DEFINE_ATOMIC(int, g_soundcardblock_size2) = 0;
static DEFINE_ATOMIC(STime, audioblock_cycle_start_time) = 0;
static DEFINE_ATOMIC(STime, audioblock_last_frame_stime) = 0;
static DEFINE_ATOMIC(double, audioblock_cycle_start_ms) = 0.0;
static DEFINE_ATOMIC(Blocks *, audioblock_block) = NULL;
static DEFINE_ATOMIC(STime, audioblock_seqtime) = 0;
static DEFINE_ATOMIC(double, audioblock_song_tempo_multiplier) = 1.0;

//static DEFINE_SPINLOCK(audioblock_spinlock); // used by two realtime threads (midi input and audio thread)
static radium::SetSeveralAtomicVariables audioblock_variables_protector;

static QElapsedTimer pause_time;
static bool g_process_plugins = true;
static DEFINE_ATOMIC(bool, g_request_to_pause_plugins) = false;

int g_audio_system_input_latency = 0;
int g_audio_system_output_latency = 0;


void RT_pause_plugins(void){
  ATOMIC_SET(g_request_to_pause_plugins, true);
}

#ifdef FOR_WINDOWS
extern "C" {
  static void my_silent_jack_error_callback(const char *desc){
  }
}
#endif

namespace{
  enum class RadiumTransportState{
    NO_STATE,
    WAITING_FOR_PLAYER_TO_BE_READY,
    PLAY_REQUEST_FAILED,
    PLAYER_IS_READY
  };
}

namespace{

struct Mixer{
  SoundProducer *_bus[NUM_BUSES] = {};

  radium::Vector<SoundProducer*> _sound_producers;
  
  jack_client_t *_rjack_client = NULL;
  int64_t _last_time = 0;
  int64_t _time = 0;

  float _sample_rate;

  jack_port_t *_main_inputs[NUM_SYSTEM_INPUT_JACK_PORTS];
  
  bool _is_saving_sound = false; // should only be accessed from the player

  RadiumTransportState _radium_transport_state = RadiumTransportState::NO_STATE;
  
  Mixer()
  {
  }

  void add_SoundProducer(SoundProducer *sound_producer){    
    SoundPlugin *plugin = SP_get_plugin(sound_producer);
    int bus_num = SP_get_bus_num(sound_producer);
#if !defined(RELEASE)
    if (bus_num>=0){
      if (plugin->patch->id.id==0)
        abort();
    }
#endif
      
    bool is_click_patch = false;
    SoundPlugin **new_g_click_plugins = NULL;
    Patch **new_g_click_patches = NULL;
    int new_num_allocated_click_plugins = g_num_allocated_click_plugins;

    SoundPlugin **old_g_click_plugins = g_click_plugins;
    Patch **old_g_click_patches = g_click_patches;

    if (!strcmp(plugin->type->type_name,"Sample Player")) {
      if(!strcmp(plugin->type->name,g_click_name)) {

        if (g_num_allocated_click_plugins<=0){
          R_ASSERT(g_num_allocated_click_plugins==0);
          new_num_allocated_click_plugins = 16;
        }
        
        while (new_num_allocated_click_plugins < (1+g_num_click_plugins))
          new_num_allocated_click_plugins = new_num_allocated_click_plugins * 2;
        
        if (new_num_allocated_click_plugins > g_num_allocated_click_plugins) {
          new_g_click_plugins = (SoundPlugin **)V_calloc(sizeof(SoundPlugin*), new_num_allocated_click_plugins);
          new_g_click_patches = (Patch **)V_calloc(sizeof(Patch*), new_num_allocated_click_plugins);
        }
        
        is_click_patch = true;
      }
    }

    _sound_producers.ensure_there_is_room_for_more_without_having_to_allocate_memory(1);

    PLAYER_lock();{

      if (bus_num>=0)
        _bus[bus_num] = sound_producer;

      if (is_click_patch) {
        if (new_g_click_plugins != NULL) {
          
          if (g_num_allocated_click_plugins > 0)
            memcpy(new_g_click_plugins, g_click_plugins, sizeof(SoundPlugin*)*g_num_allocated_click_plugins);
          
          g_click_plugins = new_g_click_plugins;
          
          g_click_patches = new_g_click_patches;
          g_num_allocated_click_plugins = new_num_allocated_click_plugins;
        }
        
        int i;
        
        for(i=0; i<g_num_allocated_click_plugins ; i++){
          if (g_click_plugins[i]==NULL) {
            g_click_plugins[i] = plugin;
            g_num_click_plugins++;
            break;
          }
        }
        
        R_ASSERT(i<g_num_allocated_click_plugins);
      }

      //MULTICORE_ensure_capacity(_sound_producers.num_elements);
      _sound_producers.push_back(sound_producer);
      MULTICORE_add_sp(sound_producer);

    }PLAYER_unlock();

    _sound_producers.post_add();

    if (new_g_click_plugins != NULL) {
      V_free(old_g_click_plugins);
      V_free(old_g_click_patches);
    }

    SP_call_me_after_solo_has_changed();
    
    SEND_RECEIVE_update_send_receivers();
  }

  // Called from MIXER_remove_SoundProducer, which is called from ~SoundProducer
  //
  void remove_SoundProducer(SoundProducer *sound_producer){
    const SoundPlugin *plugin = SP_get_plugin(sound_producer);
    
    bool is_click_patch = false;

    if (!strcmp(plugin->type->type_name,"Sample Player"))
      if(!strcmp(plugin->type->name,g_click_name))
        is_click_patch = true;

    
    PLAYER_lock();{
      //_sound_producers.remove(_sound_producers.indexOf(sound_producer));
      _sound_producers.remove(sound_producer);
      MULTICORE_remove_sp(sound_producer);

      if (is_click_patch) {
        int i;
        for(i=0; i<g_num_allocated_click_plugins ; i++){
          if (g_click_plugins[i] == plugin) {
            g_click_plugins[i] = NULL;
            g_num_click_plugins--;
            break;
          }
        }
        
        R_ASSERT(i<g_num_allocated_click_plugins);
      }

      for(int busnum=0;busnum<NUM_BUSES;busnum++)
        if (sound_producer==_bus[busnum])
          _bus[busnum]=NULL;
      
    }PLAYER_unlock();

    SP_call_me_after_solo_has_changed();
  }

  void set_jack_output_latency(void) const {
    const char **inportnames=jack_get_ports(_rjack_client,NULL,NULL,JackPortIsPhysical|JackPortIsInput);

    if (inportnames != NULL) {
      
      for(int portnum=0 ; inportnames[portnum] != NULL ; portnum++){
        
        jack_port_t *physical_port = jack_port_by_name(_rjack_client, inportnames[portnum]);
        
        if (physical_port!=NULL){

          jack_latency_range_t range;
          jack_port_get_latency_range(physical_port, JackPlaybackLatency, &range);
          g_audio_system_output_latency = R_MAX(0, (int)range.max);

          if (g_audio_system_output_latency > 0)
            break;

        }
      }

      jack_free(inportnames);

    }
  }
  
  bool start_jack(void){
    if (SETTINGS_read_bool("use_jack_if_jack_server_is_running", true)==false)
      return false;
  
    if (have_libjack()!=0)
      return false;
      
    jack_status_t status;

    const char *client_name;

    waitUntilNsmHasInited();
      
    if (supportsSwitchNsmCapability()) {

      client_name = "radium_audio";

    } else {
      
      if (!nsmIsActive()) {
        
        client_name = "radium_audio";
        
      } else {
        
        client_name = getNsmClientId();
        
      }

    }
        
    _rjack_client = jack_client_open(client_name,JackNoStartServer,&status,NULL);

    if (_rjack_client == NULL) {
      fprintf (stderr, "jack_client_open() failed, "
	       "status = 0x%2.0x\n", status);
      if (status & JackServerFailed) {
	fprintf (stderr, "Unable to connect to JACK server\n");
      }

      /*
      ScopedQPointer<MyQMessageBox> msgBox(MyQMessageBox::create(true));
      msgBox->setIcon(QMessageBox::Critical);
      msgBox->setText("Unable to connect to Jack.");
      msgBox->setInformativeText("The Jack Audio Connection Kit server must be started before running Radium. "
                                "\n\n"
                                "Please read the file README_first.txt"
				);
      
      msgBox->setStandardButtons(QMessageBox::Ok);
      safeExec(msgBox, false);
      */
      
      return false;

    }

    // Ensure we are not freewheeling. This happens when restarting after radium has died while saving soundfile.
    // No, if that happens, jack should be resetted.
    //jack_set_freewheel(_rjack_client, 0);
  
    //if(_buffer_size < RADIUM_BLOCKSIZE)
    //  GFX_Message(NULL, "Jack's blocksize of %d is less than Radium's block size of %d. You will get bad sound. Adjust your audio settings.", _buffer_size, RADIUM_BLOCKSIZE);

    int buffer_size = jack_get_buffer_size(_rjack_client);

    if(buffer_size < RADIUM_BLOCKSIZE){

      //SETTINGS_write_int("audio_block_size",_buffer_size);
      
      GFX_Message(NULL,
                  "Radium's internal block size (%d) is higher than Jack's block size (%d). This is not supported. Program will not use jack.\n"
                  //"Radium block size has now been set to %d, but Radium needs to restart first. After program is finished shutting down, please start Radium again."
                  ,
                  RADIUM_BLOCKSIZE, buffer_size);

      jack_client_close(_rjack_client);
      _rjack_client = NULL;
      
      return false;
      
    } else if((buffer_size % RADIUM_BLOCKSIZE) != 0) {
      
      GFX_Message(NULL, "Jack's blocksize of %d is not dividable by Radium's block size of %d. You will get bad sound. Adjust your audio settings.", buffer_size, RADIUM_BLOCKSIZE);

    }

    _sample_rate = jack_get_sample_rate(_rjack_client);
    
    g_jack_client = _rjack_client;

    g_jack_client_priority = jack_client_real_time_priority(_rjack_client);

    if(_sample_rate<100.0)
      GFX_Message(NULL, "Sample rate value is strange: %f",(float)_sample_rate);

    //if (_sample_rate < 30000)
    //  GFX_addMessage("Warning. Jack runs with a low sample rate (%d). Some effects may misbehave.\n", (int)_sample_rate);
    
    pc->pfreq = _sample_rate; // bang!

    jack_set_buffer_size_callback(_rjack_client,RT_rjack_buffer_size_changed,this);
    jack_set_freewheel_callback(_rjack_client, RT_rjack_freewheel_changed, this);
    jack_on_info_shutdown(_rjack_client, RT_rjack_shutdown, this);
    //jack_set_process_thread(_rjack_client,RT_rjack_thread,this);
    jack_set_process_callback(_rjack_client,RT_jack_process,this);
    jack_set_sync_callback(_rjack_client, RT_rjack_sync, this);
    jack_set_xrun_callback(_rjack_client, RT_rjack_xrun, this);
    
    if(isJackTimebaseMaster())
      MIXER_set_jack_timebase_master(true);
  
    if (jack_activate (_rjack_client)){
      fprintf (stderr, "Error. Cannot activate jack client.\n");

      ScopedQPointer<MyQMessageBox> msgBox(MyQMessageBox::create(true));
      msgBox->setIcon(QMessageBox::Critical);
      msgBox->setText("Failed to activate Jack client.");
      msgBox->setInformativeText("This is very unusual. You might want to run \"Kill Jack\", and then start jack again.");

      msgBox->setStandardButtons(QMessageBox::Ok);
      safeExec(msgBox, false);

      return false;
    }

    //jack_set_error_function(my_silent_jack_error_callback);

#ifdef FOR_WINDOWS // Noise from jack on windows when changing thread priority
    jack_set_info_function(my_silent_jack_error_callback);
#endif
    
    if (!nsmIsActive()) {
      for(int ch = 0 ; ch < NUM_SYSTEM_INPUT_JACK_PORTS ; ch++){
        _main_inputs[ch] = jack_port_register(_rjack_client, talloc_format("main_input_%d", ch+1), JACK_DEFAULT_AUDIO_TYPE, JackPortIsInput, 0);
        
        if (_main_inputs[ch]==NULL){
          
          ScopedQPointer<MyQMessageBox> msgBox(MyQMessageBox::create(true));
          msgBox->setIcon(QMessageBox::Critical);
          msgBox->setText("Failed to create main input jack port.");
          msgBox->setInformativeText("This is very unusual. You might want to run \"Kill Jack\", and then start jack again.");
          
          msgBox->setStandardButtons(QMessageBox::Ok);
          safeExec(msgBox, false);
          
          return false;          
        }

      }
      
      const char **outportnames = jack_get_ports(_rjack_client,NULL,NULL,JackPortIsPhysical|JackPortIsOutput);

      if (outportnames != NULL) {

        for(int portnum=0, ch=0 ; ch < NUM_SYSTEM_INPUT_JACK_PORTS && outportnames[portnum] != NULL ; portnum++){

          jack_port_t *physical_port = jack_port_by_name(_rjack_client, outportnames[portnum]);
          
          if (physical_port==NULL){
            
            R_ASSERT_NON_RELEASE(false);
            
          } else {

            if (g_audio_system_input_latency == 0){
              jack_latency_range_t range;
              jack_port_get_latency_range(physical_port, JackCaptureLatency, &range);
              g_audio_system_input_latency = R_MAX(0, (int)range.max);
            }

            const char *radium_port_name = jack_port_name(_main_inputs[ch]);

            if (radium_port_name==NULL){

              ScopedQPointer<MyQMessageBox> msgBox(MyQMessageBox::create(true));
              msgBox->setIcon(QMessageBox::Critical);
              msgBox->setText("Unable to lookup name of input jack port.");
              msgBox->setInformativeText("This is very unusual. You might want to run \"Kill Jack\", and then start jack again.");
              
              msgBox->setStandardButtons(QMessageBox::Ok);
              safeExec(msgBox, false);
          
              return false;          
              
            } else {
              
              if (!strcmp(jack_port_type(physical_port), jack_port_type(_main_inputs[ch]))) {
                if (0 != jack_connect(_rjack_client,
                                      outportnames[portnum],
                                      radium_port_name
                                      ))
                  {
                    GFX_addMessage("Warning. Could not connect to jack capture port %d: \"%s\".\n",ch,outportnames[portnum]);
                  }
                ch++;
              }
            }
            
          }
              
        }

        jack_free(outportnames);
      }

      set_jack_output_latency();
    }

    //create_jack_plugins(_rjack_client);

    return true;
  }

  DEFINE_ATOMIC(bool, _juce_audio_start_saving_soundfile) = false;
  DEFINE_ATOMIC(bool, _juce_audio_stop_saving_soundfile) = false;

  void juce_start_saving_soundfile_thread(void){

    auto thread = std::thread([this](){

#if defined(FOR_LINUX)
        pthread_setname_np(pthread_self(), "juce_start_saving_soundfile_thread");
#endif
        

        // Note: RT_process_audio_block sets player thread type at every call (just in case), so it's not necessary to set it here.
        
        MULTICORE_disable_RT_priority();
        {
          
          while(!ATOMIC_GET(_juce_audio_stop_saving_soundfile))
            RT_process_audio_block(g_soundcardblock_size);
          
        }
        MULTICORE_enable_RT_priority();
        
        ATOMIC_SET(_juce_audio_stop_saving_soundfile, false);
          
        R_ASSERT_NON_RELEASE(_is_saving_sound==true);
        
        _is_saving_sound = false;
        
      });
    
    thread.detach();
  }
    
  static bool juce_audio_device_callback(const int num_frames, void *data){
    Mixer *mixer = static_cast<Mixer*>(data);

    if (ATOMIC_GET(mixer->_juce_audio_start_saving_soundfile)){
      
      ATOMIC_SET(mixer->_juce_audio_start_saving_soundfile, false);
      
      mixer->_is_saving_sound = true;

      RSEMAPHORE_signal(g_saving_sound_has_started,1);
    }

    R_ASSERT_NON_RELEASE(!MIXER_dummy_driver_is_running());
          
    if (!mixer->_is_saving_sound)
      mixer->RT_process_audio_block(num_frames);
    
    return true;
  }

  bool start_juce_audio(void) {
    
    auto called_before_starting_audio = [this](int blocksize, float samplerate){
      printf("Block: %d. Sample rate: %f\n", blocksize, samplerate);
      _sample_rate = samplerate;
      pc->pfreq = _sample_rate;
    };
    
    if (JUCE_init_audio_device(juce_audio_device_callback, this, called_before_starting_audio)==false)
      return false;

#if !defined(RELEASE)
    {
      radium::PlayerLock lock;
      R_ASSERT(equal_floats(pc->pfreq,_sample_rate));
      R_ASSERT(equal_floats(_sample_rate, JUCE_audio_get_sample_rate()));
    }
#endif
    
    return true;
  }
  
  /*
  static int compare_sound_producers(const void *vsp1, const void *vsp2) {
    const SoundProducer **sp1 = (const SoundProducer**)(vsp1);
    const SoundProducer **sp2 = (const SoundProducer**)(vsp2);
    double dur1 = SP_get_running_time(*sp1);
    double dur2 = SP_get_running_time(*sp2);

    if (dur2<dur1)
      return -1;
    else if(dur2>dur1)
      return 1;
    else
      return 0;
  }
  */

  // Start the most cpu intensive soundproducers first
  void RT_sort_sound_producers_by_running_time(void){
#if 0    
    qsort(_sound_producers.elements, _sound_producers.size(), sizeof(SoundProducer*), compare_sound_producers); NO no no. Cant change the vector in this thread.
    printf("\n\n\n****************** START\n");
    for(int i=0;i<_num_sound_producers;i++){
      printf("%.4f\n", 1000.0*SP_get_running_time(_sound_producers[i]));
    }
#endif
  }


  radium::Time _excessive_time;

  void RT_before_processing_audio_block_not_holding_lock(void) {
    if (ATOMIC_GET(g_request_to_pause_plugins)==true){
      ATOMIC_SET(g_request_to_pause_plugins, false);
      g_process_plugins = false;
      pause_time.restart();
    }
      
    if (g_process_plugins==false) {
      if (pause_time.elapsed() > 5000)
        g_process_plugins = true;
    } else if (_is_saving_sound==false && _excessive_time.elapsed() > 2000) { // 2 seconds
      if (ATOMIC_GET(pc->player_state)==PLAYER_STATE_PLAYING) {
        RT_request_to_stop_playing();
        RT_message("Error!\n"
                   "\n"
                   "Audio using too much CPU. Stopping player to avoid locking up the computer.%s",
                   MULTICORE_get_num_threads()>1 ? "" : "\n\nTip: Turning on Multi CPU processing might help."
                   );
      } else {
        RT_message("Error!\n"
                   "\n"
                   "Audio using too much CPU. Pausing audio generation for 5 seconds to avoid locking up the computer.%s",
                   MULTICORE_get_num_threads()>1 ? "" : "\n\nTip: Turning on Multi CPU processing might help."
                   );
        printf("stop processing plugins\n");
        g_process_plugins = false; // Because the main thread waits very often waits for the audio thread, we can get very long breaks where nothing happens if the audio thread uses too much CPU.
      }
        
      pause_time.restart();
      _excessive_time.restart();
    }

    RT_AUDIOBUFFERS_optimize();
  }

#if 0
  int64_t _last_frame_time = 0;
#endif

#if !defined(RELEASE)
  int _num_visitors = 0;
  struct ScopedVisitors{
    int &_num_visitors;
    
    ScopedVisitors(int &num_visitors)
      : _num_visitors(num_visitors)
    {
      _num_visitors++;
    }
    ~ScopedVisitors(){
      _num_visitors--;
    }
  };
#endif

  // Instead, we just patched juce_linux_ALSA.cpp.
  //radium::SimpleHandShake _about_to_stop_audio_process;
  
  DEFINE_ATOMIC(bool, _RT_process_has_inited) = false;

  void RT_process_audio_block(int num_frames) {
#if !defined(RELEASE)
    ScopedVisitors scoped_visitors(_num_visitors);
    if (_num_visitors != 1)
      abort();
#endif
    
#ifdef MEMORY_DEBUG
    debug_wait.wait(&debug_mutex, 1000*10); // Speed up valgrind
#endif

    R_ASSERT_NON_RELEASE(num_frames >= RADIUM_BLOCKSIZE);

#if 0 //!defined(FOR_MACOSX)
    if (!_is_saving_sound){
      R_ASSERT_NON_RELEASE(THREADING_has_player_thread_priority());
    }
#endif

    /*
    _about_to_stop_audio_process.B_check_signal_from_A_and_signal_back_immediately();
      
    if (_about_to_stop_audio_process.B_has_shaken())
      THREADING_acquire_player_thread_priority();
    */
    
    if (ATOMIC_GET_RELAXED(_RT_process_has_inited)==false || THREADING_init_player_thread_type()){
      RADIUM_AVOIDDENORMALS;
      
      THREADING_init_player_thread_type(); // This is a light operation. Just call it again in case "_RT_process_has_inited" was false above.
      R_ASSERT(THREADING_is_player_thread());
      R_ASSERT_NON_RELEASE(!THREADING_is_main_thread());
      
      ATOMIC_SET(_RT_process_has_inited, true);
    }

    RT_before_processing_audio_block_not_holding_lock();

    PlayerLock_Local player_lock;

    // Make sure num_frames is legal. Seen some weird memory corruption errors when it's not. (note that the user should already have seen an error if num_frames is illegal)
    //if (num_frames >= RADIUM_BLOCKSIZE && (num_frames % RADIUM_BLOCKSIZE) != 0)
    //  num_frames = RADIUM_BLOCKSIZE;

    if (g_soundcardblock_size != num_frames) {
      ATOMIC_SET(g_soundcardblock_size, num_frames);
      g_soundcardblock_size = num_frames;
    }
    //_sample_rate = samplerate;
    //pc->pfreq = samplerate;
    
#if 0
    int64_t now_frame_time = (int64_t)jack_last_frame_time(_rjack_client);
    //printf("Frame time: %" PRId64 " (dx: %d)\n", now_frame_time, (int)(now_frame_time - _last_frame_time));
    _last_frame_time = now_frame_time;
#endif

    //printf("So far: %f\n", RT_TIME_get_ms() - JUCE_audio_time_at_cycle_start());
    
    audioblock_variables_protector.write_start();{
        
      struct SeqTrack *seqtrack = RT_get_curr_seqtrack();
      struct SeqBlock *curr_seqblock = seqtrack==NULL ? NULL : seqtrack->curr_seqblock;
        
      //R_ASSERT_NON_RELEASE((num_frames % RADIUM_BLOCKSIZE) == 0);
        
      ATOMIC_SET(g_soundcardblock_size2, g_soundcardblock_size);
        
      if (seqtrack!=NULL)
        ATOMIC_SET(audioblock_cycle_start_time, seqtrack->end_time);
      else
        ATOMIC_SET(audioblock_cycle_start_time, 0);
        
      if (_rjack_client != NULL)
        ATOMIC_SET(audioblock_last_frame_stime, jack_last_frame_time(_rjack_client));
      else
        ATOMIC_DOUBLE_SET(audioblock_cycle_start_ms, JUCE_audio_time_at_cycle_start());
        
      if (curr_seqblock != NULL && curr_seqblock->block!=NULL) {
        ATOMIC_SET(audioblock_seqtime, curr_seqblock->t.time);
        ATOMIC_SET(audioblock_block, curr_seqblock->block);
      } else {
        ATOMIC_SET(audioblock_seqtime, 0);
        ATOMIC_SET(audioblock_block, NULL);
      }
        
      ATOMIC_DOUBLE_SET(audioblock_song_tempo_multiplier, ATOMIC_DOUBLE_GET(g_curr_song_tempo_automation_tempo));
        
    }audioblock_variables_protector.write_end();
      
      
    if(g_test_crashreporter_in_audio_thread){
      //R_ASSERT(false);
      int *ai2=NULL;
      ai2[0] = 50;
    }
      
    //audioblock_size = num_frames;

    // Process sound.

    double start_time = RT_TIME_get_ms();

    if (MULTICORE_get_num_threads() > 1)
      RT_sort_sound_producers_by_running_time();

    //printf("\n\nNew:\n");

    for (SoundProducer *sp : _sound_producers)
      SP_RT_called_for_each_soundcard_block1(sp, _time);
      
    for (SoundProducer *sp : _sound_producers)
      SP_RT_called_for_each_soundcard_block2(sp, _time);

    bool can_not_start_playing_right_now_because_jack_transport_is_not_ready_yet = false;

    if (g_jack_client!=NULL && useJackTransport()){
        
      jack_transport_state_t state = jack_transport_query(g_jack_client,NULL);
        
      if (state == JackTransportStarting){

        R_ASSERT(_radium_transport_state!=RadiumTransportState::NO_STATE);
                   
        if (_radium_transport_state!=RadiumTransportState::PLAY_REQUEST_FAILED)
          can_not_start_playing_right_now_because_jack_transport_is_not_ready_yet = true;
          
      }
        
#if 0
      // When debugging jack transport handling
      {
        static int downcount = 0;
        static jack_transport_state_t last_state = (jack_transport_state_t)-1;
        static jack_nframes_t frame0time = -1894944768; // Must be a constant in order to compare values of several simultaneous running radium instances.
        jack_position_t pos;
        jack_transport_state_t state = jack_transport_query(_rjack_client,&pos);
        if (state != last_state){
          downcount = 5;
          last_state = state;
        }
        if (downcount > 0 || state==JackTransportStarting){
          printf("** %d: %d - %s. seq: %d, jack: %d\n", (int)pos.frame, can_not_start_playing_right_now_because_jack_transport_is_not_ready_yet, state==JackTransportStarting ? "Starting" : state==JackTransportRolling ? "Rolling" : "Stopped", (int)ATOMIC_DOUBLE_GET(pc->song_abstime), (int)(jack_last_frame_time(_rjack_client) - frame0time));
          downcount--;
        }
      }
#endif
        
      if (_radium_transport_state == RadiumTransportState::PLAYER_IS_READY && !can_not_start_playing_right_now_because_jack_transport_is_not_ready_yet){
        //R_ASSERT(state==JackTransportRolling); Guess it could be JackTransportStopped too.
        R_ASSERT(state!=JackTransportStarting);
        _radium_transport_state = RadiumTransportState::NO_STATE;
      }
    }
      
    static float max_playertask_audio_cycle_fraction = 0.01;

    MULTICORE_start_block(); {

      g_soundcardblock_delta_time = 0;
      while(g_soundcardblock_delta_time < num_frames){
            
        double curr_song_tempo_automation_tempo = pc->playtype==PLAYSONG ? RT_TEMPOAUTOMATION_get_value(ATOMIC_DOUBLE_GET(pc->song_abstime)) : 1.0;
        ATOMIC_DOUBLE_SET(g_curr_song_tempo_automation_tempo, curr_song_tempo_automation_tempo);

        PlayerTask((double)RADIUM_BLOCKSIZE * curr_song_tempo_automation_tempo, can_not_start_playing_right_now_because_jack_transport_is_not_ready_yet, max_playertask_audio_cycle_fraction);
            
        if (is_playing()) {
          if (pc->playtype==PLAYBLOCK) {

            RT_LPB_set_beat_position(root->song->block_seqtrack, RADIUM_BLOCKSIZE);

          } else { 

            VECTOR_FOR_EACH(struct SeqTrack *, seqtrack, &root->song->seqtracks){
              RT_LPB_set_beat_position(seqtrack, RADIUM_BLOCKSIZE);
            }END_VECTOR_FOR_EACH;
            
          }
        }

            
        RT_MIDI_handle_play_buffer();

        RT_MODULATOR_process(); // Important that the modulators run after handling midi so MIDI-learned parameters won't be delayed by a block.

        float start_time;

        if (g_soundcardblock_delta_time==0) start_time = MIXER_get_curr_audio_block_cycle_fraction(); else start_time = 0; // else clause added to silence compiler warning.

        if (g_soundcardblock_delta_time + RADIUM_BLOCKSIZE <= num_frames){
          // Note that the user should already have seen an error if num_frames is illegal.
          MULTICORE_run_all(_sound_producers, _time, RADIUM_BLOCKSIZE, g_process_plugins);
        }
        
        if (g_soundcardblock_delta_time==0){
          float curr_audio_fraction = MIXER_get_curr_audio_block_cycle_fraction() - start_time;
          curr_audio_fraction *= (num_frames / RADIUM_BLOCKSIZE);
          max_playertask_audio_cycle_fraction = (1.0 - curr_audio_fraction) / 2;
          //printf("   max fract: %f\n", max_playertask_audio_cycle_fraction);
        }
            
        //static int bufs = 0; printf("Buf: %d\n", (int)bufs++);

        _time += RADIUM_BLOCKSIZE;
        g_soundcardblock_delta_time += RADIUM_BLOCKSIZE;
      }

    } MULTICORE_end_block();
        
    ATOMIC_SET(g_last_mixer_time, _time);

    double process_duration = RT_TIME_get_ms() - start_time;

    // CPU usage
    {
      float new_cpu_usage = process_duration * 0.1 *_sample_rate / num_frames;
      
      g_cpu_usage.addUsage(new_cpu_usage);
      
      // Uncomment these two lines to continually test realtime message.
      if (new_cpu_usage < 98)
        _excessive_time.restart();
    }

    //printf("        FRAMES: %d (duration: %f). How much: %f\n", jack_time_to_frames(g_jack_client, process_duration), (double)process_duration/1000.0, MIXER_get_curr_audio_block_cycle_fraction());
    
    // Call this one right before releasing lock and waiting for cycle. This function is allowed to run almost to the end of the jack cycle.
    RT_call_functions_scheduled_to_run_on_player_thread();

    /*
    if (_about_to_stop_audio_process.B_has_shaken())
      THREADING_drop_player_thread_priority();
    */
  }

  DEFINE_ATOMIC(bool, _dummy_driver_is_running) = false;
  std::thread _dummy_driver_thread;

  // Note: Might be called from the JUCE message thread or from the jack shutdown thread.
  void start_dummy_driver(void){
    if (ATOMIC_SET_RETURN_OLD(_dummy_driver_is_running, true)==true) {
      R_ASSERT_NON_RELEASE(false);
      return;
    }
    
    _dummy_driver_thread = std::thread([this](){

#if defined(FOR_LINUX)
        pthread_setname_np(pthread_self(), "dummy_audio_driver");
#endif
        
        int blocksize = RADIUM_BLOCKSIZE;
        while(blocksize < 2048)
          blocksize += RADIUM_BLOCKSIZE;

        THREADING_acquire_player_thread_priority();
        
        while(ATOMIC_GET(_dummy_driver_is_running)){
          RT_process_audio_block(blocksize);
          QThread::msleep(frames_to_ms(blocksize));
        }

        THREADING_drop_player_thread_priority();
        
      });
  }

  // Note: Might be called from the JUCE message thread or from the jack shutdown thread.
  void stop_dummy_driver(void){
    if (ATOMIC_SET_RETURN_OLD(_dummy_driver_is_running, false)==false) {      
      R_ASSERT_NON_RELEASE(false);
      return;
    }

    _dummy_driver_thread.join();
  }

  // Note: Might be called from JUCE message thread.
  bool dummy_driver_is_running(void){
    return ATOMIC_GET(_dummy_driver_is_running);
  }


  static int RT_jack_process(jack_nframes_t nframes, void *arg){
    Mixer *mixer = static_cast<Mixer*>(arg);
    if (!MIXER_dummy_driver_is_running())
      mixer->RT_process_audio_block(nframes);
    return 0;
  }
  
  static void RT_rjack_freewheel_changed(int starting, void *arg){
    Mixer *mixer = static_cast<Mixer*>(arg);

    if(starting!=0){
      
      MULTICORE_disable_RT_priority();
      
      mixer->_is_saving_sound = true;
      RSEMAPHORE_signal(g_saving_sound_has_started,1);
      //SOUNDFILESAVER_start();
      
    }else{

      MULTICORE_enable_RT_priority();
      
      printf("MIXER: Saving sound stopped\n");
      mixer->_is_saving_sound = false;
      //SOUNDFILESAVER_stop();
      
    }
  }

  static void RT_rjack_shutdown(jack_status_t code, const char *reason, void *arg){
    if (!MIXER_dummy_driver_is_running())
      MIXER_start_dummy_driver();
    
    RT_message("The jack server shut down\n"
               "(Reason from the server: \"%s\").\n"
               "\n"
               "To continue working:\n"
               "1. Save.\n"
               "2. Exit Radium.\n"
               "3. Start Jack again.\n"
               "4. Start Radium again.\n",
               reason
               );
  }
  
  static int RT_rjack_buffer_size_changed(jack_nframes_t num_frames2, void *arg){

    int num_frames = num_frames2;
    
    //Mixer *mixer = static_cast<Mixer*>(arg);

    //const char *main_message = "Error: Radium does not officially support changing jack block size during runtime. It might work, it might not work. You should save your song and restart Radium to avoid undefined behavior.";

    if (num_frames < RADIUM_BLOCKSIZE){
      
       RT_message("jack's blocksize (%d) can not be smaller than Radium's internal block size of %d.", num_frames, RADIUM_BLOCKSIZE);
       if (!MIXER_dummy_driver_is_running())
         MIXER_start_dummy_driver();

    } else {

      if (MIXER_dummy_driver_is_running())
         MIXER_stop_dummy_driver();

      if ((num_frames % RADIUM_BLOCKSIZE) != 0)
        RT_message("jack's blocksize of %d is not dividable by Radium's block size of %d. You will get bad sound. Adjust your audio settings.", num_frames, RADIUM_BLOCKSIZE);
    }

    
    //else
    //  RT_message("%s", main_message);

    return 0;
  }

  static int RT_rjack_xrun(void *arg){
    ATOMIC_ADD(g_num_jack_xruns, 1);
    return 0;
  }
  
  static int RT_rjack_sync(jack_transport_state_t state, jack_position_t *pos, void *arg){
    Mixer *mixer = static_cast<Mixer*>(arg);
    
    if(state==JackTransportStopped){
      //printf("=================== trans: Stopped\n");
    
    } else if (state==JackTransportStarting){
      
      if (useJackTransport()==false)
        return 1;

      int64_t absabstime = (int64_t)pos->frame; // song position frame number is called absabstime in radium.
      
      Player_State state = ATOMIC_GET(pc->player_state);
        
      if(state==PLAYER_STATE_STARTING_TO_PLAY && pc->absabstime==absabstime) {
        //printf(" ************************   trans: Starting. frame: %d, abstime: %f, is playing: %d.\n", (int)absabstime, TEMPOAUTOMATION_get_abstime_from_absabstime(absabstime), is_playing());
        mixer->_radium_transport_state = RadiumTransportState::PLAYER_IS_READY;
        //printf("=================== trans: Starting %f. State: IS_READY. Return 1.\n", (double)pos->frame/44100.0);
        return 1;
      }
      
      if(mixer->_radium_transport_state != RadiumTransportState::WAITING_FOR_PLAYER_TO_BE_READY){

        if (mixer->_radium_transport_state != RadiumTransportState::PLAY_REQUEST_FAILED || RT_jack_transport_play_request_is_finished()){
          //printf(" ************************   trans: Requesting start. frame: %d, is playing: %d.\n", (int)absabstime, is_playing());
          RT_request_from_jack_transport_to_play(absabstime); // (pos->frame is unsigned, so no need to ensure that pos->frame>=0)
        } else{
          //printf(" ************************   trans: Has already requested start. frame: %d, is playing: %d.\n", (int)absabstime, is_playing());
        }
        
        mixer->_radium_transport_state = RadiumTransportState::WAITING_FOR_PLAYER_TO_BE_READY;
        //printf("=================== trans: Starting %f. State: Waiting. Return 0\n", (double)pos->frame/44100.0);
        return 0;
      }

      if (state==PLAYER_STATE_STARTING_TO_PLAY || RT_jack_transport_play_request_is_finished()){
        //
        // We have already requested to play song, but the player state is wrong. (could be many reasons for this)
        //
        // Now we need to set _radium_transport_state to something other than WAITING_FOR_PLAYER_TO_BE_READY, to avoid deadlock.
        //
        // Next time this function is called, we will do a new request.
        //
        // We can not call RT_request_to_start_playing_song directly, now, since _radium_transport_state != WAITING_FOR_PLAYER_TO_BE_READY.
        // If _radium_transport_state != WAITING_FOR_PLAYER_TO_BE_READY, we could start playing before returning 1 from this function,
        // i.e. that we start playing before all other clients. (we could work around this by adding another state, and so forth, but it's proabably not worth it)
        
        //printf(" ************************   trans: Failed start. Waiting. frame: %d, is playing: %d.\n", (int)absabstime, is_playing());
        mixer->_radium_transport_state = RadiumTransportState::PLAY_REQUEST_FAILED;
        //printf("=================== trans: Starting %f. State: Failed. Return 0\n", (double)pos->frame/44100.0);
        return 0;
      }

      //printf(" ************************   trans: Waiting... frame: %d, is playing: %d.\n", (int)absabstime, is_playing());
      //printf("=================== trans: Starting %f. State: ??? (%d). Return 0\n", (double)pos->frame/44100.0, (int)mixer->_radium_transport_state);
      return 0;
      
    }else if (state==JackTransportRolling) {
      //printf("=================== trans: Rolling %f\n", (double)pos->frame/44100.0);
      //printf("    trans: Rolling\n");
    
    } else {
      //printf("=================== trans: ???\n");
      //printf("    trans: ?? %d\n", (int)state);
    }
    
    return 1;
  }

  static void RT_rjack_timebase(jack_transport_state_t state, jack_nframes_t nframes, jack_position_t *pos, int new_pos, void *arg){
    /*
      int32_t   bar
      int32_t 	beat
      int32_t 	tick
      double 	bar_start_tick
      float 	beats_per_bar
      float 	beat_type
      double 	ticks_per_beat
      double 	beats_per_minute
    */

    bool isplaying = is_playing();
          
    pos->valid = JackPositionBBT;

    const struct SeqTrack *seqtrack;

    if (pc->playtype==PLAYBLOCK)
      seqtrack = root->song->block_seqtrack;
    else
      seqtrack = (struct SeqTrack *)root->song->seqtracks.elements[0];
    
    const int ticks_per_beat = 1920*16;

    if (!isplaying) {

      pos->bar = 1;
      pos->beat = 1;

      pos->bar_start_tick = 0;
      pos->tick = 0;

    } else {

      const bool using_sequencer_timing = root->song->use_sequencer_tempos_and_signatures;
      
      // 1. Set bar start tick.
      /////////////////////////////////
      
      // Note: If changing this line, might also have to change ppqPositionOfLastBarStart in Juce_plugins.c. (note that we don't subtract "latency_beats" here.)
      pos->bar_start_tick = (using_sequencer_timing ? g_rt_sequencer_ppq_of_last_bar_start : seqtrack->beat_iterator.beat_position_of_last_bar_start) * ticks_per_beat;

      
      // 2. Set tick.
      ///////////////////
      
      const double beatpos = RT_LPB_get_beat_position(seqtrack);
      const double beats_since_beat_start = beatpos - floor(beatpos);
    
      pos->tick           =  ticks_per_beat * beats_since_beat_start;


      // 3. Set bar and beat.
      ///////////////////////////////////
      
      pos->beat = g_rt_beatnum;
      pos->bar = g_rt_barnum;

    }
    

    // 4. Set signature
    ///////////////////////////////////
    
    //printf("bar_tick: %f. beat_tick: %f\n", (float)pos->bar_start_tick / (float)ticks_per_beat, (float)pos->tick/(float)ticks_per_beat);
      
    StaticRatio signature = RT_Signature_get_current_Signature(seqtrack);
    pos->beats_per_bar = signature.numerator;
    pos->beat_type = signature.denominator;


    // 5. Set ticks per beat
    ///////////////////////////////////
    

    pos->ticks_per_beat = ticks_per_beat;
    

    
    // 6. Set BPM
    ///////////////////////////////////
    

    pos->beats_per_minute = RT_LPB_get_current_BPM(seqtrack);

    
    /* The documentation says that we are encouraged to fill out the field bbt_offset. And here's the documentation for it:

       "frame offset for the BBT fields (the given bar, beat, and tick values actually refer to a time frame_offset frames before the start of the cycle), should be assumed to be 0 if JackBBTFrameOffset is not set. If JackBBTFrameOffset is set and this value is zero, the BBT time refers to the first frame of this cycle. If the value is positive, the BBT time refers to a frame that many frames before the start of the cycle."

      So... uh...
     */
    //pos->bbt_offset = ???;
  }
};
}
    

#if 1 //defined(RELEASE)
static void maybe_warn_about_jack1(void){

  const char *config_name = "show_jack1_warning_during_startup";
  
  if (SETTINGS_read_bool(config_name, true)==false)
    return;
    
  bool ok = true;

  const char *version_string = WJACK_get_version_string();
  
  if (version_string==NULL) {
    
    ok = false;

  } else {
    
    auto splitted = QString(version_string).split(".");
    
    if (splitted.size() != 3){
      
      ok = false;
      
    } else {

      bool ok1 = true, ok2 = true, ok3 = true;
      int major = splitted[0].toInt(&ok1);
      int minor = splitted[1].toInt(&ok2);
      splitted[2].toInt(&ok3);
      
      if (!ok1 || !ok2 || !ok3 || major < 1 || (major==1 && minor < 9)){
        
        ok = false;
        
      }
    }
  }


  if (!ok) {

    vector_t v = {};
    
    VECTOR_push_back(&v,"Ok");
    int hide = VECTOR_push_back(&v,"Don't show this message again");

    int ret = GFX_Message(&v, "Warning: Jack 1, or an old version of Jack 2, detected. Radium does not work very well with Jack 1. At minimum, you need to provide the option \"-Z\". If not, Radium will freeze now and then.");

    if (ret == hide)
      SETTINGS_write_bool(config_name, false);

  }
}
#endif

void MIXER_start_dummy_driver(void){
  g_mixer->start_dummy_driver();
}

void MIXER_stop_dummy_driver(void){
  g_mixer->stop_dummy_driver();
}

bool MIXER_dummy_driver_is_running(void){
  if (g_mixer==NULL)
    return false;
  else
    return g_mixer->dummy_driver_is_running();
}

bool MIXER_start(void){
  
  R_ASSERT(THREADING_is_main_thread());

#if 1 //defined(RELEASE)

  if (have_libjack()==0) {
    
    maybe_warn_about_jack1(); 
    
    if (KILLJACKD_kill_jackd_if_unresponsive()==true){
      return false;
    }
  }
  
#endif
  
  // Read a couple of settings variables from disk, so we don't read from disk in the realtime threads.
  useJackTransport();

  AUDIOBUFFERS_init();
  
  SampleRecorder_Init();
    
  g_saving_sound_has_started = RSEMAPHORE_create(0);
  g_player_stopped_semaphore = RSEMAPHORE_create(0);

  g_mixer = new Mixer();  
  
  if(g_mixer->start_jack()==false)
    if (g_mixer->start_juce_audio()==false) {

      ScopedQPointer<MyQMessageBox> msgBox(MyQMessageBox::create(true));
      msgBox->setIcon(QMessageBox::Critical);
      msgBox->setText("Unable to start audio driver. Configure audio under <b>Edit -> Soundcard preferences</b>");
      msgBox->setStandardButtons(QMessageBox::Ok);
      safeExec(msgBox, false);

      g_mixer->_sample_rate = JUCE_audio_get_sample_rate();
      if (g_mixer->_sample_rate < 500 || g_mixer->_sample_rate > 500000)
        g_mixer->_sample_rate = 44100;
      pc->pfreq = g_mixer->_sample_rate;
      
      MIXER_start_dummy_driver();

      //return false;
    }


  while(ATOMIC_GET(g_mixer->_RT_process_has_inited)==false)
    msleep(50);
    
  PR_init_plugin_types();

  //Sleep(3000);

  if (have_libjack()==0 && g_jack_client!=NULL)
    check_jackd_arguments();

  // Multicore is initialized after starting jack, since the "runners" call THREADING_acquire_player_thread_priority in the constructor, which don't work before jack has started.
  MULTICORE_init();

  // Read in autobypass configuration from settings file
  autobypassEnabled();
  getAutoBypassDelay();
  
  return true;
}

void MIXER_stop(void){
  static bool has_been_called = false;
  
  //  R_ASSERT(g_mixer->_rjack_client != NULL);
  R_ASSERT(has_been_called==false);

  SampleRecorder_shut_down();

  fprintf(stderr,"            MIXER STOP 2\n");

  if (g_mixer->_rjack_client != NULL) {
    
    jack_client_close(g_mixer->_rjack_client);
    
  } else {

    /*
    if (!g_mixer->_about_to_stop_audio_process.A_signal_B_and_wait(2000)){ // 2s timeout
#if !defined(RELEASE)
      abort();
#endif
    }
    */
      
  }
  
  JUCE_stop_audio_device();

  if (MIXER_dummy_driver_is_running())
    MIXER_stop_dummy_driver();
  
  has_been_called=true;
}

static STime g_startup_time = 0;
void OS_InitAudioTiming(void){
  g_startup_time = g_mixer->_time;
  //printf("OS_InitAudioTiming called. New time: %d\n",(int)g_startup_time);
}


int64_t MIXER_TRANSPORT_set_pos(double abstime){
  R_ASSERT_NON_RELEASE(g_jack_client!=NULL);
  
  if (g_jack_client==NULL)
    return 0;
  
  //printf("    MIXER_TRANSPORT_set_pos: %f\n", abstime/44100.0);
  
  int64_t absabstime = TEMPOAUTOMATION_get_absabstime(abstime);
  
  if (absabstime >= UINT32_MAX)
    
    RT_message("Can not seek that far when using Jack Transport. Jack time format is 32 bit only.");
  
  else if (absabstime < 0){
    
    RT_message("Jack transport: Can not seek to negative position\n");
    
  }else {

    int ret = jack_transport_locate(g_jack_client, (jack_nframes_t)absabstime);
    
    if (ret!=0){
      
      printf("   jack_transport_locate failed: %d\n",(int)absabstime);
      
    } else {

      if(jack_transport_query(g_jack_client,NULL) == JackTransportStopped){
    
        //printf("  Starting jack transport. Curr pos: %d\n", jack_get_current_transport_frame(g_jack_client));
        
        // Wait until the the jack transport location has changed or we time out. (the transport frame is not updated immediately when claling jack_transport_query)

        double start_time = TIME_get_ms();
        const int64_t max_time = 1000; // won't wait more than this (ms).
        
        while( (TIME_get_ms()-start_time) < max_time && jack_get_current_transport_frame(g_jack_client)!=absabstime)
          msleep(5);
        
      }

    }
    
  }

  return absabstime;
}

void MIXER_TRANSPORT_play(double abstime){
  R_ASSERT_NON_RELEASE(g_jack_client!=NULL);
  
  if (g_jack_client==NULL)
    return;

  MIXER_TRANSPORT_set_pos(abstime);
  
  if(jack_transport_query(g_jack_client,NULL) == JackTransportStopped){

    jack_transport_start(g_jack_client);

  }else{
    
    //printf("  We weren't in stopped. So not starting jack transport\n");
    
  }
}

void MIXER_TRANSPORT_stop(void){
  R_ASSERT_NON_RELEASE(g_jack_client!=NULL);
  
  if (g_jack_client==NULL)
    return;
  if (jack_transport_query(g_jack_client,NULL) != JackTransportStopped)
    jack_transport_stop(g_jack_client);
}

void MIXER_set_jack_timebase_master(bool doit){
  if (g_jack_client == NULL)
    return;
  
  jack_set_timebase_callback(g_jack_client, 0, doit ? Mixer::RT_rjack_timebase : NULL, g_mixer);
}

 
void MIXER_call_very_often(void){

  if (g_jack_client==NULL)
    return;

  static bool use_jack_transport = useJackTransport();
  
  if (is_called_every_ms(105)){
    use_jack_transport = useJackTransport();
  }
  
  if (use_jack_transport && is_called_every_ms(15)){

    bool isplaying = is_playing();

    jack_position_t pos;
    jack_transport_state_t state = jack_transport_query(g_jack_client,isplaying ? NULL : &pos);
          
    if(state==JackTransportStopped){
      if (isplaying && pc->playtype==PLAYSONG)
        PlayStop_from_jack_transport();
    }else if (state==JackTransportStarting){
    }else if (state==JackTransportRolling){
    }
    
    if (!isplaying) {

      int64_t absabstime = (int64_t)pos.frame;

      if (absabstime != pc->absabstime)
        PLAYER_set_song_pos(-1, absabstime, true, true);
    }
  }

}

bool MIXER_is_saving(void){
  if (g_mixer==NULL)
    return false;

  return g_mixer->_is_saving_sound;
}


void MIXER_set_all_non_realtime(bool is_non_realtime){
  const radium::Vector<SoundProducer*> &sp_all = MIXER_get_all_SoundProducers();
  for (SoundProducer *sp : sp_all) {
    SoundPlugin *plugin = SP_get_plugin(sp);
    if (plugin->type->set_non_realtime != NULL)
      plugin->type->set_non_realtime(plugin, is_non_realtime);
  }
}


void MIXER_start_saving_soundfile(void){
  EVENTLOG_add_event("MIXER_request_start_saving_soundfile Enter");
    
  RSEMAPHORE_reset(g_saving_sound_has_started); // Must do this in case a different jack client started freewheeling since last call to sem_init.
    
  if (g_jack_client==NULL) {

    ATOMIC_SET(g_mixer->_juce_audio_start_saving_soundfile, true);
               
  } else {
    
    EVENTLOG_add_event("MIXER_request_start_saving_soundfile Step 1");
    
    jack_set_freewheel(g_jack_client, 1);
    
    EVENTLOG_add_event("MIXER_request_start_saving_soundfile Step 2");
    
  }

  RSEMAPHORE_wait(g_saving_sound_has_started,1);

  if (g_jack_client==NULL)
    g_mixer->juce_start_saving_soundfile_thread();
  
  EVENTLOG_add_event("MIXER_request_start_saving_soundfile Leave");
}

void MIXER_request_stop_saving_soundfile(void){
  EVENTLOG_add_event("MIXER_request_stop_saving_soundfile Enter");
  
  if (g_jack_client==NULL) {

    ATOMIC_SET(g_mixer->_juce_audio_stop_saving_soundfile, true);
    
  } else {
    
    jack_set_freewheel(g_jack_client, 0);

  }

  EVENTLOG_add_event("MIXER_request_stop_saving_soundfile Leave");
  
  printf("REQUEST to stop saving received\n");
}

// dont work.
STime MIXER_get_block_delta_time(STime time){
  return (time+g_startup_time) - g_mixer->_time;
}

int MIXER_get_num_xruns(void){  
  if (g_mixer==NULL || MIXER_dummy_driver_is_running())
    return 0;
  
  if (g_jack_client==NULL)
    return JUCE_get_num_xruns();
  else
    return ATOMIC_GET(g_num_jack_xruns);
}

int MIXER_get_main_inputs(const float **audio, int max_num_ch){
  if (MIXER_dummy_driver_is_running()){
    return 0;
  }
  
  if (g_jack_client==NULL) {

    int ch_out = 0;
    
    for(int ch_in = 0 ; ch_in < R_MIN(max_num_ch, g_juce_num_input_audio_channels) ; ch_in++)
      if (g_juce_input_audio_channels[ch_in] != NULL)
        audio[ch_out++] = g_juce_input_audio_channels[ch_in] + g_soundcardblock_delta_time;
      
    return ch_out;
    
  } else {
  
    int num_ch = R_MIN(NUM_SYSTEM_INPUT_JACK_PORTS, max_num_ch);
    
    for(int i=0;i<num_ch;i++)
      audio[i] = ((float*)jack_port_get_buffer(g_mixer->_main_inputs[i],g_soundcardblock_size)) + g_soundcardblock_delta_time;
    
    return num_ch;
  }
}

/*
int64_t MIXER_get_time(void){
  return g_mixer->_time;
}
*/

// Returns a number between 0 and infinity.
// 0 = audio block cycle just started
// 1 = audio block cycle just ended.
// > ~1 = we will probably get xrun(s)
static float MIXER_get_curr_audio_block_cycle_fraction2(double cycle_start_ms, int soundcardblock_size){
  R_ASSERT_NON_RELEASE(g_jack_client==NULL);
  
  double duration = RT_TIME_get_ms() - cycle_start_ms;
  double block_duration = frames_to_ms(soundcardblock_size); //g_mixer->_buffer_size);
  float ret = duration / block_duration;
  return ret; //R_BOUNDARIES(0, ret, 1);
}

float MIXER_get_curr_audio_block_cycle_fraction(void){
#if !defined(RELEASE)
  if (!THREADING_is_player_thread())
    printf("MIXER_get_curr_audio_block_cycle_fraction: Warning, not called from player thread\n");
#endif

  if (MIXER_dummy_driver_is_running())
    return 0.1;
  
  else if (g_jack_client==NULL) {

    return MIXER_get_curr_audio_block_cycle_fraction2(JUCE_audio_time_at_cycle_start(), g_soundcardblock_size);
    
  } else {

    return (float)jack_frames_since_cycle_start(g_jack_client) / (float)g_soundcardblock_size;

  }
}

static int get_audioblock_time(STime jack_block_start_time){
  STime abs_jack_time = jack_frame_time(g_mixer->_rjack_client);

  return int(abs_jack_time - jack_block_start_time);
}

// Like seqtrack->start_time, but sub-block accurately. Can be called from any thread.
//
// I understand the function quite well when writing this comment, but I might not next time reading this code.
// Should probably think about how to abstract all this stuff.
static bool fill_in_time_position2(time_position_t *time_position){
  STime audioblock_cycle_start_time2;
  STime audioblock_last_frame_stime2 = 0; // Set to 0 to silence buggy error message in gcc 10.
  double audioblock_cycle_start_ms2;
  STime audioblock_size2;
  struct Blocks *block;
  STime seqtime;
  double song_tempo_multiplier;

  const bool use_jack = g_jack_client!=NULL; // Use local variable to tell the compiler that the value won't change. (if not, it might report that audioblock_cycle_start_ms2 might be used uninitialized)
  
  //int playlistpos;
  //int playlistpos_numfromcurrent = 0;

  int num_tries = 0;
  
  int generation;
  do{
    num_tries++;
    generation = audioblock_variables_protector.read_start();
    
    audioblock_cycle_start_time2 = ATOMIC_GET(audioblock_cycle_start_time);
    if (use_jack)
      audioblock_last_frame_stime2  = ATOMIC_GET(audioblock_last_frame_stime);
    else
      audioblock_cycle_start_ms2 = ATOMIC_DOUBLE_GET(audioblock_cycle_start_ms);
    audioblock_size2              = ATOMIC_GET(g_soundcardblock_size2);
    block                        = ATOMIC_GET(audioblock_block);
    seqtime                      = ATOMIC_GET(audioblock_seqtime);
    song_tempo_multiplier        = ATOMIC_DOUBLE_GET(audioblock_song_tempo_multiplier);
    //playlistpos                  = ATOMIC_GET(audioblock_playlistpos);
    
  } while(audioblock_variables_protector.read_end(generation)==false); // ensure that the variables inside this loop are read atomically.

  if (block==NULL)
    return false;
  
  STime deltatime;

  if (use_jack)
    deltatime = scale(get_audioblock_time(audioblock_last_frame_stime2),
                      0, audioblock_size2,
                      0, audioblock_size2 * ATOMIC_DOUBLE_GET(block->reltempo) * song_tempo_multiplier
                      );
  else
    deltatime = true ? 0 : scale(MIXER_get_curr_audio_block_cycle_fraction2(audioblock_cycle_start_ms2, audioblock_size2), // FIX, maybe. JUCE don't call callbacks regularly, so setting deltatime to 0 is less worse than the alternative.
                                 0, 1,
                                 0,audioblock_size2 * ATOMIC_DOUBLE_GET(block->reltempo) * song_tempo_multiplier
                                 );

#if 0
  float gakk;
  if (use_jack)
    gakk = ((float)get_audioblock_time(audioblock_last_frame_stime2) / (float)audioblock_size2);
  else
    gakk = MIXER_get_curr_audio_block_cycle_fraction2(audioblock_cycle_start_ms2, audioblock_size2);

  printf("%d: Deltatime: %d. Fraction: %f. num_tries: %d. blocksize: %d / %d\n",
         (int)audioblock_cycle_start_time2,
         (int)deltatime,
         gakk,
         //MIXER_get_curr_audio_block_cycle_fraction(),
         num_tries,
         (int)audioblock_size2, g_soundcardblock_size);
#endif
  
  STime accurate_radium_time = audioblock_cycle_start_time2 + deltatime;
  
  STime accurate_block_time = accurate_radium_time - seqtime;
#if !defined(RELEASE)
  if (accurate_block_time < 0){
    fprintf(stderr, "accurate_block_time: %d, accurate_radium_time: %d, seqtime: %d\n", (int)accurate_block_time, (int)accurate_radium_time, (int)seqtime);
    R_ASSERT(false);
  }
#endif

  while (accurate_block_time >= getBlockSTimeLength(block)){
#if 1 // fixme
    return false;
#else
    accurate_block_time -= getBlockSTimeLength(block);
    if(pc->playtype==PLAYSONG) {
      playlistpos_numfromcurrent++;
      block = BL_GetBlockFromPos(playlistpos + playlistpos_numfromcurrent);
      if (block==NULL)
        return false; // end of song or pause
    }
#endif
  }

#if !defined(RELEASE)
  R_ASSERT_RETURN_IF_FALSE2(accurate_block_time >= 0, false);
#else
  if (accurate_block_time < 0) // Something went wrong, but there's probably not much to learn in a crash report.
    return false;
#endif
  
  time_position->blocknum = (int16_t)block->l.num;
  time_position->blocktime = accurate_block_time;

  //printf("...............blocktime: %d\n", (int)accurate_block_time);
  return true;
}

// Can be called from any thread
bool MIXER_fill_in_time_position(time_position_t *time_position){
  if (root==NULL || root->song==NULL)
    return false;

  struct Tracker_Windows *window = root->song->tracker_windows;
  if (window==NULL)
    return false;
  
  R_ASSERT_RETURN_IF_FALSE2(ATOMIC_GET(root->song_state_is_locked), false);

  time_position->tracknum = (int16_t)ATOMIC_READ(window->curr_track);

  return fill_in_time_position2(time_position);
}

#if 0
void MIXER_get_buses(SoundProducer* &bus1, SoundProducer* &bus2){
  bus1 = g_mixer->_bus1;
  bus2 = g_mixer->_bus2;
}
#endif

Buses MIXER_get_buses(void){
  Buses ret = {
    g_mixer->_bus[0],
    g_mixer->_bus[1],
    g_mixer->_bus[2],
    g_mixer->_bus[3],
    g_mixer->_bus[4]
  };
  return ret;
}

#if 0
struct SoundProducer *MIXER_get_bus(int bus_num){
  //R_ASSERT(g_mixer->_bus1 != NULL);
  //R_ASSERT(g_mixer->_bus2 != NULL);
  
  if (bus_num==0)
    return g_mixer->_bus1;
  if (bus_num==1)
    return g_mixer->_bus2;

  R_ASSERT(false);
  return NULL;
}
#endif

void MIXER_add_SoundProducer(SoundProducer *sound_producer){
  g_mixer->add_SoundProducer(sound_producer);
}

// Called from ~SoundProducer
void MIXER_remove_SoundProducer(SoundProducer *sound_producer){
  g_mixer->remove_SoundProducer(sound_producer);
}
/*
const radium::Vector<SoundProducer*> *MIXER_get_all_SoundProducers(void){
  if (g_mixer==NULL)
    return NULL;
  else
    return &g_mixer->_sound_producers;
}
*/

static const radium::Vector<SoundProducer*> g_empty_sound_producers;

const radium::Vector<SoundProducer*> &MIXER_get_all_SoundProducers(void){
  if (g_mixer==NULL)
    return g_empty_sound_producers;
  else
    return g_mixer->_sound_producers;
}

struct Patch **RT_MIXER_get_all_click_patches(int *num_click_patches){
  int num = 0;
  
  for (int i=0 ; i<g_num_allocated_click_plugins ; i++){
    SoundPlugin *plugin = g_click_plugins[i];
    if (plugin != NULL && plugin->patch != NULL) {      
      g_click_patches[num] = (struct Patch*)plugin->patch;
      num++;
    }
  }

  //  if (g_num_click_plugins != num)
  //   printf("Error: g_num_click_plugins != num: %d != %d\n",g_num_click_plugins, num);
  
  *num_click_patches = num;

  return g_click_patches;
}

// Can be called from any thread.
float MIXER_get_sample_rate(void){
  return g_mixer->_sample_rate;
}

int64_t MIXER_get_recording_latency_compensation_from_system_in(void){
  if (getRecordingLatencyFromSystemInputIsAutomaticallyDetermined())
    return g_audio_system_input_latency + g_audio_system_output_latency;
  else
    return ms_to_frames(getCustomRecordingLatencyFromSystemInput());
}

static SoundProducer *get_main_system_out_soundproducer(void){
  struct Patch *patch = GFX_OS_get_system_out();
  if (patch==NULL)
    return NULL;

  if (patch->instrument != get_audio_instrument()){
    R_ASSERT(false);
    return NULL;
  }

  struct SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
  R_ASSERT_RETURN_IF_FALSE2(plugin!=NULL, NULL);
  
  return SP_get_sound_producer(plugin);
}

int64_t MIXER_get_latency_for_main_system_out(void){
  auto *soundproducer = get_main_system_out_soundproducer();
  
  if (soundproducer==NULL){
    R_ASSERT_NON_RELEASE(false);
    return 0;
  }else{
    radium::PlayerLock lock;
    return RT_SP_get_input_latency(soundproducer);
  }
}

bool MIXER_is_connected_to_system_out(const SoundProducer *sp){
  auto *main_system_out = get_main_system_out_soundproducer();  
  if (main_system_out==NULL)
    return false;

  return SP_is_audio_connected(sp, main_system_out);
}


int MIXER_get_remaining_num_audioblock_frames(void){
  R_ASSERT_NON_RELEASE(THREADING_is_player_or_runner_thread());
  return g_soundcardblock_size - g_soundcardblock_delta_time;
}


// Returns first sound plugin in mixer that matches type_name and name. name can be NULL.
struct SoundPlugin *MIXER_get_soundplugin(const char *type_name, const char *name){
  for (SoundProducer *sp : g_mixer->_sound_producers) {
    SoundPlugin *plugin = SP_get_plugin(sp);
    if (!strcmp(plugin->type->type_name,type_name)){
      if (name==NULL)
        return plugin;
      if(!strcmp(plugin->type->name,name))
        return plugin;
    }
  }

  return NULL;
}

struct Patch *MIXER_get_bus(int bus_num){
  R_ASSERT_RETURN_IF_FALSE2(bus_num>=0 && bus_num < 5, NULL);

  SoundProducer *producer = g_mixer->_bus[bus_num];
  if(producer==NULL)
    return NULL;

  struct SoundPlugin *plugin = SP_get_plugin(producer);
  
  return (struct Patch*)plugin->patch;
}


void MIXER_set_bus(int bus_num, SoundProducer *producer){
  R_ASSERT_NON_RELEASE(false);
  g_mixer->_bus[bus_num] = producer;
}


void MIXER_called_regularly_by_main_thread(void){
  // Not enabled. Enable in Qt_Main.cpp.
  if (g_mixer != NULL)
    for (SoundProducer *sp : g_mixer->_sound_producers)
      SP_called_regularly_by_main_thread(sp);
}

void MIXER_set_all_plugins_to_not_recording(void){
  for (SoundProducer *sp : g_mixer->_sound_producers)
    PLUGIN_set_all_effects_to_not_recording(SP_get_plugin(sp));
}


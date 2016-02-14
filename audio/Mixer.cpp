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

#include "../weakjack/weak_libjack.h"

#include <QMessageBox>
#include <QString>
#include <QStringList>
#include <QTime>
#include <QThread>


// I'm not entirely sure where memory barriers should be placed, so I've tried to be more safe than sorry.
//#include "pa_memorybarrier.h"

#include "../common/nsmtracker.h"
#include "../common/Mutex.hpp"
#include "../common/visual_proc.h"
#include "../common/player_proc.h"
#include "../common/playerclass.h"
#include "../common/Semaphores.hpp"
#include "../common/stacktoucher_proc.h"
#include "../common/OS_Player_proc.h"
#include "../common/threading.h"
#include "../common/PEQ_LPB_proc.h"
#include "../common/OS_visual_input.h"
#include "../midi/midi_i_input_proc.h"

#include "Jack_plugin_proc.h"
#include "SoundfileSaver_proc.h"
#include "get_windows_commandlines_proc.h"

#include "SoundProducer_proc.h"
#include "SoundPluginRegistry_proc.h"
#include "MultiCore_proc.h"

#include "Mixer_proc.h"

volatile bool g_test_crashreporter_in_audio_thread = false;

extern PlayerClass *pc;
extern int num_users_of_keyboard;

static int g_last_set_producer_buffersize;
static RSemaphore *g_freewheeling_has_started = NULL;

extern const char *g_click_name;

// these four variables can only be written to in the audio thread.
static volatile int g_num_allocated_click_plugins = 0;
static volatile int g_num_click_plugins = 0;
static SoundPlugin **g_click_plugins = NULL;
static Patch **g_click_patches = NULL; // only written to in RT_MIXER_get_all_click_patches.


#ifdef MEMORY_DEBUG

static radium::Mutex debug_mutex;
static radium::CondWait debug_wait;

void PLAYER_memory_debug_wake_up(void){
  debug_wait.notify_one();
}
#endif

DEFINE_ATOMIC(bool, g_currently_processing_dsp) = false;


jack_client_t *g_jack_client;
static int g_jack_client_priority;
DEFINE_ATOMIC(int, g_max_cpu_usage) = 0.0f;
DEFINE_ATOMIC(int, g_min_cpu_usage) = 0.0f;
DEFINE_ATOMIC(int, g_num_cpu_usage) = 0;
DEFINE_ATOMIC(int, g_avg_cpu_usage) = 0.0f;
static float g_total_cpu_usage = 0;

static DEFINE_ATOMIC(bool, g_jack_is_running) = true;

bool PLAYER_is_running(void){
  return ATOMIC_GET(g_jack_is_running);
}

void THREADING_acquire_player_thread_priority(void){
  static bool has_shown_warning = false;

  int err = jack_acquire_real_time_scheduling(GET_CURRENT_THREAD(), g_jack_client_priority);
  if (err != 0 && has_shown_warning==false) {
    has_shown_warning=true;
    RT_message("Unable to set real time priority. You might want to check your system configuration\n\nError code: %d.", err);
  }
}

static void PLAYER_acquire_same_priority(void){
  //printf("Setting real time priority temporarily for %p.\n",(void*)pthread_self());
  THREADING_acquire_player_thread_priority();
}	

void THREADING_drop_player_thread_priority(void){
  jack_drop_real_time_scheduling(GET_CURRENT_THREAD());
}

static void PLAYER_drop_same_priority(void){
  THREADING_drop_player_thread_priority();
}



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

    if(line.startsWith("jackd") || line.startsWith("jackd.exe") || line.contains("/jackd.exe ") || line.contains("\\jackd.exe ") || line.contains("/jackd.exe\"") || line.contains("\\jackd.exe\"")) {

      found_jack = true;

      QStringList elements = line.split(" ", QString::SkipEmptyParts);

      for(int i=0;i<elements.size();i++){
        QString element = elements.at(i);
        if(element=="-S")
          found_sync_flag=true;
      }
    }
  }

  if(found_jack==false){
    
    num_users_of_keyboard++;
    QMessageBox msgBox;
    msgBox.setIcon(QMessageBox::Warning);
    msgBox.setText("Unable to find jack process command line arguments.");
    msgBox.setInformativeText("Please make sure the -S flag was added to the jackd argument line. If not, glitches in sound will occur.\n ");
    msgBox.setInformativeText(mandatory);
    msgBox.setStandardButtons(QMessageBox::Ok);
    msgBox.exec();
    num_users_of_keyboard--;
    
  } else if(found_sync_flag==false){

    num_users_of_keyboard++;
    QMessageBox msgBox;
    msgBox.setIcon(QMessageBox::Critical);
    msgBox.setText("The -S parameter was not set for Jack.");
    msgBox.setInformativeText(mandatory);
    
    msgBox.setStandardButtons(QMessageBox::Ok);
    msgBox.exec();
    num_users_of_keyboard--;
  }


#endif
}

#if defined(FOR_LINUX) || defined(FOR_MACOSX)
static pthread_mutexattr_t player_lock_mutexattr;
#endif
static LockType player_lock;
static LockType player_runner_lock;

static __thread bool g_current_thread_has_player_lock = false;
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

static void lock_player(void){
  R_ASSERT(!PLAYER_current_thread_has_lock()); // the player lock is reentrant, just in case, but reentrancy is not supposed to be used.
  
  LOCK_LOCK(player_lock);
  g_current_thread_has_player_lock = true;
}

static void unlock_player(void){
  R_ASSERT_RETURN_IF_FALSE(PLAYER_current_thread_has_lock());
    
  g_current_thread_has_player_lock = false;
  LOCK_UNLOCK(player_lock);
}

static void RT_lock_player(){
  R_ASSERT(THREADING_is_player_thread());
  lock_player();
}

static void RT_unlock_player(){
  R_ASSERT(THREADING_is_player_thread());
  unlock_player();
}

#ifndef FOR_LINUX
static priority_t g_priority_used_inside_PLAYER_lock;
#endif

void PLAYER_lock(void){
  
  R_ASSERT(!THREADING_is_player_thread());

  
#ifdef FOR_LINUX // we use mutex with the PTHREAD_PRIO_INHERIT on linux
  
  lock_player();
  //print_backtrace();
  
#elif defined(FOR_WINDOWS) || defined(FOR_MACOSX)
  priority_t priority = THREADING_get_priority();
  
  PLAYER_acquire_same_priority();
  lock_player();

  g_priority_used_inside_PLAYER_lock = priority;
#else
  #error "undknown architehercu"
#endif
}

void PLAYER_unlock(void){
  R_ASSERT(!THREADING_is_player_thread());

#ifdef FOR_LINUX // we use mutex with the PTHREAD_PRIO_INHERIT on linux
  
    unlock_player();
    
#elif defined(FOR_WINDOWS) || defined(FOR_MACOSX)
  priority_t priority = g_priority_used_inside_PLAYER_lock;
  
  unlock_player();

  //PLAYER_drop_same_priority();
  THREADING_set_priority(priority);
#else
  #error "undknown architehercu"
#endif
}

void RT_PLAYER_runner_lock(void){
  LOCK_LOCK(player_runner_lock);
  g_current_thread_has_player_runner_lock = true;
}

void RT_PLAYER_runner_unlock(void){
  g_current_thread_has_player_runner_lock = false;
  LOCK_UNLOCK(player_runner_lock);
}

bool PLAYER_current_thread_has_lock(void){
  return g_current_thread_has_player_lock || g_current_thread_has_player_runner_lock;
}


static void init_player_lock(void){

  LOCK_INITIALIZE(player_runner_lock); // Don't have to do anything special. It's always called from a realtime thread, and never recursively.

#if defined(FOR_LINUX) || defined(FOR_MACOSX)
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

#elif defined(FOR_WINDOWS)
   LOCK_INITIALIZE(player_lock);
#else

#error "unkwndonw arcthinerture"
#endif
}

int jackblock_size = 0;
jack_time_t jackblock_delta_time = 0;
static STime jackblock_cycle_start_stime = 0;

static QTime pause_time;
static bool g_process_plugins = true;
static DEFINE_ATOMIC(bool, g_request_to_pause_plugins) = false;
  
void RT_pause_plugins(void){
  ATOMIC_SET(g_request_to_pause_plugins, true);
}

#if FOR_WINDOWS
#define USE_WORKAROUND 1
#else
#define USE_WORKAROUND 0
#endif

#if USE_WORKAROUND
static void start_workaround_thread(void);
#endif

#if FOR_WINDOWS
extern "C" {
  static void my_silent_jack_error_callback(const char *desc){
  }
}
#endif


struct Mixer{
  SoundProducer *_bus1;
  SoundProducer *_bus2;

  radium::Vector<SoundProducer*> _sound_producers;
  
  jack_client_t *_rjack_client;
  int64_t _last_time;
  int64_t _time;

  float _sample_rate;
  int _buffer_size;

  bool _is_freewheeling;

  Mixer()
    : _bus1(NULL)
    , _bus2(NULL)
    , _rjack_client(NULL)
    , _last_time(0)
    , _time(0)
    , _is_freewheeling(false)
  {
  }

  void add_SoundProducer(SoundProducer *sound_producer){
    SoundPlugin *plugin = SP_get_plugin(sound_producer);
    int bus_num = SP_get_bus_num(sound_producer);

    bool is_click_patch = false;
    SoundPlugin **new_g_click_plugins = NULL;
    Patch **new_g_click_patches = NULL;
    int new_num_allocated_click_plugins = g_num_allocated_click_plugins;
    
    if (!strcmp(plugin->type->type_name,"Sample Player")) {
      if(!strcmp(plugin->type->name,g_click_name)) {
        if (g_num_allocated_click_plugins >= g_num_click_plugins) {
          if (g_num_allocated_click_plugins<=0)
            new_num_allocated_click_plugins = 16;
          else
            new_num_allocated_click_plugins = g_num_allocated_click_plugins * 2;
          new_g_click_plugins = (SoundPlugin **)V_calloc(sizeof(SoundPlugin*), new_num_allocated_click_plugins);
          new_g_click_patches = (Patch **)V_malloc(sizeof(Patch*)*new_num_allocated_click_plugins);
        }
        is_click_patch = true;
      }
    }
    
    PLAYER_lock();{

      if(bus_num==0)
        _bus1 = sound_producer;
      if(bus_num==1)
        _bus2 = sound_producer;

      if (is_click_patch) {
        if (new_g_click_plugins != NULL) {
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
      _sound_producers.add(sound_producer);
      
    }PLAYER_unlock();
  }
    
  void remove_SoundProducer(SoundProducer *sound_producer){
    SoundPlugin *plugin = SP_get_plugin(sound_producer);
    
    bool is_click_patch = false;

    if (!strcmp(plugin->type->type_name,"Sample Player"))
      if(!strcmp(plugin->type->name,g_click_name))
        is_click_patch = true;

    
    PLAYER_lock();{
      _sound_producers.remove(sound_producer);
      
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

      if (sound_producer==_bus1)
        _bus1=NULL;
      if (sound_producer==_bus2)
        _bus2=NULL;
      
    }PLAYER_unlock();
  }

  bool start_jack(){
    jack_status_t status;

    _rjack_client=jack_client_open("radium_audio",JackNoStartServer,&status,NULL);

    if (_rjack_client == NULL) {
      fprintf (stderr, "jack_client_open() failed, "
	       "status = 0x%2.0x\n", status);
      if (status & JackServerFailed) {
	fprintf (stderr, "Unable to connect to JACK server\n");
      }

      num_users_of_keyboard++;
      QMessageBox msgBox;
      msgBox.setIcon(QMessageBox::Critical);
      msgBox.setText("Unable to connect to Jack.");
      msgBox.setInformativeText("The Jack Audio Connection Kit server must be started before running Radium. "
                                "\n\n"
#if defined(FOR_MACOSX)
                                "Download Jack from http://www.jackosx.com"
#endif

#if defined(__linux__)
                                "Download Jack from http://www.jackaudio.org"
#endif

#if defined(FOR_WINDOWS)
                                "Please read the file README_first.txt"
#endif
				);
      
      msgBox.setStandardButtons(QMessageBox::Ok);
      msgBox.exec();
      num_users_of_keyboard--;
      return false;
    }

    _sample_rate = jack_get_sample_rate(_rjack_client);
    _buffer_size = jack_get_buffer_size(_rjack_client);
    if(_buffer_size < RADIUM_BLOCKSIZE)
      GFX_Message(NULL, "Jack's blocksize of %d is less than Radium's block size of %d. You will get bad sound. Adjust your audio settings.", _buffer_size, RADIUM_BLOCKSIZE);
    else if((_buffer_size % RADIUM_BLOCKSIZE) != 0)
      GFX_Message(NULL, "Jack's blocksize of %d is not dividable by Radium's block size of %d. You will get bad sound. Adjust your audio settings.", _buffer_size, RADIUM_BLOCKSIZE);

    g_last_set_producer_buffersize = _buffer_size;
    g_jack_client_priority = jack_client_real_time_priority(_rjack_client);

    if(_sample_rate<100.0)
      GFX_Message(NULL, "Sample rate value is strange: %f",(float)_sample_rate);

    pc->pfreq = _sample_rate; // bang!

    jack_set_buffer_size_callback(_rjack_client,RT_rjack_buffer_size_changed,this);
    jack_set_freewheel_callback(_rjack_client, RT_rjack_freewheel_changed, this);
    jack_on_info_shutdown(_rjack_client, RT_rjack_shutdown, this);
    jack_set_process_thread(_rjack_client,RT_rjack_thread,this);

    if (jack_activate (_rjack_client)){
      fprintf (stderr, "Error. Cannot activate jack client.\n");

      num_users_of_keyboard++;
      QMessageBox msgBox;
      msgBox.setIcon(QMessageBox::Critical);
      msgBox.setText("Unable to activate Jack client.");
      msgBox.setInformativeText("This is very unusual. Try restarting Jack.");

      msgBox.setStandardButtons(QMessageBox::Ok);
      msgBox.exec();
      num_users_of_keyboard--;

      return false;
    }

    //jack_set_error_function(my_silent_jack_error_callback);

#if FOR_WINDOWS // Noise from jack on windows when changing thread priority
    jack_set_info_function(my_silent_jack_error_callback);
#endif
    
    g_jack_client = _rjack_client;
    //create_jack_plugins(_rjack_client);

    return true;
  }

  static int compare_sound_producers(const void *vsp1, const void *vsp2){
    SoundProducer **sp1 = (SoundProducer**)(vsp1);
    SoundProducer **sp2 = (SoundProducer**)(vsp2);
    double dur1 = SP_get_running_time(*sp1);
    double dur2 = SP_get_running_time(*sp2);

    if (dur2<dur1)
      return -1;
    else if(dur2>dur1)
      return 1;
    else
      return 0;
  }
  
  // Start the most cpu intensive soundproducers first
  void RT_sort_sound_producers_by_running_time(void){
    qsort(_sound_producers.elements, _sound_producers.size(), sizeof(SoundProducer*), compare_sound_producers);
#if 0    
    printf("\n\n\n****************** START\n");
    for(int i=0;i<_num_sound_producers;i++){
      printf("%.4f\n", 1000.0*SP_get_running_time(_sound_producers[i]));
    }
#endif
  }

  // Starting to get very chaotic...

  void RT_thread(void){
    

    //#ifndef DOESNT_HAVE_SSE
    AVOIDDENORMALS;
    //#endif

    touch_stack();

    RT_lock_player();  // This is an RT-safe lock. Priority inversion can (or at least should) not happen.

    pause_time.start();
    
    QTime excessive_time;
    excessive_time.start();

    while(true){

      // Schedule new notes, etc.
      //PlayerTask(_buffer_size); // The editor player.

      RT_unlock_player();

#ifdef MEMORY_DEBUG
      debug_wait.wait(&debug_mutex, 1000*10); // Speed up valgrind
#endif
      
      // Wait for our jack cycle
      jack_nframes_t num_frames = jack_cycle_wait(_rjack_client);
      
      if((int)num_frames!=_buffer_size)
        printf("What???\n");

      if (ATOMIC_GET(g_request_to_pause_plugins)==true){
        ATOMIC_SET(g_request_to_pause_plugins, false);
        g_process_plugins = false;
        pause_time.restart();
      }
      
      if (g_process_plugins==false) {
        if (pause_time.elapsed() > 5000)
          g_process_plugins = true;
      } else if (_is_freewheeling==false && excessive_time.elapsed() > 2000) { // 2 seconds
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
        excessive_time.restart();
      }
            

      RT_lock_player();

      jackblock_size = num_frames;
      jackblock_cycle_start_stime = pc->end_time;

      if(g_test_crashreporter_in_audio_thread){
        int *ai2=NULL;
        ai2[0] = 50;
      }
      
      jack_time_t start_time = jack_get_time();

      //jackblock_size = num_frames;

      // Process sound.

      if (MULTICORE_get_num_threads() > 1)
        RT_sort_sound_producers_by_running_time();
      
      for (SoundProducer *sp : _sound_producers) {
        SP_RT_reset_running_time(sp);
        SP_RT_called_for_each_soundcard_block(sp);
      }

      ATOMIC_SET(g_currently_processing_dsp, true);
        
      jackblock_delta_time = 0;
      while(jackblock_delta_time < num_frames){

        PlayerTask(RADIUM_BLOCKSIZE);
        
        RT_LPB_set_beat_position(RADIUM_BLOCKSIZE);

        RT_MIDI_handle_play_buffer();
        
        MULTICORE_run_all(_sound_producers, _time, RADIUM_BLOCKSIZE, g_process_plugins);

        _time += RADIUM_BLOCKSIZE;
        jackblock_delta_time += RADIUM_BLOCKSIZE;
      }

      ATOMIC_SET(g_currently_processing_dsp, false);
      
      jack_time_t end_time = jack_get_time();


      // Tell jack we are finished.

      jack_cycle_signal(_rjack_client, 0);


      // Calculate CPU usage

      float new_cpu_usage = (double)(end_time-start_time) * 0.0001 *_sample_rate / num_frames;
      int i_new_cpu_usage = 1000.0 * new_cpu_usage;

      int num_cpu_usage = ATOMIC_GET(g_num_cpu_usage);

      if (num_cpu_usage==0) {
        
        g_total_cpu_usage = i_new_cpu_usage;
        ATOMIC_SET(g_max_cpu_usage, i_new_cpu_usage);
        ATOMIC_SET(g_min_cpu_usage, i_new_cpu_usage);
        
      } else {
        g_total_cpu_usage += i_new_cpu_usage;
        
        if (i_new_cpu_usage > ATOMIC_GET(g_max_cpu_usage))
          ATOMIC_SET(g_max_cpu_usage, i_new_cpu_usage);
      
        if (i_new_cpu_usage < ATOMIC_GET(g_min_cpu_usage))
          ATOMIC_SET(g_min_cpu_usage, i_new_cpu_usage);
      }

      num_cpu_usage++;
      
      ATOMIC_SET(g_num_cpu_usage, num_cpu_usage);

      ATOMIC_SET(g_avg_cpu_usage, g_total_cpu_usage / num_cpu_usage);
      
      if (new_cpu_usage < 98)
        excessive_time.restart();

    } // end while

    RT_unlock_player();
  }
      
  static void *RT_rjack_thread(void *arg){
    /*
      // crashreporter test
      int *p=NULL;
      p[5] = 2;
    */

#if USE_WORKAROUND
    // workaround for non-working UnhandledExceptionFilter in the jack thread. (strange)
    start_workaround_thread();
    
    while(true){
      OS_WaitForAShortTime(1000*1000);
    }
#endif
    
    Mixer *mixer = static_cast<Mixer*>(arg);
    //printf("RT_rjack_process called %d\n",num_frames);

    //char *hello = NULL;
    //hello[0] = 50;


    THREADING_init_player_thread_type();
    R_ASSERT(THREADING_is_player_thread());
    R_ASSERT(!THREADING_is_main_thread());

    PLAYER_acquire_same_priority();

    mixer->RT_thread();
    return NULL;
  }

  static void RT_rjack_freewheel_changed(int starting, void *arg){
    Mixer *mixer = static_cast<Mixer*>(arg);

    if(starting!=0){
      mixer->_is_freewheeling = true;
      RSEMAPHORE_signal(g_freewheeling_has_started,1);
      //SOUNDFILESAVER_start();
    }else{
      printf("MIXER: Freewheeling stopped\n");
      mixer->_is_freewheeling = false;
      //SOUNDFILESAVER_stop();
    }
  }

  static void RT_rjack_shutdown(jack_status_t code, const char *reason, void *arg){
    ATOMIC_SET(g_jack_is_running, false); // must be set before rt_message to avoid deadlock
    RT_message("The jack server shut down\n"
               "(Reason from the server: \"%s\").\n"
               "\n"
               "To continue working:\n"
               "1. Save.\n"
               "2. Exit Radium.\n"
               "3. Start Jack again.\n"
               "4. Start Radium again.\n"
               "\n"
               "Radium can be unstable if you do other operations.",
               reason
               );
  }
  
  static int RT_rjack_buffer_size_changed(jack_nframes_t num_frames, void *arg){
    Mixer *mixer = static_cast<Mixer*>(arg);

    lock_player();{  // Not sure which thread this callback is called from.
      mixer->_buffer_size = num_frames;
      if( (mixer->_buffer_size % RADIUM_BLOCKSIZE) != 0)
        GFX_Message(NULL, "Jack's blocksize of %d is not dividable by Radium's block size of %d. You will get bad sound. Adjust your audio settings.", mixer->_buffer_size, RADIUM_BLOCKSIZE);

      for (SoundProducer *sp : mixer->_sound_producers)
        SP_set_buffer_size(sp, mixer->_buffer_size);
      
    }unlock_player();

    return 0;
  }

};

static Mixer *g_mixer = NULL;

#if USE_WORKAROUND

// workaround for non-working UnhandledExceptionFilter in the jack thread. (strange)

namespace{
struct Workaround : public QThread {

  radium::Semaphore startit;
  
  Workaround(){
    start(QThread::LowestPriority); //QThread::TimeCriticalPriority); // The priority shouldn't matter though since PLAYER_acquire_same_priority() is called inside run().
  }
  
  void run(){
        
    THREADING_init_player_thread_type();
    R_ASSERT(THREADING_is_player_thread());
    R_ASSERT(!THREADING_is_main_thread());

    startit.wait();
    
    PLAYER_acquire_same_priority();

    g_mixer->RT_thread();
  }
};
}

static Workaround *g_workaround;

static void create_workaround_thread(void){
  g_workaround = new Workaround;
}

static void start_workaround_thread(void){
  g_workaround->startit.signal();
}
#endif

bool MIXER_start(void){
  
  R_ASSERT(THREADING_is_main_thread());

  init_player_lock();
  g_freewheeling_has_started = RSEMAPHORE_create(0);

#if USE_WORKAROUND
  create_workaround_thread();
#endif
  
  g_mixer = new Mixer();
  
  if(g_mixer->start_jack()==false)
    return false;
  
  PR_init_plugin_types();

  //Sleep(3000);

  check_jackd_arguments();

  // Multicore is initialized after starting jack, since the "runners" call THREADING_acquire_player_thread_priority in the constructor, which don't work before jack has started.
  MULTICORE_init();
  
  return true;
}

void MIXER_stop(void){
  static bool has_been_called = false;
  
  R_ASSERT(g_mixer->_rjack_client != NULL);
  R_ASSERT(has_been_called==false);
    
  if (g_mixer->_rjack_client != NULL)
    jack_client_close(g_mixer->_rjack_client);

  has_been_called=true;
}

static STime g_startup_time = 0;
void OS_InitAudioTiming(void){
  g_startup_time = g_mixer->_time;
  printf("OS_InitAudioTiming called. New time: %d\n",(int)g_startup_time);
}

bool MIXER_is_saving(void){
  return g_mixer->_is_freewheeling;
}

void MIXER_start_saving_soundfile(void){
  RSEMAPHORE_reset(g_freewheeling_has_started); // Must do this in case a different jack client started freewheeling since last call to sem_init.
  jack_set_freewheel(g_jack_client, 1);
  RSEMAPHORE_wait(g_freewheeling_has_started,1);
}

void MIXER_request_stop_saving_soundfile(void){
  jack_set_freewheel(g_jack_client, 0);
  printf("REQUEST to stop saving received\n");
}

// dont work.
STime MIXER_get_block_delta_time(STime time){
  return (time+g_startup_time) - g_mixer->_time;
}

/*
int64_t MIXER_get_time(void){
  return g_mixer->_time;
}
*/

// Like pc->start_time, but sub-block accurately
STime MIXER_get_accurate_radium_time(void){
  struct Blocks *block = pc->block;

  if (block==NULL)
    return pc->start_time;

  int deltatime = jack_frames_since_cycle_start(g_mixer->_rjack_client);
  return
    jackblock_cycle_start_stime +
    scale(deltatime,
          0, jackblock_size,
          0, jackblock_size * safe_volatile_float_read(&block->reltempo)
          );
}

#if 0
void MIXER_get_buses(SoundProducer* &bus1, SoundProducer* &bus2){
  bus1 = g_mixer->_bus1;
  bus2 = g_mixer->_bus2;
}
#endif

Buses MIXER_get_buses(void){
  Buses ret = {g_mixer->_bus1, g_mixer->_bus2};
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

void MIXER_remove_SoundProducer(SoundProducer *sound_producer){
  g_mixer->remove_SoundProducer(sound_producer);
}

radium::Vector<SoundProducer*> *MIXER_get_all_SoundProducers(void){
  if (g_mixer==NULL)
    return NULL;
  else
    return &g_mixer->_sound_producers;
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

float MIXER_get_sample_rate(void){
  return g_mixer->_sample_rate;
}

int MIXER_get_buffer_size(void){
  return RADIUM_BLOCKSIZE; //g_mixer->_buffer_size;
}


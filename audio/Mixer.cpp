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

#include "../weakjack/weak_libjack.h"

#include <QMessageBox>
#include <QString>
#include <QStringList>
#include <QTime>


// I'm not entirely sure where memory barriers should be placed, so I've tried to be more safe than sorry.
//#include "pa_memorybarrier.h"

#include "../common/nsmtracker.h"
#include "../common/player_proc.h"
#include "../common/playerclass.h"
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


/*

  TODO/FIX. This sometimes happens during startup:

-3.14.3-1.fc17.x86_64 openldap-2.4.33-3.fc17.x86_64 qt4-theme-quarticurve-0.0-0.17.beta8.fc17.x86_64 raptor2-2.0.7-1.fc17.x86_64 xz-libs-5.1.2-1alpha.fc17.x86_64 yajl-2.0.4-1.fc17.x86_64
(gdb) bt
#0  0x0000003b49435935 in raise () from /lib64/libc.so.6
#1  0x0000003b494370e8 in abort () from /lib64/libc.so.6
#2  0x00000000004ec954 in show_message (type=0, message=0x7fffffffa0b0 "Calling lock_player while holding the player lock") at X11/X11_error.c:71
#3  0x00000000004eca31 in RError (fmt=0xa4e898 "Calling lock_player while holding the player lock") at X11/X11_error.c:87
#4  0x00000000005d38e1 in lock_player () at audio/Mixer.cpp:228
#5  PLAYER_lock () at audio/Mixer.cpp:253
#6  0x00000000005b23f5 in remove_SoundProducerInput (ch=<optimized out>, sound_producer_ch=<optimized out>, sound_producer=<optimized out>, this=<optimized out>) at audio/SoundProducer.cpp:415
#7  SP_remove_link (target=<optimized out>, target_ch=<optimized out>, source=<optimized out>, source_ch=<optimized out>) at audio/SoundProducer.cpp:728
#8  0x00000000005de1fc in CONNECTION_delete_connection (connection=0x4959180) at mixergui/QM_chip.cpp:626
#9  0x00000000005deb5f in Chip::~Chip (this=0x48bfe90, __in_chrg=<optimized out>) at mixergui/QM_chip.cpp:761
#10 0x00000000005dece0 in Chip::~Chip (this=0x48bfe90, __in_chrg=<optimized out>) at mixergui/QM_chip.cpp:772
#11 0x00000000005e8924 in MW_delete_plugin (plugin=0x488f5b0) at mixergui/QM_MixerWidget.cpp:1110
#12 0x00000000005e9044 in delete_a_chip () at mixergui/QM_MixerWidget.cpp:1223
#13 0x00000000005e90a6 in MW_cleanup () at mixergui/QM_MixerWidget.cpp:1231
#14 0x00000000005e9486 in MW_create_from_state (state=0x2584340) at mixergui/QM_MixerWidget.cpp:1298
#15 0x0000000000472a2e in DLoadSong (newroot=0x2bab380, song=0x2af90a0) at common/disk_song.c:168
#16 0x0000000000473563 in DLoadRoot (theroot=0x2bab380) at common/disk_root.c:139
#17 0x00000000004742fa in Load (filename=0x2a60630 "/home/kjetil/radium3.0/bin/new_song.rad") at common/disk_load.c:154
#18 0x000000000047453d in Load_CurrPos_org (window=0x2affe00, filename=0x2a60630 "/home/kjetil/radium3.0/bin/new_song.rad") at common/disk_load.c:218
#19 0x00000000004746db in NewSong_CurrPos (window=0x2affe00) at common/disk_load.c:258
#20 0x000000000047b4de in radium_main (arg=0x23a152c "") at Qt/Qt_Main.cpp:642
#21 0x00000000004e5f7a in init_radium (arg=0x23a152c "", gkf=<function at remote 0x261a140>) at api/api_common.c:61
#22 0x00000000004dfa90 in _wrap_init_radium (self=0x0, args=('', <function at remote 0x261a140>)) at api/radium_wrap.c:572
#23 0x0000003b5d4dd0e1 in call_function (oparg=<optimized out>, pp_stack=0x7fffffffcb88) at /usr/src/debug/Python-2.7.3/Python/ceval.c:4098
#24 PyEval_EvalFrameEx (f=f@entry=Frame 0x25e8b50, for file /home/kjetil/radium3.0/bin/start.py, line 137, in <module> (), throwflag=throwflag@entry=0) at /usr/src/debug/Python-2.7.3/Python/ceval.c:2740
#25 0x0000003b5d4ddb1f in PyEval_EvalCodeEx (co=co@entry=0x25af0b0, globals=globals@entry=

*/


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
#include <QMutex>
#include <QWaitCondition>

static QMutex debug_mutex;
static QWaitCondition debug_wait;

void PLAYER_memory_debug_wake_up(void){
  debug_wait.wakeOne();
}
#endif



jack_client_t *g_jack_client;
static int g_jack_client_priority;
float g_cpu_usage = 0.0f;

static bool g_jack_is_running = true;

bool PLAYER_is_running(void){
  return g_jack_is_running;
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

static void PLAYER_drop_same_priority(void){
  jack_drop_real_time_scheduling(GET_CURRENT_THREAD());
}



static void check_jackd_arguments(void){
#if defined(FOR_WINDOWS)
  
  QString mandatory= "The \"-S\" parameter is mandatory to make the jack server work correctly.\n\n"
    "The jackd argument line can be set in QJackCtl (\"Jack Control\") under Setup -> Settings -> Server Prefix. The line should look like this:\n\n"
    "jackd -S";
  
  bool found_jack = false;
  
  vector_t *command_lines = get_windows_command_lines();

  for(int command_line_num=0;command_line_num<command_lines->num_elements;command_line_num++){
    
    const char *command_line = (const char *)command_lines->elements[command_line_num];
    QString line(command_line);

    line = line.trimmed();

    //printf("Got line: \"%s\" %s\n",line.toUtf8().constData(),command_line);

    if(line.startsWith("jackd") || line.startsWith("jackd.exe")){

      found_jack = true;

      bool found_sync_flag=false;
      QStringList elements = line.split(" ", QString::SkipEmptyParts);

      for(int i=0;i<elements.size();i++){
        QString element = elements.at(i);
        if(element=="-S")
          found_sync_flag=true;
      }

      if(found_sync_flag==false){

        num_users_of_keyboard++;
        QMessageBox msgBox;
        msgBox.setIcon(QMessageBox::Critical);
        msgBox.setText("The -S parameter was not set for Jack.");
        msgBox.setInformativeText(mandatory);
        
        msgBox.setStandardButtons(QMessageBox::Ok);
        msgBox.exec();
        num_users_of_keyboard--;
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
  }

#endif
}

#if defined(FOR_LINUX) || defined(FOR_MACOSX)
static pthread_mutexattr_t player_lock_mutexattr;
#endif
static LockType player_lock;

static __thread bool g_current_thread_has_player_lock = false;


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

bool PLAYER_current_thread_has_lock(void){
  return g_current_thread_has_player_lock;
}


static void init_player_lock(void){

#if defined(FOR_LINUX) || defined(FOR_MACOSX)
  int s1 = pthread_mutexattr_init(&player_lock_mutexattr);
  if (s1!=0)
    RError("pthread_mutexattr_init failed: %d\n",s1);

  int s2 = pthread_mutexattr_settype(&player_lock_mutexattr, PTHREAD_MUTEX_RECURSIVE);
  if (s2!=0)
    RError("pthread_mutexattr_settype failed: %d\n",s2);

#ifdef FOR_LINUX
  int s3 = pthread_mutexattr_setprotocol(&player_lock_mutexattr, PTHREAD_PRIO_INHERIT);  // Regarding macosx, I don't know whether it supports PTHREAD_PRIO_INHERIT, so we just boost priority manually instead on that platform.
  if (s3!=0)
    RError("pthread_mutexattr_setprotocol failed: %d\n",s3);
#endif
  
  int s4 = pthread_mutex_init(&player_lock, &player_lock_mutexattr);
  if (s4!=0)
    RError("pthread_mutex_init failed: %d\n",s4);

#elif defined(FOR_WINDOWS)
   LOCK_INITIALIZE(player_lock);
#else

#error "unkwndonw arcthinerture"
#endif
}

int jackblock_size = 0;
jack_time_t jackblock_delta_time = 0;
static STime jackblock_cycle_start_stime = 0;

struct Mixer{
  SoundProducer *_bus1;
  SoundProducer *_bus2;

  int _num_sound_producers;
  SoundProducer **_sound_producers;
  
  jack_client_t *_rjack_client;
  int64_t _last_time;
  int64_t _time;

  float _sample_rate;
  int _buffer_size;

  bool _is_freewheeling;

  Mixer()
    : _bus1(NULL)
    , _bus2(NULL)
    , _num_sound_producers(0)
    , _sound_producers(NULL)
    , _rjack_client(NULL)
    , _last_time(0)
    , _time(0)
    , _is_freewheeling(false)
  {
  }

  void RT_set_bus_descendant_type_for_all_plugins(){
    
    // First set all descendant types to MAYBE.
    for(int i=0 ; i<_num_sound_producers ; i++){
      struct SoundPlugin *plugin = SP_get_plugin(_sound_producers[i]);
      plugin->bus_descendant_type = MAYBE_A_BUS_DESCENDANT;      
    }
    
    // Then set one by one.
    for(int i=0 ; i<_num_sound_producers ; i++)
      SP_RT_set_bus_descendant_type_for_plugin(_sound_producers[i]);

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
          new_g_click_plugins = (SoundPlugin **)calloc(sizeof(SoundPlugin*), new_num_allocated_click_plugins);
          new_g_click_patches = (Patch **)malloc(sizeof(Patch*)*new_num_allocated_click_plugins);
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

      _num_sound_producers++;
      _sound_producers = (SoundProducer**)realloc(_sound_producers, sizeof(SoundProducer*) * _num_sound_producers);
      _sound_producers[_num_sound_producers-1] = sound_producer;
      
    }PLAYER_unlock();
  }
    
  void remove_SoundProducer(SoundProducer *sound_producer){
    SoundPlugin *plugin = SP_get_plugin(sound_producer);
    
    bool is_click_patch = false;

    if (!strcmp(plugin->type->type_name,"Sample Player"))
      if(!strcmp(plugin->type->name,g_click_name))
        is_click_patch = true;

    
    PLAYER_lock();{
      {
        int pos;
        for(pos=0 ; pos<_num_sound_producers ; pos++)
          if (_sound_producers[pos]==sound_producer)
            break;
        R_ASSERT(pos < _num_sound_producers);
        
        if (_num_sound_producers==1){
          R_ASSERT(pos==0);
          _sound_producers[pos] = NULL;
        } else {
          _sound_producers[pos] = _sound_producers[_num_sound_producers-1];
        }
        _num_sound_producers--;
      }
      
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
      RWarning("Jack's blocksize of %d is less than Radium's block size of %d. You will get bad sound. Adjust your audio settings.", _buffer_size, RADIUM_BLOCKSIZE);
    else if((_buffer_size % RADIUM_BLOCKSIZE) != 0)
      RWarning("Jack's blocksize of %d is not dividable by Radium's block size of %d. You will get bad sound. Adjust your audio settings.", _buffer_size, RADIUM_BLOCKSIZE);

    g_last_set_producer_buffersize = _buffer_size;
    g_jack_client_priority = jack_client_real_time_priority(_rjack_client);

    if(_sample_rate<100.0)
      RWarning("Sample rate value is strange: %f",(float)_sample_rate);

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
    qsort(_sound_producers, _num_sound_producers, sizeof(SoundProducer*), compare_sound_producers);
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

#if 0
#define CSR_FLUSH_TO_ZERO         (1 << 15)
    unsigned csr = __builtin_ia32_stmxcsr();
    csr |= CSR_FLUSH_TO_ZERO;
    __builtin_ia32_ldmxcsr(csr);
#endif

    RT_lock_player();  // This is a RT-safe lock. Priority inversion can not happen.

    QTime pause_time;
    pause_time.start();
    
    QTime excessive_time;
    excessive_time.start();
    
    bool process_plugins = true;

    while(true){

      // Schedule new notes, etc.
      //PlayerTask(_buffer_size); // The editor player.

      RT_unlock_player();

#ifdef MEMORY_DEBUG
      debug_wait.wait(&debug_mutex, 1000*10); // Speed up valgrind
#endif
      
      // Wait for our jack cycle
      jack_nframes_t num_frames = jack_cycle_wait(_rjack_client);
      jack_time_t start_time = jack_get_time();
      
      if((int)num_frames!=_buffer_size)
        printf("What???\n");

      jackblock_size = num_frames;
      jackblock_cycle_start_stime = pc->end_time;

      if (process_plugins==false) {
        if (pause_time.elapsed() > 5000)
          process_plugins = true;
      } else if (_is_freewheeling==false && excessive_time.elapsed() > 2000) { // 2 seconds
        if (pc->isplaying) {
          RT_request_to_stop_playing();
          RT_message("Error!\n"
                     "\n"
                     "Audio using too much CPU. Stopping player to avoid locking up the computer.%s",
                     g_running_multicore ? "" : "\n\nTip: Turning on Multi CPU processing might help."
                     );
        } else {
          RT_message("Error!\n"
                     "\n"
                     "Audio using too much CPU. Pausing audio generation for 5 seconds to avoid locking up the computer.%s",
                     g_running_multicore ? "" : "\n\nTip: Turning on Multi CPU processing might help."
                     );
          printf("stop processing plugins\n");
          process_plugins = false; // Because the main thread waits very often waits for the audio thread, we can get very long breaks where nothing happens if the audio thread uses too much CPU.
        }
        
        pause_time.restart();
        excessive_time.restart();
      }
            

      RT_lock_player();

      //jackblock_size = num_frames;

      // Process sound.

      jackblock_delta_time = 0;
      while(jackblock_delta_time < num_frames){

        PlayerTask(RADIUM_BLOCKSIZE);
        
        RT_LPB_set_beat_position(RADIUM_BLOCKSIZE);

        RT_MIDI_handle_play_buffer();
        
        if (g_running_multicore) {

          RT_sort_sound_producers_by_running_time();
          MULTICORE_run_all(_sound_producers, _num_sound_producers, _time, RADIUM_BLOCKSIZE, process_plugins);
          
        } else {
          
          if (_bus1!=NULL)
            SP_RT_process(_bus1,_time,RADIUM_BLOCKSIZE, process_plugins);
          
          if (_bus2!=NULL)
            SP_RT_process(_bus2,_time,RADIUM_BLOCKSIZE, process_plugins);

          for(int i=0 ; i<_num_sound_producers ; i++)
            // A soundproducer is self responsible for first running other soundproducers it gets data or audio from, and not running itself more than once per block.
            // (Unless running MultiCore, then the MultiCore system takes care of running in the right order)
            SP_RT_process(_sound_producers[i], _time, RADIUM_BLOCKSIZE, process_plugins );
          
        }

        _time += RADIUM_BLOCKSIZE;
        jackblock_delta_time += RADIUM_BLOCKSIZE;
      }

      // Tell jack we are finished.
      jack_cycle_signal(_rjack_client, 0);

      jack_time_t end_time = jack_get_time();
      float new_cpu_usage = (double)(end_time-start_time) * 0.0001 *_sample_rate / num_frames;
      if (new_cpu_usage > g_cpu_usage)
        g_cpu_usage = new_cpu_usage;
      //g_cpu_usage = (double)(end_time-start_time) * 0.0001 *_sample_rate / num_frames;

      if (new_cpu_usage < 98)
        excessive_time.restart();
          
    } // end while

    RT_unlock_player();
  }
      

  static void *RT_rjack_thread(void *arg){
    Mixer *mixer = static_cast<Mixer*>(arg);
    //printf("RT_rjack_process called %d\n",num_frames);

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
    g_jack_is_running = false; // must be set before rt_message to avoid deadlock
    RT_message("The jack server shut down\n"
               "(Reason from the server: \"%s\").\n"
               "\n"
               "To continue working:\n"
               "1. Exit Radium.\n"
               "2. Start Jack again.\n"
               "3. Start Radium again."
               );
  }
  
  static int RT_rjack_buffer_size_changed(jack_nframes_t num_frames, void *arg){
    Mixer *mixer = static_cast<Mixer*>(arg);

    lock_player();{  // Not sure which thread this callback is called from.
      mixer->_buffer_size = num_frames;
      if( (mixer->_buffer_size % RADIUM_BLOCKSIZE) != 0)
        RWarning("Jack's blocksize of %d is not dividable by Radium's block size of %d. You will get bad sound. Adjust your audio settings.", mixer->_buffer_size, RADIUM_BLOCKSIZE);

      for(int i=0 ; i<mixer->_num_sound_producers ; i++)
        SP_set_buffer_size(mixer->_sound_producers[i], mixer->_buffer_size);
      
    }unlock_player();

    return 0;
  }

};

static Mixer *g_mixer;

bool MIXER_start(void){
  
  R_ASSERT(THREADING_is_main_thread());

  init_player_lock();
  g_freewheeling_has_started = RSEMAPHORE_create(0);
  
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

int64_t MIXER_get_time(void){
  return g_mixer->_time;
}

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
          0, jackblock_size * block->reltempo
          );
}

void MIXER_RT_set_bus_descendand_type_for_all_plugins(void){
  g_mixer->RT_set_bus_descendant_type_for_all_plugins();
}

void MIXER_add_SoundProducer(SoundProducer *sound_producer){
  g_mixer->add_SoundProducer(sound_producer);
}

void MIXER_remove_SoundProducer(SoundProducer *sound_producer){
  g_mixer->remove_SoundProducer(sound_producer);
}

SoundProducer **MIXER_get_all_SoundProducers(int &num_sound_producers){
  num_sound_producers = g_mixer->_num_sound_producers;
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

float MIXER_get_sample_rate(void){
  return g_mixer->_sample_rate;
}

int MIXER_get_buffer_size(void){
  return RADIUM_BLOCKSIZE; //g_mixer->_buffer_size;
}


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

#include <jack/jack.h>
#include <jack/thread.h>

#include <QMessageBox>
#include <QString>
#include <QStringList>

// I'm not entirely sure where memory barriers should be placed, so I've tried to be more safe than sorry.
//#include "pa_memorybarrier.h"

#include "../common/nsmtracker.h"
#include "../common/player_proc.h"
#include "../common/playerclass.h"
#include "../common/OS_Player_proc.h"
#include "../common/threading.h"

#include "Jack_plugin_proc.h"
#include "SoundfileSaver_proc.h"
#include "get_windows_commandlines_proc.h"

#include "SoundProducer_proc.h"
#include "SoundPluginRegistry_proc.h"

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

#ifndef DOESNT_HAVE_SSE
#  include <xmmintrin.h>
#endif


#include <float.h>

//#pragma fenv_access (on)

// On Intel set FZ (Flush to Zero) and DAZ (Denormals Are Zero)
// flags to avoid costly denormals
// (Copied from faust)
#ifdef __SSE__
    #ifdef __SSE2__
        #define AVOIDDENORMALS _mm_setcsr(_mm_getcsr() | 0x8040)
    #else
        #define AVOIDDENORMALS _mm_setcsr(_mm_getcsr() | 0x8000)
    #endif
#else
#   error "AVOIDDENORMALS is not defined"
    #define AVOIDDENORMALS 
#endif


jack_client_t *g_jack_client;
static int g_jack_client_priority;
float g_cpu_usage = 0.0f;

static void PLAYER_acquire_same_priority(void){
  //printf("Setting real time priority temporarily for %p.\n",(void*)pthread_self());
  jack_acquire_real_time_scheduling(GET_CURRENT_THREAD(),g_jack_client_priority);
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

    printf("Got line: \"%s\" %s\n",line.ascii(),command_line);

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


static LockType player_lock;

static bool someone_has_player_lock = false;
static bool player_thread_has_player_lock = false;

static void lock_player(void){
  if (PLAYER_current_thread_has_lock()){
#if !defined(RELEASE)
    RError("Calling lock_player while holding the player lock");
#endif
    return;
  }
  
  LOCK_LOCK(player_lock);
  someone_has_player_lock = true;
}

static void unlock_player(void){
  someone_has_player_lock = false;
  LOCK_UNLOCK(player_lock);
}

static void RT_lock_player(){
  R_ASSERT_RETURN_IF_FALSE(THREADING_is_player_thread());
  lock_player();
  player_thread_has_player_lock = true;
}

static void RT_unlock_player(){
  R_ASSERT_RETURN_IF_FALSE(THREADING_is_player_thread());
  player_thread_has_player_lock = false;
  unlock_player();
}

void PLAYER_lock(void){
  R_ASSERT_RETURN_IF_FALSE(!THREADING_is_player_thread());
  R_ASSERT_RETURN_IF_FALSE(THREADING_is_main_thread());

  PLAYER_acquire_same_priority();
  lock_player();
}

void PLAYER_unlock(void){
  R_ASSERT_RETURN_IF_FALSE(!THREADING_is_player_thread());
  R_ASSERT_RETURN_IF_FALSE(THREADING_is_main_thread());

  unlock_player();
  PLAYER_drop_same_priority();
}

bool PLAYER_current_thread_has_lock(void){
  if (someone_has_player_lock==false)
    return false;
  else if (THREADING_is_player_thread())
    return player_thread_has_player_lock;
  else
    return !player_thread_has_player_lock;
}

bool PLAYER_someone_has_player_lock(void){
  return someone_has_player_lock;
}

bool PLAYER_player_has_player_lock(void){
  return player_thread_has_player_lock;
}

static void init_player_lock(void){
  LOCK_INITIALIZE(player_lock);
}

int jackblock_size = 0;
jack_time_t jackblock_delta_time = 0;

struct Mixer{
  SoundProducer *_bus1;
  SoundProducer *_bus2;

  DoublyLinkedList _sound_producers;
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

  void RT_set_bus_descendant_type_for_all_plugins(){

    // First set all descendant types to MAYBE.
    {
      DoublyLinkedList *sound_producer = _sound_producers.next;
      while(sound_producer!=NULL){
        struct SoundPlugin *plugin = SP_get_plugin((SoundProducer*)sound_producer);
        plugin->bus_descendant_type = MAYBE_A_BUS_DESCENDANT;
        sound_producer = sound_producer->next;
      }    
    }

    // Then set one by one.
    {
      DoublyLinkedList *sound_producer = _sound_producers.next;
      while(sound_producer!=NULL){
        SP_RT_set_bus_descendant_type_for_plugin((SoundProducer*)sound_producer);
        sound_producer = sound_producer->next;
      } 
    }
  }

  void add_SoundProducer(SoundProducer *sound_producer){
    SoundPlugin *plugin = SP_get_plugin(sound_producer);
    int bus_num = -1;

    if(!strcmp(plugin->type->type_name,"Bus")){
      if(!strcmp(plugin->type->name,"Bus 1"))
        bus_num = 0;
      else
        bus_num = 1;
    }

    PLAYER_lock();{

      if(bus_num==0)
        _bus1 = sound_producer;
      if(bus_num==1)
        _bus2 = sound_producer;

      _sound_producers.add((DoublyLinkedList*)sound_producer);

    }PLAYER_unlock();
  }

  void remove_SoundProducer(SoundProducer *sound_producer){
    PLAYER_lock();{
      _sound_producers.remove((DoublyLinkedList*)sound_producer);
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
      RError("Sample rate value is strange");

    pc->pfreq = _sample_rate; // bang!

    jack_set_buffer_size_callback(_rjack_client,RT_rjack_buffer_size_changed,this);
    jack_set_freewheel_callback(_rjack_client, RT_rjack_freewheel_changed, this);
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

    while(true){

      // Schedule new notes, etc.
      //PlayerTask(_buffer_size); // The editor player.

      RT_unlock_player();

      // Wait for our jack cycle
      jack_nframes_t num_frames = jack_cycle_wait(_rjack_client);
      if((int)num_frames!=_buffer_size)
        printf("What???\n");

      RT_lock_player();

      jackblock_size = num_frames;

      // Process sound.

      jack_time_t start_time = jack_get_time();

      jackblock_delta_time = 0;
      while(jackblock_delta_time < num_frames){
        PlayerTask(RADIUM_BLOCKSIZE);

        if(_bus1!=NULL)
          SP_RT_process(_bus1,_time,RADIUM_BLOCKSIZE);
        if(_bus2!=NULL)
          SP_RT_process(_bus2,_time,RADIUM_BLOCKSIZE);
      
        {
          DoublyLinkedList *sound_producer = _sound_producers.next;
          while(sound_producer!=NULL){
            SP_RT_process((SoundProducer*)sound_producer,_time,RADIUM_BLOCKSIZE);
            sound_producer = sound_producer->next;
          }
        }

        _time += RADIUM_BLOCKSIZE;
        jackblock_delta_time += RADIUM_BLOCKSIZE;
      }

      //_time += num_frames;
      
      jack_time_t end_time = jack_get_time();
        g_cpu_usage = (double)(end_time-start_time) * 0.0001 *_sample_rate / num_frames;
        
      // Tell jack we are finished.
      jack_cycle_signal(_rjack_client, 0);

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

  static int RT_rjack_buffer_size_changed(jack_nframes_t num_frames, void *arg){
    Mixer *mixer = static_cast<Mixer*>(arg);

    lock_player();{  // Not sure which thread this callback is called from.
      mixer->_buffer_size = num_frames;
      if( (mixer->_buffer_size % RADIUM_BLOCKSIZE) != 0)
        RWarning("Jack's blocksize of %d is not dividable by Radium's block size of %d. You will get bad sound. Adjust your audio settings.", mixer->_buffer_size, RADIUM_BLOCKSIZE);

      DoublyLinkedList *sound_producer = mixer->_sound_producers.next;
      while(sound_producer!=NULL){
        SP_set_buffer_size((SoundProducer*)sound_producer, mixer->_buffer_size);
        sound_producer = sound_producer->next;
      }
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

  check_jackd_arguments();
  
  return true;
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

void MIXER_RT_set_bus_descendand_type_for_all_plugins(void){
  g_mixer->RT_set_bus_descendant_type_for_all_plugins();
}

void MIXER_add_SoundProducer(SoundProducer *sound_producer){
  g_mixer->add_SoundProducer(sound_producer);
}

void MIXER_remove_SoundProducer(SoundProducer *sound_producer){
  g_mixer->remove_SoundProducer(sound_producer);
}

DoublyLinkedList *MIXER_get_all_SoundProducers(void){
  return g_mixer->_sound_producers.next;
}

float MIXER_get_sample_rate(void){
  return g_mixer->_sample_rate;
}

int MIXER_get_buffer_size(void){
  return RADIUM_BLOCKSIZE; //g_mixer->_buffer_size;
}


/*

Mystery backtrace. Happens quite seldom though:

Thread 1 (Thread 0x7ffff7fa5900 (LWP 8234)):
#0  0x00007ffff2c59877 in raise () from /lib64/libc.so.6
#1  0x00007ffff2c5af68 in abort () from /lib64/libc.so.6
#2  0x00000000004683bd in show_message (type=0, message=0x7fffffff9fb0 "Calling lock_player while holding the player lock") at common/error.c:68
#3  0x000000000046849e in RError (fmt=0x9e38b8 "Calling lock_player while holding the player lock") at common/error.c:83
#4  0x00000000005def3e in lock_player () at audio/Mixer.cpp:201
#5  PLAYER_lock () at audio/Mixer.cpp:229
#6  0x00000000005b92f5 in remove_SoundProducerInput (ch=<optimized out>, sound_producer_ch=<optimized out>, sound_producer=<optimized out>, this=<optimized out>)
    at audio/SoundProducer.cpp:415
#7  SP_remove_link (target=<optimized out>, target_ch=<optimized out>, source=<optimized out>, source_ch=<optimized out>) at audio/SoundProducer.cpp:728
#8  0x00000000005eabe0 in CONNECTION_delete_connection (connection=0x6a27970) at mixergui/QM_chip.cpp:626
#9  0x00000000005eb463 in Chip::~Chip (this=0x6994f50, __in_chrg=<optimized out>) at mixergui/QM_chip.cpp:761
#10 0x00000000005eb5e0 in Chip::~Chip (this=0x6994f50, __in_chrg=<optimized out>) at mixergui/QM_chip.cpp:772
#11 0x00000000005f4d00 in MW_delete_plugin (plugin=0x7385b80) at mixergui/QM_MixerWidget.cpp:1110
#12 0x00000000005f541f in delete_a_chip () at mixergui/QM_MixerWidget.cpp:1223
#13 0x00000000005f547f in MW_cleanup () at mixergui/QM_MixerWidget.cpp:1231
#14 0x00000000005f5863 in MW_create_from_state (state=0x140d2680) at mixergui/QM_MixerWidget.cpp:1298
#15 0x0000000000470f0e in DLoadSong (newroot=0x140c61c0, song=0x140c10f0) at common/disk_song.c:168
#16 0x0000000000471a7f in DLoadRoot (theroot=0x140c61c0) at common/disk_root.c:139
#17 0x00000000004727fa in Load (filename=0x25e8eb4 "/home/kjetil/radium/bin/sounds/song1.rad") at common/disk_load.c:154
#18 0x0000000000472a40 in Load_CurrPos_org (window=0x2534200, filename=0x25e8eb4 "/home/kjetil/radium/bin/sounds/song1.rad") at common/disk_load.c:218
#19 0x0000000000472b6d in LoadSong_CurrPos (window=0x2534200, filename=0x25e8eb4 "/home/kjetil/radium/bin/sounds/song1.rad") at common/disk_load.c:252
#20 0x00000000004e93e3 in loadSong (filename=0x25e8eb4 "/home/kjetil/radium/bin/sounds/song1.rad") at api/api_various.c:338
#21 0x00000000004e08ad in _wrap_loadSong (self=0x0, args=0x2551110) at api/radium_wrap.c:2018
#22 0x00007ffff47fbbc4 in PyEval_EvalFrameEx () from /lib64/libpython2.7.so.1.0
#23 0x00007ffff47fd1dd in PyEval_EvalCodeEx () from /lib64/libpython2.7.so.1.0
#24 0x00007ffff47fd2e2 in PyEval_EvalCode () from /lib64/libpython2.7.so.1.0
#25 0x00007ffff481671f in ?? () from /lib64/libpython2.7.so.1.0
#26 0x00007ffff4817585 in PyRun_StringFlags () from /lib64/libpython2.7.so.1.0
#27 0x00007ffff4818f2b in PyRun_SimpleStringFlags () from /lib64/libpython2.7.so.1.0
#28 0x0000000000480004 in MenuItem::clicked (this=0x44ccfb0) at Qt/Qt_Menues.cpp:91
#29 0x000000000047f46e in MenuItem::qt_static_metacall (_o=0x44ccfb0, _c=QMetaObject::InvokeMetaMethod, _id=0, _a=0x7fffffffbd80) at Qt/mQt_Menues.cpp:47
#30 0x00007ffff5b3737a in QMetaObject::activate(QObject*, QMetaObject const*, int, void**) () from /lib64/libQtCore.so.4
#31 0x00007ffff604e941 in QAction::activated(int) () from /lib64/libQtGui.so.4
#32 0x00007ffff605040c in QAction::activate(QAction::ActionEvent) () from /lib64/libQtGui.so.4
#33 0x00007ffff649967d in QMenuPrivate::activateCausedStack(QList<QPointer<QWidget> > const&, QAction*, QAction::ActionEvent, bool) () from /lib64/libQtGui.so.4
#34 0x00007ffff649df19 in QMenuPrivate::activateAction(QAction*, QAction::ActionEvent, bool) () from /lib64/libQtGui.so.4
#35 0x00007ffff60a7cc8 in QWidget::event(QEvent*) () from /lib64/libQtGui.so.4
#36 0x00007ffff64a1f6b in QMenu::event(QEvent*) () from /lib64/libQtGui.so.4
#37 0x00007ffff6054e5c in QApplicationPrivate::notify_helper(QObject*, QEvent*) () from /lib64/libQtGui.so.4
#38 0x00007ffff605b8f1 in QApplication::notify(QObject*, QEvent*) () from /lib64/libQtGui.so.4
#39 0x00007ffff5b228fd in QCoreApplication::notifyInternal(QObject*, QEvent*) () from /lib64/libQtCore.so.4
#40 0x00007ffff605b067 in QApplicationPrivate::sendMouseEvent(QWidget*, QMouseEvent*, QWidget*, QWidget*, QWidget**, QPointer<QWidget>&, bool) () from /lib64/libQtGui.so.4
#41 0x00007ffff60d096c in QETWidget::translateMouseEvent(_XEvent const*) () from /lib64/libQtGui.so.4
#42 0x00007ffff60cf0ac in QApplication::x11ProcessEvent(_XEvent*) () from /lib64/libQtGui.so.4
#43 0x00007ffff60f6ac4 in x11EventSourceDispatch(_GSource*, int (*)(void*), void*) () from /lib64/libQtGui.so.4
#44 0x00007ffff3e6d2a6 in g_main_context_dispatch () from /lib64/libglib-2.0.so.0
#45 0x00007ffff3e6d628 in g_main_context_iterate.isra () from /lib64/libglib-2.0.so.0
#46 0x00007ffff3e6d6dc in g_main_context_iteration () from /lib64/libglib-2.0.so.0
---Type <return> to continue, or q <return> to quit---
#47 0x00007ffff5b5141e in QEventDispatcherGlib::processEvents(QFlags<QEventLoop::ProcessEventsFlag>) () from /lib64/libQtCore.so.4
#48 0x00007ffff60f6c46 in QGuiEventDispatcherGlib::processEvents(QFlags<QEventLoop::ProcessEventsFlag>) () from /lib64/libQtGui.so.4
#49 0x00007ffff5b2138f in QEventLoop::processEvents(QFlags<QEventLoop::ProcessEventsFlag>) () from /lib64/libQtCore.so.4
#50 0x00007ffff5b216dd in QEventLoop::exec(QFlags<QEventLoop::ProcessEventsFlag>) () from /lib64/libQtCore.so.4
#51 0x00007ffff5b26da9 in QCoreApplication::exec() () from /lib64/libQtCore.so.4
#52 0x0000000000479c5c in radium_main (arg=0x7ffff7e9952c "") at Qt/Qt_Main.cpp:758
#53 0x00000000004e5cfb in init_radium (arg=0x7ffff7e9952c "", gkf=0x248e2a8) at api/api_common.c:61
#54 0x00000000004dcc8f in _wrap_init_radium (self=0x0, args=0x7fffe1625200) at api/radium_wrap.c:572
#55 0x00007ffff47fbbc4 in PyEval_EvalFrameEx () from /lib64/libpython2.7.so.1.0
#56 0x00007ffff47fd1dd in PyEval_EvalCodeEx () from /lib64/libpython2.7.so.1.0
#57 0x00007ffff47fd2e2 in PyEval_EvalCode () from /lib64/libpython2.7.so.1.0
#58 0x00007ffff481671f in ?? () from /lib64/libpython2.7.so.1.0
#59 0x00007ffff48178de in PyRun_FileExFlags () from /lib64/libpython2.7.so.1.0
#60 0x00007ffff47f481c in ?? () from /lib64/libpython2.7.so.1.0
#61 0x00007ffff47fbbc4 in PyEval_EvalFrameEx () from /lib64/libpython2.7.so.1.0
#62 0x00007ffff47fd1dd in PyEval_EvalCodeEx () from /lib64/libpython2.7.so.1.0
#63 0x00007ffff47fd2e2 in PyEval_EvalCode () from /lib64/libpython2.7.so.1.0
#64 0x00007ffff481671f in ?? () from /lib64/libpython2.7.so.1.0
#65 0x00007ffff4817585 in PyRun_StringFlags () from /lib64/libpython2.7.so.1.0
#66 0x00007ffff4818f2b in PyRun_SimpleStringFlags () from /lib64/libpython2.7.so.1.0
#67 0x000000000047a5a5 in main (argc=1, argv=0x7fffffffd888) at Qt/Qt_Main.cpp:964
(gdb) 
*/


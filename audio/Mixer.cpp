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

#include "Jack_plugin_proc.h"
#include "SoundfileSaver_proc.h"
#include "get_windows_commandlines_proc.h"

#include "SoundProducer_proc.h"
#include "SoundPluginRegistry_proc.h"

#include "Mixer_proc.h"

extern PlayerClass *pc;
extern int num_users_of_keyboard;

static int g_last_set_producer_buffersize;
static RSemaphore *g_freewheeling_has_started = NULL;

#include <xmmintrin.h>

#if 0

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
#endif


// Mutex and sleeping (copied from RtAudio.h, RtAudio.cpp and tests/qmidiin.cpp)

#if defined(FOR_WINDOWS)

  #include <windows.h>
  #include <process.h>

  #define LockType CRITICAL_SECTION
  #define LOCK_INITIALIZE(A) InitializeCriticalSection(&A)
  #define LOCK_DESTROY(A)    DeleteCriticalSection(&A)
  #define LOCK_LOCK(A)       EnterCriticalSection(&A)
  #define LOCK_UNLOCK(A)     LeaveCriticalSection(&A)

#elif defined(__linux__) || defined(FOR_MACOSX)

  #include <pthread.h>
  #include <unistd.h>

  // pthread API
  #define LockType pthread_mutex_t
  #define LOCK_INITIALIZE(A) pthread_mutex_init(&A, NULL)
  #define LOCK_DESTROY(A)    pthread_mutex_destroy(&A)
  #define LOCK_LOCK(A)       pthread_mutex_lock(&A)
  #define LOCK_UNLOCK(A)     pthread_mutex_unlock(&A)

#else

# error "unkwnonw architantaiehnr"

#endif

jack_client_t *g_jack_client;
static int g_jack_client_priority;
float g_cpu_usage = 0.0f;

#if defined(FOR_WINDOWS)

#  include <windows.h>
#  include <comutil.h>

static void PLAYER_acquire_same_priority(void){
  //printf("Setting real time priority temporarily for %p.\n",(void*)pthread_self());
  //#warning "does this work?"
  jack_acquire_real_time_scheduling(GetCurrentThread(),g_jack_client_priority);
}	

static void PLAYER_drop_same_priority(void){
  jack_drop_real_time_scheduling(GetCurrentThread());
}

#endif // defined(FOR_WINDOWS)

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


#if defined(__linux__) || defined(FOR_MACOSX)

static void PLAYER_acquire_same_priority(void){
  //printf("Setting real time priority temporarily for %p.\n",(void*)pthread_self());
  jack_acquire_real_time_scheduling(pthread_self(),g_jack_client_priority);
}	

static void PLAYER_drop_same_priority(void){
  jack_drop_real_time_scheduling(pthread_self());
}

#endif // defined(__linux__) || defined(FOR_MACOSX)


static LockType player_lock;

// This function will deadlock if called from the player thread!
void PLAYER_lock(void){
  PLAYER_acquire_same_priority();
  LOCK_LOCK(player_lock);
}

void PLAYER_unlock(void){
  LOCK_UNLOCK(player_lock);
  PLAYER_drop_same_priority();
}

static void init_player_lock(){
  LOCK_INITIALIZE(player_lock);
}


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
                                "Download Jack from http://www.jackaudio.org");
      
      msgBox.setStandardButtons(QMessageBox::Ok);
      msgBox.exec();
      num_users_of_keyboard--;
      return false;
    }

    _sample_rate = jack_get_sample_rate(_rjack_client);
    _buffer_size = jack_get_buffer_size(_rjack_client);
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


  void RT_thread(void){
    //AVOIDDENORMALS;

    // Denormal handling. These two lines are copied from supernova by Tim Blechmann.
    _MM_SET_FLUSH_ZERO_MODE(_MM_FLUSH_ZERO_ON);
    _mm_setcsr(_mm_getcsr() | 0x40);

#if 0
#define CSR_FLUSH_TO_ZERO         (1 << 15)
    unsigned csr = __builtin_ia32_stmxcsr();
    csr |= CSR_FLUSH_TO_ZERO;
    __builtin_ia32_ldmxcsr(csr);
#endif

    while(true){

      // Schedule new notes, etc.
      LOCK_LOCK(player_lock);{ // This is a RT-safe lock. Priority inversion can not happen.
        PlayerTask(_buffer_size); // The editor player.
      }LOCK_UNLOCK(player_lock);
      

      // Wait for our jack cycle
      jack_nframes_t num_frames = jack_cycle_wait(_rjack_client);
      if((int)num_frames!=_buffer_size)
        printf("What???\n");


      // Process sound.
      LOCK_LOCK(player_lock);{

        jack_time_t start_time = jack_get_time();
        
        if(_bus1!=NULL)
          SP_RT_process(_bus1,_time,num_frames);
        if(_bus2!=NULL)
          SP_RT_process(_bus2,_time,num_frames);
        
        {
          DoublyLinkedList *sound_producer = _sound_producers.next;
          while(sound_producer!=NULL){
            SP_RT_process((SoundProducer*)sound_producer,_time,num_frames);
            sound_producer = sound_producer->next;
          }
        }
        
        _time += num_frames;
      
        jack_time_t end_time = jack_get_time();
        g_cpu_usage = (double)(end_time-start_time) * 0.0001 *_sample_rate / num_frames;

      }LOCK_UNLOCK(player_lock);

      // Tell jack we are finished.
      jack_cycle_signal(_rjack_client, 0);
    }
  }
      

  static void *RT_rjack_thread(void *arg){
    Mixer *mixer = static_cast<Mixer*>(arg);
    //printf("RT_rjack_process called %d\n",num_frames);
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

    LOCK_LOCK(player_lock);{  // Not sure which thread this callback is called from. But since the player thread can not run here anyway, it's safest not to use PLAYER_lock(). (If it's called from the realtime thread, PLAYER_unlock() would set the priority of the realtime thread to non-realtime.)
      mixer->_buffer_size = num_frames;

      DoublyLinkedList *sound_producer = mixer->_sound_producers.next;
      while(sound_producer!=NULL){
        SP_set_buffer_size((SoundProducer*)sound_producer, mixer->_buffer_size);
        sound_producer = sound_producer->next;
      }
    }LOCK_UNLOCK(player_lock);

    return 0;
  }

};

static Mixer *g_mixer;

bool MIXER_start(void){

  init_player_lock();
  g_freewheeling_has_started = RSEMAPHORE_create(0);

  g_mixer = new Mixer();

  if(g_mixer->start_jack()==false)
    return false;

  PR_init_plugin_types();

  check_jackd_arguments();

  return true;
}

static STime startup_time = 0;
void OS_InitAudioTiming(void){
  startup_time = g_mixer->_time;
  printf("OS_InitAudioTiming called. New time: %d\n",(int)startup_time);
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

STime MIXER_get_block_delta_time(STime time){
  return (time+startup_time) - g_mixer->_time;
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
  return g_mixer->_buffer_size;
}


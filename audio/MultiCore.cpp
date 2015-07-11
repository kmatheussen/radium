#include <QThread>
#include <QMutex>
#include <QMutexLocker>
#include <QWaitCondition>

#include "../common/nsmtracker.h"
#include "../common/threading.h"

#include "SoundProducer_proc.h"

#include "MultiCore_proc.h"


const static int num_runners = 8;

static RSemaphore *there_is_a_free_runner;

static QMutex lock;
static QWaitCondition broadcast;

bool g_running_multicore = true;

namespace{
  

struct Runner;

static Runner *running_runners; // access to these two are protected by 'lock'.
static Runner *free_runners;

static void add_runner(Runner **root, Runner *runner);
static void remove_runner(Runner **root, Runner *runner);

struct Runner : public QThread {
  //  Q_OBJECT

public:
  
  Runner *prev;
  Runner *next;
  
  SoundProducer *sp;
  int64_t time;
  int num_frames;
  bool process_plugins;

  RSemaphore *ready;

  Runner() {
    ready = RSEMAPHORE_create(0);

    start(QThread::TimeCriticalPriority); // The priority shouldn't matter though since PLAYER_acquire_same_priority() is called inside run().
  }

  ~Runner() {
    RSEMAPHORE_delete(ready);
  }

  
private:
  
  void run() {

    AVOIDDENORMALS;
        
    THREADING_acquire_player_thread_priority();

    while(true){
      RSEMAPHORE_wait(ready, 1);
      
      SP_RT_process(sp, time, num_frames, process_plugins);

      SP_set_running_state(sp, FINISHED_RUNNING);
      
      {
        QMutexLocker locker(&lock);
        remove_runner(&running_runners, this);
        add_runner(&free_runners, this);

        RSEMAPHORE_signal(there_is_a_free_runner, 1);
        broadcast.wakeOne(); // Only the main player thread may be waiting.
      }

    }
  }

  
public:
  
  void start_rt_process() {
    RSEMAPHORE_signal(ready, 1);
  }
};




static void add_runner(Runner **root, Runner *runner){
  runner->next = (Runner*)*root;
  if(*root!=NULL)
    (*root)->prev = runner;
  *root = runner;
  runner->prev = NULL;
}

static void remove_runner(Runner **root, Runner *runner){
  if(runner->prev!=NULL)
    runner->prev->next = runner->next;
  else
    *root=runner->next;

  if(runner->next!=NULL)
    runner->next->prev = runner->prev;
}



static Runner *get_free_runner(void){
  R_ASSERT(THREADING_is_player_thread());
  
  R_ASSERT(free_runners!=NULL);

  Runner *ret;

  {
    QMutexLocker locker(&lock);
    
    ret = (Runner*)free_runners;

    remove_runner(&free_runners, ret);
    add_runner(&running_runners, ret);
    
  }

  return ret;
}


} // end anonymous namespace




static void schedule(SoundProducer *sp, int64_t time, int num_frames, bool process_plugins){
  R_ASSERT(THREADING_is_player_thread());

  RSEMAPHORE_wait(there_is_a_free_runner, 1);

  Runner *runner = get_free_runner();

  runner->sp = sp;
  runner->time = time;
  runner->num_frames = num_frames;
  runner->process_plugins = process_plugins;

  SP_set_running_state(sp, IS_RUNNING);

  runner->start_rt_process();

}

#if 0
static bool sp_is_bus_dependant(SoundProducer *sp){
  SoundPlugin *plugin = SP_get_plugin(sp);

  if (plugin->bus_descendant_type==IS_NOT_A_BUS_DESCENDANT) {
    int bus_num = SP_get_bus_num(sp);
    Smooth *smooth = &plugin->bus_volume[bus_num];
    if (SMOOTH_are_we_going_to_modify_target_when_mixing_sounds_questionmark(smooth))
      return true;
  }

  return false;
}
#endif

static void run_soundproducers(SoundProducer *all_sp, int64_t time, int num_frames, bool process_plugins, BusDescendantType bus_descendant_type){

  bool all_are_scheduled = false;

  while(all_are_scheduled==false) {
    
    all_are_scheduled = true; // Start optimistically
  
    SoundProducer *sp = all_sp;
    
    while(sp != NULL) {
        
      if (SP_get_running_state(sp)==HASNT_RUN_YET) {

        SoundPlugin *plugin = SP_get_plugin(sp);
        
        if (plugin->bus_descendant_type==bus_descendant_type) {
          SoundProducer *ready_to_process = SP_get_ready_to_process(sp);

          if (ready_to_process != sp)
            all_are_scheduled = false;

          if (ready_to_process != NULL)
            schedule(ready_to_process, time, num_frames, process_plugins);
        }
      }
      
      sp = SP_next(sp);
    }
  
    if (all_are_scheduled==false) {
      QMutexLocker locker(&lock);
      if (free_runners==NULL)
        broadcast.wait(&lock, 1000);
    }

  }

  // Wait for all runners to finish.
  {
    QMutexLocker locker(&lock);
    while(running_runners != NULL)
      broadcast.wait(&lock, 1000);
  }

}

void MULTICORE_run_all(SoundProducer *all_sp, int64_t time, int num_frames, bool process_plugins){

  R_ASSERT(g_running_multicore);

  
  // 1. Set running state for all soundproducers

  {
    SoundProducer *sp = all_sp;

    while(sp != NULL) {
      SP_set_running_state(sp, HASNT_RUN_YET);
      sp = SP_next(sp);
    }
  }


  // 2. run all soundproducers.

  run_soundproducers(all_sp, time, num_frames, process_plugins, IS_NOT_A_BUS_DESCENDANT);
  run_soundproducers(all_sp, time, num_frames, process_plugins, IS_BUS_DESCENDANT);

  #if 0
    // Wait for all runners to finish.
  {
    QMutexLocker locker(&lock);
    while(running_runners != NULL)
      broadcast.wait(&lock, 1000);
  }
#endif
}


void MULTICORE_init(void){

  there_is_a_free_runner = RSEMAPHORE_create(0);

  for(int i=0 ; i < num_runners ; i++) {
    add_runner(&free_runners, new Runner);
    RSEMAPHORE_signal(there_is_a_free_runner, 1);
  }
  
}

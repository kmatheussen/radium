// This file is not compiled directly, but #included into SoundProducer.cpp


#include <QThread>

//QAtomicInt g_num_waits(0);

#include "../common/nsmtracker.h"
#include "../common/visual_proc.h"
#include "../common/threading.h"
#include "../common/stacktoucher_proc.h"
#include "../common/settings_proc.h"
#include "../common/Semaphores.hpp"
#include "../common/Queue.hpp"

#include "../common/OS_Player_proc.h"

#include "SoundProducer_proc.h"


#include "MultiCore_proc.h"

//#undef R_ASSERT
//#define R_ASSERT(a) do{ if(!(a)){ fflush(stderr);fprintf(stderr,">>>>>>>>>>>>>>>>>>> Assert failed: \"" # a "\". %s: " __FILE__":%d\n", __FUNCTION__, __LINE__);something_is_wrong=true;}}while(0)



static volatile bool something_is_wrong = false;

static const char *settings_key = "num_cpus";


static radium::Semaphore all_sp_finished;

static DEFINE_ATOMIC(int, num_sp_left) = 0;

#define MAX_NUM_SP 8192

static radium::Queue< SoundProducer* , MAX_NUM_SP > soundproducer_queue;

static void dec_sp_dependency(const SoundProducer *parent, SoundProducer *sp, SoundProducer **next){
  if (ATOMIC_ADD_RETURN_NEW(sp->num_dependencies_left, -1) == 0) {
    if (*next == NULL)
      *next = sp;
    else
      soundproducer_queue.put(sp);
  }
}

static void process_soundproducer(SoundProducer *sp, int64_t time, int num_frames, bool process_plugins){

 again:
  
  R_ASSERT(sp!=NULL);

  //  fprintf(stderr,"   Processing %p: %s %d\n",sp,sp->_plugin->patch==NULL?"<null>":sp->_plugin->patch->name,int(sp->is_processed));
  //fflush(stderr);
  
  bool old = ATOMIC_SET_RETURN_OLD(sp->is_processed, true);
  R_ASSERT(old==false);
  
  double start_time = monotonic_seconds();
  {
    sp->RT_process(time, num_frames, process_plugins);
  }
  double duration = monotonic_seconds() - start_time;
  if (duration > sp->running_time)
    sp->running_time = duration;

  //int num_left = num_sp_left;
  //printf("num_left2: %d\n",num_left);

  SoundProducer *next = NULL;
  
  for(SoundProducerLink *link : sp->_output_links)
    if (ATOMIC_GET_RELAXED(link->is_active)) // We can relax since is_active can only be set in the main mixer thread before running the multicore threads (But why is it atomic then?)
      dec_sp_dependency(link->source, link->target, &next);

  // Important that we decrease 'num_sp_left' AFTER scheduling other soundproducers for processing. (i.e. calling when 'dec_sp_dependency')
  if (ATOMIC_ADD_RETURN_NEW(num_sp_left, -1) == 0) {
    R_ASSERT(next==NULL);
    all_sp_finished.signal();
    return;
  }
  
  if (next != NULL){
    sp = next;
    next = NULL;
    goto again;
  }
}



namespace{

  
struct Runner : public QThread {
  Q_OBJECT

  
  Runner(const Runner&) = delete;
  Runner& operator=(const Runner&) = delete;


public:
  radium::Semaphore can_start_main_loop;
  DEFINE_ATOMIC(bool, must_exit);

  int64_t time;
  int num_frames;
  bool process_plugins;

  Runner()
  {
    ATOMIC_SET(must_exit, false);
    QObject::connect(this, SIGNAL(finished()), this, SLOT(onFinished()));
    start(QThread::NormalPriority); //QThread::TimeCriticalPriority); // The priority shouldn't matter though since PLAYER_acquire_same_priority() is called inside run().
  }

  void run(){
    AVOIDDENORMALS;

    touch_stack();

    setPriority(QThread::TimeCriticalPriority); // shouldn't matter, but just in case the call below is not working, for some reason.
  
    THREADING_acquire_player_thread_priority();

    can_start_main_loop.wait();

    while(true){
      
      SoundProducer *sp = soundproducer_queue.get();
      if (ATOMIC_GET(must_exit)) {
        soundproducer_queue.put(sp);
        break;
      }
            
      process_soundproducer(sp, time, num_frames, process_plugins);
    }


    THREADING_drop_player_thread_priority();    
  }

private slots:
  void onFinished(){
    //printf("\n\n\n ***************** FINISHED ****************** \n\n\n\n");
    delete this;
  }

};
#include "mMultiCore.cpp"
}


static void process_single_core(int64_t time, int num_frames, bool process_plugins){
  while( ATOMIC_GET_RELAXED(num_sp_left) > 0 ) {
    // R_ASSERT(sp_ready.numSignallers()>0); // This assert can sometimes fail if there are still running runners with must_exit==true.
    SoundProducer *sp = soundproducer_queue.get();    
    process_soundproducer(sp, time, num_frames, process_plugins);
  }

  R_ASSERT(all_sp_finished.numSignallers()==1);
  
  all_sp_finished.wait();
}


static int g_num_runners = 0;
static Runner **g_runners = NULL;


void MULTICORE_run_all(const radium::Vector<SoundProducer*> &sp_all, int64_t time, int num_frames, bool process_plugins){

  if (sp_all.size()==0)
    return;

  if (sp_all.size() >= MAX_NUM_SP){
    // Not doing anything in the audio thread might cause a deadlock, so the message window might now show if using RT_Message here.
    GFX_Message(NULL,
                "Maximum number of sound objects reached. Tried to play %d sound objects, but only %d sound objects are supported. Radium must be recompiled to increase this number.",
                sp_all.size(),
                MAX_NUM_SP);
    return;
  }

  int num_ready_sp = 0;


  // 1. Initialize threads

  for(int i=0;i<g_num_runners;i++){
    g_runners[i]->time = time;
    g_runners[i]->num_frames = num_frames;
    g_runners[i]->process_plugins = process_plugins;
  }


  
  // 2. initialize soundproducers
  
  ATOMIC_SET(num_sp_left, sp_all.size());
  
  //  fprintf(stderr,"**************** STARTING %d\n",sp_all.size());
  //fflush(stderr);

  for (SoundProducer *sp : sp_all) {
    ATOMIC_SET(sp->num_dependencies_left, sp->num_dependencies);
    ATOMIC_SET(sp->is_processed, false); // is this really necessary? When could it be true?
  }

  
#if 0
  SP_print_tree();
#endif

  //int num_waits_start = int(g_num_waits);
  

  SoundProducer *sp_in_main_thread = NULL;
      
  // 3. start threads;

  for (SoundProducer *sp : sp_all)
    if (sp->num_dependencies==0){
      if (sp_in_main_thread == NULL && g_num_runners>0){
        sp_in_main_thread = sp;
      } else {
        num_ready_sp++;
        //fprintf(stderr,"Scheduling %p: %s\n",sp,sp->_plugin->patch==NULL?"<null>":sp->_plugin->patch->name);
        //fflush(stderr);
        soundproducer_queue.putWithoutSignal(sp);
      }
    }
  if(num_ready_sp > 0)
    soundproducer_queue.signal(num_ready_sp); // signal everyone at once to try to lower number of semaphore waits. Doesn't seem to make a difference on my machine though.


  if (g_num_runners==0) {

    R_ASSERT(num_ready_sp > 0);

    process_single_core(time, num_frames, process_plugins);
    
  } else {

    R_ASSERT(sp_in_main_thread!=NULL);

    // 4. process as much as we can in the main thread
    while(sp_in_main_thread != NULL){
      process_soundproducer(sp_in_main_thread, time, num_frames, process_plugins);

      bool gotit;
      sp_in_main_thread = soundproducer_queue.tryGet(gotit); // This is not perfect. It's likely that we fail to get a soundproducer before everything is done.
      if (!gotit)
        sp_in_main_thread = NULL;
    }

    // 5. wait.
    all_sp_finished.wait();
  }
  
  //int num_waits_end = int(g_num_waits);

  //printf("num_waits: %d, %d\n",int(g_num_waits),num_waits_end-num_waits_start);
  
  if(something_is_wrong){
    fflush(stderr);
    abort();
  }

}



#if 0
void MULTICORE_stop(void){
  R_ASSERT(running_runners==NULL);

  while(free_runners != NULL){
    Runner *next = free_runners->next;
    delete free_runners;
    free_runners = next;
  }

  free_runners = NULL;
}
#endif


int MULTICORE_get_num_threads(void){
  if (g_num_runners==0)
    return 1;
  else
    return g_num_runners;
}


static int get_num_runners_from_config(void){
  static int default_num_runners = -1;

  if (default_num_runners==-1)
    default_num_runners = QThread::idealThreadCount();

  if (default_num_runners<1 || default_num_runners>64) // ensure sane value
    default_num_runners = 4;

  return SETTINGS_read_int(settings_key, default_num_runners);
}

void MULTICORE_set_num_threads(int num_new_runners){  
  R_ASSERT(num_new_runners >= 1);

  if (get_num_runners_from_config() != num_new_runners)
    SETTINGS_write_int(settings_key, num_new_runners);

  if (num_new_runners==1)
    num_new_runners=0;
  
  if (num_new_runners==g_num_runners)
    return;

  int num_old_runners = g_num_runners;
  Runner **old_runners = g_runners;
  
  Runner **new_runners = (Runner**)V_calloc(num_new_runners,sizeof(Runner*));

  // start them
  for(int i=0 ; i < num_new_runners ; i++)
    new_runners[i]=new Runner;

  // wait until they are ready (takes some time, but if we don't wait here, there will be a moment where there are no active runners, and no sound)
  for(int i=0 ; i < num_new_runners ; i++)
    while(new_runners[i]->can_start_main_loop.numWaiters()==0)
      usleep(1000);

  PLAYER_lock(); {

    for(int i=0 ; i < num_new_runners ; i++)
      new_runners[i]->can_start_main_loop.signal();

    g_num_runners = num_new_runners;
    g_runners = new_runners;

    for(int i=0 ; i < num_old_runners ; i++)
      ATOMIC_SET(old_runners[i]->must_exit, true);

  } PLAYER_unlock();


  V_free(old_runners);
}

void MULTICORE_shut_down(void){
  for(int i=0 ; i < g_num_runners ; i++)
    ATOMIC_SET(g_runners[i]->must_exit, true);
}

void MULTICORE_init(void){

  R_ASSERT(g_num_runners==0);
  
  int num_new_runners = get_num_runners_from_config();

  MULTICORE_set_num_threads(num_new_runners);
}

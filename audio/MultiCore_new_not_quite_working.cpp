// This file is not compiled directly, but #included into SoundProducer.cpp


#include <QThread>

//QAtomicInt g_num_waits(0);

#include "../common/nsmtracker.h"
#include "../common/spinlock.h"
#include "../common/visual_proc.h"
#include "../common/threading.h"
#include "../common/stacktoucher_proc.h"
#include "../common/settings_proc.h"
#include "../common/Semaphores.hpp"
#include "../common/DoublyLinkedList.hpp"

#include "../common/OS_Player_proc.h"

#include "../midi/midi_i_input_proc.h"

#include "SoundProducer_proc.h"


#include "MultiCore_proc.h"

static const char *settings_key = "num_cpus";

//static radium::SpinlockSemaphore all_sp_finished;
static DEFINE_ATOMIC(int, g_start_block_time) = 0;
const int g_num_blocks_to_pause_buzylooping = 400; // Should be some seconds
static DEFINE_ATOMIC(int, g_num_blocks_pausing_buzylooping) = 0;


static DEFINE_ATOMIC(int, g_num_sp_left) = 0;

DEFINE_ATOMIC(bool, g_buzy_get_is_enabled) = false;

#if !defined(RELEASE)
#  include "../common/LockAsserter.hpp"
   static radium::LockAsserter g_lockAsserter;
#  define ASSERT_SHARED LOCKASSERTER_SHARED(&g_lockAsserter)
#  define ASSERT_EXCLUSIVE LOCKASSERTER_EXCLUSIVE(&g_lockAsserter)
#else
#  define ASSERT_SHARED
#  define ASSERT_EXCLUSIVE
#endif


namespace{


static int g_num_runners = 0;

struct Runner;
static Runner **g_runners = NULL;


static void avoid_lockup(int counter){
  if ((counter % (1024*64)) == 0){
    int time_now = (int)(1000*monotonic_seconds());
    int duration = time_now - ATOMIC_GET_RELAXED(g_start_block_time);
    //printf("  Duration: %d\n", duration);
    if (duration >= 1000){
      ATOMIC_SET(g_num_blocks_pausing_buzylooping, g_num_blocks_to_pause_buzylooping);
      RT_message("We have used more than 1 second to process an audio block. Something is wrong.\n"
                 "\n"
                 "It is likely that the number of CPUs used for audio processing is set too high.\n"
                 "\n"
                 "You can fix this by reducing the number of CPUs under Edit -> Preferences -> Audio -> Multi Core\n" 
                 "\n"
                 "To avoid locking up the computer, we're going to lighten up the strain on the CPU for a few seconds. "
                 "Note that the audio performance will suffer because of this."
                 );
    }
  }
}


#define MAX_OWNERS 16

struct Owner;
static Owner *g_owners;


struct Owner{

  radium::Spinlock _sp_lock;
  //radium::Spinlock _sp_next_lock; // tsan claims that this lock is necessary, but I have my doubts. It shouldn't make any difference on the performance though.

  // In the beginning, the sp order might be pretty bad, but this should self-adjust immediately. At least to a certain degree.
  //
  radium::DoublyLinkedList<SoundProducer> _now_sp;  // must hold _sp_lock.
  radium::DoublyLinkedList<SoundProducer> _next_sp; // must hold _sp_next_lock. _next_sp is transfered into _now_sp, and _next_sp cleared after block ends. Only accessed by one thread at a time, so no need for locking.

  enum{
    OWNER_LOOKING,
    OWNER_PROCESSING,
    OWNER_WAITING_BIG_BLOCK,
    OWNER_WAITING_SMALL_BLOCK,
    OWNER_PAUSING_TO_AVOID_LOCKING_CPU,
    OWNER_NOT_RUNNING
  };

  DEFINE_ATOMIC(int, _status) = OWNER_NOT_RUNNING;

  int _owner_num; // Set in MULTICORE_init

  Owner()
  {    
  }

  void flip(void){
    R_ASSERT_NON_RELEASE(_now_sp._first==NULL);
    R_ASSERT_NON_RELEASE(_now_sp._last==NULL);

    //radium::ScopedSpinlock lock1(_sp_lock);
    //radium::ScopedSpinlock lock2(_sp_next_lock);

    _now_sp.set(_next_sp);
    _next_sp.clear();
  }

  SoundProducer *RT_get_next_my_own_soundproducer(void){
    radium::ScopedSpinlock lock(_sp_lock);

    SoundProducer *sp = _now_sp._first;
    
    while (sp != NULL){
      
      if (ATOMIC_GET(sp->num_dependencies_left)==0) {
        
        _now_sp.remove(sp);
        
        return sp;
        
      }
      
      sp = sp->dll_next;
    }

    return NULL;
  }

  bool RT_can_steal(void) const {
    int owner_status = ATOMIC_GET(_status);
    if (owner_status==OWNER_PROCESSING || owner_status==OWNER_PAUSING_TO_AVOID_LOCKING_CPU || owner_status==OWNER_NOT_RUNNING)
      return true;
    else
      return false;
  }

  SoundProducer *RT_steal_soundproducer_from_me(void){
    radium::ScopedTrySpinlock lock(_sp_lock);

    if (lock.gotit()==false)
      return NULL;

    SoundProducer *sp = _now_sp._last; // Search from the end as an attempt to try fixing bad order
    //SoundProducer *sp = _now_sp._first;

    while (sp != NULL){

      if (ATOMIC_GET(sp->num_dependencies_left)==0) {

#if 1
          _now_sp.remove(sp);

          sp->downcounter = 1024;

          return sp;
#else
        // Seems to reduce performance.

        sp->downcounter--;
        if (sp->downcounter==0){//|| not the last one){

          _now_sp.remove(sp);

          sp->downcounter = 1024;

          return sp;

        } else {

          return NULL;

        }
#endif

      }

      if (!RT_can_steal())
        return NULL;

      sp = sp->dll_prev;
      //sp = sp->dll_next;
    }

    return NULL;
  }

  SoundProducer *RT_get_next_soundproducer(void){

    SoundProducer *sp = RT_get_next_my_own_soundproducer();
    if (sp!=NULL){
      R_ASSERT_NON_RELEASE(ATOMIC_GET(sp->is_processed)==false);
      R_ASSERT_NON_RELEASE(ATOMIC_GET(g_num_sp_left)>0);
      return sp;
    }

    int num_left_top = ATOMIC_GET_RELAXED(g_num_sp_left);
      
    // We have no free soundproducers ourself. Try to steal from another owner instead.
    for(int i = 0 ; i < MAX_OWNERS ; i++) {
      if ( i != _owner_num){
        Owner &owner = g_owners[i];
        if (owner.RT_can_steal()){
          // This owner is not currently looking for new soundproducers to process, so lets try to steal a soundproducer from it.
          SoundProducer *sp = owner.RT_steal_soundproducer_from_me();
          if (sp!=NULL){
            R_ASSERT_NON_RELEASE(ATOMIC_GET(sp->is_processed)==false);
            R_ASSERT_NON_RELEASE(ATOMIC_GET(g_num_sp_left)>0);
            //sp->owner = this;

            //printf("     Owner %d stole \"%s\" from %d\n", _owner_num, sp->_plugin->patch->name, i);

            return sp;
          }

          if (ATOMIC_GET_RELAXED(g_num_sp_left) != num_left_top){ // Something has happened. Try to get my own again.
            SoundProducer *sp = RT_get_next_my_own_soundproducer();
            if (sp!=NULL){
              R_ASSERT_NON_RELEASE(ATOMIC_GET(sp->is_processed)==false);
              R_ASSERT_NON_RELEASE(ATOMIC_GET(g_num_sp_left)>0);
              return sp;
            }
            num_left_top = ATOMIC_GET_RELAXED(g_num_sp_left);
          }
        }
      }
    }

    return NULL;
  }

  void dec_sp_dependency(SoundProducer *target){
#if defined(RELEASE)
    ATOMIC_ADD(target->num_dependencies_left, -1);
#else
    int num_left = ATOMIC_ADD_RETURN_NEW(target->num_dependencies_left, -1);
    R_ASSERT(num_left >= 0);
#endif
  }

  void RT_process_soundproducer(SoundProducer *sp, int64_t time, int num_frames, bool process_plugins){
  
    R_ASSERT_NON_RELEASE(sp!=NULL);

    //fprintf(stderr,"   Processing %p: %s %d. Owner: %d\n",sp,sp->_plugin->patch==NULL?"<null>":sp->_plugin->patch->name,ATOMIC_GET(sp->is_processed),_owner_num);
    //fflush(stderr);

#if !defined(RELEASE)  
    bool old = ATOMIC_SET_RETURN_OLD(sp->is_processed, true);
    R_ASSERT(old==false);
#endif

    bool autosuspend = RT_PLUGIN_can_autosuspend(sp->_plugin, time);

    // We don't autosuspend current patch when not playing.
    if (autosuspend && !is_playing()){
      struct Patch *current_patch = ATOMIC_GET(g_through_patch);
      if (sp->_plugin->patch == current_patch)
        autosuspend = false;
    }
      
    sp->_autosuspending_this_cycle = autosuspend;
    ATOMIC_SET_RELAXED(sp->_is_autosuspending, sp->_autosuspending_this_cycle);
  
    if ( ! sp->_autosuspending_this_cycle) {
      ATOMIC_SET(_status, OWNER_PROCESSING);
      SP_RT_process(sp, time, num_frames, process_plugins);
      ATOMIC_SET(_status, OWNER_LOOKING);
    }

    for(SoundProducerLink *link : sp->_output_links)
      if (link->is_active)
        dec_sp_dependency(link->target);

    R_ASSERT_NON_RELEASE(ATOMIC_GET(g_num_sp_left)>0); // This one sometimes fails!
    
    {
      //radium::ScopedSpinlock lock(_sp_next_lock);
      _next_sp.push_back(sp);
    }

    {
      ASSERT_SHARED;
    }

#if defined(RELEASE)
    ATOMIC_ADD(g_num_sp_left, -1);
#else
    R_ASSERT(ATOMIC_ADD_RETURN_OLD(g_num_sp_left, -1) >= 0);
#endif

  }

};
  
static Owner g_owners2[MAX_OWNERS];  // [NO_STATIC_ARRAY_WARNING]


#if 0
static void bound_thread_to_cpu(int cpu){
  cpu_set_t set;
  CPU_ZERO(&set);
  CPU_SET(cpu,&set);
  pthread_setaffinity_np(pthread_self(), sizeof(cpu_set_t), &set);
  //fprintf(stderr,"pthread_setaffinity_np returned: %d\n",sched_setaffinity(0, sizeof(cpu_set_t), &set));
}
#endif

  
struct Runner : public QThread {
  Q_OBJECT

  
  Runner(const Runner&) = delete;
  Runner& operator=(const Runner&) = delete;


public:
  DEFINE_ATOMIC(bool, must_exit);

  int64_t time;
  int num_frames = 0;
  bool process_plugins;
  Owner *_owner;

  radium::Semaphore _runner_wakeup;
  
  Runner(Owner *owner)
    : _owner(owner)
  {
    ATOMIC_SET(must_exit, false);
    QObject::connect(this, SIGNAL(finished()), this, SLOT(onFinished()));
    start(QThread::NormalPriority); //QThread::TimeCriticalPriority); // The priority shouldn't matter though since PLAYER_acquire_same_priority() is called inside run().
  }

  int _curr_status = Owner::OWNER_NOT_RUNNING;
  void set_owner_status(int new_status){
    if (new_status != _curr_status){
      ATOMIC_SET(_owner->_status, new_status);
      _curr_status = new_status;
    }
  }

  //#define FOR_MACOSX
  void run() override {
    AVOIDDENORMALS;

    touch_stack();

#if 0
    if (g_num_runners==1)
      bound_thread_to_cpu(2);
    else
      bound_thread_to_cpu(_owner->_owner_num);
#endif

    setPriority(QThread::TimeCriticalPriority); // shouldn't matter, but just in case the call below is not working, for some reason.
  
    THREADING_acquire_player_thread_priority();

    while(true){
      int counter = 0;

      SoundProducer *sp = NULL;

      for(;;){

        if (_curr_status == Owner::OWNER_NOT_RUNNING) {
          _runner_wakeup.wait();
            
          if (ATOMIC_GET(must_exit))
            goto exit_thread;

          continue;
        }

        bool is_pausing = ATOMIC_GET_RELAXED(g_num_blocks_pausing_buzylooping) > 0;

        if (!is_pausing && ATOMIC_GET(g_buzy_get_is_enabled)){

          if (ATOMIC_GET(g_num_sp_left) > 0){

            set_owner_status(Owner::OWNER_LOOKING);

            ASSERT_SHARED;

            sp = _owner->RT_get_next_soundproducer();
            R_ASSERT_NON_RELEASE(sp==NULL || ATOMIC_GET(g_num_sp_left)>0);

          } else {

            set_owner_status(Owner::OWNER_WAITING_SMALL_BLOCK);
            
          }

          if (sp!=NULL)
            break;

          avoid_lockup(counter++);
          __asm__ __volatile__("nop");
          
        } else {

          if (is_pausing)
            set_owner_status(Owner::OWNER_PAUSING_TO_AVOID_LOCKING_CPU);
          else
            set_owner_status(Owner::OWNER_WAITING_BIG_BLOCK);

          _runner_wakeup.wait();
            
          if (ATOMIC_GET(must_exit))
            goto exit_thread;
        }

      }


      {
        R_ASSERT_NON_RELEASE(ATOMIC_GET(g_num_sp_left)>0);
        {
          ASSERT_SHARED;
        }
        _owner->RT_process_soundproducer(sp, time, num_frames, process_plugins);
      }
    }

  exit_thread:

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


void MULTICORE_start_block(void){
  ATOMIC_SET(g_start_block_time, (int) (1000*monotonic_seconds()));
  ATOMIC_SET(g_buzy_get_is_enabled, true);

  for(int i=0;i<g_num_runners;i++)
    g_runners[i]->_runner_wakeup.signal();
}

void MULTICORE_end_block(void){
  ATOMIC_SET(g_buzy_get_is_enabled, false);

  int num_blocks_left = ATOMIC_GET(g_num_blocks_pausing_buzylooping);
  if (num_blocks_left > 0)
    ATOMIC_SET(g_num_blocks_pausing_buzylooping, num_blocks_left-1);
}


void MULTICORE_run_all(const radium::Vector<SoundProducer*> &sp_all, int64_t time, int num_frames, bool process_plugins){
#if 0
  static bool has_inited = false;
  if (has_inited==false){
    bound_thread_to_cpu(0);
    has_inited = true;
  }
#endif

  if (sp_all.size()==0)
    return;

  //printf("                      Starting new block. size: %d\n", sp_all.size());

  // 1. Initialize threads (runners)

  R_ASSERT_NON_RELEASE(num_frames>0);

  for(int i=0;i<g_num_runners;i++){
    g_runners[i]->time = time;
    g_runners[i]->num_frames = num_frames;
    g_runners[i]->process_plugins = process_plugins;
  }


  // 2. initialize soundproducers

  //printf(" mc: SET: %d\n",sp_all.size());
  
  //  fprintf(stderr,"**************** STARTING %d\n",sp_all.size());
  //fflush(stderr);

  for (SoundProducer *sp : sp_all) {
    ATOMIC_SET_RELAXED(sp->num_dependencies_left, sp->num_dependencies);
#if !defined(RELEASE)
    ATOMIC_SET(sp->is_processed, false);
#endif
  }

  R_ASSERT_NON_RELEASE(ATOMIC_GET(g_num_sp_left)==0);

  for(int i=1;i<MAX_OWNERS;i++){
    for(;;){
      int status = ATOMIC_GET(g_owners2[i]._status);
      if (status==Owner::OWNER_WAITING_BIG_BLOCK || status==Owner::OWNER_WAITING_SMALL_BLOCK || status==Owner::OWNER_NOT_RUNNING || status==Owner::OWNER_PAUSING_TO_AVOID_LOCKING_CPU)
        break;

      __asm__ __volatile__("nop");
    }
  }

  {
    ASSERT_EXCLUSIVE;

    // prepare more
    for(int i=0;i<MAX_OWNERS;i++){
      g_owners2[i].flip();
    }
  }
    
  // start runners
  ATOMIC_SET(g_num_sp_left, sp_all.size());

  Owner &owner = g_owners[0];

  // Process owners 0
  int counter = 0;
  while( ATOMIC_GET(g_num_sp_left) > 0 ) {
    SoundProducer *sp = owner.RT_get_next_soundproducer();
    if (sp != NULL)
      owner.RT_process_soundproducer(sp, time, num_frames, process_plugins);
    else{
      avoid_lockup(counter++);      
      __asm__ __volatile__("nop");
    }
  }

  //printf("num_waits: %d, %d\n",int(g_num_waits),num_waits_end-num_waits_start);
}

void MULTICORE_add_sp(SoundProducer *sp){
  ASSERT_EXCLUSIVE;

  R_ASSERT(PLAYER_current_thread_has_lock());
  //radium::ScopedSpinlock lock(g_owners2[0]._sp_next_lock);
  g_owners2[0]._next_sp.push_back(sp);
}

void MULTICORE_remove_sp(SoundProducer *sp){
  ASSERT_EXCLUSIVE;

  R_ASSERT(PLAYER_current_thread_has_lock());

  for(int i=0;i<MAX_OWNERS;i++){
    //radium::ScopedSpinlock lock(g_owners2[i]._sp_next_lock);
    if (g_owners2[i]._next_sp.in_list(sp)){
      g_owners2[i]._next_sp.remove(sp);
      return;
    }
  }

  RError("MULTICORE_remove_sp. Unable to find sp %p\n", sp);
}


int MULTICORE_get_num_threads(void){
  return g_num_runners+1;
}


static int get_num_cpus_from_config(void){
  static int default_num_cpus = -1;

  if (default_num_cpus==-1)
    default_num_cpus = QThread::idealThreadCount();

  if (default_num_cpus<1 || default_num_cpus>64) // ensure sane value
    default_num_cpus = 1;

  return SETTINGS_read_int32(settings_key, default_num_cpus);
}


static void RT_set_runner_owner_statuses(void){
  for(int i=1;i<MAX_OWNERS;i++){
    //radium::ScopedSpinlock lock(g_owners2[i]._sp_next_lock);
    Runner *runner = g_runners[i-1];
    
    if (i <= g_num_runners)
      runner->set_owner_status(Owner::OWNER_WAITING_BIG_BLOCK);
    else
      runner->set_owner_status(Owner::OWNER_NOT_RUNNING);
  }
}

void MULTICORE_set_num_threads(int num_new_cpus){
  R_ASSERT_RETURN_IF_FALSE(num_new_cpus >= 1);

  //GFX_Message(NULL, "Setting to %d", num_new_cpus);
  if (num_new_cpus > MAX_OWNERS)
    num_new_cpus = MAX_OWNERS;

  if (get_num_cpus_from_config() != num_new_cpus)
    SETTINGS_write_int(settings_key, num_new_cpus);

  int num_new_runners = num_new_cpus - 1;
  
  if (num_new_runners==g_num_runners)
    return;

  PLAYER_lock(); {

    g_num_runners = num_new_runners;

    RT_set_runner_owner_statuses();
    
  } PLAYER_unlock();
}


static void MULTICORE_start_threads(void){

  Runner **new_runners = (Runner**)V_calloc(MAX_OWNERS-1,sizeof(Runner*));

  // start them
  for(int i=0 ; i < MAX_OWNERS-1 ; i++)
    new_runners[i]=new Runner(&g_owners2[i+1]);

  int num_new_cpus = get_num_cpus_from_config();


  PLAYER_lock(); {

    g_runners = new_runners;
    g_num_runners = num_new_cpus - 1;

    RT_set_runner_owner_statuses();

  } PLAYER_unlock();
}

void MULTICORE_shut_down(void){
  for(int i=0 ; i < g_num_runners ; i++){
    ATOMIC_SET(g_runners[i]->must_exit, true);
    g_runners[i]->_runner_wakeup.signal();
  }
  g_num_runners = 0;
  for(int i=0 ; i < g_num_runners ; i++){
    g_runners[i]->wait(2000);
  }
}

void MULTICORE_init(void){

  R_ASSERT(g_num_runners==0);

  for(int i=0;i<MAX_OWNERS;i++)
    g_owners2[i]._owner_num = i;

  g_owners = g_owners2;
  
  MULTICORE_start_threads();
}

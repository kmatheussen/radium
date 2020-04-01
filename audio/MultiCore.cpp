// This file is not compiled directly, but #included into SoundProducer.cpp


#include <QThread>

//QAtomicInt g_num_waits(0);

#include "../common/nsmtracker.h"
#include "../common/visual_proc.h"
#include "../common/threading.h"
#include "../common/stacktoucher_proc.h"
#include "../common/settings_proc.h"
#include "../common/Semaphores.hpp"
#include "../common/QueueStack.hpp"

#include "../common/OS_Player_proc.h"

#include "SoundProducer_proc.h"
#include "SendReceive_plugins_proc.h"

#include "MultiCore_proc.h"

//#undef R_ASSERT
//#define R_ASSERT(a) do{ if(!(a)){ fflush(stderr);fprintf(stderr,">>>>>>>>>>>>>>>>>>> Assert failed: \"" # a "\". %s: " __FILE__":%d\n", __FUNCTION__, __LINE__);something_is_wrong=true;}}while(0)



static volatile bool something_is_wrong = false;

static const char *settings_key = "num_cpus";

static bool g_use_buzy_get = false;


static DEFINE_ATOMIC(int, g_start_block_time) = 0;
const int g_num_blocks_to_pause_buzylooping = 400; // Should be some seconds
static DEFINE_ATOMIC(int, g_num_blocks_pausing_buzylooping) = 0;

namespace{
  struct All_Sp_Finished{
    radium::SpinlockSemaphore all_sp_finished_spin;
    radium::Semaphore all_sp_finished;

    void signal(void){
      if (g_use_buzy_get)
	all_sp_finished_spin.signal();
      else
	all_sp_finished.signal();
    }

    void wait(void){
      if (g_use_buzy_get)
	all_sp_finished_spin.wait();
      else
	all_sp_finished.wait();
    }

    bool tryWaitLightly(void){
#if !defined(RELEASE)
      R_ASSERT(g_use_buzy_get);
#endif
      return all_sp_finished_spin.tryWaitLightly();
    }

    int numSignallers(void){
      if (g_use_buzy_get)
	return all_sp_finished_spin.numSignallers();
      else
	return all_sp_finished.numSignallers();
    }
  };

  All_Sp_Finished all_sp_finished;
}

static DEFINE_ATOMIC(int, num_sp_left) = 0;

#define MAX_NUM_SP 8192

#if 0

// 1. The original Queue type.
static radium::Queue< SoundProducer* , MAX_NUM_SP > soundproducer_queue;
#define GET_NEXT_SP_DIRECTLY 1
#define LET_MAIN_THREAD_TAKE_FIRST_SP 1

#elif 0

// 2. Lighter. A little bit faster than 1.
static radium::LinkedListStack< SoundProducer*> soundproducer_queue;
#define GET_NEXT_SP_DIRECTLY 1
#define LET_MAIN_THREAD_TAKE_FIRST_SP 1

#elif 1

// 3. Same as 2, but stored in a vector. Probably less cache misses when queue has many elements.
static radium::VectorStack< SoundProducer*, MAX_NUM_SP > soundproducer_queue;
#define GET_NEXT_SP_DIRECTLY 1
#define LET_MAIN_THREAD_TAKE_FIRST_SP 1

#else

// 3. Tries to be smart. Should in theory work better than the others, but the implementation of SameCpuQueue isn't finished.
static radium::SameCpuQueue< SoundProducer*> soundproducer_queue;
#define GET_NEXT_SP_DIRECTLY 0
#define LET_MAIN_THREAD_TAKE_FIRST_SP 0

#endif

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

static void dec_sp_dependency(const SoundProducer *parent, SoundProducer *sp, SoundProducer *&next){
  
  if (ATOMIC_ADD_RETURN_NEW(sp->_num_active_input_links_left, -1) == 0) {
#if GET_NEXT_SP_DIRECTLY
    if (next == NULL)
      next = sp;
    else
#endif
      soundproducer_queue.put(sp);
  }
}

static void process_soundproducer(int cpunum, SoundProducer *sp, int64_t time, int num_frames, bool process_plugins){

  R_ASSERT(sp!=NULL);

  //int level = 0;
  
  while(sp != NULL){

    //printf("    Processing %p\n", sp);

    //for(int i=0;i<level;i++)printf("  "); printf("     >>> %d processes %s\n", cpunum, sp->_plugin->patch->name);
            
#if !defined(RELEASE)
    //  fprintf(stderr,"   Processing %p: %s %d\n",sp,sp->_plugin->patch==NULL?"<null>":sp->_plugin->patch->name,int(sp->is_processed));
    //fflush(stderr);

    bool old = ATOMIC_SET_RETURN_OLD(sp->is_processed, true);
    R_ASSERT(old==false);
#endif

    //printf("Running %s\n", sp->_plugin->patch->name);

    if (! sp->RT_is_autosuspending()) {

      //double start_time = monotonic_seconds();
      
      {
        SP_RT_process(sp, time, num_frames, process_plugins);
      }
      //    double duration = monotonic_seconds() - start_time;
      //if (duration > sp->running_time)
      //  sp->running_time = duration;
      
    } else {

      for(auto *buffer : sp->_input_buffers)
        buffer->RT_release_channels_if_necessary(radium::NeedsLock::YES);

      /*
      for(auto link : sp->_output_links){        
        if(!strcmp("Main Pipe", link->source->_plugin->patch->name)
           && !strcmp("System Out", link->target->_plugin->patch->name)){
          printf("    Main pipe -> Out: NULL in/out 2\n");
          break;
        }
      }
      */
      
      sp->RT_iterate_output_links_after_process(time, num_frames, NULL); // last argument is sound (i.e. there's no sound)
      
    }
    
    //for(int i=0;i<level;i++)printf("  "); printf("     <<< %d processes %s\n",cpunum, sp->_plugin->patch->name);

    SoundProducer *next = NULL;

    for(SoundProducerLink *link : sp->_output_links)
      if (link->is_active)
        dec_sp_dependency(link->source, link->target, next);

    // Important that we decrease 'num_sp_left' AFTER scheduling other soundproducers for processing. (i.e. calling when 'dec_sp_dependency')
    if (ATOMIC_ADD_RETURN_NEW(num_sp_left, -1) == 0) {
      R_ASSERT(next==NULL);
      all_sp_finished.signal();
      return;
    }

    sp = next;
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

  int _cpunum;
  
  Runner(int cpunum)
    : _cpunum(cpunum)
  {
    ATOMIC_SET(must_exit, false);
    QObject::connect(this, SIGNAL(finished()), this, SLOT(onFinished()));
    start(QThread::NormalPriority); //QThread::TimeCriticalPriority); // The priority shouldn't matter though since PLAYER_acquire_same_priority() is called inside run().
  }
  //#define FOR_MACOSX
  void run() override {
    AVOIDDENORMALS;

    THREADING_init_runner_thread_type();
      
    touch_stack();

#ifdef FOR_MACOSX
    printf("  Trying to call setPriority(QThread::TimeCriticalPriority);\n");
#endif
    
    setPriority(QThread::TimeCriticalPriority); // shouldn't matter, but just in case the call below is not working, for some reason.
  
#ifdef FOR_MACOSX
    printf("  Trying to call THREADING_acquire_player_thread_priority();\n");
#endif
    THREADING_acquire_player_thread_priority();

#ifdef FOR_MACOSX
    printf("  Trying to call can_start_main_loop.wait();\n");
#endif
    can_start_main_loop.wait();

#ifdef FOR_MACOSX
    //bool started = false;
    //printf("  Trying to call soundproducer_queue.get();\n");
#endif

    while(true){

      SoundProducer *sp;

      if(g_use_buzy_get) {

	for(;;){
	  int counter = 0;
	  
	  if (ATOMIC_GET_RELAXED(g_num_blocks_pausing_buzylooping) <= 0) {
	    
	    if (soundproducer_queue.buzyGetEnabled()) {
	      
	      bool gotit;
	      sp = soundproducer_queue.tryGet(gotit);
	      if (gotit)
		break;
	      else
		avoid_lockup(counter++);
            
	    } else {
	      
	      soundproducer_queue.waitUntilBuzyGetIsEnabled();
	      
	    }
	    
	  } else {
	    
	    sp = soundproducer_queue.get();
	    break;
	    
	  }
	  
	}


      } else {

	// not buzy-getting
	sp = soundproducer_queue.get();

      }
      

#ifdef FOR_MACOSX
      //if (started==false){
      //  printf("  Success. Running Runner thread main loop started.\n");
      //  started = true;
      //}
#endif

      if (ATOMIC_GET(must_exit)) {
        soundproducer_queue.put(sp);
        break;
      }

      process_soundproducer(_cpunum, sp, time, num_frames, process_plugins);
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
    process_soundproducer(0, sp, time, num_frames, process_plugins);
  }

  R_ASSERT_RETURN_IF_FALSE(all_sp_finished.numSignallers()==1);

  all_sp_finished.wait();
}



static int g_num_runners = 0;
static Runner **g_runners = NULL;


// Called for each sound card block, not for each radium block
void MULTICORE_start_block(void){
  if (g_use_buzy_get){
    ATOMIC_SET(g_start_block_time, (int) (1000*monotonic_seconds()));
    soundproducer_queue.enableBuzyGet();
  }
}

void MULTICORE_end_block(void){
  if (g_use_buzy_get){
    soundproducer_queue.disableBuzyGet();

    int num_blocks_left = ATOMIC_GET(g_num_blocks_pausing_buzylooping);
    if (num_blocks_left > 0)
      ATOMIC_SET(g_num_blocks_pausing_buzylooping, num_blocks_left-1);
  }
}

static SoundProducer *tryget_next_sp_in_main_thread(void){
  bool gotit;
  auto *ret = soundproducer_queue.tryGet(gotit);
  if (!gotit)
    return NULL;
  else
    return ret;
}

void MULTICORE_run_all(const radium::Vector<SoundProducer*> &sp_all, int64_t time, int num_frames, bool process_plugins){

  if (sp_all.size()==0)
    return;

  if (sp_all.size() >= MAX_NUM_SP){
    // Not doing anything in the audio thread might cause a deadlock, so the message window might now show if using RT_Message here.
    RT_message("Maximum number of sound objects reached. Tried to play %d sound objects, but only %d sound objects are supported. Radium must be recompiled to increase this number.",
               sp_all.size(),
               MAX_NUM_SP);
    return;
  }


  //printf("\n\n\n               ================== MULTICORE_run_all startT==============\n\n\n\n\n");

  int num_ready_sp = 0;


  // 1. Initialize threads

  for(int i=0;i<g_num_runners;i++){
    g_runners[i]->time = time;
    g_runners[i]->num_frames = num_frames;
    g_runners[i]->process_plugins = process_plugins;
  }


  
  // 2. initialize soundproducers

  //printf(" mc: SET: %d\n",sp_all.size());
  ATOMIC_SET(num_sp_left, sp_all.size());
  
  //fprintf(stderr,"**************** STARTING %d\n",sp_all.size()); fflush(stderr);

  for (SoundProducer *sp : sp_all) {

    SoundPlugin *plugin = SP_get_plugin(sp);
    if (is_receiver_plugin(plugin))
      RT_SEND_RECEIVE_swap_receiver_send_channels(plugin);
    
    ATOMIC_NAME(sp->_num_active_input_links_left) = sp->_num_active_input_links;

#if !defined(RELEASE)
    ATOMIC_SET(sp->is_processed, false); // is this really necessary? When could it be true?
#endif
  }

  
#if 0
  SP_print_tree();
#endif

  //int num_waits_start = int(g_num_waits);

#define START_ALL_RUNNERS_SIMULTANEOUSLY 1

  SoundProducer *sp_in_main_thread = NULL;
    
      
  // 3. start threads;

  for (SoundProducer *sp : sp_all)
    if (sp->_num_active_input_links==0){
      if (LET_MAIN_THREAD_TAKE_FIRST_SP && sp_in_main_thread == NULL && g_num_runners>0){
        sp_in_main_thread = sp;
      } else {
        num_ready_sp++;
        //fprintf(stderr,"Scheduling %p: %s\n",sp,sp->_plugin->patch==NULL?"<null>":sp->_plugin->patch->name);
        //fflush(stderr);
#if START_ALL_RUNNERS_SIMULTANEOUSLY
        soundproducer_queue.putWithoutSignal(sp);
#else
        soundproducer_queue.put(sp);
#endif
      }
    }
  
#if !LET_MAIN_THREAD_TAKE_FIRST_SP
  sp_in_main_thread = tryget_next_sp_in_main_thread();
#endif


#if START_ALL_RUNNERS_SIMULTANEOUSLY
  if(num_ready_sp > 0)
    soundproducer_queue.signal(num_ready_sp); // signal everyone at once to try to lower number of semaphore waits. Doesn't seem to make a difference on my machine though.
#endif

  if (g_num_runners==0) {

    R_ASSERT(num_ready_sp > 0);

    process_single_core(time, num_frames, process_plugins);
    
  } else {

#if LET_MAIN_THREAD_TAKE_FIRST_SP
    R_ASSERT(sp_in_main_thread!=NULL);
#endif

    if (g_use_buzy_get){

      // 4. Use main thread as another runner.

      process_soundproducer(0, sp_in_main_thread, time, num_frames, process_plugins);
            
      int counter = 0;
      
      while(all_sp_finished.tryWaitLightly()==false){
	
        sp_in_main_thread = tryget_next_sp_in_main_thread();
        if (sp_in_main_thread)
	  process_soundproducer(0, sp_in_main_thread, time, num_frames, process_plugins);
	else
	  avoid_lockup(counter++);      
      }

    } else {

      // 4. process as much as we can in the main thread
      
      while(sp_in_main_thread != NULL){
	process_soundproducer(0, sp_in_main_thread, time, num_frames, process_plugins);

        sp_in_main_thread = tryget_next_sp_in_main_thread(); // This is not perfect. It's likely that we fail to get a soundproducer before everything is done.
      }
      
      // 5. wait.
      all_sp_finished.wait();

    }


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


void MULTICORE_add_sp(SoundProducer *sp){
}

void MULTICORE_remove_sp(SoundProducer *sp){
}

int MULTICORE_get_num_threads(void){
  return g_num_runners+1;
}


static int get_num_cpus_from_config(void){
  static int default_num_cpus = -1;

  if (default_num_cpus==-1)
    default_num_cpus = QThread::idealThreadCount();

  if (default_num_cpus<1)
    default_num_cpus = 1;

  if (default_num_cpus>MAX_NUM_CPUS)
    default_num_cpus = MAX_NUM_CPUS; // ensure sane value

  return SETTINGS_read_int32(settings_key, default_num_cpus);
}

void MULTICORE_set_num_threads(int num_new_cpus){
  R_ASSERT(num_new_cpus >= 1);

  if (get_num_cpus_from_config() != num_new_cpus)
    SETTINGS_write_int(settings_key, num_new_cpus);

  int num_new_runners = num_new_cpus - 1;
  
  if (num_new_runners==g_num_runners)
    return;

  int num_old_runners = g_num_runners;
  Runner **old_runners = g_runners;
  
  Runner **new_runners = (Runner**)V_calloc(num_new_runners,sizeof(Runner*));

  // start them
  for(int i=0 ; i < num_new_runners ; i++)
    new_runners[i]=new Runner(i+1); // add 1 since, cpunum 0 is main player thread.

  // wait until they are ready (takes some time, but if we don't wait here, there will be a moment where there are no active runners, and no sound)
  for(int i=0 ; i < num_new_runners ; i++)
    while(new_runners[i]->can_start_main_loop.numWaiters()==0)
      msleep(1);

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

  bool do_buzy_get = doAudioBuzyLoop();

  PLAYER_lock(); { // Lock to avoid tsan hit.
    g_use_buzy_get = do_buzy_get;
  } PLAYER_unlock();
  
  int num_new_cpus = get_num_cpus_from_config();

  MULTICORE_set_num_threads(num_new_cpus);
}

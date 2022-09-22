#ifdef FOR_MACOSX

/* This code is used when jack is not running */

/* The code is mostly copied from the jack2 repository. */

/*
Apple has documented the mach threads API here:

  http://mirror.informatimago.com/next/developer.apple.com/documentation/Darwin/Conceptual/KernelProgramming/scheduler/chapter_8_section_4.html

But it's not easy to understand. Instead we just use the jack code.
*/


#include <pthread.h>

#include <TargetConditionals.h>
#include <mach/thread_policy.h>
#include <mach/thread_act.h>
#include <CoreAudio/CoreAudio.h>

#define LANGSPEC "C"
#include "../common/threading_lowlevel.h"

#include "machthreads_proc.h"

#define THREAD_SET_PRIORITY         0
#define THREAD_SCHEDULED_PRIORITY   1

static int GetThreadPriority(pthread_t thread, int inWhichPriority)
{
    thread_basic_info_data_t threadInfo;
    policy_info_data_t thePolicyInfo;
    unsigned int count;

    // get basic info
    count = THREAD_BASIC_INFO_COUNT;
    thread_info(pthread_mach_thread_np(thread), THREAD_BASIC_INFO, (thread_info_t)&threadInfo, &count);

    switch (threadInfo.policy) {
        case POLICY_TIMESHARE:
            count = POLICY_TIMESHARE_INFO_COUNT;
            thread_info(pthread_mach_thread_np(thread), THREAD_SCHED_TIMESHARE_INFO, (thread_info_t)&(thePolicyInfo.ts), &count);
            if (inWhichPriority == THREAD_SCHEDULED_PRIORITY) {
                return thePolicyInfo.ts.cur_priority;
            } else {
                return thePolicyInfo.ts.base_priority;
            }
            break;

        case POLICY_FIFO:
            count = POLICY_FIFO_INFO_COUNT;
            thread_info(pthread_mach_thread_np(thread), THREAD_SCHED_FIFO_INFO, (thread_info_t)&(thePolicyInfo.fifo), &count);
            if ( (thePolicyInfo.fifo.depressed) && (inWhichPriority == THREAD_SCHEDULED_PRIORITY) ) {
                return thePolicyInfo.fifo.depress_priority;
            }
            return thePolicyInfo.fifo.base_priority;
            break;

        case POLICY_RR:
            count = POLICY_RR_INFO_COUNT;
            thread_info(pthread_mach_thread_np(thread), THREAD_SCHED_RR_INFO, (thread_info_t)&(thePolicyInfo.rr), &count);
            if ( (thePolicyInfo.rr.depressed) && (inWhichPriority == THREAD_SCHEDULED_PRIORITY) ) {
                return thePolicyInfo.rr.depress_priority;
            }
            return thePolicyInfo.rr.base_priority;
            break;
    }

    return 0;
}

static int GetThreadSetPriority(pthread_t thread)
{
    return GetThreadPriority(thread, THREAD_SET_PRIORITY);
}

static bool SetThreadToPriority(pthread_t thread, int inPriority, bool inIsFixed, uint64_t period, uint64_t computation, uint64_t constraint)
{
    if (inPriority == 96) {
      
        // REAL-TIME / TIME-CONSTRAINT THREAD
        thread_time_constraint_policy_data_t	theTCPolicy;

#ifdef MY_TARGET_OS_IPHONE
        theTCPolicy.period = CAHostTimeBase::ConvertFromNanos(period);
        theTCPolicy.computation = CAHostTimeBase::ConvertFromNanos(computation);
        theTCPolicy.constraint = CAHostTimeBase::ConvertFromNanos(constraint);
#else
        theTCPolicy.period = AudioConvertNanosToHostTime(period);
        theTCPolicy.computation = AudioConvertNanosToHostTime(computation);
        theTCPolicy.constraint = AudioConvertNanosToHostTime(constraint);
#endif
        theTCPolicy.preemptible = true;
        kern_return_t res = thread_policy_set(pthread_mach_thread_np(thread),
                                              THREAD_TIME_CONSTRAINT_POLICY,
                                              (thread_policy_t) &theTCPolicy,
                                              THREAD_TIME_CONSTRAINT_POLICY_COUNT
                                              );
#if 0 //!defined(RELEASE)        
        jack_log("JackMachThread::thread_policy_set res = %ld", res);
#endif
#if 1
        return (res == KERN_SUCCESS); // ? 0 : -1;
#else
        if (res != KERN_SUCCESS)
          printf("   Non-RT FAILED: %d\n", res);
        //return (res == KERN_SUCCESS); // ? 0 : -1;
        return true;
#endif
        
    } else {
        // OTHER THREADS
        thread_extended_policy_data_t theFixedPolicy;
        thread_precedence_policy_data_t thePrecedencePolicy;
        int relativePriority;

        // [1] SET FIXED / NOT FIXED
        theFixedPolicy.timeshare = !inIsFixed;
        thread_policy_set(pthread_mach_thread_np(thread),
                          THREAD_EXTENDED_POLICY,
                          (thread_policy_t)&theFixedPolicy,
                          THREAD_EXTENDED_POLICY_COUNT);

        // [2] SET PRECEDENCE
        // N.B.: We expect that if thread A created thread B, and the program wishes to change
        // the priority of thread B, then the call to change the priority of thread B must be
        // made by thread A.
        // This assumption allows us to use pthread_self() to correctly calculate the priority
        // of the feeder thread (since precedency policy's importance is relative to the
        // spawning thread's priority.)
        relativePriority = inPriority - GetThreadSetPriority(pthread_self());

        thePrecedencePolicy.importance = relativePriority;
        kern_return_t res = thread_policy_set(pthread_mach_thread_np(thread),
                                              THREAD_PRECEDENCE_POLICY,
                                              (thread_policy_t) &thePrecedencePolicy,
                                              THREAD_PRECEDENCE_POLICY_COUNT);
#if 0 //!defined(RELEASE)        
        printf("JackMachThread::thread_policy_set res = %ld", res);
#endif
#if 1
        return (res == KERN_SUCCESS); // ? 0 : -1;
#else
        if (res != KERN_SUCCESS)
          printf("   RT FAILED: %d\n", res);
        return true;
#endif
    }
}

static bool JackMachThread_AcquireRealTimeImp2(pthread_t thread, UInt64 period, UInt64 computation, UInt64 constraint)
{
  return SetThreadToPriority(thread, 96, true, period, computation, constraint);
}


static uint64_t fPeriod = 0;
static uint64_t fComputation = 0;
static uint64_t fConstraint;

static bool JackMachThread_AcquireRealTimeImp(pthread_t thread, /* int priority, */ UInt64 period, UInt64 computation, UInt64 constraint)
{
  return JackMachThread_AcquireRealTimeImp2(thread, period, computation, constraint);
}

bool MACH_THREADS_jack_acquire_real_time_scheduling(pthread_t thread /* , int priority */){ // Note: priority is not actually used here.

#if !defined(RELEASE)
  g_t_current_thread_is_RT = R_IS_RT;
#endif

  return JackMachThread_AcquireRealTimeImp(thread,
                                           /* priority, */
                                           fPeriod, // I guess this one must be how long time between each wakeup.
                                           fComputation, // How long time it takes to compute when woken up?
                                           fConstraint // ? (Set to fPeriod anyway)
                                           );
}

static bool JackMachThread_DropRealTimeImp(pthread_t thread)
{
  return SetThreadToPriority(thread, 63, false, 0, 0, 0);
}

bool MACH_THREADS_jack_drop_real_time_scheduling(pthread_t thread)
{
#if !defined(RELEASE)
  g_t_current_thread_is_RT = R_IS_NOT_RT;
#endif
  return JackMachThread_DropRealTimeImp(thread);
}


// ?? -Kjetil
static int ComputationMicroSec(int buffer_size)
{
  if (buffer_size < 128) {
    return 500;
  } else if (buffer_size < 256) {
    return 300;
  } else {
    return 100;
  }
}


/*

  fPeriodUsecs = 1000000.f / fSampleRate * fBufferSize;

  fPeriod = fConstraint = fPeriodUsecs * 1000;

  fComputation = ComputationMicroSec(fBufferSize) * 1000;

  Jack: JackAudioDriver::Attach fBufferSize = 1024 fSampleRate = 48000

*/


void MACH_THREADS_set_period_and_buffer_size(double sample_rate, int buffer_size){
  const int fPeriodUsecs = 1000000.f / sample_rate * buffer_size;
  fPeriod = fPeriodUsecs * 1000;
  fConstraint = fPeriodUsecs * 1000;
  fComputation = ComputationMicroSec(buffer_size) * 1000;
}


#endif

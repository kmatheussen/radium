#ifndef _RADIUM_COMMON_SEMAPHORES_HPP
#define _RADIUM_COMMON_SEMAPHORES_HPP


/*
  I'm not 100% sure about if the 10000-iterations-looping in the lightweight semaphore (in sema.h) will work well for Radium
  (the critical use is in audio/MultiCore.cpp), so here's one without that iteration.

  The important thing in audio/MultiCore.cpp is just to avoid any unnecessary type of OS thread switch when a runner schedules a new job.
  It's possible to avoid it manually in audio/MultiCore.cpp though, but then the scheduling order would be wrong (might lower performance since
  the jobs are sorted by how much cpu they use), and/or the audio/MultiCore.cpp would become (even) more messy. It's likely that the
  native posix/win/osx functions do this already, but we do it here anyway, just to be sure, since the overhead is small.
 */


#include "sema.h"


namespace radium{


class Semaphore{

  private:

  DEFINE_ATOMIC(int, m_count);
  
#if 0
  cpp11onmulticore::LightweightSemaphore m_sema; // Calling pthread_yield inside the spin lock gives extremly promising results though. Must be investigated more.
#else
  cpp11onmulticore::Semaphore m_sema;
#endif

  Semaphore(const Semaphore&) = delete;
  Semaphore& operator=(const Semaphore&) = delete;

  public:
    
    Semaphore(int n = 0)
    {
      R_ASSERT(n >= 0);
      ATOMIC_SET(m_count, n);
      //m_count = n;
    }

    int numSignallers(void) const {
      return ATOMIC_GET(m_count);
      //return int(m_count);
    }

    int numWaiters(void) const {
      return -numSignallers();
    }
  

    bool tryWait(void){

      for(;;){
        int old_count = ATOMIC_GET(m_count);
        //int old_count = int(m_count);

        if (old_count <= 0)
          return false;

        if (ATOMIC_COMPARE_AND_SET_INT(m_count, old_count, old_count-1)==true)
          return true;
        //        if (m_count.testAndSetOrdered(old_count, old_count-1)==true)
        //          return true;
      }

      return false;
    }

  
    void wait(void)
    {
      if (ATOMIC_ADD_RETURN_OLD(m_count, -1) <= 0)
        m_sema.wait();
      /*
      if (m_count.fetchAndAddOrdered(-1) <= 0) {
        m_sema.wait();
      }
      */
    }

    void wait(int n){
      R_ASSERT(n>0);
      while(n--)
        wait();
    }

#if 0 // Not tested that much. Also, I have a feeling that there is something wrong with the design of the code if using this one.
	bool trySignal(void)
	{
		for(int i = 0 ; i < 100 ; i++)
		{
			int count = ATOMIC_GET(m_count);
			
			if (count >= 0)
				return false;
			
			if (ATOMIC_COMPARE_AND_SET_INT(m_count, count, count+1))
			{
				m_sema.signal();
				return true;
			}
		}

		R_ASSERT_NON_RELEASE(false); // That's strange...
		
		return false;
	}
#endif
	
    void signal(void)
    {
      if (ATOMIC_ADD_RETURN_OLD(m_count, 1) < 0)
        m_sema.signal();
      
      /*
      if (m_count.fetchAndAddOrdered(1) < 0) {
        m_sema.signal();
      }
      */
    }

    void signal(int n){
      R_ASSERT(n>0);
      while(n--)
        signal();
    }

    bool signalIfAnyoneIsWaiting(void){
      if (numWaiters() > 0){
        signal();
        return true;
      } else
        return false;
    }
  
    void signalAll(void){
      while(numWaiters() > 0)
        signal();
    }
};


// Same as Semaphore, except that it keeps track of the number of times signal() is going to be called in the future
// with the function 'is_going_to_be_signalled_another_time_in_the_future'.
//
// Used for instance if sending a message to another thread that will do something and call signal() when finished.
//
// Note that the methods 'is_going_to_be_signalled_another_time_in_the_future' and 'wait_for_all_future_signals' are not thread safe.
class FutureSignalTrackingSemaphore : public Semaphore{

  int number_of_future_signals = 0;

public:
  void is_going_to_be_signalled_another_time_in_the_future(void){
    number_of_future_signals++;
  }

  void wait_for_all_future_signals(void){
    if (number_of_future_signals > 0){
      wait(number_of_future_signals);
      number_of_future_signals = 0;
    }
  }
};

  
//---------------------------------------------------------
// SpinlockSemaphore
//---------------------------------------------------------

class SpinlockSemaphore {

private:

  DEFINE_ATOMIC(int, m_count);


public:

    SpinlockSemaphore(int initialCount = 0)
    {
      R_ASSERT(initialCount >= 0);
      ATOMIC_SET(m_count, initialCount);
    }

    int numSignallers(void){
      return ATOMIC_GET(m_count);
    }

    // "lightly" means that we don't buzy-loop until ATOMIC_COMPARE_AND_SET_INT returns true.
    bool tryWaitLightly(void)
    {
      
      int oldCount = ATOMIC_GET_RELAXED(m_count);
      if (oldCount == 0)
        return false;
      
      return ATOMIC_COMPARE_AND_SET_INT(m_count, oldCount, oldCount-1);
    }

    void wait(void)
    {
      while(!tryWaitLightly());
    }

    void signal(int count = 1)
    {
      ATOMIC_ADD(m_count, count);
    }
};




#if 0
struct RequestAcknowledge {
  DEFINE_ATOMIC(bool, request);  // t1 -> t2
  Semaphore acknowledge;         // t2 -> t1

  // Called by thread 1.
  void t1_request_and_wait(void){
    ATOMIC_SET(request, true);
    acknowledge.wait();
  }

  // Called by thread 2.
  bool t2_is_requested(void){
    return ATOMIC_GET(request);
  }

  // Called by thread 2. Must be called after t2_is_requested has returned true. (t2_is_requested can be called several times though)
  void t2_acknowledge(void){
    ATOMIC_SET(request, false);
    acknowledge.signal();
  }
  
};
#endif
  
}

#endif

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


#include <QAtomicInt>


#include "sema.h"


namespace radium{

  
class Semaphore{

  private:
    
    QAtomicInt m_count;
    cpp11onmulticore::Semaphore m_sema;


  Semaphore(const Semaphore&) = delete;
  Semaphore& operator=(const Semaphore&) = delete;

  public:
    
    Semaphore(int n = 0)
    {
      R_ASSERT(n >= 0);
      m_count = n;
    }

    int numSignallers(void) {
      return int(m_count);
    }

    int numWaiters(void){
      return -numSignallers();
    }
  

    bool tryWait(void){

      for(;;){
        int old_count = int(m_count);

        if (old_count <= 0)
          return false;

        if (m_count.testAndSetOrdered(old_count, old_count-1)==true)
          return true;
      }

      return false;
    }

  
    void wait(void)
    {
      if (m_count.fetchAndAddOrdered(-1) <= 0) {
        m_sema.wait();
      }
    }

    void wait(int n){
      R_ASSERT(n>0);
      while(n--)
        wait();
    }
  
    void signal(void)
    {
      if (m_count.fetchAndAddOrdered(1) < 0) {
        m_sema.signal();
      }
    }

    void signal(int n){
      R_ASSERT(n>0);
      while(n--)
        signal();
    }

};

}

#endif

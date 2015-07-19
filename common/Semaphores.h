#include <QAtomicInt>


#include "sema.h"


namespace radium{

  
class Semaphore{

  private:
    
    QAtomicInt m_count;
    cpp11onmulticore::Semaphore m_sema;

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
  
  /*
    void tryWait(void){
    }
  */
  
    void wait(void)
    {
      if (m_count.fetchAndAddOrdered(-1) <= 0) {
        m_sema.wait();
      }
    }

    void wait(int n){
      R_ASSERT(n>0);
      while(--n)
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
      while(--n)
        signal();
    }

};

}

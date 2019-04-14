

#ifndef _RADIUM_COMMON_MUTEX_HPP
#define _RADIUM_COMMON_MUTEX_HPP

#include <pthread.h>
#include <sys/time.h>
# include <errno.h>

#ifdef FOR_MACOSX
# include <sys/types.h>
# include <sys/socket.h>
#endif


// QMutex/QMutexLocker/QWaitCondition, which were used everywhere, and worked great othervice, didn't work with tsan, so that's the reason for this file.
// Tried to use boost instead first, but it generated too much drama. (took too much time trying to understand the APIs, plus that I had to add -Wno-unused-variable as a compiler flag and link with two boost libraries)
//
// However, seems like pthread works just fine with mingw, so this is just fine.



namespace radium {

//clock_gettime is not implemented on OSX, but for the type of usage in this file, it's absolutely no problem at all using gettimeofday instead, so we just as well do that on all platforms. Code copied from http://stackoverflow.com/questions/5167269/clock-gettime-alternative-in-mac-os-x
static inline int my_clock_gettime(struct timespec* t) {
    struct timeval now;
    int rv = gettimeofday(&now, NULL);
    if (rv) return rv;
    t->tv_sec  = now.tv_sec;
    t->tv_nsec = now.tv_usec * 1000;
    return 0;
}
  
  /*
class MaybeQuickMutex{

  DEFINE_ATOMIC(int, num_visitors);

  Mutex mutex;
  
public:
  
  void lock(void){
    if (ATOMIC_ADD_RETURN_OLD(num_visitors, 1) > 0)
      mutex.lock(); 
  }

  void unlock(void){
    if (ATOMIC_ADD_RETURN_OLD(num_visitors, -1) > 0)
      mutex.unlock();
  }

  bool has_lock(void){
    return ATOMIC_GET(num_visitors) > 0;
  }
  
};
  */
  
struct Mutex {
  friend struct CondWait;
  
private:

  pthread_mutex_t mutex;
  bool is_recursive;
  int num_locks;

public:
  
  Mutex(bool is_recursive = false)
    :is_recursive(is_recursive)
    ,num_locks(0)
  {
    if (is_recursive){
      pthread_mutexattr_t attr;
      pthread_mutexattr_init(&attr);
      pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
      pthread_mutex_init(&mutex, &attr);
    } else {
      pthread_mutex_init(&mutex, NULL);
    }
  }

  ~Mutex(){
    pthread_mutex_destroy(&mutex);
  }

  void lock(void){    
    pthread_mutex_lock(&mutex); // Note that pthread_mutex_lock is a lighweight lock, meaning that it only have to do an atomic test-and-set if the mutex wasn't already obtained. So no need to do that optimization here (we would avoid a function call though, but that shouldn't matter). (winpthread implementation: https://sourceforge.net/p/mingw-w64/mingw-w64/ci/master/tree/mingw-w64-libraries/winpthreads/src/mutex.c)

    // ehm.
    if (!is_recursive)
      R_ASSERT_RETURN_IF_FALSE(num_locks==0);

    num_locks++;
  }

  void unlock(void){
    R_ASSERT_RETURN_IF_FALSE(num_locks>0);
    
    num_locks--;
    
    pthread_mutex_unlock(&mutex);
  }

  bool is_locked(void) const {
    return num_locks>0;
  }
};

struct ScopedMutex{
  Mutex &mutex;
  
  ScopedMutex(Mutex &mutex)
    : mutex(mutex)
  {
    mutex.lock();
  }

  ~ScopedMutex(){
    mutex.unlock();
  }

  void wait_and_pause_lock(int ms){
    mutex.unlock();
    OS_WaitForAShortTime(ms);
    mutex.lock();
  }
};



struct CondWait {

  pthread_cond_t cond;

  CondWait(){
    pthread_cond_init(&cond,NULL);
  }

  ~CondWait(){
    pthread_cond_destroy(&cond);
  }

  bool wait(Mutex *mutex, int timeout_in_milliseconds) {
    struct timespec ts;

    R_ASSERT( (timeout_in_milliseconds % 1000) == 0); // Only whole seconds are supported for now (don't bother to support milliseconds until it's actually needed)

    my_clock_gettime(&ts);
    ts.tv_sec += timeout_in_milliseconds/1000;

    int ret = pthread_cond_timedwait(&cond,&mutex->mutex,&ts);
    if (ret==0)
      return true;
    if (ret==ETIMEDOUT)
      return false;

    if (ret==EINVAL)
      RError("pthread_cond_wait returned EINVAL");
    else if (ret==EPERM)
      RError("pthread_cond_wait returned EPERM");
#ifndef RELEASE
    else
      RError("Unknown return message from pthread_cond_wait: %d",ret);
#endif
    return true;
  }

  void wait(Mutex *mutex){
    int ret =  pthread_cond_wait(&cond,&mutex->mutex);
    if (ret==EINVAL)
      RError("pthread_cond_wait returned EINVAL");
    else if (ret==EPERM)
      RError("pthread_cond_wait returned EINVAL");
#ifndef RELEASE
    else if (ret!=0)
      RError("Unknown return message from pthread_cond_wait: %d",ret);
#endif
  }

  void notify_one(void){
    pthread_cond_signal(&cond);
  }

  void notify_all(void){
    pthread_cond_broadcast(&cond);
  }
};


}



#endif

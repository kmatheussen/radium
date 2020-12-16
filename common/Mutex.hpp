

#ifndef _RADIUM_COMMON_MUTEX_HPP
#define _RADIUM_COMMON_MUTEX_HPP

#include <pthread.h>
#include <sys/time.h>
#include <errno.h>

#include <thread>

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

  
struct AbstractMutex {
  AbstractMutex(const AbstractMutex&) = delete;
  AbstractMutex& operator=(const AbstractMutex&) = delete;

  AbstractMutex(){
  }
  
  virtual void lock(void) = 0;
  virtual void unlock(void) = 0;
};

  
struct Mutex : public AbstractMutex {
  friend struct CondWait;
  
private:

  pthread_mutex_t mutex;
  bool is_recursive;
  int num_locks;

public:
  
  Mutex(bool is_recursive)
    : is_recursive(is_recursive)
    , num_locks(0)
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

  Mutex()
    : Mutex(false)
  {
  }
  
  ~Mutex(){
    pthread_mutex_destroy(&mutex);
  }

  void lock(void) override {
    pthread_mutex_lock(&mutex); // Note that pthread_mutex_lock is a lighweight lock, meaning that it only have to do an atomic test-and-set if the mutex wasn't already obtained. So no need to do that optimization here (we would avoid a function call though, but that shouldn't matter). (winpthread implementation: https://sourceforge.net/p/mingw-w64/mingw-w64/ci/master/tree/mingw-w64-libraries/winpthreads/src/mutex.c)

    // ehm.
    if (!is_recursive)
      R_ASSERT_RETURN_IF_FALSE(num_locks==0);

    num_locks++;
  }

  void unlock(void) override {
    R_ASSERT_RETURN_IF_FALSE(num_locks>0);
    
    num_locks--;
    
    pthread_mutex_unlock(&mutex);
  }

  bool is_locked(void) const {
    return num_locks>0;
  }
};


// Se her for tuna spinlock for audio:  https://timur.audio/using-locks-in-real-time-audio-processing-safely (pthrad_unlock bruker muligens en del tid)
  // Kanskje det kan løses ved å vente med å kalle unlock() til audio-callback er ferdig.
struct RT_Mutex : public AbstractMutex {
  
private:

  pthread_mutex_t _mutex;
  bool _is_recursive;
  int _num_locks;

#if !defined(FOR_LINUX)
  priority_t _priority_before_locking;
#endif
  
public:
  
  RT_Mutex(bool is_recursive)
    : _is_recursive(is_recursive)
    , _num_locks(0)
  {
    pthread_mutexattr_t attr;

    int s1 = pthread_mutexattr_init(&attr);
    if (s1!=0)
      GFX_Message(NULL, "RT_Mutex: pthread_mutexattr_init failed: %d\n",s1);

#if defined(FOR_LINUX)
    // It doesn't seem like macos supports PTHREAD_PRIO_INHERIT.
    // (Googling it, there is one blog post claiming that macosx supports PTHREAD_PRIO_INHERIT,
    // but there's nothing in the pthread source code of of macos that indicates that it is supported).
    int s3 = pthread_mutexattr_setprotocol(&attr, PTHREAD_PRIO_INHERIT);
    if (s3!=0)
      GFX_Message(NULL, "RT_Mutex: pthread_mutexattr_setprotocol failed: %d\n",s3);
#endif
    
    if (_is_recursive)
      pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
    else
      pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_NORMAL);
    
    int s4 = pthread_mutex_init(&_mutex, &attr);
    if (s4!=0)
      GFX_Message(NULL, "RT_Mutex: pthread_mutex_init failed: %d\n",s4);
  }

  RT_Mutex()
    : RT_Mutex(false)
  {
  }
  
  ~RT_Mutex(){
    pthread_mutex_destroy(&_mutex);
  }

  // Only call if already running realtime.
  void RT_lock(void) {
    pthread_mutex_lock(&_mutex);

    // ehm.
    if (!_is_recursive)
      R_ASSERT_RETURN_IF_FALSE(_num_locks==0);

    _num_locks++;
  }
  
  void lock(void) override {
#if !defined(FOR_LINUX)
    _priority_before_locking = THREADING_get_priority();
    THREADING_acquire_player_thread_priority();
#endif
    
    RT_lock();
  }
  
  // Only call if already running realtime.
  void RT_unlock(void){
    R_ASSERT_RETURN_IF_FALSE(_num_locks>0);
    
    _num_locks--;
    
    pthread_mutex_unlock(&_mutex);
  }
  
  void unlock(void) override {
    RT_unlock();

#if !defined(FOR_LINUX)
    THREADING_set_priority(_priority_before_locking);
#endif
  }

  bool is_locked(void) const {
    return _num_locks>0;
  }
};

struct ScopedMutex{

  ScopedMutex(const ScopedMutex&) = delete;
  ScopedMutex& operator=(const ScopedMutex&) = delete;

  AbstractMutex &mutex;
  const bool _doit;
  
  ScopedMutex(AbstractMutex &mutex, bool doit = true)
    : mutex(mutex)
    , _doit(doit)
  {
    if (_doit)
      mutex.lock();
  }

  ~ScopedMutex(){
    if (_doit)
      mutex.unlock();
  }

  void wait_and_pause_lock(int ms){
    if (_doit)
      mutex.unlock();
    OS_WaitForAShortTime(ms);
    if (_doit)
      mutex.lock();
  }
};

// Class written by Timur Doumler. Code copied from https://timur.audio/using-locks-in-real-time-audio-processing-safely
// (I assume it is public domain)
struct AudioSpinMutex : public AbstractMutex{
  
  void lock(void) noexcept override {
    // approx. 5x5 ns (= 25 ns), 10x40 ns (= 400 ns), and 3000x350 ns 
    // (~ 1 ms), respectively, when measured on a 2.9 GHz Intel i9
    constexpr int iterations[3] = {5, 10, 3000};
    
    for (int i = 0; i < iterations[0]; ++i) {
      if (try_lock())
        return;
    }
    
    for (int i = 0; i < iterations[1]; ++i) {
      if (try_lock())
        return;
      
      _mm_pause();
    }
    
    while (true) {
      for (int i = 0; i < iterations[2]; ++i) {
        if (try_lock())
          return;
        
        _mm_pause();
        _mm_pause();
        _mm_pause();
        _mm_pause();
        _mm_pause();
        _mm_pause();
        _mm_pause();
        _mm_pause();
        _mm_pause();
        _mm_pause();
      }
      
      // waiting longer than we should, let's give other threads 
      // a chance to recover
      std::this_thread::yield();
    }
  }

  bool try_lock(void) noexcept {
    return !flag.test_and_set(std::memory_order_acquire);
  }
  
  void unlock(void) noexcept override {
    flag.clear(std::memory_order_release);
  }
  
private:
  std::atomic_flag flag = ATOMIC_FLAG_INIT;
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

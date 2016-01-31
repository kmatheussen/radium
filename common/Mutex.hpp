

#ifndef _RADIUM_COMMON_MUTEX_HPP
#define _RADIUM_COMMON_MUTEX_HPP

// QMutex, which was used everywhere, didn't work with tsan. Using boost:mutex instead.

#include <boost/version.hpp>
#if (BOOST_VERSION < 100000) || ((BOOST_VERSION / 100 % 1000) < 60)
  #error "Boost too old. Need at least 1.60.\n Quick fix: cd $HOME ; wget http://downloads.sourceforge.net/project/boost/boost/1.60.0/boost_1_60_0.tar.bz2 ; tar xvjf boost_1_60_0.tar.bz2 (that's it!)"
#endif

#define BOOST_ERROR_CODE_HEADER_ONLY
#include <boost/thread/mutex.hpp>

namespace radium {

struct Mutex : public boost::mutex{
};

struct ScopedMutex{
  Mutex *mutex;
  
  ScopedMutex(Mutex *mutex)
    : mutex(mutex)
  {
    mutex->lock();
  }

  ~ScopedMutex(){
    mutex->unlock();
  }
};

}

#endif


// QTime sometimes crashes when used in threads, and we shut down.

#ifndef _RADIUM_COMMON_TIME_HPP
#define _RADIUM_COMMON_TIME_HPP

    
//#define GET_MS_FUNC() TIME_get_ms()
#define GET_MS_FUNC() (monotonic_seconds() * 1000.0)

namespace radium{
  struct Time{
    DEFINE_ATOMIC(double, _start_time);

    Time(){
      restart();
    }

    void restart(void){
      ATOMIC_NAME(_start_time) = GET_MS_FUNC();
    }

    double elapsed(void) const {
      return GET_MS_FUNC() - ATOMIC_NAME(_start_time);
    }

    // Can be called from any thread at any time
    void RT_restart(void){
      ATOMIC_DOUBLE_SET(_start_time, GET_MS_FUNC());
    }

    // Can be called from any thread at any time
    double RT_elapsed(void) const {
      return GET_MS_FUNC() - ATOMIC_DOUBLE_GET(_start_time);
    }
  };
}

#endif

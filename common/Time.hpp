
// QTime sometimes crashes when used in threads, and we shut down.

#ifndef _RADIUM_COMMON_TIME_HPP
#define _RADIUM_COMMON_TIME_HPP

namespace radium{
  struct Time{
    double _start_time;

    Time()
      : _start_time(TIME_get_ms())
    {
    }

    void restart(void){
      _start_time = TIME_get_ms();
    }
    
    double elapsed(void){
      return TIME_get_ms() - _start_time;
    }
  };
}

#endif

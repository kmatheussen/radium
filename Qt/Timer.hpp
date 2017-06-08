#ifndef _RADIUM_QT_TIMER_HPP
#define _RADIUM_QT_TIMER_HPP

#include <QTimer>

namespace radium{
  
struct Timer{

private:
  
  struct MyTimer : public QTimer {
    Timer *_timer;
    
    void timerEvent(QTimerEvent *e) override {
      _timer->calledFromTimer();
    }
  };
  
  MyTimer _mytimer_timer;
  
public:
  
  Timer(int interval, bool start_now=true)
  {
    _mytimer_timer._timer = this;
    _mytimer_timer.setInterval(interval);
    
    if (start_now)
      _mytimer_timer.start();
  }

  void start_timer(void){
    _mytimer_timer.start();
  }
  
  void stop_timer(void){
    _mytimer_timer.stop();
  }

  const QTimer &qtimer(void){
    return _mytimer_timer;
  }
  
  virtual void calledFromTimer(void) = 0;
};
  
}

#endif

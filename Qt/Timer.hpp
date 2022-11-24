#ifndef _RADIUM_QT_TIMER_HPP
#define _RADIUM_QT_TIMER_HPP

#include <QThread>
#include <QTimer>


namespace radium{

#if 0 //defined(FOR_LINUX)

  struct Timer {

  public:
    
    Timer(int interval, bool start_now=true)
      : _interval(interval)
    {
      
      if (start_now)
        start();
    }

    void start(void){
      
    }
    
  };
  
  struct Timer : public QThread {
    
    Q_OBJECT;
    
    int _interval;
    
  public:
    
    Timer(int interval, bool start_now=true)
      : _interval(interval)
    {
      
      connect(this, SIGNAL(timeout()), this, SLOT(onTime()));
      
      if (start_now)
        start();
    }


  protected:
    
    void run() {
      while(true) {
        msleep(_interval);
        emit timeout();
      }
    }

  signals:
    void timeout();

  private slots:
  
    void onTime() {
      calledFromTimer();
    }

  public:
    
    void start_timer(void){
      start();
    }
    
  /*
  void stop_timer(void){
    _mytimer.stop();
  }
  */

  /*
  const QTimer &qtimer(void) const {
    return _mytimer;
  }
  
  QTimer &qtimer(void) {
    return _mytimer;
  }
  */
  
  virtual void calledFromTimer(void) = 0;
};
  
#else
  
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

  int _interval;

  double _start_time;
  
  Timer(int interval, bool start_now=true)
    : _interval(interval)
  {
    _mytimer_timer._timer = this;
    _mytimer_timer.setInterval(interval);
    
    if (start_now)
      _mytimer_timer.start();
  }

  virtual ~Timer(){
  }
  
  bool is_running(void) const {
    return _mytimer_timer.isActive();
  }

  double get_duration(void) const {
    return TIME_get_ms() - _start_time;
  }
  
  void start_timer(void){
    _start_time = TIME_get_ms();
    _mytimer_timer.start();
  }
  
  void stop_timer(void){
    _mytimer_timer.stop();
  }

  const QTimer &qtimer(void) const {
    return _mytimer_timer;
  }
  
  QTimer &qtimer(void) {
    return _mytimer_timer;
  }
  
  virtual void calledFromTimer(void) = 0;
};
#endif
}

#endif

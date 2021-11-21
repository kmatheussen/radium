
#ifndef _RADIUM_COMMON_PROCESS_HPP
#define _RADIUM_COMMON_PROCESS_HPP

#include <QProcess>
#include <QPointer>
#include <QString>


/*
  Workaround for broken QProcess API/implementation:

  * It's not documented how error handling in QProcess actually works. The radium::Process class is made by trying and failing.

  * The errorOccured signal is not called when timing out while calling QProcess::waitForFinished.
    The result of QProcess::error() is always "unknown error" after calling QProcess::waitForFinished, not crashed or timeout.
    The finished() signal is called though, so events seems to be processed while calling QProcess::waitForFinished.

    Edit: errorOccured is called when radium is compiled in RELEASE mode.

  * QProcess::waitForFinished returns false if the program had exited normally before calling. This is documented, and perhaps even
    the intended behaviour, but it is also obviously the wrong behavior, and the reason for writing this file.

  * When program crashes, QProcess::waitForFinished doesn't return. Instead QProcess::waitForFinished waits until it times out.
    QProcess::error() also returns TimedOut when it crashes. I have no workaround for this.    
    This bug is not present on OSX though so it could be a linux thing or something in my environment.
    (haven't tested on Windows)
 */

#include "helpers.h"

namespace radium{

struct Process : QObject{

private:
  
  QPointer<QProcess> _process = new QProcess;
  
  bool _error_occured = false;
  QProcess::ProcessError _proc_error;

  bool _has_exited = false;
  int _exit_code = -1;
  QProcess::ExitStatus _exit_status;
  
  bool _start_has_been_called = false;

  double _start_time;
  
  void connect_signals(void){
    {
      IsAlive is_alive(this);
    
      QObject::connect(_process.data(), &QProcess::errorOccurred, [is_alive, this](QProcess::ProcessError error)
                       {
                         //fprintf(stderr,"       ERROR_OCCURED: %d\n\n\n\n", (int)error);
                         if(is_alive){
                           _error_occured = true;
                           _proc_error = error;
                         }
                       });
    }

    {      
      IsAlive is_alive(this);
      QProcess *process = _process.data();
      IsAlive process_is_alive(process);
      
      QObject::connect(process, QOverload<int, QProcess::ExitStatus>::of(&QProcess::finished), [is_alive, this, process, process_is_alive](int exitCode, QProcess::ExitStatus exitStatus)
                       {
                         //fprintf(stderr,"       FINISHED: %d %d\n\n\n\n", exitCode, (int)exitStatus);

                         if(is_alive){
                           _has_exited = true;
                           _exit_status = exitStatus;
                           _exit_code = exitCode;
                           if (_exit_status==QProcess::CrashExit)
                             handle_error(-1, _start_time);

                           if(get_qprocess() == NULL){
                             R_ASSERT_NON_RELEASE(false);
                           }
                         }
                           
                         if (process_is_alive)
                           process->deleteLater();
                         else{
                           R_ASSERT_NON_RELEASE(false);
                         }                                                    
                       });
    }
  }
  
public:

  Process(){
    connect_signals();
  }
  
  QPointer<QProcess> get_qprocess(void){
    QPointer<QProcess> ret = _process.data();
    return ret;
  }
  
  void start(QString program, QStringList args){
    R_ASSERT_RETURN_IF_FALSE(_start_has_been_called==false);

    R_ASSERT_RETURN_IF_FALSE(get_qprocess() != NULL);

    _start_has_been_called = true;
    
#if defined(FOR_LINUX) || defined(FOR_MACOSX)
    QProcessEnvironment env = QProcessEnvironment::systemEnvironment();
    env.insert("LD_LIBRARY_PATH", getenv("LD_LIBRARY_PATH"));
    _process->setProcessEnvironment(env);
#endif

#if FOR_WINDOWS
    _process->start("\"" + program + "\"", args); // The surrounding """"  are necessary if path contains spaces.
#else
    _process->start(program, args);
#endif
    
    _start_time = TIME_get_ms();
  }

  enum class Status{
    NOT_STARTED,
    RUNNING,
    NOT_FINISHED,
    FINISHED,
    TIMED_OUT,
    CRASHED,
    FAILED_TO_START,
    OTHER_ERROR
  };

  bool error_has_occured(void) const {
    return _error_occured;
  }

  double get_running_time(double start_time) const {
    R_ASSERT_RETURN_IF_FALSE2(_start_has_been_called, -1);

    return TIME_get_ms() - start_time;
  }

  double get_running_time(void) const {
    return get_running_time(_start_time);
  }
    
  bool has_timed_out(double timeout, double start_time) const {
    if (timeout < 0)
      return false;
    
    return get_running_time() >= timeout;
  }

  bool has_timed_out(double timeout) const {
    return has_timed_out(timeout, _start_time);
  }

private:

  Status get_error_status(void) const {
    R_ASSERT(_error_occured);
    
    if(_proc_error==QProcess::Crashed)
      return Status::CRASHED;
    else if(_proc_error==QProcess::FailedToStart)
      return Status::FAILED_TO_START;
    else if(_proc_error==QProcess::Timedout)
      return Status::TIMED_OUT;
    else
      return Status::OTHER_ERROR;
  }
  
public:
  
  Status get_status(double timeout = -1) {
    if (!_start_has_been_called)
      return Status::NOT_STARTED;

    if (_error_occured)
      return get_error_status();

    if (_has_exited){
      R_ASSERT_RETURN_IF_FALSE2(_exit_status==QProcess::NormalExit, Status::OTHER_ERROR);
      return Status::FINISHED;
    }
    
    //printf("   timeout: %d. running_time: %d. Finished? (%d && %d). Error_occured: %d\n", (int)timeout, (int)get_running_time(), timeout >= 0, (get_running_time() >= timeout), _error_occured);
    if (has_timed_out(timeout)){
      handle_timedout(timeout, _start_time);
      if (!_error_occured){
        _proc_error = QProcess::Timedout;
        _error_occured = true;
      }

      if (_error_occured)
        return get_error_status();
    }
      
    return Status::RUNNING;
  }

  QString get_status_string(double timeout = -1) {
    R_ASSERT_RETURN_IF_FALSE2(_start_has_been_called==true, "");
    
    switch(get_status(timeout)){
      case Status::NOT_STARTED: return "has not started";
      case Status::RUNNING: return "is running";
      case Status::NOT_FINISHED: return "has not finished";
      case Status::FINISHED: return "has finished";
      case Status::TIMED_OUT: return "has timed out";
      case Status::CRASHED: return "has crashed";
      case Status::FAILED_TO_START: return "failed to start";
      case Status::OTHER_ERROR: return "did not succeed because of an unknown error";
    }

    R_ASSERT_NON_RELEASE(false);
    return "(something is wrong)";
  }

  int get_exit_code(void) {
    R_ASSERT(_start_has_been_called==true);
    R_ASSERT(get_status()==Status::FINISHED);
    return _exit_code;
  }

  void kill(void){
    if (get_qprocess() == NULL)
      return;
    
    if (_process->state() != QProcess::NotRunning)
      _process->kill();
  }
  
private:

  void handle_error(double timeout, double start_time) {
    R_ASSERT_RETURN_IF_FALSE(get_qprocess() != NULL);
#if !defined(RADIUM_USES_TSAN)
    R_ASSERT_NON_RELEASE(_error_occured==false);
#endif
    _error_occured = true;
    
    _proc_error = _process->error();
    
    //printf("C: %f %f. _proc_error: %d\n",TIME_get_ms(), start_time, _proc_error);
    
    if(_proc_error == QProcess::UnknownError){
      
      if (has_timed_out(timeout, start_time)) {
        
        _proc_error = QProcess::Timedout;
        
      } else {
        
        _proc_error = QProcess::Crashed;
        
      }
      
      
    }

  }
  
  void handle_timedout(double timeout, double start_time) {
    R_ASSERT_RETURN_IF_FALSE(get_qprocess() != NULL);
    
    if(_has_exited==false){ // Yes, QProcess::waitForFinished returns false also if the program exited normally. (!$%^!#$%!#$%)

      // The errorOccured signal does not seem to be called while calling waitForFinished(), so we have to do this check.
      if (_error_occured==false){

        handle_error(timeout, start_time);
        
      }
      
    }
      
  }
  
public:
  
  void wait_for_finished(int msecs){
    R_ASSERT_RETURN_IF_FALSE(_start_has_been_called==true);
    //fprintf(stderr,"4. Waiting. has_exited: %d.\n", _has_exited);

    if(_has_exited==true){
      R_ASSERT_NON_RELEASE(false);
      return;
    }

    R_ASSERT_RETURN_IF_FALSE(get_qprocess() != NULL);
    
    double start_time = TIME_get_ms();

    if (_process->waitForFinished(msecs)==false){

      handle_timedout(msecs, start_time);
      
      //fprintf(stderr,"5. Waiting. Error: %d. State: %d. has_exited: %d\n", (int)_process->error(), (int)_process->state(), _has_exited);
    }
    
    //fprintf(stderr,"6. Waiting\n");        
  }

  
  
};
  
}


#endif


#ifndef _RADIUM_COMMON_PROCESS_HPP
#define _RADIUM_COMMON_PROCESS_HPP

#include <QProcess>
#include <QPointer>
#include <QString>


// Workaround for horrible QProcess API.

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

public:

  QPointer<QProcess> get_qprocess(void){
    QPointer<QProcess> ret = _process.data();
    return ret;
  }
  
  void start(QString program){
    R_ASSERT_RETURN_IF_FALSE(_start_has_been_called==false);
    
    _start_has_been_called = true;
    
#if defined(FOR_LINUX) || defined(FOR_MACOSX)
    QProcessEnvironment env = QProcessEnvironment::systemEnvironment();
    env.insert("LD_LIBRARY_PATH", getenv("LD_LIBRARY_PATH"));
    _process->setProcessEnvironment(env);
#endif

#if FOR_WINDOWS
    _process->start("\"" + program + "\""); // The surrounding """"  are necessary if path contains spaces.
#else
    _process->start(program);
#endif


    {
      /* This is the ONLY way in Qt to check for error properly. Workaround found here: https://stackoverflow.com/questions/37961852/how-to-check-if-qprocess-is-executing-correctly
       */

      IsAlive is_alive(this);
    
      QObject::connect(_process, &QProcess::errorOccurred, [is_alive, this](QProcess::ProcessError error)
                       {
                         if(is_alive){
                           _error_occured = true;
                           _proc_error = error;
                         }
                       });
    }

    {
      /* And this one is also necessary. */
      
      IsAlive is_alive(this);
      QProcess *process = _process.data();
      
      QObject::connect(_process, QOverload<int, QProcess::ExitStatus>::of(&QProcess::finished), [is_alive, this, process](int exitCode, QProcess::ExitStatus exitStatus)
                       {
                         if(is_alive){
                           _has_exited = true;
                           _exit_status = exitStatus;
                           _exit_code = exitCode;
                         }
                         process->deleteLater();
                       });
    }
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
  
  Status get_status(void) const {
    if (!_start_has_been_called)
      return Status::NOT_STARTED;

    if (_error_occured){
      if(_proc_error==QProcess::Crashed)
        return Status::CRASHED;
      else if(_proc_error==QProcess::FailedToStart)
        return Status::FAILED_TO_START;
      else
        return Status::OTHER_ERROR;
    }

    if (_has_exited==false)
      return Status::RUNNING;

    if (_exit_status==QProcess::NormalExit)      
      return Status::FINISHED;
    
    if (_exit_status==QProcess::CrashExit) {      
      R_ASSERT_NON_RELEASE(false); // _error_occured should be true.
      return Status::CRASHED;      
    }

    R_ASSERT_NON_RELEASE(false); // _error_occured should be true.
    return Status::OTHER_ERROR;    
  }

  QString getStatusString(void) const {
    R_ASSERT_RETURN_IF_FALSE2(_start_has_been_called==true, "");
    
    switch(get_status()){
      case Status::NOT_STARTED: return "has not started";
      case Status::RUNNING: return "is running";
      case Status::NOT_FINISHED: return "is not finished";
      case Status::FINISHED: return "has finished";
      case Status::TIMED_OUT: return "has timed out";
      case Status::CRASHED: return "has crashed";
      case Status::FAILED_TO_START: return "failed to start";
      case Status::OTHER_ERROR: return "did not succeed because of an other error";
    }

    R_ASSERT_NON_RELEASE(false);
    return "(something is wrong)";
  }

  int get_exit_code(void) const{
    R_ASSERT(_start_has_been_called==true);
    R_ASSERT(get_status()==Status::FINISHED);
    return _exit_code;
  }
  
  void waitForFinished(int msecs){
    R_ASSERT_RETURN_IF_FALSE(_start_has_been_called==true);
    _process->waitForFinished(msecs);
  }

  
  
};
  
}


#endif


#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>


#ifdef COMPILE_EXECUTABLE

#include <QWidget>
#include <QGLFormat>
#include <QGuiApplication>

//extern void init_weak_jack(void);

int main(int argc, char **argv){

  QGuiApplication app(argc, argv);

  if (QGLFormat::hasOpenGL()==false)
    return 0;

  QGLFormat::openGLVersionFlags();
    
  return 0;
}


#else // COMPILE_EXECUTABLE -> !COMPILE_EXECUTABLE

#include <QProcess>

#include "../common/nsmtracker.h"
#include "../common/visual_proc.h"

#include "CheckOpenGL_proc.h"


// Returns true if we want to exit;
bool CHECKOPENGL_checkit(void){

  QProcess *myProcess = new QProcess();

#if defined(FOR_LINUX) || defined(FOR_MACOSX)
  QProcessEnvironment env = QProcessEnvironment::systemEnvironment();
  env.insert("LD_LIBRARY_PATH", getenv("LD_LIBRARY_PATH"));
  myProcess->setProcessEnvironment(env);
#endif

#if FOR_WINDOWS
  QString program = OS_get_full_program_file_path("radium_check_opengl.exe");
#else
  QString program = OS_get_full_program_file_path("radium_check_opengl");
#endif

#if defined(FOR_WINDOWS)
  program = QString("\"") + program + "\""; // necessary if path contains spaces.
#endif

  
  myProcess->start(program);

#if defined(RELEASE)
  int msecs = 10000;
#else
  int msecs = 5000;
#endif

  int status = 0;

  bool timed_out = (myProcess->waitForFinished(msecs) == false);

  bool may_try = false;

  QString message;
  
  if (timed_out==true) {

    message = "OpenGL test program timed out. It might help to update the GFX driver.";
    may_try = true;
    
  } else if (myProcess->exitStatus()==QProcess::CrashExit) {

    message = "OpenGL crashed. Radium can not start. It might help to update the GFX driver.";
    
  } else {
    
    status = myProcess->exitCode();

    if (status != 0){
      message = "OpenGL test process returned a strange value. It might help to update the GFX driver.";
      may_try = true;
    }
  }

  if (message != ""){
    vector_t v = {};
    
    if(may_try)
      VECTOR_push_back(&v, "Try to run anyway");

    int exit_ = VECTOR_push_back(&v, "Exit program");
    
    int hmmm = GFX_Message(&v, "%s", message.toUtf8().constData());
    
    if (hmmm==exit_)
      return true;
  }

  myProcess->connect(myProcess, SIGNAL(finished(int)), myProcess, SLOT(deleteLater()));

  msleep(1000);

  return false;
}


#endif // !COMPILE_EXECUTABLE

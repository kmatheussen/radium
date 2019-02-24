
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
#include "../common/Process.hpp"

#include "CheckOpenGL_proc.h"


// Returns true if we want to exit;
bool CHECKOPENGL_checkit(void){
  radium::Process process;

#if FOR_WINDOWS  
  process.start(OS_get_full_program_file_path("radium_check_opengl.exe"));
#else
  process.start(OS_get_full_program_file_path("radium_check_opengl"));
#endif
  
#if defined(RELEASE)
  int msecs = 10000;
#else
  int msecs = 5000;
#endif
  
  process.waitForFinished(msecs);

  if (process.error_has_occured()){
    bool may_try = true;

    if (process.get_status()==radium::Process::Status::CRASHED)
      may_try = false;

    QString message = "OpenGL process " + process.getStatusString() + ". Radium can not start. It might help to update the GFX driver.";
    
    if (!may_try)
      message = "OpenGL crashed. Radium can not start. It might help to update the GFX driver.";
    
    vector_t v = {};
    
    if(may_try)
      VECTOR_push_back(&v, "Try to run anyway");
    
    int exit_ = VECTOR_push_back(&v, "Exit program");
    
    int hmmm = GFX_Message(&v, "%s", message.toUtf8().constData());
    
    if (hmmm==exit_)
      return true;
  }

  return false;
}


#endif // !COMPILE_EXECUTABLE

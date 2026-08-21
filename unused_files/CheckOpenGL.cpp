
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>


#ifdef COMPILE_EXECUTABLE

#include <QWidget>
//#include <QGLFormat>
#include <QGuiApplication>
#include <QOperatingSystemVersion>


//extern void init_weak_jack(void);

int main(int argc, char **argv){

  /*
  int *ai2=NULL;
  ai2[0] = 50;
  */

#if defined(FOR_MACOSX)
  if (QOperatingSystemVersion::current() >= QOperatingSystemVersion::MacOSBigSur)
    setenv("QT_MAC_WANTS_LAYER", "1", 1);
#endif

  QGuiApplication app(argc, argv);
#if 0 // FIX
  if (QGLFormat::hasOpenGL()==false)
    return 0;

  QGLFormat::openGLVersionFlags();
#endif
  return 0;
}


#else // COMPILE_EXECUTABLE -> !COMPILE_EXECUTABLE

#include <QProcess>

#include "../common/nsmtracker.h"
#include "../common/visual_proc.h"
#include "../common/Process.hpp"

#include "CheckOpenGL_proc.h"


// Returns true if we want to exit;
bool CHECKOPENGL_checkit(void)
{
	return false;
	
  radium::Process process;

#if FOR_WINDOWS  
  process.start(STRING_get_qstring(OS_get_full_program_file_path("radium_check_opengl.exe").id), {});
#else
  process.start(STRING_get_qstring(OS_get_full_program_file_path("radium_check_opengl").id), {});
#endif
  
#if defined(RELEASE)
  int msecs = 10000;
#else
  int msecs = 5000;
#endif
  
  process.wait_for_finished(msecs);

  if (process.error_has_occured()){

    CRASHREPORTER_dont_report();
      
    bool may_try = true;

    if (process.get_status()==radium::Process::Status::CRASHED)
      may_try = false;

    QString message = "OpenGL process " + process.get_status_string() + ".";
    
    if (!may_try)
      message = "OpenGL crashed.";

    message += " Radium can not start. This is not a bug in Radium. It might help to update the GFX driver.";
    
    vector_t v = {};
    
    if(may_try)
      VECTOR_push_back(&v, "Try to run anyway");
    
    int exit_ = VECTOR_push_back(&v, "Exit program");
    
    int hmmm = GFX_Message(&v, "%s", message.toUtf8().constData());
    
    if (hmmm==exit_)
      return true;

    // No. Don't enable crash reporter. OpenGL is likely to crash now and we don't want those reports.
    //CRASHREPORTER_do_report();
  }

  return false;
}


#endif // !COMPILE_EXECUTABLE

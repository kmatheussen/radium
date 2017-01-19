#include <stdio.h>

#include <QWidget>

#include <qapplication.h>
#include <qmainwindow.h>
#include <QPushButton>
#include <QProcess>
#include <QDir>

#include "../common/nsmtracker.h"

#include "helpers.h"

#ifndef TEST_MAIN

#define LANGSPEC "C"  // LANGSPEC doesn't work in OS_error_proc.h, so can't include nsmtracker.h. Haven't found out why this happens.

  #include "../common/OS_settings_proc.h"

#endif


/*
  g++ Qt_Error.cpp `pkg-config --cflags --libs QtGui` -DCOMPILE_EXECUTABLE -o ../bin/radium_error_message && ../bin/radium_error_message gakkgakk1
  g++ Qt_Error.cpp `pkg-config --cflags --libs QtGui` -DTEST_MAIN -o test_radium_error_message && ./test_radium_error_message gakkgakk2
 */


#ifdef COMPILE_EXECUTABLE

static int show_message(const char *message){

  MyQMessageBox msgBox;

  msgBox.setText(QString(message));

  QPushButton *continue_button = msgBox.addButton("continue", QMessageBox::AcceptRole);
  QPushButton *quit_button     = msgBox.addButton("quit", QMessageBox::AcceptRole);
  QPushButton *ignore1_button  = msgBox.addButton("ignore warnings and errors for two seconds", QMessageBox::AcceptRole);
  QPushButton *ignore2_button  = msgBox.addButton("ignore warnings and errors for the rest of the program", QMessageBox::AcceptRole);

  msgBox.show();
  msgBox.raise();
  msgBox.activateWindow();
  
  printf("hepp: %d\n",msgBox.exec()); // safeExec(&msgBox)); <-- We are not inside the radium executable here.

  QAbstractButton *clicked_button = msgBox.clickedButton();

  if(clicked_button->text()==continue_button->text())
    return 0;
  if(clicked_button==quit_button)
    return 1;
  if(clicked_button==ignore1_button)
    return 2;
  if(clicked_button==ignore2_button)
    return 3;
  
  fprintf(stderr,"what?\n");
  return -1;
}

QMainWindow *g_main_window = NULL; // referenced by helpers.h
QSplashScreen *g_splashscreen = NULL; // referenced by helpers.h
bool g_radium_runs_custom_exec = false; // used by helpers.h
QWidget *MIXERSTRIPS_get_curr_widget(void){
  return NULL;
}
bool MIXERSTRIPS_has_mouse_pointer(void){  // used by helpers.h
  return false;
}

int main(int argc, char **argv){
  QCoreApplication::setLibraryPaths(QStringList());
  
  QApplication app(argc,argv);
  int ret = show_message(argv[1]);
  printf("ret: %d\n",ret);
  return ret;
}



#else // COMPILE_EXECUTABLE

#include "../OpenGL/Widget_proc.h"

extern bool g_qt_is_running;

extern "C" {
int SYSTEM_show_message(const char *message){

  if (g_qt_is_running==false){
    fprintf(stderr,"\n\n\n   === %s ===\n\n\n", message);
    return 0;
  }
    
#if FOR_WINDOWS
  QString program = OS_get_full_program_file_path("radium_error_message.exe");
#else
  QString program = OS_get_full_program_file_path("radium_error_message");
#endif
  
  QStringList arguments;
  arguments << message;

  QProcess myProcess;

#if defined(FOR_LINUX) || defined(FOR_MACOSX)
  QProcessEnvironment env = QProcessEnvironment::systemEnvironment();
  env.insert("LD_LIBRARY_PATH", getenv("LD_LIBRARY_PATH"));
  myProcess.setProcessEnvironment(env);
#endif

  GL_lock();
  
  myProcess.start(program, arguments);

  if (myProcess.waitForFinished(-1)==false) {
    fprintf(stderr,"Something went wrong when trying to start radium_error_message executable \"%s\"\n",(program+" "+message).toUtf8().constData());
    //system((program+" "+message).toUtf8().constData());
    //abort();

    GL_unlock();
    return 0;
  }

  GL_unlock();
  
  return myProcess.exitCode();
}
}



#ifdef TEST_MAIN
int main(int argc, char **argv){
  QApplication app(argc,argv);
  int ret = SYSTEM_show_message(argv[1]);
  printf("ret: %d\n",ret);
  return 0;
}
#endif // TEST_MAIN


  
#endif // !COMPILE_EXECUTABLE

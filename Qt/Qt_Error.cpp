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

  //QPointer<MyQMessageBox> msgBox = MyQMessageBox::create();
  ScopedQPointer<MyQMessageBox> msgBox(MyQMessageBox::create(true));
  
  msgBox->setText(QString(message));

  QPushButton *continue_button = msgBox->addButton("continue", QMessageBox::AcceptRole);
  QPushButton *quit_button     = msgBox->addButton("quit", QMessageBox::AcceptRole);
  QPushButton *ignore1_button  = msgBox->addButton("ignore warnings and errors for two seconds", QMessageBox::AcceptRole);
  QPushButton *ignore2_button  = msgBox->addButton("ignore warnings and errors for the rest of the program", QMessageBox::AcceptRole);

  msgBox->show();
  msgBox->raise();
  msgBox->activateWindow();
  
  printf("hepp: %d\n",msgBox->exec()); // safeExec(&msgBox)); <-- We are not inside the radium executable here.

  if (msgBox==NULL)
    return -1;
      
  QAbstractButton *clicked_button = msgBox->clickedButton();

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
bool g_qt_is_painting = false;
const char *g_qt_is_painting_where = "nowhere";
QVector<QWidget*> g_static_toplevel_widgets; // same here
QPointer<QWidget> g_current_parent_before_qmenu_opened;
QPointer<QMenu> g_curr_popup_qmenu; // and here
QWidget *MIXERSTRIPS_get_curr_widget(void){
  return NULL;
}
bool MIXERSTRIPS_has_mouse_pointer(void){  // used by helpers.h
  return false;
}


int main(int argc, char **argv){
#if FOR_LINUX
  bool faulty_installation = false;
  if(getenv("QT_QPA_PLATFORM_PLUGIN_PATH")==NULL){
    faulty_installation = true;
  }else{
    QCoreApplication::setLibraryPaths(QStringList());
  }
#else
  QCoreApplication::setLibraryPaths(QStringList());
#endif

  argv = getQApplicationConstructorArgs(argc, argv);
  QApplication app(argc, argv);

#if FOR_LINUX
  if(faulty_installation){
    QMessageBox msgBox;
    msgBox.setIcon(QMessageBox::Critical);
    msgBox.setText("Error!\nRadium has not been installed properly.\nRadium is likely to be unstable because of this.\n(See bin/packages/README for instructions on how to properly install Radium)");
    msgBox.setStandardButtons(QMessageBox::Ok);
    msgBox.exec();
  }
#endif

  int ret = show_message(argv[1]);
  printf("ret: %d\n",ret);
  return ret;
}



#else // COMPILE_EXECUTABLE

#include "../OpenGL/Widget_proc.h"

extern bool g_qt_is_running;

extern "C" {
int SYSTEM_show_message(const char *message){

  static bool ignore_forever = false;
  static double ignore_until = -1;
  static double last_time = -1;
  double time_now = TIME_get_ms();

  if (time_now <= ignore_until || ignore_forever)
    return 0;

  if (g_qt_is_running==false){
    fprintf(stderr,"\n\n\n   === %s ===\n\n\n", message);
    return 0;
  }

  radium::ScopedExec scopedExec(false);
    

#if FOR_WINDOWS
  QString program = OS_get_full_program_file_path("radium_error_message.exe");
#else
  QString program = OS_get_full_program_file_path("radium_error_message");
#endif
  
  QStringList arguments;
  arguments << message;

  QProcess *myProcess = new QProcess();
  myProcess->connect(myProcess, SIGNAL(finished(int)), myProcess, SLOT(deleteLater()));
  
#if defined(FOR_LINUX) || defined(FOR_MACOSX)
  QProcessEnvironment env = QProcessEnvironment::systemEnvironment();
  env.insert("LD_LIBRARY_PATH", getenv("LD_LIBRARY_PATH"));
  myProcess->setProcessEnvironment(env);
#endif

  GL_lock();
  
  myProcess->start(program, arguments);

  if (myProcess->waitForStarted(10000)==false) {
    fprintf(stderr,"Something went wrong when trying to start radium_error_message executable \"%s\"\n",(program+" "+message).toUtf8().constData());
    //system((program+" "+message).toUtf8().constData());
    //abort();

    GL_unlock();
    return 0;
  }

  GL_unlock();

  if (myProcess->waitForFinished(20000)==false){ // Have timeout value in case the GUI doesn't show up or is hidden somehow.
    printf("WARN: radium_error_message timed out. Returning -1\n");
    return -1;
  }
  
  last_time = TIME_get_ms();

  R_ASSERT_RETURN_IF_FALSE2(myProcess->exitStatus()==QProcess::NormalExit, -1);
  
      
  int status = myProcess->exitCode();

  if (status==1){
    exit(-1);
    abort();
  }
    
  if (status==2)
    ignore_until = last_time + 2000;
  else if (status==3)
    ignore_forever = true;

  return status;
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

#include <stdio.h>

#include <qapplication.h>
#include <qmainwindow.h>
#include <QMessageBox>
#include <QPushButton>
#include <QProcess>
#include <QDir>

#ifndef TEST_MAIN

#define LANGSPEC "C"  // LANGSPEC doesn't work in OS_error_proc.h, so can't include nsmtracker.h. Haven't found out why this happens.

  #include "../common/OS_settings_proc.h"

#endif


/*
  g++ Qt_Error.cpp `pkg-config --cflags --libs QtGui` -DCOMPILE_EXECUTABLE -o ../bin/radium_error_message && ../bin/radium_error_message gakkgakk1
  g++ Qt_Error.cpp `pkg-config --cflags --libs QtGui` -DTEST_MAIN -o test_radium_error_message && ./test_radium_error_message gakkgakk2
 */


#ifdef COMPILE_EXECUTABLE

namespace{
  struct MyQMessageBox : public QMessageBox {
  };
}
  
static int show_message(const char *message){

  MyQMessageBox msgBox;

#ifdef RELEASE
  msgBox.setWindowFlags(Qt::Popup);
#endif

  msgBox.setText(QString(message));

  QPushButton *continue_button = msgBox.addButton("continue", QMessageBox::AcceptRole);
  QPushButton *quit_button     = msgBox.addButton("quit", QMessageBox::AcceptRole);
  QPushButton *ignore1_button  = msgBox.addButton("ignore warnings and errors for two seconds", QMessageBox::AcceptRole);
  QPushButton *ignore2_button  = msgBox.addButton("ignore warnings and errors for the rest of the program", QMessageBox::AcceptRole);

  msgBox.show();
  msgBox.raise();
  msgBox.activateWindow();
  
  printf("hepp: %d\n",msgBox.exec());

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

int main(int argc, char **argv){
  QCoreApplication::setLibraryPaths(QStringList());
  
  QApplication app(argc,argv);
  int ret = show_message(argv[1]);
  printf("ret: %d\n",ret);
  return ret;
}



#else // COMPILE_EXECUTABLE



extern "C" {
int SYSTEM_show_message(const char *message){
  
  QString program = QCoreApplication::applicationDirPath() + QDir::separator() + "radium_error_message";

  QStringList arguments;
  arguments << message;

  QProcess myProcess;

#if defined(FOR_LINUX) || defined(FOR_MACOSX)
  QProcessEnvironment env = QProcessEnvironment::systemEnvironment();
  env.insert("LD_LIBRARY_PATH", getenv("LD_LIBRARY_PATH"));
  myProcess.setProcessEnvironment(env);
#endif
  
  myProcess.start(program, arguments);

  if (myProcess.waitForFinished(-1)==false) {
    fprintf(stderr,"Something went wrong when trying to start radium_error_message executable \"%s\"\n",(program+" "+message).toUtf8().constData());
    //system((program+" "+message).toUtf8().constData());
    //abort();
    return 0;
  }

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

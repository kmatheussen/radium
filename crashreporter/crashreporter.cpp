/* Copyright 2012 Kjetil S. Matheussen

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. */


#include <stdio.h>
#include <unistd.h>
#include <signal.h>
#include <time.h>
#include <errno.h>

#if defined(FOR_LINUX)
#  include <sys/types.h>
#endif

#include <QSharedMemory>
#include <QString>
#include <QApplication>
#include <QMessageBox>
#include <QFile>
#include <QTextStream>
#include <QTime>
#include <QTextEdit>
#include <QLabel>
#include <QLayout>

#if defined(CRASHREPORTER_BIN)
#include <QNetworkRequest>
#include <QNetworkAccessManager>
#endif

#include "../common/nsmtracker.h"
#include "../common/OS_settings_proc.h"

#include "crashreporter_proc.h"

#if defined(FOR_WINDOWS)
#  include <windows.h>
#  define mysleep(ms) Sleep(ms)
#else
#  define mysleep(ms) usleep((ms)*1000);
#endif

#define MESSAGE_LEN (1024*32)

struct Report{
  enum{
    NO_MESSAGE,
    THERE_IS_A_MESSAGE,
    EXIT
  } status;

  char data[MESSAGE_LEN];

  Report() : status(NO_MESSAGE) {}
};


static QSharedMemory *g_sharedmemory;

#if defined(CRASHREPORTER_BIN)


static void exit_handler(int sig){
  fprintf(stderr,"\n\nCrashreporter: Crashreporter crashed. signal: %d\n\n",sig);
  delete g_sharedmemory;
  abort();
}

#if defined(FOR_LINUX)
static QString string_to_file(QString filename){
  QFile file(filename);
  bool ret = file.open(QIODevice::ReadOnly | QIODevice::Text);
  if( ret )
    {
      QTextStream stream(&file);
      QString content = stream.readAll();
      return content;
    }
  return "";
}
#endif

int main(int argc, char **argv){
  QString key = argv[1];

#if defined(FOR_LINUX)
  pid_t parent_pid = atoi(argv[2]);
#endif

  g_sharedmemory = new QSharedMemory(key);

  signal(SIGSEGV,exit_handler);
  signal(SIGFPE,exit_handler);
  signal(SIGINT,exit_handler);
  signal(SIGTERM,exit_handler);

  QApplication app(argc,argv);

  if(g_sharedmemory->attach()==false){
    fprintf(stderr,"Crashreporter: Couldn't attach... Error: %s\n",g_sharedmemory->error()==QSharedMemory::NoError?"No error (?)":g_sharedmemory->errorString().toAscii().data());
    return 0;
  }

  bool do_exit = false;

  Report *report = (Report*)g_sharedmemory->data();

  QString tosend = VERSION "\n\n";

  while(do_exit==false){
    mysleep(1000);


#if defined(FOR_LINUX)
    // Should something like this be done for windows as well?
    static int counter = 10; // wait 10 seconds

    //printf("killing: %d / %d\n",(int)kill(parent_pid,0),(int)ESRCH);

    if(kill(parent_pid,0)==-1){
      counter--;
      if(counter>0){
        if(0)
          fprintf(stderr, "Radium crashreporter: Seems like parent process died. Counting down before exit: %d\n", counter);
      }else{
        if(0)
          fprintf(stderr, "Radium crashreporter: Seems like parent process died. Exiting\n");
        do_exit=true;
      }
    }
#endif    


    if(g_sharedmemory->lock()==false){
      fprintf(stderr,"Crashreporter: Couldn't lock...\n");
      continue;
    }else{

      switch(report->status){
      case Report::NO_MESSAGE:
        break;
      case Report::THERE_IS_A_MESSAGE:
        {
          fprintf(stderr,"Got message. Waiting 2 seconds.\n");

          g_sharedmemory->unlock();
          mysleep(2000);
          g_sharedmemory->lock();

          fprintf(stderr,"Got message:\n%s\n",report->data);
          
          tosend += report->data;
          tosend += "\n\n";

#if defined(FOR_LINUX)
          tosend += "LINUX\n\n";
          tosend += "/etc/os-release: "+string_to_file("/etc/os-release");
          tosend += "\n\n";
          tosend += "/proc/version: "+string_to_file("/proc/version");
          tosend += "\n\n";
          tosend += "/proc/cpuinfo: "+string_to_file("/proc/cpuinfo");
#endif

          {
            QMessageBox box;

            box.setIcon(QMessageBox::Critical);
            box.addButton("SEND", QMessageBox::AcceptRole);
            box.addButton("DON'T SEND", QMessageBox::RejectRole);

            box.setText("Radium Crashed. :((");
            box.setInformativeText("This crash will be automatically reported when you press \"SEND\".\n"
                                   "\n"
                                   "The report is sent anonymously, and will only be seen by the author of Radium.\n"
                                   //"The reporting is anonymous, and the report will not be available to the public.\n"
                                   "\n"
                                   "Only the information in \"Show details...\" is sent.\n"
                                   "\n"
                                   "Please don't send more than one message for the same crash.\n"

                                   );
            box.setDetailedText(tosend);

            QLabel space(" ");
            box.layout()->addWidget(&space);

            QLabel text_edit_label("\n\n"
                                   "Please also include additional information below.\n"
                                   "\n"
                                   "The best type of help you "
                                   "can give is to write "
                                   "down\na step by step "
                                   "recipe in the following "
                                   "format:"
                                   "\n\n"
                                   "1. Start Radium\n"
                                   "2. Move the cursor to track 3.\n"
                                   "3. Press the Q button.\n"
                                   "4. Radium crashes\n"
                                   "\n"
                                   );

            //text_edit.setMinimumWidth(1000000);
            //text_edit.setSizePolicy(QSizePolicy(QSizePolicy::Minimum,QSizePolicy::Minimum));
            box.layout()->addWidget(&text_edit_label);

            QLabel space2(" ");
            box.layout()->addWidget(&space2);

            QTextEdit text_edit;
            text_edit.setText("<Please add recipe and/or email address here>");
            //text_edit.setMinimumWidth(1000000);
            //text_edit.setSizePolicy(QSizePolicy(QSizePolicy::Minimum,QSizePolicy::Minimum));
            box.layout()->addWidget(&text_edit);

            box.show();

            box.activateWindow();
            box.raise();
            //box.stackUnder(box.parentWidget());
            box.setWindowFlags(Qt::WindowStaysOnTopHint);
            box.setWindowModality(Qt::ApplicationModal);

#ifdef FOR_WINDOWS
            HWND wnd=box.winId();
            SetFocus(wnd);
            SetWindowPos(wnd, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE|SWP_NOSIZE);
#endif
            
            int ret = box.exec();

            if(ret==QMessageBox::AcceptRole){

              QByteArray data;
              QUrl params;
              params.addQueryItem("data", tosend);
              data.append(params.toString().toAscii(),params.toString().length()-1);
              data.remove(0,1);
              data.append("\n");
              data.append(text_edit.toPlainText());

              QNetworkAccessManager nam;
              QNetworkRequest request(QUrl("http://users.notam02.no/~kjetism/radium/crashreport.php"));
              request.setHeader(QNetworkRequest::ContentTypeHeader, "application/x-www-form-urlencoded" );

              nam.post(request,data);
            
#if 1
              {
                QMessageBox box;
                box.setText("Thanks for reporting the bug!");
                box.setInformativeText("The bug will hopefully be fixed in the next version of Radium.");
                box.exec();
              }
#endif

              g_sharedmemory->unlock();
              mysleep(50000);
              g_sharedmemory->lock();
            }
          }

          do_exit=true;
          break;
        }

      case Report::EXIT:
        do_exit=true;
        break;

      }

    }g_sharedmemory->unlock();
  }

  //fprintf(stderr,"Crashreporter exiting\n");

  g_sharedmemory->detach();

  delete g_sharedmemory;

  return 0;
}

#endif // defined(CRASHREPORTER_BIN)



#if !defined(CRASHREPORTER_BIN)

void CRASHREPORTER_init(void){
  QString key = "radium_crashreporter_" + QString::number(QDateTime::currentMSecsSinceEpoch());

  g_sharedmemory = new QSharedMemory(key);

  if(g_sharedmemory->create(sizeof(Report))==false){
    fprintf(stderr,"Crashreporter: Couldn't create... Error: %s\n",g_sharedmemory->error()==QSharedMemory::NoError?"No error (?)":g_sharedmemory->errorString().toAscii().data());
#ifndef RELEASE
    fprintf(stderr,"press return\n");
    abort();
#endif
  }

#if defined(FOR_WINDOWS)
  //system(QString(QString("start ") + QCoreApplication::applicationDirPath() + "\\crashreporter " + key + " /B").toAscii());

#if 0
  QString command=QString("start /B ") + QCoreApplication::applicationDirPath() + "\\crashreporter.exe " + key;
  //system(command.toAscii());
#endif
  //QString command=QCoreApplication::applicationDirPath() + "\\crashreporter.exe";
  QString command="crashreporter.exe";

  char *c = strdup(command.toAscii());
  char *k = strdup(key.toAscii());

  if(_spawnl( _P_DETACH, c, c, k, NULL)==-1){
    //if(_spawnl( _P_NOWAIT, c, c, k, NULL)==-1){
    //if(_spawnl( _P_NOWAIT, "start", "start", "/B", c, k, NULL)==-1){
    fprintf(stderr,"Couldn't launch crashreporter: \"%s\" \"%s\"\n",c,k);
    Sleep(3000);
  }


  //execv(args[0],args);

  CRASHREPORTER_windows_init();

#elif defined(FOR_LINUX)
  //if(system(QString(QCoreApplication::applicationDirPath() + "/crashreporter " + key + " " + QString::number(getpid()) + "&").toAscii())==-1) { // how to fix utf-8 here ?
  if(system(QString("./crashreporter " + key + " " + QString::number(getpid()) + "&").toAscii())==-1) {
    fprintf(stderr,"Couldn't start crashreporter\n");
#ifndef RELEASE
    abort();
#endif
  }

  CRASHREPORTER_posix_init();

#elif defined(FOR_MACOSX)

#else
# error "Unknown machine"

#endif

  mysleep(1000);
}

void CRASHREPORTER_report_crash(const char **messages, int num_messages){
  g_sharedmemory->lock();{

    static int pos=0;
    static int bytes_left=MESSAGE_LEN - 1;

    Report *report = (Report*)g_sharedmemory->data();

    for(int i=0;i<num_messages;i++){

      if(num_messages>1)
        snprintf(report->data+pos,bytes_left,"%d: %s\n",i,messages[i]);
      else
        snprintf(report->data+pos,bytes_left,"%s\n",messages[i]);

      pos=strlen(report->data);
      bytes_left = MESSAGE_LEN - pos - 1;
    }

    if(num_messages!=1){
      snprintf(report->data+pos,bytes_left,"\n\n");
      pos=strlen(report->data);
      bytes_left = MESSAGE_LEN - pos - 1;
    }

    report->status=Report::THERE_IS_A_MESSAGE;
    
  }g_sharedmemory->unlock();
}

void CRASHREPORTER_close(void){
#if defined(FOR_WINDOWS)
  CRASHREPORTER_windows_close();
#endif

  Report *report = (Report*)g_sharedmemory->data();

  if(g_sharedmemory->lock()==true){

    report->status=Report::EXIT;

    g_sharedmemory->unlock();
  }

  g_sharedmemory->detach();

  delete g_sharedmemory;
}


#endif // !defined(CRASHREPORTER_BIN)


#ifdef TEST_MAIN

void goto2(void){
  int a = 5;
  char *b=NULL;
  //printf("g: %d\n",a/0);
  sprintf(b,"gakk");
}

void goto1(void){
  goto2();
}

int main(){
  CRASHREPORTER_init();
  //getc(stdin);
  //CRASHREPORTER_close();
  //CRASHREPORTER_report_crash("Gakk!!!\n1\n2\n3\n");
  goto1();
}

#endif // TEST_MAIN

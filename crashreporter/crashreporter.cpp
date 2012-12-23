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

#include <QSharedMemory>
#include <QString>
#include <QApplication>
#include <QMessageBox>
#include <QFile>
#include <QTextStream>
#include <QTime>

#if defined(CRASHREPORTER_BIN)
#include <QNetworkRequest>
#include <QNetworkAccessManager>
#endif

#include "../common/nsmtracker.h"
#include "../common/OS_settings_proc.h"

#include "crashreporter_proc.h"


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

int main(int argc, char **argv){
  QString key = argv[1];

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
    sleep(1);
    
    if(g_sharedmemory->lock()==false){
      fprintf(stderr,"Crashreporter: Couldn't lock...\n");
      continue;
    }else{

      switch(report->status){
      case Report::NO_MESSAGE:
        break;
      case Report::THERE_IS_A_MESSAGE:
        {
          fprintf(stderr,"Got message. Waiting 5 seconds.\n");

          g_sharedmemory->unlock();
          sleep(5);
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
                                   "The reporting is anonymous.\n"
                                   "\n"
                                   "Only the information in \"Show details...\" is sent.\n"
                                   );
            box.setDetailedText(tosend);
            int ret = box.exec();

            if(ret==QMessageBox::AcceptRole){

              QByteArray data;
              QUrl params;
              params.addQueryItem("data", tosend);
              data.append(params.toString());
              data.remove(0,1);

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
              sleep(50);
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
  QTime now = QTime::currentTime();
  qsrand(now.msec());
  QString key = "radium_crashreporter_" + QString::number(qrand());

  g_sharedmemory = new QSharedMemory(key);

  if(g_sharedmemory->create(sizeof(Report))==false){
    fprintf(stderr,"Crashreporter: Couldn't attach... Error: %s\n",g_sharedmemory->error()==QSharedMemory::NoError?"No error (?)":g_sharedmemory->errorString().toAscii().data());
  }

#if defined(FOR_WINDOWS)
  system(QString(QString("start ") + OS_get_program_path() + "\crashreporter " + key + " /B").toAscii());

#elif defined(FOR_LINUX) || defined(FOR_MACOSX)
  system(QString(QString(OS_get_program_path()) + "/crashreporter " + key + "&").toAscii());
  CRASHREPORTER_posix_init();

#else
# error "Unknown machine"

#endif

  sleep(1);
}

void CRASHREPORTER_report_crash(const char **messages, int num_messages){
  g_sharedmemory->lock();{

    static int pos=0;
    static int bytes_left=MESSAGE_LEN - 1;

    Report *report = (Report*)g_sharedmemory->data();

    for(int i=0;i<num_messages;i++){
    
      snprintf(report->data+pos,bytes_left,"%d: %s\n",i,messages[i]);

      pos=strlen(report->data);
      bytes_left = MESSAGE_LEN - pos - 1;
    }

    {
      snprintf(report->data+pos,bytes_left,"\n\n");
      pos=strlen(report->data);
      bytes_left = MESSAGE_LEN - pos - 1;
    }

    report->status=Report::THERE_IS_A_MESSAGE;
    
  }g_sharedmemory->unlock();
}

void CRASHREPORTER_close(void){
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

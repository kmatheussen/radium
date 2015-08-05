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

#include <QString>
#include <QApplication>
#include <QMessageBox>
#include <QFile>
#include <QTextStream>
#include <QTime>
#include <QTextEdit>
#include <QLabel>
#include <QLayout>

#include <QTemporaryFile>
#include <QDir>

#include <QProcess>

#include <QNetworkRequest>
#include <QNetworkAccessManager>

#include "../common/nsmtracker.h"
#include "../common/OS_settings_proc.h"
#include "../common/OS_Player_proc.h"
#include "../common/OS_string_proc.h"
#include "../common/threading.h"
#include "../common/disk_save_proc.h"
#include "../common/undo.h"
#include "../OpenGL/Widget_proc.h"

#include "../audio/SoundProducer_proc.h"
//extern void SP_write_mixer_tree_to_disk(QFile *file);

#include "crashreporter_proc.h"

#if defined(FOR_WINDOWS)
#  include <windows.h>
#  define mysleep(ms) Sleep(ms)
#else
#  define mysleep(ms) usleep((ms)*1000);
#endif


#if !defined(CRASHREPORTER_BIN)
static const char *g_no_plugin_name = "<noplugin>";
#endif

#define NOPLUGINNAMES "<nopluginnames>"
#define NOEMERGENCYSAVE "<noemergencysave>"


static QString toBase64(QString s){
  return s.toLocal8Bit().toBase64();
}

static QString fromBase64(QString encoded){
  return QString::fromLocal8Bit(QByteArray::fromBase64(encoded.toLocal8Bit()).data());
}


static QString file_to_string(QString filename){
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

static void delete_file(QString filename){
  QFile::remove(filename);
}

static void clear_file(QString filename){
  QFile file(filename);
  file.open(QIODevice::WriteOnly | QIODevice::Text);
  file.write("");
  file.close();
}


#if !defined(CRASHREPORTER_BIN)

#define NUM_EVENTS 100

static const char *g_event_log[NUM_EVENTS] = {""};
static volatile int g_event_pos = 0;

void EVENTLOG_add_event(const char *log_entry){
 R_ASSERT(THREADING_is_main_thread());

 g_event_log[g_event_pos] = log_entry;
 
 g_event_pos++;
 if (g_event_pos==NUM_EVENTS)
   g_event_pos = 0;
}

#endif



static void send_crash_message_to_server(QString message, QString plugin_names, QString emergency_save_filename, bool is_crash){

  fprintf(stderr,"Got message:\n%s\n",message.toUtf8().constData());

  {
    QMessageBox box;
    
    box.setIcon(QMessageBox::Critical);
    box.addButton("SEND", QMessageBox::AcceptRole);
    box.addButton("DON'T SEND", QMessageBox::RejectRole);
    
    if (is_crash)
      box.setText("Radium Crashed. :((");
    else
      box.setText("Radium is in a state it should not be in.\n"
                  );

    bool dosave = emergency_save_filename!=QString(NOEMERGENCYSAVE);
    
    box.setInformativeText(QString("This %0 will be automatically reported when you press \"SEND\".\n"
                                   "\n"
                                   "The report is sent anonymously, and will only be seen by the author of Radium.\n"
                                   "\n"
                                   "Only the information in \"Show details...\" is sent.\n"
                                   "\n"
                                   "Please don't report the same %0 more than two or three times.\n"
                                   ).arg(is_crash?"crash":"incident")
                           + ( (is_crash && plugin_names != NOPLUGINNAMES)
                               ? QString("\nPlease note that the following third party plugins: \"" + plugin_names + "\" was/were currently processing audio. It/they might be responsible for the crash.\n")
                               : QString())
                           + (!is_crash ? "\nAfterwards, you should save your work and start the program again.\n\nIf this window just pops up again immediately after closing it, just hide it instead." : "")
                           + (dosave ? "\nAn emergency version of your song has been saved as \""+emergency_save_filename+"\". However, this file should not be trusted. It could be malformed. (it is most likely okay though)" : "")
                           + "\n"
                           );
    box.setDetailedText(message);

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
      params.addQueryItem("data", message);
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

    }
  }

}



#if defined(CRASHREPORTER_BIN)

int main(int argc, char **argv){
  
  QCoreApplication::setLibraryPaths(QStringList());
  
  QApplication app(argc,argv);

  QString filename = fromBase64(argv[1]);

  QString running_plugin_names = fromBase64(argv[2]);
  
  QString emergency_save_filename = fromBase64(argv[3]);
    
  bool is_crash = QString(argv[4])=="is_crash";

  send_crash_message_to_server(file_to_string(filename), running_plugin_names, emergency_save_filename, is_crash);

  if (is_crash)
    delete_file(filename);
  else
    clear_file(filename);

  
  return 0;
}

#endif // defined(CRASHREPORTER_BIN)



#if !defined(CRASHREPORTER_BIN)

#define MAX_NUM_PLUGIN_NAMES 100
static QAtomicInt g_plugin_name_pos;

static const char *g_plugin_names[MAX_NUM_PLUGIN_NAMES]={g_no_plugin_name};
//static QString g_plugin_name=g_no_plugin_name;

static QTime running_time;


int CRASHREPORTER_set_plugin_name(const char *plugin_name){
  //fprintf(stderr,"plugin_name: -%s-\n",plugin_name);

  int pos = g_plugin_name_pos.fetchAndAddOrdered(1) % MAX_NUM_PLUGIN_NAMES;

  if (plugin_name[0]!=0)
    g_plugin_names[pos] = plugin_name;

  return pos;
}

void CRASHREPORTER_unset_plugin_name(int pos){
  g_plugin_names[pos] = g_no_plugin_name;
}

static QString get_plugin_names(void){
  QString ret;
  
  for(int i=0;i<MAX_NUM_PLUGIN_NAMES;i++){
    if (g_plugin_names[i] != g_no_plugin_name){
      if (ret!="")
        ret += ", "+QString(g_plugin_names[i]);
      else
        ret = g_plugin_names[i];
    }
  }

  if (ret=="")
    return NOPLUGINNAMES;
  else
    return ret;
}

static void run_program(QString program, QString arg1, QString arg2, QString arg3, QString arg4, bool wait_until_finished){

#if defined(FOR_WINDOWS)

  char *p = strdup(program.toAscii());
  char *a1 = strdup(arg1.toAscii());
  char *a2 = strdup(arg2.toAscii());
  char *a3 = strdup(arg3.toAscii());
  char *a4 = strdup(arg4.toAscii());

  if(_spawnl(wait_until_finished ? _P_WAIT :  _P_DETACH, p, p, a1, a2, a3, a4, NULL)==-1){
    fprintf(stderr,"Couldn't launch crashreporter: \"%s\" \"%s\"\n",p,a1);
    SYSTEM_show_message(strdup(talloc_format("Couldn't launch crashreporter: \"%s\" \"%s\"\n",p,a1)));
    Sleep(3000);
  }

#elif defined(FOR_LINUX) || defined(FOR_MACOSX)
      
  //if(system(QString(QCoreApplication::applicationDirPath() + "/crashreporter " + key + " " + QString::number(getpid()) + "&").toAscii())==-1) { // how to fix utf-8 here ?
  QString a = "LD_LIBRARY_PATH=" + QString(getenv("LD_LIBRARY_PATH"));
  QString full_command = a + " " + program + " " + arg1 + " " + arg2 + " " + arg3 + " " + arg4;

  if (wait_until_finished==false)
    full_command += "&";

  fprintf(stderr, "Executing -%s-\n",full_command.toUtf8().constData());
  
  if(system(strdup(full_command.toUtf8().constData()))==-1) {
    SYSTEM_show_message(strdup(talloc_format("Couldn't start crashreporter. command: -%s-\n",full_command.toUtf8().constData())));
  }

#else
  #error "unknown system"
#endif

}


static QTemporaryFile *g_crashreporter_file = NULL;

static bool file_is_empty(QTemporaryFile *file){
  if (file->pos()==0 && file->bytesAvailable()==0)
    return true;

  char data[16] = {0};
  if (file->peek(data, 1) <= 0)
    return true;

  if (data[0]==0)
    return true;

  return false;
}

static bool string_to_file(QString s, QTemporaryFile *file, bool save_mixer_tree){
  bool ret = false;
  
  if (!file->open()){
    SYSTEM_show_message("Unable to create temporary file. Disk may be full");
    return false;
  }

  const QByteArray data = s.toUtf8();
  int bytesWritten = file->write(data);
    
  if (bytesWritten==0){
    SYSTEM_show_message("Unable to write to temporary file. Disk may be full");
    goto exit;
  }

  ret = true;
  
  if (bytesWritten != data.size())
    SYSTEM_show_message("Unable to write everything to temporary file. Disk may be full");

  if (save_mixer_tree)
    SP_write_mixer_tree_to_disk(file);
  
 exit:
  file->close();
  
  return ret;
}


void CRASHREPORTER_send_message(const char *additional_information, const char **messages, int num_messages, bool is_crash){
  QString plugin_names = get_plugin_names();
  
  QString tosend = QString(additional_information) + "\n\n";

  tosend += VERSION "\n\n";

  tosend += "OpenGL vendor: " + QString((GE_vendor_string==NULL ? "(null)" : (const char*)GE_vendor_string )) + "\n";
  tosend += "OpenGL renderer: " + QString((GE_renderer_string==NULL ? "(null)" : (const char*)GE_renderer_string)) + "\n";
  tosend += "OpenGL version: " + QString((GE_version_string==NULL ? "(null)" : (const char*)GE_version_string)) + "\n";
  tosend += QString("OpenGL flags: %1").arg(GE_opengl_version_flags, 0, 16) + "\n\n";

  tosend += "Running plugins: " + plugin_names + "\n\n";

  tosend += "Running time: " + QString::number(running_time.elapsed()) + "\n\n";

  tosend += "\n\n";

    
  for(int i=0;i<num_messages;i++)
    tosend += QString::number(i) + ": "+messages[i] + "\n";

  tosend += "\n\n";

  int event_pos = g_event_pos;

  tosend += "start event_pos: " + QString::number(event_pos) + "\n";

  for(int i=event_pos-1; i>=0 ; i--)
    tosend += QString(g_event_log[i]) + "\n";

  for(int i=NUM_EVENTS-1; i>=event_pos ; i--)
    tosend += QString(g_event_log[i]) + "\n";

  tosend += "end event_pos: " + QString::number(g_event_pos) + "\n";
  
  tosend += "\n\n";

#if defined(FOR_LINUX)
  tosend += "LINUX\n\n";
  tosend += "/etc/os-release: "+file_to_string("/etc/os-release");
  tosend += "\n\n";
  tosend += "/proc/version: "+file_to_string("/proc/version");
  tosend += "\n\n";
  tosend += "/proc/cpuinfo: "+file_to_string("/proc/cpuinfo");
#endif

  // start process
  {
    QString program = QCoreApplication::applicationDirPath() + QDir::separator() + "crashreporter";
#if FOR_WINDOWS
    program += ".exe";
#endif

    QTemporaryFile *file;
    
    if (is_crash) {
      file = new QTemporaryFile;
    } else {
      if (g_crashreporter_file==NULL) {
        g_crashreporter_file = new QTemporaryFile;
        g_crashreporter_file->setAutoRemove(false); // We delete it in the sub process. This process is just going to exit as soon as possible.
      }
      file = g_crashreporter_file;
    }

    bool save_mixer_tree;
    
    if (is_crash)
      save_mixer_tree = false; // Don't want to risk crashing inside the crash handler.
    else
      save_mixer_tree = true;
    
    string_to_file(tosend, file, save_mixer_tree);

    /*
      Whether to block
      ================
                                    RELEASE      !RELEASE
                                -------------------------
      Crash in main thread      |     no [1]       yes [2]
      Crash in other thread     |     no [1]       yes [2]
      Assert in main thread     |     no [4]       yes [2]
      Assert in other thread    |     no [4]       yes [2,3]

      [1] When crashing in RELEASE mode, it doesn't matter wheter we block or not, because
          radium will exit immediately after finishing this function anyway, and it's
          probably better to do that as quickly as possible.

      [2] Ideally, this should happen though:
          1. All threads immediately freezes
          2. A dialog pops up asking whether to:
             a) Stop program (causing gdb to kick in)
             b) Ignore
             c) Run assert crashreporter

      [3] This can be annoying if the assert happens in the audio thread though.

      [4] Asserts are not really supposed to happen, but there are a lot of them, 
          and they might pop up unnecessarily (for instance a bug in the asserts themselves):
          * Blocking might cause the program to be non-functional unnecessarily.
          * Blocking could prevent the user from saving the current song,
            for instance if the assert window just pops up immediately after closing it.
     */

#ifdef RELEASE
    bool do_block = false;
#else
    bool do_block = true;
#endif

    QTemporaryFile emergency_save_file("radium_crash_save");

#if 0
    bool dosave = is_crash && Undo_num_undos_since_last_save()>0;
#else
    bool dosave = false; // saving inside a forked version of the program didn't really work that well. Maybe it works better in windows.
#endif
    
    if (dosave)
      emergency_save_file.open();
    
    run_program(program,
                toBase64(file->fileName()),
                toBase64(plugin_names),
                toBase64(dosave ? emergency_save_file.fileName() : NOEMERGENCYSAVE),
                (is_crash ? "is_crash" : "is_assert"),
                do_block
                );

    if (dosave)
      Save_Clean(STRING_create(emergency_save_file.fileName()),root,false);
  }
  
}

#ifdef FOR_MACOSX
#include "../common/visual_proc.h"
void CRASHREPORTER_send_message_with_backtrace(const char *additional_information, bool is_crash){
  GFX_Message(NULL, additional_information);
}
#endif

void CRASHREPORTER_send_assert_message(const char *fmt,...){
  static bool is_currently_sending = false;
  
#if 0
  static int last_time = -10000;
  
  if ( last_time < (running_time.elapsed()-(30*1000)))
    return;

  last_time = running_time.elapsed();
#endif

  if (is_currently_sending)
    return;

  char message[1000];
  va_list argp;
  
  va_start(argp,fmt);
  /*	vfprintf(stderr,fmt,argp); */
  vsprintf(message,fmt,argp);
  va_end(argp);
  
  if (g_crashreporter_file!=NULL) {

    if (!g_crashreporter_file->open()){
      SYSTEM_show_message("Unable to create temprary file. Disk may be full");
      send_crash_message_to_server(message, get_plugin_names(), NOEMERGENCYSAVE, false);
      goto exit;
    }

    if (false==file_is_empty(g_crashreporter_file)) {
      g_crashreporter_file->close();
      goto exit;
    }

    g_crashreporter_file->close();
  }

  is_currently_sending = true;
  RT_request_to_stop_playing();
  RT_pause_plugins();


  CRASHREPORTER_send_message_with_backtrace(message, false);

#if 0
  if (may_do_blocking && THREADING_is_main_thread())
    send_crash_message_to_server(message, g_plugin_name, false);
  else{
    const char *messages[1] = {message};
    CRASHREPORTER_send_message(messages, 1, false);
  }
#endif
  
 exit:
  is_currently_sending = false;
}


void CRASHREPORTER_close(void){
#if defined(FOR_WINDOWS)
  CRASHREPORTER_windows_close();
#endif

  if (g_crashreporter_file==NULL)
    delete g_crashreporter_file;
}



void CRASHREPORTER_init(void){
  running_time.start();
  
#if defined(FOR_WINDOWS)
  CRASHREPORTER_windows_init();

#elif defined(FOR_LINUX)
      
  CRASHREPORTER_posix_init();

#elif defined(FOR_MACOSX)

#else
# error "Unknown machine"

#endif

  mysleep(1000);
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

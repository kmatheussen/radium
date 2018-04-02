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

#include <sys/time.h>

#if defined(FOR_LINUX)
#  include <sys/types.h>
#endif

#include <QWidget>
#include <QString>
#include <QApplication>
#include <QFile>
#include <QTextStream>
#include <QPlainTextEdit>
#include <QLabel>
#include <QLayout>
#include <QThread>

#if USE_QT5
#include <QUrlQuery>
#endif

#include <QTemporaryFile>
#include <QDir>

#include <QProcess>

#include <QNetworkRequest>
#include <QNetworkAccessManager>
#include <QNetworkReply>

#include "../common/nsmtracker.h"
#include "../common/OS_settings_proc.h"
#include "../common/OS_Player_proc.h"
#include "../common/OS_string_proc.h"
#include "../common/threading.h"
#include "../common/disk_save_proc.h"
#include "../common/undo.h"
#include "../embedded_scheme/scheme_proc.h"
#include "../OpenGL/Widget_proc.h"

#include "../Qt/helpers.h"

#include "../audio/SoundProducer_proc.h"
//extern void SP_write_mixer_tree_to_disk(QFile *file);

#include "crashreporter_proc.h"

#if defined(FOR_WINDOWS)
#  include <windows.h>
#  define mysleep(ms) Sleep(ms)
#else
#  define mysleep(ms) QThread::msleep(ms)
#endif

double g_rt_set_bus_descendant_types_duration;

#if !defined(CRASHREPORTER_BIN)
static const char *g_no_plugin_name = "<noplugin>";
static DEFINE_ATOMIC(bool, g_dont_report) = false;
#endif

#define NOPLUGINNAMES "<nopluginnames>"
#define NOEMERGENCYSAVE "<noemergencysave>"


namespace local{
static QString toBase64(QString s){
  return s.toLocal8Bit().toBase64();
}

static QString fromBase64(QString encoded){
  return QString::fromLocal8Bit(QByteArray::fromBase64(encoded.toLocal8Bit()).data());
}
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
  return "(unable to open file -"+filename+"-)";
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
static double g_time_log[NUM_EVENTS] = {0.0};
static DEFINE_ATOMIC(int, g_event_pos) = 0;


void EVENTLOG_add_event(const char *log_entry){
 R_ASSERT(THREADING_is_main_thread());

 int pos = ATOMIC_ADD_RETURN_OLD(g_event_pos, 1) % NUM_EVENTS;
 
 g_event_log[pos] = log_entry;
 g_time_log[pos] = TIME_get_ms();
}

QString get_event_string(int pos, double time_now){
  return QString(QString::number((time_now - g_time_log[pos])/1000.0, 'f', 6) + ": " + g_event_log[pos]) + "\n";
}

QString get_event_log(int event_pos, double time_now){
  QString ret;
  
  for(int i=event_pos-1; i>=0 ; i--)
    ret += get_event_string(i, time_now);

  for(int i=NUM_EVENTS-1; i>=event_pos ; i--)
    ret += get_event_string(i, time_now);

  return ret;
}

const char *EVENTLOG_get(void){
  int event_pos = ATOMIC_GET(g_event_pos)  % NUM_EVENTS;
  return talloc_strdup(get_event_log(event_pos, TIME_get_ms()).toUtf8().constData());
}


#endif


#if defined(FOR_MACOSX)
#include "get_osx_diagnostic_reports.cpp"
#endif

#include <QHBoxLayout>
#include <QPushButton>
#include <QCheckBox>
#include <QButtonGroup>
#include <QGroupBox>
#include <QStyle>


static void send_crash_message_to_server(QString message, QString plugin_names, QString emergency_save_filename, Crash_Type crash_type){

  bool is_crash = crash_type==CT_CRASH;

#if FULL_VERSION==0
  message = "DEMO VERSION " + message;
#else
  message = "FULL VERSION " + message; 
#endif

#if defined(FOR_LINUX)
#if defined(IS_LINUX_BINARY)
  message += "\nLinux binary\n";
#else
  message += "\nCustom build\n";
#endif
#endif
    
#if defined(FOR_MACOSX)
  if (is_crash)
    message = message + "\n\n" + get_latest_diagnostic_report();
#endif

  fprintf(stderr,"Got message:\n%s\n",message.toUtf8().constData());

  {
    QDialog box;
    //box.stackUnder(box.parentWidget());
    box.setWindowFlags(Qt::Window | DEFAULT_WINDOW_FLAGS);
    box.setWindowModality(Qt::ApplicationModal);
    
    QVBoxLayout layout;

    //box.setIcon(QMessageBox::Critical);

    QLabel iconlabel;
    QIcon icon = box.style()->standardIcon(QStyle::SP_MessageBoxWarning);
    iconlabel.setPixmap(icon.pixmap(icon.availableSizes()[0]));
    layout.addWidget(&iconlabel);
      
    QLabel space0(" ");
    layout.addWidget(&space0);

    QLabel text1;
    
    if (crash_type==CT_CRASH)
      text1.setText("Radium Crashed. :((");
    else if (crash_type==CT_ERROR)
      text1.setText("Error! Radium is in a state it should not be in.\n(Note that Radium has NOT crashed)\n");
    else
      text1.setText("Warning! Radium is in a state it should not be in.\n(Note that Radium has NOT crashed, you can continue working)\n");

    layout.addWidget(&text1);
    

    QLabel text2;
    text2.setSizePolicy(QSizePolicy::Minimum, QSizePolicy::Minimum);
    
    bool dosave = emergency_save_filename!=QString(NOEMERGENCYSAVE);
    
    text2.setText(QString(
                                   #if FOR_LINUX
                                   "Linux users: Please don't report bugs caused by a non-properly compiled, or old, version of Radium (usually distributed by Arch linux). "
                                   "If you have compiled Radium yourself, or you are using a version of Radium "
                                   "distributed by a third party, please try the official binaries first. "
                                   "At least execute a \"make very_clean\" command to see if the bug disappears. Thank you!\n"
                                   "\n"
                                   #endif
                                   "This %0 will be automatically reported when you press \"SEND\".\n"
                                   "\n"
                                   "The report is sent anonymously, and will only be seen by the author of Radium.\n"
                                   "\n"
                                   "Only the information in \"Show details...\" is sent. You can also edit this text if there is anything you don't want to send.\n"
                                   "\n"
                                   "Please don't report the same %0 more than two or three times for the same version of Radium.\n"
                                   ).arg(crash_type==CT_CRASH ? "crash" : crash_type==CT_ERROR ? "error" : "warning")
                           + ( (is_crash && plugin_names != NOPLUGINNAMES)
                               ? QString("\nPlease note that the following third party plugins: \"" + plugin_names + "\" was/were currently processing audio. It/they might be responsible for the crash.\n")
                               : QString())
                           + (crash_type==CT_ERROR ? "\nAfterwards, you should save your work and start the program again.\n\nIf this window just pops up again immediately after closing it, just hide it instead." : "")
                           + (dosave ? "\nAn emergency version of your song has been saved as\n\""+emergency_save_filename+"\".\nHowever, this file should not be trusted. It could be malformed.\n(The file is most likely okay though)" : "")
                           + "\n"
                           );
    
    text2.setWordWrap(true);
    //text2.setMaximumWidth(600);
        
    //box.setDetailedText(message);
    layout.addWidget(&text2);
    text2.adjustSize();
    text2.updateGeometry();
    
    /*
    QButtonGroup buttons;
    buttons.addButton(new QPushButton("Show Details"), 0);
    buttons.addButton(new QPushButton("SEND"), 1);
    buttons.addButton(new QPushButton("DON'T SEND"), 2);

    layout.addWidget(&buttons);
    */

    QPlainTextEdit details(message);
    layout.addWidget(&details);
    details.hide();
    
    QHBoxLayout button_layout;
    auto *b1 = new QCheckBox("Show details");
    auto *b2 = new QPushButton("SEND");
    auto *b3 = new QPushButton("DON'T SEND");
    
    button_layout.addWidget(b1);
    button_layout.addWidget(b2);
    button_layout.addWidget(b3);

    layout.addLayout(&button_layout);

    b1->setCheckable(true);
    b1->connect(b1, SIGNAL(toggled(bool)), &details, SLOT(setVisible(bool)));
    
    QButtonGroup buttons;
    //buttons.addButton(b1, 1);
    buttons.addButton(b2, 2);
    buttons.addButton(b3, 3);
    
    box.connect(&buttons, SIGNAL(buttonClicked(int)), &box, SLOT(done(int)));

    

    QLabel space(" ");
    layout.addWidget(&space);

    QLabel text_edit_label("<br><br>"
                           "Please also include additional information below.<br>"
                           "<br>"
                           "The best type of help you can give is to write down a step by step recipe in the following format:<br>"
                           "<br>"
                           "1. Start Radium<br>"
                           "2. Move the cursor to track 3.<br>"
                           "3. Press the Q button.<br>"
                           "4. Radium crashes<br>"
                           "<br>"
                           "<b>Note: Sometimes it is virtually impossible to fix the bug without a recipe on how to reproduce the bug</b><br>"
                           "<br>"
                           "Also remember that the report is sent anonymously. If you want feedback or want to help further to track down the<br>"
                           "bug, please include an email address below."
                           );
    text_edit_label.setSizePolicy(QSizePolicy::Minimum, QSizePolicy::Minimum);
 
    //text_edit.setMinimumWidth(1000000);
    //text_edit.setSizePolicy(QSizePolicy(QSizePolicy::Minimum,QSizePolicy::Minimum));
    layout.addWidget(&text_edit_label);

    QLabel space2(" ");
    layout.addWidget(&space2);

    QTextEdit text_edit;
    text_edit.setText("<Please add recipe and/or email address here>\n");
    //text_edit.setMinimumWidth(1000000);
    //text_edit.setSizePolicy(QSizePolicy(QSizePolicy::Minimum,QSizePolicy::Minimum));
    layout.addWidget(&text_edit);

    if (crash_type==CT_CRASH)
      box.setWindowTitle("Report crash");
    else if (crash_type==CT_ERROR)
      box.setWindowTitle("Report error");
    else
      box.setWindowTitle("Report warning");

    
    //layout.setSizeConstraint(QLayout::SetFixedSize);
    box.setLayout(&layout);


    const QFontMetrics fn = QFontMetrics(QApplication::font()); //editor->font);
    int width = 2.3 * fn.width("Note: Sometimes it is virtually impossible to fix the bug");

    //box.setMaximumWidth(600);
    box.adjustSize();
    box.updateGeometry();
    box.resize(width, box.height());

    box.setVisible(true);
    box.show();
    box.activateWindow();
    box.raise();

#ifdef FOR_WINDOWS
    HWND wnd=(HWND)box.winId();
    SetFocus(wnd);
    SetWindowPos(wnd, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE|SWP_NOSIZE);
#endif

    int ret = box.exec();
    //int ret = 2;

    if(ret==2){ //QMessageBox::AcceptRole){

      QByteArray data;
      QUrl params;

#if USE_QT5
      QUrlQuery query;
      query.addQueryItem("data", details.toPlainText().replace("&", "_amp_")); // replace all '&' with _amp_ since we don't receive anything after '&'.
      params.setQuery(query);
#else
      params.addQueryItem("data", details.toPlainText().replace("&", "_amp_")); // replace all '&' with _amp_ since we don't receive anything after '&'.
#endif
      
      const char *s = strdup(params.toString().toUtf8().constData());
      data.append(s, (int)strlen(s)-1);
      //free(s);
      data.remove(0,1);
      data.append("\n");
      data.append(text_edit.toPlainText());

      QNetworkAccessManager nam;
      QNetworkRequest request(QUrl("http://users.notam02.no/~kjetism/radium/crashreport.php"));
      request.setHeader(QNetworkRequest::ContentTypeHeader, "application/x-www-form-urlencoded" );

      QNetworkReply *reply = nam.post(request,data);

      while(reply->isFinished()==false) {
        QCoreApplication::processEvents();
        QThread::msleep(50);
      }
      
#if 1
      {
        ScopedQPointer<MyQMessageBox> box(MyQMessageBox::create(true));
        box->setText("Thanks for reporting the bug!");
        
        box->setInformativeText("The bug will hopefully be fixed in the next version of Radium.");
        box->exec();
      }
#endif

    }
  }

}

#if defined(CRASHREPORTER_BIN)




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
void register_modal_qwidget(QWidget *widget){
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
    msgBox.setText("Error!\nRadium has not been installed properly.\nRadium is likely to be unstable because of this.\n(See bin/packages/README for instructions on how to properly install Radium)\n");
    msgBox.setStandardButtons(QMessageBox::Ok);
    msgBox.exec();
  }
#endif
  
  QString filename = local::fromBase64(argv[1]);

  QString running_plugin_names = local::fromBase64(argv[2]);
  
  QString emergency_save_filename = local::fromBase64(argv[3]);
    
  Crash_Type crash_type = QString(argv[4])=="is_crash" ? CT_CRASH : QString(argv[4])=="is_error" ? CT_ERROR : CT_WARNING;

  send_crash_message_to_server("-"+QString(argv[4])+"-"+file_to_string(filename), running_plugin_names, emergency_save_filename, crash_type);

  if (crash_type==CT_CRASH)
    delete_file(filename);
  else
    clear_file(filename);

  
  return 0;
}

#endif // defined(CRASHREPORTER_BIN)



#if !defined(CRASHREPORTER_BIN)

#define MAX_NUM_PLUGIN_NAMES 100
static DEFINE_ATOMIC(int, g_plugin_name_pos) = 0;

double g_last_midi_receive_time = 0;

static DEFINE_ATOMIC(const char *, g_plugin_names)[MAX_NUM_PLUGIN_NAMES]={g_no_plugin_name};
//static QString g_plugin_name=g_no_plugin_name;

int CRASHREPORTER_set_plugin_name(const char *plugin_name){
  //fprintf(stderr,"plugin_name: -%s-\n",plugin_name);

  int pos = ATOMIC_ADD_RETURN_OLD(g_plugin_name_pos, 1) % MAX_NUM_PLUGIN_NAMES;

  ATOMIC_SET_ARRAY(g_plugin_names, pos, plugin_name);

  return pos;
}

void CRASHREPORTER_unset_plugin_name(int pos){
  ATOMIC_SET_ARRAY(g_plugin_names, pos, g_no_plugin_name);
}

static QString get_plugin_names(void){
  QString ret;
  
  for(int i=0;i<MAX_NUM_PLUGIN_NAMES;i++){
    const char *plugin_name = ATOMIC_GET_ARRAY(g_plugin_names, i);
    if (plugin_name != g_no_plugin_name){
      if (ret!="")
        ret += ", "+QString(plugin_name);
      else
        ret = plugin_name;
    }
  }

  if (ret=="")
    return NOPLUGINNAMES;
  else
    return ret;
}

static void run_program(QString program, QString arg1, QString arg2, QString arg3, QString arg4, bool wait_until_finished){

  // NOTE: We might not be in the main thread here.

  
  if (ATOMIC_GET(g_dont_report))
    return;

#if defined(FOR_WINDOWS)

  wchar_t *p = STRING_create(program, false);
  wchar_t *a1 = STRING_create(arg1, false);
  wchar_t *a2 = STRING_create(arg2, false);
  wchar_t *a3 = STRING_create(arg3, false);
  wchar_t *a4 = STRING_create(arg4, false);

  if(_wspawnl(wait_until_finished ? _P_WAIT :  _P_DETACH, p, p, a1, a2, a3, a4, NULL)==-1){
    char *temp = (char*)malloc(program.size()+arg1.size()+1024);
    sprintf(temp, "Couldn't launch crashreporter: \"%s\" \"%s\"\n",program.toUtf8().constData(), arg1.toUtf8().constData());
    fprintf(stderr,temp);
    SYSTEM_show_error_message(strdup(temp));
    Sleep(3000);
  }

#elif defined(FOR_LINUX) || defined(FOR_MACOSX)
      
  //if(system(QString(QCoreApplication::applicationDirPath() + "/crashreporter " + key + " " + QString::number(getpid()) + "&").toAscii())==-1) { // how to fix utf-8 here ?
  QString a = "LD_LIBRARY_PATH=" + QString(getenv("LD_LIBRARY_PATH"));
  QString full_command = a + " " + program + " " + arg1 + " " + arg2 + " " + arg3 + " " + arg4;

  if (wait_until_finished==false)
    full_command += "&";

  fprintf(stderr, "Executing -%s-\n",full_command.toUtf8().constData());
  const char *command = strdup(full_command.toUtf8().constData());
  if(system(command)==-1) {
    char *temp = (char*)malloc(strlen(command)+10);
    sprintf(temp, "Couldn't start crashreporter. command: -%s-\n",command);
    SYSTEM_show_error_message(strdup(temp));
  }

#else
  #error "unknown system"
#endif

#if !defined(RELEASE) && defined(FOR_LINUX)
  abort();
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
    SYSTEM_show_error_message("Unable to create temporary file. Disk may be full");
    return false;
  }

  const QByteArray data = s.toUtf8();
  int64_t bytesWritten = file->write(data);
    
  if (bytesWritten==0){
    SYSTEM_show_error_message("Unable to write to temporary file. Disk may be full");
    goto exit;
  }

  ret = true;
  
  if (bytesWritten != data.size())
    SYSTEM_show_error_message("Unable to write everything to temporary file. Disk may be full");

  if (save_mixer_tree)
    SP_write_mixer_tree_to_disk(file);
  
 exit:
  file->close();
  
  return ret;
}

/*
void CRASHREPORTER_send_message(const char *additional_information, const char **messages, int num_messages, enum Crash_Type crash_type){
  printf("CR_send_message_called with %d lines of info about -%s-\n",num_messages,additional_information);
  int i;
  for(i=0;i<num_messages;i++)
    printf("%d: %s\n",i,messages[i]);
}
*/

void CRASHREPORTER_send_message(const char *additional_information, const char **messages, int num_messages, Crash_Type crash_type, double time){
  if (ATOMIC_GET(g_dont_report))
    return;

  QString plugin_names = get_plugin_names();
  
  QString tosend = QString(additional_information) + "\n\n";
  
  tosend += VERSION "\n\n";

  tosend += "OS version: " + QSysInfo::productVersion() + "\n\n";
    
  tosend += "OpenGL vendor: " + QString((ATOMIC_GET(GE_vendor_string)==NULL ? "(null)" : (const char*)ATOMIC_GET(GE_vendor_string) )) + "\n";
  tosend += "OpenGL renderer: " + QString((ATOMIC_GET(GE_renderer_string)==NULL ? "(null)" : (const char*)ATOMIC_GET(GE_renderer_string))) + "\n";
  tosend += "OpenGL version: " + QString((ATOMIC_GET(GE_version_string)==NULL ? "(null)" : (const char*)ATOMIC_GET(GE_version_string))) + "\n";
  tosend += QString("OpenGL flags: %1").arg(ATOMIC_GET(GE_opengl_version_flags), 0, 16) + "\n\n";

  tosend += "Runtime Qt version: " + QString(qVersion()) + " on " + QSysInfo::currentCpuArchitecture().toUtf8().constData() + "\n";
  tosend += "Compile-time Qt version:" + QString(QT_VERSION_STR) +  " for " + QSysInfo::buildAbi().toUtf8().constData() + ".\n\n";
  
  tosend += "Running plugins: " + plugin_names + "\n\n";
  tosend += "Running time: " + QString::number(time/1000.0) + " seconds.\n\n";
  tosend += "Last painter: " + QString(g_qt_is_painting_where) + "\n\n";
  tosend += "Last g_rt_set_bus_descendant_types_duration: " + QString::number(g_rt_set_bus_descendant_types_duration) + "ms\n\n";
  tosend += "Time since last received MIDI message: " + QString::number(time - g_last_midi_receive_time) + "ms\n\n";
  tosend += "\n\n";

  for(int i=0;i<num_messages;i++)
    tosend += QString::number(i) + ": "+messages[i] + "\n";

  tosend += "\n\n";

  int event_pos = ATOMIC_GET(g_event_pos)  % NUM_EVENTS;

  tosend += "start event_pos: " + QString::number(event_pos) + "\n";

  tosend += get_event_log(event_pos, time);

  tosend += "end event_pos: " + QString::number(ATOMIC_GET(g_event_pos) % NUM_EVENTS) + "\n";
  
  tosend += "\n\n";

  if (crash_type!=CT_CRASH && THREADING_is_main_thread()){
    tosend += QString(SCHEME_get_history()) + "\n\n";
  }
  

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
#ifdef FOR_WINDOWS
    QString program = OS_get_full_program_file_path("radium_crashreporter.exe");
#else
    QString program = OS_get_full_program_file_path("radium_crashreporter");
#endif

    QTemporaryFile *file;
    
    if (crash_type==CT_CRASH) {
      file = new QTemporaryFile;
    } else {
      if (g_crashreporter_file==NULL) {
        g_crashreporter_file = new QTemporaryFile;
        g_crashreporter_file->setAutoRemove(false); // We delete it in the sub process. This process is just going to exit as soon as possible.
      }
      file = g_crashreporter_file;
    }

    bool save_mixer_tree;
    
    if (crash_type==CT_CRASH)
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
    bool do_block = false; //true;
#endif

    QTemporaryFile emergency_save_file("radium_crash_save");

#if 0
    bool dosave = is_crash && Undo_num_undos_since_last_save()>0;
#else
    bool dosave = false; // saving inside a forked version of the program didn't really work that well. Maybe it works better in windows.
#endif

    if (dosave)
      emergency_save_file.open();

    //printf("tosend: -%s-. Filename: %s\n",tosend.toUtf8().constData(),file->fileName().toUtf8().constData());
    //getchar();
    
    run_program(program,
                local::toBase64(file->fileName()),
                local::toBase64(plugin_names),
                local::toBase64(dosave ? emergency_save_file.fileName() : NOEMERGENCYSAVE),
                crash_type==CT_CRASH ? "is_crash" : crash_type==CT_ERROR ? "is_error" : "is_warning",
                do_block
                );

    if (dosave)
      Save_Clean(STRING_create(emergency_save_file.fileName()),root,false);
  }

}

#if 0
#ifdef FOR_MACOSX
#include "../common/visual_proc.h"
void CRASHREPORTER_send_message_with_backtrace(const char *additional_information, Crash_Type crash_type){
  GFX_Message(NULL, additional_information);
}
#endif
#endif

void CRASHREPORTER_send_assert_message(Crash_Type crash_type, const char *fmt,...){
  static DEFINE_ATOMIC(bool, is_currently_sending);

  if (ATOMIC_SET_RETURN_OLD(is_currently_sending, true)==true) {
    // already sending
    return;
  }

  if (ATOMIC_GET(g_dont_report))
    return;

#if 0
  static int last_time = -10000;
  
  if ( last_time < (running_time.elapsed()-(30*1000)))
    return;

  last_time = running_time.elapsed();
#endif

  double time = TIME_get_ms();

  char message[1000];
  va_list argp;

#if !defined(__clang__)
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wsuggest-attribute=format"
#endif
  
  va_start(argp,fmt);
  /*	vfprintf(stderr,fmt,argp); */
  vsnprintf(message,998,fmt,argp);
  va_end(argp);
  
#if !defined(__clang__)
#  pragma GCC diagnostic pop
#endif

  if (g_crashreporter_file!=NULL) {

    if (!g_crashreporter_file->open()){
      SYSTEM_show_error_message("Unable to create temprary file. Disk may be full");
      send_crash_message_to_server(message, get_plugin_names(), NOEMERGENCYSAVE, crash_type);
      goto exit;
    }

    if (false==file_is_empty(g_crashreporter_file)) {
      g_crashreporter_file->close();
      goto exit;
    }

    g_crashreporter_file->close();
  }

  RT_request_to_stop_playing();
  RT_pause_plugins();


  CRASHREPORTER_send_message_with_backtrace(message, crash_type, time);

#if 0
  if (may_do_blocking && THREADING_is_main_thread())
    send_crash_message_to_server(message, g_plugin_name, false);
  else{
    const char *messages[1] = {message};
    CRASHREPORTER_send_message(messages, 1, false);
  }
#endif
  
 exit:
  ATOMIC_SET(is_currently_sending, false);
}

// We don't want the crashreporter to pop up when program exits, or we scan plugins.
void CRASHREPORTER_do_report(void){
  ATOMIC_SET(g_dont_report, false);
}

void CRASHREPORTER_dont_report(void){
  ATOMIC_SET(g_dont_report, true);
}

void CRASHREPORTER_close(void){
#if defined(FOR_WINDOWS)
  CRASHREPORTER_windows_close();
#endif

  if (g_crashreporter_file==NULL)
    delete g_crashreporter_file;
}



void CRASHREPORTER_init(void){
  
#if defined(FOR_WINDOWS)
  CRASHREPORTER_windows_init();

#elif defined(FOR_LINUX) || defined(FOR_MACOSX)
      
  CRASHREPORTER_posix_init();

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

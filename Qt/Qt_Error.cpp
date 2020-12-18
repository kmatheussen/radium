#include <stdio.h>

#include <QWidget>

#include <qapplication.h>
#include <QPushButton>
#include <QProcess>
#include <QDir>
#include <qfontdatabase.h>
#include <QMouseEvent>

#include "../common/nsmtracker.h"


#include "helpers.h"

#ifdef TEST_MAIN
static inline QString STRING_get_qstring(const wchar_t *string){ // TODO: Rename to STRING_create_qstring.
  if (string==NULL)
    return QString("");
  else
    return QString::fromWCharArray(string);
}
#endif

#ifndef TEST_MAIN

#define LANGSPEC "C"  // LANGSPEC doesn't work in OS_error_proc.h, so can't include nsmtracker.h. Haven't found out why this happens.

#if !defined(COMPILE_EXECUTABLE)
  #include "../common/OS_settings_proc.h"
#endif

#endif


/*
  g++ Qt_Error.cpp `pkg-config --cflags --libs Qt5Gui Qt5Widgets` -DCOMPILE_EXECUTABLE -o ../bin/radium_error_message `cat ../buildtype.opt` `cat ../flagopts.opt` -DFOR_LINUX -I. -fPIC -std=gnu++11  -Wno-class-memaccess && ../bin/radium_error_message gakkgakk1
  g++ Qt_Error.cpp `pkg-config --cflags --libs Qt5Gui Qt5Widgets` -DTEST_MAIN -o test_radium_error_message `cat ../buildtype.opt` `cat ../flagopts.opt` -DFOR_LINUX -I. -fPIC -std=gnu++11  -Wno-class-memaccess &&  ./test_radium_error_message gakkgakk2

`cat ../buildtype.opt` 

both:
  g++ Qt_Error.cpp `pkg-config --cflags --libs Qt5Gui Qt5Widgets` -DCOMPILE_EXECUTABLE -o ../bin/radium_error_message `cat ../flagopts.opt` -DFOR_LINUX -I. -fPIC -std=gnu++11  -Wno-class-memaccess && g++ Qt_Error.cpp `pkg-config --cflags --libs Qt5Gui Qt5Widgets` -DTEST_MAIN -o test_radium_error_message `cat ../buildtype.opt` `cat ../flagopts.opt` -DFOR_LINUX -I. -fPIC -std=gnu++11  -Wno-class-memaccess &&  ./test_radium_error_message gakkgakk2

 */


#ifdef COMPILE_EXECUTABLE

extern "C"{
  void CRASHREPORTER_send_assert_message(Crash_Type tye, const char *message, ...){
    abort();
  }
}


static int show_message(QString message, const QVector<QString> &menu_strings){

  //QPointer<MyQMessageBox> msgBox = MyQMessageBox::create();
  ScopedQPointer<MyQMessageBox> msgBox(MyQMessageBox::create(true));
  
  msgBox->setText(message);

  QVector<QPushButton*> buttons;
  for(QString menu_string : menu_strings){
    QPushButton *button = msgBox->addButton(menu_string, QMessageBox::AcceptRole);
    buttons.push_back(button);
  }

  msgBox->show();
  adjustSizeAndMoveWindowToCentre(msgBox);
  msgBox->raise();
  msgBox->activateWindow();
  
  printf("hepp: %d. msgBox: %p\n",msgBox->exec(), msgBox.data()); // safeExec(&msgBox)); <-- We are not inside the radium executable here.

  if (msgBox.data()==NULL){
    return -1;
  }

  QAbstractButton *clicked_button = msgBox->clickedButton();

  int i = 0;
  for(QString menu_string : menu_strings){
    if(clicked_button->text()==menu_string){
      /*
      {
        QMessageBox msgBox;
        msgBox.setIcon(QMessageBox::Critical);
        msgBox.setText("AI: " + QString::number(i));
        msgBox.setStandardButtons(QMessageBox::Ok);
        msgBox.exec();
      }
      */
      return i;
    }
    i++;
  }

  /*
  {
    QMessageBox msgBox;
    msgBox.setIcon(QMessageBox::Critical);
    msgBox.setText("AI2: " + QString::number(0));
    msgBox.setStandardButtons(QMessageBox::Ok);
    msgBox.exec();
  }
  */
  
  fprintf(stderr,"what?\n");
  return -1;
}

QWidget *g_main_window = NULL; // referenced by helpers.h
//QSplashScreen *g_splashscreen = NULL; // referenced by helpers.h
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
  /*
  {
    char command[1000];
    snprintf(command,998,"echo %d >/tmp/res.txt", 2);
    system(command);
  }
  */
  
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

  QLocale::setDefault(QLocale::c());
  QLocale::setDefault(QLocale::C);

  QString message = QByteArray::fromBase64(argv[1]);

  QVector<QString> menu_strings;
  for(int i=2 ; i<argc;i++)
    menu_strings.push_back(QByteArray::fromBase64(argv[i]));

  argv = getQApplicationConstructorArgs(argc, argv);
  QApplication app(argc, argv);

  /*
  {
    QFontDatabase::addApplicationFont(QCoreApplication::applicationDirPath() + QDir::separator() + "fonts" + QDir::separator() + "Lato-Black.ttf");
    QFont font;
    font.fromString("Lato,8,-1,5,87,0,0,0,0,0");
    QApplication::setFont(font);
  }
  */

#if FOR_LINUX
  if(faulty_installation){
    QMessageBox msgBox;
    msgBox.setIcon(QMessageBox::Critical);
    msgBox.setText("Error!\nRadium has not been installed properly.\nRadium is likely to be unstable because of this.\n(See bin/packages/README for instructions on how to properly install Radium)");
    msgBox.setStandardButtons(QMessageBox::Ok);
    msgBox.exec();
  }
#endif

  int ret = show_message(message, menu_strings);
  printf("retgakk: %d\n",ret);
  /*
  {
    char command[1000];
    snprintf(command,998,"echo %d >/tmp/res.txt", ret);
    system(command);
  }
  */
  
  return ret;
}



#else // COMPILE_EXECUTABLE

#include "../OpenGL/Widget_proc.h"

extern bool g_qt_is_running;

extern "C" {

int SYSTEM_show_message_menu(const struct vector_t_ *options, const char *message){

  if (g_qt_is_running==false){
    fprintf(stderr,"\n\n\n   === %s ===\n\n\n", message);
    return 0;
  }

  //if (THREADING_is_main_thread()) // Don't work with scoped variables.
  //radium::ScopedExec scopedExec(false, false); // We could run from another thread here.
    

#if FOR_WINDOWS
  QString program = STRING_get_qstring(OS_get_full_program_file_path(L"radium_error_message.exe").id);
#else
  QString program = STRING_get_qstring(OS_get_full_program_file_path(L"radium_error_message").id);
#endif

#if defined(FOR_WINDOWS)
  program = QString("\"") + program + "\""; // necessary if path contains spaces.
#endif

  QStringList arguments;
  arguments << QString(QString(message).toUtf8().toBase64().constData());

  if(options==NULL) {

    arguments << QString(QString("OK").toUtf8().toBase64().constData());

  }else {

    VECTOR_FOR_EACH(const char*, menu_string, options){
      arguments << QString(QString(menu_string).toUtf8().toBase64().constData());
    }END_VECTOR_FOR_EACH;

  }

  if (THREADING_is_main_thread())
    closePopup();
  
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
    return -1;
  }

  GL_unlock();

  int res;
  
  GFX_HideProgress();{
    res = myProcess->waitForFinished(20000);
  }GFX_ShowProgress();
  
  if (res==false){ // Have timeout value in case the GUI doesn't show up or is hidden somehow.
    printf("WARN: radium_error_message timed out. Returning -1\n");
    return -1;
  }
  
  R_ASSERT_RETURN_IF_FALSE2(myProcess->exitStatus()==QProcess::NormalExit, -1);
  
  printf("  Exit code: %d\n", myProcess->exitCode());
  return myProcess->exitCode();
}

// Note: Can be called from any thread. No gc-alloc.
int SYSTEM_show_error_message(const char *message){
  static DEFINE_ATOMIC(bool, ignore_forever) = false;
  static DEFINE_ATOMIC(double, ignore_until) = -1;
  static DEFINE_ATOMIC(double, last_time) = -1;
  
  double time_now = TIME_get_ms();

  if (time_now <= ATOMIC_DOUBLE_GET(ignore_until) || ATOMIC_GET(ignore_forever))
    return 0;

  ATOMIC_DOUBLE_SET(last_time, TIME_get_ms());

  const char *choices[] = {
    "continue",
    "quit",
    "ignore warnings and errors for two seconds",
    "ignore warnings and errors for the rest of the program"
  };
  
  const vector_t v = create_static_vector_t(4, (void**)choices);
  
  //int continue_ = 0;
  int quit = 1;
  int ignore1 = 2;
  int ignore2 = 3;

  int ret = SYSTEM_show_message_menu(&v, message);

  if (ret==quit){
    exit(-1);
    abort();
  }
    
  if (ret==ignore1)
    ATOMIC_DOUBLE_SET(ignore_until, ATOMIC_DOUBLE_GET(last_time) + 2000);
  else if (ret==ignore2)
    ATOMIC_SET(ignore_forever, true);
  
  return ret;
}
}



#ifdef TEST_MAIN

double TIME_get_ms(void){
  return 0;
}

bool g_qt_is_running = true;
extern "C"{
filepath_t OS_get_full_program_file_path(filepath_t filename){
  QString ret = QString("../bin/") + QString::fromWCharArray(filename.id);
  wchar_t * ch = (wchar_t*)calloc(sizeof(wchar_t), ret.size()+1);
  ret.toWCharArray(ch);
  return make_filepath(ch);
}
  bool THREADING_is_player_or_runner_thread(void){
    return false;
  }
  bool PLAYER_current_thread_has_lock(void){
    return false;
  }
  bool THREADING_is_main_thread(void){
    return true;
  }
}

void MOUSE_CYCLE_schedule_unregister_all(void){
}

QPointer<QMenu> g_curr_popup_qmenu;

void GL_lock(void){
}
void GL_unlock(void){
}

void GFX_ShowProgress(void){
}
void GFX_HideProgress(void){
}

void *talloc__(int size, const char *filename, int linenumber){
  return calloc(1, size);
}

void *talloc_realloc__(void *v, int new_size, const char *filename, int linenumber){
  return realloc(v, new_size);
}

extern "C"{
  void CRASHREPORTER_send_assert_message(Crash_Type tye, const char *message, ...){
    abort();
  }
}

int main(int argc, char **argv){
  QApplication app(argc,argv);
  int ret = 2;//SYSTEM_show_error_message(argv[1]);
  printf("ret1: %d\n",ret);

  vector_t menu = {};
  VECTOR_push_back(&menu, "Option 1/3 øæå ååå");
  VECTOR_push_back(&menu, "Option 2/3 øæå ååå");
  VECTOR_push_back(&menu, "Option 3/3 øæå ååå");
  ret = SYSTEM_show_message_menu(&menu, "HELLO!");
  printf("ret2: %d\n",ret);
  return 0;
}
#endif // TEST_MAIN


  
#endif // !COMPILE_EXECUTABLE

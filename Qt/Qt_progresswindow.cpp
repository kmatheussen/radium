
#include <stdio.h>
#include <unistd.h>

#include <QApplication>
#include <QTextBrowser>
#include <QFile>
#include <QProcess>
#include <QTimer>
#include <QDesktopWidget>
#include <QMainWindow>
#include <QLayout>
#include <QThread>
#include <QMutex>
#include <QQueue>
#include <QMutexLocker>


static const QString message_hide = "_MESSAGE_HIDE";
static const QString message_show = "_MESSAGE_SHOW";
static const QString message_exit = "_MESSAGE_EXIT";
static const QString message_split_string = "_____SPLIT______";


#ifdef P_CLIENT

static QString g_main_message;
static QTextBrowser *progressWindow = NULL;

/*
static int longest_line(QString text){
  int ret = 0;
  for(auto line : text.split("\n"))
    if (line.size() > ret)
      ret = line.size();

  return ret;
}
*/

class MyTimer : public QTimer{
public:
  MyTimer(){
    setInterval(200);
  }

  void timerEvent(QTimerEvent *e) override {
    if (progressWindow != NULL && progressWindow->isVisible()) {
      progressWindow->raise();
    }
  }
};

static MyTimer mytimer;

static void positionWindow(const QRect &rect, QWidget *widget){
  int height = widget->height();
  int width = widget->width();

  if (rect.height() > height){
    height = rect.height() + 100;
  }

  if (rect.width() > width){
    width = rect.width() + 100;
  }

  widget->setMinimumHeight(height);
  widget->setMaximumHeight(height);
  widget->setMinimumWidth(width);
  widget->setMaximumWidth(width);
  widget->resize(width, height);
  
  widget->move(rect.x(), rect.y());
}

static QString handle_rect_in_message(QString message, QWidget *widget){
  QStringList spl = message.split(message_split_string);
  if (spl.size() != 2)
    return "Something is wrong in Qt_progresswindow.cpp. Size of spl:" + QString::number(spl.size()) + ", message: "+message;

#if 0  // Set to 1 let window always adjust to the middle of the main window.
  QString rectstring = spl[0];
  auto spl2 = spl[0].split(",");
  if (spl2.size() != 4)
    return "Something is wrong in Qt_progresswindow.cpp. Size of spl2:" + QString::number(spl2.size()) + ", message: "+message;

  QRect rect(spl2[0].toInt(), spl2[1].toInt(), spl2[2].toInt(), spl2[3].toInt());

  positionWindow(rect, widget);
#endif
  
  return spl[1];
}

static void setContent(QString message){
  progressWindow->setHtml("<p><pre>\n\n</pre><center><b>" + g_main_message + "</b></center><p><pre>\n\n\n</pre><blockquote>" + message + "</blockquote>");
}

void process_OpenProgress(QString message, QRect rect){
  delete progressWindow;

  g_main_message = message;
  
  progressWindow = new QTextBrowser;
  progressWindow->setStyleSheet("QTextBrowser { padding-left:20; padding-top:20; padding-bottom:20; padding-right:20; background-color: white;}");
  
  
#if 1 // defined(FOR_LINUX) // popup locks up X on my computer if radium crashes while the progress window is open.
  progressWindow->setWindowFlags(progressWindow->windowFlags() | Qt::FramelessWindowHint | Qt::Tool | Qt::WindowStaysOnTopHint | Qt::MSWindowsFixedSizeDialogHint);
#else
  progressWindow->setWindowFlags(progressWindow->windowFlags() | Qt::Popup);//Qt::WindowStaysOnTopHint|Qt::SplashScreen|Qt::Window | Qt::FramelessWindowHint|Qt::Popup);
#endif

  progressWindow->resize(30,50); // positionWindow doesn't shrink window.
  positionWindow(rect, progressWindow);
  setContent("");
  
  progressWindow->show();
  progressWindow->raise();
  progressWindow->activateWindow();
}

void process_ShowProgressMessage(QString message, QRect rect){
  if (progressWindow == NULL)
    process_OpenProgress("...", rect);

  setContent(message);
}

void process_CloseProgress(void){
  delete progressWindow;
  progressWindow = NULL;
}


namespace{
  
class ReadStdinThread : public QThread
{
  QMutex mutex;
  QQueue<QString> queue;
  
  QFile in;
  
public:

  ReadStdinThread(){
    if (in.open(stdin, QIODevice::ReadOnly)==false){
      printf("Unable to open stdin\n");
    }else{
      start();
    }
  }
  
  QString get_data(void){
    QMutexLocker locker(&mutex);
    //printf("queue.size: %d\n", queue.size());
    if (queue.size()==0)
      return "";
    else
      return queue.dequeue();
  }
  
private:
  void run() override {

    for(;;){
      
      char data[1024];
      int64_t num_bytes = in.readLine(data, 1024-10);
      
      QString line;
      
      if (num_bytes==-1){
        printf("   Unable to read data. Exiting progress window process.\n");
        line = message_exit;
      }else{
        line = QByteArray::fromBase64(data);
        //printf(" *********** Got line -%s- ************* \n",line.toUtf8().constData());
      }

      if (line != ""){
        QMutexLocker locker(&mutex);
        //printf("Adding -%s- to queue\n", line.toUtf8().constData());
        queue.enqueue(line);
      }
    
      if (line==message_exit)
        return;
    }
  }
};

}


#include "getqapplicationconstructorargs.hpp"


int main(int argc, char **argv){

  QLocale::setDefault(QLocale::c());
  QLocale::setDefault(QLocale::C);

  argv = getQApplicationConstructorArgs(argc, argv);
  QApplication app(argc, argv);

  int fontsize = atoi(argv[1]);

  QRect rect(atoi(argv[2]), atoi(argv[3]), atoi(argv[4]), atoi(argv[5]));
    
  QFont font = QApplication::font();
  font.setPointSize(fontsize);
  QApplication::setFont(font);

  mytimer.start();

  QString header = QByteArray::fromBase64(argv[6]).constData();

  process_OpenProgress(header, rect);

  ReadStdinThread read_thread;

  while(true){

    QString line;
    
    do{
      QCoreApplication::processEvents();
      QThread::msleep(20);
      line = read_thread.get_data();
    }while(line == "");

    //printf("LINE: -%s-. size: %d. Empty? %d\n", line.toUtf8().constData(), line.size(), line=="");
    
    if (line==message_exit)
      break;
    else if (line==message_hide) {
      //progressWindow->setText("HIDING\n");
      progressWindow->hide();
    } else if (line==message_show) {
      progressWindow->show();
      progressWindow->raise();
    } else if (progressWindow->isVisible()){
      QString message = handle_rect_in_message(line, progressWindow);
      process_ShowProgressMessage(message, rect);
    }

    fflush(stdout);
  }

  process_CloseProgress();

  read_thread.wait();
  
  return 0;
}

#endif


#ifdef P_SERVER

#include "../common/nsmtracker.h"
#include "../common/visual_proc.h"
#include "../common/OS_settings_proc.h"

#ifdef TEST_MAIN
static QPoint getCentrePosition(QWidget *parent, int width, int height){
  QRect rect;
  
  if (parent==NULL)
    // Move to middle of screen instead.
    rect = QApplication::desktop()->availableGeometry();
  else
    rect = parent->geometry();
  
  int x = rect.x()+rect.width()/2-width/2;
  int y = rect.y()+rect.height()/2-height/2;
  //printf("w: %d, h: %d\n",width,height);

  return QPoint(R_MAX(20, x), R_MAX(20, y));
}
#else
#include "helpers.h"
#endif

extern QMainWindow *g_main_window;

static QProcess *g_process = NULL;

static QRect get_rect(int fontsize){

#ifdef TEST_MAIN
  return QRect(50,50,400,400);
#else

  int width = fontsize*600/8;
  int height = fontsize*300/8;
  
  QPoint point = getCentrePosition(g_main_window, width, height);

  return QRect(point.x(), point.y(), width, height);
#endif
}

static QString get_rect_string(void){
  QRect rect = get_rect(QApplication::font().pointSize());

  return QString("%1,%2,%3,%4%5")
    .arg(rect.x())
    .arg(rect.y())
    .arg(rect.width())
    .arg(rect.height())
    .arg(message_split_string);    
}



void GFX_OpenProgress(const char *message){
  GFX_CloseProgress();

  g_process = new QProcess;

#if FOR_WINDOWS
  QString program = OS_get_full_program_file_path("radium_progress_window.exe");
#else
  QString program = OS_get_full_program_file_path("radium_progress_window");
#endif

#if defined(FOR_LINUX) || defined(FOR_MACOSX)
  QProcessEnvironment env = QProcessEnvironment::systemEnvironment();
  env.insert("LD_LIBRARY_PATH", getenv("LD_LIBRARY_PATH"));
  g_process->setProcessEnvironment(env);
#endif

  int fontsize = QApplication::font().pointSize();

  QRect rect = get_rect(fontsize);

  printf("   %d %d %d %d\n", rect.x(),rect.y(),rect.width(),rect.height());
  //getchar();

  //g_process->setReadChannel(QProcess::StandardOutput);
  //g_process->setProcessChannelMode(QProcess::MergedChannels);
    
  g_process->start(program+" "+
                   QString::number(fontsize) + " " +
                   QString::number(rect.x()) + " " +
                   QString::number(rect.y()) + " " +
                   QString::number(rect.width()) + " " +
                   QString::number(rect.height()) + " " +
                   QString(QString(message).toUtf8().toBase64().constData()),
                   QIODevice::WriteOnly | QIODevice::Text | QIODevice::Unbuffered | QIODevice::Append
                   );

  //printf("POINGSIZE: %d\n", QApplication::font().pointSize());
  //getchar();
  
  if (g_process->waitForStarted()==false){
    printf("PROGRESSWINDOWS: Unable to start process\n");
#if !defined(RELEASE)
    getchar();
#endif
    delete g_process;
    g_process = NULL;
  }

  //g_process->setReadChannel(QProcess::StandardOutput);
}

static void send_string(QString message){
  printf("-______ sending string %s\n",message.toUtf8().constData());
  int64_t result = g_process->write((QString(message.toUtf8().toBase64().constData())+"\n").toUtf8());
  printf("num bytes sent: %d\n", (int)result);
  
  // MUST NOT DO THIS. We risk ending up at the same place two times (in case there is an error message), and then we could hit a deadlock qt (in QXcbConnection). (seems like a bug in qt)
  //QCoreApplication::processEvents(QEventLoop::ExcludeUserInputEvents);
  
  g_process->waitForBytesWritten();
  //QCoreApplication::processEvents(QEventLoop::ExcludeUserInputEvents); // This is probably not safe either.
  
  //printf("From client: -----------------%s\n-------------------\n", g_process->readAllStandardOutput().constData());
}

static bool g_is_showing=false;

void GFX_ShowProgressMessage(const char *message){
  if (g_process == NULL)
    GFX_OpenProgress("...");

  if (g_process != NULL)
    send_string(get_rect_string() + message);

  g_is_showing=true;
}

bool GFX_ProgressIsOpen(void){
  return g_process != NULL;
}



void GFX_CloseProgress(void){
  if (g_process != NULL){
    send_string(message_exit);
    g_process->closeWriteChannel();
    g_process->waitForFinished();
    delete g_process;
    g_process = NULL;
    g_is_showing = false;
  }
}

void GFX_HideProgress(void){
  if (g_process != NULL) {
    send_string(message_hide);
    g_is_showing = false;
  }
}

void GFX_ShowProgress(void){
  if (g_process != NULL) {
    send_string(message_show);
    g_is_showing = true;
  }
}


#ifdef TEST_MAIN


/*
cd /home/kjetil/radium && BUILDTYPE=DEBUG ./build_linux.sh && cd Qt && g++ Qt_progresswindow.cpp -DTEST_MAIN `pkg-config --libs Qt5Gui --cflags Qt5Gui --cflags Qt5Widgets` -std=gnu++11 -DNDEBUG -DP_SERVER -I../Qt -DFOR_LINUX -DUSE_QT4 -DUSE_QT5 `cat ../flagopts.opt` -fPIC && ./a.out
*/

  
QString OS_get_full_program_file_path(QString name){
  //return "tee /tmp/gakk.txt";
  return "/home/kjetil/radium/bin/" + name;
}

int main(int argc, char **argv){
  QApplication app(argc, argv);

  GFX_OpenProgress("hello");

  QThread::msleep(1000);

  for(int i =0;i<=5;i++){
    printf("trying to show %d\n",i);
    GFX_ShowProgressMessage((QString("ap ")+QString::number(i)).toUtf8().constData());
    for(int i=0;i<100;i++){
      QCoreApplication::processEvents(QEventLoop::ExcludeUserInputEvents);
      QThread::msleep(10);
    }
  }
  
  GFX_CloseProgress();

  QThread::msleep(1000*1);
  
  return 0;
}

#endif

#endif
//QCoreApplication::applicationPid()

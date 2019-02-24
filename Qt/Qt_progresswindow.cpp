
#include <stdio.h>
#include <unistd.h>

#include <QApplication>
#include <QFile>
#include <QProcess>
#include <QTimer>
#include <QDesktopWidget>
#include <QLayout>
#include <QThread>
#include <QMutex>
#include <QQueue>
#include <QMutexLocker>
#include <QPainter>
#include <QDateTime>


static const QString message_hide = "_MESSAGE_HIDE";
static const QString message_show = "_MESSAGE_SHOW";
static const QString message_exit = "_MESSAGE_EXIT";
static const QString message_split_string = "_____SPLIT______";


#ifdef P_CLIENT

static QString g_main_message;
static QString g_content_message;


/*********************************
 * Start.
 isParentRunning() function made by "lisabeeren" found here: https://forum.qt.io/topic/33964/solved-child-qprocess-that-dies-with-parent/10
***********************************/

#if defined(FOR_WINDOWS)
#include <windows.h>
#include <tlhelp32.h>
#else
#include "unistd.h"
#endif


#if defined(FOR_WINDOWS)

static unsigned long currentPID()
{
#if defined(FOR_WINDOWS)
  return GetCurrentProcessId();
#else
  return getpid();
#endif
}

static unsigned long parentPID()
{
#if defined(FOR_WINDOWS)
  
  HANDLE h = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  PROCESSENTRY32 pe = { 0 };
  pe.dwSize = sizeof(PROCESSENTRY32);
  
  unsigned long pid = currentPID();
  unsigned long ppid = 0;
  
  if( Process32First(h, &pe)) {
    do {
      if (pe.th32ProcessID == pid) {
        ppid = pe.th32ParentProcessID;
        break;
      }
    } while( Process32Next(h, &pe));
  }
  
  CloseHandle(h);
  
  return ppid;
  
#else
  
  return getppid();
  
#endif
}

#endif // WIN32

static bool isParentRunning()
{
#if defined(FOR_WINDOWS)
  
  static unsigned long _parentPID = parentPID();
  static void* _parentHandle = NULL;
  
  if (_parentHandle == NULL && _parentPID != 0)
    _parentHandle = OpenProcess(PROCESS_QUERY_INFORMATION, FALSE, _parentPID);
  
  if (_parentHandle != NULL)
    {
      BOOL success;
      DWORD exitCode;

      success = GetExitCodeProcess(_parentHandle, &exitCode);
      
      return ( ! success) || exitCode == STILL_ACTIVE;
    }
  
  return true;
  
#else
  return getppid() != 1;
#endif
}

/*********************************
 * End (of isParentRunning())
***********************************/



/*
static int longest_line(QString text){
  int ret = 0;
  for(auto line : text.split("\n"))
    if (line.size() > ret)
      ret = line.size();

  return ret;
}
*/

namespace{
  class MyProgressWindow : public QWidget{
    QPixmap _pixmap;
    char _text[80];

  public:
    
    MyProgressWindow()
    {
      qsrand(QDateTime::currentMSecsSinceEpoch());
      generate_new_text();
    }

    void make_anagram(const char *input, char *output) const {
      int len = strlen(input);
      memset(output,0,len);
      
      output[0]=input[0];
      
      for(int i=1;i<len;i++){
        int pos;
        do{
          pos=qrand() % len;
        }while(output[pos] != 0);
        output[pos] = input[i];
      }

      output[len] = 0;
    }

    void generate_new_text(void){
      make_anagram("RADUM", _text);
    }

    void paintAnagram(QPainter &painter){
      float text_height = height() / 5.5;
      
      QFont font("Nimbus Sans L");
      //font.fromString("Nimbus Sans L [urw],48,-1,5,75,0,0,0,0,0,Bold"); //Nimbus Sans L");
      //font.setPointSizeF(text_height);
      font.setPixelSize(text_height);
      font.setBold(true);

      painter.setFont(font);
      
      float border = height() / 30.0;

      painter.setPen(QPen(QColor("#3138bf")));
      
      QRectF rect(border, height() - text_height - border, width()-border*2, text_height);

      //generate_new_text();
        
      painter.drawText(rect, _text);
    }
    
    void resizeEvent(QResizeEvent *ev) override{
      _pixmap = QPixmap("logo_medium.png").scaled(width(), height(), Qt::KeepAspectRatioByExpanding, Qt::SmoothTransformation);
      //QPainter painter(&_pixmap);
    }

    void paintEvent(QPaintEvent *ev) override{
      QPainter painter(this);

      painter.drawPixmap(0, 0, _pixmap);

      float border = height() / 30.0;

      QFont bold_font;
      bold_font.setBold(true);
      
      const QFontMetrics fn(bold_font);
      int header_height = fn.boundingRect(QRect(border,border,width() - border, height()),
                                          Qt::TextWordWrap,
                                          g_main_message
                                          ).height();

      int fontheight = fn.boundingRect(g_main_message).height();
      
      float ysplit1 = border + header_height + fontheight*0.5;
      //float ysplit2 = ysplit1 + (header_height * 1.5);
      
      QRectF headline_rect(border, border,      width() - border, ysplit1-border);


      QTextOption option(Qt::AlignTop|Qt::AlignLeft);
      option.setWrapMode(QTextOption::WordWrap);

      painter.setFont(bold_font);
      painter.drawText(headline_rect, g_main_message, option);


      QRectF message_rect(border, ysplit1, std::min(2 * width() / 3 - border, width()-border*2), height() - ysplit1);

      QFont nonbold_font;
      painter.setFont(nonbold_font);
      painter.drawText(message_rect, g_content_message, option);

      paintAnagram(painter);
    }
  };
}

static MyProgressWindow *g_progressWindow = NULL;

static QMutex g_mutex;
static QQueue<QString> g_queue;

class MyTimer : public QTimer{
public:
  MyTimer(){
    setInterval(200);
  }
  void timerEvent(QTimerEvent *e) override {

    /*
      static int counter=0;

      QMutexLocker locker(&g_mutex);
      //printf("Adding -%s- to queue\n", line.toUtf8().constData());
      g_queue.enqueue(QString(" "+QString::number(counter++) + ". Is parent alive? :" + (isParentRunning() ? "Yes" : "No")));
    */

    if (!isParentRunning()){
      exit(-1);
    }
    
    if (g_progressWindow != NULL && g_progressWindow->isVisible()) {
      g_progressWindow->raise();
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

  height=rect.height();
  width=rect.width();
  
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
  g_content_message = message;
  //progressWindow->setHtml("<p><pre>\n\n</pre><center><b>" + g_main_message + "</b></center><p><pre>\n\n\n</pre><blockquote>" + message + "</blockquote>");
  g_progressWindow->update();
}


static void process_OpenProgress(QString message, QRect rect){
  delete g_progressWindow;

  g_main_message = message;
  
  //progressWindow = new QTextBrowser;
  g_progressWindow = new MyProgressWindow;
  //progressWindow->setStyleSheet("QTextBrowser { padding-left:20; padding-top:20; padding-bottom:20; padding-right:20; background-color: white;}");
  //  g_progressWindow->setStyleSheet("QTextBrowser { padding-left:20; padding-top:20; padding-bottom:20; padding-right:20; background-color: none;}"); //background-image: url(./logo_smaller.png);}");
  
#if 1 // defined(FOR_LINUX) // popup locks up X on my computer if radium crashes while the progress window is open.
  g_progressWindow->setWindowFlags(g_progressWindow->windowFlags() | Qt::FramelessWindowHint | Qt::Tool | Qt::WindowStaysOnTopHint | Qt::MSWindowsFixedSizeDialogHint);
#else
  g_progressWindow->setWindowFlags(g_progressWindow->windowFlags() | Qt::Popup);//Qt::WindowStaysOnTopHint|Qt::SplashScreen|Qt::Window | Qt::FramelessWindowHint|Qt::Popup);
#endif

  g_progressWindow->resize(30,50); // positionWindow doesn't shrink window.
  positionWindow(rect, g_progressWindow);
  setContent("");

  g_progressWindow->generate_new_text();
  g_progressWindow->show();
  g_progressWindow->raise();
  g_progressWindow->activateWindow();
}

static void process_ShowProgressMessage(QString message, QRect rect){
  if (g_progressWindow == NULL)
    process_OpenProgress("...", rect);

  setContent(message);
}

static void process_CloseProgress(void){
  delete g_progressWindow;
  g_progressWindow = NULL;
}


namespace{
  
class ReadStdinThread : public QThread
{
  
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
    QMutexLocker locker(&g_mutex);
    //printf("queue.size: %d\n", queue.size());
    if (g_queue.size()==0)
      return "";
    else
      return g_queue.dequeue();
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
        QMutexLocker locker(&g_mutex);
        //printf("Adding -%s- to queue\n", line.toUtf8().constData());
        g_queue.enqueue(line);
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
      //g_progressWindow->setText("HIDING\n");
      g_progressWindow->hide();
    } else if (line==message_show) {
      g_progressWindow->generate_new_text();
      g_progressWindow->show();
      g_progressWindow->raise();
    } else if (g_progressWindow->isVisible()){
      QString message = handle_rect_in_message(line, g_progressWindow);
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

extern QWidget *g_main_window;

static QProcess *g_process = NULL;

static QRect get_rect(int fontsize){

#ifdef TEST_MAIN
  return QRect(50,50,400,400);
#else  
  
  //int width = fontsize*600/8;
  int height = fontsize*300/8;
  int width = height * 1.779291553133515;
  
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


static int g_num_open = 0;

void GFX_OpenProgress(const char *message){
  g_num_open++;

  if(g_num_open>1)
    return;
  
  //GFX_CloseProgress();

  g_process = new QProcess;

#if FOR_WINDOWS
  QString program = OS_get_full_program_file_path("radium_progress_window.exe");
#else
  QString program = OS_get_full_program_file_path("radium_progress_window");
#endif

#if defined(FOR_WINDOWS)
  program = QString("\"") + program + "\""; // necessary if path contains spaces.
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

#if 1
  g_process->start(program+" "+
                   QString::number(fontsize) + " " +
                   QString::number(rect.x()) + " " +
                   QString::number(rect.y()) + " " +
                   QString::number(rect.width()) + " " +
                   QString::number(rect.height()) + " " +
                   QString(QString(message).toUtf8().toBase64().constData()),
                   QIODevice::WriteOnly | QIODevice::Text | QIODevice::Unbuffered | QIODevice::Append
                   );
#else
  g_process->setProgram(program);
  QStringList args;
  args << QString::number(fontsize);
  args <<               QString::number(rect.x());
  args <<       QString::number(rect.y());
  args << QString::number(rect.width());
  args <<       QString::number(rect.height());
  args <<  QString(QString(message).toUtf8().toBase64().constData());
  g_process->setArguments(args);
  g_process->startDetached();
#endif
  
  //printf("POINGSIZE: %d\n", QApplication::font().pointSize());
  //getchar();
  
  if (g_process->waitForStarted()==false){
    printf("PROGRESSWINDOWS: Unable to start process: -%s-\n", program.toUtf8().constData());
#if !defined(RELEASE)
    getchar();
    abort();
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
  g_num_open--;

  if(g_num_open > 0)
    return;
  
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
cd /home/kjetil/radium && BUILDTYPE=DEBUG ./build_linux.sh && cd Qt && g++ Qt_progresswindow.cpp -DTEST_MAIN `pkg-config --libs Qt5Gui --cflags Qt5Gui --cflags Qt5Widgets` -std=gnu++11 -DNDEBUG -DP_SERVER -I../Qt -DFOR_LINUX -DUSE_QT4 -DUSE_QT5 `cat ../flagopts.opt` -fPIC -Wall && ./a.out
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

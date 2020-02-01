
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
#include <QToolButton>
#include <QScreen>


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
  PROCESSENTRY32 pe = {};
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
    QPixmap _pixmap_org;
    QPixmap _pixmap;
    char _text[80];
    QToolButton *_close_button;
    Qt::WindowFlags _org_flags;
     
  public:
    
    MyProgressWindow()
    {
      qsrand(QDateTime::currentMSecsSinceEpoch());
      generate_new_text();

      _org_flags = windowFlags();

      //setWindowFlags(_org_flags | Qt::FramelessWindowHint);
      setWindowFlags(_org_flags | Qt::Tool | Qt::WindowStaysOnTopHint | Qt::FramelessWindowHint | Qt::MSWindowsFixedSizeDialogHint);
      
      _pixmap_org = QPixmap("logo_medium.png");
      
      _close_button = new QToolButton(this);
      _close_button->setText("_");

      _close_button->setAttribute(Qt::WA_TranslucentBackground);
      _close_button->setStyleSheet("background-color: #46638f");
      
      _close_button->resize(10,10);
      _close_button->adjustSize();
      _close_button->updateGeometry();

      connect(_close_button, &QToolButton::released, this, [this]()
              {
                //printf("HERE\n");                
#if FOR_MACOSX
                //showMinimized doesn't work for frameless windows on OSX. Workaround:
                setWindowFlags(_org_flags | Qt::WindowMinMaxButtonsHint); // https://bugreports.qt.io/browse/QTBUG-64994
                _close_button->hide(); // Don"t need it since the min/max buttons are visible now.
#endif
                showMinimized();
                
              }
              );

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
      _pixmap = _pixmap_org.scaled(width(), height(), Qt::KeepAspectRatioByExpanding, Qt::SmoothTransformation);
        
      _close_button->move(width()-_close_button->width(), 0);
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

class MyTimer : public QTimer{
public:
  MyTimer(){
    setInterval(200);
  }
  void timerEvent(QTimerEvent *e) override {

    if (!isParentRunning())
      QApplication::quit();

    if (g_progressWindow != NULL && g_progressWindow->isVisible() && !g_progressWindow->isMinimized()) {
      g_progressWindow->raise();
    }
  }
};


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
  QRect _rect;
  
public:

  ReadStdinThread(const QRect &rect)
    : _rect(rect)
  {
    if (in.open(stdin, QIODevice::ReadOnly)==false){
      printf("Unable to open stdin\n");
    }else{
      start();
    }
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
        QMetaObject::invokeMethod(qApp, [line, this]
                                  {
                                    // We are now in the main thread.
                                    
                                    if (line==message_exit)
                                      
                                      QApplication::quit();
                                    
                                    else if (line==message_hide) {
                                      //g_progressWindow->setText("HIDING\n");
                                      g_progressWindow->hide();
                                      
                                    } else if (line==message_show) {
                                      g_progressWindow->generate_new_text();
                                      g_progressWindow->show();
                                      g_progressWindow->raise();
                                      
                                    } else if (g_progressWindow->isVisible()){
                                      QString message = handle_rect_in_message(line, g_progressWindow);
                                      process_ShowProgressMessage(message, _rect);
                                    }
                                    
                                  }
                                  );
        
        if (line==message_exit)
          return;
      }
    
    }
  }
};

}

static QRect get_startup_rect(void){
  int fontsize = QApplication::font().pointSize();
  
  int height = fontsize*300/8;
  int width = height * 1.779291553133515;
  
  QPoint point;
  
  QScreen *screen = QApplication::screens().first();

  if (screen==NULL) {
    
    point = QPoint(400,400);
    
  } else {
    
    QRect parentRect = screen->availableGeometry();
    
    int x = parentRect.x() + (parentRect.width() - width)/2;
    int y = parentRect.y() + (parentRect.height() - height)/2;

    point = QPoint(std::max(20, x), std::max(20, y));

  }
  
  return QRect(point.x(), point.y(), width, height);
}


#include "getqapplicationconstructorargs.hpp"


int main(int argc, char **argv){

  QLocale::setDefault(QLocale::c());
  QLocale::setDefault(QLocale::C);

  argv = getQApplicationConstructorArgs(argc, argv);
  QApplication app(argc, argv);

  bool is_startup_message = false;
  
  if (!strcmp(argv[1], "--startup"))
    is_startup_message = true;

  QFont font = QApplication::font();

  QRect rect;
  QString header;
  
  if (is_startup_message) {

    rect = get_startup_rect();

    header = "Please wait for the operating system to start Radium";
    
  } else {
    int fontsize = atoi(argv[1]);

    rect = QRect(atoi(argv[2]), atoi(argv[3]), atoi(argv[4]), atoi(argv[5]));
    
    font.setPointSize(fontsize);
    QApplication::setFont(font);

    header = QByteArray::fromBase64(argv[6]).constData();
  }
  
  MyTimer mytimer;
  mytimer.start();

  process_OpenProgress(header, rect);

  ReadStdinThread read_thread(rect);

  qApp->exec();
  
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

void CRASHREPORTER_send_assert_message(enum Crash_Type crash_type, const char *fmt,...){
  printf("Crash: -%s-\n", fmt);
  abort();
}

double TIME_get_ms(void){
  return QTime::currentTime().msecsSinceStartOfDay();
}

/*
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
*/
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
  QString program = STRING_get_qstring(OS_get_full_program_file_path("radium_progress_window.exe").id);
#else
  QString program = STRING_get_qstring(OS_get_full_program_file_path("radium_progress_window").id);
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

#ifdef RADIUM_USES_TSAN // get broken pipe in waitForBytesWritten when using tsan.
  return;
#endif
  
  int64_t result = g_process->write((QString(message.toUtf8().toBase64().constData())+"\n").toUtf8());
  printf("num bytes sent: %d\n", (int)result);
  
  // MUST NOT DO THIS. We risk ending up at the same place two times (in case there is an error message), and then we could hit a deadlock qt (in QXcbConnection). (seems like a bug in qt)
  //QCoreApplication::processEvents(QEventLoop::ExcludeUserInputEvents);
  
  g_process->waitForBytesWritten();
  //QCoreApplication::processEvents(QEventLoop::ExcludeUserInputEvents); // This is probably not safe either.
  
  //printf("From client: -----------------%s\n-------------------\n", g_process->readAllStandardOutput().constData());
}

static bool g_is_showing=false;

void GFX_ShowProgressMessage(const char *message, bool force_show){
  static double last_time = -1;
  
  if (g_process == NULL){
    //GFX_OpenProgress("...");
    R_ASSERT_NON_RELEASE(false);
    return;
  }

  if(force_show==false){
    double time = TIME_get_ms();
    if ((time-last_time) < 100)
      return;
  }
  
  if (g_process != NULL) {
    send_string(get_rect_string() + message);
    g_is_showing=true;
  }

  last_time = TIME_get_ms();
}

bool GFX_ProgressIsOpen(void){
  return g_process != NULL;
}



void GFX_CloseProgress(void){
  if(g_num_open==0){    
    R_ASSERT_NON_RELEASE(false);
    return;
  }
  
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
cd /home/kjetil/radium && BUILDTYPE=DEBUG ./build_linux.sh && cd Qt && g++ Qt_progresswindow.cpp -DTEST_MAIN `pkg-config --libs Qt5Gui --cflags Qt5Gui --cflags Qt5Widgets` -std=gnu++11 -DNDEBUG -DP_SERVER -I../Qt -DFOR_LINUX -DUSE_QT4 -DUSE_QT5 `cat ../flagopts.opt` -fPIC -Wall && cd /home/kjetil/radium/bin && ../Qt/a.out
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
    GFX_ShowProgressMessage((QString("ap ")+QString::number(i)).toUtf8().constData(), true);
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

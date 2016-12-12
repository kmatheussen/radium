
#include <stdio.h>
#include <unistd.h>

#include <QApplication>
#include <QMessageBox>
#include <QFile>
#include <QProcess>
#include <QTimer>
#include <QDesktopWidget>

static const QString message_hide = "_MESSAGE_HIDE";
static const QString message_show = "_MESSAGE_SHOW";
static const QString message_exit = "_MESSAGE_EXIT";

#ifdef P_CLIENT

static QMessageBox *progressBox = NULL;

static int longest_line(QString text){
  int ret = 0;
  for(auto line : text.split("\n"))
    if (line.size() > ret)
      ret = line.size();

  return ret;
}

class MyTimer : public QTimer{
public:
  MyTimer(){
    setInterval(200);
  }

  void timerEvent(QTimerEvent *e) override {
    if (progressBox != NULL && progressBox->isVisible()) {
      progressBox->raise();
    }
  }
};

static MyTimer mytimer;

void process_OpenProgress(QString message, int fontsize){
  delete progressBox;

  progressBox = new QMessageBox;
#if 1 // defined(FOR_LINUX) // popup locks up X on my computer if radium crashes while the progress window is open.
  progressBox->setWindowFlags(progressBox->windowFlags() | Qt::FramelessWindowHint);
#else
  progressBox->setWindowFlags(progressBox->windowFlags() | Qt::Popup);//Qt::WindowStaysOnTopHint|Qt::SplashScreen|Qt::Window | Qt::FramelessWindowHint|Qt::Popup);
#endif

  int width = fontsize*600/8;
  int height = fontsize*300/8;
  
  progressBox->setStandardButtons(0);
  progressBox->setText(message + "                                                                                                                " + "\n\n\n\n                                                                                                                ");
  progressBox->setInformativeText("             \n            \n              \n                \n               \n");


  progressBox->setMinimumWidth(width);
  progressBox->setMinimumHeight(height);
  progressBox->setMaximumWidth(width);
  progressBox->setMaximumHeight(height);

  QRect screenGeometry = QApplication::desktop()->screenGeometry();
  int x = (screenGeometry.width()-width) / 2;
  int y = (screenGeometry.height()-height) / 2;
  progressBox->move(x, y);
  
  progressBox->show();

  progressBox->setMinimumWidth(width);
  progressBox->setMinimumHeight(height);

  for(int i=0; i < 10 ; i++){
    progressBox->repaint();
    QCoreApplication::processEvents(QEventLoop::ExcludeUserInputEvents);
    usleep(10);
  }

}

void process_ShowProgressMessage(QString message, int fontsize){
  if (progressBox == NULL)
    process_OpenProgress("...", fontsize);

  // Some ridiculous code to try to work around QMessageBox window size jumping
  {
    int len1 = longest_line(progressBox->informativeText());
    int len2 = longest_line(message);

    if (len2 < len1) {
      message += "\n";
      for(int i=0;i<len2;i++)
        message += " ";
      message += "\n";
    }
    
    int num_ls = message.count("\n");
    
    QString out = message;
    for(int i=num_ls ; i<5;i++) {
      out+="                   \n";
    }
      
    progressBox->setInformativeText(out);
  }

  progressBox->setMinimumWidth(fontsize*600/8);
  progressBox->setMinimumHeight(fontsize*300/8);

  for(int i=0; i < 10 ; i++){
    progressBox->repaint();
    QCoreApplication::processEvents(QEventLoop::ExcludeUserInputEvents);
    usleep(10);
  }
}

void process_CloseProgress(void){
  delete progressBox;
  progressBox = NULL;
}

int main(int argc, char **argv){
  
  QApplication app(argc, argv);

  int fontsize = atoi(argv[1]);
    
  QFont font = QApplication::font();
  font.setPointSize(fontsize);
  QApplication::setFont(font);
  

  mytimer.start();

  QString header = QByteArray::fromBase64(argv[2]).constData();
  process_OpenProgress(header, fontsize);

  QFile in;
  in.open(stdin, QIODevice::ReadOnly);

  while(true){
    QString line = QByteArray::fromBase64(in.readLine().trimmed().constData());
    printf(" *********** Got line -%s- ************* \n",line.toUtf8().constData());
    //getchar();
    
    if (line==message_exit)
      break;
    else if (line==message_hide) {
      //progressBox->setText("HIDING\n");
      progressBox->hide();
    } else if (line==message_show) {
      progressBox->show();
      progressBox->raise();
    } else if (progressBox->isVisible())
      process_ShowProgressMessage(line, fontsize);
  }

  process_CloseProgress();

  return 0;
}

#endif


#ifdef P_SERVER

#include "../common/nsmtracker.h"
#include "../common/visual_proc.h"
#include "../common/OS_settings_proc.h"

static QProcess *g_process = NULL;

void GFX_OpenProgress(const char *message){
  delete g_process;
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

  g_process->start(program+" "+QString::number(QApplication::font().pointSize()) + " " + QString(QString(message).toUtf8().toBase64().constData()), QIODevice::WriteOnly | QIODevice::Text | QIODevice::Unbuffered | QIODevice::Append);

  //printf("POINGSIZE: %d\n", QApplication::font().pointSize());
  //getchar();
  
  if (g_process->waitForStarted()==false){
    printf("Unable to start process\n");
    delete g_process;
    g_process = NULL;
  }
}

static void send_string(QString message){
  printf("-______ sending string %s\n",message.toUtf8().constData());
  g_process->write((QString(message.toUtf8().toBase64().constData())+"\n").toUtf8());
  g_process->waitForBytesWritten();
}
    
void GFX_ShowProgressMessage(const char *message){
  if (g_process == NULL)
    GFX_OpenProgress("...");

  if (g_process != NULL) {
    send_string(message);

    //g_process->waitForFinished();
    //g_process->closeWriteChannel();
  }
}

void GFX_CloseProgress(void){
  if (g_process != NULL){
    send_string(message_exit);
    g_process->closeWriteChannel();
    g_process->waitForFinished();
    delete g_process;
    g_process = NULL;
  }
}

void GFX_HideProgress(void){
  if (g_process != NULL) {
    send_string(message_hide);
  }
}

void GFX_ShowProgress(void){
  if (g_process != NULL)
    send_string(message_show);
}


#ifdef TEST_MAIN


/*
cd ..
BUILDTYPE=DEBUG ./build_linux.sh
cd Qt
g++ Qt_progresswindow.cpp -DTEST_MAIN `pkg-config --libs Qt5Gui --cflags Qt5Gui --cflags Qt5Widgets` -std=gnu++11 -DNDEBUG -DP_SERVER -I../Qt -DFOR_LINUX -DUSE_QT4 -DUSE_QT5 `cat ../flagopts.opt`
./a.out
*/

  
QString OS_get_full_program_file_path(QString name){
  return "/home/kjetil/radium/bin/radium_progress_window";
}

int main(int argc, char **argv){
  QApplication app(argc, argv);

  GFX_OpenProgress("hello");

  usleep(1000*1000);

  for(int i =0;i<=5;i++){
    printf("trying to show %d\n",i);
    GFX_ShowProgressMessage((QString("ap ")+QString::number(i)).toUtf8().constData());
    usleep(1000*1000);
  }
  
  GFX_CloseProgress();

  usleep(1000*1000*1);
  
  return 0;
}

#endif

#endif
//QCoreApplication::applicationPid()

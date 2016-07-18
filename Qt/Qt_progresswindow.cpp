
#include <stdio.h>
#include <unistd.h>

#include <QApplication>
#include <QMessageBox>
#include <QFile>
#include <QProcess>

#ifdef P_CLIENT

static QMessageBox *progressBox = NULL;

static int longest_line(QString text){
  int ret = 0;
  for(auto line : text.split("\n"))
    if (line.size() > ret)
      ret = line.size();

  return ret;
}

void process_OpenProgress(QString message){
  delete progressBox;

  progressBox = new QMessageBox;
#if 1 // defined(FOR_LINUX) // popup locks up X on my computer if radium crashes while the progress window is open.
  progressBox->setWindowFlags(progressBox->windowFlags() | Qt::FramelessWindowHint);
#else
  progressBox->setWindowFlags(progressBox->windowFlags() | Qt::Popup);//Qt::WindowStaysOnTopHint|Qt::SplashScreen|Qt::Window | Qt::FramelessWindowHint|Qt::Popup);
#endif
  
  progressBox->setMinimumWidth(600);
  progressBox->setMinimumHeight(300);
  
  progressBox->setStandardButtons(0);
  progressBox->setText(message + "                                                                                                                " + "\n\n\n\n                                                                                                                ");
  progressBox->setInformativeText("             \n            \n              \n                \n               \n");
  progressBox->show();
  for(int i=0; i < 10 ; i++){
    progressBox->repaint();
    QCoreApplication::processEvents(QEventLoop::ExcludeUserInputEvents);
    usleep(10);
  }
}

void process_ShowProgressMessage(QString message){
  if (progressBox == NULL)
    process_OpenProgress("...");

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

  QString header = QByteArray::fromBase64(argv[1]).constData();
  process_OpenProgress(header);

  QFile in;
  in.open(stdin, QIODevice::ReadOnly);

  while(true){
    QString line = QByteArray::fromBase64(in.readLine().trimmed().constData());
    printf("Got line -%s-\n",line.toUtf8().constData());
    if (line=="exit")
      break;
    process_ShowProgressMessage(line);
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

  g_process->start(program+" "+QString(QString(message).toUtf8().toBase64().constData()), QIODevice::WriteOnly | QIODevice::Text | QIODevice::Unbuffered | QIODevice::Append);

  if (g_process->waitForStarted()==false){
    printf("Unable to start process\n");
    delete g_process;
    g_process = NULL;
  }
}

void GFX_ShowProgressMessage(const char *message){
  if (g_process == NULL)
    GFX_OpenProgress("...");

  if (g_process != NULL) {
    g_process->write((QString(QString(message).toUtf8().toBase64().constData())+"\n").toUtf8());
    g_process->waitForBytesWritten();

    //g_process->waitForFinished();
    //g_process->closeWriteChannel();
  }
}

void GFX_CloseProgress(void){
  if (g_process != NULL){
    g_process->write(QString(QString(QString("exit").toUtf8().toBase64().constData())+"\n").toUtf8().constData());
    g_process->waitForBytesWritten();
    g_process->closeWriteChannel();
    g_process->waitForFinished();
    delete g_process;
    g_process = NULL;
  }
}


#ifdef TEST_MAIN

int main(int argc, char **argv){
  QApplication app(argc, argv);

  GFX_OpenProgress("hello");

  usleep(1000*1000);

  for(int i =0;i<=10;i++){
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

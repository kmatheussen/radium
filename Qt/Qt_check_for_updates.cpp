#include <QNetworkAccessManager>
#include <QNetworkRequest>
#include <QNetworkReply>
#include <QFile>
#include <QMessageBox>
#include <QApplication>

#ifndef TEST_MAIN

#include "../common/nsmtracker.h"
#include "../common/settings_proc.h"
#include "../OpenGL/Widget_proc.h"

#else
#include <assert.h>

static void RError(const char *fmt, const char *arg){
  fprintf(stderr, "RError ");
  fprintf(stderr, fmt, arg);
}
#define R_ASSERT_RETURN_IF_FALSE2(a,b) assert(a)

static void GL_lock(void){}
static void GL_unlock(void){}
#endif




static bool getVersionNumbers(QString versionString, int &major, int &minor, int &revision){
  QStringList list = versionString.split(".");
  if (list.size()<3) {
#if !defined(RELEASE)
    RError("getVersionNumbers error: -%s-",versionString.toUtf8().constData());
#endif
    return false;
  }
  
  major = list[0].toInt();
  minor = list[1].toInt();
  revision = list[2].toInt();

  return true;
}

static bool hasNewer(QString newestversion, QString thisversion){

  int major1, minor1, revision1;
  int major2, minor2, revision2;

  if (getVersionNumbers(newestversion, major1, minor1, revision1)==false)
    return false;
  
  R_ASSERT_RETURN_IF_FALSE2(getVersionNumbers(thisversion, major2, minor2, revision2), false);

  int a = major1*10000 + minor1*100 + revision1;
  int b = major2*10000 + minor2*100 + revision2;

  return a > b;
}

#ifndef TEST_MAIN
static QString last_informed_version(void){
  return SETTINGS_read_string("latest_informed_update_version", "");
}

static void set_last_informed_version(QString version){
  SETTINGS_write_string("latest_informed_update_version", version.toUtf8().constData());
}
#else
#define VERSION "2.0.0"
static QString last_informed_version(void){
  return "9.9.9";
}

static void set_last_informed_version(QString version){}
#endif

static void maybeInformAboutNewVersion(QString newestversion = "3.5.1"){
  fprintf(stderr,"newestversion: -%s-, VERSION: -%s-, last_informed: -%s-\n",newestversion.toUtf8().constData(), VERSION, last_informed_version().toUtf8().constData());
  //abort();
  if (hasNewer(newestversion, VERSION) && last_informed_version()!=newestversion) {
    printf("Version %s of Radium is available for download at http://users.notam02.no/~kjetism/radium (%s)\n", newestversion.toUtf8().constData(), VERSION);
    GL_lock();{
      QMessageBox::information(NULL,
                               "Hello!",
                               "You are running Radium V" VERSION ".<p>"
                               "A newer version (V" + newestversion + ") is available for download at <A href=\"http://users.notam02.no/~kjetism/radium\">http://users.notam02.no/~kjetism/radium</a>"
                               );
    }GL_unlock();
    set_last_informed_version(newestversion);
  } else
    printf("Nope, %s is actually newer than (or just as old) as %s\n", VERSION, newestversion.toUtf8().constData());
}



namespace{

// Based on this example: http://www.codeprogress.com/cpp/libraries/qt/showQtExample.php?index=581&key=QNetworkAccessManagerDownloadFileHTTP
class MyNetworkAccessManager : public QNetworkAccessManager
{
  Q_OBJECT
  
public:
  MyNetworkAccessManager()
  {
    m_pBuffer = new QByteArray();
    QUrl url = QUrl("http://users.notam02.no/~kjetism/radium/demos/windows64/?C=M;O=D");
    QNetworkRequest req(url);
    reply = get(req);
    
    connect(reply,SIGNAL(readyRead()),this,SLOT(slotReadData()));
    connect(reply,SIGNAL(finished()), this,SLOT(slotFinished()));    
  }
  ~MyNetworkAccessManager()
  {
    delete m_pBuffer;
    delete reply;
  }
  
private slots:

  //If the reply is finished, save data in buffer to disk
  //as a png image: google_image_logo.png
  void slotFinished()
  {
    QString all = m_pBuffer->constData(); //reply->readAll().constData();

    //printf("got %d: -%s-\n", (int)reply->bytesAvailable(), all.conreply->readAll().constData());
    QString searchString = "radium_64bit_windows-";
    int startPos = all.indexOf(searchString);

    if (startPos > 0) {
      QString versionString = all.remove(0, startPos+searchString.length());
      int endPos = versionString.indexOf("-demo");
      if (endPos > 0) {
        versionString = versionString.left(endPos);
        printf("versionString: _%s_\n",versionString.toUtf8().constData());
        maybeInformAboutNewVersion(versionString);
      }
    }
  }

  void slotReadData()
  {
    //append data to QByteArray buffer
    *m_pBuffer += reply->readAll();
  }


private:
  QByteArray* m_pBuffer;
  QNetworkReply* reply; 
};

}




void UPDATECHECKER_doit(void){
  //MyNetworkAccessManager *nam = 
  new MyNetworkAccessManager;
}





#ifdef TEST_MAIN

// moc-qt4 Qt_check_for_updates.cpp >mQt_check_for_updates.cpp && g++ Qt_check_for_updates.cpp -DTEST_MAIN `pkg-config --libs --cflags QtNetwork QtGui` -Wall && ./a.out 


int main(int argc, char **argv){
  QApplication app (argc, argv);

  QString version = "531.3.2345";
  MyNetworkAccessManager nam;
  int major, minor, revision;
  getVersionNumbers(version,major,minor,revision);
  printf("%s: %d.%d.%d\n",version.toUtf8().constData(),major,minor,revision);

  maybeInformAboutNewVersion("2.5.0");
  maybeInformAboutNewVersion("2.5.1");
  maybeInformAboutNewVersion("2.5.7");

  maybeInformAboutNewVersion("1.5.7");

  maybeInformAboutNewVersion("2.4.70");

  return app.exec();
}

#endif


#include "mQt_check_for_updates.cpp"


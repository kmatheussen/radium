/*
  Written by Kjetil Matheussen: k.s.matheussen@notam02.no

  Requires qhttpserver, written by Nikhil Marathe:
  $wget https://github.com/kmatheussen/qhttpserver/archive/master.tar.gz


  * Ways to access it:

    * Using curl:
    curl -i -X POST -H "Content-Type: text/plain" -d '(display 50)' http://localhost:5080

    * Using s7webserver_repl.py:
    ./s7webserver_repl.py

    * Using a browser:
    firefox s7webserver_repl.html


  * The C API:

    s7webserver *s7webserver_create(s7_scheme *s7, int portnum, bool find_first_free_portnum);
    void s7webserver_set_verbose(s7webserver *s7server, bool verbose);
    void s7webserver_set_very_verbose(s7webserver *s7webserver, bool very_verbose);
    int s7webserver_get_portnumber(s7webserver *s7webserver);
    void s7webserver_delete(s7webserver *s7server);
*/


#ifndef S7WEBSERVER_H
#define S7WEBSERVER_H

#ifdef __cplusplus

#ifdef COMPILING_S7WEBSERVER

#include "qhttpserverfwd.h"

#include <QObject>
#include <QScopedPointer>


struct S7WebServer : public QObject
{
  Q_OBJECT

public:
  S7WebServer(s7_scheme *s7, int portnumber, bool enable_remote_connections);
  s7_scheme *s7;
  int portnumber;
  bool verbose;
  bool very_verbose;
  bool has_started;
  std::string input_code_so_far;
                  
private slots:
  void handleRequest(QHttpRequest *req, QHttpResponse *resp);
};


class S7WebServerResponder : public QObject
{
  Q_OBJECT

public:
  S7WebServerResponder(S7WebServer *s7server, QHttpRequest *req, QHttpResponse *resp);

  S7WebServer *s7webserver;
  
private:
  QScopedPointer<QHttpRequest> request;
  
public:
  QHttpResponse *response;

signals:
  void done();
             
private slots:
  void accumulate(const QByteArray &data);
  void reply();
};

#endif // COMPILING_S7WEBSERVER

extern "C" {

#endif // __cplusplus

typedef struct S7WebServer s7webserver_t;

s7webserver_t *s7webserver_create(s7_scheme *s7, int portnumber, bool find_first_free_portnum); // Same as calling s7webserver_create2 with enable_remote_connections==true.
s7webserver_t *s7webserver_create2(s7_scheme *s7, int portnumber, bool enable_remote_connections, bool find_first_free_portnum);
void s7webserver_set_verbose(s7webserver_t *s7server, bool verbose); // default is false
void s7webserver_set_very_verbose(s7webserver_t *s7webserver, bool very_verbose); // default is false
int s7webserver_get_portnumber(s7webserver_t *s7webserver);
void s7webserver_delete(s7webserver_t *s7server);
  
#ifdef __cplusplus
}
#endif // __cplusplus

#endif // S7WEBSERVER_H

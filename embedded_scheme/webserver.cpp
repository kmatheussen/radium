/*
  This file is based on the "bodydata" example from qhttpserver.
 */


#include "webserver.h"

#include <QCoreApplication>
#include <QRegExp>
#include <QStringList>
#include <QDebug>

#include <qhttpserver.h>
#include <qhttprequest.h>
#include <qhttpresponse.h>

#include "webserver_proc.h"


/*
Test:
curl -i -X POST -H "Content-Type: plain/text" -d '(display 50)' http://localhost:5080/user/asdf
*/


/// BodyData

BodyData::BodyData()
{
    QHttpServer *server = new QHttpServer(this);
    connect(server, SIGNAL(newRequest(QHttpRequest*, QHttpResponse*)),
        this, SLOT(handleRequest(QHttpRequest*, QHttpResponse*)));
        
    server->listen(QHostAddress::Any, 5080);
}

void BodyData::handleRequest(QHttpRequest *req, QHttpResponse *resp)
{
  printf("hepp2\n");
    new Responder(req, resp);
}

/// Responder

Responder::Responder(QHttpRequest *req, QHttpResponse *resp)
    : m_req(req)
    , m_resp(resp)
{
  printf("responder got something\n");
    QRegExp exp("^/user/([a-z]+$)");
    if (exp.indexIn(req->path()) == -1)
    {
        resp->writeHead(403);
        resp->end(QByteArray("You aren't allowed here!"));
        /// @todo There should be a way to tell request to stop streaming data
        return;
    }

    resp->setHeader("Content-Type", "plain/text");
    resp->writeHead(200);
    
    QString name = exp.capturedTexts()[1];
    QString bodyStart = tr("Hello: %1").arg(name);
    resp->write(bodyStart.toUtf8());

    connect(req, SIGNAL(data(const QByteArray&)), this, SLOT(accumulate(const QByteArray&)));
    connect(req, SIGNAL(end()), this, SLOT(reply()));
    connect(m_resp, SIGNAL(done()), this, SLOT(deleteLater()));
}

Responder::~Responder()
{
}

void Responder::accumulate(const QByteArray &data)
{
  code += data.data();
  printf("code so far: %s\n\n\n\n\n\n\n",code.toLatin1().data());
    //m_resp->write(data);
}

void Responder::reply()
{
  printf("Got code: -%s-\n",code.toLatin1().data());
  m_resp->end(QByteArray("-end-"));
}

/// main

void WEBSERVER_start(void){

  //QCoreApplication app(argc, argv);
  new BodyData(); // bodydata;
  //app.exec();
}

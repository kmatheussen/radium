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

#include "s7.h"


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

// function written by Rick Taube for common music (https://ccrma.stanford.edu/software/snd/snd/s7.html#repl)
static bool is_balanced(std::string str)
{
  int parens = 0;
  int quotes = 0;
  unsigned i = 0;
  while (i < str.size())
    {
      if (str[i] == ';')
	{
	  for (i = i + 1; i < str.size(); i++)
	    {
	      if (str[i] == '\n')
		break;
	    }
	}
      else if (str[i] == '"')
	{
	  if (i == 0 || str[i - 1] != '\\')
	    {
	      quotes = 1;
	      for (i = i + 1; i < str.size(); i++)
		{
		  if (str[i] == '"' && str[i - 1] != '\\')
		    {
		      quotes = 0;
		      break;
		    }
		}
	      if (quotes)
		return false;
	    }
	}
      else if (str[i] == '(')
	parens++;
      else if (str[i] == ')')
	parens--;
      i++;
    }
  return (parens == 0) && (quotes == 0);
}

// function written by Rick Taube for common music (https://ccrma.stanford.edu/software/snd/snd/s7.html#repl)
static bool is_not_white(std::string str)
{
  for (unsigned i = 0; (i < str.size() && str[i] != ';'); i++)
    if (str[i] != ' ' && str[i] != '\n' && str[i] != '\t')
      return true;
  return false;
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


static std::string str;
s7_scheme *s7;

void Responder::reply()
{
  printf("Got code: -%s-\n",code.toLatin1().data());
  str = str + code.toLatin1().data() + "\n";
  if (is_balanced(str)) {
    if (is_not_white(str)) {
      printf("Got balanced code: -%s-\n",str.c_str());
      s7_pointer val = s7_eval_c_string(s7, str.c_str());
      m_resp->end(QByteArray(s7_object_to_c_string(s7, val)));
      printf("result: -%s-\n",s7_object_to_c_string(s7, val));
    }
    str = "";
  } else {
    m_resp->end(QByteArray("-unbalanced, waiting for more input-"));
  }
}

/// main

void WEBSERVER_start(void){

  s7 = s7_init();

  //QCoreApplication app(argc, argv);
  new BodyData(); // bodydata;
  //app.exec();
}

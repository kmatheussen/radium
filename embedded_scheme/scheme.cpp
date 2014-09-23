/*
  This file is based on the "bodydata" example from qhttpserver.
 */


#include <unistd.h>

#include "s7.h"

#include "scheme.h"

#include <QCoreApplication>
#include <QRegExp>
#include <QStringList>
#include <QDebug>

#include <qhttpserver.h>
#include <qhttprequest.h>
#include <qhttpresponse.h>

#include "../common/nsmtracker.h"
#include "../common/OS_settings_proc.h"


#include "scheme_proc.h"


extern "C" {
  void init_radium_s7(s7_scheme *s7);
}


/*
Test:
curl -i -X POST -H "Content-Type: plain/text" -d '(display 50)' http://localhost:5080/user/asdf
*/

static Responder *current_responder;

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
    //QString bodyStart = tr("Hello: %1").arg(name);
    //resp->write(bodyStart.toUtf8());

    connect(req, SIGNAL(data(const QByteArray&)), this, SLOT(accumulate(const QByteArray&)));
    connect(req, SIGNAL(end()), this, SLOT(reply()));
    connect(m_resp, SIGNAL(done()), this, SLOT(deleteLater()));
}

Responder::~Responder()
{
}

static std::string input_code = "";
static s7_scheme *s7;


void Responder::accumulate(const QByteArray &data)
{
  input_code += data.data();
  printf("code so far: %s\n",input_code.c_str());
}


static void my_print(s7_scheme *sc, unsigned char c, s7_pointer port)
{
  printf("%c", c);
  if (current_responder != NULL) {
    current_responder->m_resp->write(QByteArray().append(c));
    current_responder->m_resp->flush();
    current_responder->m_resp->waitForBytesWritten();
  }
}

static s7_pointer our_sleep(s7_scheme *sc, s7_pointer args)
{
  /* slow down our infinite loop for demo purposes */
  sleep(1);
  return(s7_f(sc));
}

void Responder::reply()
{
  printf("Got code: -%s-\n",input_code.c_str());

  if (!is_balanced(input_code)) {
    m_resp->end(QByteArray(" ")); //-unbalanced, waiting for more input-"));
    input_code += "\n";
    return;
  }

  if(!is_not_white(input_code)) {
    m_resp->end(QByteArray(""));
    input_code = "";
    return;
  }

  // Code in here mostly copied from   https://ccrma.stanford.edu/software/snd/snd/s7.html#Cerrors


  // evaluate with error handling
  {
    int gc_loc = -1;

    /* trap error messages */
    s7_pointer old_port = s7_set_current_error_port(s7, s7_open_output_string(s7));
    if (old_port != s7_nil(s7))
      gc_loc = s7_gc_protect(s7, old_port);
        

    {
      // call eval
      current_responder = this;
      s7_pointer result = s7_eval_c_string(s7, input_code.c_str());
      current_responder = NULL;

      {
        const char *result_as_string = s7_object_to_c_string(s7, result);
        printf("result: %s\n",result_as_string);
        
        {
          QByteArray array("result: ");
          array.append(QByteArray(result_as_string));

          {
            /* look for error messages */
            const char *errmsg = s7_get_output_string(s7, s7_current_error_port(s7));
            
            /* if we got something, wrap it in "[]" */
            if ((errmsg) && (*errmsg)) {
              fprintf(stdout, "error message: [%s]", errmsg);     
              array.append(QByteArray(errmsg));
            }
          }
          
          m_resp->end(array);
        }
      }
    }

    s7_close_output_port(s7, s7_current_error_port(s7));
    s7_set_current_error_port(s7, old_port);

    if (gc_loc != -1)
      s7_gc_unprotect_at(s7, gc_loc);
  }

  input_code = "";
}

bool SCHEME_mousepress(int button, int x, int y){
  return s7_boolean(s7,
                    s7_call(s7, 
                            s7_name_to_value(s7, "radium-mouse-press"), // [1]
                            s7_list(s7,
                                    3,
                                    s7_make_integer(s7, button),
                                    s7_make_integer(s7, x),
                                    s7_make_integer(s7, y)
                                    )
                            )
                    );
  // [1] Not storing/reusing this value since 's7_name_to_value' is probably ligthing fast anyway, plus that it'll be possible to redefine radium-mouse-press from scheme this way.
}

bool SCHEME_mousemove(int button, int x, int y){
  return s7_boolean(s7,
                    s7_call(s7, 
                            s7_name_to_value(s7, "radium-mouse-move"), // [1]
                            s7_list(s7,
                                    3,
                                    s7_make_integer(s7, button),
                                    s7_make_integer(s7, x),
                                    s7_make_integer(s7, y)
                                    )
                            )
                    );
  // [1] Not storing/reusing this value since 's7_name_to_value' is probably ligthing fast anyway, plus that it'll be possible to redefine radium-mouse-press from scheme this way.
}

bool SCHEME_mouserelease(int button, int x, int y){
  return s7_boolean(s7,
                    s7_call(s7, 
                            s7_name_to_value(s7, "radium-mouse-release"), // [1]
                            s7_list(s7,
                                    3,
                                    s7_make_integer(s7, button),
                                    s7_make_integer(s7, x),
                                    s7_make_integer(s7, y)
                                    )
                            )
                    );
  // [1] Not storing/reusing this value since 's7_name_to_value' is probably ligthing fast anyway, plus that it'll be possible to redefine radium-mouse-press from scheme this way.
}



void SCHEME_start(){

  s7 = s7_init();
  if (s7==NULL)
    RError("Can't start s7 scheme");

  std::string os_path = OS_get_program_path();
  //printf("%s\n",os_path);

  s7_set_current_output_port(s7, s7_open_output_function(s7, my_print));
  s7_define_function(s7, "sleep", our_sleep, 0, 0, false, "(sleep) sleeps");

  s7_add_to_load_path(s7,(os_path+OS_get_directory_separator()+"packages"+OS_get_directory_separator()+"s7").c_str()); // bin/packages/s7
  s7_add_to_load_path(s7,(os_path+OS_get_directory_separator()+"scheme").c_str()); // bin/scheme

  s7_load(s7,"init.scm");

  init_radium_s7(s7);

  new BodyData();
}

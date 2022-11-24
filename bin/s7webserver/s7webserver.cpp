/*
  Written by Kjetil Matheussen: k.s.matheussen@notam02.no

  See s7webserver.h for instructions on how to compile the server.
*/


#include <unistd.h>

#include "s7.h"

#define COMPILING_S7WEBSERVER 1
#include "s7webserver.h"
#include "moc_s7webserver.cpp"

#include <QCoreApplication>

#include <qhttpserver.h>
#include <qhttprequest.h>
#include <qhttpresponse.h>


S7WebServer::S7WebServer(s7_scheme *s7, int portnumber)
  : s7(s7)
  , portnumber(portnumber)
  , verbose(false)
  , very_verbose(false)
{
  QHttpServer *server = new QHttpServer(this);
  connect(server, SIGNAL(newRequest(QHttpRequest*, QHttpResponse*)),
          this, SLOT(handleRequest(QHttpRequest*, QHttpResponse*)));
  
  has_started = server->listen(QHostAddress::Any, (quint16)portnumber);
}


void S7WebServer::handleRequest(QHttpRequest *req, QHttpResponse *resp)
{
  new S7WebServerResponder(this, req, resp);
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
static bool is_white(std::string str)
{
  for (unsigned i = 0; (i < str.size() && str[i] != ';'); i++)
    if (str[i] != ' ' && str[i] != '\n' && str[i] != '\t')
      return false;
  return true;
}


S7WebServerResponder::S7WebServerResponder(S7WebServer *s7webserver, QHttpRequest *request, QHttpResponse *response)
  : s7webserver(s7webserver)
  , request(request)
  , response(response)
{
  if (s7webserver->very_verbose)
    printf("responder got something\n");
  
  response->setHeader("Content-Type", "text/plain");
  response->setHeader("Access-Control-Allow-Origin", "*");

  response->writeHead(200);
  
  connect(request, SIGNAL(data(const QByteArray&)), this, SLOT(accumulate(const QByteArray&)));
  connect(request, SIGNAL(end()), this, SLOT(reply()));
  connect(response, SIGNAL(done()), this, SLOT(deleteLater()));
}


void S7WebServerResponder::accumulate(const QByteArray &data)
{
  s7webserver->input_code_so_far += data.data();
  if (s7webserver->very_verbose)
    printf("code so far: %s\n",s7webserver->input_code_so_far.c_str());
}


static void my_print(s7_scheme *sc, unsigned char c, s7_pointer port)
{
  
  S7WebServerResponder *current_responder = static_cast<S7WebServerResponder*>(s7_c_pointer(s7_name_to_value(sc, "s7webserver-current-responder")));

  if (current_responder==NULL || current_responder->s7webserver->verbose)
    putchar(c);

  if (current_responder != NULL) {
    current_responder->response->write(QByteArray().append(c));
    current_responder->response->flush();
    current_responder->response->waitForBytesWritten();
  }
}


static void set_s7webserver_current_responder(s7_scheme *sc, S7WebServerResponder *responder){
  s7_pointer current_responder = s7_make_c_pointer(sc, responder);
  s7_symbol_set_value(sc, s7_make_symbol(sc, "s7webserver-current-responder"), current_responder);
}


#include "../../embedded_scheme/s7_radium_proc.h"


void S7WebServerResponder::reply()
{
  if (s7webserver->very_verbose)
    printf("Code so far: -%s-\n",s7webserver->input_code_so_far.c_str());

  if (!is_balanced(s7webserver->input_code_so_far)) {
    response->end(QByteArray("")); //-unbalanced, waiting for more input-"));
    s7webserver->input_code_so_far += "\n";
    return;
  }

  if(is_white(s7webserver->input_code_so_far)) {
    response->end(QByteArray(""));
    s7webserver->input_code_so_far = "";
    return;
  }

  // Code in here mostly copied from   https://ccrma.stanford.edu/software/snd/snd/s7.html#Cerrors


  // evaluate with error handling
  {
    s7_scheme *s7 = s7webserver->s7;
    
    int gc_loc = -1;

    /* trap error messages */
    s7_pointer old_port = s7_set_current_error_port(s7, s7_open_output_string(s7));
    if (old_port != s7_nil(s7))
      gc_loc = s7_gc_protect(s7, old_port);
        

    {
      // call eval
      set_s7webserver_current_responder(s7, this);

      //s7_pointer result = s7_eval_c_string(s7, s7webserver->input_code_so_far.c_str());
      s7_pointer result = RADIUM_SCHEME_eval2(s7webserver->input_code_so_far.c_str());

      set_s7webserver_current_responder(s7, NULL);

      {
        const char *result_as_string = s7_object_to_c_string(s7, result);
        if (s7webserver->verbose)
          printf("result: %s\n",result_as_string);
        
        {
          QByteArray array("result: ");
          array.append(QByteArray(result_as_string));

          {
            /* look for error messages */
            const char *errmsg = s7_get_output_string(s7, s7_current_error_port(s7));
            
            /* if we got something, wrap it in "[]" */
            if ((errmsg) && (*errmsg)) {
              if (s7webserver->verbose)
                fprintf(stdout, "error message: [%s]\n", errmsg);
              array.append(QByteArray(errmsg));
            }
          }
          
          response->end(array);
        }
      }
    }

    s7_close_output_port(s7, s7_current_error_port(s7));
    s7_set_current_error_port(s7, old_port);

    if (gc_loc != -1)
      s7_gc_unprotect_at(s7, gc_loc);
  }

  s7webserver->input_code_so_far = "";
}


S7WebServer *s7webserver_create(s7_scheme *s7, int portnum, bool find_first_free_portnum){
  s7_define_variable(s7, "s7webserver-current-responder", s7_make_c_pointer(s7, NULL));
  
  s7_set_current_output_port(s7, s7_open_output_function(s7, my_print));

 try_again:
  
  S7WebServer *s7webserver = new S7WebServer(s7, portnum); 

  if (s7webserver->has_started==false) {
    delete s7webserver;
    s7webserver = NULL;
    
    if (find_first_free_portnum) {
      printf("s7webserver: Failed to open port %d, trying port %d instead.\n",portnum,portnum+1);
      portnum++;
      goto try_again;
    }
  }

  return s7webserver;
}


void s7webserver_set_verbose(S7WebServer *s7webserver, bool verbose) {
  s7webserver->verbose = verbose;
}


void s7webserver_set_very_verbose(S7WebServer *s7webserver, bool very_verbose) {
  s7webserver->very_verbose = very_verbose;
}

int s7webserver_get_portnumber(S7WebServer *s7webserver){
  return s7webserver->portnumber;
}

void s7webserver_delete(S7WebServer *s7webserver){
  delete s7webserver;
}


#ifdef WITH_MAIN
int main(int argc, char **argv){
  if (argc<=1) {
    printf("Usage: s7webserver portnumber [-verbose] [-very-verbose]\n");
    return -1;
  }

  int portnumber = atoi(argv[1]);

  bool verbose = argc>=3 && !strcmp(argv[2],"-verbose");  
  verbose = verbose || (argc>=4 && !strcmp(argv[3],"-verbose"));

  bool very_verbose = argc>=3 && !strcmp(argv[2],"-very-verbose");
  very_verbose = very_verbose || (argc>=4 && !strcmp(argv[3],"-very-verbose"));

  if (very_verbose)
    verbose = true;

  QCoreApplication app(argc, argv);

  s7_scheme *s7 = s7_init();
  if (s7==NULL) {
    fprintf(stderr, "Can't start s7 scheme");
    return -2;
  }
  
  s7webserver_t *s7webserver = s7webserver_create(s7, portnumber, true);
  if (s7webserver==NULL){
    fprintf(stderr, "Unable to start server. Port may be in use\n");
    return -3;
  }
  
  s7webserver_set_verbose(s7webserver, verbose);
  s7webserver_set_very_verbose(s7webserver, very_verbose);
  
  printf("S7 server started on port %d. (verbose=%s) (very_verbose=%s)\n", s7webserver->portnumber, s7webserver->verbose==true?"true":"false", s7webserver->very_verbose==true?"true":"false");

  app.exec();
}
#endif

/*
  This file is based on the "bodydata" example from qhttpserver.

Test:
curl -i -X POST -H "Content-Type: plain/text" -d '(display 50)' http://localhost:5080/user/asdf

 */


#include <unistd.h>

#include "s7.h"
#include "s7webserver/s7webserver.h"

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


static s7_scheme *s7;
static s7webserver_t *s7webserver;




bool SCHEME_mousepress(int button, float x, float y){

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

bool SCHEME_mousemove(int button, float x, float y){
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

bool SCHEME_mouserelease(int button, float x, float y){
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

  s7_add_to_load_path(s7,(os_path+OS_get_directory_separator()+"packages"+OS_get_directory_separator()+"s7").c_str()); // bin/packages/s7
  s7_add_to_load_path(s7,(os_path+OS_get_directory_separator()+"scheme").c_str()); // bin/scheme

  init_radium_s7(s7);

  s7_load(s7,"init.scm");

  s7webserver = s7webserver_create(s7, 5080, false);
  s7webserver_set_verbose(s7webserver, true);
}

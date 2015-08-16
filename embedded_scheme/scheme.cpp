/* Copyright 2014 Kjetil S. Matheussen

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.
-
This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. */



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
#include "../common/placement_proc.h"

#include "scheme_proc.h"

extern struct Root *root;


extern "C" {
  void init_radium_s7(s7_scheme *s7);
}


static s7_scheme *s7;
static s7webserver_t *s7webserver;

static s7_pointer place_to_ratio(const Place *p){
  R_ASSERT(p->dividor != 0);

  s7_Int a = p->line*p->dividor + p->counter;
  s7_Int b = p->dividor;
  s7_pointer ratio = s7_make_ratio(s7, a, b);
  
  //fprintf(stderr,"\n\n          a: %d, b: %d. is_number: %d, is_integer: %d, is_ratio: %d, is_real: %d\n\n\n", (int)a, (int)b, s7_is_number(ratio),s7_is_integer(ratio),s7_is_ratio(ratio),s7_is_real(ratio));  
  return ratio;
}

static Place *ratio_to_place(s7_pointer ratio){
  int num = s7_numerator(ratio);
  int den = s7_denominator(ratio);

  int lines = num/den;

  Place *ret = PlaceCreate(lines, num - (lines*den), den);
  PlaceHandleOverflow(ret);

  return ret;
}

static Place number_to_place(s7_pointer number){

  if (s7_is_ratio(number)) {
    int num = s7_numerator(number);
    int den = s7_denominator(number);

    int lines = num/den;
    
    Place ret = place(lines, num - (lines*den), den);
    PlaceHandleOverflow(&ret);
    
    return ret;
  }

  if (s7_is_integer(number))
    return place(s7_integer(number), 0, 1);

  RError("scheme.cpp/number_to_place: result was not ratio or integer. Returning 0. is_number: %d, is_integer: %d, is_ratio: %d, is_real: %d, value: %f, is_complex: %d, is_ulong: %d\n\n\n",s7_is_number(number),s7_is_integer(number),s7_is_ratio(number),s7_is_real(number),s7_number_to_real(s7,number),s7_is_complex(number),s7_is_ulong(number));  
  return place(0,0,1);
}

Place *PlaceScale(const Place *x, const Place *x1, const Place *x2, const Place *y1, const Place *y2) {
  static s7_pointer scheme_func = s7_name_to_value(s7, "scale");
  
  s7_pointer result = s7_call(s7,
                              scheme_func,
                              s7_list(s7,
                                      5,
                                      place_to_ratio(x),
                                      place_to_ratio(x1),
                                      place_to_ratio(x2),
                                      place_to_ratio(y1),
                                      place_to_ratio(y2)
                                      )
                              );

  if (s7_is_ratio(result))
    return ratio_to_place(result);
  else if (s7_is_integer(result))
    return PlaceCreate(s7_integer(result), 0, 1);
  else {
    RError("result was not ratio or integer. Returning 0");
    return PlaceCreate(0,0,1);
  }
}


bool quantitize_note(const struct Blocks *block, struct Notes *note) {
  s7_pointer scheme_func = s7_name_to_value(s7, "quantitize-note");
  
  Place last_place = p_Last_Pos(block);
  
  s7_pointer result = s7_call(s7,
                              scheme_func,
                              s7_list(s7,
                                      8,
                                      place_to_ratio(&note->l.p),
                                      place_to_ratio(&note->end),
                                      s7_make_ratio(s7, root->quantitize_options.quant.numerator, root->quantitize_options.quant.denominator),
                                      place_to_ratio(&last_place),
                                      s7_make_boolean(s7, root->quantitize_options.quantitize_start),
                                      s7_make_boolean(s7, root->quantitize_options.quantitize_end),
                                      s7_make_boolean(s7, root->quantitize_options.keep_note_length),
                                      s7_make_integer(s7, root->quantitize_options.type)
                                      )
                              );

  if (s7_is_boolean(result))
    return false;

  if (!s7_is_pair(result)) {
    RError("Unknown result after call to quantitize-note");
    return false;
  }

  s7_pointer new_start = s7_car(result);
  s7_pointer new_end = s7_cdr(result);

  note->l.p = number_to_place(new_start);
  note->end = number_to_place(new_end);

  return true;
}


static void place_operation_void_p1_p2(s7_pointer scheme_func, Place *p1,  const Place *p2){
  s7_pointer result = s7_call(s7,
                              scheme_func,
                              s7_list(s7,
                                      2,
                                      place_to_ratio(p1),
                                      place_to_ratio(p2)
                                      )
                              );
  
  if (s7_is_ratio(result))
    PlaceCopy(p1, ratio_to_place(result));
  else if (s7_is_integer(result))
    PlaceCopy(p1, PlaceCreate(s7_integer(result), 0, 1));
  else {
    RError("result was not ratio or integer. Returning 0");
    PlaceCopy(p1, PlaceCreate(0,0,1));
  }
}
                            
void PlaceAdd(Place *p1,  const Place *p2){
  static s7_pointer scheme_func = s7_name_to_value(s7, "+");
  place_operation_void_p1_p2(scheme_func, p1,p2);
}

void PlaceSub(Place *p1,  const Place *p2){
  static s7_pointer scheme_func = s7_name_to_value(s7, "-");
  place_operation_void_p1_p2(scheme_func, p1,p2);
}

void PlaceMul(Place *p1,  const Place *p2){
  static s7_pointer scheme_func = s7_name_to_value(s7, "*");
  place_operation_void_p1_p2(scheme_func, p1,p2);
}

void PlaceDiv(Place *p1,  const Place *p2){
  static s7_pointer scheme_func = s7_name_to_value(s7, "/");
  place_operation_void_p1_p2(scheme_func, p1,p2);
}


static Place place_operation_place_p1_p2(s7_pointer scheme_func, const Place p1,  const Place p2){
  s7_pointer result = s7_call(s7,
                              scheme_func,
                              s7_list(s7,
                                      2,
                                      place_to_ratio(&p1),
                                      place_to_ratio(&p2)
                                      )
                              );

  return number_to_place(result);
}

Place p_Add(const Place p1, const Place p2){
  //PrintPlace("p1: ",&p1);
  //PrintPlace("p2: ",&p2);
  static s7_pointer scheme_func = s7_name_to_value(s7, "+");
  return place_operation_place_p1_p2(scheme_func, p1,p2);
}

Place p_Sub(const Place p1, const Place p2){
  static s7_pointer scheme_func = s7_name_to_value(s7, "-");
  return place_operation_place_p1_p2(scheme_func, p1,p2);
}

Place p_Mul(const Place p1, const Place p2){
  static s7_pointer scheme_func = s7_name_to_value(s7, "*");
  return place_operation_place_p1_p2(scheme_func, p1,p2);
}

Place p_Div(const Place p1, const Place p2){
  static s7_pointer scheme_func = s7_name_to_value(s7, "/");
  return place_operation_place_p1_p2(scheme_func, p1,p2);
}

Place p_Quantitize(const Place p, const Place q){
  static s7_pointer scheme_func = s7_name_to_value(s7, "quantitize");
  return place_operation_place_p1_p2(scheme_func, p, q);
}


bool SCHEME_mousepress(int button, float x, float y){

  return s7_boolean(s7,
                    s7_call(s7, 
                            s7_name_to_value(s7, "radium-mouse-press"), // [1]
                            s7_list(s7,
                                    3,
                                    s7_make_integer(s7, button),
                                    s7_make_real(s7, x),
                                    s7_make_real(s7, y)
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
                                    s7_make_real(s7, x),
                                    s7_make_real(s7, y)
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
                                    s7_make_real(s7, x),
                                    s7_make_real(s7, y)
                                    )
                            )
                    );
  // [1] Not storing/reusing this value since 's7_name_to_value' is probably ligthing fast anyway, plus that it'll be possible to redefine radium-mouse-press from scheme this way.
}

void SCHEME_eval(const char *code){
  s7_eval_c_string(s7, code);
}

int SCHEME_get_webserver_port(void){
  return s7webserver_get_portnumber(s7webserver);
}

void SCHEME_start(void){

  s7 = s7_init();
  if (s7==NULL)
    RError("Can't start s7 scheme");

  std::string os_path = ""; //OS_get_program_path() + OS_get_directory_separator();
  //printf("%s\n",os_path);

  s7_add_to_load_path(s7,(os_path+"packages"+OS_get_directory_separator()+"s7").c_str()); // bin/packages/s7 . No solution to utf-8 here. s7_add_to_load_path takes char* only.
  s7_add_to_load_path(s7,(os_path+"scheme").c_str()); // bin/scheme

  init_radium_s7(s7);

  s7_load(s7,"init.scm");

  s7webserver = s7webserver_create(s7, 5080, true);
  s7webserver_set_verbose(s7webserver, true);
}

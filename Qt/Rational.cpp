
#include "../common/nsmtracker.h"

#include "Rational.h"

Place get_rational_from_string(const char *string){
  Place p;
  Rational rational = create_rational_from_string(string);

  p.line = 0;
  p.counter = rational._numerator;
  p.dividor = rational._denominator;

  return p;
}

char* get_string_from_rational(const Place *r){
  Rational rational(r->counter, r->dividor);

  return talloc_strdup(rational.toString().toUtf8().constData());
}


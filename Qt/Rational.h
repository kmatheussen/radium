
#ifndef QT_RATIONAL_H
#define QT_RATIONAL_H

#ifdef __cplusplus

#include <QString>
#include <QStringList>

namespace{
  
struct Rational {
  
  int _numerator;
  int _denominator;
  
  Rational(const int numerator, const int denominator)
    : _numerator(numerator)
    , _denominator(denominator)
  {}

  Rational(const Ratio ratio)
    : _numerator((int)ratio.numerator)
    , _denominator((int)ratio.denominator)
  {}

  Ratio get_ratio(void) const {
    return make_ratio(_numerator, _denominator);
  }

  bool is_valid(void) const {
    return _denominator != 0;
  }

  QString toString(void) const {
    return QString::number(_numerator) + "/" + QString::number(_denominator);
  }
  
  Rational down(void) const {
    int numerator = _numerator;
    int denominator = _denominator;
    
    if (numerator!=1 && denominator!=1){
      numerator=1;
      denominator=1;
    }
    
    if (numerator==1 && denominator==1)
      return Rational(1,2);

    if (numerator==1)
      return Rational(1, denominator+1);

    else
      return Rational(numerator-1, 1);
  }

  Rational up(void) const {
    int numerator = _numerator;
    int denominator = _denominator;

    if (numerator!=1 && denominator!=1){
      numerator=1;
      denominator=1;
    }
      
    if (numerator==1 && denominator==1)
      return Rational(2,1);

    if (numerator==1)
      return Rational(numerator, denominator-1);

    else
      return Rational(numerator+1, 1);
  }

  const Rational upNumerator(void) const {
    return Rational(_numerator+1, _denominator);
  }

  const Rational downNumerator(void) const {
    return Rational(_numerator-1, _denominator);
  }

  const Rational upDenominator(void) const {
    return Rational(_numerator, _denominator+1);
  }

  const Rational downDenominator(void) const {
    if (_denominator==1)
      return Rational(_numerator, _denominator);
    else
      return Rational(_numerator, _denominator-1);
  }
};
}

static inline Rational create_rational_from_string(QString string){
  QStringList splitted = string.split("/", QString::SkipEmptyParts);

  if (splitted.size() >= 2) {
      
    QString a = splitted[0];
    QString b = splitted[1];

    int numerator = a.toInt();
    int denominator = b.toInt();

    return Rational(numerator, denominator);
    
  } else {
    
    bool ok;
    int value = string.toInt(&ok);

    if (ok)
      return Rational(value,1);
    else
      return Rational(0,0);
    
  }
  
}
#endif // __cplusplus

// line is always 0
// dividor is 0 if error, or the input denominator was actually 0
// counter is undefined if error
extern LANGSPEC Place get_rational_from_string(const char *string);
extern LANGSPEC char *get_string_from_rational(const Place *r);


#endif // QT_RATIONAL_H

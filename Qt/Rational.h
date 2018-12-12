
#ifndef QT_RATIONAL_H
#define QT_RATIONAL_H

#ifdef __cplusplus

#include <QString>
#include <QStringList>

extern StaticRatio STATIC_RATIO_from_string(QString string);
  
namespace{
  
struct Rational {
  
  int _numerator;
  int _denominator;
  
  Rational(const int numerator, const int denominator)
    : _numerator(numerator)
    , _denominator(denominator)
  {}

  Rational(const StaticRatio ratio)
    : _numerator((int)ratio.numerator)
    , _denominator((int)ratio.denominator)
  {}
  
  Rational(QString string)
    : Rational(STATIC_RATIO_from_string(string)) // C++11 FTW!
  {             
  }
  
  StaticRatio get_static_ratio(void) const {
    return make_static_ratio(_numerator, _denominator);
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

#endif // __cplusplus



#endif // QT_RATIONAL_H


#ifndef QT_RATIONAL_H
#define QT_RATIONAL_H

#ifdef __cplusplus

struct Rational {
  int numerator;
  int denominator;
    
  Rational(int numerator, int denominator)
    : numerator(numerator)
    , denominator(denominator)
  {}

  bool is_valid(void){
    return denominator != 0;
  }

  QString toString(void){
    return QString::number(numerator) + "/" + QString::number(denominator);
  }
  
  Rational down(void){
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

  Rational up(void){
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
};

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


#endif // QT_RATIONAL_H

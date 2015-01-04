
#ifndef QT_RATIONAL_H
#define QT_RATIONAL_H

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
    
  } else

    return Rational(0,0);

  
}

#endif // QT_RATIONAL_H

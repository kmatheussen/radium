
#include <math.h>

#include <QString>

#include "../common/nsmtracker.h"
#include "../common/OS_string_proc.h"


#include "Rational.h"



Ratio RATIO_from_string(QString string){
  
  QStringList splitted = string.split("/", QString::SkipEmptyParts);

  if (splitted.size() >= 2) {
      
    QString a = splitted[0];
    QString b = splitted[1];

    bool ok;
    
    int numerator = a.toDouble(&ok);
    if (!ok)
      numerator = a.toInt(&ok);
    
    int denominator = b.toDouble(&ok);
    if (!ok)
      denominator = b.toInt(&ok);
    if (!ok)
      denominator = 1;
    
    printf("A  string: %s. a: %s. b: %s.  Num: %d.  Den: %d\n", string.toUtf8().constData(), a.toUtf8().constData(), b.toUtf8().constData(), numerator,  denominator);

    if (denominator==0)
      denominator=1;
    
    return make_ratio(numerator, denominator);
    
  } else {
    
    bool ok;
    {
      double value = string.toDouble(&ok);

      if (ok && floor(value)!=value){
        printf("B  string: %s. value: %f. Ok: %d\n", string.toUtf8().constData(), value, ok);
        return make_ratio(value*1000, 1000);
      }
    }
    
    int value = string.toInt(&ok);
    if (!ok)
      value = 1;

    printf("B  string: %s. value: %d. Ok: %d\n", string.toUtf8().constData(), value, ok);

    return make_ratio(value,1);
  }

  
}


QString RATIO_as_qstring(const Ratio ratio){
  return Rational(ratio).toString();
}

wchar_t *RATIO_as_string(const Ratio ratio){
  return STRING_create(RATIO_as_qstring(ratio));
}


Ratio RATIO_from_string(const wchar_t *wstring){
  return RATIO_from_string(STRING_get_qstring(wstring));
}

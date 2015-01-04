#ifndef QT_LZQLINEEDIT_H
#define QT_LZQLINEEDIT_H

#include <QLineEdit>

#include "Rational.h"

class LZFocusSnifferQLineEdit : public FocusSnifferQLineEdit {

public:
  
 LZFocusSnifferQLineEdit(QWidget *parent)
   : FocusSnifferQLineEdit(parent)
  {
  }

  virtual void wheelEvent(QWheelEvent *qwheelevent) {
    printf("Got lz wheel event\n");
    
  }
  
};


class GridFocusSnifferQLineEdit : public FocusSnifferQLineEdit {

public:
  
 GridFocusSnifferQLineEdit(QWidget *parent)
   : FocusSnifferQLineEdit(parent)
  {
  }

  void pushValuesToRoot(Rational rational){
    if (rational.is_valid() && rational.numerator>0 && rational.denominator>0) {
      root->grid_numerator = rational.numerator;
      root->grid_denominator = rational.denominator;
    }
  }
  
  virtual void wheelEvent(QWheelEvent *qwheelevent) {
    printf("Got grid wheel event\n");
    
    Rational ratio(root->grid_numerator, root->grid_denominator);
  
    if (qwheelevent->delta()<0)
      ratio = ratio.down();
    else
      ratio = ratio.up();

    pushValuesToRoot(ratio);
    setText(ratio.toString());
  }
  
};


#endif // QT_LZQLINEEDIT_H

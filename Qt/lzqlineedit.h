#ifndef QT_LZQLINEEDIT_H
#define QT_LZQLINEEDIT_H

#include <QLineEdit>

#include "Rational.h"
#include "../common/reallines_proc.h"



class LZFocusSnifferQLineEdit : public FocusSnifferQLineEdit {

public:
  
 LZFocusSnifferQLineEdit(QWidget *parent)
   : FocusSnifferQLineEdit(parent)
  {
  }

  Rational getRational(const struct WBlocks *wblock){
    if (wblock->num_expand_lines > 0)
      return Rational(wblock->num_expand_lines, 1);
    else
      return Rational(1, -wblock->num_expand_lines);
  }

  int getNumExpandLinesFromRational(Rational rational){
    if (!rational.is_valid())
      return 0;
    
    if (rational.numerator==0)      
      return 0;

    if (rational.numerator!=1 && rational.denominator!=1)
      return 0;
  
    if (rational.numerator==1)
      return -rational.denominator;

    //if (rational.denominator==1) // always true.
    return rational.numerator;
  }
                                    
  virtual void wheelEvent(QWheelEvent *qwheelevent) {
    printf("Got lz wheel event\n");

    struct Tracker_Windows *window = root->song->tracker_windows;
    struct WBlocks *wblock = window->wblock;

    Rational ratio = getRational(wblock);

    if (qwheelevent->delta()<0)
      ratio = ratio.down();
    else
      ratio = ratio.up();

    int value = getNumExpandLinesFromRational(ratio);
    LineZoomBlock(window,wblock,value);
    window->must_redraw = true;
    
    setText(ratio.toString());
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


class QuantizationFocusSnifferQLineEdit : public FocusSnifferQLineEdit {

public:
  
 QuantizationFocusSnifferQLineEdit(QWidget *parent)
   : FocusSnifferQLineEdit(parent)
  {
  }

  void pushValuesToRoot(Rational rational){
    if (rational.is_valid() && rational.numerator>0 && rational.denominator>0) {
      root->quantitize_numerator = rational.numerator;
      root->quantitize_denominator = rational.denominator;
    }
  }
  
  virtual void wheelEvent(QWheelEvent *qwheelevent) {
    printf("Got quantization wheel event\n");
    
    Rational ratio(root->quantitize_numerator, root->quantitize_denominator);
  
    if (qwheelevent->delta()<0)
      ratio = ratio.down();
    else
      ratio = ratio.up();

    pushValuesToRoot(ratio);
    setText(ratio.toString());
  }
  
};


class SignatureFocusSnifferQLineEdit : public FocusSnifferQLineEdit {

public:
  
 SignatureFocusSnifferQLineEdit(QWidget *parent)
   : FocusSnifferQLineEdit(parent)
  {
  }

  void pushValuesToRoot(Rational rational){
    if (rational.is_valid() && rational.numerator>0 && rational.denominator>0) {
      setSignature(rational.numerator, rational.denominator);
    }
  }
  
  virtual void wheelEvent(QWheelEvent *qwheelevent) {
    printf("Got signature wheel event\n");
    
    Rational ratio(root->signature.numerator, root->signature.denominator);

    //printf("      bef2: %s.",ratio.toString().toUtf8().constData());
    
    if (qwheelevent->delta()<0 && ratio.numerator>1)
      ratio = Rational(ratio.numerator-1, ratio.denominator);

    else if(qwheelevent->delta()>0)
      ratio = Rational(ratio.numerator+1, ratio.denominator);

    //printf(" aft: %s.",ratio.toString().toUtf8().constData());
        
    pushValuesToRoot(ratio);
    setText(ratio.toString());
  }
  
};



#endif // QT_LZQLINEEDIT_H


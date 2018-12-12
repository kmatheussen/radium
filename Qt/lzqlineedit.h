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

  const Rational getRational(const struct WBlocks *wblock){
    if (wblock->num_expand_lines > 0)
      return Rational(wblock->num_expand_lines, 1);
    else
      return Rational(1, -wblock->num_expand_lines);
  }

  int getNumExpandLinesFromRational(const Rational &rational){
    if (!rational.is_valid())
      return 0;
    
    if (rational._numerator==0)      
      return 0;

    if (rational._numerator!=1 && rational._denominator!=1)
      return 0;
  
    if (rational._numerator==1)
      return -rational._denominator;

    //if (rational.denominator==1) // always true.
    return rational._numerator;
  }
                                    
  void wheelEvent(QWheelEvent *qwheelevent) override {
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
    
    qwheelevent->accept();
  }
  
};

namespace{
class RatioSnifferQLineEdit : public FocusSnifferQLineEdit {

public:

  bool _wheelMainlyChangesNumerator;
  bool _wheelDecrasesDenominatorIfNumeratorIsOne;

  RatioSnifferQLineEdit(QWidget *parent, bool wheelMainlyChangesNumerator = true, bool wheelDecrasesDenominatorIfNumeratorIsOne = true)
   : FocusSnifferQLineEdit(parent)
   , _wheelMainlyChangesNumerator(wheelMainlyChangesNumerator)
   , _wheelDecrasesDenominatorIfNumeratorIsOne(wheelDecrasesDenominatorIfNumeratorIsOne)
     //, ratio(1,1)
  {
    setText("1/1");
  }

  StaticRatio get_ratio(void){
    return STATIC_RATIO_from_string(text());
  }
    
  void wheelEvent(QWheelEvent *qwheelevent) override {

    Rational ratio(text());
    //printf("  text: %s, ratio: %s\n", text().toUtf8().constData(), ratio.toString().toUtf8().constData());

    if (qwheelevent->modifiers() & Qt::ControlModifier) {

      if (qwheelevent->delta()<0 && ratio._denominator >= 2)
        ratio = ratio.downDenominator();
      
      else if (qwheelevent->delta()>0)
        ratio = ratio.upDenominator();
      
    } else if (_wheelMainlyChangesNumerator){
      
      if (qwheelevent->delta()<0 && ratio._numerator>1)
        ratio = ratio.downNumerator();

      else if (qwheelevent->delta()<0 && _wheelDecrasesDenominatorIfNumeratorIsOne)
        ratio = ratio.upDenominator();
        
      else if(qwheelevent->delta()>0)
        ratio = ratio.upNumerator();

    } else {
      
      if (qwheelevent->delta()<0)
        ratio = ratio.down();
      else
        ratio = ratio.up();
      
    }

    //printf("  ratio finished: %s\n\n", ratio.toString().toUtf8().constData());
    
    setText(ratio.toString());
    emit editingFinished();
    
    qwheelevent->accept();
  }
};
}


class GridFocusSnifferQLineEdit : public FocusSnifferQLineEdit {

public:
  
 GridFocusSnifferQLineEdit(QWidget *parent)
   : FocusSnifferQLineEdit(parent)
  {
  }

  void pushValuesToRoot(Rational rational){
    if (rational.is_valid() && rational._numerator>0 && rational._denominator>0) {
      root->grid_numerator = rational._numerator;
      root->grid_denominator = rational._denominator;
    }
  }
  
  void wheelEvent(QWheelEvent *qwheelevent) override {
    printf("Got grid wheel event\n");
    
    Rational ratio(root->grid_numerator, root->grid_denominator);
    
    if (qwheelevent->delta()<0)
      ratio = ratio.down();
    else
      ratio = ratio.up();

    pushValuesToRoot(ratio);
    setText(ratio.toString());
    
    qwheelevent->accept();
  }
  
};


class QuantizationFocusSnifferQLineEdit : public FocusSnifferQLineEdit {

public:
  
 QuantizationFocusSnifferQLineEdit(QWidget *parent)
   : FocusSnifferQLineEdit(parent)
  {
  }

  void pushValuesToRoot(Rational rational){
    if (rational.is_valid() && rational._numerator>0 && rational._denominator>0) {
      root->quantitize_options.quant.numerator = rational._numerator;
      root->quantitize_options.quant.denominator = rational._denominator;
    }
  }
  
  void wheelEvent(QWheelEvent *qwheelevent) override {
    printf("Got quantization wheel event\n");
    
    Rational ratio((int)root->quantitize_options.quant.numerator, (int)root->quantitize_options.quant.denominator);
  
    if (qwheelevent->delta()<0)
      ratio = ratio.down();
    else
      ratio = ratio.up();

    pushValuesToRoot(ratio);
    setText(ratio.toString());

    qwheelevent->accept();
  }
  
};


class NoteNameFocusSnifferQLineEdit : public FocusSnifferQLineEdit {

public:
  
 NoteNameFocusSnifferQLineEdit(QWidget *parent)
   : FocusSnifferQLineEdit(parent)
  {
  }

  void wheelEvent(QWheelEvent *qwheelevent) override {
    //printf("Got note name wheel event\n");

    int keynum = getNoteNameValue((char*)text().toUtf8().constData());
    if (keynum!=-1) {
      if (qwheelevent->delta()<0)
        keynum--;
      else
        keynum++;
    }

    setText(getNoteName3(keynum));
    editingFinished();

    qwheelevent->accept();
  }
  
};


class SignatureFocusSnifferQLineEdit : public FocusSnifferQLineEdit {

public:
  
 SignatureFocusSnifferQLineEdit(QWidget *parent)
   : FocusSnifferQLineEdit(parent)
  {
  }

  void pushValuesToRoot(Rational rational){
    if (rational.is_valid() && rational._numerator>0 && rational._denominator>0) {
      setMainSignature(rational._numerator, rational._denominator);
    }
  }
  
  void wheelEvent(QWheelEvent *qwheelevent) override {
    printf("Got signature wheel event\n");
    
    Rational ratio((int)root->signature.numerator, (int)root->signature.denominator);

    //printf("      bef2: %s.",ratio.toString().toUtf8().constData());
    
    if (qwheelevent->delta()<0 && ratio._numerator>1)
      ratio = ratio.downNumerator();

    else if(qwheelevent->delta()>0)
      ratio = ratio.upNumerator();

    //printf(" aft: %s.",ratio.toString().toUtf8().constData());
        
    pushValuesToRoot(ratio);
    setText(ratio.toString());

    qwheelevent->accept();
  }
  
};



#endif // QT_LZQLINEEDIT_H


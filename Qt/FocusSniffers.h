/* Copyright 2012 Kjetil S. Matheussen

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. */


#include <math.h>


#include <QSpinBox>
#include <QLineEdit>
#include <QTextEdit>
#include <QListWidget>
#include <QTableWidgetItem>
#include <QKeyEvent>

#include "helpers.h"

#include "../OpenGL/Widget_proc.h"


#define MakeFocusSnifferClass(Class)                                    \
  class FocusSniffer##Class : public GL_PauseCaller, public Class {      \
  public:                                                               \
  bool dontsniff;                                                       \
  FocusSniffer##Class(QWidget *parent = NULL, const char *name = "gakk")       \
  : Class(parent),dontsniff(false)                                     \
    {                                                                   \
    }                                                                   \
  void focusInEvent ( QFocusEvent *e ){                                 \
    printf("got in\n");                                                 \
    if(dontsniff==false)                                                \
      obtain_keyboard_focus();                                          \
    GL_lock();                                                          \
    Class::focusInEvent(e);                                             \
    GL_unlock();                                                        \
  }                                                                     \
  void focusOutEvent ( QFocusEvent *e ){                                \
    if(dontsniff==false) {                                              \
      release_keyboard_focus();                                         \
    }                                                                   \
    GL_lock();                                                          \
    Class::focusOutEvent(e);                                            \
    GL_unlock();                                                        \
  }                                                                     \
  void hideEvent (QHideEvent * event){                                  \
    if(dontsniff==false) {                                              \
      release_keyboard_focus();                                         \
    }                                                                   \
    Class::hideEvent(event);                                            \
  }                                                                     \
  void keyPressEvent ( QKeyEvent * event ){                             \
    if(event->key()==Qt::Key_Escape){                                   \
      GL_lock();                                                        \
      clearFocus();                                                     \
      GL_unlock();                                                      \
    }                                                                   \
    Class::keyPressEvent(event);                                        \
  }                                                                     \
  }                                                    


//MakeFocusSnifferClass(QSpinBox);
//MakeFocusSnifferClass(QDoubleSpinBox);
MakeFocusSnifferClass(QLineEdit);
//MakeFocusSnifferClass(QTextEdit);
//MakeFocusSnifferClass(QListWidget);

#include <Qsci/qscilexer.h>
#include <Qsci/qsciscintilla.h>  // <--- Much trouble. Any qt4 alternatives?

class FocusSnifferQsciScintilla : public GL_PauseCaller, public QsciScintilla{
  public:                                                               
  bool dontsniff;
  int zoomguess;
  FocusSnifferQsciScintilla(QWidget *parent, const char *name = "gakk")  
    : QsciScintilla(parent),dontsniff(false)
    , zoomguess(0)
    {                                                                   
    }

  // Why doesn't qscintilla do this automatically? And why can't qscintilla give me a method to find margin font (or even the editor font) so that I can do this manually without guesssing font width?
  void calculateMarginWidth(){
    QFont font = lexer()->defaultFont();
    int points = font.pointSize()+zoomguess;
    if (points < 1)
      points = 1;
    if (points > 40)
      points = 40;
    font.setPointSize(points);
    QFontMetrics fm(font);
    QString test = "";
    int num_lines_log10 = ceil(log10(lines()));
    
    for(int i = 0 ; i < R_MAX(3, num_lines_log10) ; i++)
      test += "9";
    
    double width = fm.width(test+" ");
    //printf(" width: %f %d %d\n", width, font.pixelSize(), font.pointSize());
    setMarginWidth(1, ceil(width));
  }
      
  void wheelEvent(QWheelEvent *event){                  
    if (event->modifiers() & Qt::ControlModifier){                
      if (event->delta() > 0){                                     
        zoomIn(1);
        zoomguess++;
      }else{
        zoomOut(1);
        zoomguess--;
      }
      if (zoomguess < -10)
        zoomguess = -10;
      if (zoomguess > 30)
        zoomguess = 30;
      
      calculateMarginWidth();
    } else {
      QsciScintilla::wheelEvent(event);
      //set_editor_focus();
    }
  }
  void keyPressEvent ( QKeyEvent * event ){
    if(event->key()==Qt::Key_Escape)
      set_editor_focus();
    else
      QsciScintilla::keyPressEvent(event);
  }
  void focusInEvent ( QFocusEvent *e ){                                 
    printf("Got focusInEvent\n");
    if(dontsniff==false)
      obtain_keyboard_focus();
    GL_lock();
    QsciScintilla::focusInEvent(e);                                             
    GL_unlock();
  }                                                                     
  void focusOutEvent ( QFocusEvent *e ){                                
    printf("Got focusOutEvent\n");
    if(dontsniff==false) {
      release_keyboard_focus();
    }                                                                   
    GL_lock();
    QsciScintilla::focusOutEvent(e);                                            
    GL_unlock();
  }                                                                     
  void hideEvent (QHideEvent * event){
    if(dontsniff==false) {
      release_keyboard_focus();
    }                                 
    QsciScintilla::hideEvent(event);          
  }
};

class FocusSnifferQTextEdit : public GL_PauseCaller, public QTextEdit{
  public:                                                               
  bool dontsniff;                                                       
  FocusSnifferQTextEdit(QWidget *parent = NULL, const char *name = "gakk")  
   : QTextEdit(parent),dontsniff(false)                                     
    {                                                                   
    }
  void wheelEvent(QWheelEvent *event) override {                  
    if (event->modifiers() & Qt::ControlModifier){                
      if (event->delta() > 0)                                     
        zoomIn(1);                                                      
      else                                                              
        zoomOut(1);                                                     
    } else {
      QTextEdit::wheelEvent(event);
      //set_editor_focus();
    }
  }
  void keyPressEvent ( QKeyEvent * event ) override {
    if(event->key()==Qt::Key_Escape)
      set_editor_focus();
    else
      QTextEdit::keyPressEvent(event);
  }
  void focusInEvent ( QFocusEvent *e ) override {                                 
    printf("Got focusInEvent\n");
    if(dontsniff==false)
      obtain_keyboard_focus();
    GL_lock();
    QTextEdit::focusInEvent(e);                                             
    GL_unlock();
  }                                                                     
  void focusOutEvent ( QFocusEvent *e ) override {                                
    printf("Got focusOutEvent\n");
    if(dontsniff==false) {
      release_keyboard_focus();
    }                                                                   
    GL_lock();
    QTextEdit::focusOutEvent(e);                                            
    GL_unlock();
  }                                                                     
  void hideEvent (QHideEvent * event) override {
    if(dontsniff==false) {
      release_keyboard_focus();
    }                                 
    QTextEdit::hideEvent(event);          
  }                                   
};

class FocusSnifferQSpinBox : public GL_PauseCaller, public QSpinBox{
  public:                                                               
  bool dontsniff;                                                       
 FocusSnifferQSpinBox(QWidget *parent = NULL, const char *name = "gakk")  
   : QSpinBox(parent),dontsniff(false)                                     
    {                                                                   
    }                                                                   
  void focusInEvent ( QFocusEvent *e ){                                 
    printf("Got focusInEvent\n");
    if(dontsniff==false)
      obtain_keyboard_focus();
    GL_lock();
    QSpinBox::focusInEvent(e);                                             
    GL_unlock();
  }                                                                     
  void focusOutEvent ( QFocusEvent *e ){                                
    printf("Got focusOutEvent\n");
    if(dontsniff==false) {
      release_keyboard_focus();
    }                                                                   
    GL_lock();
    QSpinBox::focusOutEvent(e);                                            
    GL_unlock();
  }                                                                     
  void hideEvent (QHideEvent * event){
    if(dontsniff==false) {
      release_keyboard_focus();
    }                                 
    QSpinBox::hideEvent(event);          
  }                                   
  void keyPressEvent ( QKeyEvent * event ){                             
    if(event->key()==Qt::Key_Escape){                                   
      GL_lock();                                                        
      clearFocus();                                                     
      GL_unlock();                                                      
    }                                                                   
    QSpinBox::keyPressEvent(event);                                        
  }                                                                     
  void 	wheelEvent ( QWheelEvent * event ){
    printf("Got wheel event\n");
    QSpinBox::wheelEvent(event);
    set_editor_focus(); 
  }
};

class FocusSnifferQDoubleSpinBox : public GL_PauseCaller, public QDoubleSpinBox{
  public:                                                               
  bool dontsniff;                                                       
  FocusSnifferQDoubleSpinBox(QWidget *parent = NULL, const char *name = "gakk")  
   : QDoubleSpinBox(parent),dontsniff(false)                                     
  {                                                                   
  }                                                                   
  void focusInEvent ( QFocusEvent *e ){                                 
    printf("Got focusInEvent\n");
    if(dontsniff==false)
      obtain_keyboard_focus();
    GL_lock();
    QDoubleSpinBox::focusInEvent(e);                                             
    GL_unlock();
  }                                                                     
  void focusOutEvent ( QFocusEvent *e ){                                
    printf("Got focusOutEvent\n");
    if(dontsniff==false) {
      release_keyboard_focus();
    }                                                                   
    GL_lock();
    QDoubleSpinBox::focusOutEvent(e);                                            
    GL_unlock();
  }                                                                     
  void hideEvent ( QHideEvent *e ){                                
    //printf("Got hideEvent\n");
    if(dontsniff==false) {
      release_keyboard_focus();
    }                                                                   
    QDoubleSpinBox::hideEvent(e);                                            
  }                                                                     
  void keyPressEvent ( QKeyEvent * event ){                             
    if(event->key()==Qt::Key_Escape){                                   
      GL_lock();                                                        
      clearFocus();                                                     
      GL_unlock();                                                      
    }                                                                   
    QDoubleSpinBox::keyPressEvent(event);                                        
  }                                                                     
  void 	wheelEvent ( QWheelEvent * event ){
    printf("Got wheel event\n");
    QDoubleSpinBox::wheelEvent(event);
    set_editor_focus(); 
  }
};

class FocusSnifferQListWidget : public GL_PauseCaller, public QListWidget{
 public:                                                               
  bool dontsniff;                                                       
 FocusSnifferQListWidget(QWidget *parent, const char *name = "gakk")  
   : QListWidget(parent),dontsniff(false)                                     
    {                                                                   
    }                                                                   
  void focusInEvent ( QFocusEvent *e ){                                 
    printf("Got focusInEvent\n");
    if(dontsniff==false)
      obtain_keyboard_focus();
    GL_lock();
    QListWidget::focusInEvent(e);                                             
    GL_unlock();
  }                                                                     
  void focusOutEvent ( QFocusEvent *e ){                                
    printf("Got focusOutEvent\n");
    if(dontsniff==false) {
      release_keyboard_focus();
    }                                                                   
    GL_lock();
    QListWidget::focusOutEvent(e);                                            
    GL_unlock();
  }                                                                     
  void hideEvent ( QHideEvent *e ){                                
    //printf("Got hideEvent\n");
    if(dontsniff==false) {
      release_keyboard_focus();
    }                                                                   
    QListWidget::hideEvent(e);                                            
  }                                                                     
  void keyPressEvent ( QKeyEvent * event ){                             
    if(event->key()==Qt::Key_Escape){                                   
      GL_lock();                                                        
      clearFocus();                                                     
      GL_unlock();                                                      
    }                                                                   
    QListWidget::keyPressEvent(event);                                        
  }                                                                     
};


#if 0
class FocusSnifferQTableWidget : public QTableWidget{
  public:                                                               
  bool dontsniff;                                                       
 FocusSnifferQTableWidget(QWidget *parent, const char *name = "gakk")  
   : QTableWidget(parent),dontsniff(false)                                     
    {                                                                   
    }                                                                   
  void focusInEvent ( QFocusEvent *e ){                                 
    printf("Got focusInEvent\n");
    if(dontsniff==false)
      obtain_keyboard_focus();
    GL_lock();
    QTableWidget::focusInEvent(e);                                             
    GL_unlock();
  }                                                                     
  void focusOutEvent ( QFocusEvent *e ){                                
    printf("Got focusOutEvent\n");
    if(dontsniff==false) {
      release_keyboard_focus();
    }    
    GL_lock();                                                               
    QTableWidget::focusOutEvent(e);                                            
    GL_unlock();
  }                                                                     
};
                                                    

#define FocusSnifferQTableWidgetItem QTableWidgetItem

#endif


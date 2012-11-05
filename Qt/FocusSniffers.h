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



#include <QSpinBox>
#include <QLineEdit>
#include <QListWidget>
#include <QTableWidgetItem>
#include <QKeyEvent>

extern int num_users_of_keyboard;

extern void set_editor_focus(void);


#define MakeFocusSnifferClass(Class)                                    \
  class FocusSniffer##Class : public Class {                            \
  public:                                                               \
  bool dontsniff;                                                       \
  FocusSniffer##Class(QWidget *parent, const char *name = "gakk")       \
  : Class(parent),dontsniff(false)                                     \
    {                                                                   \
    }                                                                   \
  void focusInEvent ( QFocusEvent *e ){                                 \
    if(dontsniff==false)                                                \
      num_users_of_keyboard++;                                          \
    Class::focusInEvent(e);                                             \
  }                                                                     \
  void focusOutEvent ( QFocusEvent *e ){                                \
    if(dontsniff==false) {                                              \
      num_users_of_keyboard--;                                          \
      if(num_users_of_keyboard<0)                                       \
        num_users_of_keyboard = 0;                                      \
    }                                                                   \
    Class::focusOutEvent(e);                                            \
  }                                                                     \
  void keyPressEvent ( QKeyEvent * event ){                             \
    if(event->key()==Qt::Key_Escape){                                   \
      clearFocus();                                                     \
    }                                                                   \
    Class::keyPressEvent(event);                                        \
  }                                                                     \
  }                                                    


//MakeFocusSnifferClass(QSpinBox);
//MakeFocusSnifferClass(QDoubleSpinBox);
MakeFocusSnifferClass(QLineEdit);
MakeFocusSnifferClass(QListWidget);

class FocusSnifferQSpinBox : public QSpinBox{
  public:                                                               
  bool dontsniff;                                                       
 FocusSnifferQSpinBox(QWidget *parent, const char *name = "gakk")  
   : QSpinBox(parent),dontsniff(false)                                     
    {                                                                   
    }                                                                   
  void focusInEvent ( QFocusEvent *e ){                                 
    printf("Got focusInEvent\n");
    if(dontsniff==false)                                                
      num_users_of_keyboard++;                                          
    QSpinBox::focusInEvent(e);                                             
  }                                                                     
  void focusOutEvent ( QFocusEvent *e ){                                
    printf("Got focusOutEvent\n");
    if(dontsniff==false) {                                              
      num_users_of_keyboard--;                                          
      if(num_users_of_keyboard<0)                                       
        num_users_of_keyboard = 0;                                      
    }                                                                   
    QSpinBox::focusOutEvent(e);                                            
  }                                                                     
  void 	wheelEvent ( QWheelEvent * event ){
    printf("Got wheel event\n");
    QSpinBox::wheelEvent(event);
    set_editor_focus(); 
  }
};

class FocusSnifferQDoubleSpinBox : public QDoubleSpinBox{
  public:                                                               
  bool dontsniff;                                                       
 FocusSnifferQDoubleSpinBox(QWidget *parent, const char *name = "gakk")  
   : QDoubleSpinBox(parent),dontsniff(false)                                     
    {                                                                   
    }                                                                   
  void focusInEvent ( QFocusEvent *e ){                                 
    printf("Got focusInEvent\n");
    if(dontsniff==false)                                                
      num_users_of_keyboard++;                                          
    QDoubleSpinBox::focusInEvent(e);                                             
  }                                                                     
  void focusOutEvent ( QFocusEvent *e ){                                
    printf("Got focusOutEvent\n");
    if(dontsniff==false) {                                              
      num_users_of_keyboard--;                                          
      if(num_users_of_keyboard<0)                                       
        num_users_of_keyboard = 0;                                      
    }                                                                   
    QDoubleSpinBox::focusOutEvent(e);                                            
  }                                                                     
  void 	wheelEvent ( QWheelEvent * event ){
    printf("Got wheel event\n");
    QDoubleSpinBox::wheelEvent(event);
    set_editor_focus(); 
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
      num_users_of_keyboard++;                                          
    QTableWidget::focusInEvent(e);                                             
  }                                                                     
  void focusOutEvent ( QFocusEvent *e ){                                
    printf("Got focusOutEvent\n");
    if(dontsniff==false) {                                              
      num_users_of_keyboard--;                                          
      if(num_users_of_keyboard<0)                                       
        num_users_of_keyboard = 0;                                      
    }                                                                   
    QTableWidget::focusOutEvent(e);                                            
  }                                                                     
};
                                                    

#define FocusSnifferQTableWidgetItem QTableWidgetItem

#endif


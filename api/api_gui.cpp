/* Copyright 2017 Kjetil S. Matheussen

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



#include "../common/includepython.h"

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wshorten-64-to-32"
#include <QVector> // Shortening warning in the QVector header. Temporarily turned off by the surrounding pragmas.
#pragma clang diagnostic pop


#include <QWidget>
#include <QCloseEvent>
#include <QPushButton>
#include <QCheckBox>
#include <QRadioButton>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QFormLayout>
#include <QGroupBox>
#include <QPlainTextEdit>
#include <QScrollArea>
#include <QUiLoader>
#include <QToolTip>
#include <QHeaderView>
#include <QStack>

#include "../common/nsmtracker.h"

#include "../audio/SoundPlugin.h"
#include "../audio/AudioMeterPeaks_proc.h"

#include "../Qt/FocusSniffers.h"
#include "../Qt/ScrollArea.hpp"
#include "../Qt/Qt_MyQCheckBox.h"

#include "../common/visual_proc.h"
#include "../embedded_scheme/s7extra_proc.h"

#include "../common/OS_system_proc.h"

#include "../Qt/Qt_MyQSlider.h"
#include "../Qt/Qt_mix_colors.h"
#include "../Qt/Qt_Fonts_proc.h"

#include "api_common_proc.h"

#include "radium_proc.h"
#include "api_gui_proc.h"


#define MOUSE_OVERRIDERS(classname)                                     \
  void mousePressEvent(QMouseEvent *event) override{                    \
    if(g_radium_runs_custom_exec) return;                               \
    if (_mouse_callback==NULL || !Gui::mousePressEvent(event))          \
      classname::mousePressEvent(event);                                \
  }                                                                     \
                                                                        \
  void mouseReleaseEvent(QMouseEvent *event) override {                 \
    if(g_radium_runs_custom_exec) return;                               \
    if (_mouse_callback==NULL || !Gui::mouseReleaseEvent(event))        \
      classname::mouseReleaseEvent(event);                              \
  }                                                                     \
                                                                        \
  void mouseMoveEvent(QMouseEvent *event) override{                     \
    if(g_radium_runs_custom_exec) return;                               \
    if (_mouse_callback==NULL || !Gui::mouseMoveEvent(event))           \
      classname::mouseMoveEvent(event);                                 \
  }


#define DOUBLECLICK_OVERRIDER(classname)                                \
  void mouseDoubleClickEvent(QMouseEvent *event) override{              \
    if(g_radium_runs_custom_exec) return;                               \
    if (_doubleclick_callback==NULL)                                    \
      classname::mouseDoubleClickEvent(event);                          \
    else                                                                \
      Gui::mouseDoubleClickEvent(event);                                \
  }                                                                     

#define CLOSE_OVERRIDER(classname)                                      \
  void closeEvent(QCloseEvent *ev) override {                           \
    if(g_radium_runs_custom_exec) return;                               \
    if (_close_callback==NULL)                                          \
      classname::closeEvent(ev);                                        \
    else                                                                \
      Gui::closeEvent(ev);                                              \
  }                                                                     


#define RESIZE_OVERRIDER(classname)                                     \
  void resizeEvent( QResizeEvent *event) override {                     \
    if(g_radium_runs_custom_exec) return;                               \
    if (_image!=NULL)                                                   \
      setNewImage(event->size().width(), event->size().height());       \
    if (_resize_callback==NULL)                                         \
      classname::resizeEvent(event);                                    \
    else                                                                \
      Gui::resizeEvent(event);                                          \
  }                                                                     

#define PAINT_OVERRIDER(classname)                                      \
  void paintEvent(QPaintEvent *ev) override {                           \
    if(g_radium_runs_custom_exec) return;                               \
    if(_image!=NULL){                                                   \
      QPainter p(this);                                                 \
      p.drawImage(ev->rect().topLeft(), *_image, ev->rect());           \
    }else{                                                              \
      if (_paint_callback==NULL)                                        \
        classname::paintEvent(ev);                                      \
      else                                                              \
        Gui::paintEvent(ev);                                            \
    }                                                                   \
  }                                                                     

#define SETVISIBLE_OVERRIDER(classname)                                 \
  void setVisible(bool visible) override {                              \
    if (parentWidget()==NULL) \
      remember_geometry.remember_geometry_setVisible_override_func(this, visible); \
    classname::setVisible(visible);                                     \
  }

#define OVERRIDERS(classname)                                           \
  MOUSE_OVERRIDERS(classname)                                           \
  DOUBLECLICK_OVERRIDER(classname)                                      \
  CLOSE_OVERRIDER(classname)                                            \
  RESIZE_OVERRIDER(classname)                                           \
  PAINT_OVERRIDER(classname)                                            \
  SETVISIBLE_OVERRIDER(classname)

/*
static float gain2db(float val){
  if(val<=0.0f)
    return -100.0f;

  return 20*log10(val);
}
*/

static QColor getQColor(int64_t color){
#if QT_VERSION >= 0x056000
  return QColor(QRgba64::fromRgba64(color));
#else
  QColor col((uint32_t)color);
  col.setAlpha(int(color>>24));
  return col;
#endif
}

static QColor getQColor(const_char* colorname){
  QColor color = get_config_qcolor(colorname);
  if (!color.isValid())
    handleError("Color \"%s\" is not valid");
  //return QColor(color);
  return color;
}

static QPen getPen(const_char* color){
  QPen pen(getQColor(color));
  
  return pen;
}

static bool g_currently_painting = false;

namespace radium_gui{

struct Gui;

static QVector<Gui*> g_guis;

struct VerticalAudioMeter;
static QVector<VerticalAudioMeter*> g_active_vertical_audio_meters;

  struct Callback : QObject {
    Q_OBJECT;

    func_t *_func;
    QWidget *_widget;
    
  public:
    
    Callback(func_t *func, QWidget *widget)
      : _func(func)
      , _widget(widget)
    {
      s7extra_protect(_func);
    }

    ~Callback(){
      s7extra_unprotect(_func);
    }

  public slots:
    void clicked(bool checked){
      s7extra_callFunc_void_void(_func);
    }
    
    void toggled(bool checked){
      s7extra_callFunc_void_bool(_func, checked);
    }

    void editingFinished(){
      QLineEdit *line_edit = dynamic_cast<QLineEdit*>(_widget);
      
      set_editor_focus();

      GL_lock();{
        line_edit->clearFocus();
      }GL_unlock();

      s7extra_callFunc_void_charpointer(_func, line_edit->text().toUtf8().constData());
    }

    void textChanged(QString text){
      s7extra_callFunc_void_charpointer(_func, text.toUtf8().constData());
    }

    void intValueChanged(int val){
      s7extra_callFunc_void_int(_func, val);
    }

    void doubleValueChanged(double val){
      s7extra_callFunc_void_double(_func, val);
    }

    void textChanged(){
      QTextEdit *text_edit = dynamic_cast<QTextEdit*>(_widget);
      s7extra_callFunc_void_charpointer(_func, text_edit->toPlainText().toUtf8().constData());
    }
    
    void plainTextChanged(){
      QPlainTextEdit *text_edit = dynamic_cast<QPlainTextEdit*>(_widget);
      s7extra_callFunc_void_charpointer(_func, text_edit->toPlainText().toUtf8().constData());
    }
    
    void cellDoubleClicked(int row, int column){
      s7extra_callFunc_void_int_int(_func, column, row);
    }
  };

  
  struct Gui {

    QVector<Callback*> _callbacks;
 
  public:
    int _gui_num;
    QWidget *_widget;

    QRect _orgRect;
    QWidget *_full_screen_parent = NULL;
    
    QVector<Gui*> _children;
    QVector<func_t*> _deleted_callbacks;

    radium::RememberGeometry remember_geometry;

    int get_gui_num(void){
      return _gui_num;
    }
    
    Gui(QWidget *widget)
      : _widget(widget)
    {
      R_ASSERT_RETURN_IF_FALSE(!g_guis.contains(this));
      
      _gui_num = g_guis.size();
      g_guis.push_back(this);
      _widget->setAttribute(Qt::WA_DeleteOnClose);
      //_widget->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);      
    }

    virtual ~Gui(){
      R_ASSERT_RETURN_IF_FALSE(g_guis.contains(this));
      R_ASSERT_RETURN_IF_FALSE(g_guis[_gui_num] != NULL);
      
      //printf("Deleting Gui %p\n",this);

      for(func_t *func : _deleted_callbacks){
        s7extra_callFunc_void_void(func);
        s7extra_unprotect(func);
      }
      
      for(Gui *child : _children)
        delete child;

      for(Callback *callback : _callbacks)
        delete callback;

      delete _image_painter;
      delete _image;

      if (_mouse_callback!=NULL)
        s7extra_unprotect(_mouse_callback);

      if (_doubleclick_callback!=NULL)
        s7extra_unprotect(_doubleclick_callback);

      if (_close_callback!=NULL)
        s7extra_unprotect(_close_callback);

      if (_resize_callback!=NULL)
        s7extra_unprotect(_resize_callback);

      if (_paint_callback!=NULL)
        s7extra_unprotect(_paint_callback);

      g_guis[_gui_num] = NULL;
    }

    
    /************ MOUSE *******************/
    
    func_t *_mouse_callback = NULL;
    int _currentButton = 0;

    int getMouseButtonEventID(QMouseEvent *qmouseevent){
      if(qmouseevent->button()==Qt::LeftButton)
        return TR_LEFTMOUSEDOWN;
      else if(qmouseevent->button()==Qt::RightButton)
        return TR_RIGHTMOUSEDOWN;
      else if(qmouseevent->button()==Qt::MiddleButton)
        return TR_MIDDLEMOUSEDOWN;
      else
        return 0;
    }

    bool mousePressEvent(QMouseEvent *event){
      R_ASSERT_RETURN_IF_FALSE2(_mouse_callback!=NULL, false);

      event->accept();

      _currentButton = getMouseButtonEventID(event);
      const QPoint &point = event->pos();

      return s7extra_callFunc_bool_int_int_float_float(_mouse_callback, _currentButton, API_MOUSE_PRESSING, point.x(), point.y());
      //printf("  Press. x: %d, y: %d. This: %p\n", point.x(), point.y(), this);
    }

    bool mouseReleaseEvent(QMouseEvent *event) {
      R_ASSERT_RETURN_IF_FALSE2(_mouse_callback!=NULL, false);

      event->accept();

      const QPoint &point = event->pos();
      bool ret = s7extra_callFunc_bool_int_int_float_float(_mouse_callback, _currentButton, API_MOUSE_RELEASING, point.x(), point.y());
      
      _currentButton = 0;
      //printf("  Release. x: %d, y: %d. This: %p\n", point.x(), point.y(), this);
      return ret;
    }

    bool mouseMoveEvent(QMouseEvent *event){
      R_ASSERT_RETURN_IF_FALSE2(_mouse_callback!=NULL, false);

      event->accept();

      const QPoint &point = event->pos();
      return s7extra_callFunc_bool_int_int_float_float(_mouse_callback, _currentButton, API_MOUSE_MOVING, point.x(), point.y());

      //printf("    move. x: %d, y: %d. This: %p\n", point.x(), point.y(), this);
    }

    void addMouseCallback(func_t* func){      
      if (_mouse_callback!=NULL){
        handleError("Gui %d already has a mouse callback.", _gui_num);
        return;
      }

      _mouse_callback = func;
      s7extra_protect(_mouse_callback);
      _widget->setMouseTracking(true);
    }


    /************ DOUBLECLICK *******************/

    func_t *_doubleclick_callback = NULL;

    void mouseDoubleClickEvent(QMouseEvent *event){
      R_ASSERT_RETURN_IF_FALSE(_doubleclick_callback!=NULL);
      event->accept();

      const QPoint &point = event->pos();

      s7extra_callFunc_void_int_float_float(_doubleclick_callback, getMouseButtonEventID(event), point.x(), point.y());
    }

    virtual void addDoubleClickCallback(func_t* func){      
      if (_doubleclick_callback!=NULL){
        handleError("Gui %d already has a doubleclick callback.", _gui_num);
        return;
      }

      _doubleclick_callback = func;
      s7extra_protect(_doubleclick_callback);
    }

    
    /************ CLOSE *******************/

    func_t *_close_callback = NULL;

    void closeEvent(QCloseEvent *event){
      R_ASSERT_RETURN_IF_FALSE(_close_callback!=NULL);

      if (false==s7extra_callFunc_bool_void(_close_callback))
        event->ignore();
      else
        event->accept();
    }

    void addCloseCallback(func_t* func){      
      if (_close_callback!=NULL){
        handleError("Gui %d already has a close callback.", _gui_num);
        return;
      }

      _close_callback = func;
      s7extra_protect(_close_callback);
    }

    
    /************ RESIZE *******************/
    
    func_t *_resize_callback = NULL;
    
    void resizeEvent(QResizeEvent *event){
      R_ASSERT_RETURN_IF_FALSE(_resize_callback!=NULL);

      if(gui_isOpen(_gui_num) && _widget->isVisible()) //  && _widget->width()>0 && _widget->height()>0)
        s7extra_callFunc_void_int_int(_resize_callback, event->size().width(), event->size().height());
    }

    void addResizeCallback(func_t* func){
      if (_resize_callback!=NULL){
        handleError("Gui %d already has a resize callback.", _gui_num);
        return;
      }

      _resize_callback = func;
      s7extra_protect(_resize_callback);
    }


    
    /************ DRAWING *******************/

    func_t *_paint_callback = NULL;

    QImage *_image = NULL;
    QPainter *_image_painter = NULL;
    QPainter *_current_painter = NULL;

    void paintEvent(QPaintEvent *event) {
      R_ASSERT_RETURN_IF_FALSE(_paint_callback!=NULL);
      
      event->accept();

      R_ASSERT_RETURN_IF_FALSE(g_currently_painting==false);

      g_currently_painting = true;

      QPainter p(_widget);
      p.setRenderHints(QPainter::Antialiasing,true);

      _current_painter = &p;

      s7extra_callFunc_void_int_int(_paint_callback, _widget->width(), _widget->height());

      _current_painter = NULL;

      g_currently_painting = false;
    }

    void addPaintCallback(func_t* func){
      if (_paint_callback!=NULL){
        handleError("Gui %d already has a paint callback.", _gui_num);
        return;
      }

      _paint_callback = func;
      s7extra_protect(_paint_callback);
    }


    void setNewImage(int width, int height){
      width = R_MAX(1,width);
      height = R_MAX(1, height);

      if (_image!=NULL && _image->width() >= width && _image->height() >= height)
        return;

      if (_image!=NULL){
        width = R_MAX(_image->width(), width);
        height = R_MAX(_image->height(), height);
      }

      //width*=2;
      //height*=2;

      //printf("   %d: num_calls to setNewImage: %d. %d >= %d, %d >= %d\n", _gui_num, num_calls++, _image==NULL ? -1 : _image->width(), width, _image==NULL ? -1 : _image->height(), height);

      auto *new_image = new QImage(width, height, QImage::Format_ARGB32);
      auto *new_image_painter = new QPainter(new_image);

      //new_image_painter->fillRect(QRect(0,0,width,height), get_qcolor(LOW_BACKGROUND_COLOR_NUM));
      
      if (_image!=NULL)
        new_image_painter->drawImage(QPoint(0,0), *_image);
      
      new_image_painter->setRenderHints(QPainter::Antialiasing,true);

      delete _image_painter;
      delete _image;
      _image_painter = new_image_painter;
      _image = new_image;

      _widget->setAttribute(Qt::WA_OpaquePaintEvent);
      //_widget->setAttribute(Qt::WA_PaintOnScreen);
    }
    
    
    QPainter *get_painter(void) {
      QPainter *painter = _current_painter;

      if (painter==NULL){

        if (_image==NULL)
          setNewImage(_widget->width(), _widget->height());

        painter=_image_painter;
      }

      return painter;
    }

    void setPen(const_char* color){
      get_painter()->setPen(getQColor(color));
    }

    void myupdate(float x1, float y1, float x2, float y2, float extra=0.0f){
      if (_current_painter == NULL){
        float min_x = R_MIN(x1, x2) - extra;
        float max_x = R_MAX(x1, x2) + extra;
        float min_y = R_MIN(y1, y2) - extra;
        float max_y = R_MAX(y1, y2) + extra;
        _widget->update(min_x-1, min_y-1, max_x-min_x+2, max_y-min_y+2);
      }
    }

    void drawLine(const_char* color, float x1, float y1, float x2, float y2, float width) {
      QPainter *painter = get_painter();
      
      QLineF line(x1, y1, x2, y2);

      QPen pen = getPen(color);
      pen.setWidthF(width);
      painter->setPen(pen);
      
      //printf("Color: %x, %s\n", (unsigned int)color, pen.color().name(QColor::HexArgb).toUtf8().constData());

      painter->drawLine(line);

      myupdate(x1, y1, x2, y2, width);
    }

    void drawBox(const_char* color, float x1, float y1, float x2, float y2, float width, float round_x, float round_y) {
      QPainter *painter = get_painter();

      QRectF rect(x1, y1, x2-x1, y2-y1);

      QPen pen = getPen(color);
      pen.setWidthF(width);
      painter->setPen(pen);

      if (round_x>0 && round_y>0)
        painter->drawRoundedRect(rect, round_x, round_y);
      else
        painter->drawRect(rect);

      myupdate(x1, y1, x2, y2);
    }

    void filledBox(const_char* color, float x1, float y1, float x2, float y2, float round_x, float round_y) {
      QPainter *painter = get_painter();

      QRectF rect(x1, y1, x2-x1, y2-y1);

      QColor qcolor = getQColor(color);

      //QPen pen = _image_rect.pen();

      painter->setPen(Qt::NoPen);
      painter->setBrush(qcolor);
      if (round_x>0 && round_y>0)
        painter->drawRoundedRect(rect, round_x, round_y);
      else
        painter->drawRect(rect);
      painter->setBrush(Qt::NoBrush);
      //_image_painter->setPen(pen);

      myupdate(x1, y1, x2, y2);
    }

    void drawText(const_char* color, const_char *text, float x1, float y1, float x2, float y2, bool wrap_lines, bool align_top, bool align_left, bool draw_vertical) {
      QPainter *painter = get_painter();

      QRectF rect(x1, y1, x2-x1, y2-y1);

      setPen(color);
      
      int flags = Qt::TextWordWrap;

      if(align_top)
        flags |= Qt::AlignTop;
      else
        flags |= Qt::AlignHCenter;

      if(align_left)
        flags |= Qt::AlignLeft;
      else
        flags |= Qt::AlignVCenter;


      if (draw_vertical){

        painter->setFont(GFX_getFittingFont(text, flags, y2-y1, x2-x1));

        painter->save();
        painter->rotate(90);
        painter->translate(y1,-x1-rect.width());

        QRect rect2(0,0,rect.height(),rect.width());
        painter->drawText(rect2, flags, text);

        painter->restore();

      } else {

        painter->setFont(GFX_getFittingFont(text, flags, x2-x1, y2-y1));

        painter->drawText(rect, flags, text);
      
      }


      myupdate(x1, y1, x2, y2);
    }


    /************ VIRTUAL METHODS *******************/
    
    virtual QLayout *getLayout(void){
      return _widget->layout();
    }

    virtual void setGuiValue(dyn_t val){
      {
        QAbstractButton *button = dynamic_cast<QAbstractButton*>(_widget);
        if (button!=NULL){
          if(val.type==BOOL_TYPE)
            button->setChecked(val.bool_number);
          else
            handleError("Button->setValue received %s, expected BOOL_TYPE", DYN_type_name(val.type));
          return;
        }
      }

      {
        QAbstractSlider *slider = dynamic_cast<QAbstractSlider*>(_widget);
        if (slider!=NULL){
          if(val.type==INT_TYPE)
            slider->setValue(val.bool_number);
          else
            handleError("Slider->setValue received %s, expected INT_TYPE", DYN_type_name(val.type));
          return;
        }
      }

      {
        QLabel *label = dynamic_cast<QLabel*>(_widget);
        if (label!=NULL){
          if(val.type==STRING_TYPE)
            label->setText(STRING_get_qstring(val.string));
          else
            handleError("Text->setValue received %s, expected STRING_TYPE", DYN_type_name(val.type));
          return;
        }
      }

      {
        QLineEdit *line_edit = dynamic_cast<QLineEdit*>(_widget);
        if (line_edit!=NULL){
          if(val.type==STRING_TYPE)
            line_edit->setText(STRING_get_qstring(val.string));
          else
            handleError("Line->setValue received %s, expected STRING_TYPE", DYN_type_name(val.type));
          return;
        }
      }

      {
        QTextEdit *text_edit = dynamic_cast<QTextEdit*>(_widget);
        if (text_edit!=NULL){ 
          if(val.type==STRING_TYPE)
            text_edit->setPlainText(STRING_get_chars(val.string));
          else
            handleError("Text->setValue received %s, expected STRING_TYPE", DYN_type_name(val.type));
          return;
        }
      }

      {
        QSpinBox *spinbox = dynamic_cast<QSpinBox*>(_widget);
        if (spinbox!=NULL){
          if (val.type==INT_TYPE)
            spinbox->setValue((int)val.int_number);
          else
            handleError("IntText->setValue received %s, expected INT_TYPE", DYN_type_name(val.type));
          return;
        }
      }

      {
        QDoubleSpinBox *doublespinbox = dynamic_cast<QDoubleSpinBox*>(_widget);
        if (doublespinbox!=NULL){
          if (val.type==FLOAT_TYPE)
            doublespinbox->setValue(val.float_number);
          else
            handleError("FloatText->setValue received %s, expected FLOAT_TYPE", DYN_type_name(val.type));
          return;
        }
      }
                  
      handleError("Gui #%d does not have a setValue method", _gui_num);
    }

    virtual dyn_t getGuiValue(void){

      QAbstractButton *button = dynamic_cast<QAbstractButton*>(_widget);
      if (button!=NULL)
        return DYN_create_bool(button->isChecked());

      QAbstractSlider *slider = dynamic_cast<QAbstractSlider*>(_widget);
      if (slider!=NULL)
        return DYN_create_int(slider->value());

      QLabel *label = dynamic_cast<QLabel*>(_widget);
      if (label!=NULL)
        return DYN_create_string(label->text());

      QLineEdit *line_edit = dynamic_cast<QLineEdit*>(_widget);
      if (line_edit!=NULL)
        return DYN_create_string(line_edit->text());

      QTextEdit *text_edit = dynamic_cast<QTextEdit*>(_widget);
      if (text_edit!=NULL)
        return DYN_create_string(text_edit->toPlainText());

      QSpinBox *spinbox = dynamic_cast<QSpinBox*>(_widget);
      if (spinbox!=NULL)
        return DYN_create_int(spinbox->value());

      QDoubleSpinBox *doublespinbox = dynamic_cast<QDoubleSpinBox*>(_widget);
      if (doublespinbox!=NULL)
        return DYN_create_float(doublespinbox->value());
      
                  
      handleError("Gui #%d does not have a getValue method", _gui_num);
      return DYN_create_bool(false);
    }

    virtual void addGuiRealtimeCallback(func_t* func){
      Callback *callback = new Callback(func, _widget);

      {
        QLineEdit *line_edit = dynamic_cast<QLineEdit*>(_widget);
        if (line_edit!=NULL){
          line_edit->connect(line_edit, SIGNAL(textChanged(QString)), callback, SLOT(textChanged(QString)));
          goto gotit;
        }
      }
      
      handleError("Gui #%d does not have a addRealtimeCallback method", _gui_num);
      delete callback;
      return;
      
    gotit:
      _callbacks.push_back(callback);
      return;      
    }
    
    virtual void addGuiCallback(func_t* func){
      Callback *callback = new Callback(func, _widget);

      {
        QCheckBox *button = dynamic_cast<QCheckBox*>(_widget);
        if (button!=NULL){
          button->connect(button, SIGNAL(toggled(bool)), callback, SLOT(toggled(bool)));
          s7extra_callFunc_void_bool(func, button->isChecked());
          goto gotit;
        }
      }

      {
        QRadioButton *button = dynamic_cast<QRadioButton*>(_widget);
        if (button!=NULL){
          button->connect(button, SIGNAL(toggled(bool)), callback, SLOT(toggled(bool)));
          s7extra_callFunc_void_bool(func, button->isChecked());
          goto gotit;
        }
      }

      {
        QAbstractButton *button = dynamic_cast<QAbstractButton*>(_widget);
        if (button!=NULL){
          button->connect(button, SIGNAL(clicked(bool)), callback, SLOT(clicked(bool)));
          goto gotit;
        }
      }

      {
        QAbstractSlider *slider = dynamic_cast<QAbstractSlider*>(_widget);
        if (slider!=NULL){
          slider->connect(slider, SIGNAL(intValueChanged(int)), callback, SLOT(intValueChanged(int)));
          s7extra_callFunc_void_int(func, slider->value());
          goto gotit;
        }
      }
      
      {
        QLineEdit *line_edit = dynamic_cast<QLineEdit*>(_widget);
        if (line_edit!=NULL){
          line_edit->connect(line_edit, SIGNAL(editingFinished()), callback, SLOT(editingFinished()));
          s7extra_callFunc_void_charpointer(func, line_edit->text().toUtf8().constData()); // Calling the callbacks here was a really bad idea. TODO: Fix that.
          goto gotit;
        }
      }

      {
        QSpinBox *spinbox = dynamic_cast<QSpinBox*>(_widget);
        if (spinbox!=NULL){
          spinbox->connect(spinbox, SIGNAL(valueChanged(int)), callback, SLOT(intValueChanged(int)));
          s7extra_callFunc_void_int(func, spinbox->value());
          goto gotit;
        }
      }
      
      {
        QDoubleSpinBox *spinbox = dynamic_cast<QDoubleSpinBox*>(_widget);
        if (spinbox!=NULL){
          spinbox->connect(spinbox, SIGNAL(valueChanged(double)), callback, SLOT(doubleValueChanged(double)));
          s7extra_callFunc_void_double(func, spinbox->value());
          goto gotit;
        }
      }

      {
        QTextEdit *text_edit = dynamic_cast<QTextEdit*>(_widget);
        if (text_edit!=NULL){
          text_edit->connect(text_edit, SIGNAL(textChanged()), callback, SLOT(textChanged()));
          s7extra_callFunc_void_charpointer(func, text_edit->toPlainText().toUtf8().constData());
          goto gotit;
        }
      }
            
      {
        QPlainTextEdit *text_edit = dynamic_cast<QPlainTextEdit*>(_widget);
        if (text_edit!=NULL){
          text_edit->connect(text_edit, SIGNAL(plainTextChanged()), callback, SLOT(plainTextChanged()));
          s7extra_callFunc_void_charpointer(func, text_edit->toPlainText().toUtf8().constData());
          goto gotit;
        }
      }
            
      handleError("Gui #%d does not have a addCallback method", _gui_num);
      delete callback;
      return;

    gotit:
      _callbacks.push_back(callback);
      return;      
    }
  };
  

  struct Widget : QWidget, Gui{
    Q_OBJECT;

  public:
    
    Widget(int width, int height)
      : Gui(this)
    {
      if (width>=0 || height>=0){
        if(width<=0)
          width=R_MAX(1, QWidget::width());
        if(height<=0)
          height=R_MAX(1, QWidget::height());
        resize(width,height);
      }
    }

    ~Widget() {
    }

    OVERRIDERS(QWidget);
  };

  
  struct VerticalAudioMeter : QWidget, Gui{
    Q_OBJECT;

    struct Patch *_patch;
    float *_pos = NULL;
    float *_falloff_pos = NULL;

    int _num_channels = 0;
    bool _is_input = false;
    bool _is_output = false;

    float _last_peak = -100.0f;
    func_t *_peak_callback = NULL;

  public:
    
    VerticalAudioMeter(struct Patch *patch)
      : Gui(this)
      , _patch(patch)
    {
      R_ASSERT_RETURN_IF_FALSE(patch->instrument==get_audio_instrument());

      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
      R_ASSERT_RETURN_IF_FALSE(plugin!=NULL);

      if(plugin->type->num_outputs > 0)
        _is_output = true;
      else if(plugin->type->num_inputs > 0)
        _is_input = true;

      if (_is_output)
        _num_channels = plugin->type->num_outputs;
      else if (_is_input)
        _num_channels = plugin->type->num_inputs;

      if (_num_channels > 0){
        _pos = (float*)calloc(_num_channels, sizeof(float));
        _falloff_pos = (float*)calloc(_num_channels, sizeof(float));
      }

      R_ASSERT_RETURN_IF_FALSE(get_audio_meter_peaks().num_channels == _num_channels);

      call_regularly(); // Set meter positions.

      g_active_vertical_audio_meters.push_back(this);
    }

    ~VerticalAudioMeter(){
      R_ASSERT(g_active_vertical_audio_meters.removeOne(this));
      if (_peak_callback!=NULL)
        s7extra_unprotect(_peak_callback);
      free(_pos);
      free(_falloff_pos);
    }
    
    MOUSE_OVERRIDERS(QWidget);
    DOUBLECLICK_OVERRIDER(QWidget);
    RESIZE_OVERRIDER(QWidget);

    void addPeakCallback(func_t *func, int guinum){
      if (_peak_callback != NULL){
        handleError("Audio Meter #%d already have a peak callback", guinum);
        return;
      }

      _peak_callback = func;

      s7extra_protect(_peak_callback);

      callPeakCallback();
    }

    AudioMeterPeaks _fallback_peaks = {0};

    AudioMeterPeaks &get_audio_meter_peaks(void){
      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;

      if(plugin==NULL)
        return _fallback_peaks;

      // Testing backtrace.
      //if (ATOMIC_GET(root->editonoff)==false) // If removing this line, there is no s7 backtrace in the crashreport when the program stops during startup. Really strange.
      //  R_ASSERT(false);
            
      if (_is_output)
        return plugin->volume_peaks;

      if (_is_input)
        return plugin->input_volume_peaks;

      R_ASSERT(false);
      return _fallback_peaks;
    }

    void callPeakCallback(void){
      if (_peak_callback != NULL){
        if (_last_peak<=-100.0)
          s7extra_callFunc_void_charpointer(_peak_callback, "-inf");
        else
          s7extra_callFunc_void_charpointer(_peak_callback, talloc_format("%.1f", _last_peak));
      }
    }

    void resetPeak(void){
      _last_peak = -100;
      const AudioMeterPeaks &peaks = get_audio_meter_peaks();
      for(int ch = 0 ; ch < _num_channels ; ch++)
        peaks.peaks[ch] = -100;
    }

    void get_x1_x2(int ch, float &x1, float &x2){
      
      int num_borders = _num_channels + 1;
      float border_width = 0;

      float meter_area_width = width() - 2;
      float start_x = 1;

      float total_meter_space = meter_area_width - num_borders * border_width;

      float meter_width = total_meter_space / _num_channels;

      x1 = start_x + border_width + (ch * (border_width+meter_width));
      x2 = start_x + border_width + (ch * (border_width+meter_width)) + meter_width;
    }

    const float upper_border = 2;
    const float down_border = 2;

    float get_pos_y1(void) const {
      return upper_border - 1;
    }
    
    float get_pos_y2(void) const {
      return height() - down_border + 2;
    }
    
    float get_pos_height(void) const {
      return height() - upper_border - down_border;
    }

    const float falloff_height = 1.5;

    void call_regularly(void){
      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
      if(plugin==NULL)
        return;

      for(int ch=0 ; ch < _num_channels ; ch++){
        float prev_pos = _pos[ch];
        float prev_falloff_pos = _falloff_pos[ch];

        float db;
        float db_falloff;
        float db_peak;

        const AudioMeterPeaks &peaks = get_audio_meter_peaks();

        db = peaks.decaying_dbs[ch];
        db_falloff = peaks.falloff_dbs[ch];
        db_peak = peaks.peaks[ch];
        
        if (db_peak > _last_peak){
          _last_peak = db_peak;
          callPeakCallback();
        }

        float pos = db2linear(db, get_pos_y1(), get_pos_y2());
        _pos[ch] = pos;

        float x1,x2;
        get_x1_x2(ch, x1,x2);

        if (pos < prev_pos){
          update(x1-1,      floorf(pos)-1,
                 x2-x1+3,   floorf(prev_pos-pos)+3);
        } else if (pos > prev_pos){
          update(x1-1,      floorf(prev_pos)-1,
                 x2-x1+3,   floorf(pos-prev_pos)+3);
        }

        //update();

        float falloff_pos = db2linear(db_falloff, get_pos_y1(), get_pos_y2());
        _falloff_pos[ch] = falloff_pos;

#if 1
        if (falloff_pos != prev_falloff_pos)
          update();
#else
        // don't know what's wrong here...
        if (falloff_pos < prev_falloff_pos){
          update(x1-1,      floorf(falloff_pos)-1-falloff_height,
                 x2-x1+2,   floorf(prev_falloff_pos-falloff_pos)+3+falloff_height*2);
        } else if (pos > prev_pos){
          update(x1-1,      floorf(prev_falloff_pos)-1-falloff_height,
                 x2-x1+2,   floorf(falloff_pos-prev_falloff_pos)+3+falloff_height*2);
        }
#endif
      }
    }
    
    void paintEvent(QPaintEvent *ev) override {
      QPainter p(this);

      p.setRenderHints(QPainter::Antialiasing,true);

      QColor qcolor1("black");
      QColor color4db = get_qcolor(PEAKS_4DB_COLOR_NUM);
      QColor color0db = get_qcolor(PEAKS_0DB_COLOR_NUM);
      QColor colorgreen  = get_qcolor(PEAKS_COLOR_NUM);
      QColor colordarkgreen  = get_qcolor(PEAKS_COLOR_NUM).darker(150);

      float posm20db = db2linear(-20, get_pos_y1(), get_pos_y2());
      float pos0db = db2linear(0, get_pos_y1(), get_pos_y2());
      float pos4db = db2linear(4, get_pos_y1(), get_pos_y2());

      p.setPen(Qt::NoPen);
        
      for(int ch=0 ; ch < _num_channels ; ch++){
        float x1,x2;
        get_x1_x2(ch, x1,x2);

        float pos = _pos[ch];

        // Background
        {
          QRectF rect1(x1,  get_pos_y1(),
                       x2-x1,  pos-get_pos_y1());
          p.setBrush(qcolor1);
          p.drawRect(rect1);
        }

        // decaying meter
        {

            // Do the dark green
            {
              float dark_green_pos = R_MAX(pos, posm20db);
              QRectF rect(x1, dark_green_pos,
                          x2-x1,  get_pos_y2()-dark_green_pos);
              
              p.setBrush(colordarkgreen);
              p.drawRect(rect);
            }

            // Do the green
            if (pos <= posm20db){
              float green_pos = R_MAX(pos, pos0db);
              QRectF rect(x1, green_pos,
                          x2-x1,  posm20db-green_pos);
              
              p.setBrush(colorgreen);
              p.drawRect(rect);
            }

            // Do the yellow
            if (pos <= pos0db){
              float yellow_pos = R_MAX(pos, pos4db);
              QRectF rect(x1, yellow_pos,
                           x2-x1,  pos0db-yellow_pos);
              
              p.setBrush(color0db);
              p.drawRect(rect);
            }

            // Do the red
            if (pos <= pos4db){
              float red_pos = pos;
              QRectF rect(x1, red_pos,
                           x2-x1,  pos4db-red_pos);
              
              p.setBrush(color4db);
              p.drawRect(rect);
            }
        }

        // falloff meter
        {
          //SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
          //if(plugin!=NULL)
          //  printf("Fallof pos: %f, y2: %f, falloffdb: %f\n", _falloff_pos[ch], get_pos_y2(), plugin->volume_peaks.falloff_dbs[ch]);
          
          if (_falloff_pos[ch] < get_pos_y2()){
            float falloff_pos = _falloff_pos[ch];
            
            QRectF falloff_rect(x1, falloff_pos-falloff_height/2.0,
                                x2-x1, falloff_height);

            if (falloff_pos > posm20db)
              p.setBrush(colordarkgreen);
            else if (falloff_pos > pos0db)
              p.setBrush(colorgreen);
            else if (falloff_pos > pos4db)
              p.setBrush(color0db);
            else
              p.setBrush(color4db);

            p.drawRect(falloff_rect);
          }
        }

        /*        
        p.setBrush(qcolor1);
        p.drawRoundedRect(QRectF(get_x1(ch), 1,
                                 get_x2(ch), height()-1),
                          5,5);

        p.setBrush(qcolor2);
        p.drawRoundedRect(rect2,5,5);
        */
      }

      p.setBrush(Qt::NoBrush);
      
      //border
#if 0
      QPen pen(QColor("#202020"));
      pen.setWidth(2.0);
      p.setPen(pen);
      p.drawRoundedRect(0,0,width()-1,height()-1, 5, 5);
#endif
    }

  };

  
  struct PushButton : QPushButton, Gui{
    Q_OBJECT;
    
  public:
    
    PushButton(const char *text)
      : QPushButton(text)
      , Gui(this)
    {
    }

    OVERRIDERS(QPushButton);
  };

  
  struct CheckBox : QCheckBox, Gui{
    Q_OBJECT;
    
  public:
    
    CheckBox(const char *text, bool is_checked)
      : QCheckBox(text)
      , Gui(this)
    {
      setChecked(is_checked);
    }

    OVERRIDERS(QCheckBox);
  };
  
  struct RadiumCheckBox : MyQCheckBox, Gui{
    Q_OBJECT;
    
  public:
    
    RadiumCheckBox(const char *text, bool is_checked)
      : MyQCheckBox(text)
      , Gui(this)
    {
      setChecked(is_checked);
    }

    OVERRIDERS(MyQCheckBox);
  };
  
  struct RadioButton : QRadioButton, Gui{
    Q_OBJECT;
    
  public:
    
    RadioButton(const char *text, bool is_checked)
      : QRadioButton(text)
      , Gui(this)
    {
      setChecked(is_checked);
    }

    OVERRIDERS(QRadioButton);
  };
  
  struct VerticalLayout : QWidget, Gui{
    VerticalLayout()
      : Gui(this)
    {
      QVBoxLayout *mainLayout = new QVBoxLayout;      
      setLayout(mainLayout);
    }

    OVERRIDERS(QWidget);
  };
  
  struct HorizontalLayout : QWidget, Gui{
    HorizontalLayout()
      : Gui(this)
    {
      QHBoxLayout *mainLayout = new QHBoxLayout;      

      setLayout(mainLayout);
    }

    OVERRIDERS(QWidget);
  };

  struct MyGridLayout : QGridLayout{
    int _num_columns;
    int _x=0,_y=0;
    
    MyGridLayout(int num_columns)
      : _num_columns(num_columns)
    {}

    void addItem(QLayoutItem *item) override {
      QGridLayout::addItem(item, _y, _x);
      _x++;
      if (_x==_num_columns){
        _x = 0;
        _y++;
      }
    }
  };
  
  struct TableLayout : QWidget, Gui{
    TableLayout(int num_columns)
      : Gui(this)
    {
      setLayout(new MyGridLayout(num_columns));
    }
    
    OVERRIDERS(QWidget);
  };
  
  struct GroupBox : QGroupBox, Gui{
    GroupBox(const char *title)
      : QGroupBox(title)
      , Gui(this)        
    {
      QVBoxLayout *mainLayout = new QVBoxLayout;
      setLayout(mainLayout);
    }

    OVERRIDERS(QGroupBox);
  };

  struct ScrollArea : radium::ScrollArea, Gui{
    QWidget *contents;
    const char *magic = "magic";

    ScrollArea(bool scroll_horizontal, bool scroll_vertical)
      : Gui(this)        
    {
      horizontalScrollBar()->setObjectName("horizontalScrollBar");
      verticalScrollBar()->setObjectName("verticalScrollBar");

      if (!scroll_horizontal)
        setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
      //else
      //  setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOn);

      if (!scroll_vertical)
        setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
      //else
      //  setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOn);

      //setWidgetResizable(true);

      contents = getWidget(); //new QWidget;//(this);
      //contents->resize(500,500);
      //contents->show();

      QLayout *layout = new QVBoxLayout;
      layout->setSpacing(0);
      layout->setContentsMargins(0,0,0,0);

      contents->setLayout(layout);

      //setWidget(contents);
    }

    QLayout *getLayout(void) override {
      return contents->layout();
    }

    OVERRIDERS(radium::ScrollArea);
  };

  struct VerticalScroll : radium::ScrollArea, Gui{
    QWidget *contents;
    const char *magic = "magic2";
    QLayout *mylayout;

    VerticalScroll(void)
      : Gui(this)        
    {
      horizontalScrollBar()->setObjectName("horizontalScrollBar");
      verticalScrollBar()->setObjectName("verticalScrollBar");

      setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);

      //setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
      //setWidgetResizable(true);
      
      contents = getWidget(); //new QWidget;//(this);
      //contents = new QWidget(this);

      mylayout = new QVBoxLayout(contents);
      mylayout->setSpacing(1);
      //mylayout->setContentsMargins(1,1,1,1);

      contents->setLayout(mylayout);
      
      //setWidget(contents);    
    }

    QLayout *getLayout(void) override {
      return mylayout;
    }

    OVERRIDERS(radium::ScrollArea);
  };

  struct HorizontalScroll : QScrollArea, Gui{
    QWidget *contents;
    const char *magic = "magic3";
    QLayout *mylayout;

    HorizontalScroll(void)
      : Gui(this)        
    {
      horizontalScrollBar()->setObjectName("horizontalScrollBar");
      verticalScrollBar()->setObjectName("verticalScrollBar");

      //setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
      setWidgetResizable(true);

      QWidget *contents = new QWidget(this);

      mylayout = new QHBoxLayout(contents);
      mylayout->setSpacing(1);
      //mylayout->setContentsMargins(1,1,1,1);

      contents->setLayout(mylayout);
      
      setWidget(contents);    
    }

    QLayout *getLayout(void) override {
      return mylayout;
    }

    OVERRIDERS(QScrollArea);
  };

  struct Slider : MyQSlider, Gui{
    Q_OBJECT;

    bool _is_int;
    QString _text;
    double _min,_max;
    QVector<func_t*> _funcs;
    
  public:
    
    Slider(Qt::Orientation orientation, const char *text, double min, double curr, double max, bool is_int)
      : MyQSlider(orientation)
      , Gui(this)
      , _is_int(is_int)
      , _text(text)
      , _min(min)
      , _max(max)
    {

      R_ASSERT(min!=max);

      if (orientation==Qt::Vertical){ // Weird Qt vertical slider behavor.
        double temp = max;
        max = min;
        min = temp;
        _max = max;
        _min = min;
      }
        
      if (is_int) {
        setMinimum(0);
        setMaximum(fabs(min-max));
        setTickInterval(1);
      } else {
        setMinimum(0);
        setMaximum(10000);
      }

      setGuiValue(DYN_create_float(curr));
      connect(this, SIGNAL(valueChanged(int)), this, SLOT(valueChanged(int)));      

      value_setted(value()); // In case value wasn't changed when calling setValue above.
    }

    ~Slider(){
      for(func_t *func : _funcs)
        s7extra_unprotect(func);
    }

    OVERRIDERS(MyQSlider);
    
    void value_setted(int value){
      double scaled_value = scale_double(value, minimum(), maximum(), _min, _max);
      
      if (_is_int) {
        SLIDERPAINTER_set_string(_painter, _text + QString::number(scaled_value));
        for(func_t *func : _funcs)
          s7extra_callFunc_void_int(func, scaled_value);
      } else {
        SLIDERPAINTER_set_string(_painter, _text + QString::number(scaled_value, 'f', 2));
        for(func_t *func : _funcs)
          s7extra_callFunc_void_double(func, scaled_value);
      }
    }

    virtual void addGuiCallback(func_t* func) override {
      s7extra_protect(func);
      _funcs.push_back(func);
      value_setted(value());
    }
    
    virtual void setGuiValue(dyn_t val) override {
      if(val.type!=INT_TYPE && val.type!=FLOAT_TYPE){
        handleError("Slider->setValue received %s, expected INT_TYPE or FLOAT_TYPE", DYN_type_name(val.type));
        return;
      }

      if (val.type==INT_TYPE)
        setValue(scale_double(val.int_number, _min, _max, minimum(), maximum()));
      else
        setValue(scale_double(val.float_number, _min, _max, minimum(), maximum()));
    }

    virtual dyn_t getGuiValue(void) override {
      double scaled_value = scale_double(value(), minimum(), maximum(), _min, _max);
      if (_is_int)
        return DYN_create_int((int)scaled_value);
      else
        return DYN_create_float(scaled_value);
    }

  public slots:
    void valueChanged(int value){
      value_setted(value);
    }
  };

  struct Text : QLabel, Gui{
    Text(QString text, QString color)
      : Gui(this)
    {
      if (color=="")
        setText(text);
      else
        setText("<span style=\" color:" + color + ";\">" + text + "</span>");
    }

    OVERRIDERS(QLabel);
  };

  struct MyFocusSnifferQLineEdit : FocusSnifferQLineEdit{
    Q_OBJECT;
  public:
    
    MyFocusSnifferQLineEdit(QWidget *parent = NULL)
      : FocusSnifferQLineEdit(parent)
    {
      connect(this, SIGNAL(editingFinished()), this, SLOT(editingFinished()));
    }

  public slots:
    void editingFinished(){
      set_editor_focus();
      GL_lock();{
        clearFocus();
      }GL_unlock();
    }
  };
  
  struct Line : MyFocusSnifferQLineEdit, Gui{
    Q_OBJECT;

  public:
    
    Line(QString content)
      : Gui(this)
    {
      setText(content);
      setCursorPosition(0);
      setContextMenuPolicy(Qt::NoContextMenu);
    }

    OVERRIDERS(MyFocusSnifferQLineEdit);
  };

  struct TextEdit : FocusSnifferQTextEdit, Gui{
    Q_OBJECT;

  public:
    
    TextEdit(QString content)
      : Gui(this)
    {
      setPlainText(content);
    }

    OVERRIDERS(FocusSnifferQTextEdit);
  };


  struct MyFocusSnifferQSpinBox : FocusSnifferQSpinBox{
    Q_OBJECT;
  public:
    
    MyFocusSnifferQSpinBox(QWidget *parent = NULL)
      : FocusSnifferQSpinBox(parent)
    {
      connect(this, SIGNAL(editingFinished()), this, SLOT(editingFinished()));
    }

  public slots:
    void editingFinished(){
      set_editor_focus();
      GL_lock();{
        clearFocus();
      }GL_unlock();
    }
  };
  

  struct IntText : MyFocusSnifferQSpinBox, Gui{
    Q_OBJECT;
    
  public:
    
    IntText(int min, int curr, int max)
      : Gui(this)
    {
      setMinimum(R_MIN(min, max));
      setMaximum(R_MAX(min, max));
    }

    OVERRIDERS(MyFocusSnifferQSpinBox);
  };
  
  struct MyFocusSnifferQDoubleSpinBox : FocusSnifferQDoubleSpinBox{
    Q_OBJECT;
  public:
    
    MyFocusSnifferQDoubleSpinBox(QWidget *parent = NULL)
      : FocusSnifferQDoubleSpinBox(parent)
    {
      connect(this, SIGNAL(editingFinished()), this, SLOT(editingFinished()));
    }

  public slots:
    void editingFinished(){
      set_editor_focus();
      GL_lock();{
        clearFocus();
      }GL_unlock();
    }
  };
  

  struct FloatText : MyFocusSnifferQDoubleSpinBox, Gui{
    Q_OBJECT;
    
  public:
    
    FloatText(double min, double curr, double max, int num_decimals, double step_interval)
      : Gui(this)
    {
      setMinimum(R_MIN(min, max));
      setMaximum(R_MAX(min, max));
      setDecimals(num_decimals);
      if (step_interval <= 0)
        setSingleStep(fabs(max-min) / 20.0);
      else
        setSingleStep(step_interval);
      setValue(curr);
    }

    OVERRIDERS(MyFocusSnifferQDoubleSpinBox);
  };


  struct Table : QTableWidget, Gui{
    Q_OBJECT;

  public:

    int _sorting_disabled = 1; // sorting is disabled if _sorting_disabled > 0
    
    Table(QStringList headers)
      : Gui(this)
    {
      setColumnCount(headers.size());
      setHorizontalHeaderLabels(headers);

      setSelectionBehavior(QAbstractItemView::SelectRows);
      setSelectionMode(QAbstractItemView::SingleSelection);

      for(int y=0;y<headers.size();y++)
        horizontalHeader()->setSectionResizeMode(y, QHeaderView::Interactive);
      //horizontalHeader()->setSectionResizeMode(y, QHeaderView::ResizeToContents);
      //      horizontalHeader()->setSectionResizeMode(y, QHeaderView::Stretch);
      
      //horizontalHeader()->setSectionResizeMode(0, QHeaderView::Stretch);
      //horizontalHeader()->setSectionResizeMode(0, QHeaderView::Interactive);
                          
      //horizontalHeader()->setSectionResizeMode(1, QHeaderView::Interactive);
    }

    virtual void addDoubleClickCallback(func_t* func) override {
      Callback *callback = new Callback(func, _widget);
      connect(this, SIGNAL(cellDoubleClicked(int,int)), callback, SLOT(cellDoubleClicked(int,int)));
      _callbacks.push_back(callback);
    }
    
    virtual dyn_t getGuiValue(void) override {
      dynvec_t ret = {};
      for(const auto *item : selectedItems())
        DYNVEC_push_back(&ret, DYN_create_string(item->text().toUtf8().constData()));
      return DYN_create_array(ret);
    }

  };


  struct MyUiLoader : public QUiLoader{
    MyUiLoader(){
      clearPluginPaths();
    }

    QWidget *createWidget(const QString &className, QWidget *parent = Q_NULLPTR, const QString &name = QString()) override {
      QWidget *ret = NULL;
      
      if (className=="QTextEdit" || className=="QPlainTextEdit")
        ret = new FocusSnifferQTextEdit(parent);
      else if (className=="QLineEdit")
        ret = new MyFocusSnifferQLineEdit(parent);
      else if (className=="QLineEdit")
        ret = new MyFocusSnifferQLineEdit(parent);
      else if (className=="QSpinBox")
        ret = new MyFocusSnifferQSpinBox(parent);
      else if (className=="QDoubleSpinBox")
        ret = new MyFocusSnifferQDoubleSpinBox(parent);
      else
        return QUiLoader::createWidget(className, parent, name);

      ret->setObjectName(name);
      return ret;
    }
  };

  static MyUiLoader g_uiloader;
}


using namespace radium_gui;

int gui_getSystemFontheight(void){
  return root->song->tracker_windows->systemfontheight;
}

void gui_toolTip(const_char* text){
  QToolTip::showText(QCursor::pos(),text,NULL,QRect());
}

const_char* gui_mixColors(const_char* color1, const_char* color2, float how_much_color1){
  QColor col1 = getQColor(color1);
  QColor col2 = getQColor(color2);
  return talloc_strdup(mix_colors(col1, col2, how_much_color1).name().toUtf8().constData());
}

float gui_textWidth(const_char* text){
  return GFX_get_text_width(root->song->tracker_windows, text);
}

static Gui *get_gui_maybeclosed(int64_t guinum){
  if (guinum < 0 || guinum > g_guis.size()){
    handleError("No Gui #%d", guinum);
    return NULL;
  }

  return g_guis[(int)guinum];
}

static Gui *get_gui(int64_t guinum){
  Gui *gui = get_gui_maybeclosed(guinum);

  if (gui==NULL)
    handleError("Gui #%d has been closed and can not be used.", guinum);

  return gui;
}

int64_t gui_ui(const_char *filename){
  
  QFile file(filename);
  if (file.open(QFile::ReadOnly)==false){
    handleError("Unable to open \"%s\": %s", filename, file.errorString().toUtf8().constData());
    return -1;
  }
  
  QWidget *widget = g_uiloader.load(&file);
  file.close();

  if (widget==NULL){
    handleError("Unable to open \"%s\": %s", filename, g_uiloader.errorString().toUtf8().constData());
    return -1;
  }
  
  Gui *gui = new VerticalLayout();
  gui->_widget->layout()->addWidget(widget);
  
  return gui->get_gui_num();
}

int64_t gui_child(int64_t guinum, const_char* childname){
  Gui *gui = get_gui(guinum);

  if (gui==NULL)
    return -1;
  
  QWidget *child = gui->_widget->findChild<QWidget*>(childname);

  if (child==NULL){
    handleError("Could not find child \"%s\" in gui #%d.", childname, guinum);
  }

  for(Gui *existing_child : gui->_children){
    if (existing_child->_widget==child)
      return existing_child->get_gui_num();
  }
  
  Gui *child_gui = new Gui(child);
  gui->_children.push_back(child_gui);
  
  return child_gui->get_gui_num();
}



/////// Callbacks
//////////////////////////


void gui_addDeletedCallback(int64_t guinum, func_t* func){
  Gui *gui = get_gui(guinum);

  if (gui==NULL)
    return;

  s7extra_protect(func);
  gui->_deleted_callbacks.push_back(func);
}
                      
void gui_addRealtimeCallback(int64_t guinum, func_t* func){
  Gui *gui = get_gui(guinum);

  if (gui==NULL)
    return;

  gui->addGuiRealtimeCallback(func);
}

void gui_addCallback(int64_t guinum, func_t* func){
  Gui *gui = get_gui(guinum);

  if (gui==NULL)
    return;

  gui->addGuiCallback(func);
}

void gui_addMouseCallback(int64_t guinum, func_t* func){
  Gui *gui = get_gui(guinum);

  if (gui==NULL)
    return;

  gui->addMouseCallback(func);
}

void gui_addDoubleClickCallback(int64_t guinum, func_t* func){
  Gui *gui = get_gui(guinum);

  if (gui==NULL)
    return;

  gui->addDoubleClickCallback(func);
}

void gui_addCloseCallback(int64_t guinum, func_t* func){
  Gui *gui = get_gui(guinum);

  if (gui==NULL)
    return;

  gui->addCloseCallback(func);
}

void gui_addResizeCallback(int64_t guinum, func_t* func){
  Gui *gui = get_gui(guinum);

  if (gui==NULL)
    return;

  gui->addResizeCallback(func);
}

void gui_addPaintCallback(int64_t guinum, func_t* func){
  Gui *gui = get_gui(guinum);

  if (gui==NULL)
    return;

  gui->addPaintCallback(func);
}


static void updateWidgetRecursively(QWidget *widget){
  if (widget != NULL){
    widget->update();
    
    for(auto *c : widget->children())
      updateWidgetRecursively(dynamic_cast<QWidget*>(c));
  }
}

void gui_updateRecursively(int64_t guinum){
  Gui *gui = get_gui(guinum);

  if (gui==NULL)
    return;

  R_ASSERT_RETURN_IF_FALSE(g_currently_painting==false);

  updateWidgetRecursively(gui->_widget);
}

void gui_update(int64_t guinum, int x1, int y1, int x2, int y2){

  Gui *gui = get_gui(guinum);

  if (gui==NULL)
    return;

  R_ASSERT_RETURN_IF_FALSE(g_currently_painting==false);

  if (x1==-1)
    gui->_widget->update();
  else
    gui->_widget->update(x1, x2, x2-x1, y2-y1);
}



/////// Widgets
//////////////////////////

int64_t gui_widget(int width, int height){
  return (new Widget(width, height))->get_gui_num();
}

int64_t gui_button(const_char *text){
  return (new PushButton(text))->get_gui_num();
}

int64_t gui_checkbox(const_char *text, bool is_checked, bool radium_style){
  //return -1;
  if (radium_style)
    return (new RadiumCheckBox(text, is_checked))->get_gui_num();
  else
    return (new CheckBox(text, is_checked))->get_gui_num();
}

int64_t gui_radiobutton(const_char *text, bool is_checked){
      //return -1;
  return (new RadioButton(text, is_checked))->get_gui_num();
}

int64_t gui_horizontalIntSlider(const_char *text, int min, int curr, int max){
  if(min==max){
    handleError("Gui slider: minimum and maximum value is the same");
    return -1;
  }
  //return -1;
  return (new radium_gui::Slider(Qt::Horizontal, text, min, curr, max, true))->get_gui_num();
}
int64_t gui_horizontalSlider(const_char *text, double min, double curr, double max){
  if(min==max){
    handleError("Gui slider: minimum and maximum value is the same");
    return -1;
  }
  //return -1;
  return (new radium_gui::Slider(Qt::Horizontal, text, min, curr, max, false))->get_gui_num();
}
int64_t gui_verticalIntSlider(const_char *text, int min, int curr, int max){
  if(min==max){
    handleError("Gui slider: minimum and maximum value is the same");
    return -1;
  }
  //return -1;
  return (new radium_gui::Slider(Qt::Vertical, text, min, curr, max, true))->get_gui_num();
}
int64_t gui_verticalSlider(const_char *text, double min, double curr, double max){
  if(min==max){
    handleError("Gui slider: minimum and maximum value is the same");
    return -1;
  }
  //return -1;
  return (new radium_gui::Slider(Qt::Vertical, text, min, curr, max, false))->get_gui_num();
}

int64_t gui_verticalLayout(void){
  //return -1;
  return (new VerticalLayout())->get_gui_num();
}

int64_t gui_horizontalLayout(void){
  //return -1;
  return (new HorizontalLayout())->get_gui_num();
}

int64_t gui_tableLayout(int num_columns){
  //return -1;
  return (new TableLayout(num_columns))->get_gui_num();
}

int64_t gui_group(const_char* title){
  //return -1;
  return (new GroupBox(title))->get_gui_num();
}

int64_t gui_scrollArea(bool scroll_horizontal, bool scroll_vertical){
  return (new ScrollArea(scroll_horizontal, scroll_vertical))->get_gui_num();
}

int64_t gui_verticalScroll(void){
  return (new VerticalScroll())->get_gui_num();
}

int64_t gui_horizontalScroll(void){
  return (new HorizontalScroll())->get_gui_num();
}

int64_t gui_text(const_char* text, const_char* color){
  //return -1;
  return (new Text(text, color))->get_gui_num();
}

int64_t gui_textEdit(const_char* content){
  //return -1;
  return (new TextEdit(content))->get_gui_num();
}

int64_t gui_line(const_char* content){
  //return -1;
  return (new Line(content))->get_gui_num();
}

int64_t gui_intText(int min, int curr, int max){
  //return -1;
  return (new IntText(min, curr, max))->get_gui_num();
}

int64_t gui_floatText(double min, double curr, double max, int num_decimals, double step_interval){
  //return -1;
  return (new FloatText(min, curr, max, num_decimals, step_interval))->get_gui_num();
}

int64_t gui_table(dyn_t header_names){
  if (header_names.type != ARRAY_TYPE){
    handleError("gui_table: Argument must be a list or vector of strings");
    return -1;
  }

  QStringList headers;

  for(int i=0;i<header_names.array->num_elements;i++){
    dyn_t el = header_names.array->elements[i];
    if(el.type != STRING_TYPE){
      handleError(talloc_format("gui_table: Element %d in header_names is not a string", i));
      return -1;
    }
    
    headers << STRING_get_qstring(el.string);
  }

  return (new Table(headers))->get_gui_num();
}

namespace{

  // these classes are used to make sorting table rows work.
  
  struct Pri{
    QString text;
    double _pri;
    Pri(double pri)
      :_pri(pri)
    {}
  };
  
  struct MyNumItem : public QTableWidgetItem, public Pri{
    MyNumItem(double num, bool is_int)
      : QTableWidgetItem(is_int ? QString::number(int(num)) : QString::number(num))
      , Pri(num)
    {}
    bool operator<(const QTableWidgetItem &other) const override{
      const Pri *myother = dynamic_cast<const Pri*>(&other);
      if (myother==NULL)
        return false;
      else
        return _pri < myother->_pri;
    }
  };
  struct MyStringItem : public QTableWidgetItem, public Pri{
    QString _name;
    MyStringItem(QString name)
      : QTableWidgetItem(name)
      , Pri(DBL_MIN)
      , _name(name)
    {}
    bool operator<(const QTableWidgetItem &other) const override{
      const MyStringItem *myother = dynamic_cast<const MyStringItem*>(&other);
      if (myother==NULL){
        const Pri *mypriother = dynamic_cast<const Pri*>(&other);
        if (mypriother!=NULL)
          return _pri < mypriother->_pri; //false; //_name < other.text(); //QTableWidgetItem::operator<(other);
        else
          return true;
      }else if (myother->_name=="")
        return true;
      else if (_name=="")
        return false;
      else
        return _name < myother->_name;
    }
  };

  struct MyGuiItem : public QTableWidgetItem, public Pri{
    MyGuiItem()
      : Pri(DBL_MAX)
    {}
    bool operator<(const QTableWidgetItem &other) const override{
      const Pri *myother = dynamic_cast<const Pri*>(&other);
      if (myother!=NULL)
        return _pri < myother->_pri; //false; //_name < other.text(); //QTableWidgetItem::operator<(other);
      else
        return true;
    }
  };
}

static int64_t add_table_cell(int64_t table_guinum, Gui *cell_gui, QTableWidgetItem *item, int x, int y, bool enabled){
  Gui *table_gui = get_gui(table_guinum);
  if (table_gui==NULL)
    return -1;

  Table *table = dynamic_cast<Table*>(table_gui->_widget);
  if (table==NULL){
    handleError("gui_addTableCell: table %d is not a Table", (int)table_guinum);
    return -1;
  }
  
  if (enabled)
    item->setFlags(Qt::ItemIsSelectable|Qt::ItemIsEnabled);
  else
    item->setFlags(Qt::NoItemFlags);
  
  if (y >= table->rowCount())
    table->setRowCount(y+1);

  table->setItem(y, x, item);

  if (cell_gui==NULL){
    
    if (table->cellWidget(y,x) != NULL)
      table->removeCellWidget(y,x); // is the cell widget deleted now?
    
    return -1;
  }
  
  table->setCellWidget(y, x, cell_gui->_widget);

  return cell_gui->get_gui_num();
}
                              
int64_t gui_addTableGuiCell(int64_t table_guinum, int64_t cell_gui_num, int x, int y, bool enabled){
  Gui *cell_gui = get_gui(cell_gui_num);
  if (cell_gui==NULL)
    return -1;

  auto *item = new MyGuiItem();
  
  return add_table_cell(table_guinum, cell_gui, item, x, y, enabled);
}

int64_t gui_addTableStringCell(int64_t table_guinum, const_char* string, int x, int y, bool enabled){
  QString name(string);
  auto *item = new MyStringItem(name);
    
  //Gui *cell_gui = new Text(name, "");

  return add_table_cell(table_guinum, NULL, item, x, y, enabled);
}

int64_t gui_addTableIntCell(int64_t table_guinum, int64_t num, int x, int y, bool enabled){
  QString name = QString::number(num);
  auto *item = new MyNumItem(num, true);
  item->setFlags(Qt::ItemIsSelectable|Qt::ItemIsEnabled);
    
  //Gui *cell_gui = new Text(name, "");

  return add_table_cell(table_guinum, NULL, item, x, y, enabled);
}

int64_t gui_addTableFloatCell(int64_t table_guinum, double num, int x, int y, bool enabled){
  QString name = QString::number(num);
  auto *item = new MyNumItem(num, false);
  item->setFlags(Qt::ItemIsSelectable|Qt::ItemIsEnabled);
    
  //Gui *cell_gui = new Text(name, "");

  return add_table_cell(table_guinum, NULL, item, x, y, enabled);
}

int gui_getTableRowNum(int64_t table_guinum, int cell_guinum){
  Gui *table_gui = get_gui(table_guinum);
  if (table_gui==NULL)
    return -1;

  Gui *cell_gui = get_gui(cell_guinum);
  if (cell_gui==NULL)
    return -1;

  Table *table = dynamic_cast<Table*>(table_gui->_widget);
  if (table==NULL){
    handleError("gui_addTableCell: table %d is not a Table", (int)table_guinum);
    return -1;
  }

  for(int x=0;x<table->columnCount();x++){
    for(int y=0;y<table->rowCount();y++){
      if(table->cellWidget(y,x)==cell_gui->_widget)
        return y;
    }
  }

  return -1;
}

void gui_addTableRows(int64_t table_guinum, int pos, int how_many){
  Gui *table_gui = get_gui(table_guinum);
  if (table_gui==NULL)
    return;

  Table *table = dynamic_cast<Table*>(table_gui->_widget);
  if (table==NULL){
    handleError("gui_addTableCell: First argument (%d) is not a Table", (int)table_guinum);
    return;
  }

  if (pos < -table->rowCount()){
    handleError("gui_addTableRows: pos (%d) too large. There's only %d rows in this table", pos, table->rowCount());
    pos = table->rowCount();
  }

  if (how_many > 0)
    for(int i=0;i<how_many;i++)
      table->insertRow(pos);
  else if (pos==0 && how_many==-table->rowCount()){
    printf("   CLEARNGING\n");
    table->clearContents();
    table->setRowCount(0);
  }else
    for(int i=0;i<-how_many;i++)
      table->removeRow(pos);
}

int gui_getNumTableRows(int64_t table_guinum){
  Gui *table_gui = get_gui(table_guinum);
  if (table_gui==NULL)
    return 0;

  Table *table = dynamic_cast<Table*>(table_gui->_widget);
  if (table==NULL){
    handleError("gui_getNumTableRows: The argument (%d) is not a Table", (int)table_guinum);
    return 0;
  }

  return table->rowCount();
}

void gui_enableTableSorting(int64_t table_guinum, bool do_sort){
  Gui *table_gui = get_gui(table_guinum);
  if (table_gui==NULL)
    return;

  Table *table = dynamic_cast<Table*>(table_gui->_widget);
  if (table==NULL){
    handleError("gui_addTableCell: table %d is not a Table", (int)table_guinum);
    return;
  }

  table->setSortingEnabled(do_sort);
  
  /*
  if (do_sort)
    table->_sorting_disabled--;
  else
    table->_sorting_disabled++;

  if (table->_sorting_disabled==0)
    table->setSortingEnabled(true);
  if (table->_sorting_disabled==1)
    table->setSortingEnabled(false);
  */
}

void gui_sortTableBy(int64_t table_guinum, int x, bool sort_ascending){
  Gui *table_gui = get_gui(table_guinum);
  if (table_gui==NULL)
    return;

  Table *table = dynamic_cast<Table*>(table_gui->_widget);
  if (table==NULL){
    handleError("gui_addTableCell: table %d is not a Table", (int)table_guinum);
    return;
  }

  if (table->columnCount()==0){
    handleError("gui_addTableRows: There are no columns in this table");
    return;
  }

  if (x < 0){
    handleError("gui_addTableRows: x is negative");
    return;
  }
  
  if (x>table->columnCount()){
    handleError("gui_addTableRows: x (%d) too large. There's only %d columns in this table", x, table->columnCount());
    return;
  }

  table->sortItems(x, sort_ascending ? Qt::AscendingOrder : Qt::DescendingOrder);
}

void gui_stretchTable(int64_t table_guinum, int x, bool do_stretch, int size){
  Gui *table_gui = get_gui(table_guinum);
  if (table_gui==NULL)
    return;

  Table *table = dynamic_cast<Table*>(table_gui->_widget);
  if (table==NULL){
    handleError("gui_addTableCell: table %d is not a Table", (int)table_guinum);
    return;
  }

  if (table->columnCount()==0){
    handleError("gui_addTableRows: There are no columns in this table");
    return;
  }

  if (x < 0){
    handleError("gui_addTableRows: x is negative");
    return;
  }
  
  if (x>table->columnCount()){
    handleError("gui_addTableRows: x (%d) too large. There's only %d columns in this table", x, table->columnCount());
    return;
  }

  if (do_stretch)
    table->horizontalHeader()->setSectionResizeMode(x, QHeaderView::Stretch);
  
  table->horizontalHeader()->resizeSection(x, size);
}

////////////////////////////

void gui_setValue(int64_t guinum, dyn_t value){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  gui->setGuiValue(value);
}

dyn_t gui_getValue(int64_t guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return DYN_create_bool(false);

  return gui->getGuiValue();
}

void gui_add(int64_t parentnum, int64_t childnum, int x1_or_stretch, int y1, int x2, int y2){
  Gui *parent = get_gui(parentnum);
  if (parent==NULL)
    return;

  Gui *child = get_gui(childnum);  
  if (child==NULL)
    return;

  QLayout *layout = parent->getLayout();

  if(layout==NULL || y1!=-1) {

    if (layout==NULL && (y1==-1 || x2==-1 || y2==-1)){
      handleError("Warning: Parent gui #%d does not have a layout", parentnum);
      x1_or_stretch = 0;
      y1 = 0;
    }
    
    int x1 = x1_or_stretch;

    if (x1<0)
      x1 = 0;
    if (y1<0)
      y1 = 0;
    
    ScrollArea *scroll_area = dynamic_cast<ScrollArea*>(parent);
    if (scroll_area != NULL){
      //printf("      Adding to scroll child\n");
      child->_widget->setParent(scroll_area->contents);
      child->_widget->move(x1,y1);
      child->_widget->show();

      int new_width = R_MAX(parent->_widget->width(), x2);
      int new_height = R_MAX(parent->_widget->height(), y2);
      
      //parent->_widget->resize(new_width, new_height);
      scroll_area->contents->resize(new_width, new_height);

    }else{
      child->_widget->setParent(parent->_widget);
      child->_widget->move(x1,y1);
    }

    if (x2>x1 && y2 > y1)
      child->_widget->resize(x2-x1, y2-y1);


    /*
    int new_width = R_MAX(parent->_widget->width(), x2);
    int new_height = R_MAX(parent->_widget->height(), y2);

    parent->_widget->resize(new_width, new_height);
    */

  } else {

    QBoxLayout *box_layout = dynamic_cast<QBoxLayout*>(layout);
    
    if (box_layout!=NULL){

      int stretch = x1_or_stretch == -1 ? 0 : x1_or_stretch;

      box_layout->addWidget(child->_widget, stretch);

    } else {

      if (x1_or_stretch != -1)
        handleError("Warning: Parent gui #%d does not have a box layout", parentnum);

      layout->addWidget(child->_widget);

    }
    
  }
  
}

void gui_replace(int64_t parentnum, int64_t oldchildnum, int64_t newchildnum){
  Gui *parent = get_gui(parentnum);
  if (parent==NULL)
    return;

  Gui *oldchild = get_gui(oldchildnum);
  if (oldchild==NULL)
    return;

  Gui *newchild = get_gui(newchildnum);  
  if (newchild==NULL)
    return;

  QLayout *layout = parent->getLayout();
  if(layout==NULL){
    handleError("Gui #%d does not have a layout", parentnum);
    return;
  }
  
  QLayoutItem *old_item = layout->replaceWidget(oldchild->_widget, newchild->_widget);

  if(old_item==NULL){
    handleError("Gui #%d not found in #%d", oldchildnum, parentnum);
    return;
  }

  delete old_item;
}

bool gui_isVisible(int64_t guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return false;

  return gui->_widget->isVisible();
}

void gui_show(int64_t guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  gui->_widget->show();
}

void gui_hide(int64_t guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  gui->_widget->hide();
}

static void deleteFullscreenParent(Gui *gui);


void gui_close(int64_t guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  if(gui->_full_screen_parent!=NULL)
    deleteFullscreenParent(gui);
  
  gui->_widget->close();
}

bool gui_isOpen(int64_t guinum){
  return get_gui_maybeclosed(guinum)!=NULL;
}

void gui_setAlwaysOnTop(int64_t guinum, int64_t parentnum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  QWidget *parent;
  
  if (parentnum==-1)
    parent = g_main_window;
  else if (parentnum==-2)
    parent = get_current_parent();
  else {
    Gui *gui = get_gui(parentnum);
    if (gui==NULL)
      return;
    parent = gui->_widget;
  }
  
  gui->_widget->setParent(parent, Qt::Window); // get_current_parent() can return anything, but I think the worst thing that could happen if the parent is deleted in this case, is that some warning messages would be displayed. The base case (and I hope only case) is just that the window closes, and that closing the window was the natural thing to happen, since the parent closed.
}

void gui_setModal(int64_t guinum, bool set_modal){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  gui->_widget->setWindowModality(set_modal ? Qt::ApplicationModal : Qt::NonModal);
}

void gui_disableUpdates(int64_t guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  gui->_widget->setUpdatesEnabled(false);
}

void gui_enableUpdates(int64_t guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  gui->_widget->setUpdatesEnabled(true);
}

int gui_width(int64_t guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return 0;

  return gui->_widget->width();
}

int gui_height(int64_t guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return 0;

  QScrollArea *area = dynamic_cast<QScrollArea*>(gui->_widget);
  if (area!=NULL){
    QScrollBar *scrollbar = area->horizontalScrollBar();
    if (scrollbar->isVisible()){
      //printf("  IS VISIBLE %d\n",scrollbar->height());
      return area->widget()->height() - scrollbar->height();
    }else{
      //printf("    NOT NOTNOT IS VISIBLE\n");
      return area->widget()->height();
    }
  }else{
    //printf("   NOT A SCROLL AREA\n");
    return gui->_widget->height();
  }
}

int gui_getX(int64_t guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return 0;

  return gui->_widget->x();
}

int gui_getY(int64_t guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return 0;

  return gui->_widget->y();
}

void gui_moveToParentCentre(int64_t guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  QObject *oparent = gui->_widget->parent();
  if (oparent==NULL){
    handleError("gui_moveToParentCentre: Gui #%d has no parent", guinum);
    return;
  }

  QWidget *parent = dynamic_cast<QWidget*>(oparent);
  if (parent==NULL){
    handleError("gui_moveToParentCentre: Parent of gui #%d is not a widget", guinum);
    return;
  }

  gui->_widget->updateGeometry();
  
  int w = gui->_widget->width();
  int h = gui->_widget->height();
  //printf("w: %d, h: %d\n",w,h);
  gui->_widget->move(parent->x()+parent->width()/2-w/2, parent->y()+parent->height()/2-h/2);
}

void gui_setPos(int64_t guinum, int x, int y){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  gui->_widget->move(x,y);
}

void gui_setSize(int64_t guinum, int width, int height){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  gui->_widget->resize(width, height);
}

bool gui_mousePointsMainlyAt(int64_t guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return false;

  if (gui->_full_screen_parent!=NULL){
    if (gui->_full_screen_parent->window()==QApplication::topLevelAt(QCursor::pos()))
      return true;
    /*
    if (gui->_full_screen_parent->hasFocus())
      return true;
    if (gui->_widget->hasFocus())
      return true;
    */
  }

  return gui->_widget->window()==QApplication::topLevelAt(QCursor::pos());
}

static void deleteFullscreenParent(Gui *gui){
  gui->_full_screen_parent->layout()->removeWidget(gui->_widget);

  gui->_widget->setParent(g_main_window, Qt::Window);
  
  gui->_widget->setGeometry(gui->_orgRect);
  
  delete gui->_full_screen_parent;
  gui->_full_screen_parent = NULL;
}

void gui_setFullScreen(int64_t guinum, bool enable){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  if(enable){

    if(gui->_full_screen_parent!=NULL)
      return;

    gui->_orgRect = gui->_widget->geometry();
      
    gui->_full_screen_parent = new QWidget();

    gui->_full_screen_parent->showFullScreen();
    
    QVBoxLayout *mainLayout = new QVBoxLayout(gui->_full_screen_parent);
    mainLayout->setSpacing(0);
    mainLayout->setContentsMargins(0,0,0,0);

    gui->_full_screen_parent->setLayout(mainLayout);

    mainLayout->addWidget(gui->_widget);

#if defined(FOR_WINDOWS)
    OS_WINDOWS_set_key_window((void*)gui->_full_screen_parent->winId()); // To avoid losing keyboard focus
#endif
    
  }else{

    if(gui->_full_screen_parent==NULL)
      return;

    deleteFullscreenParent(gui);
    
    gui->_widget->show();
  }
  
}
  
bool gui_isFullScreen(int64_t guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return false;

  //return gui->_widget->isFullScreen();
  return gui->_full_screen_parent!=NULL;
}
  
void gui_setBackgroundColor(int64_t guinum, const_char* color){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  QPalette pal = gui->_widget->palette();
  pal.setColor(QPalette::Background, QColor(color));
  pal.setColor(QPalette::Base, QColor(color));
  //gui->_widget->setAutoFillBackground(true);
  gui->_widget->setPalette(pal);
}

const_char* gui_getBackgroundColor(int64_t guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return "black";

  return talloc_strdup(gui->_widget->palette().color(gui->_widget->backgroundRole()).name().toUtf8().constData());
}

void gui_setLayoutSpacing(int64_t guinum, int spacing, int left, int top, int right, int bottom){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  QLayout *layout = gui->getLayout();
  if (layout==NULL){
    handleError("Gui #%d doesn't have a layout", guinum);
    return;
  }

  layout->setSpacing(spacing);
  layout->setContentsMargins(left, top, right, bottom);
}

static inline QSizePolicy::Policy get_grow_policy_from_bool(bool grow){
  return grow ? QSizePolicy::MinimumExpanding : QSizePolicy::Fixed;
}

void gui_addLayoutSpace(int64_t guinum, int width, int height, bool grow_horizontally, bool grow_vertically){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  QLayout *layout = gui->getLayout();
  if (layout==NULL){
    handleError("Gui #%d doesn't have a layout", guinum);
    return;
  }

  layout->addItem(new QSpacerItem(width,height,
                                  get_grow_policy_from_bool(grow_horizontally),get_grow_policy_from_bool(grow_vertically)
                                  )
                  );
}

void gui_setSizePolicy(int64_t guinum, bool grow_horizontally, bool grow_vertically){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  auto *scroll = dynamic_cast<VerticalScroll*>(gui->_widget);
  if (scroll!=NULL)
    scroll->contents->setSizePolicy(get_grow_policy_from_bool(grow_horizontally),
                                    get_grow_policy_from_bool(grow_vertically));
  else
    gui->_widget->setSizePolicy(get_grow_policy_from_bool(grow_horizontally),
                                get_grow_policy_from_bool(grow_vertically));
                              
}

void gui_setMinWidth(int64_t guinum, int minwidth){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  auto *scroll = dynamic_cast<VerticalScroll*>(gui->_widget);
  if (scroll!=NULL)
    scroll->contents->setMinimumWidth(minwidth);
  else
    gui->_widget->setMinimumWidth(minwidth);
}

void gui_setMinHeight(int64_t guinum, int minheight){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  gui->_widget->setMinimumHeight(minheight);
}

void gui_setMaxWidth(int64_t guinum, int minwidth){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  auto *scroll = dynamic_cast<VerticalScroll*>(gui->_widget);
  if (scroll!=NULL)
    scroll->contents->setMaximumWidth(minwidth);
  else
    gui->_widget->setMaximumWidth(minwidth);
}

void gui_setMaxHeight(int64_t guinum, int minheight){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  gui->_widget->setMaximumHeight(minheight);
}

void gui_setEnabled(int64_t guinum, bool is_enabled){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  gui->_widget->setEnabled(is_enabled);
}


// audio meter
////////////////

int64_t gui_verticalAudioMeter(int instrument_id){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return -1;

  return (new VerticalAudioMeter(patch))->get_gui_num();
}

void gui_addAudioMeterPeakCallback(int guinum, func_t* func){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;
  
  VerticalAudioMeter *meter = dynamic_cast<VerticalAudioMeter*>(gui->_widget);
  if (meter==NULL){
    handleError("Gui #%d is not an audio meter", guinum);
    return;
  }

  meter->addPeakCallback(func, guinum);
}

void gui_resetAudioMeterPeak(int guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;
  
  VerticalAudioMeter *meter = dynamic_cast<VerticalAudioMeter*>(gui->_widget);
  if (meter==NULL){
    handleError("Gui #%d is not an audio meter", guinum);
    return;
  }

  meter->resetPeak();
}


///////////////// Mixer strips
///////////////////////////////

static QVector<int64_t> g_mixerstrip_guinums;
void informAboutGuiBeingAMixerStrips(int64_t guinum){
  g_mixerstrip_guinums.push_back(guinum);
}

int64_t createMixerStripsWindow(int num_rows){
  return s7extra_callFunc2_int_int("create-mixer-strips-gui", num_rows);
}

int64_t showMixerStrips(int num_rows){
  int64_t gui = createMixerStripsWindow(num_rows);
  if (gui!=-1)
    gui_show(gui);
  return gui;
}

QVector<QWidget*> MIXERSTRIPS_get_all_widgets(void){ 
  QVector<QWidget*> ret;
  QVector<int64_t> to_remove;

  for(int64_t guinum : g_mixerstrip_guinums){

    Gui *gui = g_guis[(int)guinum];
    
    if (gui==NULL)     
      to_remove.push_back(guinum);
    else if (gui->_full_screen_parent != NULL)
      ret.push_back(gui->_full_screen_parent);
    else
      ret.push_back(gui->_widget);
  }

  for(int64_t guinum : to_remove)
    g_mixerstrip_guinums.removeOne(guinum);

  return ret;
}


QWidget *MIXERSTRIPS_get_curr_widget(void){
  QVector<int64_t> to_remove;

  QWidget *ret = NULL;
  
  for(int64_t guinum : g_mixerstrip_guinums){

    Gui *gui = g_guis[(int)guinum];
    
    if (gui==NULL) {
     
      to_remove.push_back(guinum);

    } else {

      QWidget *widget = gui->_widget;
      if (gui->_full_screen_parent != NULL)
        widget = gui->_full_screen_parent;
      
      QPoint p = widget->mapFromGlobal(QCursor::pos());
      int x = p.x();
      int y = p.y();

      //printf("                  MIXERSTRIPS_get_curr_widget. x: %d, y: %d, width: %d, height: %d\n", x,y,widget->width(),widget->height());
      
      if (true
          && x >= 0
          && x <  widget->width()
          && y >= 0
          && y <  widget->height()
          )
        {
          ret = widget;
          break;
        }
    }
  }

  for(int64_t guinum : to_remove)
    g_mixerstrip_guinums.removeOne(guinum);
  
  return ret;  
}

bool MIXERSTRIPS_has_mouse_pointer(void){
  return MIXERSTRIPS_get_curr_widget() != NULL;  
}

///////////////// Drawing
/////////////////////////////

void gui_drawLine(int64_t guinum, const_char* color, float x1, float y1, float x2, float y2, float width){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  gui->drawLine(color, x1, y1, x2, y2, width);
}

void gui_drawBox(int64_t guinum, const_char* color, float x1, float y1, float x2, float y2, float width, float round_x, float round_y){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  gui->drawBox(color, x1, y1, x2, y2, width, round_x, round_y);
}

void gui_filledBox(int64_t guinum, const_char* color, float x1, float y1, float x2, float y2, float round_x, float round_y){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  gui->filledBox(color, x1, y1, x2, y2, round_x, round_y);
}

void gui_drawText(int64_t guinum, const_char* color, const_char *text, float x1, float y1, float x2, float y2, bool wrap_lines, bool align_top, bool align_left) {
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  gui->drawText(color, text, x1, y1, x2, y2, wrap_lines, align_top, align_left, false);
}

void gui_drawVerticalText(int64_t guinum, const_char* color, const_char *text, float x1, float y1, float x2, float y2, bool wrap_lines, bool align_top, bool align_left) {
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  gui->drawText(color, text, x1, y1, x2, y2, wrap_lines, align_top, align_left, true);
}


/////////////

void obtainKeyboardFocus(void){
  obtain_keyboard_focus_counting();
}

void releaseKeyboardFocus(void){
  release_keyboard_focus_counting();
}


////////////

void API_gui_call_regularly(void){
  for(auto *meter : g_active_vertical_audio_meters)
    meter->call_regularly();
}

QWidget *API_gui_get_widget(int64_t guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return NULL;

  return gui->_widget;
}

bool API_gui_is_painting(void){
  return g_currently_painting;
}
  


///////////////// Mixer strips
//////////////////////////////


int64_t gui_createSingleMixerStrip(int64_t instrument_id, int width, int height){
  struct Patch *patch = getAudioPatchFromNum(instrument_id);
  if(patch==NULL)
    return -1;

  return s7extra_callFunc2_int_int_int_int("create-standalone-mixer-strip", instrument_id, width, height);
}

#include "mapi_gui.cpp"


///////////////// SoundPluginRegistry
/////////////////////////////////////

#include "../audio/SoundPluginRegistry_proc.h"

static QString extend_path(const QString a, const QString b){
  if (a=="")
    return b;
  else
    return a + " / " + b;
}

static QString get_path(const QStack<QString> &dir){
  QString ret;
  for(QString s : dir)
    ret = extend_path(ret, s);
  return ret;
}

static void push_type(hash_t *hash, const SoundPluginType *type, const QString path){
  if (!HASH_has_key(hash, ":type"))
    HASH_put_string(hash, ":type", PluginMenuEntry::type_to_string(PluginMenuEntry::IS_NORMAL));
  HASH_put_string(hash, ":path", path);
  HASH_put_string(hash, ":type-name", type->type_name);
  HASH_put_string(hash, ":name", type->name);
  HASH_put_int(hash, ":num-inputs", type->num_inputs);
  HASH_put_int(hash, ":num-outputs", type->num_outputs);
  HASH_put_chars(hash, ":category", type->category==NULL ? "" : type->category);
  HASH_put_chars(hash, ":creator", type->creator==NULL ? "" : type->creator);
  HASH_put_int(hash, ":num-uses", type->num_uses);
}


static void add_container_entries(dynvec_t &ret, const SoundPluginTypeContainer *plugin_type_container, const QString path){
  for(int i=0 ; i < plugin_type_container->num_types ; i++){
    const SoundPluginType *type = plugin_type_container->plugin_types[i];
    hash_t *new_hash = HASH_create(5);
    push_type(new_hash, type, extend_path(path, type->name));
    DYNVEC_push_back(&ret, DYN_create_hash(new_hash));
    //printf("Pushing back %s\n", type->name);
  }
}
                                  
static void add_entry(dynvec_t &ret, const PluginMenuEntry &entry, const QString path){

  hash_t *hash = HASH_create(5);
  HASH_put_string(hash, ":type", PluginMenuEntry::type_to_string(entry));
      
  if (entry.type==PluginMenuEntry::IS_CONTAINER && entry.plugin_type_container->is_populated){

    add_container_entries(ret, entry.plugin_type_container, path);
    hash=NULL;
        
  } else if (entry.type==PluginMenuEntry::IS_CONTAINER){
        
    HASH_put_string(hash, ":type-name", entry.plugin_type_container->type_name);
    HASH_put_string(hash, ":name", entry.plugin_type_container->name);
    HASH_put_string(hash, ":path",  extend_path(path, entry.plugin_type_container->name));
    HASH_put_int(hash, ":num-uses", entry.plugin_type_container->num_uses);
    
  } else if (entry.type==PluginMenuEntry::IS_LEVEL_UP){
    HASH_put_string(hash, ":name", entry.level_up_name);
  
  } else if (entry.type==PluginMenuEntry::IS_NUM_USED_PLUGIN){
    HASH_put_string(hash, ":container-name", entry.hepp.container_name);
    HASH_put_string(hash, ":type-name", entry.hepp.type_name);
    HASH_put_string(hash, ":name", entry.hepp.name);
    HASH_put_int(hash, ":num-uses", entry.hepp.num_uses);
    
  } else if (entry.type==PluginMenuEntry::IS_NORMAL){
        
    if (entry.plugin_type!=NULL){
      push_type(hash, entry.plugin_type, extend_path(path, entry.plugin_type->name));
    }else{
      R_ASSERT(false);
      hash = NULL;
    }
        
  }

  if (hash!=NULL){
    DYNVEC_push_back(&ret, DYN_create_hash(hash));
    //printf("Pushing back %p\n", hash);
  }
}

dyn_t getSoundPluginRegistry(bool only_normal_and_containers){
  const QVector<PluginMenuEntry> entries = PR_get_menu_entries();

  dynvec_t ret = {};

  QStack<QString> dir;
  
  for(const PluginMenuEntry &entry : entries){
    
    switch(entry.type){
      case PluginMenuEntry::IS_LEVEL_UP:
        dir.push(entry.level_up_name);
        break;
      case PluginMenuEntry::IS_LEVEL_DOWN:
        dir.pop();
        break;
      default:
        break;
    }

    if (!only_normal_and_containers || entry.type==PluginMenuEntry::IS_NORMAL || entry.type==PluginMenuEntry::IS_CONTAINER)
      add_entry(ret, entry, get_path(dir));
  }

  return DYN_create_array(ret);
}

dyn_t populatePluginContainer(dyn_t entry){
  dynvec_t ret = {};

  if (entry.type!=HASH_TYPE){
    handleError("openPluginContainer: argument is not a hash table");
    goto exit;
  }

  {
    const hash_t *hash=entry.hash;
    const char *name = HASH_get_chars(hash, ":name");
    QString type = HASH_get_chars(hash, ":type");
    const char *type_name = HASH_get_chars(hash, ":type-name");
    const char *path = HASH_get_chars(hash, ":path");
  
    if (PluginMenuEntry::type_to_string(PluginMenuEntry::IS_CONTAINER) != type){
      handleError("openPluginContainer: Excpected %s, found %s", PluginMenuEntry::type_to_string(PluginMenuEntry::IS_CONTAINER).toUtf8().constData(), HASH_get_chars(hash, ":type"));
      goto exit;
    }
  
    if(name==NULL){
      handleError("Missing :name");
      goto exit;
    }
  
    if(type_name==NULL){
      handleError("Missing :type-name");
      goto exit;
    }
  
    if(path==NULL){
      handleError("Missing :path");
      goto exit;
    }

    {
      SoundPluginTypeContainer *container = PR_get_container_by_name(name, type_name);
      if (container==NULL){
        handleError("Could not find container %s / %s", name, type_name);
        goto exit;
      }
    
      PR_populate(container);

      add_container_entries(ret, container, path);
    }
  }
  
 exit:
  return DYN_create_array(ret);
}

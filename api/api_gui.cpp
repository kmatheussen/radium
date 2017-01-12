
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
#include <QGroupBox>
#include <QPlainTextEdit>
#include <QScrollArea>
#include <QUiLoader>

#include "../common/nsmtracker.h"

#include "../Qt/FocusSniffers.h"

#include "../common/visual_proc.h"
#include "../embedded_scheme/s7extra_proc.h"

#include "../Qt/Qt_MyQSlider.h"
#include "../Qt/Qt_mix_colors.h"

#include "api_common_proc.h"

#include "radium_proc.h"
#include "api_gui_proc.h"


#define MOUSE_OVERRIDERS(classname)                                     \
  void mousePressEvent(QMouseEvent *event) override{                    \
    if (_mouse_callback==NULL || !Gui::mousePressEvent(event))          \
      classname::mousePressEvent(event);                                \
  }                                                                     \
                                                                        \
  void mouseReleaseEvent(QMouseEvent *event) override {                 \
    if (_mouse_callback==NULL || !Gui::mouseReleaseEvent(event))        \
      classname::mouseReleaseEvent(event);                              \
  }                                                                     \
                                                                        \
  void mouseMoveEvent(QMouseEvent *event) override{                     \
    if (_mouse_callback==NULL || !Gui::mouseMoveEvent(event))           \
      classname::mouseMoveEvent(event);                                 \
  }

#define RESIZE_OVERRIDER(classname)                                     \
  void resizeEvent( QResizeEvent *event) override {                     \
    if (_image!=NULL)                                                   \
      setNewImage(event->size().width(), event->size().height());       \
    if (_resize_callback==NULL)                                         \
      classname::resizeEvent(event);                                    \
    else                                                                \
      Gui::resizeEvent(event);                                          \
  }                                                                     

#define PAINT_OVERRIDER(classname)                                      \
  void paintEvent(QPaintEvent *ev) override {                           \
    if(_image!=NULL){                                                   \
      QPainter p(this);                                                 \
      p.drawImage(ev->rect().topLeft(), *_image, ev->rect());           \
    }else                                                               \
      classname::paintEvent(ev);                                        \
  }                                                                     

#define OVERRIDERS(classname)                                           \
  MOUSE_OVERRIDERS(classname)                                           \
  RESIZE_OVERRIDER(classname)                                           \
  PAINT_OVERRIDER(classname)


static float gain2db(float val){
  if(val<=0.0f)
    return -100.0f;

  return 20*log10(val);
}

static float db2linear(float db, float min, float max){
  if(db<-70)
    return min;
  else if(db>40)
    return max;
  else
    return scale(db,-70.0f,40.0f,min,max);
}

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
    

  };

  
  struct Gui {

    QVector<Callback*> _callbacks;
 
  public:
    int _gui_num;
    QWidget *_widget;

    QVector<Gui*> _children;
    QVector<func_t*> _close_callbacks;
    
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

      for(func_t *func : _close_callbacks){
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

      if (_resize_callback!=NULL)
        s7extra_unprotect(_resize_callback);

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


    /************ RESIZE *******************/
    
    func_t *_resize_callback = NULL;
    
    void resizeEvent(QResizeEvent *event){
      R_ASSERT_RETURN_IF_FALSE(_resize_callback!=NULL);

      event->accept();

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

    QImage *_image = NULL;
    QPainter *_image_painter = NULL;

    void setNewImage(int width, int height){
      auto *new_image = new QImage(width, height, QImage::Format_ARGB32);
      auto *new_image_painter = new QPainter(new_image);

      new_image_painter->fillRect(QRect(0,0,width,height), get_qcolor(LOW_BACKGROUND_COLOR_NUM));
      
      if (_image!=NULL)
        new_image_painter->drawImage(QPoint(0,0), *_image);
      
      new_image_painter->setRenderHints(QPainter::Antialiasing,true);

      delete _image_painter;
      delete _image;
      _image_painter = new_image_painter;
      _image = new_image;

      _widget->setAttribute(Qt::WA_OpaquePaintEvent);
    }
    
    
    void setPen(const_char* color){
      _image_painter->setPen(getQColor(color));
    }

    void myupdate(float x1, float y1, float x2, float y2, float extra=0.0f){
      float min_x = R_MIN(x1, x2) - extra;
      float max_x = R_MAX(x1, x2) + extra;
      float min_y = R_MIN(y1, y2) - extra;
      float max_y = R_MAX(y1, y2) + extra;
      _widget->update(min_x-1, min_y-1, max_x-min_x+2, max_y-min_y+2);
    }

    void drawLine(const_char* color, float x1, float y1, float x2, float y2, float width) {
      if (_image==NULL)
        setNewImage(_widget->width(), _widget->height());
      
      QLineF line(x1, y1, x2, y2);

      QPen pen = getPen(color);
      pen.setWidthF(width);
      _image_painter->setPen(pen);
      
      //printf("Color: %x, %s\n", (unsigned int)color, pen.color().name(QColor::HexArgb).toUtf8().constData());

      _image_painter->drawLine(line);

      myupdate(x1, y1, x2, y2, width);
    }

    void filledBox(const_char* color, float x1, float y1, float x2, float y2) {
      if (_image==NULL)
        setNewImage(_widget->width(), _widget->height());
      
      QRectF rect(x1, y1, x2-x1, y2-y1);

      QColor qcolor = getQColor(color);

      //QPen pen = _image_rect.pen();

      _image_painter->setPen(Qt::NoPen);
      _image_painter->setBrush(qcolor);
      _image_painter->drawRect(rect);
      _image_painter->setBrush(Qt::NoBrush);
      //_image_painter->setPen(pen);

      myupdate(x1, y1, x2, y2);
    }

    void drawText(const_char* color, const_char *text, float x1, float y1, float x2, float y2, bool align_top_left) {
      if (_image==NULL)
        setNewImage(_widget->width(), _widget->height());
      
      QRectF rect(x1, y1, x2-x1, y2-y1);

      setPen(color);

      if (align_top_left)
        _image_painter->drawText(rect, text);
      else
        _image_painter->drawText(rect, text, QTextOption(Qt::AlignCenter));

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
          s7extra_callFunc_void_charpointer(func, line_edit->text().toUtf8().constData());
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
          width=QWidget::width();
        if(height<=0)
          height=QWidget::height();
        resize(width,height);
      }
    }

    ~Widget(){
    }

    OVERRIDERS(QWidget);
  };

  
  struct VerticalAudioMeter : QWidget, Gui{
    Q_OBJECT;

    struct Patch *_patch;
    float *_pos = NULL;
    
    int _num_channels = 0;
    bool _is_input = false;
    bool _is_output = false;
    
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

      if (_num_channels > 0)
        _pos = (float*)calloc(_num_channels, sizeof(float));

      call_regularly(); // Set meter positions.

      g_active_vertical_audio_meters.push_back(this);
    }

    ~VerticalAudioMeter(){
      R_ASSERT(g_active_vertical_audio_meters.removeOne(this));
      free(_pos);
    }
    
    MOUSE_OVERRIDERS(QWidget);
    RESIZE_OVERRIDER(QWidget);

    float get_x1(int ch){
      return scale(ch, 0, _num_channels, 1, width()-2);
    }

    float get_x2(int ch){
      return scale(ch+1, 0, _num_channels, 1, width()-2);
    }

    void call_regularly(void){
      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
      if(plugin==NULL)
        return;

      for(int ch=0 ; ch < _num_channels ; ch++){
        float prev_pos = _pos[ch];

        float gain;

        if (_is_output)
          gain = plugin->volume_peak_values[ch];
        else if (_is_input)
          gain = plugin->input_volume_peak_values[ch];
        else{
          gain = 0.0f;
          R_ASSERT(false);
        }
        
        float db = gain2db(gain);

        float pos = db2linear(db, height()-1, 1);
        _pos[ch] = pos;

        if (pos < prev_pos){
          update(get_x1(ch),   floorf(pos),
                 get_x2(ch),   floorf(prev_pos)+1);
        } else if (pos > prev_pos){
          update(get_x1(ch),   floorf(prev_pos),
                 get_x2(ch),   floorf(pos)+1);
        }
      }
    }
    
    void paintEvent(QPaintEvent *ev) override {
      QPainter p(this);

      QColor qcolor1("black");
      QColor qcolor2("green");

      p.setPen(Qt::NoPen);
        
      for(int ch=0 ; ch < _num_channels ; ch++){
        float pos = _pos[ch];
        QRectF rect1(get_x1(ch),  1,
                     get_x2(ch),  pos-1);
        QRectF rect2(get_x1(ch),  pos,
                     get_x2(ch),  height()-pos-1);
        
        p.setBrush(qcolor1);
        p.drawRect(rect1);
        
        p.setBrush(qcolor2);
        p.drawRect(rect2);
      }

      p.setBrush(Qt::NoBrush);
      
      //border
      p.setPen(QColor("gray"));
      p.drawRect(0,0,width()-1,height()-1);
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

  struct ScrollArea : QScrollArea, Gui{
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

      contents = new QWidget;//(this);
      //contents->resize(500,500);
      //contents->show();
      //contents->setLayout(new QVBoxLayout);

      setWidget(contents);
    }

    OVERRIDERS(QScrollArea);
  };

  struct VerticalScroll : QScrollArea, Gui{
    QWidget *contents;
    const char *magic = "magic2";
    QLayout *mylayout;

    VerticalScroll(void)
      : Gui(this)        
    {
      horizontalScrollBar()->setObjectName("horizontalScrollBar");
      verticalScrollBar()->setObjectName("verticalScrollBar");

      //setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
      setWidgetResizable(true);

      QWidget *contents = new QWidget(this);

      mylayout = new QVBoxLayout(contents);
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

const_char* gui_mixColors(const_char* color1, const_char* color2, float how_much_color1){
  QColor col1 = getQColor(color1);
  QColor col2 = getQColor(color2);
  return talloc_strdup(mix_colors(col1, col2, how_much_color1).name().toUtf8().constData());
}

static Gui *get_gui(int64_t guinum){
  if (guinum < 0 || guinum > g_guis.size()){
    handleError("No Gui #%d", guinum);
    return NULL;
  }

  Gui *gui = g_guis[(int)guinum];

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

void gui_addCloseCallback(int64_t guinum, func_t* func){
  Gui *gui = get_gui(guinum);

  if (gui==NULL)
    return;

  s7extra_protect(func);
  gui->_close_callbacks.push_back(func);
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

void gui_addResizeCallback(int64_t guinum, func_t* func){
  Gui *gui = get_gui(guinum);

  if (gui==NULL)
    return;

  gui->addResizeCallback(func);
}

int64_t gui_button(const_char *text){
  return (new PushButton(text))->get_gui_num();
}

int64_t gui_checkbox(const_char *text, bool is_checked){
  //return -1;
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

void gui_add(int64_t parentnum, int64_t childnum, int x1, int y1, int x2, int y2){
  Gui *parent = get_gui(parentnum);
  if (parent==NULL)
    return;

  Gui *child = get_gui(childnum);  
  if (child==NULL)
    return;

  QLayout *layout = parent->getLayout();

  if(layout==NULL || x1!=-1) {

    if (layout==NULL && x1==-1){
      handleError("Warning: Parent gui #%d does not have a layout", parentnum);
      x1 = 0;
      y1 = 0;
    }

    if (x1<0)
      x1 = 0;
    if (y1<0)
      y1 = 0;
    
    ScrollArea *scroll_area = dynamic_cast<ScrollArea*>(parent);
    if (scroll_area != NULL){
      printf("      Adding to scroll child\n");
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
    
    layout->addWidget(child->_widget);
    
  }
  
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

void gui_close(int64_t guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  gui->_widget->close();
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

  
  
void gui_setBackgroundColor(int64_t guinum, const_char* color){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  QPalette pal = gui->_widget->palette();
  pal.setColor(QPalette::Background, QColor(color));
  pal.setColor(QPalette::Base, QColor(color));
  gui->_widget->setAutoFillBackground(true);
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

void gui_addLayoutSpace(int64_t guinum, int width, int height){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  QLayout *layout = gui->getLayout();
  if (layout==NULL){
    handleError("Gui #%d doesn't have a layout", guinum);
    return;
  }

  layout->addItem(new QSpacerItem(width,height,QSizePolicy::MinimumExpanding,QSizePolicy::MinimumExpanding));
}

void gui_setSizePolicy(int64_t guinum, bool grow_horizontally, bool grow_vertically){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  gui->_widget->setSizePolicy(grow_horizontally ? QSizePolicy::Expanding : QSizePolicy::Fixed,
                              grow_vertically ? QSizePolicy::Expanding : QSizePolicy::Fixed);
                              
}

void gui_setMinWidth(int64_t guinum, int minwidth){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

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

  gui->_widget->setMaximumWidth(minwidth);
}

void gui_setMaxHeight(int64_t guinum, int minheight){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  gui->_widget->setMaximumHeight(minheight);
}

// canvas
///////////

int64_t gui_widget(int width, int height){
  return (new Widget(width, height))->get_gui_num();
}

int64_t gui_verticalAudioMeter(int instrument_id){
  struct Patch *patch = getPatchFromNum(instrument_id);
  if(patch==NULL)
    return -1;

  return (new VerticalAudioMeter(patch))->get_gui_num();
}

void gui_drawLine(int64_t guinum, const_char* color, float x1, float y1, float x2, float y2, float width){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  gui->drawLine(color, x1, y1, x2, y2, width);
}

void gui_filledBox(int64_t guinum, const_char* color, float x1, float y1, float x2, float y2){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  gui->filledBox(color, x1, y1, x2, y2);
}

void gui_drawText(int64_t guinum, const_char* color, const_char *text, float x1, float y1, float x2, float y2, bool align_top_left) {
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  gui->drawText(color, text, x1, y1, x2, y2, align_top_left);
}

void API_gui_call_regularly(void){
  for(auto *meter : g_active_vertical_audio_meters)
    meter->call_regularly();
}

#include "mapi_gui.cpp"


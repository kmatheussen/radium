
#include "../common/includepython.h"


#include <QWidget>
#include <QCloseEvent>
#include <QPushButton>
#include <QCheckBox>
#include <QRadioButton>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QGroupBox>
#include <QPlainTextEdit>
#include <QUiLoader>

#include "../common/nsmtracker.h"

#include "../Qt/FocusSniffers.h"

#include "../common/visual_proc.h"
#include "../embedded_scheme/s7extra_proc.h"

#include "../Qt/Qt_MyQSlider.h"

#include "api_common_proc.h"

#include "radium_proc.h"



namespace radium_gui{

struct Gui;

static QVector<Gui*> g_guis;

  /*
  struct SetValueClass{
    virtual void setValue(double val){
      R_ASSERT(false);
    }
  };
  */

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

    QVector<Gui*> children;
    
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
    }

    virtual ~Gui(){
      R_ASSERT_RETURN_IF_FALSE(g_guis.contains(this));
      R_ASSERT_RETURN_IF_FALSE(g_guis[_gui_num] != NULL);
                               
      printf("Deleting Gui %p\n",this);

      for(Gui *child : children)
        delete child;

      for(Callback *callback : _callbacks)
        delete callback;
      
      g_guis[_gui_num] = NULL;
    }

    virtual void addMouseCallback(func_t* func){
      handleError("gui %d doesn't have addMouseCallback method.", _gui_num);
    }

    virtual void drawLine(const_char* color, float x1, float y1, float x2, float y2, float width){
      handleError("gui %d doesn't have drawLine method.", _gui_num);
    }

    virtual void filledBox(const_char* color, float x1, float y1, float x2, float y2){
      handleError("gui %d doesn't have filledBox method.", _gui_num);
    }

    virtual void drawText(const_char* color, const_char *text, float x1, float y1, float x2, float y2){
      handleError("gui %d doesn't have drawText method.", _gui_num);
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
  

  struct Canvas : QWidget, Gui{
    Q_OBJECT;

    QImage *_image = NULL;
    QPainter *_image_painter = NULL;

    func_t *_mouse_callback = NULL;

  public:
    
    Canvas(int width, int height)
      : Gui(this)
    {
      setSize(width, height);

      setAttribute(Qt::WA_OpaquePaintEvent);
    }

    ~Canvas(){
      if (_mouse_callback!=NULL)
        s7extra_unprotect(_mouse_callback);
      delete _image_painter;
      delete _image;
    }
    
    int _currentButton = 0;

    int getMouseButtonEventID( QMouseEvent *qmouseevent){
      if(qmouseevent->button()==Qt::LeftButton)
        return TR_LEFTMOUSEDOWN;
      else if(qmouseevent->button()==Qt::RightButton)
        return TR_RIGHTMOUSEDOWN;
      else if(qmouseevent->button()==Qt::MiddleButton)
        return TR_MIDDLEMOUSEDOWN;
      else
        return 0;
    }

    void mousePressEvent(QMouseEvent *event) override{
      event->accept();
      if (_mouse_callback==NULL)
        return;

      _currentButton = getMouseButtonEventID(event);
      const QPoint &point = event->pos();

      s7extra_callFunc_void_int_float_float(_mouse_callback, _currentButton, point.x(), point.y());
      printf("  Press. x: %d, y: %d. This: %p\n", point.x(), point.y(), this);
    }

    void mouseReleaseEvent(QMouseEvent *event) override{
      event->accept();
      if (_mouse_callback==NULL)
        return;

      const QPoint &point = event->pos();
      s7extra_callFunc_void_int_float_float(_mouse_callback, _currentButton, point.x(), point.y());

      _currentButton = 0;
      printf("  Release. x: %d, y: %d. This: %p\n", point.x(), point.y(), this);
    }

    void mouseMoveEvent(QMouseEvent *event) override{
      event->accept();
      if (_mouse_callback==NULL)
        return;

      const QPoint &point = event->pos();
      s7extra_callFunc_void_int_float_float(_mouse_callback, _currentButton, point.x(), point.y());
      printf("    move. x: %d, y: %d. This: %p\n", point.x(), point.y(), this);
    }

    void addMouseCallback(func_t* func) override{      
      if (_mouse_callback!=NULL){
        handleError("Canvas gui %d already has a mouse callback.", _gui_num);
        return;
      }

      _mouse_callback = func;
      s7extra_unprotect(_mouse_callback);
      setMouseTracking(true);
    }

    void setSize(int width, int height){
      delete _image_painter;
      delete _image;

      _image = new QImage(width, height, QImage::Format_ARGB32);
      _image_painter = new QPainter(_image);

      _image_painter->setRenderHints(QPainter::Antialiasing,true);

      _image_painter->fillRect(QRect(0,0,width,height), get_qcolor(LOW_BACKGROUND_COLOR_NUM));
      setFixedSize(width, height);
    }

    void paintEvent ( QPaintEvent * ev ) override {
      QPainter p(this);

      p.drawImage(ev->rect(), *_image, ev->rect());
    }

    QColor getQColor(int64_t color){
#if QT_VERSION >= 0x056000
      return QColor(QRgba64::fromRgba64(color));
#else
      QColor col((uint32_t)color);
      col.setAlpha(int(color>>24));
      return col;
#endif
    }

    QColor getQColor(const_char* color){
      return QColor(color);
    }

    QPen getPen(const_char* color){
      QPen pen(getQColor(color));
      
      return pen;
    }

    void setPen(const_char* color){
      _image_painter->setPen(getQColor(color));
    }

    void myupdate(float x1, float y1, float x2, float y2, float extra=0.0f){
      float min_x = R_MIN(x1, x2) - extra;
      float max_x = R_MAX(x1, x2) + extra;
      float min_y = R_MIN(y1, y2) - extra;
      float max_y = R_MAX(y1, y2) + extra;
      update(min_x, min_y, max_x-min_x, max_y-min_y);
    }

    void drawLine(const_char* color, float x1, float y1, float x2, float y2, float width) override {
      QLineF line(x1, y1, x2, y2);

      QPen pen = getPen(color);
      pen.setWidthF(width);
      _image_painter->setPen(pen);
      
      //printf("Color: %x, %s\n", (unsigned int)color, pen.color().name(QColor::HexArgb).toUtf8().constData());

      _image_painter->drawLine(line);

      myupdate(x1, y1, x2, y2, width);
    }

    void filledBox(const_char* color, float x1, float y1, float x2, float y2) override {
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

    void drawText(const_char* color, const_char *text, float x1, float y1, float x2, float y2) override {
      QRectF rect(x1, y1, x2-x1, y2-y1);

      setPen(color);

      _image_painter->drawText(rect, text);

      myupdate(x1, y1, x2, y2);
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
  };
  
  struct VerticalLayout : QWidget, Gui{
    VerticalLayout()
      : Gui(this)
    {
      QVBoxLayout *mainLayout = new QVBoxLayout;      
      setLayout(mainLayout);
    }
  };
  
  struct HorizontalLayout : QWidget, Gui{
    HorizontalLayout()
      : Gui(this)
    {
      QHBoxLayout *mainLayout = new QHBoxLayout;      
      setLayout(mainLayout);
    }
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
  };
  
  struct GroupBox : QGroupBox, Gui{
    GroupBox(const char *title)
      : QGroupBox(title)
      , Gui(this)        
    {
      QVBoxLayout *mainLayout = new QVBoxLayout;
      setLayout(mainLayout);
    }
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
  };

  struct TextEdit : FocusSnifferQTextEdit, Gui{
    Q_OBJECT;

  public:
    
    TextEdit(QString content)
      : Gui(this)
    {
      setPlainText(content);
    }
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

  for(Gui *existing_child : gui->children){
    if (existing_child->_widget==child)
      return existing_child->get_gui_num();
  }
  
  Gui *child_gui = new Gui(child);
  gui->children.push_back(child_gui);
  
  return child_gui->get_gui_num();
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

  QLayout *layout = parent->_widget->layout();

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

    child->_widget->setParent(parent->_widget);
    child->_widget->move(x1,y1);
    
    if (x2>x1 && y2 > y1)
      child->_widget->resize(x2-x1, y2-y1);

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

  return gui->_widget->height();
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

// canvas
///////////

int64_t gui_canvas(int width, int height){
  return (new Canvas(width, height))->get_gui_num();
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

void gui_drawText(int64_t guinum, const_char* color, const_char *text, float x1, float y1, float x2, float y2) {
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  gui->drawText(color, text, x1, y1, x2, y2);
}


#include "mapi_gui.cpp"


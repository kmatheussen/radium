
#include "../common/includepython.h"


#include <QWidget>
#include <QCloseEvent>
#include <QPushButton>
#include <QCheckBox>
#include <QRadioButton>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QGroupBox>

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
  
  struct Gui {

    int _gui_num;
    QWidget *_widget;
    func_t *_callback;
    
    int get_gui_num(void){
      return _gui_num;
    }
    
    Gui(QWidget *widget, func_t *callback = NULL)
      : _widget(widget)
      , _callback(callback)
    {
      R_ASSERT_RETURN_IF_FALSE(!g_guis.contains(this));
      
      _gui_num = g_guis.size();
      g_guis.push_back(this);
      _widget->setAttribute(Qt::WA_DeleteOnClose);
      
      if (_callback != NULL)
        s7extra_protect(_callback);
    }

    ~Gui(){
      R_ASSERT_RETURN_IF_FALSE(g_guis.contains(this));
      R_ASSERT_RETURN_IF_FALSE(g_guis[_gui_num] != NULL);
                               
      printf("Deleting Gui %p\n",this);
      
      g_guis[_gui_num] = NULL;

      if (_callback != NULL)
        s7extra_unprotect(_callback);
    }

    virtual void setGuiValue(dyn_t val){
      GFX_Message(NULL, "Gui #%d does not have a setValue method", _gui_num);
    }
  };
  
  
  struct PushButton : QPushButton, Gui{
    Q_OBJECT;
    
  public:
    
    PushButton(const char *text, func_t *callback)
      : QPushButton(text)
      , Gui(this, callback)
    {
      connect(this, SIGNAL(clicked(bool)), this, SLOT(clicked(bool)));
    }

  public slots:
    void clicked(bool checked){
      s7extra_callFunc_void_void(_callback);
    }
  };
  
  struct CheckBox : QCheckBox, Gui{
    Q_OBJECT;
    
  public:
    
    CheckBox(const char *text, bool is_checked, func_t *callback)
      : QCheckBox(text)
      , Gui(this, callback)
    {
      connect(this, SIGNAL(toggled(bool)), this, SLOT(toggled(bool)));
      setChecked(is_checked);
    }

    virtual void setGuiValue(dyn_t val) override {
      if(val.type==BOOL_TYPE)
        setChecked(val.bool_number);
      else
        GFX_Message(NULL, "Checkbox->setValue received %s, expected BOOL_TYPE", DYN_type_name(val.type));
    }

  public slots:
    void toggled(bool checked){
      s7extra_callFunc_void_bool(_callback, checked);
    }
  };
  
  struct RadioButton : QRadioButton, Gui{
    Q_OBJECT;
    
  public:
    
    RadioButton(const char *text, bool is_checked, func_t *callback)
      : QRadioButton(text)
      , Gui(this, callback)
    {
      connect(this, SIGNAL(toggled(bool)), this, SLOT(toggled(bool)));
      setChecked(is_checked);
    }

    virtual void setGuiValue(dyn_t val) override {
      if(val.type==BOOL_TYPE)
        setChecked(val.bool_number);
      else
        GFX_Message(NULL, "RadioButton->setValue received %s, expected BOOL_TYPE", DYN_type_name(val.type));
    }

  public slots:
    void toggled(bool checked){
      s7extra_callFunc_void_bool(_callback, checked);
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
      printf("gakk\n");      
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

  public:
    
    Slider(Qt::Orientation orientation, const char *text, double min, double curr, double max, bool is_int, func_t *callback)
      : MyQSlider(orientation)
      , Gui(this, callback)
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
        setMaximum(abs(min-max));
        setTickInterval(1);
      } else {
        setMinimum(0);
        setMaximum(10000);
      }

      setGuiValue(DYN_create_float(curr));

      connect(this, SIGNAL(valueChanged(int)), this, SLOT(valueChanged(int)));

      value_setted(value()); // In case value wasn't changed when calling setValue above.
    }

    void value_setted(int value){
      double scaled_value = scale_double(value, minimum(), maximum(), _min, _max);
      
      if (_is_int) {
        SLIDERPAINTER_set_string(_painter, _text + QString::number(scaled_value));
        s7extra_callFunc_void_int(_callback, scaled_value);
      } else {
        SLIDERPAINTER_set_string(_painter, _text + QString::number(scaled_value, 'f', 2));
        s7extra_callFunc_void_double(_callback, scaled_value);
      }
    }

    virtual void setGuiValue(dyn_t val) override {
      if(val.type!=INT_TYPE && val.type!=FLOAT_TYPE){
        GFX_Message(NULL, "Slider->setValue received %s, expected INT_TYPE or FLOAT_TYPE", DYN_type_name(val.type));
        return;
      }

      if (val.type==INT_TYPE)
        setValue(scale_double(val.int_number, _min, _max, minimum(), maximum()));
      else
        setValue(scale_double(val.float_number, _min, _max, minimum(), maximum()));
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

  struct Line : FocusSnifferQLineEdit, Gui{
    Q_OBJECT;

  public:
    
    Line(QString content, func_t *callback)
      : Gui(this, callback)
    {
      connect(this, SIGNAL(editingFinished()), this, SLOT(editingFinished()));
      setText(content);
      s7extra_callFunc_void_charpointer(callback, content.toUtf8().constData());
    }

  public slots:
    
    void editingFinished(){
      set_editor_focus();

      GL_lock();{
        clearFocus();
      }GL_unlock();

      s7extra_callFunc_void_charpointer(_callback, text().toUtf8().constData());
    }
    
  };

  struct TextEdit : FocusSnifferQTextEdit, Gui{
    Q_OBJECT;

  public:
    
    TextEdit(QString content, func_t *callback)
      : Gui(this, callback)
    {
      connect(this, SIGNAL(textChanged()), this, SLOT(textChanged()));
      setPlainText(content);
      s7extra_callFunc_void_charpointer(callback, content.toUtf8().constData());
    }

  public slots:
    
    void textChanged(){
      s7extra_callFunc_void_charpointer(_callback, toPlainText().toUtf8().constData());
    }
    
  };


  struct IntText : FocusSnifferQSpinBox, Gui{
    Q_OBJECT;
    int _last_sent;
    
  public:
    
    IntText(int min, int curr, int max, func_t *callback)
      : Gui(this, callback)
      , _last_sent(min-1)
    {
      connect(this, SIGNAL(valueChanged(int)), this, SLOT(valueChanged(int)));
      connect(this, SIGNAL(editingFinished()), this, SLOT(editingFinished()));
      setMinimum(R_MIN(min, max));
      setMaximum(R_MAX(min, max));
      setValue(curr);
      s7extra_callFunc_void_int(_callback, curr); // In case value wasn't changed when calling setValue above.
    }

    virtual void setGuiValue(dyn_t val) override {
      if (val.type==INT_TYPE)
        setValue(val.int_number);
      else
        GFX_Message(NULL, "IntText->setValue received %s, expected INT_TYPE", DYN_type_name(val.type));
    }

  public slots:
    void valueChanged(int val){
      if (val != _last_sent){
        s7extra_callFunc_void_int(_callback, val);
        _last_sent = val;
      }
    }
    void editingFinished(){
      set_editor_focus();

      GL_lock();{
        clearFocus();
      }GL_unlock();
    }

  };
  
  struct FloatText : FocusSnifferQDoubleSpinBox, Gui{
    Q_OBJECT;
    double _last_sent;
    
  public:
    
    FloatText(double min, double curr, double max, int num_decimals, double step_interval, func_t *callback)
      : Gui(this, callback)
      , _last_sent(min-1)
    {
      connect(this, SIGNAL(valueChanged(double)), this, SLOT(valueChanged(double)));
      connect(this, SIGNAL(editingFinished()), this, SLOT(editingFinished()));
      setMinimum(R_MIN(min, max));
      setMaximum(R_MAX(min, max));
      setDecimals(num_decimals);
      setSingleStep(step_interval);
      setValue(curr);
      s7extra_callFunc_void_double(_callback, curr); // In case value wasn't changed when calling setValue above.
    }

    virtual void setGuiValue(dyn_t val) override {
      if (val.type==FLOAT_TYPE)
        setValue(val.float_number);
      else
        GFX_Message(NULL, "FloatText->setValue received %s, expected FLOAT_TYPE", DYN_type_name(val.type));
    }

  public slots:
    void valueChanged(double val){
      if (val != _last_sent){
        s7extra_callFunc_void_double(_callback, val);
        _last_sent = val;
      }
    }
    void editingFinished(){
      set_editor_focus();

      GL_lock();{
        clearFocus();
      }GL_unlock();
    }

  };
  

}

using namespace radium_gui;

  
static Gui *get_gui(int64_t guinum){
  if (guinum < 0 || guinum > g_guis.size()){
    GFX_Message(NULL, "No Gui #%d", guinum);
  }

  Gui *gui = g_guis[guinum];

  if (gui==NULL)
    GFX_Message(NULL, "Gui #%d has been closed and can not be used.", guinum);

  return gui;
}

int64_t gui_button(const_char *text, func_t *func){
  return (new PushButton(text, func))->get_gui_num();
}

int64_t gui_checkbox(const_char *text, bool is_checked, func_t *func){
  return (new CheckBox(text, is_checked, func))->get_gui_num();
}

int64_t gui_radiobutton(const_char *text, bool is_checked, func_t *func){
  return (new RadioButton(text, is_checked, func))->get_gui_num();
}

int64_t gui_horizontalIntSlider(const_char *text, int min, int curr, int max, func_t *func){
  if(min==max){
    GFX_Message(NULL, "Gui slider: minimum and maximum value is the same");
    return -1;
  }
  return (new radium_gui::Slider(Qt::Horizontal, text, min, curr, max, true, func))->get_gui_num();
}
int64_t gui_horizontalSlider(const_char *text, double min, double curr, double max, func_t *func){
  if(min==max){
    GFX_Message(NULL, "Gui slider: minimum and maximum value is the same");
    return -1;
  }
  return (new radium_gui::Slider(Qt::Horizontal, text, min, curr, max, false, func))->get_gui_num();
}
int64_t gui_verticalIntSlider(const_char *text, int min, int curr, int max, func_t *func){
  if(min==max){
    GFX_Message(NULL, "Gui slider: minimum and maximum value is the same");
    return -1;
  }
  return (new radium_gui::Slider(Qt::Vertical, text, min, curr, max, true, func))->get_gui_num();
}
int64_t gui_verticalSlider(const_char *text, double min, double curr, double max, func_t *func){
  if(min==max){
    GFX_Message(NULL, "Gui slider: minimum and maximum value is the same");
    return -1;
  }
  return (new radium_gui::Slider(Qt::Vertical, text, min, curr, max, false, func))->get_gui_num();
}

int64_t gui_verticalLayout(void){
  return (new VerticalLayout())->get_gui_num();
}

int64_t gui_horizontalLayout(void){
  return (new HorizontalLayout())->get_gui_num();
}

int64_t gui_tableLayout(int num_columns){
  return (new TableLayout(num_columns))->get_gui_num();
}

int64_t gui_group(const_char* title){
  return (new GroupBox(title))->get_gui_num();
}

int64_t gui_text(const_char* text, const_char* color){
  return (new Text(text, color))->get_gui_num();
}

int64_t gui_textEdit(const_char* content, func_t *func){
    return (new TextEdit(content, func))->get_gui_num();
}

int64_t gui_line(const_char* content, func_t *func){
    return (new Line(content, func))->get_gui_num();
}

int64_t gui_intText(int min, int curr, int max, func_t *func){
  return (new IntText(min, curr, max, func))->get_gui_num();
}

int64_t gui_floatText(double min, double curr, double max, int num_decimals, double step_interval, func_t *func){
  return (new FloatText(min, curr, max, num_decimals, step_interval, func))->get_gui_num();
}

void gui_setValue(int64_t guinum, dyn_t value){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  gui->setGuiValue(value);
}

void gui_add(int64_t parentnum, int64_t childnum){
  Gui *parent = get_gui(parentnum);
  if (parent==NULL)
    return;

  Gui *child = get_gui(childnum);  
  if (child==NULL)
    return;

  QLayout *layout = parent->_widget->layout();

  if(layout==NULL) {

    GFX_Message(NULL, "Warning: Parent gui #%d does not have a layout", parentnum);

    child->_widget->setParent(parent->_widget);
    child->_widget->move(0,0);
    
  } else {
    
    layout->addWidget(child->_widget);
    
  }
  
}

bool gui_is_visible(int64_t guinum){
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


#include "mapi_gui.cpp"



#include "../common/includepython.h"


#include <QWidget>
#include <QCloseEvent>
#include <QPushButton>
#include <QCheckBox>
#include <QRadioButton>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QGroupBox>
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

    void editingFinished(){
      QLineEdit *line_edit = dynamic_cast<QLineEdit*>(_widget);
      
      set_editor_focus();

      GL_lock();{
        line_edit->clearFocus();
      }GL_unlock();

      s7extra_callFunc_void_charpointer(_func, line_edit->text().toUtf8().constData());
    }
    

  };

  
  struct Gui {

    QVector<Callback*> _callbacks;
 
  public:
    int _gui_num;
    QWidget *_widget;
    func_t *_func;

    QVector<Gui*> children;
    
    int get_gui_num(void){
      return _gui_num;
    }
    
    Gui(QWidget *widget, func_t *callback = NULL)
      : _widget(widget)
      , _func(callback)
    {
      R_ASSERT_RETURN_IF_FALSE(!g_guis.contains(this));
      
      _gui_num = g_guis.size();
      g_guis.push_back(this);
      _widget->setAttribute(Qt::WA_DeleteOnClose);
      
      if (_func != NULL) {
        s7extra_protect(_func);
        addGuiCallback(_func);
      }
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

      if (_func != NULL)
        s7extra_unprotect(_func);
    }

    virtual void setGuiValue(dyn_t val){
      GFX_Message(NULL, "Gui #%d does not have a setValue method", _gui_num);
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
        return DYN_create_string(label->text().toUtf8().constData());

      QLineEdit *line_edit = dynamic_cast<QLineEdit*>(_widget);
      if (line_edit!=NULL)
        return DYN_create_string(line_edit->text().toUtf8().constData());

      QTextEdit *text_edit = dynamic_cast<QTextEdit*>(_widget);
      if (text_edit!=NULL)
        return DYN_create_string(text_edit->toPlainText().toUtf8().constData());

      QSpinBox *spinbox = dynamic_cast<QSpinBox*>(_widget);
      if (spinbox!=NULL)
        return DYN_create_int(spinbox->value());

      QDoubleSpinBox *doublespinbox = dynamic_cast<QDoubleSpinBox*>(_widget);
      if (doublespinbox!=NULL)
        return DYN_create_float(doublespinbox->value());
      
                  
      GFX_Message(NULL, "Gui #%d does not have a getValue method", _gui_num);
      return DYN_create_bool(false);
    }

    virtual void addGuiCallback(func_t* func){
      Callback *callback = new Callback(func, _widget);

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
          slider->connect(slider, SIGNAL(valueChanged(int)), callback, SLOT(valueChanged(int)));
          goto gotit;
        }
      }
      
      {
        QLineEdit *line_edit = dynamic_cast<QLineEdit*>(_widget);
        if (line_edit!=NULL){
          line_edit->connect(line_edit, SIGNAL(editingFinished()), callback, SLOT(editingFinished()));
          goto gotit;
        }
      }

#if 0
      QLineEdit *line_edit = dynamic_cast<QLineEdit*>(_widget);
      if (line_edit!=NULL)
        return DYN_create_string(line_edit->text().toUtf8().constData());

      QTextEdit *text_edit = dynamic_cast<QTextEdit*>(_widget);
      if (text_edit!=NULL)
        return DYN_create_string(text_edit->toPlainText().toUtf8().constData());

      QSpinBox *spinbox = dynamic_cast<QSpinBox*>(_widget);
      if (spinbox!=NULL)
        return DYN_create_int(spinbox->value());

      QDoubleSpinBox *doublespinbox = dynamic_cast<QDoubleSpinBox*>(_widget);
      if (doublespinbox!=NULL)
        return DYN_create_float(doublespinbox->value());
#endif

      GFX_Message(NULL, "Gui #%d does not have a addCallback method", _gui_num);
      delete callback;
      return;

    gotit:
      _callbacks.push_back(callback);
      return;      
    }
    
  };
  

  struct PushButton : QPushButton, Gui{
    Q_OBJECT;
    
  public:
    
    PushButton(const char *text, func_t *callback)
      : QPushButton(text)
      , Gui(this, callback)
    {
      //connect(this, SIGNAL(clicked(bool)), this, SLOT(clicked(bool)));
    }
    /*
  public slots:
    void clicked(bool checked){
      s7extra_callFunc_void_void(_callback);
    }
    */
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

    virtual dyn_t getGuiValue(void) override {
      return DYN_create_bool(isChecked());
    }

  public slots:
    void toggled(bool checked){
      s7extra_callFunc_void_bool(_func, checked);
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

    virtual dyn_t getGuiValue(void) override {
      return DYN_create_bool(isChecked());
    }

  public slots:
    void toggled(bool checked){
      s7extra_callFunc_void_bool(_func, checked);
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
        s7extra_callFunc_void_int(_func, scaled_value);
      } else {
        SLIDERPAINTER_set_string(_painter, _text + QString::number(scaled_value, 'f', 2));
        s7extra_callFunc_void_double(_func, scaled_value);
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

    virtual void setGuiValue(dyn_t dyn) override {
      if(dyn.type==STRING_TYPE)
        setText(STRING_get_qstring(dyn.string));
      else
        GFX_Message(NULL, "Text->setValue received %s, expected STRING_TYPE", DYN_type_name(dyn.type));
    }

    virtual dyn_t getGuiValue(void) override {
      return DYN_create_string(text());
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

    virtual void setGuiValue(dyn_t dyn) override {
      if(dyn.type==STRING_TYPE)
        setText(STRING_get_qstring(dyn.string));
      else
        GFX_Message(NULL, "Text->setValue received %s, expected STRING_TYPE", DYN_type_name(dyn.type));
    }

    virtual dyn_t getGuiValue(void) override {
      return DYN_create_string(text());
    }

  public slots:
    
    void editingFinished(){
      set_editor_focus();

      GL_lock();{
        clearFocus();
      }GL_unlock();

      s7extra_callFunc_void_charpointer(_func, text().toUtf8().constData());
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

    virtual void setGuiValue(dyn_t dyn) override {
      if(dyn.type==STRING_TYPE)
        setPlainText(STRING_get_chars(dyn.string));
      else
        GFX_Message(NULL, "Text->setValue received %s, expected STRING_TYPE", DYN_type_name(dyn.type));
    }

    virtual dyn_t getGuiValue(void) override {
      return DYN_create_string(toPlainText());
    }

  public slots:
    
    void textChanged(){
      s7extra_callFunc_void_charpointer(_func, toPlainText().toUtf8().constData());
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
      s7extra_callFunc_void_int(_func, curr); // In case value wasn't changed when calling setValue above.
    }

    virtual void setGuiValue(dyn_t val) override {
      if (val.type==INT_TYPE)
        setValue(val.int_number);
      else
        GFX_Message(NULL, "IntText->setValue received %s, expected INT_TYPE", DYN_type_name(val.type));
    }

    virtual dyn_t getGuiValue(void) override {
      return DYN_create_int(value());
    }

  public slots:
    void valueChanged(int val){
      if (val != _last_sent){
        s7extra_callFunc_void_int(_func, val);
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
      s7extra_callFunc_void_double(_func, curr); // In case value wasn't changed when calling setValue above.
    }

    virtual void setGuiValue(dyn_t val) override {
      if (val.type==FLOAT_TYPE)
        setValue(val.float_number);
      else
        GFX_Message(NULL, "FloatText->setValue received %s, expected FLOAT_TYPE", DYN_type_name(val.type));
    }
    
    virtual dyn_t getGuiValue(void) override {
      return DYN_create_float(value());
    }


  public slots:
    void valueChanged(double val){
      if (val != _last_sent){
        s7extra_callFunc_void_double(_func, val);
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
    return NULL;
  }

  Gui *gui = g_guis[guinum];

  if (gui==NULL)
    GFX_Message(NULL, "Gui #%d has been closed and can not be used.", guinum);

  return gui;
}

int64_t gui_ui(const_char *filename){
  QUiLoader loader;
  loader.clearPluginPaths();
  
  QFile file(filename);
  if (file.open(QFile::ReadOnly)==false){
    GFX_Message(NULL, "Unable to open \"%s\": %s", filename, file.errorString().toUtf8().constData());
    return -1;
  }
  
  QWidget *widget = loader.load(&file);
  file.close();

  if (widget==NULL){
    GFX_Message(NULL, "Unable to open \"%s\": %s", filename, loader.errorString().toUtf8().constData());
    return -1;
  }
  
  Gui *gui = new VerticalLayout();
  gui->_widget->layout()->addWidget(widget);
  
  return gui->get_gui_num();
}

int64_t gui_child(int64_t guinum, const_char* childname){
  Gui *gui = get_gui(guinum);

  if (gui==NULL){
    GFX_Message(NULL, "Gui #%d has been closed and can not be used.", guinum);
    return -1;
  }
  
  QWidget *child = gui->_widget->findChild<QWidget*>(childname);

  if (child==NULL){
    GFX_Message(NULL, "Could not find child \"%s\" in gui #%d.", childname, guinum);
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

  if (gui==NULL){
    GFX_Message(NULL, "Gui #%d has been closed and can not be used.", guinum);
    return;
  }

  gui->addGuiCallback(func);
}


int64_t gui_button(const_char *text, func_t *func){
  //return -1;
  return (new PushButton(text, func))->get_gui_num();
}

int64_t gui_checkbox(const_char *text, bool is_checked, func_t *func){
  //return -1;
  return (new CheckBox(text, is_checked, func))->get_gui_num();
}

int64_t gui_radiobutton(const_char *text, bool is_checked, func_t *func){
  return -1;
  //return (new RadioButton(text, is_checked, func))->get_gui_num();
}

int64_t gui_horizontalIntSlider(const_char *text, int min, int curr, int max, func_t *func){
  if(min==max){
    GFX_Message(NULL, "Gui slider: minimum and maximum value is the same");
    return -1;
  }
  //return -1;
  return (new radium_gui::Slider(Qt::Horizontal, text, min, curr, max, true, func))->get_gui_num();
}
int64_t gui_horizontalSlider(const_char *text, double min, double curr, double max, func_t *func){
  if(min==max){
    GFX_Message(NULL, "Gui slider: minimum and maximum value is the same");
    return -1;
  }
  //return -1;
  return (new radium_gui::Slider(Qt::Horizontal, text, min, curr, max, false, func))->get_gui_num();
}
int64_t gui_verticalIntSlider(const_char *text, int min, int curr, int max, func_t *func){
  if(min==max){
    GFX_Message(NULL, "Gui slider: minimum and maximum value is the same");
    return -1;
  }
  //return -1;
  return (new radium_gui::Slider(Qt::Vertical, text, min, curr, max, true, func))->get_gui_num();
}
int64_t gui_verticalSlider(const_char *text, double min, double curr, double max, func_t *func){
  if(min==max){
    GFX_Message(NULL, "Gui slider: minimum and maximum value is the same");
    return -1;
  }
  //return -1;
  return (new radium_gui::Slider(Qt::Vertical, text, min, curr, max, false, func))->get_gui_num();
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

int64_t gui_textEdit(const_char* content, func_t *func){
  //return -1;
  return (new TextEdit(content, func))->get_gui_num();
}

int64_t gui_line(const_char* content, func_t *func){
  //return -1;
  return (new Line(content, func))->get_gui_num();
}

int64_t gui_intText(int min, int curr, int max, func_t *func){
  //return -1;
  return (new IntText(min, curr, max, func))->get_gui_num();
}

int64_t gui_floatText(double min, double curr, double max, int num_decimals, double step_interval, func_t *func){
  //return -1;
  return (new FloatText(min, curr, max, num_decimals, step_interval, func))->get_gui_num();
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


#include "mapi_gui.cpp"


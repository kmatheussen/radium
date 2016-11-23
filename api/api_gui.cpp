
#include "../common/includepython.h"


#include <QWidget>
#include <QCloseEvent>
#include <QPushButton>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QGroupBox>

#include "../common/nsmtracker.h"
#include "../common/visual_proc.h"
#include "../embedded_scheme/s7extra_proc.h"

#include "../Qt/Qt_MyQSlider.h"

#include "api_common_proc.h"

#include "radium_proc.h"



namespace radium_gui{

struct Gui;

static QVector<Gui*> g_guis;


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
      connect(this, SIGNAL(valueChanged(int)), this, SLOT(valueChanged(int)));

      if (is_int) {
        setMinimum(min);
        setMaximum(max);
        setTickInterval(1);
        setValue(curr);
      } else {
        //setTickInterval(1000);
        setMinimum(0);
        setMaximum(10000);
        setValue(scale(curr, min, max, 0, 1000));
      }

    }

  public slots:
    void valueChanged(int value){
      if (_is_int) {
        SLIDERPAINTER_set_string(_painter, _text + QString::number(value));
        s7extra_callFunc_void_int(_callback, value);
      } else {
        double fvalue = scale(value, 0, 10000, _min, _max);
        SLIDERPAINTER_set_string(_painter, _text + QString::number(fvalue, 'f', 2));
        s7extra_callFunc_void_int(_callback, fvalue);
      }
    }
  };

  struct Text : QLabel, Gui{
    Text(const char *text)
      : QLabel(text)
      , Gui(this)
    {}
  };
  
}

using namespace radium_gui;

  
static Gui *get_gui(int guinum){
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

int64_t gui_horizontalIntSlider(const_char *text, int min, int curr, int max, func_t *func){
  return (new radium_gui::Slider(Qt::Horizontal, text, min, curr, max, true, func))->get_gui_num();
}
int64_t gui_horizontalSlider(const_char *text, int min, int curr, int max, func_t *func){
  return (new radium_gui::Slider(Qt::Horizontal, text, min, curr, max, false, func))->get_gui_num();
}
int64_t gui_verticalIntSlider(const_char *text, int min, int curr, int max, func_t *func){
  return (new radium_gui::Slider(Qt::Vertical, text, min, curr, max, true, func))->get_gui_num();
}
int64_t gui_verticalSlider(const_char *text, int min, int curr, int max, func_t *func){
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

int64_t gui_text(const_char* text){
  return (new Text(text))->get_gui_num();
}

void gui_add(int parentnum, int childnum){
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

bool gui_is_visible(int guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return false;

  return gui->_widget->isVisible();
}

void gui_show(int guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  gui->_widget->show();
}

void gui_hide(int guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  gui->_widget->hide();
}

void gui_close(int guinum){
  Gui *gui = get_gui(guinum);
  if (gui==NULL)
    return;

  gui->_widget->close();
}


#include "mapi_gui.cpp"


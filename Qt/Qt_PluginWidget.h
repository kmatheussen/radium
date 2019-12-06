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


#include <QToolButton>
#include <QString>
#ifdef USE_QT5
#include <QStyleFactory>
#endif

#include "../common/OS_Player_proc.h"
#include "../common/Vector.hpp"
#include "../audio/SoundPlugin_proc.h"
#include "../audio/undo_audio_effect_proc.h"
#include "../audio/Faust_plugins_proc.h"
#include "../midi/midi_i_input_proc.h"

#include "Qt_MyQLabel.h"
#include "Qt_MyQCheckBox.h"


// Widget generation code is based on / copied from qtractor, written by Rui Nuno Capela.

class ParamWidget : public QWidget {
  Q_OBJECT;
 public:

  radium::GcHolder<struct Patch> _patch; // Pointer to a patch never changes, but the patch's plugin might. That's why we store the patch and not the plugin, although we only use the plugin.
  int _effect_num;
  const SoundPluginType *_type;

  MyQCheckBox *_randomized_button;
  MyQSlider *_slider;
  //QToolButton *_check_button;
  MyQCheckBox *_check_button;
  bool _can_update_effect_value;
  QString _name;
  SizeType _size_type;
  int _tab_page_num;
  
  ParamWidget(QWidget *parent, struct Patch *patch, int effect_num, int tab_page_num)
    : QWidget(parent)
    , _patch(patch)
    , _effect_num(effect_num)
    , _slider(NULL)
    , _check_button(NULL)
    , _can_update_effect_value(false)
    , _size_type(SIZETYPE_NORMAL)
    , _tab_page_num(tab_page_num)
  {
      
      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
      const SoundPluginType *type  = plugin->type;
      _type = type;
      reload_name();
      
      int format = type->get_effect_format!=NULL ? type->get_effect_format(plugin, effect_num) : EFFECT_FORMAT_FLOAT;

      QGridLayout *grid_layout = new QGridLayout(this);
      grid_layout->setMargin(0);
      grid_layout->setSpacing(0);

      if(0){
        _randomized_button = new MyQCheckBox(this);
        _randomized_button->setCheckable(true);
        //_randomized_button->setText("R");
        _randomized_button->setToolTip("Whether to randomize when pressing the \"Random\" button");
        _randomized_button->resize(10,12);
        grid_layout->addWidget(_randomized_button, 1, 0, 1, 1);
        QSizePolicy sizePolicy5(QSizePolicy::Fixed, QSizePolicy::Expanding);
        sizePolicy5.setHorizontalStretch(0);
        sizePolicy5.setVerticalStretch(0);
        //sizePolicy5.setHeightForWidth(_randomized_button->sizePolicy().hasHeightForWidth());
        _randomized_button->setSizePolicy(sizePolicy5);
      }

      if (format==EFFECT_FORMAT_BOOL){
        //_check_button = new QToolButton(this);
        _check_button = new MyQCheckBox(this);
        _check_button->setCheckable(true);
        
        _check_button->_patch.set(patch);
        _check_button->_effect_num = effect_num;

        _check_button->_show_enabled_marker = true;
        
        QSizePolicy sizePolicy5(QSizePolicy::Expanding, QSizePolicy::Expanding);
        sizePolicy5.setHorizontalStretch(1);
        sizePolicy5.setVerticalStretch(0);
        sizePolicy5.setHeightForWidth(_check_button->sizePolicy().hasHeightForWidth());
        _check_button->setSizePolicy(sizePolicy5);

        _check_button->setText(_name);
        if(type->get_effect_description!=NULL){
          QString description = type->get_effect_description(plugin,effect_num);
          if (description != "")
            _check_button->setToolTip(description);
        }
        
        grid_layout->addWidget(_check_button, 1,1,1,1);
      } else {
        _slider = new MyQSlider(this);
        _slider->_patch.set(patch);
        _slider->_effect_num = effect_num;

        _slider->setOrientation(Qt::Horizontal);
        _slider->setTickPosition(QSlider::NoTicks);
        _slider->setMinimumWidth(120);
        _slider->setMinimum(0);
        _slider->setMaximum(10000);
        _slider->setPageStep(1000);
        _slider->setSingleStep(100);
        //_slider->setValue(value);

        QSizePolicy sizePolicy5(QSizePolicy::Expanding, QSizePolicy::Expanding);
        sizePolicy5.setHorizontalStretch(1);
        sizePolicy5.setVerticalStretch(0);
        sizePolicy5.setHeightForWidth(_slider->sizePolicy().hasHeightForWidth());
        _slider->setSizePolicy(sizePolicy5);

        if(type->get_effect_description!=NULL){
          QString description = type->get_effect_description(plugin,effect_num);
          if (description != "")
            _slider->setToolTip(description);
        }

        grid_layout->addWidget(_slider, 1, 1, 1, 1);
      }

      if (format==EFFECT_FORMAT_BOOL){
        QObject::connect(_check_button,
                         SIGNAL(toggled(bool)),
                         SLOT(checkBoxValueToggled(bool)));
        QObject::connect(_check_button,
                         SIGNAL(pressed()),
                         SLOT(checkBoxPressed()));
      }

      if (_slider) {
        connect(_slider,
                SIGNAL(valueChanged(int)),
                this,
                SLOT(sliderValueChanged(int)));

        _slider->setValue(PLUGIN_get_effect_value(plugin, effect_num, VALUE_FROM_STORAGE)*10000);
        sliderValueChanged(PLUGIN_get_effect_value(plugin, effect_num, VALUE_FROM_STORAGE)*10000);

        if(!strcmp(type->type_name,"Faust")){
          float *peak_pointers = FAUST_get_peak_value_pointer(plugin, effect_num);
          if(peak_pointers != NULL)
            SLIDERPAINTER_set_peak_value_pointers(_slider->_painter, 1, peak_pointers, true);
        }
        
        if(_check_button){
          _check_button->setChecked(PLUGIN_get_effect_value(plugin, effect_num,VALUE_FROM_STORAGE)>0.5f);        
        }
      }

      setLayout(grid_layout);
      
      _can_update_effect_value = true;
    }

  void prepare_for_deletion(void){
    if(_slider!=NULL)
      _slider->prepare_for_deletion();
  }
  
  ~ParamWidget(){
    prepare_for_deletion();
    //printf("           Deleting ParamWidget %d\n",_effect_num);
  }

  void calledRegularlyByParent(void){        
    //printf(" drawing effect %d\n", paramWidget->_effect_num);
    if (_slider != NULL)
      _slider->calledRegularlyByParent();
  }
  
  // TODO: Optimize with binary search.
  //
  void adjustFontSize(void){
    
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    
    if (plugin != NULL) {

      int last_size = -1;

      QWidget *w;
      if (_slider!=NULL)
        w = _slider;
      else
        w = _check_button;
      
      if (w != NULL){

        if (_size_type==SIZETYPE_NORMAL){
          w->setFont(font());
          return;
        }

        char buf[64]={0};

        if (_check_button != NULL)
          sprintf(buf, "%s", "");
        else
          PLUGIN_get_display_value_string(plugin, _effect_num, buf, 64);
        
        QString text = _name + ": " + buf + "513451";

        int newsize = 100;
        QFont the_font(w->font().family(), newsize);
      
        for(;;){
          QFontMetrics fm(the_font);
          
          if (fm.height() <= height() && fm.boundingRect(text).width() < width())
            break;
          
          int size = the_font.pointSize();
          if (size==last_size)
            break;
          
          newsize = size - 2;
          if (newsize < 10)
            break;
          
          //printf("Setting font size to %d (%d - %d)  (%s)\n",newsize,fm.width(text),width(),text.toUtf8().constData());
          the_font = QFont(the_font.family(), newsize);
        }

        w->setFont(QFont(the_font.family(), newsize - 6));
      }
    }
  }

  void resizeEvent(QResizeEvent * event) override{
    radium::ScopedResizeEventTracker resize_event_tracker;
    RETURN_IF_DATA_IS_INACCESSIBLE();
    adjustFontSize();
    QWidget::resizeEvent(event);
  }
  
  void set_effect_value(float value){
    if (_can_update_effect_value) {

      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;

      PLUGIN_set_effect_value(plugin, -1, _effect_num, value, STORE_VALUE, FX_single, EFFECT_FORMAT_SCALED);
    }
  }

  void update_gui_element(){
    //printf("Update GUI called. _slider: %p. _Check_button: %p\n",_slider,_check_button);

    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;

    float value;

    value = PLUGIN_get_effect_value(plugin, _effect_num, VALUE_FROM_STORAGE);

    //printf("UPDATE GUI ELEMENT %d for %s (%p / %p). Value: %f\n",_effect_num,plugin->type->name,_slider,_check_button,value);

    _can_update_effect_value = false; {
      
      if(_slider!=NULL) {
        set_slider_string(); // maybe fix: set_slider_string will be called twice if the value has changed.
        _slider->setValue(value * 10000.0f);
        //if (_effect_num==0)
        //  printf("   ===== ParamWidget. Setting slider value to %f (%s)\n", value,get_slider_string().toUtf8().constData());
      } else if(_check_button!=NULL)
        _check_button->setChecked(value>0.5f);
      
    }_can_update_effect_value = true;

    update();
  }

  void reload_name(void){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    QString name = _type->get_effect_name(plugin, _effect_num);
    if(name.size()>25)
      name = name.left(25);
    _name = name;

    //    if (name == "<unused>")
    //  hide();
  }

  QString get_slider_string(void){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    char buf[66]={0};
    PLUGIN_get_display_value_string(plugin, _effect_num, buf, 64);

    return get_parameter_prepend_text(_patch.data(), _effect_num) + _name + ": " + QString::fromUtf8(buf);
  }
  
  void set_slider_string(void){
    SLIDERPAINTER_set_string(_slider->_painter, get_slider_string());
  }
  
  public slots:

    void sliderValueChanged (int value){
      if(_slider!=NULL){
        set_effect_value(value/10000.0f);
        set_slider_string();
        //_slider->display_string.sprintf("%s: %s",_name.toUtf8().constData(),
        //printf(".... sliderValueChanged slot: Has set value to %s (%f)\n", get_slider_string().toUtf8().constData(), value / 1000.0f);
      }
    }

    void checkBoxPressed(){
      printf("checkbox pressed\n");
      ADD_UNDO(AudioEffect_CurrPos(_patch.data(), _effect_num, AE_NO_FLAGS)); // Undo for sliders is taken care of in MyQSlider.h.
    }

    void checkBoxValueToggled(bool toggle){
      //printf("got something %d\n",(int)toggle);

      if(toggle==true)
        set_effect_value(1.0f);
      else
        set_effect_value(0.0f);
    }
};


struct PluginWidget : public QWidget{
  radium::GcHolder<struct Patch> _patch;
  QTabWidget  *pTabWidget = NULL;
  
  radium::Vector<ParamWidget*> _param_widgets;

  int _num_rows;
  
  PluginWidget(QWidget *parent, struct Patch *patch)
    : QWidget(parent)
    , _patch(patch)
    , _num_rows(0)
  {
  }

  void update_gui(void){
    for(ParamWidget *param_widget : _param_widgets)
      param_widget->update_gui_element();

    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;

    if (plugin != NULL){
      if (ATOMIC_GET(plugin->effect_num_to_show_because_it_was_used_externally) >= 0){
        int effect_num_to_show = ATOMIC_SET_RETURN_OLD(plugin->effect_num_to_show_because_it_was_used_externally, -1);
        R_ASSERT_RETURN_IF_FALSE(effect_num_to_show >= 0);
        R_ASSERT_RETURN_IF_FALSE(effect_num_to_show < _param_widgets.size());
        if (pTabWidget != NULL)
          pTabWidget->setCurrentIndex(_param_widgets[effect_num_to_show]->_tab_page_num);
      }
    }

  }
  
  void calledRegularlyByParent(void){
    bool has_been_visible = false;
    
    for(ParamWidget *paramWidget : _param_widgets){
      MyQSlider *slider = paramWidget->_slider;
      if (slider != NULL){
        bool is_visible = slider->isVisible();
        if (is_visible==true)
          has_been_visible = true;
        
        if (is_visible==false && has_been_visible==true) // Optimize a bit. some vst plugins have thousands of parameters.
          break;
        
        if (is_visible)
          paramWidget->calledRegularlyByParent();
      }
    }
  }
      
  void prepare_for_deletion(void){
    for (auto *param_widget : _param_widgets)
      param_widget->prepare_for_deletion();
  }
  
  ~PluginWidget(){
    printf("        Deleting PluginWidget\n");
  }
};

PluginWidget *PluginWidget_create(QWidget *parent, struct Patch *patch, SizeType size_type);

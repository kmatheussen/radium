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

#include "../common/OS_Player_proc.h"
#include "../audio/SoundPlugin_proc.h"
#include "../audio/undo_audio_effect_proc.h"
#include "../audio/Faust_plugins_proc.h"

#include "Qt_MyQCheckBox.h"


// Widget generation code is based on / copied from qtractor, written by Rui Nuno Capela.


class ParamWidget : public QWidget{
  Q_OBJECT;
 public:

  struct Patch *_patch; // Pointer to a patch never changes, but the patch's plugin might. That's why we store the patch and not the plugin, although we only use the plugin.
  int _effect_num;
  const SoundPluginType *_type;

  MyQCheckBox *_randomized_button;
  MyQSlider *_slider;
  //QToolButton *_check_button;
  MyQCheckBox *_check_button;
  bool _can_update_effect_value;
  QString _name;
    
 ParamWidget(QWidget *parent, struct Patch *patch, int effect_num)
   : QWidget(parent)
    , _patch(patch)
    , _effect_num(effect_num)
    , _slider(NULL)
    , _check_button(NULL)
    , _can_update_effect_value(false)
    {
      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
      const SoundPluginType *type  = plugin->type;
      _type = type;
      _name = type->get_effect_name(plugin, effect_num);
      if(_name.size()>25)
        _name = _name.left(25);

      int format = type->get_effect_format!=NULL ? type->get_effect_format(plugin, effect_num) : EFFECT_FORMAT_FLOAT;

      QGridLayout *grid_layout = new QGridLayout();
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

        _check_button->_patch = patch;
        _check_button->_effect_num = effect_num;

        QSizePolicy sizePolicy5(QSizePolicy::Expanding, QSizePolicy::Expanding);
        sizePolicy5.setHorizontalStretch(1);
        sizePolicy5.setVerticalStretch(0);
        sizePolicy5.setHeightForWidth(_check_button->sizePolicy().hasHeightForWidth());
        _check_button->setSizePolicy(sizePolicy5);

        _check_button->setText(_name);
        if(type->get_effect_description!=NULL)
          _check_button->setToolTip(type->get_effect_description(type,effect_num));

        grid_layout->addWidget(_check_button, 1,1,1,1);
      } else {
        _slider = new MyQSlider();
        _slider->_patch = patch;
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

        if(type->get_effect_description!=NULL)
          _slider->setToolTip(type->get_effect_description(type,effect_num));

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
            SLIDERPAINTER_set_peak_value_pointers(_slider->_painter, 1, peak_pointers);
        }
        
        if(_check_button){
          _check_button->setChecked(PLUGIN_get_effect_value(plugin, effect_num,VALUE_FROM_STORAGE)>0.5f);        
        }
      }

      setLayout(grid_layout);
      
      _can_update_effect_value = true;
    }

  void set_effect_value(float value){
    if (_can_update_effect_value) {

      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;

      PLAYER_lock();{
        PLUGIN_set_effect_value(plugin, -1, _effect_num, value, PLUGIN_NONSTORED_TYPE, PLUGIN_STORE_VALUE, FX_single);
      }PLAYER_unlock();
    }
  }

  void update_gui_element(){
    //printf("Update GUI called. _slider: %p. _Check_button: %p\n",_slider,_check_button);

    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    float value;

    PLAYER_lock();{
      value = PLUGIN_get_effect_value(plugin, _effect_num, VALUE_FROM_STORAGE);
    }PLAYER_unlock();

    //printf("UPDATE GUI ELEMENT %d for %s (%p / %p). Value: %f\n",_effect_num,plugin->type->name,_slider,_check_button,value);

    _can_update_effect_value = false; {
      
      if(_slider!=NULL)
        _slider->setValue(value * 10000.0f);
      else if(_check_button!=NULL)
        _check_button->setChecked(value>0.5f);
      
    }_can_update_effect_value = true;
  }

  public slots:

    void sliderValueChanged (int value){
      //printf("got something %d\n",value);
      if(_slider!=NULL){
        set_effect_value(value/10000.0f);
        SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
        char buf[64]={0};
        PLUGIN_get_display_value_string(plugin, _effect_num, buf, 64);
        SLIDERPAINTER_set_string(_slider->_painter, _name + ": " + QString::fromUtf8(buf));
        //_slider->display_string.sprintf("%s: %s",_name.toUtf8().constData(),
        //printf("Has set value to %s\n",_slider->_painter->display_string.toUtf8().constData());
      }
    }

    void checkBoxPressed(){
      printf("checkbox pressed\n");
      Undo_AudioEffect_CurrPos(_patch, _effect_num); // Undo for sliders is taken care of in MyQSlider.h.
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
  PluginWidget(QWidget *parent)
    : QWidget(parent)
  {}

  std::vector<ParamWidget*> _param_widgets;
};

PluginWidget *PluginWidget_create(QWidget *parent, struct Patch *patch);

/* Copyright 2012-2013 Kjetil S. Matheussen

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


#include <QMessageBox>
#include <QFileDialog>
#include <QTimer>
#include <QTimerEvent>
#include <QSettings>

#include "../audio/SoundPlugin_proc.h"
#include "../audio/Pd_plugin_proc.h"

#include "mQt_pd_controller_config_dialog_callbacks.h"

#include "Qt_pd_controller_widget.h"

//#include "Qt_pd_plugin_widget_callbacks_proc.h"


static const int k_timer_interval_here = 50;

class Pd_Controller_widget : public QWidget, public Ui::Pd_Controller_widget{
  Q_OBJECT;

public:
  SoundPlugin *_plugin; // Pd_plugin.recreate_from_state also recreate gui, so it is safe to store this one here.
  Pd_Controller *_controller; // Pd_plugin.recreate_from_state also recreate gui, so it is safe to store this one here.
  Pd_Controller_Config_dialog _conf;

  struct Timer : public QTimer{

    Pd_Controller *_controller; // Pd_plugin.recreate_from_state also recreate gui, so it is safe to store this one here.
    Pd_Controller_widget *_pd_plugin_widget;
    SoundPlugin *_plugin;

    QByteArray _conf_geometry;
    int _type;
    QString _name;

    void timerEvent(QTimerEvent * e){
      if(_pd_plugin_widget->isVisible()==false)
        return;

      Pd_Controller_Config_dialog *conf = &_pd_plugin_widget->_conf;

      if (conf->isVisible() && conf->_showing==false) {
        _conf_geometry = conf->saveGeometry();
        conf->hide();
        _pd_plugin_widget->popup_button->setChecked(false);
      } if (!conf->isVisible() && conf->_showing==true) {
        conf->show();
        conf->restoreGeometry((const QByteArray &)_conf_geometry);
      }

      if (conf->_showing==false && _pd_plugin_widget->popup_button->checkState()==Qt::Checked){
        _pd_plugin_widget->popup_button->setChecked(false);
      } else if (conf->_showing==true && _pd_plugin_widget->popup_button->checkState()!=Qt::Checked){
        _pd_plugin_widget->popup_button->setChecked(true);
      }

      if (_controller->type != _type) {
        _type = _controller->type;
        _pd_plugin_widget->update_gui();
        _pd_plugin_widget->update();
      }

      if (_controller->name!=NULL && strcmp(_controller->name, _name.ascii())) {
        _name = _controller->name;
        _pd_plugin_widget->paint_slider_text();
        _pd_plugin_widget->paint_onoff_text();
        _pd_plugin_widget->update();
      }

      if(_controller->type==2){
        float new_value = PLUGIN_get_effect_value(_plugin, _controller->num, VALUE_FROM_PLUGIN);

        bool is_checked = _pd_plugin_widget->onoff_widget->isChecked();

        if(is_checked && new_value<0.49)
          _pd_plugin_widget->onoff_widget->setChecked(false);

        else if(!is_checked && new_value>0.51)
          _pd_plugin_widget->onoff_widget->setChecked(true);

      } else {
        int new_value = PLUGIN_get_effect_value(_plugin, _controller->num, VALUE_FROM_PLUGIN) * 10000;
        int old_value = _pd_plugin_widget->value_slider->value();

        if (new_value != old_value)
          _pd_plugin_widget->value_slider->setValue(new_value);
      }
    }
  };


  Timer _timer;

  Pd_Controller_widget(QWidget *parent, SoundPlugin *plugin, int controller_num)
    : QWidget(parent,"pd_plugin widget")
    , _plugin(plugin)
    , _controller(PD_get_controller(plugin, controller_num))
    , _conf(this, plugin, controller_num)
  {
    setupUi(this);

    value_slider->_is_a_pd_slider = true;

    //SLIDERPAINTER_start_auto_updater(value_slider->_painter);

    update_gui();

    _controller->has_gui = true;

    _timer._pd_plugin_widget = this;

    _timer._type = _controller->type;
    _timer._controller = _controller;
    _timer._plugin = _plugin;

    _timer.setInterval(k_timer_interval_here);
    _timer.start();
  }

  void update_gui(void){
    if(_controller->type==0 || _controller->type==1) {
      value_slider->show();
      onoff_widget->hide();
      //min_value_widget->show();
      //max_value_widget->show();

      float value = PLUGIN_get_effect_value(_plugin, _controller->num, VALUE_FROM_PLUGIN);
      //SLIDERPAINTER_setValue(value_slider->_painter, 5000);//value*10000);
      value_slider->setValue(value*10000);
      //value_slider->update();

      paint_slider_text();

    } else {
      value_slider->hide();
      //min_value_widget->hide();
      //max_value_widget->hide();
      onoff_widget->show();

      paint_onoff_text();
    }
  }

  void paint_slider_text(void){
    char buf[64]={0};
    _plugin->type->get_display_value_string(_plugin, _controller->num, buf, 64);
    SLIDERPAINTER_set_string(value_slider->_painter, QString::fromLatin1(buf));
  }

  void paint_onoff_text(void){
    bool is_checked = onoff_widget->isChecked();
  
    const char *name = _controller->name==NULL ? "<not set>" : _controller->name;

    if(is_checked)
      onoff_widget->setText(QString(name)+": On");
    else
      onoff_widget->setText(QString(name)+": Off");
  }

#if 0
  void paintEvent ( QPaintEvent * ev ){
    printf("paintEvent called\n");
    paint_slider_text();
  }
#endif

public slots:

  void on_popup_button_stateChanged(int val){
    if (val==Qt::Checked){
      _conf._showing = true;
    } else {
      _conf._showing = false;
    }
  }

  void on_value_slider_valueChanged( int val) {
    PLAYER_lock();{
      PLUGIN_set_effect_value(_plugin, -1, _controller->num, val/10000.0, PLUGIN_NONSTORED_TYPE, PLUGIN_STORE_VALUE);
    }PLAYER_unlock();

    paint_slider_text();
  }

  void on_onoff_widget_stateChanged(int val){
    float effect_value;
    if (val==Qt::Checked){
      effect_value = 1.0f;
    }else if(val==Qt::Unchecked){
      effect_value = 0.0f;
    }

    printf("Setting to %f\n",effect_value);

    PLAYER_lock();{
      PLUGIN_set_effect_value(_plugin, -1, _controller->num, effect_value, PLUGIN_NONSTORED_TYPE, PLUGIN_STORE_VALUE);
    }PLAYER_unlock();

    paint_onoff_text();
  }

};

#if 0
// (can be) Called from the player thread.
void PDGUI_update(void *gui){
  //Pd_Controller_widget *w = (Pd_Controller_widget*)gui;
  //w->value_slider->setValue(0.5*10000);
  //w->update();
  //w->update_gui();
}

void PDGUI_update_slider_value(void *gui){
  // Pd_Controller_widget *w = (Pd_Controller_widget*)gui;
  //w->update_gui();
}
#endif

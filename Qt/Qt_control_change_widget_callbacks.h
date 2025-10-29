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



#include "Qt_control_change_widget.h"


class Control_change_widget : public QWidget, public Ui::Control_change_widget{
  Q_OBJECT;



public:

  Control_change_widget(QWidget *parent=NULL, const char *name="control change widget")
    : QWidget(parent)
  {
    setupUi(this);
  }
  int ccnum;
  struct PatchData *patchdata;



public slots:

  void on_value_slider_valueChanged( int val)
  {
    value_spin->setValue(val);
  }


  void on_value_spin_valueChanged(int val)
  {
	  R_ASSERT_NON_RELEASE(val >= -127 && val<128);
	  
    if(value_slider->value()!=val)
      value_slider->setValue(val);
    
    patchdata->ccvalues[this->ccnum] = (char)val;
    printf("num: %d, value: %d\n",patchdata->cc[this->ccnum],val);

    MIDI_send3(patchdata,
               0xb0|patchdata->channel,
               patchdata->cc[this->ccnum],
               val
               );

    //set_editor_focus();
  }

  void on_value_spin_editingFinished (){
    set_editor_focus();
  }

  void on_onoff_toggled(bool val)
  {
    if (val==true){
      value_slider->setEnabled(true);
      value_spin->setEnabled(true);
      cctype->setEnabled(true);
      if (this->patchdata==NULL) return;
      this->patchdata->ccsonoff[this->ccnum] = true;
    }else if(val==false){
      value_slider->setEnabled(false);
      value_spin->setEnabled(false);
      cctype->setEnabled(false);
      if (this->patchdata==NULL) return;
      this->patchdata->ccsonoff[this->ccnum] = false;
    }
  }


  void on_cctype_activated( int val)
  {
	  R_ASSERT_NON_RELEASE(val >= -127 && val<128);
	  
	  patchdata->cc[this->ccnum] = (char)val;
	  patchdata->ccnames[this->ccnum] = talloc_strdup(get_ccnames()[val]);
	  
	  value_slider->_effect_num = patchdata->cc[ccnum];
	  printf("on_cctype_activated: Update effectnum for %d to %d\n",ccnum,patchdata->cc[ccnum]);
  }

};

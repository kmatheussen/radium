#include "Qt_control_change_widget.h"

class Control_change_widget : public QWidget, public Ui::Control_change_widget{
  Q_OBJECT;



public:

  Control_change_widget(QWidget *parent=NULL, const char *name="control change widget")
    : QWidget(parent,name)
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


  void on_value_spin_valueChanged( int val)
  {
    if(value_slider->value()!=val)
      value_slider->setValue(val);
    
    patchdata->ccvalues[this->ccnum] = val;
    printf("num: %d, value: %d\n",patchdata->cc[this->ccnum],val);

    D_PutMidi3(
               patchdata->midi_port,
               0xb0|patchdata->channel,
               patchdata->cc[this->ccnum],
               val
               );

    set_editor_focus();
  }

  void on_groupBox_toggled(bool val)
  {
    if (val==true){
      value_slider->setEnabled(true);
      value_spin->setEnabled(true);
      cctype->setEnabled(true);
      this->patchdata->ccsonoff[this->ccnum] = true;
    }else if(val==false){
      value_slider->setEnabled(false);
      value_spin->setEnabled(false);
      cctype->setEnabled(false);
      this->patchdata->ccsonoff[this->ccnum] = false;
    }
  }


  void on_cctype_activated( int val)
  {
    patchdata->cc[this->ccnum] = val;
    patchdata->ccnames[this->ccnum] = talloc_strdup((char*)ccnames[val]);
  }

};

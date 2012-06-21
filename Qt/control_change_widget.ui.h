/****************************************************************************
** ui.h extension file, included from the uic-generated form implementation.
**
** If you want to add, delete, or rename functions or slots, use
** Qt Designer to update this file, preserving your code.
**
** You should not define a constructor or destructor in this file.
** Instead, write your code in functions called init() and destroy().
** These will automatically be called by the form's constructor and
** destructor.
*****************************************************************************/


void Control_change_widget::value_slider_valueChanged( int val)
{
    value_spin->setValue(val);
}


void Control_change_widget::value_spin_valueChanged( int val)
{
    if(value_slider->value()!=val)
	value_slider->setValue(val);
    
    patchdata->ccvalues[this->ccnum] = val;
    printf("num: %d, value: %d\n",this->ccnum,val);
}


void Control_change_widget::onoff_stateChanged( int val)
{
    if (val==QButton::On){
	value_slider->setEnabled(true);
	value_spin->setEnabled(true);;
    }else if(val==QButton::Off){
	value_slider->setEnabled(false);
	value_spin->setEnabled(false);
    }
}

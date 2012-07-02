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

#include <qtabbar.h>
#include "../common/patch_proc.h"


// Volume

void Instrument_widget::volume_slider_valueChanged( int val)
{
    volume_spin->setValue(127-val);
}


void Instrument_widget::volume_spin_valueChanged( int val )
{
    if( volume_slider->value() != 127-val)
	volume_slider->setValue(127-val);
    fprintf(stderr,"Hepp hepp2 %d\n",val);
    
    patchdata->volume = val;
    set_editor_focus();
}


void Instrument_widget::volume_onoff_stateChanged( int val)
{
    if (val==QButton::On){
	volume_slider->setEnabled(true);
	volume_spin->setEnabled(true);;
	 patchdata->volumeonoff = true;
    }else if(val==QButton::Off){
	volume_slider->setEnabled(false);
	volume_spin->setEnabled(false);
	patchdata->volumeonoff = false;	
    }

}


// Velocity

void Instrument_widget::velocity_slider_valueChanged( int val)
{
    velocity_spin->setValue(127-val);
}


void Instrument_widget::velocity_spin_valueChanged( int val)
{
    if( velocity_slider->value() != 127-val)
	velocity_slider->setValue(127-val);
    fprintf(stderr,"Hepp hepp3 %d\n",val);

    patch->standardvel = val;
    set_editor_focus();
}


void Instrument_widget::velocity_onoff_stateChanged( int val)
{
    if (val==QButton::On){
	velocity_slider->setEnabled(true);
	velocity_spin->setEnabled(true);;
    }else if(val==QButton::Off){
	velocity_slider->setEnabled(false);
	velocity_spin->setEnabled(false);
    }
    
    
    fprintf(stderr,"state changed %d\n",val);
}


// Panning

void Instrument_widget::panning_slider_valueChanged( int val)
{
    panning_spin->setValue(val);
}

void Instrument_widget::panning_spin_valueChanged( int val)
{
    if( panning_slider->value() != val)
	panning_slider->setValue(val);
    fprintf(stderr,"Pan %d\n",val);
    
    patchdata->pan = val + 63;
    set_editor_focus();
}

void Instrument_widget::panning_onoff_stateChanged( int val)
{
    if (val==QButton::On){
	panning_slider->setEnabled(true);
	panning_spin->setEnabled(true);;
	patchdata->panonoff = true;
    }else if(val==QButton::Off){
	panning_slider->setEnabled(false);
	panning_spin->setEnabled(false);
	patchdata->panonoff = false;	
    }
}



void Instrument_widget::channel_valueChanged( int val)
{
    patchdata->channel = val;
    set_editor_focus();                                               
}


void Instrument_widget::msb_valueChanged( int val)
{
    patchdata->MSB = val;
    set_editor_focus();
}


void Instrument_widget::lsb_valueChanged( int val)
{
    patchdata->LSB = val;
    set_editor_focus();
}



// The returnPressed signal might do exactly what we need. This one is called during instrument widget init as well.
void Instrument_widget::name_widget_textChanged( const QString &string )
{
  //QTabBar *tab_bar = instruments_widget->tabs->tabBar();
  //tab_bar->tab(tab_bar->currentTab())->setText(string);
}



void Instrument_widget::name_widget_returnPressed()
{
    printf("return pressed\n");
    //focus = false;
    
    //QTabBar *tab_bar = instruments_widget->tabs->tabBar();
    //tab_bar->tab(tab_bar->currentTab())->setText(name_widget->text());
    instruments_widget->tabs->setTabLabel(this, name_widget->text());

    {
      struct Tracker_Windows *window = root->song->tracker_windows;
      struct WBlocks *wblock = window->wblock;
      struct WTracks *wtrack = wblock->wtrack;
      DO_GFX(
             patch->name = talloc_strdup((char*)name_widget->text().ascii());
             DrawWTrackHeader(window,wblock,wtrack);
             );
      MyWidget *my_widget = static_cast<MyWidget*>(window->os_visual.widget);
      my_widget->update();
    }

    set_editor_focus();
}



void Instrument_widget::preset_activated( int num)
{
    printf("activated preset %d\n",num-1);
    
    patchdata->preset = num-1;    
     set_editor_focus();
}


void Instrument_widget::port_activated( const QString &portname )
{
  if(portname=="<Create new port>"){
    struct Tracker_Windows *window=root->song->tracker_windows;
    ReqType reqtype=GFX_OpenReq(window,70,50,(char*)"Create port");

    char *name = GFX_GetString(window,reqtype,(char*)"Name: ");
    if(name!=NULL)
      patchdata->midi_port = MIDIgetPort(window,reqtype,name);

    GFX_CloseReq(window,reqtype);
  }else
    MIDISetPatchData(patch, (char*)"port", (char*)portname.ascii());

  fprintf(stderr, "Setting new port: \"%s\"\n",(char*)portname.ascii());

  updatePortsWidget(this);
}


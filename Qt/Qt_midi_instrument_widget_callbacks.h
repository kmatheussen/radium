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



#include "Qt_midi_instrument_widget.h"

class MIDI_instrument_widget : public QWidget, public Ui::MIDI_instrument_widget{
  Q_OBJECT;

public:

  Patch_widget *_patch_widget;
  Control_change_widget *cc_widgets[8];
  struct PatchData *patchdata;
  struct Patch *patch;

  MIDI_instrument_widget(QWidget *parent, struct Patch *patch)
    : QWidget(parent)
    , patch(patch)
  {
    setupUi(this);

    _patch_widget = new Patch_widget(this,patch);
    main_layout->insertWidget(0,_patch_widget);

    panning_slider->_patch = patch;
    panning_slider->_effect_num = 10;

    volume_slider->_patch = patch;
    volume_slider->_effect_num = 7;
  }
  

public slots:

  // Volume

  void on_volume_slider_valueChanged( int val)
  {
    volume_spin->setValue(127-val);
  }


  void on_volume_spin_valueChanged( int val )
  {
    if( volume_slider->value() != 127-val)
      volume_slider->setValue(127-val);

    //fprintf(stderr,"Volume: %d. channel: %d\n",val,patchdata->channel);
    
    patchdata->volume = val;

    D_PutMidi3(
               patchdata->midi_port,
               0xb0|patchdata->channel,
               7,
               patchdata->volume
               );
  }

  void on_volume_spin_editingFinished (){
    set_editor_focus();
  }

  void on_volumeBox_toggled(bool val)
  {
    if (val==true){
      volume_slider->setEnabled(true);
      volume_spin->setEnabled(true);;
      patchdata->volumeonoff = true;
    }else if(val==false){
      volume_slider->setEnabled(false);
      volume_spin->setEnabled(false);
      patchdata->volumeonoff = false;	
    }

  }


#if 0
  // Velocity

  void on_velocity_slider_valueChanged( int val)
  {
    velocity_spin->setValue(val);
  }


  void on_velocity_spin_valueChanged( int val)
  {
    if( velocity_slider->value() != val)
      velocity_slider->setValue(val);

    patch->standardvel = val*MAX_VELOCITY/127;
    //set_editor_focus();
  }

  void on_velocity_spin_editingFinished (){
    set_editor_focus();
  }
#endif

#if 0
  void on_velocity_onoff_stateChanged( int val)
  {
    if (val==Qt::Checked){
      velocity_slider->setEnabled(true);
      velocity_spin->setEnabled(true);;
    }else if(val==Qt::Unchecked){
      velocity_slider->setEnabled(false);
      velocity_spin->setEnabled(false);
    }
  }
#endif

  // Panning

  void on_panning_slider_valueChanged( int val)
  {
    panning_spin->setValue(val);
  }

  void on_panning_spin_valueChanged( int val)
  {
    if( panning_slider->value() != val)
      panning_slider->setValue(val);
    printf("Pan %d\n",val);
    
    patchdata->pan = val + 63;

    D_PutMidi3(
               patchdata->midi_port,
               0xb0|patchdata->channel,
               10,
               patchdata->pan
               );

    //set_editor_focus();
  }

  void on_panning_spin_editingFinished (){
    set_editor_focus();
  }

  void on_panningBox_toggled(bool val)
  {
    if (val==true){
      panning_slider->setEnabled(true);
      panning_spin->setEnabled(true);;
      patchdata->panonoff = true;
    }else if(val==false){
      panning_slider->setEnabled(false);
      panning_spin->setEnabled(false);
      patchdata->panonoff = false;	
    }
  }



  void on_channel_valueChanged( int val)
  {
    patchdata->channel = val-1;
    //set_editor_focus();                                               
  }

  void on_channel_editingFinished (){
    set_editor_focus();
  }

  void on_msb_valueChanged( int val)
  {
    patchdata->MSB = val;
    //set_editor_focus();
  }

  void on_msb_editingFinished (){
    set_editor_focus();
  }


  void on_lsb_valueChanged( int val)
  {
    patchdata->LSB = val;
    //set_editor_focus();
  }

  void on_lsb_editingFinished (){
    set_editor_focus();
  }


  void on_preset_activated( int num)
  {
    printf("activated preset %d\n",num-1);
    
    patchdata->preset = num-1;    
    set_editor_focus();
  }


  void on_port_activated( const QString &portname )
  {
    if(portname=="<Create new port>"){
      struct Tracker_Windows *window=root->song->tracker_windows;
      ReqType reqtype=GFX_OpenReq(window,70,50,(char*)"Create port");

      char *name = GFX_GetString(window,reqtype,(char*)"Name: ", true);
      if(name!=NULL)
        patchdata->midi_port = MIDIgetPort(window,reqtype,name,true);

      GFX_CloseReq(window,reqtype);
    }else
      MIDISetPatchData(patch, (char*)"port", (char*)portname.toUtf8().constData(), true);

    fprintf(stderr, "Setting new port: \"%s\"\n",(char*)portname.toUtf8().constData());

    updateMidiPortsWidget(this);
  }

};

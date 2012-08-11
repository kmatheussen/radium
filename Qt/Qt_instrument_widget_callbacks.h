#include "Qt_instrument_widget.h"

class Instrument_widget : public QWidget, public Ui::Instrument_widget{
  Q_OBJECT;

public:
  Instrument_widget(QWidget *parent=NULL)
    : QWidget(parent)
  {
    setupUi(this);
  }
  
  Control_change_widget *cc_widgets[8];
  struct PatchData *patchdata;
  struct Patch *patch;
                     

public slots:

  // Volume

  void on_volume_slider_valueChanged( int val)
  {
    volume_spin->setValue(val);
  }


  void on_volume_spin_valueChanged( int val )
  {
    if( volume_slider->value() != val)
      volume_slider->setValue(val);

    fprintf(stderr,"Volume: %d. channel: %d\n",val,patchdata->channel);
    
    patchdata->volume = val;

    D_PutMidi3(
               patchdata->midi_port,
               0xb0|patchdata->channel,
               7,
               patchdata->volume
               );

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


  // Velocity

  void on_velocity_slider_valueChanged( int val)
  {
    velocity_spin->setValue(val);
  }


  void on_velocity_spin_valueChanged( int val)
  {
    if( velocity_slider->value() != val)
      velocity_slider->setValue(val);

    patch->standardvel = val;
    set_editor_focus();
  }


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
    patchdata->channel = val;
    set_editor_focus();                                               
  }


  void on_msb_valueChanged( int val)
  {
    patchdata->MSB = val;
    set_editor_focus();
  }


  void on_lsb_valueChanged( int val)
  {
    patchdata->LSB = val;
    set_editor_focus();
  }



  // The returnPressed signal might do exactly what we need. This one is called during instrument widget init as well.
  void on_name_widget_textChanged( const QString &string )
  {
    //QTabBar *tab_bar = instruments_widget->tabs->tabBar();
    //tab_bar->tab(tab_bar->currentTab())->setText(string);
  }



  void on_name_widget_returnPressed()
  {
    printf("return pressed\n");

    if(name_widget->text()=="")
      return;

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
      EditorWidget *editor = static_cast<EditorWidget*>(window->os_visual.widget);
      editor->update();
    }

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

      char *name = GFX_GetString(window,reqtype,(char*)"Name: ");
      if(name!=NULL)
        patchdata->midi_port = MIDIgetPort(window,reqtype,name);

      GFX_CloseReq(window,reqtype);
    }else
      MIDISetPatchData(patch, (char*)"port", (char*)portname.ascii());

    fprintf(stderr, "Setting new port: \"%s\"\n",(char*)portname.ascii());

    updatePortsWidget(this);
  }

};

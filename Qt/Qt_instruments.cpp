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

extern "C"{
#include "../common/includepython.h"
#include "../api/radium_proc.h"
}

#include <qspinbox.h>

#include <qstring.h>
#include <qlineedit.h>
#include <qsplitter.h>
#include <qmainwindow.h>
#include <qevent.h>
#include <qtreeview.h>
#include <qlistwidget.h>

#include "../common/nsmtracker.h"

#include "../common/hashmap_proc.h"
#include "../common/undo.h"
#include "../mixergui/undo_chip_addremove_proc.h"
#include "../mixergui/undo_mixer_connections_proc.h"
#include "undo_instruments_widget_proc.h"
#include "../common/undo_patch_proc.h"
#include "../common/undo_tracks_proc.h"
#include "../common/visual_proc.h"
#include "../common/gfx_proc.h"
#include "../common/gfx_wtrackheaders_proc.h"
#include "../common/settings_proc.h"
#include "../midi/midi_i_plugin.h"
#include "../midi/midi_i_plugin_proc.h"
#include "../midi/midi_i_input_proc.h"
#include "../midi/OS_midigfx_proc.h"
#include "../midi/OS_midi_proc.h"
#include "Qt_colors_proc.h"

#include "Qt_MyQSlider.h"
#include "Qt_MyQCheckBox.h"
#include "Qt_MyQButton.h"
#include "Qt_MyQSpinBox.h"

#include "../mixergui/QM_MixerWidget.h"
#include "../audio/SoundPluginRegistry_proc.h"



#include "Qt_instruments_proc.h"


extern QApplication *qapplication;

extern struct Root *root;
extern struct Patch *g_currpatch;

#include "FocusSniffers.h"

#include "EditorWidget.h"
#include "../GTK/GTK_visual_proc.h"
#include "../OpenGL/Widget_proc.h"

void set_editor_focus(void){
  if(root==NULL)
    return;
  EditorWidget *editor = static_cast<EditorWidget*>(root->song->tracker_windows->os_visual.widget);

  // GL_lock is needed when using intel gfx driver to avoid crash caused by opening two opengl contexts simultaneously from two threads.
  GL_lock();{
    editor->setFocus();
  }GL_unlock();

#ifdef USE_GTK_VISUAL
  //GTK_SetFocus();
#endif
}



class Instruments_widget;
static Instruments_widget *instruments_widget;

class No_instrument_widget;
//static No_instrument_widget *no_instrument_widget;

class MIDI_instrument_widget;
class Audio_instrument_widget;

static void tab_name_has_changed(QWidget *tab, QString new_name);
//static void tab_selected();
static MIDI_instrument_widget *get_midi_instrument_widget(struct Patch *patch);
//static Audio_instrument_widget *get_audio_instrument_widget_from_patchdata(void *patchdata);
static Audio_instrument_widget *get_audio_instrument_widget(struct Patch *patch);
static void updateMidiPortsWidget(MIDI_instrument_widget *instrument);
static MIDI_instrument_widget *create_midi_instrument(struct Patch *patch);
static Audio_instrument_widget *create_audio_instrument_widget(struct Patch *patch);


const char **get_ccnames(void){
  static bool is_inited = false;
  static const char *ccnames[128];

  if (is_inited == false) {
    for(int i=0;i<128;i++)
      ccnames[i] = "";

    {
      ccnames[01] = "Modulation Wheel";
      ccnames[02] = "Breath Controller";
      ccnames[04] = "Foot Controller";
      ccnames[05] = "Portamento Time";
      ccnames[06] = "Data Entry MSB";
      ccnames[07] = "Volume";
      ccnames[10] = "Pan";
      ccnames[11] = "Expression";
      ccnames[16] = "Foot Controller";
      ccnames[28] = "Data Entry LSB";
      ccnames[64] = "Sustain Switch";
      ccnames[65] = "Portamento Switch";
      ccnames[66] = "Sostenuto";
      ccnames[67] = "Soft Pedal";
      ccnames[84] = "Portamento";
      ccnames[94] = "Variation Depth";
      ccnames[120] = "All Sounds Off";
      ccnames[121] = "Reset All Controllers";
      ccnames[123] = "All Notes Off";
      ccnames[124] = "Omni Mode Off";
      ccnames[125] = "Omni Mode On";
      ccnames[126] = "Mono Mode";
      ccnames[127] = "Poly Mode";
    }

    is_inited = true;
  }

    return ccnames;
}

//#define protected public
#if USE_QT3
#include "Qt_instruments_widget.cpp"
#include "Qt_control_change_widget.cpp"
#include "Qt_midi_instrument_widget.cpp"
#include "Qt_audio_instrument_widget.cpp"
#include "Qt_no_instrument_widget.cpp"
#endif

#if USE_QT4
#include "mQt_instruments_widget_callbacks.h"
#include "mQt_control_change_widget_callbacks.h"
#include "mQt_patch_widget_callbacks.h"
#include "mQt_midi_instrument_widget_callbacks.h"
#include "mQt_audio_instrument_widget_callbacks.h"
#include "mQt_no_instrument_widget_callbacks.h"
#include "mQt_vst_paths_widget_callbacks.h"
//#include "mQt_mixer_widget_callbacks.h"
#endif

static const char *gm_names[] = {
  "Acoustic Grand Piano",
  "Bright Acoustic Piano",
  "Electric Grand Piano",
  "Honky-tonk Piano",
  "Electric Piano 1",
  "Electric Piano 2",
  "Harpsichord",
  "Clavinet",
  "Celesta",
  "Glockenspiel",
  "Music Box",
  "Vibraphone",
  "Marimba",
  "Xylophone",
  "Tubular Bells",
  "Dulcimer",
  "Drawbar Organ",
  "Percussive Organ",
  "Rock Organ",
  "Church Organ",
  "Reed Organ",
  "Accordion",
  "Harmonica",
  "Tango Accordion",
  "Acoustic Guitar (nylon)",
  "Acoustic Guitar (steel)",
  "Electric Guitar (jazz)",
  "Electric Guitar (clean)",
  "Electric Guitar (muted)",
  "Overdriven Guitar",
  "Distortion Guitar",
  "Guitar Harmonics",
  "Acoustic Bass",
  "Electric Bass (finger)",
  "Electric Bass (pick)",
  "Fretless Bass",
  "Slap Bass 1",
  "Slap Bass 2",
  "Synth Bass 1",
  "Synth Bass 2",
  "Violin",
  "Viola",
  "Cello",
  "Contrabass",
  "Tremolo Strings",
  "Pizzicato Strings",
  "Orchestral Harp",
  "Timpani",
  "String Ensemble 1",
  "String Ensemble 2",
  "Synth Strings 1",
  "Synth Strings 2",
  "Choir Aahs",
  "Voice Oohs",
  "Synth Choir",
  "Orchestra Hit",
  "Trumpet",
  "Trombone",
  "Tuba",
  "Muted Trumpet",
  "French Horn",
  "Brass Section",
  "Synth Brass 1",
  "Synth Brass 2",
  "Soprano Sax",
  "Alto Sax",
  "Tenor Sax",
  "Baritone Sax",
  "Oboe",
  "English Horn",
  "Bassoon",
  "Clarinet",
  "Piccolo",
  "Flute",
  "Recorder",
  "Pan Flute",
  "Blown Bottle",
  "Shakuhachi",
  "Whistle",
  "Ocarina",
  "Lead 1 (square)",
  "Lead 2 (sawtooth)",
  "Lead 3 (calliope)",
  "Lead 4 (chiff)",
  "Lead 5 (charang)",
  "Lead 6 (voice)",
  "Lead 7 (fifths)",
  "Lead 8 (bass + lead)",
  "Pad 1 (new age)",
  "Pad 2 (warm)",
  "Pad 3 (polysynth)",
  "Pad 4 (choir)",
  "Pad 5 (bowed)",
  "Pad 6 (metallic)",
  "Pad 7 (halo)",
  "Pad 8 (sweep)",
  "FX 1 (rain)",
  "FX 2 (soundtrack)",
  "FX 3 (crystal)",
  "FX 4 (atmosphere)",
  "FX 5 (brightness)",
  "FX 6 (goblins)",
  "FX 7 (echoes)",
  "FX 8 (sci-fi)",
  "Sitar",
  "Banjo",
  "Shamisen",
  "Koto",
  "Kalimba",
  "Bagpipe",
  "Fiddle",
  "Shanai",
  "Tinkle Bell",
  "Agogo",
  "Steel Drums",
  "Woodblock",
  "Taiko Drum",
  "Melodic Tom",
  "Synth Drum",
  "Reverse Cymbal,",
  "Guitar Fret Noise",
  "Breath Noise",
  "Seashore",
  "Bird Tweet",
  "Telephone Ring",
  "Helicopter",
  "Applause",
  "Gunshot"
  };

static void updateMidiPortsWidget(MIDI_instrument_widget *instrument){
  int item_num = 0;

  int num_ports;
  char **portnames = MIDI_getPortNames(&num_ports,false);

  while(instrument->port->count()>0)
    instrument->port->removeItem(0);

  for(int i = 0; i<num_ports ; i++){
    instrument->port->insertItem(portnames[i]);
    if(!strcmp(portnames[i],instrument->patchdata->midi_port->name))
      item_num = i;
  }

  instrument->port->insertItem("<Create new port>");

  instrument->port->setCurrentItem(item_num);

instrument->_patch_widget->updateWidgets();
}

static MIDI_instrument_widget *create_midi_instrument_widget(const char *name, struct Patch *patch){
  EditorWidget *editor = static_cast<EditorWidget*>(root->song->tracker_windows->os_visual.widget);
  MIDI_instrument_widget *instrument = new MIDI_instrument_widget(editor->main_window,patch);
    instrument->patch = patch;

    struct PatchData *patchdata = (struct PatchData*)patch->patchdata;
    instrument->patchdata = patchdata;

    const char **ccnames = get_ccnames();

    {
      int ccnum = 0;
      for(int y=0;y<8;y++){
        for(int x=0;x<1;x++){
          Control_change_widget *cc = new Control_change_widget(instrument, "hepp");
          cc->value_slider->_patch = patch;
          cc->value_slider->_effect_num = patchdata->cc[ccnum];

          for(int i=0;i<128;i++){
            char temp[500];
            bool is_set = false;
            
            if(patchdata!=NULL)
              for(int ip=0;ip<8;ip++)
                if (patchdata->cc[ip] == i) {
                  sprintf(temp, "%3d: %s", i, patchdata->ccnames[ip]);
                  is_set = true;
                  break;
                }

            if (is_set==false)
              sprintf(temp, "%3d: %s", i, ccnames[i]==NULL?"":ccnames[i]);
            
            cc->cctype->insertItem(temp);
          }

          if(patchdata!=NULL){
            cc->patchdata = patchdata;

            cc->cctype->setCurrentItem(patchdata->cc[ccnum]);
          }

          instrument->cc_widgets[ccnum] = cc;
          cc->ccnum = ccnum++;

          //cc->groupBox->setTitle(" cc " + QString::number(ccnum));

          // todo: fix
          //instrument->control_change_groupLayout->addWidget(cc,y,x);
          instrument->control_change_layout->addWidget(cc,y,x);
          //instrument->control_change_layout->addWidget(cc,0,ccnum-1);
        }
      }
    }

    instrument->preset->insertItem("<Not set>");
    {
      for(int i=0;i<128;i++) {
        char temp[500];
        sprintf(temp,"%3d:  %s", i+1, gm_names[i]);
        instrument->preset->insertItem(temp);
      }
    }

    //updatePortsWidget(instrument);

    instrument->_patch_widget->name_widget->setText(name);

    return instrument;
}


static Audio_instrument_widget *create_audio_instrument_widget(struct Patch *patch){
  EditorWidget *editor = static_cast<EditorWidget*>(root->song->tracker_windows->os_visual.widget);
  Audio_instrument_widget *instrument = new Audio_instrument_widget(editor->main_window,patch);

  fprintf(stderr,"instrument: %p, patch: %p\n",instrument,patch);
  if(instrument==NULL){
    fprintf(stderr,"instrument==NULL\n");
    return NULL;
  }

  instrument->_patch_widget->name_widget->setText(patch->name);

  //instruments_widget->tabs->insertTab(instrument, QString::fromLatin1(patch->name), instruments_widget->tabs->count());
  instruments_widget->tabs->insertWidget(instruments_widget->tabs->count(),instrument);

  // (forgot to take copy)
  instruments_widget->tabs->setCurrentWidget(instrument);
  MW_update_all_chips();

  instrument->updateWidgets();

  return instrument;
}

static void InstrumentWidget_create_audio_instrument_widget(struct Patch *patch){
  create_audio_instrument_widget(patch);
}

static MIDI_instrument_widget *create_midi_instrument(struct Patch *patch){
  
  MIDI_instrument_widget *instrument = create_midi_instrument_widget(patch->name, patch);

#if 0
  //This was a failed attempt to make the no_widget instrument more fancy
  {
    //instrument->Instrument_widgetLayout->removeItem(instrument->select_operationLayout);
    delete instrument->select_operation;
    //delete instrument->select_operationLayout;

    instrument->Instrument_widgetLayout->removeItem(instrument->nameBoxLayout);
    instrument->Instrument_widgetLayout->removeItem(instrument->panningBoxLayout);

    // The next two lines seems to do what I want, but I don't know why.
    instrument->Instrument_widgetLayout->addItem(instrument->nameBoxLayout, 0,1);
    instrument->Instrument_widgetLayout->addItem(instrument->panningBoxLayout, 0,1);
  }
#endif

  //instrument->nameBox->setEnabled(true);
  instrument->panningBox->setEnabled(true);
  instrument->volumeBox->setEnabled(true);
#if 0
  instrument->velocityBox->setEnabled(true);
  instrument->velocity_slider->setEnabled(true);
  instrument->velocity_spin->setEnabled(true);
#endif
  instrument->voiceBox->setEnabled(true);
#if 0
  instrument->presetBox->setEnabled(true);
  instrument->channelBox->setEnabled(true);
  instrument->MSBBox->setEnabled(true);
  instrument->LSBBox->setEnabled(true);
  //instrument->control_change_group->setEnabled(true);
#endif

  //instruments_widget->tabs->insertTab(instrument, QString::fromLatin1(patch->name), instruments_widget->tabs->count());
  instruments_widget->tabs->insertWidget(instruments_widget->tabs->count(),instrument);
  //instruments_widget->tabs->showPage(instrument);

  return instrument;
}


QWidget *createInstrumentsWidget(void){
  instruments_widget = new Instruments_widget();

  {
    //const char *name = "<No Instrument>";

    //no_instrument_widget = new No_instrument_widget();

    delete instruments_widget->tabs->widget(0); // Delete default tab
    //instruments_widget->tabs->insertTab(no_instrument_widget, QString::fromLatin1(name), 0);
    //instruments_widget->tabs->showPage(no_instrument_widget);
  }

  setWidgetColors(instruments_widget);

  return instruments_widget;
}

#if 1
static void set_widget_height(int height){
  QMainWindow *main_window = static_cast<QMainWindow*>(root->song->tracker_windows->os_visual.main_window);
  EditorWidget *editor = static_cast<EditorWidget*>(root->song->tracker_windows->os_visual.widget);
  QSplitter *splitter = editor->ysplitter;

#if USE_QT4
#  define QValueList QList
#endif
  //printf("set_widghet_height. main_window->height(): %d, splitter: %p\n",main_window->height(),splitter);
  QValueList<int> currentSizes = splitter->sizes();
  currentSizes[0] = main_window->height() - height;
  currentSizes[1] = height;
  splitter->setSizes(currentSizes);
#if USE_QT4
#  undef QValueList
#endif
}
#endif

bool GFX_InstrumentWindowIsVisible(void){
  return instruments_widget->isVisible();
}

void GFX_SetMinimalInstrumentWindow(void){
  set_widget_height(30);

  instruments_widget->adjustSize();
}

void GFX_InstrumentWindowToFront(void){
  //set_widget_height(30);
  instruments_widget->show();
}

void GFX_InstrumentWindowToBack(void){
  instruments_widget->hide();
  //set_widget_height(0);
}


void GFX_showHideInstrumentWidget(struct Tracker_Windows *window){
  if(instruments_widget->height() < 10)
    GFX_InstrumentWindowToFront();
  else
    GFX_InstrumentWindowToBack();
}

// These functions (MIDIGFX_*) seems to be only used by the GTK1 instrument window when the wrong value
// was received from the GUI, and the original value must be sent back.
// It would make sense to use them though, to make it simpler to implement new GUI backends.
void MIDIGFX_UpdateAll(void){}
void MIDIGFX_SetPanSlider(bool on,int value){}
void MIDIGFX_SetVolumeSlider(bool on,int value){}
void MIDIGFX_SetLSB(int lsb){}
void MIDIGFX_SetMSB(int msb){}
void MIDIGFX_SetChannel(int ch){}
void MIDIGFX_SetCCSlider(int slidernum,bool on,int value){}


// Warning, tabs are not updated immediately after they are created.
static MIDI_instrument_widget *get_midi_instrument_widget(struct Patch *patch){
  QStackedWidget* tabs = instruments_widget->tabs;

  for(int i=0;i<tabs->count();i++){
    MIDI_instrument_widget *instrument = dynamic_cast<MIDI_instrument_widget*>(tabs->widget(i));
    if(instrument!=NULL && instrument->patch==patch)
      return instrument;
  }

  return NULL;
}
/*
// Warning, tabs is not updated immediately after a tab has been inserted into it. (or deleted from it)
static Audio_instrument_widget *get_audio_instrument_widget_from_patchdata(void *patchdata){
  QStackedWidget* tabs = instruments_widget->tabs;

  for(int i=0;i<tabs->count();i++){
    Audio_instrument_widget *instrument = dynamic_cast<Audio_instrument_widget*>(tabs->widget(i));
    if(instrument!=NULL && instrument->_patch->patchdata==patchdata)
      return instrument;
  }

  return NULL;
}
*/

static Audio_instrument_widget *get_audio_instrument_widget(struct Patch *patch){
  QStackedWidget* tabs = instruments_widget->tabs;

  for(int i=0;i<tabs->count();i++){
    Audio_instrument_widget *instrument = dynamic_cast<Audio_instrument_widget*>(tabs->widget(i));
    if(instrument!=NULL && instrument->_patch==patch)
      return instrument;
  }

  return NULL;
}

// * This is the entry point for creating audio instruments.
// * The entry point for delete any instrument is common/patch.c/PATCH_delete
//
SoundPlugin *add_new_audio_instrument_widget(struct SoundPluginType *plugin_type, double x, double y, bool autoconnect, const char *name){
    if(plugin_type==NULL)
      plugin_type = MW_popup_plugin_selector(name, x, y, autoconnect);

    if(plugin_type==NULL)
      return NULL;

    SoundPlugin *plugin;

    {

      struct Tracker_Windows *window = root->song->tracker_windows;
      struct WBlocks *wblock = window->wblock;
      struct WTracks *wtrack = wblock->wtrack;

      Undo_Track(window,wblock,wtrack,wblock->curr_realline);      
      Undo_Patch_CurrPos();
      Undo_InstrumentsWidget_CurrPos();
      Undo_MixerConnections_CurrPos();

      plugin = MW_add_plugin(plugin_type, x, y);
      if(plugin==NULL)
        return NULL;

      struct Patch *patch = NewPatchCurrPos(AUDIO_INSTRUMENT_TYPE, plugin, name==NULL ? PLUGIN_generate_new_patchname(plugin_type) : name);

      Undo_Chip_Add_CurrPos(patch); // It works fine to call Undo_Chip_Add right after the chip has been created.

      //wtrack->track->patch = patch; // Bang!
      plugin->patch = patch;

      create_audio_instrument_widget(patch);

      if(autoconnect==true)
        MW_autoconnect_plugin(plugin);
    }

    return plugin;
}

static void update_midi_instrument_widget(MIDI_instrument_widget *instrument, struct Patch *patch){
  struct PatchData *patchdata = (struct PatchData*)patch->patchdata;

  instrument->volume_spin->setValue(patchdata->volume);
  instrument->panning_spin->setValue(patchdata->pan-63);
  instrument->channel->setValue(patchdata->channel);
  instrument->msb->setValue(patchdata->MSB);
  instrument->lsb->setValue(patchdata->LSB);

  instrument->volumeBox->setChecked(patchdata->volumeonoff);
  instrument->panningBox->setChecked(patchdata->panonoff);

  instrument->preset->setCurrentItem(patchdata->preset+1);

  updateMidiPortsWidget(instrument);

  for(int ccnum=0;ccnum<8;ccnum++){
    Control_change_widget *cc = instrument->cc_widgets[ccnum];
    cc->value_spin->setValue(patchdata->ccvalues[ccnum]);
    cc->onoff->setChecked(patchdata->ccsonoff[ccnum]);
    cc->cctype->setCurrentItem(patchdata->cc[ccnum]);

    cc->value_slider->_patch = patch;
    cc->value_slider->_effect_num = patchdata->cc[ccnum];
    printf("Update effectnum for %d to %d\n",ccnum,patchdata->cc[ccnum]);
  }
}

void update_audio_instrument_widget(Audio_instrument_widget *instrument, struct Patch *patch){
  
  instrument->updateWidgets();

  instrument->_plugin_widget->update_widget();
}

void GFX_update_instrument_widget(struct Patch *patch){

  if(patch->instrument==get_MIDI_instrument()){
    printf("PP update. Instrument name: \"%s\". port name: \"%s\"\n",patch==NULL?"(null)":patch->name,patch==NULL?"(null)":((struct PatchData*)patch->patchdata)->midi_port->name);

    MIDI_instrument_widget *instrument = get_midi_instrument_widget(patch);
    if(instrument==NULL){
      return;
    }
    
    update_midi_instrument_widget(instrument,patch);

  }else if(patch->instrument==get_audio_instrument()){
    Audio_instrument_widget *instrument = get_audio_instrument_widget(patch);
    if(instrument==NULL){
      printf("GFX_update_instrument_widget. instrument==NULL\n");
      return;
    }
      
    update_audio_instrument_widget(instrument,patch);
  }
}

void GFX_update_all_instrument_widgets(void){
  QStackedWidget* tabs = instruments_widget->tabs;

  //printf("*(((((((((( Calling. Update all %d:\n",tabs->count());
  for(int i=0;i<tabs->count();i++){
    MIDI_instrument_widget *midi_instrument = dynamic_cast<MIDI_instrument_widget*>(tabs->widget(i));
    Audio_instrument_widget *audio_instrument = dynamic_cast<Audio_instrument_widget*>(tabs->widget(i));
    //printf("Update all %d: %p/%p\n",i,midi_instrument,audio_instrument);

    if(midi_instrument!=NULL){

      if(midi_instrument->patch->patchdata!=NULL)
        update_midi_instrument_widget(midi_instrument,midi_instrument->patch);

    }else if(audio_instrument!=NULL){

      if(audio_instrument->_patch->patchdata!=NULL)
        update_audio_instrument_widget(audio_instrument,audio_instrument->_patch);

    }
  }
}

static bool called_from_pp_update = false;

void GFX_PP_Update(struct Patch *patch){
  called_from_pp_update = true;{
    if(g_currpatch==patch)
      goto exit;
    if(patch==NULL){

      //instruments_widget->tabs->showPage(no_instrument_widget);

    }else if(patch->instrument==get_MIDI_instrument()){
      printf("PP update. Instrument name: \"%s\". port name: \"%s\"\n",patch==NULL?"(null)":patch->name,patch==NULL?"(null)":((struct PatchData*)patch->patchdata)->midi_port->name);

      MIDI_instrument_widget *instrument = get_midi_instrument_widget(patch);
      if(instrument==NULL){
        instrument = create_midi_instrument(patch);
        //instrument = get_midi_instrument_widget(patch);
      }

      update_midi_instrument_widget(instrument,patch);
      
      instruments_widget->tabs->setCurrentWidget(instrument);
      MW_update_all_chips();
      DrawAllWTrackHeaders(root->song->tracker_windows, root->song->tracker_windows->wblock);

      MIDI_SetThroughPatch(patch);

    }else if(patch->instrument==get_audio_instrument()){
      Audio_instrument_widget *instrument = get_audio_instrument_widget(patch);
      if(instrument==NULL){
        instrument = create_audio_instrument_widget(patch);
        //instrument = get_audio_instrument_widget(patch);
      }
      
      update_audio_instrument_widget(instrument,patch);
      instruments_widget->tabs->setCurrentWidget(instrument);
      MW_update_all_chips();
      DrawAllWTrackHeaders(root->song->tracker_windows, root->song->tracker_windows->wblock);

      MIDI_SetThroughPatch(patch);

    }else if(patch->instrument!=NULL){
      RError("PP_Update: Don't know how to handle instrument %p",patch->instrument);
    }

    g_currpatch = patch;


  }exit: called_from_pp_update = false;
}


static QString last_filename;
static QString last_preset_path = "";


static hash_t *load_preset_state(void){
  num_users_of_keyboard++;
  QString filename;
  
  GL_lock();{ // GL_lock is needed when using intel gfx driver to avoid crash caused by opening two opengl contexts simultaneously from two threads.
    filename = QFileDialog::getOpenFileName(
                                            g_mixer_widget,
                                            "Load Effect configuration",
                                            last_preset_path,
                                            "Radium Effect Configuration (*.rec)",
                                            0,
                                            QFileDialog::DontUseNativeDialog
                                            );
  }GL_unlock();
  
  num_users_of_keyboard--;

  if(filename=="")
    return NULL;

  last_preset_path = QFileInfo(filename).absoluteDir().path();
  
  disk_t *file = DISK_open_for_reading(filename);
  if(file==NULL){
    QMessageBox msgBox;
    msgBox.setText("Could not open file.");
    msgBox.setStandardButtons(QMessageBox::Ok);
    msgBox.setDefaultButton(QMessageBox::Ok);
    safeExec(msgBox);
    return NULL;
  }

  hash_t *state = HASH_load(file);
  DISK_close_and_delete(file);

  if(state==NULL){
    QMessageBox msgBox;
    msgBox.setText("File does not appear to be a valid effects settings file");
    msgBox.setStandardButtons(QMessageBox::Ok);
    msgBox.setDefaultButton(QMessageBox::Ok);
    safeExec(msgBox);
    return NULL;
  }

  last_filename = QFileInfo(filename).baseName();

  return state;
}


struct Patch *InstrumentWidget_new_from_preset(hash_t *state, const char *name, double x, double y, bool autoconnect){
  if (state==NULL) {
    state = load_preset_state();
    if (state != NULL && name==NULL)
      name = talloc_strdup(last_filename.toUtf8().constData());      
  }
  
  if (state == NULL)
    return NULL;

  //Undo_Track(window,wblock,wtrack,wblock->curr_realline);      
  Undo_Patch_CurrPos();
  Undo_InstrumentsWidget_CurrPos();
  Undo_MixerConnections_CurrPos();
  
  struct Patch *patch = CHIP_create_from_plugin_state(state, name, x, y);
  if (patch!=NULL){
    if (autoconnect) {
      struct SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
      MW_autoconnect_plugin(plugin);
    }

    Undo_Chip_Add_CurrPos(patch); // It works fine to call Undo_Chip_Add right after the chip has been created. (except that it's not very logical)
    create_audio_instrument_widget(patch);
  }

  return patch;
}

void InstrumentWidget_load_preset(struct Patch *patch){
  hash_t *state = load_preset_state();
  if (state==NULL)
    return;
  
  SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
  
  Undo_Open();{
    plugin = PLUGIN_set_from_state(plugin, state);
  }Undo_Close();

  if (plugin==NULL)
    Undo_CancelLastUndo();
  else
    GFX_update_instrument_widget(plugin->patch);
}

void InstrumentWidget_save_preset(struct Patch *patch){

  SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
  
  num_users_of_keyboard++;
  QString filename;
  
  GL_lock();{ // GL_lock is needed when using intel gfx driver to avoid crash caused by opening two opengl contexts simultaneously from two threads.
    filename = QFileDialog::getSaveFileName(
                                            g_mixer_widget,
                                            "Save Effect configuration",
                                            last_preset_path,
                                            "Radium Effect Configuration (*.rec)",
                                            0,
                                            QFileDialog::DontUseNativeDialog                                            
                                            );
  }GL_unlock();
  
  num_users_of_keyboard--;
  
  if(filename=="")
    return;

  last_preset_path = QFileInfo(filename).absoluteDir().path();
    
  disk_t *file = DISK_open_for_writing(filename);
  
  if(file==NULL){
    QMessageBox msgBox;
    msgBox.setText("Could not save file.");
    msgBox.setStandardButtons(QMessageBox::Ok);
    msgBox.setDefaultButton(QMessageBox::Ok);
    safeExec(msgBox);
    return;
  }
  
  hash_t *state = PLUGIN_get_state(plugin);
  
  HASH_save(state, file);
  
  DISK_close_and_delete(file);
}

static void InstrumentWidget_remove_patch(struct Patch *patch){
  QStackedWidget* tabs = instruments_widget->tabs;

  Undo_InstrumentsWidget_CurrPos();

  MIDI_instrument_widget *w1 = get_midi_instrument_widget(patch);
  if(w1!=NULL){
    tabs->removeWidget(w1); // Undo is storing the tab widget, so we can't delete it.
    return;
  }

  Audio_instrument_widget *w2 = get_audio_instrument_widget(patch);
  if(w2==NULL){
    RError("No such patch widget: %p\n",patch);
    return;
  } else
    tabs->removeWidget(w2);  // Undo is storing the tab widget, so we can't delete it.
}

void InstrumentWidget_update(struct Patch *patch){
  InstrumentWidget_remove_patch(patch);
  InstrumentWidget_create_audio_instrument_widget(patch);
}

void GFX_remove_patch_gui(struct Patch *patch){
  InstrumentWidget_remove_patch(patch);

  if (patch->instrument==get_audio_instrument()) {
    
    SoundPlugin *plugin = (SoundPlugin*) patch->patchdata;
    MW_delete_plugin(plugin);

    MW_update_all_chips();
      
  }
}

void GFX_update_instrument_patch_gui(struct Patch *patch){
  printf("Called GFX_update_instrument_patch_gui for patch \"%s\"\n",patch==NULL?"<>":patch->name);
  if(patch!=NULL && patch->patchdata!=NULL && patch->instrument->PP_Update!=NULL)
    patch->instrument->PP_Update(patch->instrument,
                                 patch);
#if 0
  if(wblock->wtrack->track->patch!=NULL && wblock->wtrack->track->patch->instrument->PP_Update!=NULL)
    wblock->wtrack->track->patch->instrument->PP_Update(wblock->wtrack->track->patch->instrument,
                                                        wblock->wtrack->track->patch);
#endif
}

static void tab_name_has_changed(QWidget *tab, QString new_name) {

  if(new_name==""){
    //name_widget->setText("pip");
    new_name = "pip";
  }

  //QTabBar *tab_bar = instruments_widget->tabs->tabBar();
  //tab_bar->tab(tab_bar->currentTab())->setText(name_widget->text());
  //instruments_widget->tabs->setTabLabel(tab, new_name);
  
  {
    struct Tracker_Windows *window = root->song->tracker_windows;
    struct WBlocks *wblock = window->wblock;
    DO_GFX(
           g_currpatch->name = talloc_strdup((char*)new_name.toUtf8().constData());
           DrawAllWTrackHeaders(window,wblock);
           );
    EditorWidget *editor = static_cast<EditorWidget*>(window->os_visual.widget);
    editor->updateEditor();
  }
}

#if 0
static void tab_selected(){
  //printf("tab selected -%s-\n",tabname.toUtf8().constData());

  if(called_from_pp_update==true)
    return;

  MIDI_instrument_widget *midi_instrument = dynamic_cast<MIDI_instrument_widget*>(instruments_widget->tabs->currentWidget());

  // We don't want to current track patch when selecting a different widget instrument.
  Audio_instrument_widget *audio_instrument = dynamic_cast<Audio_instrument_widget*>(instruments_widget->tabs->currentWidget());

  if(midi_instrument==NULL && audio_instrument==NULL)
    return;

  if(midi_instrument!=NULL){
    g_currpatch = midi_instrument->patch;
  }else
    g_currpatch = audio_instrument->_patch;

#if 0

  if(midi_instrument!=NULL)
    MIDI_SetThroughPatch(g_currpatch);

  {
    struct Tracker_Windows *window = root->song->tracker_windows;
    struct WBlocks *wblock = window->wblock;
    struct WTracks *wtrack = wblock->wtrack;

    DO_GFX(
           wtrack->track->patch = g_currpatch;
           DrawWTrackHeader(window,wblock,wtrack);
           );

    EditorWidget *editor = static_cast<EditorWidget*>(window->os_visual.widget);
    editor->updateEditor();
  }
#endif

  if(midi_instrument!=NULL){
    updateMidiPortsWidget(midi_instrument);
    //update_midi_instrument_widget(midi_instrument,midi_instrument->patch);
  }else if(audio_instrument!=NULL){
    //update_audio_instrument_widget(audio_instrument,audio_instrument->_patch);
  }
}
#endif

void close_all_instrument_widgets(void){
  QStackedWidget* tabs = instruments_widget->tabs;

  while(tabs->count()>0){
    //if(dynamic_cast<No_instrument_widget*>(tabs->page(0)))
    //  tabs->removeTab(1);
    //else
    tabs->removeWidget(tabs->widget(0));
  }

  MW_update_all_chips();
}

struct Patch *get_current_instruments_gui_patch(void){
  if(instruments_widget==NULL || instruments_widget->tabs==NULL)
    return NULL;

  QStackedWidget* tabs = instruments_widget->tabs;

  {
    MIDI_instrument_widget *instrument = dynamic_cast<MIDI_instrument_widget*>(tabs->currentWidget());
    if(instrument!=NULL)
      return instrument->patch;
  }

  {
    Audio_instrument_widget *instrument = dynamic_cast<Audio_instrument_widget*>(tabs->currentWidget());
    if(instrument!=NULL)
      return instrument->_patch;
  }

  //RError("Current widget is not a known instrument: %p",tabs->currentWidget());
  // above line uncommented since widgets are not available during load.

  return NULL;
}

#if 0
hash_t *create_instrument_widget_order_state(void){
  QTabWidget* tabs = instruments_widget->tabs;

  hash_t *state = HASH_create(tabs->count());

  for(int i=0;i<tabs->count();i++)
    HASH_put_string_at(state, i, tabs->tabText(i).toUtf8().constData());

  printf("___________Saving state: =-----\n");
  HASH_save(state,stdout);
  printf("____________State saved: =-----\n");
  return state;
}

void recreate_instrument_widget_order_from_state(hash_t *state){
  QTabWidget* tabs = instruments_widget->tabs;

  int num_tabs = tabs->count();

  QWidget *tab_widgets[num_tabs];
  QString tab_names[num_tabs];

  int i=0;
  while(tabs->count()>0){
    tab_widgets[i]=tabs->widget(0);
    tab_names[i]=tabs->tabText(0);
    i++;
    tabs->removeTab(0);
  }

  for(int i=HASH_get_num_elements(state)-1 ; i>=0 ; i--){

    QString tab_name = HASH_get_string_at(state,i);
    for(int pos=0;pos<num_tabs;pos++){
      if(tab_name==tab_names[pos]){
        tabs->insertTab(0,tab_widgets[pos],tab_name);
        tab_names[pos]="<<<NOT THIS ONE 666 _!#@$!%__ John Wane was a nazi."; // several tabs may have the same name, but hopefully not this one.
        break;
      }
    }
  } 

  //update_all_instrument_widgets();
}
#endif

#include "undo_instruments_widget.cpp"


void OS_VST_config(struct Tracker_Windows *window){
#if defined(FOR_MACOSX)
  GFX_Message(NULL,"No VST options to edit on OSX");
#else
  //EditorWidget *editor=(EditorWidget *)window->os_visual.widget;
  Vst_paths_widget *vst_paths_widget=new Vst_paths_widget(NULL); // I'm not quite sure i it's safe to make this one static. It seems to work, but shouldn't the dialog be deleted when destroying the window? Not having it static is at least safe, although it might leak some memory.
  vst_paths_widget->show();
#endif  
  printf("Ohjea\n");
}


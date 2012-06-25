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
#include "Python.h"
#include "../api/radium_proc.h"
}

#include "../common/nsmtracker.h"
#include "../common/visual_proc.h"
#include "../common/gfx_proc.h"
#include "../common/gfx_wtrackheaders_proc.h"
#include "../midi/midi_i_plugin.h"
#include "../midi/midi_i_plugin_proc.h"
#include "../midi/OS_midigfx_proc.h"
#include "../midi/OS_midi_proc.h"
#include "MyWidget.h"

#include "Qt_instruments_proc.h"


extern struct Root *root;
extern struct Patch *g_currpatch;

#include "qstring.h"
#include "qlineedit.h"
#include "qspinbox.h"

static int num_focus = 0;

void set_editor_focus(void){
  MyWidget *my_widget = static_cast<MyWidget*>(root->song->tracker_windows->os_visual.widget);
  my_widget->setFocus();
}


#define MakeFocusOverrideClass(Class)                    \
  class My##Class : public Class {                       \
  public:                                                \
  My##Class(QWidget *parent, const char *name)           \
  : Class(parent, name)                                  \
    {                                                    \
    }                                                    \
                                                         \
  void focusInEvent ( QFocusEvent *e ){                  \
    fprintf(stderr," oh yeah, i got fokus\n");           \
    num_focus++;                                         \
    Class::focusInEvent(e);                              \
  }                                                      \
  void focusOutEvent ( QFocusEvent *e ){                 \
    fprintf(stderr," lsot focus\n");                     \
    num_focus--;                                         \
    Class::focusOutEvent(e);                             \
  }                                                                     \
  bool eventFilter ( QObject * o, QEvent * ev ) {                       \
    if(ev->type()==QEvent::FocusIn)                                     \
      num_focus++;                                                      \
    if(ev->type()==QEvent::FocusOut)                                    \
      num_focus--;                                                      \
    if(ev->type()==QEvent::KeyPress && (static_cast<QKeyEvent*>(ev)->key()==Qt::Key_Return  || static_cast<QKeyEvent*>(ev)->key()==Qt::Key_Enter)) \
      set_editor_focus();                                               \
    return Class::eventFilter(o,ev);                                    \
  }                                                                     \
}                                                    

MakeFocusOverrideClass(QSpinBox);
MakeFocusOverrideClass(QLineEdit);

static void tab_selected();

class Instruments_widget;
static Instruments_widget *instruments_widget;

class No_instrument_widget;
static No_instrument_widget *no_instrument_widget;

//#define protected public
#include "Qt_instruments_widget.cpp"
#include "Qt_control_change_widget.cpp"
#include "Qt_instrument_widget.cpp"
#include "Qt_no_instrument_widget.cpp"


bool instrumentWidgetUsesKeyboard(void){
  return num_focus>0;
}

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

static const char *ccnames[128];

static Instrument_widget *createInstrumentWidget(const char *name, struct PatchData *patchdata){
    Instrument_widget *instrument = new Instrument_widget();

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
      if(patchdata!=NULL)
        for(int i=0;i<8;i++){
          ccnames[(int)patchdata->standardccs[i]] = patchdata->ccnames[i];
        }
    }

    {
      int ccnum = 0;
      for(int x=0;x<4;x++){
        for(int y=0;y<2;y++){
          Control_change_widget *cc = new Control_change_widget(instrument->control_change_group, "hepp");

          for(int i=0;i<128;i++){
            char temp[500];
            sprintf(temp, "%3d: %s", i, ccnames[i]==NULL?"":ccnames[i]);
            cc->cctype->insertItem(temp);
          }

          if(patchdata!=NULL){
            cc->patchdata = patchdata;

            cc->cctype->setCurrentItem(patchdata->standardccs[ccnum]);
            patchdata->cc[ccnum] = patchdata->standardccs[ccnum];
          }

          instrument->cc_widgets[ccnum] = cc;
          cc->ccnum = ccnum++;

          instrument->control_change_groupLayout->addWidget(cc,y,x);
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

    {
      int num_ports;
      char **portnames = MIDI_getPortNames(&num_ports);

      for(int i = 0; i<num_ports ; i++)
        instrument->port->insertItem(portnames[i]);
    }

    instrument->name_widget->setText(name);

    return instrument;
}


void addInstrument(struct Patch *patch){
  Instrument_widget *instrument = createInstrumentWidget(patch->name, (struct PatchData*)patch->patchdata);

  instrument->patch = patch;
  instrument->patchdata = (struct PatchData*)patch->patchdata;

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

  instrument->nameBox->setEnabled(true);
  instrument->panningBox->setEnabled(true);
  instrument->volumeBox->setEnabled(true);
  instrument->velocityBox->setEnabled(true);
  instrument->portBox->setEnabled(true);
  instrument->presetBox->setEnabled(true);
  instrument->channelBox->setEnabled(true);
  instrument->MSBBox->setEnabled(true);
  instrument->LSBBox->setEnabled(true);
  instrument->control_change_group->setEnabled(true);

  instruments_widget->tabs->insertTab(instrument, QString::fromLatin1(patch->name), instruments_widget->tabs->count()-1);
  instruments_widget->tabs->showPage(instrument);
}


QWidget *createInstrumentsWidget(void){
  instruments_widget = new Instruments_widget();

  {
    const char *name = "<No Instrument>";

    no_instrument_widget = new No_instrument_widget();

    delete instruments_widget->tabs->page(0);
    instruments_widget->tabs->insertTab(no_instrument_widget, QString::fromLatin1(name), 0);
    instruments_widget->tabs->showPage(no_instrument_widget);
  }

  {
    QPalette pal = QPalette(instruments_widget->palette());
    pal.setColor(QPalette::Active, QColorGroup::Base, QColor(0xd0, 0xd5, 0xd0));
    pal.setColor(QPalette::Inactive, QColorGroup::Base, QColor(0xd0, 0xd5, 0xd0));
    instruments_widget->setPalette(pal);
  }

  return instruments_widget;
}

void GFX_InstrumentWindowToFront(void){
}


// These functions (MIDIGFX_*) seems to be only used by the GTK1 instrument window when the wrong value
// was received from the GUI, and the original value must be sent back.
void MIDIGFX_UpdateAll(void){}
void MIDIGFX_SetPanSlider(bool on,int value){}
void MIDIGFX_SetVolumeSlider(bool on,int value){}
void MIDIGFX_SetLSB(int lsb){}
void MIDIGFX_SetMSB(int msb){}
void MIDIGFX_SetChannel(int ch){}
void MIDIGFX_SetCCSlider(int slidernum,bool on,int value){}


static Instrument_widget *get_instrument_widget(struct Patch *patch){
  QTabWidget* tabs = instruments_widget->tabs;

  for(int i=0;i<tabs->count()-1;i++){
    Instrument_widget *instrument = static_cast<Instrument_widget*>(tabs->page(i));
    if(instrument->patch==patch)
      return instrument;
  }

  return NULL;
}

static void update_instrument_widget(Instrument_widget *instrument, struct Patch *patch){
  struct PatchData *patchdata = (struct PatchData*)patch->patchdata;

  instrument->volume_spin->setValue(patchdata->volume);
  instrument->panning_spin->setValue(patchdata->pan);
  instrument->channel->setValue(patchdata->channel);
  instrument->msb->setValue(patchdata->MSB);
  instrument->lsb->setValue(patchdata->LSB);

  instrument->volume_onoff->setChecked(patchdata->volumeonoff);
  instrument->panning_onoff->setChecked(patchdata->panonoff);

  instrument->preset->setCurrentItem(patchdata->preset+1);

  for(int ccnum=0;ccnum<8;ccnum++){
    Control_change_widget *cc = instrument->cc_widgets[ccnum];
    cc->value_spin->setValue(patchdata->ccvalues[ccnum]);
    cc->onoff->setChecked(patchdata->ccsonoff[ccnum]);
    cc->cctype->setCurrentItem(patchdata->cc[ccnum]);
  }
}

void MIDIGFX_PP_Update(struct Instruments *instrument_not_used,struct Patch *patch){
  printf("PP update. Instrument name: \"%s\"\n",patch==NULL?"(null)":patch->name);
  if(g_currpatch==patch)
    return;

  if(patch==NULL){

    instruments_widget->tabs->showPage(no_instrument_widget);

  }else{

    Instrument_widget *instrument = get_instrument_widget(patch);
    if(instrument==NULL){
      addInstrument(patch);
      instrument = get_instrument_widget(patch);
    }

    update_instrument_widget(instrument,patch);

    instruments_widget->tabs->showPage(instrument);
  }

  g_currpatch = patch;
}

static void tab_selected(){
  //printf("tab selected -%s-\n",tabname.ascii());
  if(instruments_widget->tabs->currentPage()==no_instrument_widget)
    return;

  Instrument_widget *instrument = static_cast<Instrument_widget*>(instruments_widget->tabs->currentPage());
  g_currpatch = instrument->patch;

  {
    struct Tracker_Windows *window = root->song->tracker_windows;
    struct WBlocks *wblock = window->wblock;
    struct WTracks *wtrack = wblock->wtrack;

    DO_GFX(
           wtrack->track->patch = instrument->patch;
           DrawWTrackHeader(window,wblock,wtrack);
           );

    MyWidget *my_widget = static_cast<MyWidget*>(window->os_visual.widget);
    my_widget->update();
  }
}

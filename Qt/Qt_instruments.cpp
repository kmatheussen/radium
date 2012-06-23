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
#include "../midi/midi_i_plugin.h"
#include "../midi/midi_i_plugin_proc.h"
#include "../midi/OS_midigfx_proc.h"
#include "../midi/OS_midi_proc.h"
#include "MyWidget.h"


static struct Patch dummy_patch;
extern struct Root *root;
extern struct Patch *g_currpatch;
static struct PatchData *patchdata;

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

class Instruments_widget;
Instruments_widget *instruments_widget;

class Instrument_widget;
Instrument_widget *current_instrument;

//#define protected public
#include "Qt_instruments_widget.cpp"
#include "Qt_instrument_widget.cpp"
#include "Qt_control_change_widget.cpp"



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

static Instrument_widget *createInstrumentWidget(const char *name){
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

          cc->cctype->setCurrentItem(patchdata->standardccs[ccnum]);
          patchdata->cc[ccnum] = patchdata->standardccs[ccnum];

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


QWidget *createInstrumentsWidget(void){
  MIDI_InitPatch(&dummy_patch);
  g_currpatch = &dummy_patch;
  patchdata = (struct PatchData*)dummy_patch.patchdata;

  instruments_widget = new Instruments_widget();

  {
    Instrument_widget *instrument = createInstrumentWidget("Test instrument");
    current_instrument = instrument;

    instruments_widget->tabs->insertTab(instrument, QString::fromLatin1("Test instrument"), 0);
    instruments_widget->tabs->showPage(instrument);
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

void MIDIGFX_UpdateAll(void){}
void MIDIGFX_SetPanSlider(bool on,int value){}
void MIDIGFX_SetVolumeSlider(bool on,int value){}
void MIDIGFX_SetLSB(int lsb){}
void MIDIGFX_SetMSB(int msb){}
void MIDIGFX_SetChannel(int ch){}
void MIDIGFX_SetCCSlider(int slidernum,bool on,int value){}

void MIDIGFX_PP_Update(struct Instruments *instrument,struct Patch *patch){
  printf("PP update. Instrument name: \"%s\"\n",patch==NULL?"(null)":patch->name);
}

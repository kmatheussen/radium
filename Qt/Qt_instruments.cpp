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

#include "../common/nsmtracker.h"
#include "../common/visual_proc.h"
#include "../midi/midi_i_plugin.h"
#include "../midi/OS_midigfx_proc.h"
#include "../midi/OS_midi_proc.h"
#include "MyWidget.h"

extern struct Root *root;
extern struct Patch *g_currpatch;
static struct PatchData dummy_patchdata;
static struct PatchData *patchdata = &dummy_patchdata;

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



bool instrumentWindowUsesKeyboard(){
  return num_focus>0;
}

QWidget *createInstrumentsWidget(void){
  instruments_widget = new Instruments_widget();

  {
    Instrument_widget *instrument = new Instrument_widget();
    int ccnum = 0;
    for(int x=0;x<4;x++){
      for(int y=0;y<2;y++){
        Control_change_widget *cc = new Control_change_widget(instrument->control_change_group, "hepp");
        cc->ccnum = ccnum++;
        instrument->control_change_groupLayout->addWidget(cc,y,x);
      }
    }

    {
      int num_ports;
      char **portnames = MIDI_getPortNames(&num_ports);

      for(int i = 0; i<num_ports ; i++)
        instrument->port->insertItem(portnames[i]);
    }

    instrument->name_widget->setText("Test instrument");

    current_instrument = instrument;

    instruments_widget->tabs->insertTab(instrument, QString::fromLatin1("Test instrument"), 0);
    instruments_widget->tabs->showPage(instrument);
  }

  QPalette pal = QPalette(instruments_widget->palette());
  pal.setColor(QPalette::Active, QColorGroup::Base, QColor(0xd0, 0xd5, 0xd0));
  pal.setColor(QPalette::Inactive, QColorGroup::Base, QColor(0xd0, 0xd5, 0xd0));
  instruments_widget->setPalette(pal);

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
}

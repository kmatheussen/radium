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

#include "qstring.h"

#include "../common/nsmtracker.h"
#include "../common/visual_proc.h"
#include "../midi/midi_i_plugin.h"
#include "../midi/OS_midigfx_proc.h"
#include "../midi/OS_midi_proc.h"

extern struct Patch *g_currpatch;
static struct PatchData dummy_patchdata;
static struct PatchData *patchdata = &dummy_patchdata;

#define protected public // Stupid uic program
#  include "Qt_instruments_widget.cpp"
#  include "Qt_instrument_widget.cpp"
#  include "Qt_control_change_widget.cpp"
#undef protected

Instruments_widget *instruments_widget;


#if 0
static void name_widget_textChanged( const QString &string);

static void name_widget_textChanged( const QString &string ){
  //QTabBar *tab_bar = instruments_widget->tabs->tabBar();
  //  tab_bar->tab(tab_bar->currentTabl())->setText(string);
}
#endif


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

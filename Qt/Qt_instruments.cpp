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


#define protected public // Stupid qtdesigner
#  include "Qt_instruments_widget.cpp"
#  include "Qt_instrument_widget.cpp"
#  include "Qt_control_change_widget.cpp"
#undef protected

#include "../common/nsmtracker.h"
#include "../common/visual_proc.h"


QWidget *createInstrumentsWidget(void){
  Instruments_widget *instruments = new Instruments_widget();

  Instrument_widget *instrument = new Instrument_widget();
  for(int x=0;x<4;x++){
    for(int y=0;y<2;y++){
      Control_change_widget *cc = new Control_change_widget(instrument->control_change_group, "hepp");
      instrument->control_change_groupLayout->addWidget(cc,y,x);
    }
  }


  instruments->tabs->insertTab(instrument, QString::fromLatin1("Test instruments"), 0);
  instruments->tabs->showPage(instrument);

  QPalette pal = QPalette(instruments->palette());
  pal.setColor(QPalette::Active, QColorGroup::Base, QColor(0xd0, 0xd5, 0xd0));
  pal.setColor(QPalette::Inactive, QColorGroup::Base, QColor(0xd0, 0xd5, 0xd0));
  instruments->setPalette(pal);

  return instruments;
}

void GFX_InstrumentWindowToFront(void){
}

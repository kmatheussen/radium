/* Copyright 2000 Kjetil S. Matheussen

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










#include "nsmtracker.h"
#include "../common/disk.h"
#include "midi_instrument.h"

#include "OS_midi_proc.h"

#include "disk_midi_mymidilinks_proc.h"

extern struct MyMidiLinks *usedmidilinks;

void SaveMyMidiLinks(void){
	struct MyMidiLinks *mymidilink=usedmidilinks;
	struct ChannelSpecific *sc;
	int lokke,lokke2;
DC_start("MYMIDILINKS");

	while(mymidilink!=NULL){
		DC_SaveS(mymidilink->name);
		for(lokke=0;lokke<8;lokke++){
			DC_SaveS(mymidilink->ccnames[lokke]);
			DC_SaveI(mymidilink->standardccs[lokke]);
		}
		for(lokke=0;lokke<16;lokke++){
			sc=&mymidilink->channelspecific[lokke];
			DC_SaveB(sc->volumeonoff);
			DC_SaveB(sc->panonoff);
			DC_SaveI(sc->volume);
			DC_SaveI(sc->pan);

			for(lokke2=0;lokke2<8;lokke2++){
				DC_SaveB(sc->ccsonoff[lokke2]);
				DC_SaveI(sc->ccvalues[lokke2]);
			}
		}
		mymidilink=mymidilink->next;
	}
DC_end();
}


void LoadMyMidiLinks(void){
	struct MyMidiLinks *mymidilink;
	struct ChannelSpecific *sc;
	int lokke,lokke2;

	while(dc.success){
		DC_fgets();
		if(dc.ret[0]=='/'){
			return;
		}
		mymidilink=MIDI_getMyMidiLink(NULL,NULL,dc.ret+1);
		for(lokke=0;lokke<8;lokke++){
			mymidilink->ccnames[lokke]=DC_LoadS()+1;
			mymidilink->standardccs[lokke]=DC_LoadI();
		}
		for(lokke=0;lokke<16;lokke++){
			sc=&mymidilink->channelspecific[lokke];
			sc->volumeonoff=DC_LoadB();
			sc->panonoff=DC_LoadB();
			sc->volume=DC_LoadI();
			sc->pan=DC_LoadI();

			for(lokke2=0;lokke2<8;lokke2++){
				sc->ccsonoff[lokke2]=DC_LoadB();
				sc->ccvalues[lokke2]=DC_LoadI();
			}
		}
	}

}


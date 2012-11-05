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
#include "disk.h"
#include "disk_windows_proc.h"
#include "disk_block_proc.h"
#include "disk_playlist_proc.h"
#include "disk_instrument_proc.h"
#include "OS_Bs_edit_proc.h"
#include "instruments_proc.h"
#include "vector_proc.h"
#include "hashmap_proc.h"
#include "fxlines_proc.h"

#include "../mixergui/QM_MixerWidget.h"
#include "../Qt/Qt_instruments_proc.h"
#include "../audio/audio_instrument_proc.h"

#include "disk_song_proc.h"


void SaveSong(struct Song *song){
DC_start("SONG");

	DC_SSN("num_blocks",song->num_blocks);
	DC_SSI("length",song->length);
	DC_SSS("songname",song->songname);
	DC_SSN("maxtracks",song->maxtracks);

	SaveWindow(song->tracker_windows);
	SaveBlock(song->blocks);
	SavePlayList(song->playlist,song->length);

	SaveInstrument(get_MIDI_instrument());
	SaveInstrument(get_audio_instrument()); //TODO.

        // Patchdata for audio patches are saved here, not in disk_patches.
        DC_start("MIXERWIDGET");{
          HASH_save(MW_get_state(),dc.file);
          //HASH_save(create_instrument_widget_order_state(),dc.file);
        }DC_end();

DC_end();
}

struct Song *LoadSong(void){
	static char *objs[5]={
		"TRACKER_WINDOW",
		"BLOCK",
		"PLAYLIST",
		"INSTRUMENT",
                "MIXERWIDGET"
	};
	static char *vars[4]={
		"num_blocks",
		"length",
		"songname",
		"maxtracks",
	};
	struct Song *song=DC_alloc(sizeof(struct Song));

        VECTOR_clean(&get_MIDI_instrument()->patches);
        VECTOR_clean(&get_audio_instrument()->patches);

	GENERAL_LOAD(5,4)

obj0:
	DC_ListAdd1(&song->tracker_windows,LoadWindow());
	goto start;
obj1:
	DC_ListAdd1(&song->blocks,LoadBlock());
	goto start;
obj2:
	LoadPlayList();
	goto start;
obj3:
        LoadInstrument();
	goto start;

obj4:
        song->mixerwidget_state = HASH_load(dc.file);
        //song->instrument_widget_order_state = HASH_load(dc.file);
        goto start;
        
var0:
	song->num_blocks=DC_LoadI();
	goto start;
var1:
	song->length=DC_LoadI();
	goto start;
var2:
	song->songname=DC_LoadS();
	goto start;
var3:
	song->maxtracks=DC_LoadN();
	goto start;

var4:
var5:
var6:
var7:
var8:
var9:
var10:
var11:
var12:
var13:
var14:
var15:
var16:
var17:
var18:

obj5:
obj6:

error:
end:

	return song;
}

void DLoadSong(struct Root *newroot,struct Song *song){

        DLoadInstrument(get_MIDI_instrument());
        DLoadInstrument(get_audio_instrument());

	DLoadBlocks(newroot,song->blocks);
	DLoadPlayList(newroot,song);

	DLoadWindows(newroot,song->tracker_windows);

        if(song->mixerwidget_state!=NULL)
          MW_create_from_state(song->mixerwidget_state); // In addition, all audio plugins are created here and put into the patch->patchdata field.
        else
          MW_create_plain(); // older song.

        song->mixerwidget_state=NULL; // release memory.

        DLoadAudioInstrument(); // Sets correct effect_num for fx, since mapping between fx name and effect_num was not available when loading fx. (The MIDI instrument doesn't map between name and number since the MIDI standard is not going to change, and therefore it's safe to use the numbers directly.)

        DLoadInstrumentGUI(get_MIDI_instrument());
        DLoadInstrumentGUI(get_audio_instrument());

        //if(song->instrument_widget_order_state!=NULL)
        //  recreate_instrument_widget_order_from_state(song->instrument_widget_order_state);
        song->instrument_widget_order_state=NULL; // release memory

        BS_UpdateBlockList();
        BS_UpdatePlayList();

        FX_update_all_slider_automation_visuals();
}

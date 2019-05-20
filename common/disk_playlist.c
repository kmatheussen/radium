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
#include "sequencer_proc.h"

#include "disk_playlist_proc.h"


struct PlayListHolder{
	struct ListHeader1 l;
	NInt listnum;
};
#define NextPlayListHolder(a) ((struct PlayListHolder *)((a)->l.next))



void SavePlayList(struct Blocks **playlist,int length){
	int lokke;

DC_start("PLAYLIST");
	DC_SaveI(length);

	for(lokke=0;lokke<length;lokke++){
		DC_SaveN(playlist[lokke]->l.num);
	}

DC_end();
}


void LoadPlayList(void){
	struct PlayListHolder *playlist;
	int lokke=0;

	dc.num_playlists=DC_LoadI();

	while(dc.success){
		DC_fgets();
		if(dc.ret[0]=='/') return;
		playlist=DC_alloc(sizeof(struct PlayListHolder));
		playlist->l.num=lokke;
		playlist->listnum=atoi(dc.ret);
		DC_ListAdd1(&dc.playlist,playlist);
		lokke++;
	}

error:
	return;
}


void DLoadPlayList(struct Root *newroot,struct Song *song){
	struct PlayListHolder *playlist=dc.playlist;
	int num_playlists=dc.num_playlists;
	int lokke;

        int *to_paste = talloc_atomic(sizeof(int) * num_playlists);

	for(lokke=0;lokke<num_playlists;lokke++){
          to_paste[lokke]=playlist->listnum;

          playlist=NextPlayListHolder(playlist);
	}

        struct SeqTrack *new_seqtrack = SEQTRACK_create_from_playlist(to_paste, num_playlists);

        while(root->song->seqtracks.num_elements > 1)
          SEQUENCER_delete_seqtrack(0);

        SEQUENCER_replace_seqtrack(new_seqtrack, 0);

        //BL_paste2(song, to_paste);
        
	dc.playlist=NULL;
}







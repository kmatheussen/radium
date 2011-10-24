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
#include "list_proc.h"
#include "placement_proc.h"

#include "clipboard_localzooms_proc.h"



void CB_CopyLocalZoomsRec(
	struct LocalZooms **tolocalzoom,
	struct LocalZooms *localzoom
){
	struct LocalZooms *new;
	if(localzoom==NULL) return;

	new=talloc(sizeof(struct LocalZooms));
	PlaceCopy(&new->l.p,&localzoom->l.p);
	new->zoomline=localzoom->zoomline;
	new->level=localzoom->level;

	if(localzoom->uplevel!=NULL){
		CB_CopyLocalZoomsRec(&new->uplevel,localzoom->uplevel);
	}

	ListAddElement3(tolocalzoom,&new->l);

	CB_CopyLocalZoomsRec(tolocalzoom,NextLocalZoom(localzoom));
}

struct LocalZooms *CB_CopyLocalZooms(
	struct WBlocks *wblock
){
	struct LocalZooms *tolocalzoom=NULL;
	struct LocalZooms *localzoom=wblock->localzooms;

	if(localzoom->level>0){			//Might be that we are trying to pack an allready packed localzoom-tree.
		CB_CopyLocalZoomsRec(&tolocalzoom,localzoom);
		return tolocalzoom;
	}

	while(localzoom!=NULL){
		if(localzoom->uplevel!=NULL){
			CB_CopyLocalZoomsRec(&tolocalzoom,localzoom->uplevel);
		}
		localzoom=NextLocalZoom(localzoom);
	}

	return tolocalzoom;

}




/********************************************************
  FUNCTION
    Localzooms take up quite a bit of space, and most of
    it are actually a bit unnecesarry. Therefore, we
    don't save everything to the clipboard. (not to disk
    either, infact). But it can unpack (ie. copy) a
    normal localzoom tree too, which is practical
    for undo/redo.
********************************************************/

void CB_UnpackLocalZooms(
	struct LocalZooms **tolocalzoom,
	struct LocalZooms *localzoom,
	int num_lines
){
	struct LocalZooms *new;
	struct LocalZooms *uplevel;
	int lokke;

	if(localzoom!=NULL && localzoom->level==0){
		CB_CopyLocalZoomsRec(tolocalzoom,localzoom);
		return;
	}

	for(lokke=0;lokke<num_lines;lokke++){
		new=talloc(sizeof(struct LocalZooms));
		new->Tline=lokke;
		new->Tdividor=1;
		new->zoomline=lokke;
		ListAddElement3(tolocalzoom,&new->l);

		while(localzoom!=NULL && localzoom->Tline==lokke){
			uplevel=talloc(sizeof(struct LocalZooms));
			PlaceCopy(&uplevel->l.p,&localzoom->l.p);
			uplevel->level=1;
			uplevel->zoomline=localzoom->zoomline;
			ListAddElement3(&new->uplevel,&uplevel->l);
			if(localzoom->uplevel!=NULL){
				CB_CopyLocalZoomsRec(&uplevel->uplevel,localzoom->uplevel);
			}
			localzoom=NextLocalZoom(localzoom);
		}
	}

}



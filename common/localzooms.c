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

#include "localzooms_proc.h"




void NewLocalZoom(
	struct LocalZooms **tolocalzoom,
	int line,
	uint_32 counter,
	uint_32 dividor,
	int zoomline,
	int level,
	int realline
){
	struct LocalZooms *temp;
	temp=talloc(sizeof(struct LocalZooms));

	temp->Tline=line;
	temp->Tcounter=counter,
	temp->Tdividor=dividor;

	temp->zoomline=zoomline;
	temp->level=level;
	temp->realline=realline;

	ListAddElement3(tolocalzoom,&temp->l);
}


/* When making a new block. */
void NewLocalZooms(struct Tracker_Windows *window,struct WBlocks *wblock){
	int lokke;
	for(lokke=0;lokke < wblock->block->num_lines;lokke++){
		NewLocalZoom(
			&wblock->localzooms,
			lokke,
			0,
			1,
			lokke,
			0,
			lokke
		);
	}
}


/*******************************************************************
  FUNCTION
    Returns the LocalZoom Element that is placed on the next
    realline. Or NULL if there is no more reallines.
*******************************************************************/
struct LocalZooms *GetNextRealLocalZoom(
	struct WBlocks *wblock,
	struct LocalZooms *localzoom
){
	if(localzoom->realline<wblock->num_reallines-1){
		return wblock->reallines[localzoom->realline+1];
	}
	return NULL;

/*
	if(localzoom->uplevel!=NULL){
		return localzoom->uplevel;
	}else{
		if(localzoom->next!=NULL){
			return localzoom->next;
		}else{
			return NULL;
		}
	}
*/
}




/*******************************************************************
  FUNCTION
    Finds out if line and counter/dividor is placed on the realline
    definition structure 'localzoom'. Returns 0 if it is, -1 if
    it is before, and 1 if it is after.
  NOTES
    Obsolete, I think.. placement.c has a better function for this..
*******************************************************************/
int IsOnRealLine(
	struct WBlocks *wblock,
	struct LocalZooms *localzoom,
	Place *placement
){
	struct LocalZooms *localzoom2;
	uint_32 counter1;
	uint_32 dividor1;
	uint_32 counter2;
	uint_32 dividor2;
	int line=placement->line;
	uint_32 counter=placement->counter;
	uint_32 dividor=placement->dividor;

	if(line<localzoom->Tline) return -1;
	if(line>localzoom->Tline) return 1;

	localzoom2=GetNextRealLocalZoom(wblock,localzoom);
	if(localzoom2==NULL) return 0;
	if(localzoom2->Tline>line) return 0;

	counter1=localzoom->Tcounter;
	dividor1=localzoom->Tdividor;
	counter2=localzoom2->Tcounter;
	dividor2=localzoom2->Tdividor;

	if(counter*dividor2>=counter2*dividor) return -1;
	if(counter*dividor1<counter1*dividor) return 1;
	return 0;

}









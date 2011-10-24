/* Copyright 2003 Kjetil S. Matheussen

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



/*******************************************************
  This function does the same as
  AllocListRequest/SelectCluster from midilist.lib,
  but uses asl.library in a different way. This is
  much better, if asl.library is patched to use
  reqtools.library.
*******************************************************/



#include "nsmtracker.h"
#include "../common/visual_proc.h"
#include "../common/list_proc.h"
#include <string.h>


//extern struct Library *CamdBase;

extern struct Root *root;

struct ClusterNames{
	struct ListHeader1 l;
	char *name;
};

char *MIDI_getClusterName(ReqType reqtype){
	APTR lock;
	char **menu;
	char *ret;
	struct MidiCluster *cluster=NULL;
	struct ClusterNames *clustername;
	struct ClusterNames *clusternames=NULL;
	int selection;
	int lokke;
	int num_clusters=0;
	struct Tracker_Windows *window=root->song->tracker_windows;

	lock=LockMIDI(CD_Linkages);

	for(;;){
		cluster=NextCluster(cluster);
		if(cluster!=NULL){
			clustername=talloc(sizeof(*clustername));
			clustername->name=cluster->mcl_Node.ln_Name;
			ListAddElement1_ff(&clusternames,&clustername->l);
			num_clusters++;
		}else{
			break;
		}
	}
	
	if(clusternames==NULL){
		UnlockMIDI(lock);
		return NULL;
	}

	menu=talloc(sizeof(char *)*(num_clusters+1));

	for(lokke=0;lokke<num_clusters;lokke++){
		menu[lokke]=clusternames->name;
		clusternames=(struct ClusterNames *)clusternames->l.next;
	}
	menu[num_clusters]="New MidiLink";

	selection=GFX_Menu(window,reqtype,"Select Output Port",num_clusters+1,menu);
	if(selection==-1){
		ret=NULL;
	}else{
		if(selection==num_clusters){
			ret=GFX_GetString(window,reqtype,"Name of the new cluster: ");
		}else{
			ret=talloc_atomic(strlen(menu[selection])+10);
			sprintf(ret,"%s",menu[selection]);
		}
	}

	UnlockMIDI(lock);

	return ret;
}





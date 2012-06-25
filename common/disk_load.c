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



#include <gc.h>

#include "nsmtracker.h"
#include "disk.h"

#include "disk_load_proc.h"

#include "disk_root_proc.h"
#include "disk_save_proc.h"
#include "trackreallineelements_proc.h"
#include "visual_proc.h"
#include "wblocks_proc.h"
#include "windows_proc.h"

#ifdef _AMIGA
#include "Amiga_bs.h"
#include "instrprop/Amiga_instrprop.h"
#include "plug-ins/disk_camd_mymidilinks_proc.h"
#endif

#include "control_proc.h"
#include "OS_disk_proc.h"
#include "undo.h"
#include "player_proc.h"

#ifdef _AMIGa
#include "mmd2loader/mmd2load_proc.h"
#endif

#include "../config/config.h"

#include "disk_load_proc.h"


extern struct Root *root;

#ifdef _AMIGA
extern NInt currpatch;
#endif

struct Instruments *def_instrument;

bool Load(char *filename){
	float version;
	struct Root *newroot;

	dc.file=fopen(filename,"r");
	if(dc.file==NULL){
		RError("Could not open %s for loading\n",filename);
		return false;
	}

	dc.ls=talloc(BUFFERLENGTH+1);
	DC_fgets();

	if(strcmp("RADIUM SONG",dc.ls)){
		RError("First line in song was not 'RADIUM SONG', but '%s'\n",dc.ls);
		fclose(dc.file);
		return false;
	}

	version=DC_LoadF();

	if(version>0.4201 && version<0.50){
		version=0.51;
		dc.colorize=true;
		dc.startcolor=5;
	}else{
		dc.colorize=false;
	}

	if(version>DISKVERSION+0.0001){
		RError("You are trying to load a %f version, while this program is only %f.\n",version,DISKVERSION);
		return false;
	}else{
		printf("Song diskVersion: %f\n",version);
	}

	dc.filename=filename;

	dc.playlist=NULL;
	dc.success=true;

	DC_Next();
	if(strcmp(dc.ls,"OSSTUFF")){
		fprintf(stderr,"OSSTUFF not found, but: '%s'. File: '%s'\n",dc.ls,filename);
		fclose(dc.file);
		EndProgram();
		exit(4);
	}

	LoadOsStuff();

	printf("dc.ls: -%s-\n",dc.ls);
	if(strcmp(dc.ls,"ROOT")){
          fprintf(stderr,"ROOT not found. Found '%s' instead.\n", dc.ls);
          fclose(dc.file);
          EndProgram();
          exit(5);
	}

	newroot=LoadRoot();

	fclose(dc.file);

	if(!dc.success){
		printf("Loading failed.\n");
		EndProgram();
		exit(6);
	}


	ResetUndo();

	def_instrument=root->def_instrument;

#ifdef _AMIGA
	CloseHelpWindow();
	CloseCPPWindowWindow();
	CloseBlockSelectWindow();
#endif
	CloseAllTrackerWindows();

	root=newroot;		//BANG!

	DLoadRoot(newroot);



#ifdef _AMIGA
	currpatch=-1;
#endif

	return true;

}

extern int num_undos;
extern int max_num_undos;

#ifdef _AMIGA
extern char *mmp2filename;
#endif


bool Load_CurrPos(struct Tracker_Windows *window){
	bool ret = false;
	char *filename;
	char temp[200];

        // So many things happen here, that we should turn off garbage collection while loading.
        //
        // For instance, the instrument widget contains pointers (which are unreachable from the GC) to Patch
        // and PatchData objects. The instrument widget is updated after setting a new root, so it may access
        // that memory while a new song is loaded (since we don't have control over what Qt may do while we
        // access it). Not unlikely to be other similar situations.
        {
          GC_disable();
        }


	PlayStop();

	if(num_undos>0){
		char *retreq=NULL;

//		sprintf(temp,"%s%d changes has been made to file. Are you shure? (yes/no)",num_undos>=max_num_undos-1?"At least":"",num_undos);
		sprintf(
			temp,
			"%s%d change%s has been made to file.\nAre you shure? (yes/no) >"
			,num_undos>=max_num_undos-1?"At least":"",
			num_undos,
			num_undos>1?"s":""
		);
		while(
			retreq==NULL || (
				strcmp("yes",retreq) &&
				strcmp("no",retreq)
			)
		){
			retreq=GFX_GetString(
				window,
				NULL,
				temp
			);
		}
		if(!strcmp("no",retreq)) goto exit;
	}

	filename=GFX_GetLoadFileName(window,NULL,"Select file to load","obsolete");

	if(filename==NULL) goto exit;

	if(strlen(filename)>4){
		if(
			!strcmp(filename+strlen(filename)-5,".mmd2") ||
			!strcmp(filename+strlen(filename)-5,".mmd3") ||
			!strcmp(filename+strlen(filename)-4,".mmd")
		){
#ifdef _AMIGA
			mmp2filename=filename;
#endif
			ret = Load("radium:Init.rad");
                        goto exit;
		}
	}

        {
          ret = Load(filename);
          fprintf(stderr,"Got here (loading finished)\n");
        }

 exit:

        {
          GC_enable();
        }

        return ret;

}


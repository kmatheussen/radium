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
#include "visual_proc.h"
#include "wblocks_proc.h"
#include "windows_proc.h"
#include "OS_settings_proc.h"
#include "../mmd2loader/mmd2load_proc.h"

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

void COMMENTDIALOG_open(void);
bool COMMENT_show_after_loading(void);


extern struct Root *root;

#ifdef _AMIGA
extern NInt currpatch;
#endif

float disk_load_version;

static bool Load(const wchar_t *filename){
	struct Root *newroot;

        curr_disk_line = 0;

	dc.file=DISK_open_for_reading(filename);
	if(dc.file==NULL){
          GFX_Message(NULL,"Could not open \"%s\" for loading\n",STRING_get_chars(filename));
          return false;
	}

	DC_fgets();

	if(strcmp("RADIUM SONG",dc.ls)){
          GFX_Message(NULL,"First line in song was not 'RADIUM SONG', but '%s'. Last: %d\n",dc.ls,dc.ls[strlen(dc.ls)-1]);
          DISK_close_and_delete(dc.file);
          return false;
	}

	disk_load_version=DC_LoadF();

	if(disk_load_version>0.4201 && disk_load_version<0.50){
		disk_load_version=0.51;
		dc.colorize=true;
		dc.startcolor=5;
	}else{
		dc.colorize=false;
	}

        if(disk_load_version>DISKVERSION+0.0001){
          GFX_Message(NULL,"You are trying to load a %f version, while this program is only %f.\n",disk_load_version,DISKVERSION);
          return false;
        }else{
          printf("Song diskVersion: %f\n",disk_load_version);
        }

	dc.filename=filename;

	dc.playlist=NULL;
	dc.success=true;

	DC_Next();
	if(strcmp(dc.ls,"OSSTUFF")){
          RError("OSSTUFF not found, but: '%s'. File: '%s'\n",dc.ls,STRING_get_chars(filename));
          DISK_close_and_delete(dc.file);
          EndProgram();
          exit(4);
	}

	LoadOsStuff();

	printf("dc.ls: -%s-\n",dc.ls);
	if(strcmp(dc.ls,"ROOT")){
          RError("ROOT not found. Found '%s' instead.\n", dc.ls);
          DISK_close_and_delete(dc.file);
          EndProgram();
          exit(5);
	}

	newroot=LoadRoot();

        DISK_close_and_delete(dc.file);

	if(!dc.success){
          RError("Loading failed.\n");
          EndProgram();
          exit(6);
	}


	ResetUndo();

#ifdef _AMIGA
	CloseHelpWindow();
	CloseCPPWindowWindow();
	CloseBlockSelectWindow();
#endif
	CloseAllTrackerWindows();

	root=newroot;		//BANG!

	DLoadRoot(newroot);

        if(COMMENT_show_after_loading())
          COMMENTDIALOG_open();

#ifdef _AMIGA
	currpatch=-1;
#endif

        ResetUndo();
                
	return true;

}

//#ifdef _AMIGA
static const wchar_t *mmp2filename;
//#endif

static bool Load_CurrPos_org(struct Tracker_Windows *window, const wchar_t *filename){
	bool ret = false;

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

        if(Undo_are_you_shure_questionmark()==false)
          goto exit;

        if(filename==NULL)
          filename=GFX_GetLoadFileName(window,NULL,"Select file to load", NULL, NULL);

	if(filename==NULL) goto exit;

        if (STRING_ends_with(filename,".MMD2") ||
            STRING_ends_with(filename,".MMD3") ||
            STRING_ends_with(filename,".MMD") ||
            STRING_ends_with(filename,".mmd2") ||
            STRING_ends_with(filename,".mmd3") ||
            STRING_ends_with(filename,".mmd")
            )
          {

            mmp2filename=filename;
            ret = Load(STRING_create("new_song.rad"));

          } else {

            OS_set_loading_path(filename);
            {
              ret = Load(filename);
            }
            OS_unset_loading_path();
            
            GFX_SetWindowTitle(root->song->tracker_windows, filename);
            
            GFX_EditorWindowToFront(root->song->tracker_windows);
            
            struct WBlocks *wblock = root->song->tracker_windows->wblock;
            GFX_update_instrument_patch_gui(wblock->wtrack->track->patch);
            
            DrawUpTrackerWindow(root->song->tracker_windows);
            
            fprintf(stderr,"Got here (loading finished)\n");
        }

 exit:

        {
          GC_enable();
        }

	if(mmp2filename!=NULL) {
          LoadMMP2(root->song->tracker_windows, mmp2filename);
          mmp2filename=NULL;
        }

        ResetUndo();
        
        return ret;
}

bool Load_CurrPos(struct Tracker_Windows *window){
  return Load_CurrPos_org(window,NULL);
}

bool LoadSong_CurrPos(struct Tracker_Windows *window, const wchar_t *filename){
  return Load_CurrPos_org(window,filename);
}

void NewSong_CurrPos(struct Tracker_Windows *window){
  //char temp[4098];
  //sprintf(temp,"%s%s%s",OS_get_program_path(), OS_get_directory_separator(), "new_song.rad");
  //Load_CurrPos_org(window,talloc_strdup(temp));
  Load_CurrPos_org(window, STRING_create("new_song.rad"));
  GFX_SetWindowTitle(root->song->tracker_windows, STRING_create("Radium - New song."));
  dc.filename=NULL;
}


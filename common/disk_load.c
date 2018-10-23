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
#include "OS_Player_proc.h"
#include "control_proc.h"
#include "OS_disk_proc.h"
#include "undo.h"
#include "player_proc.h"
#include "patch_proc.h"

#include "../mmd2loader/mmd2load_proc.h"

#include "../OpenGL/Widget_proc.h"
#include "../OpenGL/Render_proc.h"

#include "../audio/Seqtrack_plugin_proc.h"
#include "../audio/SampleReader_proc.h"

#ifdef _AMIGA
#include "Amiga_bs.h"
#include "instrprop/Amiga_instrprop.h"
#include "plug-ins/disk_camd_mymidilinks_proc.h"
#endif

#include "../Qt/Qt_comment_dialog_proc.h"

#include "../config/config.h"

#include "disk_load_proc.h"


extern struct Root *root;

#ifdef _AMIGA
extern NInt currpatch;
#endif

float disk_load_version;
int g_disk_load_radium_version_major = -1;
int g_disk_load_radium_version_minor = -1;
int g_disk_load_radium_version_revision = -1;

bool g_is_loading = false;


bool Load_Initialize(const wchar_t *filename, const char *type){
  
        dc.success=true;
        
        dc.has_warned_about_acc_rit = false;
        
        g_curr_disk_line = 0;

	dc.file=DISK_open_for_reading(filename);
	if(dc.file==NULL){
          GFX_Message2(NULL,true,"Could not open \"%S\".\n\nMessage from the system: \"%s\".\n",filename, DISK_get_error(dc.file));
          goto failed;
	}

        dc.filename_with_full_path = DISK_get_absolute_file_path(filename);
        
	DC_fgets();
        if (dc.success==false) {
          GFX_Message2(NULL,true,"Loading failed. File too short. (1)\n");
          goto failed;
        }
        
	if(strcmp(type,dc.ls)){
          GFX_Message2(NULL,true,"First line in song was not '%s', but '%s'. Last: %d\n",type,dc.ls,dc.ls[strlen(dc.ls)-1]);
          goto failed;
	}

	disk_load_version=DC_LoadF();
        if (dc.success==false){
          GFX_Message2(NULL,true,"Loading failed. File too short. (2)\n");
          goto failed;
        }

	if(disk_load_version>0.4201 && disk_load_version<0.50){
		disk_load_version=0.51;
		dc.colorize=true;
		dc.startcolor=5;
	}else{
		dc.colorize=false;
	}

#if 0
        if (disk_load_version < 0.75){
          GFX_Message(NULL,
                      "<p>Note! The portamento behavior for polyphonic tracks changed in Radium V3.4.9"
                      "</p>"
                      "<p>"
                      "Before Radium V3.4.9, the default final portamento value was the pitch value of the next note after the <i>start</i> of the note. "
                      "Now, the default final portamento value is the pitch value of the next note after the <i>end</i> of the note."
                      "</p>"
                      "<p>"
                      "Old songs with portamento in polyphonic tracks might not sound the same."
                      "</p>"
                      );
        }
#endif

        if(disk_load_version>DISKVERSION+0.0001){
          GFX_Message2(NULL,true,"Need a newer version of Radium to load this file. The file version is %f, while this program only supports %f.\n",disk_load_version,DISKVERSION);
          goto failed;
        }else{
          printf("Song diskVersion: %f\n",disk_load_version);
        }

        if (dc.success==false){
          GFX_Message2(NULL, true, "Loading failed. File too short. (3)\n");
          return false;
        }
          
        if (disk_load_version < 0.975){
          
          g_disk_load_radium_version_major = -1;
          
        } else {

          hash_t *configuration = HASH_load(dc.file);

          if (configuration==NULL)
            goto failed;

          if (false==HASH_has_key(configuration, "version_major")
              || false==HASH_has_key(configuration, "version_minor")
              || false==HASH_has_key(configuration, "version_revision")
              )
            {
              GFX_Message2(NULL, true, "Something is wrong with the file. Could not find radium version information.");
              goto failed;
            }
          
          const char *error = DISK_get_error(dc.file);
          if (error != NULL){
            GFX_Message2(NULL, true, "Unable to read from file \"%S\".\n\nMessage from the system: \"%s\"", filename, error);
            goto failed;
          }

          g_disk_load_radium_version_major = HASH_get_int(configuration, "version_major");
          g_disk_load_radium_version_minor = HASH_get_int(configuration, "version_minor");
          g_disk_load_radium_version_revision = HASH_get_int(configuration, "version_revision");
          
          printf("Song radium version: %d.%d.%d\n", g_disk_load_radium_version_major, g_disk_load_radium_version_minor, g_disk_load_radium_version_revision);
        }
        
	dc.filename=filename;

        dc.embedded_files_dirname = DISK_get_absolute_file_path(STRING_append(filename,
                                                                              STRING_create("_embedded_samples")));
        dc.has_shown_embedded_files_dirname_warning = false;
        dc.has_deleted_files_in_embedded_dir = false;
        
	dc.playlist=NULL;

	DC_Next();
        if (dc.success==false){
          GFX_Message2(NULL,true,"Loading failed. File too short. (6)\n");
          return false;
        }
        
        return true;


 failed:        
        if (dc.file != NULL)
          DISK_close_and_delete(dc.file);
        
        dc.file = NULL;
        
        return false;
}
  
static bool Load(const wchar_t *filename){
	struct Root *newroot;
        
        if (Load_Initialize(filename, "RADIUM SONG")==false)
          return false;
        
	if(strcmp(dc.ls,"OSSTUFF")){
          GFX_Message2(NULL, true,"OSSTUFF not found, but: '%s'. File: '%S'\n",dc.ls,filename);
          DISK_close_and_delete(dc.file);
          //EndProgram();
          //exit(4);
          //goto exit;
          return false;
	}

        SEQTRACKPLUGIN_clear_resampler_warning_hashmap();

        g_curr_song_contains_embedded_samples = false; // Might be set to true during loading.
        
	LoadOsStuff();
        
	printf("dc.ls: -%s-\n",dc.ls);
	if(strcmp(dc.ls,"ROOT")){
          GFX_Message2(NULL, true, "ROOT not found. Found '%s' instead.\n", dc.ls);
          DISK_close_and_delete(dc.file);
          //EndProgram();
          //exit(5);
          return false;
	}

        GFX_OpenProgress("Loading song");
  
	newroot=LoadRoot();
        
        DISK_close_and_delete(dc.file);
        dc.file = NULL;
        
	if(!dc.success){
          GFX_Message(NULL, "Loading failed.\n");
          //EndProgram();
          //exit(6);
          return false;
	}
        
	ResetUndo();

        
#ifdef _AMIGA
	CloseHelpWindow();
	CloseCPPWindowWindow();
	CloseBlockSelectWindow();
#endif
	CloseAllTrackerWindows();

        GL_lock();{
          GL_pause_gl_thread_a_short_while();
        }GL_unlock();

        //GL_draw_lock();{
        PLAYER_lock();{ //<-- Locks within locks are dangerous. But it doesn't matter since the player isn't playing now anyway.
          root=newroot;		//BANG!
        }PLAYER_unlock();
        //}GL_draw_unlock();

        GFX_ShowProgressMessage("Preparing");
        
        Undo_start_ignoring_undo_operations();{
          GFX_DisablePainting();{
            DLoadRoot(newroot);
          }GFX_EnablePainting();
        }Undo_stop_ignoring_undo_operations();

        GFX_ShowProgressMessage("Loading all graphical data into memory");
        GL_create_all(root->song->tracker_windows);
        

        GFX_CloseProgress();
          
        if(COMMENT_show_after_loading())
          COMMENTDIALOG_open();

#ifdef _AMIGA
	currpatch=-1;
#endif

        ResetUndo();

        // Not necessary. The call to ResetUndo() above takes care of this automagically.
        //SAMPLEREADER_delete_all_deletable_audio_files();
        
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
        if(0){
          Threadsafe_GC_disable();
        }


	PlayStop();

        if(Undo_are_you_sure_questionmark()==false)
          goto exit;

        if(filename==NULL)
          filename=GFX_GetLoadFileName(window,NULL,"Select file to load", NULL, NULL, NULL, true);
            
	if(filename==NULL)
          goto exit;

        g_is_loading = true;

        
        if (STRING_ends_with(filename,".MMD2") ||
            STRING_ends_with(filename,".MMD3") ||
            STRING_ends_with(filename,".MMD") ||
            STRING_ends_with(filename,".mmd2") ||
            STRING_ends_with(filename,".mmd3") ||
            STRING_ends_with(filename,".mmd")
            )
          {

            mmp2filename=filename;
            ret = Load(STRING_create("sounds/new_song.rad"));

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

            root->song->tracker_windows->must_redraw = true;
            
            fprintf(stderr,"Got here (loading finished)\n");
        }

 exit:

        //fprintf(stderr,"Got here2 (loading finished)\n");
        
        if(0){
          Threadsafe_GC_enable();
        }

	if(mmp2filename!=NULL) {
          LoadMMP2(root->song->tracker_windows, mmp2filename);
          mmp2filename=NULL;
        }

        //fprintf(stderr,"Got here3 (loading finished)\n");
        
        if (ret)
          ResetUndo();

        g_is_loading = false;

        if (ret)
          PATCH_clean_unused_patches();

        if (!ret){
          const vector_t deletable_audiofiles = SAMPLEREADER_get_all_deletable_filenames();
          
          VECTOR_FOR_EACH(const wchar_t *filename, &deletable_audiofiles){      
            SAMPLEREADER_mark_what_to_do_with_deletable_file_when_loading_or_quitting(filename, WTT_DONT_KNOW);
          }END_VECTOR_FOR_EACH;
        }
        
        //fprintf(stderr,"Got here4 (loading finished)\n");
        
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

  Load_CurrPos_org(window, OS_get_full_program_file_path(STRING_create("sounds/new_song.rad")));

  GFX_SetWindowTitle(root->song->tracker_windows, STRING_create("Radium - New song."));
  
  dc.filename=NULL;
}


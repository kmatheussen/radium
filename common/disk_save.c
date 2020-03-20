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





#include <unistd.h>

#include "nsmtracker.h"
#include "disk.h"
#include "disk_root_proc.h"
#include "OS_disk_proc.h"
#include "disk_save_proc.h"
#include "visual_proc.h"
#include "player_proc.h"
#include "sequencer_proc.h"
#include "undo.h"
#include "nag.h"
#include "OS_settings_proc.h"
#include "settings_proc.h"

#include "../api/api_proc.h"

#include "../config/config.h"


bool Save_Initialize(const filepath_t filename, const char *type){
  	dc.success=true;

        dc.file=DISK_open_for_writing(filename);
	if(dc.file==NULL){
          GFX_Message2(NULL, true, "Could not save song file \"%S\".\n\nMessage from the system: \"%s\".", filename.id, DISK_get_error(NULL));
          return false;
	}

        OS_set_saving_path(filename);
        
	int length1=DISK_write(dc.file,type);
        int length2=DISK_write(dc.file,"\n");
	int length3=DISK_printf(dc.file,"%s\n\n", OS_get_string_from_double(DISKVERSION));
        
	if(length1<0 || length2<0 || length3<0){
          const char *error = DISK_get_error(dc.file);
          GFX_Message2(NULL, true, "Unable to write to file \"%S\".\n\nMessage from the system: \"%s\".", filename.id, error!=NULL ? error : "(no info)");
          DISK_close_and_delete(dc.file);
          return false;
	}

        hash_t *configuration = HASH_create(3);
        HASH_put_int(configuration, "version_major", radiumMajorVersion());
        HASH_put_int(configuration, "version_minor", radiumMinorVersion());
        HASH_put_int(configuration, "version_revision", radiumRevisionVersion());
        HASH_save(configuration, dc.file);
        
        const char *error = DISK_get_error(dc.file);
        if (error != NULL){
          GFX_Message2(NULL, true, "Unable to write to file \"%S\".\n\nMessage from the system: \"%s\"", filename.id, error);
          DISK_close_and_delete(dc.file);
          return false;
        }
        
        return true;
}

bool g_is_saving = false;

void Save_Clean(const filepath_t filename,struct Root *theroot, bool is_backup){

        if (g_user_interaction_enabled==false){
          dc.success = false;
          return;
        }
        
        if (Save_Initialize(filename, "RADIUM SONG")==false){
          dc.success = false;
          return;
        }

        g_is_saving = true;
        
	DC_start("OSSTUFF");
		SaveOsStuff();
	DC_end();

	SaveRoot(theroot);

	if( ! dc.success){
          GFX_Message2(NULL, true,"Problems writing to file \"%S\".\n", filename.id);
	}

        bool success=DISK_close_and_delete(dc.file);

        if (is_backup==false) {
          Undo_saved_song();

          SEQUENCER_make_all_used_audio_files_undeletable(); // I.e. don't auto-delete anymore recording takes that are in use. Wouldn't be good if loading the song later.
          
          if (success)
            show_nag_window("File successfully saved.<p>");
        }

        g_is_saving = false;

        return;
}

bool SaveAs2(const filepath_t filename, struct Root *theroot){
  if (doStopPlayingWhenSavingSong())
    PlayStop();

  if(isIllegalFilepath(filename)){
    GFX_addMessage("Could not save song. %S is an illegal filename", filename.id);
    return false;
  }

  SETTINGS_write_wchars("filerequester_song_path", DISK_get_absolute_dir_path(filename).id);
  
  dc.filename = copy_filepath(filename);
  
  Save_Clean(filename,theroot,false);
  
  if (dc.success)
    GFX_SetWindowTitle(theroot->song->tracker_windows,filename.id);

  return dc. success;
}


bool SaveAs(struct Root *theroot){

        if (doStopPlayingWhenSavingSong())
          PlayStop();

        if (g_user_interaction_enabled==false)
          return false;

        filepath_t wdir = make_filepath(SETTINGS_read_wchars("filerequester_song_path", NULL));
        const filepath_t filename = GFX_GetSaveFileName(theroot->song->tracker_windows, NULL, " Select file to save", wdir, "*.rad", NULL, true);

        if(isIllegalFilepath(filename))
          return false;

#ifndef GUIISQT // Qt asks this question for us.
	char *ret=NULL;
	if( ! access(filename,F_OK)){
		while(
			ret==NULL || (
				! strcmp("yes",ret) &&
				! strcmp("no",ret)
			)
		){
			ret=GFX_GetString(
				theroot->song->tracker_windows,
				NULL,
				"File already exists, are you sure? (yes/no)"
			);
		}
		if(!strcmp("no",ret)) return false;
	}
#endif

        return SaveAs2(filename, theroot);
}

bool g_embed_samples = false;

bool SaveWithEmbeddedSamples(filepath_t filename, struct Root *theroot){
  bool ret = false;
  
  g_embed_samples=true;
  
  if (isIllegalFilepath(filename))
    ret = SaveAs(theroot);
  else
    ret = SaveAs2(filename, theroot);
  
  g_embed_samples=false;
  g_curr_song_contains_embedded_samples = true;
  SONGPROPERTIES_update(theroot->song);

  return ret;
}

bool SaveWithoutEmbeddedSamples(filepath_t filename, struct Root *theroot){
  g_embed_samples=false;

  bool ret;
  
  if (isIllegalFilepath(filename))
    ret = SaveAs(theroot);
  else
    ret = SaveAs2(filename, theroot);
    
  g_curr_song_contains_embedded_samples = false;
  SONGPROPERTIES_update(theroot->song);

  return ret;
}

bool Save(struct Root *theroot){

  bool ret = false;
  
  if (doStopPlayingWhenSavingSong())
    PlayStop();
  
  if(isIllegalFilepath(dc.filename)){
    
    g_embed_samples = g_curr_song_contains_embedded_samples;

    ret = SaveAs(theroot);

    g_embed_samples=false;
    
  }else{
    
    bool embed = g_curr_song_contains_embedded_samples;
    if (embed)
      g_embed_samples=true;

    Save_Clean(dc.filename,theroot,false);
    ret = dc.success;
    
    if (embed)
      g_embed_samples=false;
    
  }

  return ret;
}

void Save_Backup(const filepath_t filename, struct Root *theroot){
  printf("not saving backup to %S\n", filename.id);

  const filepath_t filename_org = dc.filename;

  Save_Clean(filename, theroot, true);

  dc.filename = filename_org;
}

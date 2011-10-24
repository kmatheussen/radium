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
#include "../config/config.h"

void Save_Clean(char *filename,struct Root *theroot){
	int length1,length2;

	dc.success=true;

	dc.file=fopen(filename,"w");
	if(dc.file==NULL){
		RError("Could not open file for writing.\n");
		return;
	}

	length1=fprintf(dc.file,"RADIUM SONG\n");
	length2=fprintf(dc.file,"%f\n",DISKVERSION);

	if(length1<0 || length2<0){
		RError("Could not write to file.\n");
		fclose(dc.file);
		return;
	}

	DC_start("OSSTUFF");
		SaveOsStuff();
	DC_end();

	SaveRoot(theroot);

	if( ! dc.success){
		RError("Problems writing to file.\n");
	}

	if(fclose(dc.file)==EOF){
		RError("Could not close file. Out of disk-space?\n");
		RError("Saving failed.\n");
	}
}

void SaveAs(struct Root *theroot){
	char *filename;
	char *ret=NULL;

	PlayStop();

	filename=GFX_GetSaveFileName(theroot->song->tracker_windows,NULL,"Select file to save","obsolete");

	if(filename==NULL) return;

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
				"File allready exists, are you shure? (yes/no)"
			);
		}
		if(!strcmp("no",ret)) return;
	}

	dc.filename=filename;

	Save_Clean(filename,theroot);

}

void Save(struct Root *theroot){

	PlayStop();

	if(dc.filename==NULL){
		SaveAs(theroot);
	}else{
		Save_Clean(dc.filename,theroot);
	}
}


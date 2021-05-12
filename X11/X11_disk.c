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


#include "../common/nsmtracker.h"

#include "../common/disk.h"

#include "../common/OS_disk_proc.h"


void SaveOsStuff(void){
  DC_SaveCleanString("X11\n");
}

void LoadOsStuff(void){
  const char *osname=DC_LoadS();
  printf("osname: %s\n",osname);

  DC_SkipBlock();
  DC_fgets();
}

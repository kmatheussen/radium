/* Copyright 2012 Kjetil S. Matheussen

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

#include <sys/stat.h>
#include <sys/types.h>

#include "../common/nsmtracker.h"
#include "../common/OS_settings_proc.h"


char *OS_get_config_filename(){
  char temp[500];
  sprintf(temp,"mkdir %s/.radium 2>/dev/null",getenv("HOME"));
  system(temp);

  sprintf(temp,"%s/.radium/config",getenv("HOME"));
  return talloc_strdup(temp);
}


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

#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>

#include "nsmtracker.h"
#include "settings_proc.h"
#include "OS_settings_proc.h"

// todo: strip whitespace and so forth, so the file can be more easely edited manually.

static bool line_contains_key(char *key, char *string){
  if(key[0]==0)
    return true;
  if(string[0]==0)
    return false;
  if(key[0]==string[0])
    return line_contains_key(key+1,string+1);
  return false;
}

static int find_linenum(char *key, char **lines, int num_lines){
  int linenum;
  for(linenum=0;linenum<num_lines;linenum++){
    if(line_contains_key(key,lines[linenum]))
      return linenum;
    linenum++;
  }
  return -1;
}

static char** get_lines(int *linenums){
  char *filename = OS_get_config_filename();
  FILE *file = fopen(filename, "r");

  *linenums = 0;
  char **ret = talloc(10000 * sizeof(char*));

  if(file==NULL)
    return ret;

  char line[500];  
  while(fgets(line,499,file)!=NULL){
    ret[*linenums] = talloc_strdup(line);
    *linenums = *linenums + 1;
  }

  fclose(file);

  return ret;
}

static void write_lines(char **lines, int num_lines){
  char *filename = OS_get_config_filename();
  FILE *file = fopen(filename, "w");
  if(file==NULL){
    RError("Unable to write config data to \"%s\"",filename);
    return;
  }

  int i;
  for(i=0;i<num_lines;i++)
    fputs(lines[i],file);

  fclose(file);
}

static void SETTINGS_put(char *key, char *val){
  int num_lines;
  char **lines = get_lines(&num_lines);
  int linenum = find_linenum(key,lines,num_lines);

  if(linenum==-1){
    linenum = num_lines;
    num_lines++;
  }

  char temp[500];
  sprintf(temp,"%s=%s",key,val);
  lines[linenum] = talloc_strdup(temp);

  write_lines(lines, num_lines);
}

static char *SETTINGS_get(char *key){
  int num_lines;
  char **lines = get_lines(&num_lines);
  int linenum = find_linenum(key,lines,num_lines);

  if(linenum==-1)
    return NULL;

  char *line = lines[linenum];
  int pos=0;
  while(line[pos]!='=')
    pos++;

  return line+pos;
}

bool SETTINGS_get_bool(char *key, bool def){
  return SETTINGS_get_int(key, def==true?1:0) == 1 ? true : false;
}

int64_t SETTINGS_get_int(char *key, int64_t def){
  char *val = SETTINGS_get(key);

  if(val==NULL)
    return def;
  else
    return atoll(val);
}

double SETTINGS_get_double(char *key, double def){
  char *val = SETTINGS_get(key);

  if(val==NULL)
    return def;
  else
    return atof(val);
}

char *SETTINGS_get_string(char *key, char *def){
  char *val = SETTINGS_get(key);

  if(val==NULL)
    return def;
  else
    return val;
}

void SETTINGS_write_bool(char *key, bool val){
  SETTINGS_write_int(key, val==true?1:0);
}

void SETTINGS_write_int(char *key, int64_t val){
  char temp[500];
  sprintf(temp,"%" PRId64,val);
  SETTINGS_put(key,temp);
}

void SETTINGS_write_double(char *key, double val){
  char temp[500];
  sprintf(temp,"%f",val);
  SETTINGS_put(key,temp);
}

void SETTINGS_write_string(char *key, char *val){
  SETTINGS_put(key,val);
}

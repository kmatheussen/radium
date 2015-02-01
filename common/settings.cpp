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

#define __STDC_FORMAT_MACROS
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>

#include <QVector>
#include <QString>
#include <QFile>
#include <QDir>
#include <QTextStream>
#include <QCoreApplication>

#include "nsmtracker.h"
#include "OS_settings_proc.h"
#include "../config/config.h"

#include "settings_proc.h"

static QString custom_configuration_filename;

static QString not_found("___________NOT-FOUND");


static QString get_value(QString line){

  int pos = line.indexOf("=");

  if (pos==-1)
    return not_found;

  return line.remove(0,pos+1).trimmed();

}

static bool line_has_key(const char* key, const char* string){
  if(isblank(string[0])) // strip whitespace
    return line_has_key(key,string+1);

  if(string[0]=='=' && key[0]==0)
    return true;

  if(key[0]==0)
    return false;

  if(string[0]==0)
    return false;

  if(key[0]==string[0])
    return line_has_key(key+1,string+1);

  return false;
}

static int find_linenum(const char* key, QVector<QString> lines){
  for(int linenum = 0 ; linenum < lines.size() ; linenum++)
    if(line_has_key(key,lines[linenum].toUtf8().constData()))
      return linenum;
  return -1;
}
  
static QVector<QString> get_lines(const char* key){
  bool is_color_config = OS_config_key_is_color(key);

  QVector<QString> ret;

  //const char *filename = custom_configuration_filename==NULL ? OS_get_config_filename(key) : custom_configuration_filename;
  QString filename = custom_configuration_filename=="" ? OS_get_config_filename(key) : custom_configuration_filename;

  QFile file(filename);

  if(file.open(QIODevice::ReadOnly | QIODevice::Text)==false){
#if 0
    const char* curr_dir = OS_get_program_path();
    const char* separator = OS_get_directory_separator();
    const char* basefilename = is_color_config ? "colors" : "config";
    char filename[strlen(curr_dir)+strlen(separator)+strlen(basefilename)+10];
    sprintf(filename,"%s%s%s",curr_dir,separator,basefilename);
    file = fopen(filename,"r");
#endif
    QString bin_filename = QCoreApplication::applicationDirPath() + QDir::separator() + (is_color_config ? "colors" : "config");
    
    file.setFileName(bin_filename);
    if (file.open(QIODevice::ReadOnly | QIODevice::Text)==false){
      RError("Unable to open %s",bin_filename.toUtf8().constData());
      return ret;
    }
  }


  QTextStream in(&file);
  while ( !in.atEnd() ){
    QString line = in.readLine();    
    ret.push_back(line);
  }

  file.close();
  
  QString version_line(talloc_format("settings_version = %s # dont change this one",OS_get_string_from_double(SETTINGSVERSION)));

  int version_linenum = find_linenum("settings_version",ret);
  if(version_linenum==-1)
    ret.push_back(version_line);
  else
    ret[version_linenum] = version_line;

#if 0
  {
    int i=0;
    while(ret[i]!=NULL){
      printf("ret[%d]: -%s-\n",i,ret[i]);
      i++;
    }
    //getchar();
    //    char temp[1024];
    //fgets(temp,1000,stdin);
  }
#endif

  return ret;
}

static void write_lines(const char* key, QVector<QString> lines){
  QString filename = OS_get_config_filename(key);

  printf("config filename: -%s-\n",filename.toUtf8().constData());
  
  QFile file(filename);

  if (file.open(QIODevice::WriteOnly | QIODevice::Text)==false) {
    RError("Unable to write config data to \"%s\"",filename.toUtf8().constData());
    return;
  }

  QTextStream out(&file);
  out.setCodec("UTF-8"); 

  for (int i=0 ; i<lines.size(); i++){
    printf("writing -%s-\n",lines[i].toUtf8().constData());
    out << lines[i] << "\n";
  }

  file.close();

  //getchar();  
}

#if 0
static void append_line(const char** lines, const char* line){
  int i=0;
  while(lines[i]!=NULL)
    i++;
  lines[i] = line;
}
#endif

static void SETTINGS_put(const char* key, QString val){
  QVector<QString> lines = get_lines(key);
  if(lines.size()==0)
    return;
  
  int linenum = find_linenum(key,lines);

  QString temp = QString(key) + " = " + val;

  if(linenum==-1)
    lines.push_back(temp);
  else
    lines[linenum] = temp;
  
  write_lines(key, lines);
}

static QString SETTINGS_get(const char* key){
  QVector<QString> lines = get_lines(key);
  if(lines.size()==0)
    return not_found;

  int linenum = find_linenum(key,lines);

  if(linenum==-1)
    return not_found;

  return get_value(lines[linenum]);
}

static const char* SETTINGS_get_chars(const char* key){
  QString ret = SETTINGS_get(key);
  if (ret==not_found)
    return NULL;
  else
    return talloc_strdup(ret.toUtf8().constData());
}

void SETTINGS_set_custom_configfile(QString filename){
  custom_configuration_filename=filename;
}

void SETTINGS_unset_custom_configfile(void){
  custom_configuration_filename="";
}

bool SETTINGS_read_bool(const char* key, bool def){
  return SETTINGS_read_string(key, def==true?"true":"false")[0] == 't';
}

int64_t SETTINGS_read_int(const char* key, int64_t def){
  const char* val = SETTINGS_get_chars(key);

  if(val==NULL)
    return def;
  else
    return atoll(val);
}

double SETTINGS_read_double(const char* key, double def){
  const char* val = SETTINGS_get_chars(key);

  if(val==NULL)
    return def;
  else
    return OS_get_double_from_string(val);
}

const char* SETTINGS_read_string(const char* key, const char* def){
  const char* val = SETTINGS_get_chars(key);

  if(val==NULL)
    return def;
  else
    return val;
}

QString SETTINGS_read_qstring(const char* key, QString def){
  QString val = SETTINGS_get(key);

  if(val==not_found)
    return def;
  else
    return val;
}

void SETTINGS_write_bool(const char* key, bool val){
  SETTINGS_write_string(key, val==true?"true":"false");
}

void SETTINGS_write_int(const char* key, int64_t val){
  char temp[500];
  sprintf(temp, "%" PRId64 "", val);
  SETTINGS_put(key,temp);
}

void SETTINGS_write_double(const char* key, double val){
  char temp[500];
  sprintf(temp,"%s",OS_get_string_from_double(val));
  SETTINGS_put(key,temp);
}

void SETTINGS_write_string(const char* key, QString val){
  SETTINGS_put(key,val);
}

void SETTINGS_write_string(const char* key, const char* val){
  SETTINGS_put(key,val);
}

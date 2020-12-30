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
#include <ctype.h>
#include <string.h>
#include <unistd.h>

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wshorten-64-to-32"
#include <QVector> // Shortening warning in the QVector header. Temporarily turned off by the surrounding pragmas.
#pragma clang diagnostic pop

#include <QString>
#include <QFile>
#include <QDir>
#include <QTextStream>
#include <QCoreApplication>
#include <QTemporaryFile>

#include "nsmtracker.h"
#include "vector_proc.h"
#include "visual_proc.h"
#include "OS_settings_proc.h"
#include "threading.h"
#include "../config/config.h"
#include "../api/api_proc.h"

#include "settings_proc.h"


static QString custom_configuration_filename;

static QString not_found("___________NOT-FOUND");


static QString get_value(QString line){

  {
    int pos = line.indexOf(QString("#"));
    if (pos!=-1 && !line.contains("color"))
      line.truncate(pos);
  }

  int pos = line.indexOf("=");
  if (pos==-1)
    return not_found;

  return line.remove(0,pos+1).trimmed();

}

static bool line_has_key(QString key, QString das_string){
  QString string = das_string.trimmed();

  if (!string.startsWith(key))
    return false;

  string = string.remove(0, key.length()).trimmed();

  return string.startsWith("=");
}

static int find_linenum(QString key, QVector<QString> lines){
  for(int linenum = 0 ; linenum < lines.size() ; linenum++)
    if(line_has_key(key,lines[linenum]))
      return linenum;
  return -1;
}

static QString get_settings_version_line(void){
  return QString("settings_version = ") + OS_get_qstring_from_double(SETTINGSVERSION) + " # dont change this one";
}

static QVector<QString> get_lines2(QFile &file){
  QVector<QString> ret;
  
  QTextStream in(&file);
  in.setCodec("UTF-8");
  
  while ( !in.atEnd() ){
    QString line = in.readLine();
    
    if (!line.contains("#") && !line.contains("=")) // Make malformed lines empty
      line = "";
    
    else if (line.contains("#") && !line.contains("=")) // another possible malformed line fix: "a#b" -> "#b"
      line.remove(0, line.indexOf("#"));
    
    if (line.length()>512){ // Because of an old bug, lines could grow and grow. Just delete those lines.
      GFX_Message(NULL, "A very long line (%d characters) in the config file was ignored",line.length());
      line = "";
    }
    
    //printf("line: -%s-\n",line.toUtf8().constData());
    ret.push_back(line);
  }
  
  file.close();

  return ret;
}

static filepath_t get_program_configuration_filename(bool is_color_config){
  return OS_get_full_program_file_path(is_color_config ? "colors" : "config");
}

// Warning, called before GC_init, so it cannot allocate with talloc or talloc_atomic.
static QVector<QString> get_lines(const char* key){
  R_ASSERT(THREADING_is_main_thread());
  
  bool is_color_config = OS_config_key_is_color(key);

  QVector<QString> ret;

  //const char *filename = custom_configuration_filename==NULL ? OS_get_config_filename(key) : custom_configuration_filename;
  QString filename = custom_configuration_filename=="" ? STRING_get_qstring(OS_get_config_filename(key).id) : custom_configuration_filename;
  //printf("Filename: -%s-. custom: -%s-\n", filename.toUtf8().constData(), custom_configuration_filename.toUtf8().constData());
  
  QFile file(filename);

  bool load_default = false;

  if(!file.open(QIODevice::ReadOnly | QIODevice::Text)){
    
    load_default = true;
    
  } else {
  
    ret = get_lines2(file);
    
    if (ret.size()==0)
      load_default = true;
  }

  
  if(load_default) {
    
#if 0
    const char* curr_dir = OS_get_program_path();
    const char* separator = OS_get_directory_separator();
    const char* basefilename = is_color_config ? "colors" : "config";
    char filename[strlen(curr_dir)+strlen(separator)+strlen(basefilename)+10];
    sprintf(filename,"%s%s%s",curr_dir,separator,basefilename);
    file = fopen(filename,"r");
#endif
    
    QString bin_filename = STRING_get_qstring(get_program_configuration_filename(is_color_config).id);
    
    file.setFileName(bin_filename);
    if (file.open(QIODevice::ReadOnly | QIODevice::Text)==false){
      GFX_Message(NULL, "Unable to open %s. Make sure Radium is installed properly. Exiting program.",bin_filename.toUtf8().constData());
      exit(-1);
      abort();
    }

    ret = get_lines2(file);
  }


  /*
  QString version_line = get_settings_version_line(); //QString("settings_version = ") + OS_get_qstring_from_double(SETTINGSVERSION) + " # dont change this one";

  int version_linenum = find_linenum("settings_version",ret);
  if(version_linenum==-1)
    ret.push_back(version_line);
  else
    ret[version_linenum] = version_line;
  */
  
#if 0
  {
    int i=0;
    for(auto s : ret){
      printf("ret[%d]: -%s-\n",i,s.toUtf8().constData());
      i++;
    }
    //getchar();
    //    char temp[1024];
    //fgets(temp,1000,stdin);
  }
#endif

  return ret;
}

#if 0
// Warning, called before GC_init, so it cannot allocate with talloc or talloc_atomic.
static void transfer_temporary_file_to_file(QString from, QString to){
  
  if (QFile::exists(to)){
    QFile::remove(to+".bak");
    if (QFile::copy(to, to+".bak")==false){
      GFX_Message(NULL, "Unable to make backup of \%s\". Something is wrong with the file system. Will not try to update the configuration file.",to.toUtf8().constData());
      return;
    }
    QFile::remove(to);
  }
  
  if (QFile::copy(from, to)==false)
    GFX_Message(NULL, "Unable to write config file (\%s\")",to.toUtf8().constData());
}
#endif
// Warning, called before GC_init, so it cannot allocate with talloc or talloc_atomic.
static void write_lines(const char* key, QVector<QString> lines){
  R_ASSERT(THREADING_is_main_thread());

  R_ASSERT_NON_RELEASE(lines.size()>0);

  filepath_t filename = OS_get_config_filename(key);
  //QString filename = STRING_get_qstring();

  printf("config filename: -%S-\n",filename.id);

  disk_t *file = DISK_open_for_writing(filename);
  
  if (file==NULL) {
    GFX_Message(NULL, "Unable to write config data to temporary file. Disk full? (\%S\")",filename.id);
    return;
  }
    
/*
  QFile file(filename);

  if (file.open(QIODevice::WriteOnly | QIODevice::Text)==false) {
    GFX_Message(NULL, "Unable to write config data to \"%s\"",filename.toUtf8().constData());
    return;
  }
*/

  /*
  QTextStream out(&temporary_write_file);
  out.setCodec("UTF-8"); 
  */
  int version_linenum = find_linenum("settings_version",lines);
  if (version_linenum == -1)
    DISK_write_qstring(file, get_settings_version_line() + "\n");
  
    //out << get_settings_version_line() << "\n";
  
  for (int i=0 ; i<lines.size(); i++){
    //printf("writing -%s-\n",lines[i].toUtf8().constData());
    if (i==version_linenum)
      DISK_write_qstring(file, get_settings_version_line() + "\n");
    //out << get_settings_version_line() << "\n";
    else
      DISK_write_qstring(file, lines[i] + "\n");
    //out << lines[i] << "\n";
  }

  /*
  temporary_write_file.close();

  transfer_temporary_file_to_file(temporary_write_file.fileName(), filename);
  //getchar();  
  */

  DISK_close_and_delete(file);
}

#if 0
static void append_line(const char** lines, const char* line){
  int i=0;
  while(lines[i]!=NULL)
    i++;
  lines[i] = line;
}
#endif

// Warning, called before GC_init, so it cannot allocate with talloc or talloc_atomic.
bool SETTINGS_remove(const char* key){
  QVector<QString> lines = get_lines(key);
  if(lines.size()==0)
    return false;
  
  int linenum = find_linenum(key,lines);

  if (linenum == -1)
    return false;

  lines.remove(linenum);

  write_lines(key, lines);

  return true;
}

// Warning, called before GC_init, so it cannot allocate with talloc or talloc_atomic.
static void SETTINGS_put(const char* key, QString val){
  QVector<QString> lines = get_lines(key);
  if(lines.size()==0){
    R_ASSERT(false);
    return;
  }
  
  int linenum = find_linenum(key,lines);

  QString temp = QString(key) + " = " + val;

  if(linenum==-1)
    lines.push_back(temp);
  else
    lines[linenum] = temp;
  
  write_lines(key, lines);
}

// Warning, called before GC_init, so it cannot allocate with talloc or talloc_atomic.
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

bool SETTINGS_has_key(const char *key){
  return SETTINGS_get_chars(key) != NULL;
}

void SETTINGS_set_custom_configfile(filepath_t filename){
  custom_configuration_filename=STRING_get_qstring(filename.id);
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

int SETTINGS_read_int32(const char* key, int def){
  const char* val = SETTINGS_get_chars(key);

  if(val==NULL)
    return def;
  else
    return atoi(val);
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

const wchar_t* SETTINGS_read_wchars(const char* key, const wchar_t* def){
  QString val = SETTINGS_get(key);

  if(val==not_found)
    return def;
  else
    return STRING_create(val);
}

QString SETTINGS_read_qstring(QString key, QString def){
  return SETTINGS_read_qstring(key.toUtf8().constData(), def);
}

vector_t *SETTINGS_get_all_lines_starting_with(const char *prefix){
  vector_t *ret = (vector_t*)talloc(sizeof(vector_t));
  auto lines = get_lines(prefix);

  for(int linenum = 0 ; linenum < lines.size() ; linenum++) {
    QString line = lines[linenum].trimmed();
    if (line.startsWith(prefix))
      VECTOR_push_back(ret, talloc_strdup(line.toUtf8().constData()));
  }

  return ret;
}

void SETTINGS_write_bool(const char* key, bool val){
  SETTINGS_write_string(key, val==true?"true":"false");
}

void SETTINGS_write_int(const char* key, int64_t val){
  qlonglong val2 = val;
  SETTINGS_put(key, QString::number(val2));
  /*
    char temp[500];
    sprintf(temp, "%" PRId64 "", val);
    SETTINGS_put(key,temp);
  */
}

void SETTINGS_write_double(const char* key, double val){
  char temp[500];
  sprintf(temp,"%s",OS_get_string_from_double(val));
  SETTINGS_put(key,temp);
}

void SETTINGS_write_string(const char* key, const char* val){
  SETTINGS_put(key,val);
}

void SETTINGS_write_string(const char* key, QString val){
  SETTINGS_put(key,val);
}

void SETTINGS_write_wchars(const char* key, const wchar_t *wchars){
  SETTINGS_put(key, STRING_get_chars(wchars));
}

void SETTINGS_write_string(QString key, const char* val){
  SETTINGS_put(key.toUtf8().constData(),val);
}

void SETTINGS_write_string(QString key, QString val){
  SETTINGS_put(key.toUtf8().constData(),val);
}

// Note: Called before SETTINGS_init.
void SETTINGS_delete_configuration(void){
  
  {
    filepath_t conf = OS_get_config_filename("something");
    if (DISK_file_exists(conf) && !DISK_delete_file(conf))
      GFX_Message(NULL, "Error: Unable to delete file \"%S\". You should try to delete it manually.", conf.id);
  }

  {
    filepath_t color = OS_get_config_filename("color5");
    if (DISK_file_exists(color) && !DISK_delete_file(color))
      GFX_Message(NULL, "Error: Unable to delete file \"%S\". You should try to delete it manually.", color.id);
  }
  
  filepath_t dotradiumpath = make_filepath(OS_get_dot_radium_path());

  {
    filepath_t mod_samples = appendFilePaths(dotradiumpath, make_filepath("mod_samples"));
    if (DISK_dir_exists(mod_samples) && !DISK_delete_all_files_in_dir(mod_samples))
      GFX_Message(NULL, "Error: Unable to delete all files in \"%S\". You should try to delete them manually.", mod_samples.id);
  }

  {
    filepath_t scanned_plugins = appendFilePaths(dotradiumpath, make_filepath("scanned_plugins"));
    if (DISK_dir_exists(scanned_plugins) && !DISK_delete_all_files_in_dir(scanned_plugins))
      GFX_Message(NULL, "Error: Unable to delete all files in \"%S\". You should try to delete them manually.", scanned_plugins.id);
  }

  
}

void SETTINGS_init(void){
  double settings_version = SETTINGS_read_double("settings_version", 0.0);

  // Enable draw_in_separate_process if it had been excplicitly disabled in a version where it didn't always work very well.
  if (settings_version <= 0.725){

    const char *draw_in_separate = "opengl_draw_in_separate_process";

    if (SETTINGS_has_key(draw_in_separate)){
      bool val = SETTINGS_read_bool(draw_in_separate,true);
      if (val==false)
        SETTINGS_write_bool(draw_in_separate,true);
    }
  }
}


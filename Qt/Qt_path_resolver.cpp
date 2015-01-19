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



#include <QDir>
#include <QHash>
#include <QMessageBox>
#include <QFileDialog>

#include "../common/nsmtracker.h"
#include "../common/OS_visual_input.h"
#include "../common/OS_settings_proc.h"

#ifndef TEST_PATH_RESOLVER

#include "EditorWidget.h"

extern int num_users_of_keyboard;

extern struct Root *root;

static QHash<QString, QDir> resolved_paths;

static void ask_to_add_resolved_path(QDir key, QDir value){
  QMessageBox msgBox;

  msgBox.setText("A different path was selected");
  msgBox.setInformativeText("Do you want Radium to automatically check "+value.path()+" for all future files not found in "+key.path()+"?");
  msgBox.setStandardButtons(QMessageBox::Yes | QMessageBox::No);
  msgBox.setDefaultButton(QMessageBox::Yes);

  int ret = msgBox.exec();

  if(ret==QMessageBox::Yes)
    resolved_paths[key.path()] = value;
}

static const char *_loading_path  = NULL;

void OS_set_loading_path(const char *filename){
  _loading_path = filename;
}

void OS_unset_loading_path(void){
  _loading_path = NULL;
}

#endif // !TEST_PATH_RESOLVER

static QString saving_path;

void OS_set_saving_path(const char *filename){
  QFileInfo info(filename);
  saving_path = info.absolutePath();
  printf("saving_path: -%s-\n",saving_path.toUtf8().constData());
}



/*
  Returns a path which is relative to the loading/saving path, if possible.

  It works like this:

  filepath      saving_path   result
  ========================================
  /a/b/c.wav    /a/b          c.wav          (returns a relative path)
  /d/e/f.wav    /a/b          /d/e/f.wav     (returns a full path
  /a/b/c.wav    /a            b/c.wav        (returns a relative path)

*/
const char *OS_saving_get_relative_path_if_possible(const char *filepath){
  if (saving_path.isEmpty())
    return filepath;

  QFileInfo info(filepath);

  if (info.isRelative())
    return filepath;
  
  printf("canonical: -%s-\n",info.absolutePath().toUtf8().constData());

  QString filepath2 = info.absolutePath()+QDir::separator();
  QString savepath2 = saving_path+QDir::separator();
  printf("filepath2: -%s-, savepath2: -%s-\n",filepath2.toUtf8().constData(),savepath2.toUtf8().constData());

  if (filepath2.startsWith(savepath2))
    return filepath + savepath2.length();
  else
    return filepath;
}





#ifndef TEST_PATH_RESOLVER

const char *OS_loading_get_resolved_file_path(const char *path){
  QFileInfo info(path);

  printf("path: -%s-, loading-path: -%s-\n",path,_loading_path);

  // Try the original path
  if(!info.isRelative() && info.exists()==true){
    return talloc_strdup(path);
  }
  
  QDir dir = info.dir();

  // Try song path if relative
  if(_loading_path!=NULL && info.isRelative()){
    QFileInfo info3(_loading_path);
    QFileInfo info2(info3.dir().path(), info.filePath());
    
    //printf("gotit2 -%s- -%s-\n",info3.filePath(),info2.filePath());
    //char temp[50];
    //gets(temp);

    if(info2.exists()){
      return talloc_strdup(info2.filePath());
    }
  }

  // Try resolved paths
  if(resolved_paths.contains(dir.path())){
    QFileInfo info2(resolved_paths[dir.path()], info.fileName());

    if(info2.exists()) {

      return talloc_strdup(info2.filePath());
    }
  }

  // Ask user for path. Last resort.
  {
    QFileInfo info3;

    do{
      struct Tracker_Windows *window=static_cast<struct Tracker_Windows*>(root->song->tracker_windows);
      EditorWidget *editor=(EditorWidget *)window->os_visual.widget;

      num_users_of_keyboard++;

      QMessageBox msgBox;
        
      msgBox.setText(QString("Could not find "+info.fileName()+" in"+dir.path()+".\nPlease select new file."));
      //msgBox.setInformativeText("Could not find "+info.fileName()+" in"+dir.path()+". Please select new file."
      msgBox.setStandardButtons(QMessageBox::Ok);
      
      msgBox.exec();

      QString filename = QFileDialog::getOpenFileName(editor, 
                                                      QString("Select file to replace ")+info.fileName()
                                                      //,QString()
                                                      //,info.fileName()
                                                      );
      num_users_of_keyboard--;

      if(filename == "")
        return NULL;

      info3 = QFileInfo(filename);

    }while(info3.exists()==false);

    if(info3.fileName() == info.fileName())
      ask_to_add_resolved_path(dir, info3.dir());

    return talloc_strdup(info3.filePath());
  }
}

#endif // !TEST_PATH_RESOLVER



#ifdef TEST_PATH_RESOLVER

#include <stdarg.h>
#include <assert.h>

#include "../common/control_proc.h"

char *talloc_strdup(char *s){
  return strdup(s);
}
                   
void EndProgram(void){
  printf("ENDPROGRAM called\n");
}

void RError(const char *fmt,...){
  char message[1000];
  va_list argp;
  
  va_start(argp,fmt);
  /*	vfprintf(stderr,fmt,argp); */
  vsprintf(message,fmt,argp);
  va_end(argp);

  fprintf(stderr,"error: %s\n",message);
}

int main(void){

  OS_set_saving_path("/asdf/tmp/filename.rad");

  printf("-%s- -%s-\n",OS_saving_get_relative_path_if_possible("/asdf/tmp/aiai"),saving_path.toUtf8().constData());

  assert(!strcmp(OS_saving_get_relative_path_if_possible("/badffa"), "/badffa"));
  assert(!strcmp(OS_saving_get_relative_path_if_possible("/badffa/aba"), "/badffa/aba"));
  assert(!strcmp(OS_saving_get_relative_path_if_possible("badffa/aba"), "badffa/aba"));

  assert(!strcmp(OS_saving_get_relative_path_if_possible("/asdf/tmp/aiai"), "aiai"));
  assert(!strcmp(OS_saving_get_relative_path_if_possible("/asdf/tmp2/aiai"), "/asdf/tmp2/aiai"));

  assert(!strcmp(OS_saving_get_relative_path_if_possible("/asdf/tmp/aiai/aiai234"), "aiai/aiai234"));


  printf("Success, no errors\n");

  return 0;
}

#endif

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
#include "../common/OS_disk_proc.h"

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

  int ret = safeExec(msgBox);

  if(ret==QMessageBox::Yes)
    resolved_paths[key.path()] = value;
}


#endif // !TEST_PATH_RESOLVER

static const wchar_t *_loading_path  = NULL;

void OS_set_loading_path(const wchar_t *filename){  
  QFileInfo info(STRING_get_qstring(filename));
  _loading_path = STRING_create(info.absoluteFilePath());
}

void OS_unset_loading_path(void){
  _loading_path = NULL;
}

static QString saving_path;

void OS_set_saving_path(const wchar_t *filename){
  QFileInfo info(STRING_get_qstring(filename));
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
const wchar_t *OS_saving_get_relative_path_if_possible(const wchar_t *wfilepath){  
  if (saving_path.isEmpty())
    return wfilepath;

  QString filepath = STRING_get_qstring(wfilepath);
  
  QFileInfo info(filepath);

  if (info.isRelative())
    return wfilepath;
  
  printf("canonical: -%s-\n",info.absolutePath().toUtf8().constData());

  QString filepath2 = info.absolutePath()+QDir::separator();
  QString savepath2 = saving_path+QDir::separator();
  printf("filepath2: -%s-, savepath2: -%s-\n",filepath2.toUtf8().constData(),savepath2.toUtf8().constData());

  if (filepath2.startsWith(savepath2))
    return STRING_create(filepath.remove(0, savepath2.length()));
  else
    return wfilepath;
}



#ifdef TEST_PATH_RESOLVER
static QHash<QString, QDir> resolved_paths;
#endif


#ifndef TEST_PATH_RESOLVER

const wchar_t *OS_loading_get_resolved_file_path(const wchar_t *wpath){
  QString path = QString::fromWCharArray(wpath);
  QFileInfo info(path);

  printf("path: -%s-, loading-path: -%s-\n",path.toUtf8().constData(),STRING_get_chars(_loading_path));

  // If the path is absolute, first try the original path.
  if(!info.isRelative() && info.exists()==true){
    return STRING_create(path);
  }
  
  QDir dir = info.dir();

  // Try song path if relative
  if(_loading_path!=NULL && info.isRelative()){
    QFileInfo info3(QString::fromWCharArray(_loading_path));
    QFileInfo info2(info3.dir().path(), info.filePath());
    
    //printf("gotit2 -%s- -%s-\n",info3.filePath(),info2.filePath());
    //char temp[50];
    //gets(temp);

    if(info2.exists()){
      return STRING_create(info2.filePath());
    }
  }

  // Try resolved paths
  if(resolved_paths.contains(dir.path())){
    QFileInfo info2(resolved_paths[dir.path()], info.fileName());

    if(info2.exists()) {

      return STRING_create(info2.filePath());
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

      msgBox.setText(QString("Could not find "+info.fileName()+" in "+dir.path()+".\nPlease select new file."));
      //msgBox.setInformativeText("Could not find "+info.fileName()+" in"+dir.path()+". Please select new file."
      msgBox.setStandardButtons(QMessageBox::Ok);
      
      safeExec(msgBox);

      QString filename;

      GL_lock();{ // GL_lock is needed when using intel gfx driver to avoid crash caused by opening two opengl contexts simultaneously from two threads.
        filename = QFileDialog::getOpenFileName(editor, 
                                                QString("Select file to replace ")+info.fileName()
                                                //,QString()
                                                //,info.fileName()
                                                );
      }GL_unlock();

      num_users_of_keyboard--;

      if(filename == "")
        return NULL;

      info3 = QFileInfo(filename);

    }while(info3.exists()==false);

    if(info3.fileName() == info.fileName())
      ask_to_add_resolved_path(dir, info3.dir());

    return STRING_create(info3.filePath());
  }
}

#endif // !TEST_PATH_RESOLVER



#ifdef TEST_PATH_RESOLVER

#include <stdarg.h>
#include <assert.h>

#include "../common/control_proc.h"

#define talloc_strdup(a) strdup(a)
#define talloc_atomic(a) malloc(a)

#include "Qt_settings.cpp"

                   
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


static wchar_t *s(const char *st){
  QString string(st);
  return STRING_create(string);
}

int main(void){

  OS_set_saving_path(s("/asdf/tmp/filename.rad"));

  printf("-%s- -%s-\n",STRING_get_chars(OS_saving_get_relative_path_if_possible(s("/asdf/tmp/aiai"))),saving_path.toUtf8().constData());

  assert(STRING_equals(OS_saving_get_relative_path_if_possible(s("/badffa")), "/badffa"));
  assert(STRING_equals(OS_saving_get_relative_path_if_possible(s("/badffa/aba")), "/badffa/aba"));
  assert(STRING_equals(OS_saving_get_relative_path_if_possible(s("badffa/aba")), "badffa/aba"));
  
  assert(STRING_equals(OS_saving_get_relative_path_if_possible(s("/asdf/tmp/aiai")), "aiai"));
  assert(STRING_equals(OS_saving_get_relative_path_if_possible(s("/asdf/tmp2/aiai")), "/asdf/tmp2/aiai"));
  
  assert(STRING_equals(OS_saving_get_relative_path_if_possible(s("/asdf/tmp/aiai/aiai234")), "aiai/aiai234"));
  

  OS_set_saving_path(s("sounds/filename.rad"));
  assert(STRING_equals(OS_saving_get_relative_path_if_possible(s("aiai234.wav")), "aiai234.wav"));
  assert(STRING_equals(OS_saving_get_relative_path_if_possible(s("aiai234.wav")), "aiai234.wav"));


  printf("Success, no errors\n");

  return 0;
}

#endif

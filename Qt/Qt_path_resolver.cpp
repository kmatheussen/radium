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


const char *OS_get_resolved_file_path(const char *path){
  QFileInfo info(path);

  // Try the original path
  if(info.exists()==true)
    return talloc_strdup(path);

  QDir dir = info.dir();

  // Try resolved paths
  if(resolved_paths.contains(dir.path())){
    QFileInfo info2(resolved_paths[dir.path()], info.fileName());

    if(info2.exists())
      return talloc_strdup(info2.filePath());
  }

  // Try song path
  if(_loading_path!=NULL){
    QFileInfo info3(_loading_path);
    QFileInfo info2(info3.dir().path(), info.fileName());
    if(info2.exists())
      return talloc_strdup(info2.filePath());
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

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


#include <QFileDialog>

#include "../audio/VST_plugins_proc.h"
#include "../audio/SoundPlugin.h"
#include "../audio/SoundPluginRegistry_proc.h"

#include "Qt_MyQButton.h"

#include "Qt_vst_paths_widget.h"

namespace{
class Vst_paths_widget : public QWidget, public Ui::Vst_paths_widget{
  Q_OBJECT

 public:
  bool initing;

 Vst_paths_widget(QWidget *parent=NULL)
    : QWidget(parent)
  {
    initing = true;
    setupUi(this);
    read_settings();
    initing = false;
  }

   bool is_in_list(QString string){
    for(int i=0;i<path_list->count();i++){
      QString vst_path = path_list->item(i)->text();
      if(vst_path==string)
        return true;
    }
    return false;
  }

  void read_settings(){
    std::vector<QString> paths = VST_get_vst_paths();

    for(unsigned int i=0; i<paths.size(); i++)
      if(is_in_list(paths.at(i))==false)
        path_list->addItem(paths.at(i));
  }

  void write_settings(){
    std::vector<QString> paths;

    for(int i=0;i<path_list->count();i++)
      paths.push_back(path_list->item(i)->text());

    VST_write_vst_paths(paths);
  }

  void add_current_path(){
    QString path = path_edit->text();
    path = path.trimmed();
    if(path.length()>0 && is_in_list(path)==false){
      path_list->addItem(path);
      write_settings();
      PR_init_plugin_types();
    }
    path_edit->clear();
  }

 public slots:

  void on_path_edit_editingFinished(){
    add_current_path();
    set_editor_focus();
  }

  void on_rescan_button_pressed(){
    PR_init_plugin_types();
  }

  void on_delete_button_pressed(){
    //printf("Trying to remove %p\n",path_list->currentItem());
    delete path_list->currentItem();
    write_settings();
  }

  void on_add_button_pressed(){
    add_current_path();    
  }

  void on_open_file_dialog_button_pressed(){
    QString dirname = QFileDialog::getExistingDirectory(this,"Select VST Directory");
    path_edit->setText(dirname);
  }

  void on_buttonBox_clicked(){
    hide();
  }

};
}

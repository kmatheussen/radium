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

#include "../api/api_proc.h"

#include "Qt_MyQButton.h"

#include "Qt_vst_paths_widget.h"


namespace{
class Vst_paths_widget : public QWidget, public Ui::Vst_paths_widget{
  Q_OBJECT

 public:
  bool initing;
  bool _is_updating_widgets = false;
  
 Vst_paths_widget(QWidget *parent=NULL)
    : QWidget(parent)
  {
    initing = true;
    setupUi(this);

    rescan_button->hide(); // we rescan when pressing "add".
    
#if defined(FOR_MACOSX)
    path_list->addItem("/Library/Audio/Plug-Ins/VST/");

#if 0
    path_list->setEnabled(false);
    delete_button->setEnabled(false);
    add_button->setEnabled(false);
    open_file_dialog_button->setEnabled(false);
    path_edit->setEnabled(false);
#else
    path_list->hide();
    delete_button->hide();
    add_button->hide();
    open_file_dialog_button->hide();
    path_edit->hide();
    //horizontalLayout->hide();
    groupBox->setTitle("VST plugins");
#endif
    
#else
    
    read_settings();
    
#endif

#if FOR_WINDOWS
    always_on_top->hide();
#endif
    
    updateWidgets();

    initing = false;
  }

  void updateWidgets(void){
    _is_updating_widgets = true;
    
    always_on_top->setChecked(vstGuiIsAlwaysOnTop());

    show_virtual_keyboard->setChecked(showVirtualMidiKeyboardBelowNativeGUIs());

    show_instrument_widget_when_double_clicking->setChecked(showInstrumentWidgetWhenDoubleClickingSoundObject());

    bool lock_juce = doLockJuceWhenSwappingOpenGL();
    lock_juce_when_swapping_onoff->setChecked(lock_juce);
      

    _is_updating_widgets = false;
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

  void scan(void){    
    PR_init_plugin_types();
  }
  
  void add_current_path(){
    QString path = path_edit->text();
    path = path.trimmed();
    if(path.length()>0 && is_in_list(path)==false){
      path_list->addItem(path);
      write_settings();
      scan();
    }
    path_edit->clear();
  }

 public slots:

  void on_always_on_top_toggled(bool val){
    if (_is_updating_widgets == false){
      setVstGuiAlwaysOnTop(val);
    }
  }

  void on_show_virtual_keyboard_toggled(bool val){
    if (_is_updating_widgets == false){
      setShowVirtualMidiKeyboardBelowNativeGUIs(val);
    }
  }

  void on_show_instrument_widget_when_double_clicking_toggled(bool val){
    if (_is_updating_widgets==false)
      setShowInstrumentWidgetWhenDoubleClickingSoundObject(val);
  }

  void on_lock_juce_when_swapping_onoff_toggled(bool val){
    if (_is_updating_widgets == false)
      setLockJuceWhenSwappingOpenGL(val);
  }
  
  void on_path_edit_returnPressed(){
    add_current_path();
    set_editor_focus();
  }

  void on_rescan_button_clicked(){
    PR_init_plugin_types();
  }

  void on_delete_button_clicked(){
    //printf("Trying to remove %p\n",path_list->currentItem());
    delete path_list->currentItem();
    write_settings();
    scan();
  }

  void on_add_button_clicked(){
    add_current_path();    
  }

  void on_open_file_dialog_button_clicked(){
    obtain_keyboard_focus();

    GL_lock();{ // GL_lock is needed when using intel gfx driver to avoid crash caused by opening two opengl contexts simultaneously from two threads.
      QString dirname = QFileDialog::getExistingDirectory(this,
                                                          "Select VST Directory",
                                                          QString(),
                                                          QFileDialog::DontUseCustomDirectoryIcons);
      path_edit->setText(dirname);
    }GL_unlock();

    release_keyboard_focus();

    add_current_path();
  }

  void on_buttonBox_clicked(){
    //hide();
  }

};

}

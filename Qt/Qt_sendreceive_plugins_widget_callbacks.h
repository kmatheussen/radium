/* Copyright 2020 Kjetil S. Matheussen

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


#include "Qt_sendreceive_plugins_widget.h"

#include "../audio/SendReceive_plugins_proc.h"


class SendReceive_Plugin_widget : public QWidget, public Ui::SendReceive_Plugin_widget{
  Q_OBJECT;

public:

  radium::Initing _initing;
  
  radium::GcHolder<struct Patch> _patch;

  SendReceive_Plugin_widget(QWidget *parent, struct Patch *patch)
    : QWidget(parent)
    , _patch(patch)
  {
    radium::ScopedIniting initing(_initing);

    setupUi(this);
    update_gui();
  }

  ~SendReceive_Plugin_widget() {
  }

  void update_gui(void){
    R_ASSERT_RETURN_IF_FALSE(_patch.data()!=NULL);
    R_ASSERT_RETURN_IF_FALSE(_patch->patchdata!=NULL);

    radium::ScopedIniting initing(_initing);
    
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    
    QLineEdit *line_edit = nameEdit;
    
    R_ASSERT_RETURN_IF_FALSE(line_edit!=NULL);

    printf(" --------------------------- SETTING TEXT TO -%S-\n", SEND_RECEIVE_get_name(plugin));
    
    line_edit->setText(STRING_get_qstring(SEND_RECEIVE_get_name(plugin)));

    if (is_receiver_plugin(plugin)) {
      compensate_latency->setChecked(SEND_RECEIVE_get_compensate_latency(plugin));
    } else {
      compensate_latency->hide();
    }
  }


  int _editing_name_generation = 0;
  
public slots:
  
  void on_nameEdit_editingFinished(){

    if (!_initing.can_access())
      return;

    radium::ScopedIniting initing(_initing);
        
    radium::ScopedGeneration generation(_editing_name_generation);
    
    if (_editing_name_generation > 4){ // Safety. If get_name/set_name is misbehaving, we could end up in an endless loop here.
      printf("        Qt_sendreceive_plugins_widget_callback.h:on_nameEdit_editingFinished: Error: Might be an endless recursive loop here.");
      return;
    }

    QLineEdit *line_edit = nameEdit;
    
    R_ASSERT_RETURN_IF_FALSE(line_edit!=NULL);

    // trim content of line edit.
    //if (text != STRING_get_qstring(STRING_trim(STRING_create(text))))
    //  line_edit->setText(STRING_get_qstring(STRING_trim(STRING_create(line_edit->text()))));

    R_ASSERT_RETURN_IF_FALSE(_patch.data()!=NULL);
    R_ASSERT_RETURN_IF_FALSE(_patch->patchdata!=NULL);

    instrument_t patch_id = _patch->id;
    
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;

    QString old_name = STRING_get_qstring(SEND_RECEIVE_get_name(plugin));
    QString new_name = STRING_get_qstring(STRING_trim(STRING_create(line_edit->text())));
    
    //QString text = STRING_trim(STRING_create(new_name))));

    //printf("A: %S. B: %S\n", text, SEND_RECEIVE_get_name(plugin));

    if (new_name != old_name) {

      auto setit = [patch_id](QString name){
        Patch *patch = PATCH_get_from_id(patch_id);
        
        R_ASSERT_RETURN_IF_FALSE(patch!=NULL);
        R_ASSERT_RETURN_IF_FALSE(patch->patchdata!=NULL);
        
        SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
        SEND_RECEIVE_set_name(plugin, STRING_create(name), true);
      };
      
      UNDO_functions(
                     talloc_format("Send/Receive name for %s", _patch->name),
                     [setit, new_name](){
                       setit(new_name);
                     },
                     [setit, old_name](){
                      setit(old_name);
                     });
  
      //SEND_RECEIVE_set_name(plugin, new_name, true);
      
      set_editor_focus();
    }
  }

  void on_compensate_latency_toggled(bool val){
    if (!_initing.can_access())
      return;
    
    R_ASSERT_RETURN_IF_FALSE(_patch.data()!=NULL);
    R_ASSERT_RETURN_IF_FALSE(_patch->patchdata!=NULL);

    instrument_t patch_id = _patch->id;
    
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    
    R_ASSERT_RETURN_IF_FALSE(is_receiver_plugin(plugin));

    auto setit = [patch_id](bool doit){
      Patch *patch = PATCH_get_from_id(patch_id);
      
      R_ASSERT_RETURN_IF_FALSE(patch!=NULL);
      R_ASSERT_RETURN_IF_FALSE(patch->patchdata!=NULL);
      
      SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;

      R_ASSERT_RETURN_IF_FALSE(is_receiver_plugin(plugin));

      SEND_RECEIVE_set_compensate_latency(plugin, doit);
    };
      
    UNDO_functions(
                   talloc_format("Receive compensate latency for %s", _patch->name),
                   [setit, val](){
                     setit(val);
                   },
                   [setit, val](){
                     setit(!val);
                   });
  }
};

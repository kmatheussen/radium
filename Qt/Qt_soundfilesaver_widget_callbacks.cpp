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


#include <stdio.h>
#include <unistd.h>

#include <sndfile.h>

#include <QTimer>
#include <QFileDialog>

#include "../common/nsmtracker.h"

#include <FocusSniffers.h>


#include "../common/cursor_updown_proc.h"
#include "../common/windows_proc.h"
#include "../common/visual_proc.h"
#include "../common/player_proc.h"

#include "../audio/Mixer_proc.h"
#include "../audio/SoundProducer_proc.h"
#include "../audio/SoundfileSaver_proc.h"

#include "../mixergui/QM_chip.h"

#include "Qt_instruments_proc.h"
#include "helpers.h"

#include "Qt_soundfilesaver_widget.h"


class Soundfilesaver_widget : public RememberGeometryQDialog, public Ui::Soundfilesaver_widget {
  Q_OBJECT

  SoundPlugin *currently_saving_plugin;
  radium::Vector<SoundPlugin*> plugins_to_save;

  MyQMessageBox *msgBox;

  QString get_suffix(void){
    if(format_wav->isChecked())
      return "wav";
    else if(format_aiff->isChecked())
      return "aiff";
    else
      return "flac";
  }
  
  void save(QString filename){

    const char *error_string;

    int format;
    
    if(format_wav->isChecked())
      format = SF_FORMAT_WAV;
    else if(format_aiff->isChecked())
      format = SF_FORMAT_AIFF;
    else
      format = SF_FORMAT_FLAC;
    
    if(format_16->isChecked())
      format |= SF_FORMAT_PCM_16;
    else if(format_24->isChecked())
      format |= SF_FORMAT_PCM_24;
    else if(format_32->isChecked())
      format |= SF_FORMAT_PCM_32;
    else
      format |= SF_FORMAT_FLOAT;
    
    enum SOUNDFILESAVER_what what_to_save
      = save_range_button->isChecked()==true ? SAVE_RANGE
      : save_block_button->isChecked()==true ? SAVE_BLOCK
      : SAVE_SONG;
    
    if(SOUNDFILESAVER_save(filename.toUtf8().constData(), what_to_save, MIXER_get_sample_rate(), format, post_silence_spin->value(), &error_string)==false){

      //QMessageBox msgBox;
      
      msgBox->setText("Unable to save file:");
      msgBox->setInformativeText(error_string);
      msgBox->setStandardButtons(QMessageBox::Ok);
      
      //safeExec(msgBox);
      return;
      
    } else {

      msgBox->setText("Please wait, saving "+filename);
      _timer.start();
      
    }
  }

  void clean_prev(void){
    if (currently_saving_plugin != NULL) {
      ATOMIC_SET(currently_saving_plugin->solo_is_on, false);
      currently_saving_plugin = NULL;
    }
  }
  
  void save_next(void){

    clean_prev();
    
    if (plugins_to_save.is_empty()){
      //QMessageBox msgBox;
        
      msgBox->setText("All files saved");
      //msgBox->setInformativeText(message);
      msgBox->setStandardButtons(QMessageBox::Ok);

      safeExec(msgBox); 
      return;
    }

    currently_saving_plugin = plugins_to_save.pop(0);

    ATOMIC_SET(currently_saving_plugin->solo_is_on, true);
    CHIP_update(currently_saving_plugin);
    GFX_update_instrument_widget((struct Patch*)currently_saving_plugin->patch);

    QDir dir(filename_edit->text());
    QString dirname = dir.absolutePath();

    save(dirname + QDir::separator() + currently_saving_plugin->patch->name + "." + get_suffix());
  }
  
  public:

  struct Timer : public QTimer{
    Soundfilesaver_widget *parent;
    
    DEFINE_ATOMIC(const char *, async_message);

    void timerEvent(QTimerEvent * e){
      printf("clicked: %p\n", parent->msgBox->clickedButton());

      if (parent->msgBox->clickedButton()!=NULL){
        SOUNDFILESAVER_request_stop();

        // Reset clickedButton().
        delete parent->msgBox;
        parent->msgBox = new MyQMessageBox;
      }
      
      const char *message = ATOMIC_GET(async_message);
      if(message != NULL){
        
        ATOMIC_SET(async_message, NULL);

        MIXER_request_stop_saving_soundfile(); // This is very messy. The code would be far simpler if jack_set_freewheel could be called from any thread.

        //usleep(1000*100); // Wait a bit for saving to stop;

        ScrollEditorToRealLine_CurrPos(root->song->tracker_windows, root->song->tracker_windows->wblock->bot_realline);
        root->song->tracker_windows->must_redraw = true;
#if 0        
        MyQMessageBox msgBox;
        
        msgBox->setText(QString(message));
        //msgBox->setInformativeText(message);
        msgBox->setStandardButtons(QMessageBox::Ok);

        safeExec(msgBox);
#endif

        bool was_cancelled = !strcmp(message, "Cancelled");
        
        V_free((void*)message);

        stop();

        parent->clean_prev();
        
        if (was_cancelled)
          PlayStop(); // Sometimes it continues playing after pressing "cancel".
        else
          parent->save_next();
      }
    }
  };

  bool _initing;
  Timer _timer;

 Soundfilesaver_widget(QWidget *parent=NULL)
    : RememberGeometryQDialog(parent)
    , currently_saving_plugin(NULL)
    , msgBox(NULL)
  {
    _initing = true;

    msgBox = new MyQMessageBox;
    
    setupUi(this);

    _initing = false;

    _timer.parent = this;
    ATOMIC_SET(_timer.async_message, NULL);
    _timer.setInterval(100);
  }

  // Don't want return to close the dialog
  void keyPressEvent(QKeyEvent *evt)
  {
    if(evt->key() == Qt::Key_Enter || evt->key() == Qt::Key_Return)
      evt->accept();
    else
      QDialog::keyPressEvent(evt);
  }

public slots:
  void on_buttonBox_clicked(QAbstractButton * button){
    if(button->text() == QString("Save")){
      printf("Save\n");

#if FULL_VERSION==0

      GFX_Message(NULL,
                  "Soundfile export is only available to subscribers.<p>"
                  "Subscribe <a href=\"http://users.notam02.no/~kjetism/radium/download.php\">here</a>."
                  );

#else // FULL_VERSION==0

      if (MIXER_get_soundplugin("Jack", "System Out")==NULL) {
        GFX_Message(NULL, "No \"System Out\" instrument found in the mixer.");
        return;
      }

      bool save_multi = many_soundfiles->isChecked();

      if(filename_edit->text()==QString("")){
        GFX_Message(NULL,
                    "%s was not specified.",
                    save_multi ? "Directory" : "Filename"
                    );
        return;
      }

      delete msgBox;
      msgBox = new MyQMessageBox;  // ensure clickedButton()==NULL.


      msgBox->setStandardButtons(QMessageBox::Cancel);
            
      ATOMIC_SET(_timer.async_message, NULL);

      if (save_multi){

        QFileInfo info(filename_edit->text());

        if (info.isFile()){
          GFX_Message(NULL,
                      "Can not save. \"%s\" is a file, and not a directory", filename_edit->text().toUtf8().constData()
                      );
          return;
        }

        QDir dir(filename_edit->text());
        QString dirname = dir.absolutePath();
        
        if (dir.exists()){
          vector_t options = {};
          VECTOR_push_back(&options, "Yes");
          VECTOR_push_back(&options, "No");
          
          if (GFX_Message(&options,
                          "Directory \%s\" already exists. Overwrite files in that directory?",
                          dirname.toUtf8().constData()
                          )
              ==1)
            return;
        } else {

          if(QDir::root().mkpath(dirname)==false){ // why on earth isn't mkpath a static function?
            GFX_Message(NULL, "Unable to create directory \"%s\".", dirname.toUtf8().constData());
            return;
          }
          
        }
        
        plugins_to_save.clear(); // In case we were interrupted earlier.

        const radium::Vector<SoundProducer*> *sp_all = MIXER_get_all_SoundProducers();
        for (auto sp : *sp_all){
          if (!SP_has_input_links(sp)){
            plugins_to_save.push_back(SP_get_plugin(sp));
          }
        }

        safeShow(msgBox);

        save_next();

      } else {

        safeShow(msgBox);

        save(filename_edit->text());

      }
      
#endif //FULL_VERSION==0
    }
  }

  void on_post_silence_spin_editingFinished(){
    set_editor_focus();
  }

  void on_filename_edit_returnPressed(){
    printf("return pressed\n");
    filename_edit->clearFocus();
  }
  
  void on_filename_button_clicked(){
    QFileDialog::Options options = useNativeFileRequesters() ? (QFileDialog::Option)0 : QFileDialog::DontUseNativeDialog;
    if (many_soundfiles->isChecked()) {
      
      QFileDialog dialog(this, 
                         QString("Select directory")
                         );
      dialog.setOptions(options | QFileDialog::ShowDirsOnly);
      dialog.setFileMode(QFileDialog::Directory);
      //dialog.setFileMode(QFileDialog::AnyFile);

      auto state = safeExec(&dialog);

      if(state == QDialog::Accepted){
        QString dirname = dialog.directory().absolutePath();

        printf("dir: %s\n",dirname.toUtf8().constData());
  
        filename_edit->setText(dirname);
      }
        
    } else {
      QString filename = QFileDialog::getSaveFileName(this, 
                                                      QString("Select file"),
                                                      QString(),
                                                      QString(),
                                                      0,
                                                      options
                                                      );
      filename_edit->setText(filename);
    }
  }

};


static Soundfilesaver_widget *widget=NULL;

extern "C"{
  void SOUNDFILESAVERGUI_open(void){

    if(widget==NULL)
      widget = new Soundfilesaver_widget(NULL);

#if 0
    widget->show();
#else
    safeExec(widget);
#endif
  }

  void SOUNDFILESAVERGUI_stop(const char *message){
    ATOMIC_SET(widget->_timer.async_message, V_strdup(message));
  }
}

#include "mQt_soundfilesaver_widget_callbacks.cpp"


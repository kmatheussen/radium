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

#include <QTimer>
#include <QFileDialog>

#define INCLUDE_SNDFILE_OPEN_FUNCTIONS 1
#include "../common/nsmtracker.h"

#include "Qt_MyQSpinBox.h"
#include <FocusSniffers.h>


#include "../common/cursor_updown_proc.h"
#include "../common/windows_proc.h"
#include "../common/visual_proc.h"
#include "../common/player_proc.h"

#include "../audio/Mixer_proc.h"
#include "../audio/SoundProducer_proc.h"
#include "../audio/SoundfileSaver_proc.h"
#include "../audio/Sampler_plugin_proc.h"
#include "../audio/SampleReader_proc.h"

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

    EVENTLOG_add_event(talloc_format("Saving sound \"%S\"", STRING_create(filename)));
    
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
    
    if(SOUNDFILESAVER_save(make_filepath(filename), what_to_save, MIXER_get_sample_rate(), format, num_channels->value(), post_silence_spin->value(), (enum ResamplerType)interpolation_type->currentIndex(), &error_string)==false){

      //QMessageBox msgBox;
      
      msgBox->setText("Unable to save file:");
      msgBox->setInformativeText(error_string);
      msgBox->setStandardButtons(QMessageBox::Ok);
      
      //safeExec(msgBox, true);
      return;
      
    } else {

      msgBox->setText("Please wait, saving "+filename);
      _timer.start();
      
    }
  }

  void clean_prev(void){
    if (currently_saving_plugin != NULL) {
      setInstrumentSolo(false, currently_saving_plugin->patch->id);
      currently_saving_plugin = NULL;
    }
  }
  
  void save_next(void){

    clean_prev();
    
    if (plugins_to_save.is_empty()){

      msgBox->hide();
      
      showAsyncMessage("All files saved");
      
      return;
    }

    currently_saving_plugin = plugins_to_save.pop(0);

    setInstrumentSolo(true, currently_saving_plugin->patch->id);
      
    QDir dir(filename_edit->text());
    QString dirname = dir.absolutePath();

    save(dirname + QDir::separator() + currently_saving_plugin->patch->name + "." + get_suffix());
  }
  
  public:

  struct Timer : public QTimer{
    Soundfilesaver_widget *parent;
    
    DEFINE_ATOMIC(const char *, async_message);

    void timerEvent(QTimerEvent * e) override {
      printf("clicked: %p\n", parent->msgBox->clickedButton());

      // Could seem like this one returns sometimes. We should never exit from here. Think it happens when the player calls rt_message.
      //RETURN_IF_DATA_IS_INACCESSIBLE();

      if (parent->msgBox->clickedButton()!=NULL){
        SOUNDFILESAVER_request_stop();

        // Reset clickedButton().
        delete parent->msgBox;
        parent->msgBox = MyQMessageBox::create(false, g_main_window);
        //parent->msgBox->setAttribute(Qt::WA_DeleteOnClose); Not sure if that is safe.
      }
      
      const char *message = ATOMIC_GET(async_message);
      if(message != NULL){
        
        ATOMIC_SET(async_message, NULL);

        MIXER_request_stop_saving_soundfile(); // This is very messy. The code would be far simpler if jack_set_freewheel could be called from any thread.

        SOUNDFILESAVER_writer_has_been_stopped();
        
        //msleep(100); // Wait a bit for saving to stop;

        ScrollEditorToRealLine_CurrPos(root->song->tracker_windows, root->song->tracker_windows->wblock->bot_realline);
        root->song->tracker_windows->must_redraw = true;
#if 0        
        MyQMessageBox msgBox;
        
        msgBox->setText(QString(message));
        //msgBox->setInformativeText(message);
        msgBox->setStandardButtons(QMessageBox::Ok);

        safeExec(msgBox, true);
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
   : RememberGeometryQDialog(parent, radium::NOT_MODAL)
    , currently_saving_plugin(NULL)
    , msgBox(NULL)
  {
    _initing = true;

    msgBox = MyQMessageBox::create(false, g_main_window);
    
    setupUi(this);

    interpolation_type->setToolTip("None   = No interpolation. Gives a metallic sound.\n"
                                   "Linear = Quick interpolation. Has a much less metallic sound than no interpolation.\n"
                                   "Cubic  = Also quick, but has a slightly less metallic sound than linear interpolation.\n"
                                   "Sinc1  = Excellent sound: \"The fastest bandlimited interpolator, providing a Signal-to-Noise Ratio (SNR) of 97dB and a bandwidth of 80%.\" (libsamplerate)\n"
                                   "Sinc2  = Excellent sound: \"The highest quality sinc based converter, providing a worst case SNR of 97dB at a bandwidth of 97%.\" (libsamplerate)");
    
    _initing = false;

    _timer.parent = this;
    ATOMIC_SET(_timer.async_message, NULL);
    _timer.setInterval(100);
  }

  // Don't want return to close the dialog
  void keyPressEvent(QKeyEvent *evt) override
  {
    if(evt->key() == Qt::Key_Enter || evt->key() == Qt::Key_Return)
      evt->accept();
    else
      QDialog::keyPressEvent(evt);
  }


public slots:

  // Override this one to prevent qt from automatically closing dialog when pressing a button in the button box.
  void accept() override{
  }
  

  void on_format_flac_toggled(bool is_on){
    if (is_on){
      if (format_32->isChecked() || format_float->isChecked())
        format_24->setChecked(true);            
    }

    format_32->setEnabled(!is_on);
    format_float->setEnabled(!is_on);
  }
  
  void on_buttonBox_clicked(QAbstractButton * button){

    if (buttonBox->buttonRole(button)!=QDialogButtonBox::AcceptRole)
      return;
      
    printf("Save\n");

#if FULL_VERSION==0

    GFX_Message2(NULL,
                 true,
                 "Soundfile export is only available to subscribers.<p>"
                 "Subscribe <a href=\"http://users.notam02.no/~kjetism/radium/download.php\">here</a>."
                 );

#else // FULL_VERSION==0

    bool save_multi = many_soundfiles->isChecked();

    if (GFX_OS_get_system_out() == NULL) {
      GFX_Message2(NULL, true, "No \"System Out\" instrument found in the mixer.");
      return;
    }

    if(filename_edit->text()==QString("")){
      GFX_Message2(NULL,
                   true,
                   "%s was not specified.",
                   save_multi ? "Directory" : "Filename"
                   );
      return;
    }

    const filepath_t filepath = make_filepath(filename_edit->text());
      
    if (!save_multi && DISK_file_exists(filepath)){
        
      if (SAMPLEREADER_has_file(filepath)){
        GFX_Message2(NULL,
                     true,
                     "Can not save to \"%S\" because the file already exists and is currently used in the program.",
                     filepath.id);
        return;
      }
                              
      vector_t options = {};
      int yes = VECTOR_push_back(&options, "Yes");
      VECTOR_push_back(&options, "No");
        
      if (GFX_Message2(&options,
                       true,
                       "File \%S\" already exists. Overwrite file?",
                       filepath.id
                       )
          !=yes)
        return;
    }
      
    delete msgBox;
    msgBox = MyQMessageBox::create(false, g_main_window);  // ensure clickedButton()==NULL.


    msgBox->setStandardButtons(QMessageBox::Cancel);
            
    ATOMIC_SET(_timer.async_message, NULL);

    plugins_to_save.clear(); // In case we were interrupted earlier.
      
    if (save_multi){

      QFileInfo info(filename_edit->text());

      if (info.isFile()){
        GFX_Message2(NULL,
                     true,
                     "Can not save. \"%S\" is a file, and not a directory", filepath.id
                     );
        return;
      }

      QDir dir(filename_edit->text());
      QString dirname = dir.absolutePath();
        
      if (dir.exists()){
        vector_t options = {};
        VECTOR_push_back(&options, "Yes");
        VECTOR_push_back(&options, "No");
          
        if (GFX_Message2(&options,
                         true,
                         "Directory \%S\" already exists. Overwrite files in that directory?",
                         STRING_create(dirname.toUtf8().constData())
                         )
            ==1)
          return;
      } else {

        if(QDir::root().mkpath(dirname)==false){ // why on earth isn't mkpath a static function?
          GFX_Message2(NULL, true, "Unable to create directory \"%S\".", STRING_create(dirname));
          return;
        }
          
      }
        
      const radium::Vector<SoundProducer*> &sp_all = MIXER_get_all_SoundProducers();
      for (auto sp : sp_all){
        SoundPlugin *plugin = SP_get_plugin(sp);
          
        if (!SP_has_input_links(sp) && SP_has_output_links(sp) && !SP_is_bus(sp) && strcmp(plugin->type->type_name, "Pipe")){
          bool doit = true;
            
          if (!strcmp(plugin->type->name, g_click_name) && !strcmp("Sample Player", plugin->type->type_name))
            doit = metronomeEnabled();
                
          if (doit)
            plugins_to_save.push_back(plugin);
        }
      }
        
      safeShow(msgBox);

      save_next();

    } else {

      safeShow(msgBox);

      save(filename_edit->text());

    }

    close();
      
#endif //FULL_VERSION==0
  }

  void on_post_silence_spin_editingFinished(){
    set_editor_focus();

    GL_lock();{
      post_silence_spin->clearFocus();
    }GL_unlock();
  }

  void on_num_channels_editingFinished(){
    set_editor_focus();
    GL_lock();{
      num_channels->clearFocus();
    }GL_unlock();
  }

  void on_filename_edit_returnPressed(){
    printf("return pressed\n");
    filename_edit->clearFocus();
  }
  
  void on_filename_button_clicked(){
    QFileDialog::Options options = QFileDialog::DontUseCustomDirectoryIcons | (useNativeFileRequesters() ? (QFileDialog::Option)0 : QFileDialog::DontUseNativeDialog);
    if (many_soundfiles->isChecked()) {
      
      QFileDialog dialog(this, 
                         QString("Select directory")
                         );

      dialog.setOptions(options | QFileDialog::ShowDirsOnly);
      dialog.setFileMode(QFileDialog::Directory);
      //dialog.setFileMode(QFileDialog::AnyFile);

      auto state = safeExec(&dialog, true);

      if(state == QDialog::Accepted){
        QString dirname = dialog.directory().absolutePath();

        printf("dir: %s\n",dirname.toUtf8().constData());
  
        filename_edit->setText(dirname);
      }
        
    } else {

      R_ASSERT_RETURN_IF_FALSE(g_radium_runs_custom_exec==false);

      radium::ScopedExec scopedExec(true);
      
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


static QPointer<Soundfilesaver_widget> widget=NULL;


void SOUNDFILESAVERGUI_open(void){
  
  if(widget==NULL){
    widget = new Soundfilesaver_widget(g_main_window);
  }
  
#if 1
  safeShow(widget);
#else
  safeExec(widget, true);
#endif
}

void SOUNDFILESAVERGUI_stop(const char *message){
  ATOMIC_SET(widget->_timer.async_message, V_strdup(message));
}


#include "mQt_soundfilesaver_widget_callbacks.cpp"


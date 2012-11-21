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
#include <sndfile.h>
#include <unistd.h>

#include <QMessageBox>
#include <QTimer>
#include <QFileDialog>
#include <FocusSniffers.h>

#include "../common/nsmtracker.h"
#include "../common/cursor_updown_proc.h"
#include "../common/windows_proc.h"
#include "../common/visual_proc.h"

#include "../audio/Mixer_proc.h"
#include "../audio/SoundfileSaver_proc.h"

#include "Qt_soundfilesaver_widget.h"

extern struct Root *root;

class Soundfilesaver_widget : public QDialog, public Ui::Soundfilesaver_widget {
  Q_OBJECT

 public:

  struct Timer : public QTimer{
    const char *async_message;

    void timerEvent(QTimerEvent * e){
      const char *message = async_message;
      if(message != NULL){
        async_message = NULL;

        MIXER_request_stop_saving_soundfile(); // This is very messy. The code would be far simpler if jack_set_freewheel could be called from any thread.

        //usleep(1000*100); // Wait a bit for saving to stop;

        ScrollEditorToRealLine_CurrPos(root->song->tracker_windows, root->song->tracker_windows->wblock->bot_realline);
        DrawUpTrackerWindow(root->song->tracker_windows);

        QMessageBox msgBox;
        
        msgBox.setText(QString(message));
        //msgBox.setInformativeText(message);
        msgBox.setStandardButtons(QMessageBox::Ok);

        msgBox.exec();

        free((void*)message);

        stop();
      }
    }
  };

  bool _initing;
  Timer _timer;

 Soundfilesaver_widget(QWidget *parent=NULL)
    : QDialog(parent)
  {
    _initing = true;

    setupUi(this);

    _initing = false;

    _timer.async_message = NULL;
    _timer.setInterval(100);
  }

public slots:
  void on_buttonBox_clicked(QAbstractButton * button){
    if(button->text() == QString("Save")){
      printf("Save\n");

      if(filename_edit->text()==QString("")){
        QMessageBox msgBox;
        
        msgBox.setText("Filename was not specified.");
        //msgBox.setInformativeText(error_string);
        msgBox.setStandardButtons(QMessageBox::Ok);

        msgBox.exec();
        return;
      }
      
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

      enum SOUNDFILESAVER_what what_to_save = save_block_button->isChecked()==true ? SAVE_BLOCK : SAVE_SONG;

      if(SOUNDFILESAVER_save(filename_edit->text(), what_to_save, MIXER_get_sample_rate(), format, post_silence_spin->value(), &error_string)==false){

        QMessageBox msgBox;
        
        msgBox.setText("Unable to save file:");
        msgBox.setInformativeText(error_string);
        msgBox.setStandardButtons(QMessageBox::Ok);

        msgBox.exec();
        return;

      } else {

        _timer.start();

      }
    }
  }

  void on_post_silence_spin_editingFinished(){
    set_editor_focus();
  }

  void on_filename_button_clicked(){
    QString filename = QFileDialog::getSaveFileName(this, 
                                                    QString("Select file")
                                                    //,QString()
                                                    //,info.fileName()
                                                    );
    filename_edit->setText(filename);
  }

};


extern int num_users_of_keyboard;

static Soundfilesaver_widget *widget=NULL;

extern "C"{
  void SOUNDFILESAVERGUI_open(void){

#if FULL_VERSION==0

    GFX_Message("Soundfile export is only available to subscribers.<p>"
                "Subscribe <a href=\"http://users.notam02.no/~kjetism/radium/download.php\">here</a>.");

#else // FULL_VERSION==0
    if(widget==NULL)
      widget = new Soundfilesaver_widget(NULL);

#if 0
    widget->show();
#else
    num_users_of_keyboard++;
    widget->exec();
    num_users_of_keyboard--;
#endif

#endif // FULL_VERSION==0
  }

  void SOUNDFILESAVERGUI_stop(const char *message){
    widget->_timer.async_message = strdup(message);
  }
}

#include "mQt_soundfilesaver_widget_callbacks.cpp"


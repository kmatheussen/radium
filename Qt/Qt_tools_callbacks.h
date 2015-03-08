#include <Python.h>

#include <stdio.h>
#include <sndfile.h>
#include <unistd.h>

#include <QMessageBox>
#include <QFileDialog>
#include <FocusSniffers.h>

#include "../common/nsmtracker.h"
#include "../common/OS_visual_input.h"
#include "../OpenGL/Widget_proc.h"

extern "C" {
#include "../api/radium_proc.h"
}

#include "Qt_MyQButton.h"

#include "Qt_tools.h"



namespace{

class Tools : public QDialog, public Ui::Tools {
  Q_OBJECT

 public:
  bool _initing;

 Tools(QWidget *parent=NULL)
   : QDialog(parent, "Tools")
  {
    _initing = true;

    setupUi(this);

    updateWidgets();

    _initing = false;
  }

  void updateWidgets(){
  }

public slots:

  void on_buttonBox_clicked(QAbstractButton * button){    
    if (button->text() == QString("Close")){
      printf("close\n");
      this->hide();
    } else
      RError("Unknown button \"%s\"\n",button->text().toUtf8().constData());
  }

  void on_make_track_monophonic_button_clicked(){
    makeTrackMonophonic(-1,-1,-1);
  }

  void on_split_track_button_clicked(){
    splitTrackIntoMonophonicTracks(-1,-1,-1);
  }

  void on_quantitize_track_clicked(){
    printf("track\n");
    quantitizeTrack(-1);
  }

  void on_quantitize_block_clicked(){
    printf("block\n");
    quantitizeBlock(-1);
  }

  void on_quantitize_range_clicked(){
    printf("range\n");
    quantitizeRange(-1);
  }

  void on_quantization_value_editingFinished(){
    printf("qu\n");

    //struct Tracker_Windows *window = root->song->tracker_windows;
    //struct WBlocks *wblock = window->wblock;

    Rational rational = create_rational_from_string(quantization_value->text());
    quantization_value->pushValuesToRoot(rational);

    updateWidgets();

    set_editor_focus();

    /*
    GL_lock();{
      quantitize_track->setFocus();
    }GL_unlock();
    */

  }

};

}

//#include "mQt_tools_callbacks.cpp"

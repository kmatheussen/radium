#include "../common/includepython.h"

#include <stdio.h>
#include <sndfile.h>
#include <unistd.h>

#include <QMessageBox>
#include <QFileDialog>
#include <FocusSniffers.h>

#include "../common/nsmtracker.h"
#include "../common/OS_visual_input.h"
#include "../OpenGL/Widget_proc.h"

#include "../api/api_proc.h"

#include "Qt_MyQButton.h"

#include "Qt_pianorollheader.h"

extern EditorWidget *g_editor;

namespace{

class Pianorollheader : public QWidget, public Ui::Pianorollheader {
  Q_OBJECT

 public:
  bool _initing;
  int blocknum;
  int tracknum;
  
  Pianorollheader(QWidget *parent=NULL)
    : QWidget(parent, "Pianorollheader")
    , blocknum(-1)
    , tracknum(-1)
  {
    _initing = true;

    setupUi(this);

    _initing = false;

    adjustSize();
  }

  void paintEvent ( QPaintEvent * ev ){
    QPainter p(this);
    p.eraseRect(rect());
  }
  
  void updateWidgets(){
    int lowKey = getPianorollLowKey(tracknum, blocknum, -1);
    int highKey = getPianorollHighKey(tracknum, blocknum, -1);
    lowkey->setText(getNoteName3(lowKey));
    highkey->setText(getNoteName3(highKey));
  }

public slots:

  void on_autorange_clicked(){
    printf("Hepp hepp\n");
    //update();
    //autorange->setDown(false);
    //autorange->update();

    int minkey = getLowestKey(tracknum, blocknum, -1);
    int maxkey = getHighestKey(tracknum, blocknum, -1);

    if (minkey==-1)
      return;    
    
    do{
      if (minkey>0)
        minkey--;
      if (maxkey<127)
        maxkey++;
    }while((maxkey - minkey) < 5);

    setPianorollLowKey(minkey,tracknum,blocknum,-1);
    setPianorollHighKey(maxkey,tracknum,blocknum,-1);

    updateWidgets();
    g_editor->updateEditor();
  }

  void on_lowkey_editingFinished(){
    printf("minkey\n");

    if (blocknum!=-1 && tracknum!=-1) {
      
      int keynum = getNoteNameValue((char*)lowkey->text().toUtf8().constData());
      if (keynum!=-1)
        setPianorollLowKey(keynum,tracknum,blocknum,-1);

      updateWidgets();

      //struct Tracker_Windows *window = root->song->tracker_windows;
      //window->must_redraw = true;
      //DrawUpTrackerWindow(window);

      g_editor->updateEditor();
    }    

    set_editor_focus();
  }

  void on_highkey_editingFinished(){
    printf("maxkey\n");

    if (blocknum!=-1 && tracknum!=-1) {
      
      int keynum = getNoteNameValue((char*)highkey->text().toUtf8().constData());
      if (keynum!=-1)
        setPianorollHighKey(keynum,tracknum,blocknum,-1);

      updateWidgets();

      //struct Tracker_Windows *window = root->song->tracker_windows;
      //DrawUpTrackerWindow(window);
      //window->must_redraw = true;

      g_editor->updateEditor();
    }

    set_editor_focus();
  }
};

}

//#include "mQt_pianorollheader_callbacks.cpp"

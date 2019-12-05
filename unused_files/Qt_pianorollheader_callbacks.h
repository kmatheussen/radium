
#include <inttypes.h>


#include <stdio.h>
#include <unistd.h>


#include "../common/nsmtracker.h"

#include "EditorWidget.h"
#include "FocusSniffers.h"

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

  int64_t _vap;
  
  Pianorollheader(QWidget *parent=NULL)
    : QWidget(parent)
    , blocknum(-1)
    , tracknum(-1)
  {
    _initing = true;

    setupUi(this);

    _initing = false;

    autorange->y1_border = 0;
    autorange->y2_border = 0;

    /*
    _vap = gui_verticalAudioMeter(0,-1);
    gui_add(API_get_gui_from_existing_widget(g_editor),
            _vap,
            0,0,100,100);
    */
    
    adjustSize();
  }

  // TODO: Why is this necessary?
  void paintEvent ( QPaintEvent * ev ) override {
    TRACK_PAINT();
    
    QPainter p(this);
    p.eraseRect(rect());
  }
  
  void updateWidgets(){
    int lowKey = getPianorollLowKey(tracknum, blocknum, -1);
    int highKey = getPianorollHighKey(tracknum, blocknum, -1);
    lowkey->setText(getNoteName3(lowKey));
    highkey->setText(getNoteName3(highKey-1));
  }

public slots:

  void on_autorange_clicked(){
    if (_initing)
      return;
    
    printf("Hepp hepp\n");
    //update();
    //autorange->setDown(false);
    //autorange->update();

    int minkey = floorf(getLowestKey(tracknum, blocknum, -1));
    int maxkey = floorf(getHighestKey(tracknum, blocknum, -1));

    if (minkey==-1)
      return;    
    
    do{
      if (minkey>0)
        minkey--;
      if (maxkey<127)
        maxkey = maxkey + 2;
    }while((maxkey - minkey) < 5);

    setPianorollLowKey(minkey,tracknum,blocknum,-1);
    setPianorollHighKey(maxkey,tracknum,blocknum,-1);

    updateWidgets();
    g_editor->updateEditor();
  }

  void on_lowkey_editingFinished(){
    if (_initing)
      return;
    
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
    if (_initing)
      return;

    printf("maxkey\n");

    if (blocknum!=-1 && tracknum!=-1) {
      
      int keynum = getNoteNameValue((char*)highkey->text().toUtf8().constData());
      if (keynum!=-1)
        setPianorollHighKey(keynum+1,tracknum,blocknum,-1);

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

/* Copyright 2012- Kjetil S. Matheussen

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


#include "Qt_instruments_widget.h"

class Instruments_widget : public QWidget, public Ui::Instruments_widget{
  Q_OBJECT;

public:

  struct Timer : public QTimer{

    Instruments_widget *instruments_widget;

    // horror
    void timerEvent(QTimerEvent * e) override {

      if (g_is_starting_up)
        return;

      RETURN_IF_DATA_IS_INACCESSIBLE();

      //printf("instrwi: %p\n",instruments_widget);
      
      if (instruments_widget->isVisible()==false)
        return;
      
      instruments_widget->calledRegularlyByParent();
    }
    
    Timer(Instruments_widget *instruments_widget){
      this->instruments_widget = instruments_widget;
      setInterval(50); // 50 = 3 * 16.666 (each third frame on a 60hz screen)
    }
  };

  Timer timer;

  Instruments_widget(QWidget *parent=NULL)
    : QWidget(parent)
    , timer(this)
  {
    setupUi(this);
    //tabs->tabBar()->hide();
    resize(width(),30);
    
    setAttribute(Qt::WA_Hover, true);
 
    timer.start();
  }

  void calledRegularlyByParent(void){
    Audio_instrument_widget *audio_widget = dynamic_cast<Audio_instrument_widget*>(g_instruments_widget->tabs->currentWidget());
    if (audio_widget != NULL){
      audio_widget->calledRegularlyByParent();
    }
  }

  void showEvent(QShowEvent *event) override {
    GFX_update_current_instrument_widget(); // Fix arrow colors, etc.
  }

  void enterEvent(QEnterEvent * event) override {
    instrument_t current = getCurrentInstrument();
    if (isLegalInstrument(current) && instrumentIsAudio(current))
      setCurrentInstrumentUnderMouse(current);

    //setMixerKeyboardFocus(true);
  }

public slots:

  /*
  void on_tabs_currentChanged(int index){
    Audio_instrument_widget *instrument = dynamic_cast<Audio_instrument_widget*>(tabs->widget(index));
    if (instrument!=NULL){
      if (instrument->_sample_requester_widget != NULL)
        instrument->_sample_requester_widget->update_file_list(false, true);
    }
  }
  */
  
#if 0
  void on_tabs_selected( const QString &tabname)
  {
    printf("on tabs selected \"%s\"\n",tabname.ascii());
    
    if(strlen(tabname)==0) // During startup, this happens, for some reason.
      return;
    
    tab_selected();

  }
#endif

};

#pragma once
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


#include <QPointer>
#include <QVector>

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

  // Minimum heights of the instrument pages are dropped while the widget is
  // hidden, so a hidden page (for instance the Instrument tab when the
  // Sequencer tab is selected) does not prevent the lower tabs from being
  // minimized. QStackedLayout includes the minimum size of every page, not
  // only the current one, and the audio instrument widget raises its minimum
  // height to fit its scroll area (much more so while the LLM terminal is
  // shown).
  bool _minimum_heights_are_cleared = false;
  QVector<QPointer<QWidget>> _cleared_pages;
  QVector<int> _cleared_page_minimum_heights;
  int _cleared_parent_minimum_height = -1;

  void clear_page_minimum_heights(void){
    if (_minimum_heights_are_cleared)
      return;

    _cleared_pages.clear();
    _cleared_page_minimum_heights.clear();

    if (tabs != NULL){
      for (int i = 0 ; i < tabs->count() ; i++){
        QWidget *page = tabs->widget(i);
        if (page != NULL){
          _cleared_pages.push_back(page);
          _cleared_page_minimum_heights.push_back(page->minimumHeight());
          // Use 1 rather than 0: 0 means "no explicit minimum height" and
          // lets Qt fall back to the page's (large) minimumSizeHint again.
          page->setMinimumHeight(1);
        }
      }
      tabs->setMinimumHeight(1);
    }

    setMinimumHeight(1);

    // The tab page (a KeyboardFocusFrame) is the widget whose minimum size
    // the lower tab widget actually uses. Drop its minimum height too.
    QWidget *parent = parentWidget();
    if (parent != NULL){
      _cleared_parent_minimum_height = parent->minimumHeight();
      parent->setMinimumHeight(1);
      parent->updateGeometry();
    }

    updateGeometry();
    _minimum_heights_are_cleared = true;
  }

  void restore_page_minimum_heights(void){
    if (!_minimum_heights_are_cleared)
      return;

    if (_cleared_pages.size() != _cleared_page_minimum_heights.size())
      return;

    for (int i = 0 ; i < _cleared_pages.size() ; i++){
      QWidget *page = _cleared_pages[i].data();
      // Only restore if the height is still cleared. If someone changed the
      // minimum height while the widget was hidden (for instance the LLM
      // checkbox resetting it), that newer value is the correct one.
      if (page != NULL && page->minimumHeight() <= 1)
        page->setMinimumHeight(_cleared_page_minimum_heights[i]);
    }

    QWidget *parent = parentWidget();
    if (parent != NULL && _cleared_parent_minimum_height >= 0){
      if (parent->minimumHeight() <= 1)
        parent->setMinimumHeight(_cleared_parent_minimum_height);
      parent->updateGeometry();
    }
    _cleared_parent_minimum_height = -1;

    _cleared_pages.clear();
    _cleared_page_minimum_heights.clear();
    _minimum_heights_are_cleared = false;
    updateGeometry();
  }

  void hideEvent(QHideEvent *event) override {
    clear_page_minimum_heights();
    QWidget::hideEvent(event);
  }

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

  void call_regularly_for_painters(void){
    Audio_instrument_widget *audio_widget = dynamic_cast<Audio_instrument_widget*>(g_instruments_widget->tabs->currentWidget());
    if (audio_widget != NULL){
      audio_widget->call_regularly_for_painters();
    }
  }

  void showEvent(QShowEvent *event) override {
    restore_page_minimum_heights();
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

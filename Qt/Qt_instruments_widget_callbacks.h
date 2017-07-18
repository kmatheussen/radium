#include "Qt_instruments_widget.h"

class Instruments_widget : public QWidget, public Ui::Instruments_widget{
  Q_OBJECT;

public:

  struct Timer : public QTimer{

    Instruments_widget *instruments_widget;

    // horror
    void timerEvent(QTimerEvent * e){

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

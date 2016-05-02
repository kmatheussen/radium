#include "Qt_instruments_widget.h"

class Instruments_widget : public QWidget, public Ui::Instruments_widget{
  Q_OBJECT;

public:

  struct Timer : public QTimer{

    Instruments_widget *instruments_widget;

    // horror
    void timerEvent(QTimerEvent * e){

      if (ATOMIC_GET(is_starting_up))
        return;

      //printf("instrwi: %p\n",instruments_widget);
      
      if (instruments_widget->isVisible()==false)
        return;
      
      instruments_widget->calledRegularlyByParent();
    }
    
    Timer(Instruments_widget *instruments_widget){
      this->instruments_widget = instruments_widget;
      setInterval(50); // 3 * 16.666 (each third frame on a 60hz screen)
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
    Audio_instrument_widget *audio_widget = dynamic_cast<Audio_instrument_widget*>(instruments_widget->tabs->currentWidget());
    if (audio_widget != NULL){
      audio_widget->calledRegularlyByParent();
    }
  }

public slots:
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

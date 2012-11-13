#include "Qt_instruments_widget.h"

class Instruments_widget : public QWidget, public Ui::Instruments_widget{
  Q_OBJECT;

public:
  Instruments_widget(QWidget *parent=NULL)
    : QWidget(parent)
  {
    setupUi(this);
    //tabs->tabBar()->hide();
    //resize(width(),30);
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

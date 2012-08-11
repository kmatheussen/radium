#include "../common/patch_proc.h"

#include "Qt_no_instrument_widget.h"

class No_instrument_widget : public QWidget, public Ui::No_instrument_widget{
  Q_OBJECT

 public:
 No_instrument_widget(QWidget *parent=NULL)
    : QWidget(parent)
  {
    setupUi(this);
  }

public slots:
  void on_create_instrument_clicked()
  {
    printf("Got it\n");
    struct Patch *patch = NewPatchCurrPos();
    addInstrument(patch);
    Instrument_widget *instrument = get_instrument_widget(patch);
    instruments_widget->tabs->showPage(instrument);
  }

};

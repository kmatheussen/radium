#pragma once

#include <inttypes.h>

#include <stdio.h>
#include <unistd.h>

#include <QCheckBox>
#include <QTimer>
#include <QDialogButtonBox>
#include <QPainter>
#include <QLabel>
#include <QSlider>
#include <QGuiApplication>
#include <QScreen>

#include "../common/nsmtracker.h"

#include <FocusSniffers.h>

#include "../common/OS_visual_input.h"
#include "../common/Vector.hpp"
#include "../common/sequencer_proc.h"

#include "../OpenGL/Widget_proc.h"
#include "../midi/midi_i_input_proc.h"
#include "../embedded_scheme/s7extra_proc.h"

#include "../api/api_gui_proc.h"
#include "../api/api_proc.h"

#include "Qt_MyQButton.h"

#include "ScrollArea.hpp"

#include "Qt_tools.h"


namespace{

struct MidiLearnItem : public QWidget {
  Q_OBJECT;
  
  QCheckBox *enabled;
  QPushButton *delete_button;
  QCheckBox *incremental;
  QLabel *speed_label;
  QSlider *speed_slider;
  QLabel *speed_value_label;

public:

  radium::MidiLearn *midi_learn;

  QLabel *source,*dest;
  
  MidiLearnItem(radium::MidiLearn *midi_learn)
    : midi_learn(midi_learn)
  {
    auto *layout = new QHBoxLayout(this);

    enabled = new QCheckBox("Enabled", this);
    enabled->setChecked(true);
    layout->addWidget(enabled, 0);
    connect(enabled, SIGNAL(toggled(bool)), this, SLOT(enabled_toggled(bool)));
    
    delete_button = new QPushButton("Delete", this);
    layout->addWidget(delete_button, 0);
    connect(delete_button, SIGNAL(released()), this, SLOT(delete_released()));

    incremental = new QCheckBox("Incremental mode", this);
    incremental->setChecked(ATOMIC_GET(midi_learn->is_incremental));
    layout->addWidget(incremental, 0);
    connect(incremental, SIGNAL(toggled(bool)), this, SLOT(incremental_toggled(bool)));

    speed_label = new QLabel("Speed:", this);
    layout->addWidget(speed_label, 0);

    speed_slider = new QSlider(Qt::Horizontal, this);
    speed_slider->setRange(-100, 100); // -10x to +10x, in 0.1 steps
    speed_slider->setSingleStep(1);
    speed_slider->setPageStep(10);
    speed_slider->setTickPosition(QSlider::TicksBelow);
    speed_slider->setTickInterval(50);
    speed_slider->setMinimumWidth(80);
    speed_slider->setValue(qBound(-100, qRound(ATOMIC_DOUBLE_GET(midi_learn->incremental_speed) * 10.0), 100));
    layout->addWidget(speed_slider, 0);
    connect(speed_slider, SIGNAL(valueChanged(int)), this, SLOT(speed_changed(int)));

    speed_value_label = new QLabel("", this);
    speed_value_label->setMinimumWidth(50);
    layout->addWidget(speed_value_label, 0);

    update_speed_label();
    update_incremental_controls_visibility(ATOMIC_GET(midi_learn->is_incremental));

    source = new QLabel("", this);
    dest = new QLabel("", this);
    
    layout->addWidget(source, 0);
    layout->addWidget(dest, 1);

    update_gui();
    
    setLayout(layout);  
  }

  void update_gui(){
    source->setText(midi_learn->get_source_info() + " => ");
    dest->setText(midi_learn->get_dest_info());
    incremental->setChecked(ATOMIC_GET(midi_learn->is_incremental));
  }

  void update_speed_label(void){
    const double speed = speed_slider->value() / 10.0;
    QString text = QString::number(speed, 'f', 1) + "x";
    if (speed > 0.0)
      text = "+" + text;
    speed_value_label->setText(text);
  }

  void update_incremental_controls_visibility(bool is_incremental){
    speed_label->setVisible(is_incremental);
    speed_slider->setVisible(is_incremental);
    speed_value_label->setVisible(is_incremental);
    emit incremental_controls_visibility_changed();
  }

  void update_midi_learn_gui_things(void){
    ATOMIC_SET(PATCH_get_current()->widget_needs_to_be_updated, true);
    SEQUENCER_update(SEQUPDATE_HEADERS);
  }
  
signals:
  void incremental_controls_visibility_changed(void);

public slots:

  void enabled_toggled(bool val){
    ATOMIC_SET(midi_learn->is_enabled, val);
    update_midi_learn_gui_things();
  }
  
  void delete_released(){
    midi_learn->delete_me();
  }

  void incremental_toggled(bool val){
    ATOMIC_SET(midi_learn->is_incremental, val);
    update_incremental_controls_visibility(val);
    update_midi_learn_gui_things();
  }

  void speed_changed(int val){
    ATOMIC_DOUBLE_SET(midi_learn->incremental_speed, val / 10.0);
    update_speed_label();
  }
  
};
 
class MidiLearnPrefs : public RememberGeometryQDialog {
  Q_OBJECT;

  QLayout *main_layout;

  radium::VerticalScroll *vertical_scroll;

  radium::Vector<MidiLearnItem*> items;

  struct MyTimer : public QTimer {
    MidiLearnPrefs *parent;
    MyTimer(MidiLearnPrefs *parent)
      : parent(parent)
    {      
    }

    void timerEvent(QTimerEvent * e) override {
      RETURN_IF_DATA_IS_INACCESSIBLE();

      if (parent->isVisible()){
        for(auto *item : parent->items)
          item->update_gui();
      }
    }
  };

  MyTimer timer;

  void ensure_content_is_visible(void){
    QScreen *screen = g_main_window != NULL ? g_main_window->screen() : QGuiApplication::primaryScreen();
    if (screen == NULL)
      return;

    const QRect available = screen->availableGeometry();
    const int max_width = R_MAX(400, available.width() - 20);
    const int max_height = R_MAX(200, available.height() - 20);

    // The scroll area's default width hint is too small to show the
    // "source => destination" text for an item. Calculate the width needed by
    // the widest item instead.
    int wanted_width = 0;
    for (auto *item : items)
      wanted_width = R_MAX(wanted_width, item->sizeHint().width());

    if (wanted_width > 0) {
      wanted_width += vertical_scroll->verticalScrollBar()->sizeHint().width();
      wanted_width += main_layout->contentsMargins().left() + main_layout->contentsMargins().right();
      wanted_width += 4; // Scroll area border.
    }

    wanted_width = R_MIN(wanted_width, max_width);

    // Grow the width first, so the height calculation doesn't include a
    // horizontal scrollbar which will not be needed.
    if (wanted_width > width())
      resize(wanted_width, height());

    main_layout->activate();

    QWidget *contents = vertical_scroll->getWidget();
    if (contents->layout() != NULL)
      contents->layout()->activate();

    // QScrollArea caches its size hint, so adding or removing items doesn't
    // necessarily update the height returned by sizeHint(). Set the scroll
    // area's minimum height to the height of all items while calculating the
    // dialog size, then reset it so the scroll bar can still be used when
    // there are too many items to fit on screen.
    const int old_minimum_height = vertical_scroll->minimumHeight();
    const int content_height = contents->layout() != NULL ? contents->layout()->sizeHint().height() : contents->sizeHint().height();
    vertical_scroll->setMinimumHeight(R_MIN(content_height + 4, max_height));
    main_layout->activate();

    QSize wanted = sizeHint();

    vertical_scroll->setMinimumHeight(old_minimum_height);

    wanted.setWidth(R_MAX(wanted.width(), wanted_width));
    wanted.setHeight(R_MAX(wanted.height(), height()));
    wanted.setWidth(R_MIN(wanted.width(), max_width));
    wanted.setHeight(R_MIN(wanted.height(), max_height));

    // Never shrink a window the user has made larger.
    wanted = QSize(R_MAX(wanted.width(), width()), R_MAX(wanted.height(), height()));

    // If the window is (or becomes) bigger than the available screen area, move
    // it so the content stays visible.
    const QPoint pos = this->pos();
    int x = R_MAX(pos.x(), available.left());
    int y = R_MAX(pos.y(), available.top());
    if (x + wanted.width() - 1 > available.right())
      x = R_MAX(available.left(), available.right() - wanted.width() + 1);
    if (y + wanted.height() - 1 > available.bottom())
      y = R_MAX(available.top(), available.bottom() - wanted.height() + 1);

    if (wanted != size())
      resize(wanted);

    if (x != pos.x() || y != pos.y())
      move(x, y);
  }
  
public:
  MidiLearnPrefs(QWidget *parent=NULL)
    : RememberGeometryQDialog(parent, radium::NOT_MODAL)
    , timer(this)
  {
    setWindowTitle("MIDI Learn Configuration");
    
    main_layout = new QGridLayout(this);
    setLayout(main_layout);

    vertical_scroll = new radium::VerticalScroll(this);
    main_layout->addWidget(vertical_scroll);

    QDialogButtonBox *button_box = new QDialogButtonBox(this);
    button_box->setStandardButtons(QDialogButtonBox::Close);
    main_layout->addWidget(button_box);

    connect(button_box, SIGNAL(clicked(QAbstractButton*)), this, SLOT(on_button_box_clicked(QAbstractButton*)));
        
    timer.setInterval(100);
    timer.start();
  }

  void setVisible(bool visible) override {
    RememberGeometryQDialog::setVisible(visible);

    if (visible) {
      ensure_content_is_visible();

      if (remember_geometry.has_stored_geometry == false)
        moveWindowToCentre(this);
    }
  }

  void add(radium::MidiLearn *midi_learn){
    auto *gakk = new MidiLearnItem(midi_learn);
    connect(gakk, SIGNAL(incremental_controls_visibility_changed()), this, SLOT(on_incremental_controls_visibility_changed()));
    items.push_back(gakk);
    vertical_scroll->addWidget(gakk);
    gakk->show(); // The new item is hidden until shown, and hidden items are not included when calculating the layout size.

    if (isVisible())
      ensure_content_is_visible();
  }
  
  void remove(radium::MidiLearn *midi_learn){
    MidiLearnItem *item=NULL;
    for (auto *item2 : items)
      if (item2->midi_learn==midi_learn){
        item = item2;
        break;
      }
    
    R_ASSERT_RETURN_IF_FALSE(item!=NULL);

    vertical_scroll->removeWidget(item);
    items.remove(item);

    delete item;

    if (isVisible())
      ensure_content_is_visible();
  }

public slots:
  void on_button_box_clicked(QAbstractButton *button){
    hide();
  }

  void on_incremental_controls_visibility_changed(void){
    if (isVisible())
      ensure_content_is_visible();
  }
};

 
class Tools : public RememberGeometryQDialog, public Ui::Tools {
  Q_OBJECT

 public:
  bool _initing;

  Tools(QWidget *parent=NULL)
    : RememberGeometryQDialog(parent, radium::NOT_MODAL)
  {
    _initing = true;

    setupUi(this);

    dyn_t quant_gui_dyn = s7extra_callFunc2_dyn_void("FROM_C-create-quantitize-gui");
    if (quant_gui_dyn.type==INT_TYPE){
      
      int64_t quant_gui_num = quant_gui_dyn.int_number;
      if (quant_gui_num > 0){
        
        QWidget *quant_gui = API_gui_get_widget(quant_gui_num);
        if (quant_gui != NULL)
          quantization_layout->addWidget(quant_gui);
        
      }
      
    }
    
    updateWidgets();
 
    _initing = false;

    adjustSize();
  }

  void showEvent(QShowEvent *event_) override {
    updateWidgets(); // background color is set back to high_background_color when changing colors in the preferences. This way at least the lighter colors come back when reopening the window.
    RememberGeometryQDialog::showEvent(event_);
  }
  
  void updateWidgets(){
    QPalette pal = palette();
    pal.setColor(QPalette::Window, get_qcolor(LOW_BACKGROUND_COLOR_NUM)); // Default HIGH_BACKGROUND_COLOR is very dark.
    setPalette(pal);
  }

public slots:

  void on_closeButton_clicked(){
    this->hide();
  }

  void on_make_track_monophonic_button_clicked(){
    makeTrackMonophonic(-1,-1,-1);
  }

  void on_split_track_button_clicked(){
    splitTrackIntoMonophonicTracks(-1,-1,-1);
  }
};

}

//#include "mQt_tools_callbacks.cpp"

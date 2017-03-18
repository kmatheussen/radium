#include "../common/includepython.h"

#include <stdio.h>
#include <unistd.h>

#pragma GCC diagnostic ignored "-Wshadow"
#pragma GCC diagnostic push

#include <QScrollArea>
#include <QCheckBox>
#include <QTimer>
#include <QDialogButtonBox>

#pragma GCC diagnostic pop


#include "../common/nsmtracker.h"

#include <FocusSniffers.h>

#include "../common/OS_visual_input.h"
#include "../common/Vector.hpp"
#include "../OpenGL/Widget_proc.h"
#include "../midi/midi_i_input_proc.h"

#include "../api/api_proc.h"

#include "Qt_MyQButton.h"

#include "Qt_tools.h"

namespace{

struct MidiLearnItem : public QWidget {
  Q_OBJECT;
  
  QCheckBox *enabled;
  QPushButton *delete_button;

public:

  MidiLearn *midi_learn;

  QLabel *source,*dest;
  
  MidiLearnItem(MidiLearn *midi_learn)
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
  }

public slots:

  void enabled_toggled(bool val){
    ATOMIC_SET(midi_learn->is_enabled, val);
    if (g_currpatch != NULL)
      ATOMIC_SET(g_currpatch->widget_needs_to_be_updated, true);
  }
  
  void delete_released(){
    midi_learn->delete_me();
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

    void timerEvent(QTimerEvent * e){
      if (parent->isVisible()){
        for(auto *item : parent->items)
          item->update_gui();
      }
    }
  };

  MyTimer timer;
  
public:
  MidiLearnPrefs(QWidget *parent=NULL)
    : RememberGeometryQDialog(parent)
    , timer(this)
  {
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

  void add(MidiLearn *midi_learn){
    auto *gakk = new MidiLearnItem(midi_learn);
    items.push_back(gakk);
    vertical_scroll->addWidget(gakk);
  }
  
  void remove(MidiLearn *midi_learn){
    MidiLearnItem *item=NULL;
    for (auto item2 : items)
      if (item2->midi_learn==midi_learn){
        item = item2;
        break;
      }
    
    R_ASSERT_RETURN_IF_FALSE(item!=NULL);

    vertical_scroll->removeWidget(item);
    items.remove(item);

    delete item;
  }

public slots:
  void on_button_box_clicked(QAbstractButton *button){
    hide();
  }
};

 
class Tools : public RememberGeometryQDialog, public Ui::Tools {
  Q_OBJECT

 public:
  bool _initing;

  Tools(QWidget *parent=NULL)
   : RememberGeometryQDialog(parent)
  {
    _initing = true;

    setupUi(this);

    updateWidgets();

    _initing = false;

    adjustSize();
  }

  void updateWidgets(){
    switch(root->quantitize_options.type){
    case 1:
      type1->setChecked(true);
      break;
    case 2:
      type2->setChecked(true);
      break;
    case 3:
      type3->setChecked(true);
      break;
    case 4:
      type4->setChecked(true);
      break;
    case 5:
      type5->setChecked(true);
      break;
    default:
      RError("Unknwon quantitize option %d",root->quantitize_options.type);
    }

    if (root->quantitize_options.quantitize_start)
      quant_start->setChecked(true);
    if (root->quantitize_options.quantitize_end)
      quant_end->setChecked(true);
    if (root->quantitize_options.keep_note_length)
      keep_length->setChecked(true);

    Rational ratio((int)root->quantitize_options.quant.numerator, (int)root->quantitize_options.quant.denominator);

    quantization_value->setText(ratio.toString());
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

  void on_quant_start_toggled(bool val){
    printf("quant start: %d\n",val);
    root->quantitize_options.quantitize_start = val;    
  }
  
  void on_quant_end_toggled(bool val){
    printf("quant end: %d\n",val);
    root->quantitize_options.quantitize_end = val;    
  }
  
  void on_keep_length_toggled(bool val){
    printf("keep length: %d\n",val);
    root->quantitize_options.keep_note_length = val;    
  }

  void on_type1_toggled(bool val){
    if (val)
      root->quantitize_options.type = 1;
  }
  
  void on_type2_toggled(bool val){
    if (val)
      root->quantitize_options.type = 2;
  }
  
  void on_type3_toggled(bool val){
    if (val)
      root->quantitize_options.type = 3;
  }
  
  void on_type4_toggled(bool val){
    if (val)
      root->quantitize_options.type = 4;
  }
  
  void on_type5_toggled(bool val){
    if (val)
      root->quantitize_options.type = 5;
  }
  
  void on_quantization_value_editingFinished(){
    printf("qu\n");

    Rational rational = create_rational_from_string(quantization_value->text());
    quantization_value->pushValuesToRoot(rational);

    updateWidgets();

    GL_lock();{
      quantization_value->clearFocus();
    }GL_unlock();

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

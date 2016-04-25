#include "../common/includepython.h"

#include <stdio.h>
#include <sndfile.h>
#include <unistd.h>

#include <QMessageBox>
#include <FocusSniffers.h>

#include "../common/nsmtracker.h"
#include "../common/OS_visual_input.h"
#include "../OpenGL/Widget_proc.h"

#include "../api/api_proc.h"

#include "Qt_MyQButton.h"

#include "Qt_tools.h"

namespace{

class Tools : public QDialog, public Ui::Tools {
  Q_OBJECT

 public:
  bool _initing;

  Tools(QWidget *parent=NULL)
   : QDialog(parent)
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

    Rational ratio(root->quantitize_options.quant.numerator, root->quantitize_options.quant.denominator);

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

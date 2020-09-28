/* Copyright 2013 Kjetil S. Matheussen

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

#include <stdio.h>

#include "Qt_compressor_widget.h"

#include <QFileDialog>
#include <QMessageBox>

#include "Qt_MyWidget.h"

#include "../audio/system_compressor_wrapper_proc.h"


#ifdef COMPILING_RADIUM
const int max_attack_release = 500;

// These two functions are not used in Radium. The start function could be used to create undo though. (Fix)
static void  set_compressor_automation_start(struct Patch *patch, int num){
}

static void  set_compressor_automation_end(struct Patch *patch, int num){
}

// I'm pretty sure it's not necessary to test for plugin!=NULL in these five functons.
// Normally, I'm 100% sure it's safe, but these functions can also be called from a timer.
//
static void set_compressor_parameter(struct Patch *patch, int num,float value){
  SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
  if(plugin!=NULL)
    PLUGIN_set_effect_value(plugin, 0, plugin->type->num_effects+EFFNUM_COMP_RATIO+num, value, STORE_VALUE, FX_single, EFFECT_FORMAT_NATIVE);
}

#else // COMPILING_RADIUM

static void set_compressor_parameter(struct Patch *patch, int num,float value){
  SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
  if(plugin!=NULL)
    COMPRESSOR_set_parameter(plugin->compressor,num,value);
}

#endif // #else COMPILING_RADIUM

static float get_compressor_parameter(struct Patch *patch, int num){
  SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
  if(plugin!=NULL)
    return COMPRESSOR_get_parameter(plugin->compressor,num);
  else
    return 0.0f;
}

static float get_graph_value(struct Patch *patch, int num){
  SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
  if(plugin!=NULL)
    return COMPRESSOR_get_graph_value(plugin->compressor,num);
  else
    return 0.0;
}


//#ifdef COMPILING_RADIUM
//namespace radium_comp{ // got an error trying to use anonymous namespace here.
  //#endif


#include "compressor_vertical_sliders.cpp"


static double OS_get_double_from_string_here(const char *s){
  QLocale::setDefault(QLocale::C);
  QString string(s);
  return string.toDouble();
}

static float read_float(FILE *file){
  char temp[512] = {0};

  if(fgets(temp,500,file)==NULL)
    fprintf(stderr,"Unable to read float\n");

  return OS_get_double_from_string_here(temp);
}

namespace{
class Compressor_widget : public QWidget, public Ui::Compressor_widget{
  Q_OBJECT

 public:
  bool initing;
  cvs::Comp *comp;
  radium::GcHolder<struct Patch> _patch;

 Compressor_widget(struct Patch *patch, QWidget *parent=NULL)
    : QWidget(parent)
    , _patch(patch)
  {
    initing = true;
    setupUi(this);

    QWidget *parent_for_comp = this;
    comp = new cvs::Comp(patch,parent_for_comp);

#ifdef COMPILING_RADIUM
    comp->w->setEnabled(false);
#else
    attack_slider->setEnabled(true);
    release_slider->setEnabled(true);
    enable_checkbox->setVisible(false);
#endif
    verticalLayout->insertWidget(1,comp->w);

#ifdef COMPILING_RADIUM
    // Enable MyQSlider and MyQCheckBox to take care of undo/redo.
    {
      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
      attack_slider->_patch.set(patch);
      attack_slider->_effect_num = plugin->type->num_effects+EFFNUM_COMP_ATTACK;
      release_slider->_patch.set(patch);
      release_slider->_effect_num = plugin->type->num_effects+EFFNUM_COMP_RELEASE;
      enable_checkbox->_patch.set(patch);
      enable_checkbox->_effect_num = plugin->type->num_effects+EFFNUM_COMP_ONOFF;
    }
#endif

    update_gui();
    initing = false;
  }

  void prepare_for_deletion(void){
    comp->prepare_for_deletion();
    attack_slider->prepare_for_deletion();
    release_slider->prepare_for_deletion();
  }
  
  ~Compressor_widget(){
    prepare_for_deletion();
    delete comp;  // <- 'comp' is not a QObect subclass (i.e a child widget of 'this'), and must therefore be deleted manually.
  }

  void calledRegularlyByParent(void){
    comp->calledRegularlyByParent();
    attack_slider->update();
  }
  
  float get_exp_value(double val, double max_val, double y1, double y2){
    return scale_double(exp(val/max_val),
                        exp(0.0),expf(1.0),
                        y1,y2
                        );
  }

  float get_exp_inverted_value(double y, double max_val, double y1, double y2){
    return max_val * log(scale_double(y,y1,y2,exp(0.0),exp(1.0)));
  }

  void update_gui(){
    comp->set_gui_parameters();
    attack_slider->setValue(get_exp_inverted_value(get_compressor_parameter(_patch.data(), COMP_EFF_ATTACK),1000,0,max_attack_release));
    release_slider->setValue(get_exp_inverted_value(get_compressor_parameter(_patch.data(), COMP_EFF_RELEASE),1000,0,max_attack_release));

    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    if (plugin==NULL) return;

    enable_checkbox->setChecked(ATOMIC_GET(plugin->comp.is_on));

    //paint_all=true;
    //updateBackgroundImage();
    update();
  }

  void load(QString filename){
    FILE *file = fopen(filename.toUtf8().constData(),"r");

    if(file==NULL){
      MyQMessageBox *msgBox = MyQMessageBox::create(false, g_main_window);
      msgBox->setAttribute(Qt::WA_DeleteOnClose);

      msgBox->setText("Could not open file \""+filename+"\".");
      msgBox->setStandardButtons(QMessageBox::Ok);
      msgBox->setDefaultButton(QMessageBox::Ok);
      
      safeShow(msgBox);
      return;
    }
    
    set_compressor_parameter(_patch.data(), COMP_EFF_RATIO,read_float(file)); // ratio
    set_compressor_parameter(_patch.data(), COMP_EFF_THRESHOLD,read_float(file)); // threshold
    set_compressor_parameter(_patch.data(), COMP_EFF_ATTACK,read_float(file)); // attack
    set_compressor_parameter(_patch.data(), COMP_EFF_RELEASE,read_float(file)); // release
    //set_compressor_parameter(INPUT_VOLUME,read_float(file)); // input volume (currently not used)
    read_float(file); // input volume (currently not used)
    set_compressor_parameter(_patch.data(), COMP_EFF_OUTPUT_VOLUME,read_float(file)); // output volume

    fclose(file);

    update_gui();
  }

public slots:

void on_enable_checkbox_toggled(bool val){
#ifdef COMPILING_RADIUM
  SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
  if (plugin==NULL) return;
  
  PLUGIN_set_effect_value(plugin, 0, plugin->type->num_effects+EFFNUM_COMP_ONOFF, val==true?1.0:0.0, STORE_VALUE, FX_single, EFFECT_FORMAT_SCALED);

  attack_slider->setEnabled(val);
  release_slider->setEnabled(val);
  comp->background_image_must_be_updated=true;
  comp->w->setEnabled(val);
#endif
}

  void on_attack_slider_valueChanged(int val){
    float attack = get_exp_value(val,1000,0,max_attack_release);
    set_compressor_parameter(_patch.data(), COMP_EFF_ATTACK,attack);
    char temp[512];
    sprintf(temp,"Attack: %.2fms",attack);
    SLIDERPAINTER_set_string(attack_slider->_painter, temp);
  }

  void on_release_slider_valueChanged(int val){
    float release = get_exp_value(val,1000,0,max_attack_release);
    set_compressor_parameter(_patch.data(), COMP_EFF_RELEASE,release);
    char temp[512];
    sprintf(temp,"Release: %.2fms",release);
    SLIDERPAINTER_set_string(release_slider->_painter, temp);
  }

  void on_bypass_toggled(bool val){
    printf("bypass: %d\n",(int)val);
    set_compressor_parameter(_patch.data(), COMP_EFF_BYPASS,val==true?1.0f:0.0f);
  }

  void on_load_button_clicked(void){
    printf("load pressed\n");

    QString filename;

    GL_lock();{ // GL_lock is needed when using intel gfx driver to avoid crash caused by opening two opengl contexts simultaneously from two threads.
      filename = QFileDialog::getOpenFileName(this, "Load Effect configuration", "", "Radium Compressor Configuration (*.rcc)");
    }GL_unlock();

    if(filename=="")
      return;

    load(filename);
  }

  void on_save_button_clicked(void){
    printf("save pressed\n");

    QString filename;

    GL_lock();{ // GL_lock is needed when using intel gfx driver to avoid crash caused by opening two opengl contexts simultaneously from two threads.
      filename = QFileDialog::getSaveFileName(this, "Save Effect configuration", "", "Radium Compressor Configuration (*.rcc)");
    }GL_unlock();

    if(filename=="")
      return;

    FILE *file = fopen(filename.toUtf8().constData(),"w");

    if(file==NULL){
      MyQMessageBox *msgBox(MyQMessageBox::create(false, g_main_window));
      msgBox->setAttribute(Qt::WA_DeleteOnClose);

      msgBox->setText("Could not save file.");
      msgBox->setStandardButtons(QMessageBox::Ok);
      msgBox->setDefaultButton(QMessageBox::Ok);
      safeShow(msgBox);
      
      return;
    }

    fprintf(file,"%f\n%f\n%f\n%f\n%f\n%f\n",
            get_compressor_parameter(_patch.data(), COMP_EFF_RATIO),
            get_compressor_parameter(_patch.data(), COMP_EFF_THRESHOLD),
            get_compressor_parameter(_patch.data(), COMP_EFF_ATTACK),
            get_compressor_parameter(_patch.data(), COMP_EFF_RELEASE),
            //get_compressor_parameter(INPUT_VOLUME),
            0.0f, // input volume (in dB)
            get_compressor_parameter(_patch.data(), COMP_EFF_OUTPUT_VOLUME)
            );

    fclose(file);
  }
};
}

  //#ifdef COMPILING_RADIUM
  //} // radium_comp namespace
  //#endif


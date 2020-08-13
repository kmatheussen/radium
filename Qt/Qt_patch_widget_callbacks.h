/* Copyright 2012 Kjetil S. Matheussen

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


#include <QToolTip>


#include "../common/patch_proc.h"
#include "../common/undo_patchname_proc.h"

#include "Qt_patch_widget.h"

#ifdef USE_QT5
#include <QStyleFactory>
#else
#include <QCleanlooksStyle>
#endif

class Patch_widget : public QWidget, public GL_PauseCaller, public Ui::Patch_widget{
  Q_OBJECT;

 public:
  bool _called_from_update = false;
  bool initing;

  radium::GcHolder<struct Patch> _patch;
  struct PatchVoice *_voices;

  bool _has_inited = false;
  
#ifndef USE_QT5
  QCleanlooksStyle _cleanlooksStyle;
#endif

  void setStyleRec(QWidget *widget, QStyle *style){
    widget->setStyle(style);
    
    const QList<QObject*> list = widget->children();

    for(auto *element : list){

#if USE_QT5
#if FOR_WINDOWS // Why can't qt make widgets look the same on all platform?
      QHBoxLayout *h = dynamic_cast<QHBoxLayout*>(element);
      if (h != NULL)
        h->setSpacing(0);
#endif
#endif
      
      QWidget *widget = dynamic_cast<QWidget*>(element);
      if(widget!=NULL)
        setStyleRec(widget, style);
    }
  }

  bool _show_pan = false;
  
  Patch_widget(QWidget *parent, struct Patch *patch)
    : QWidget(parent)
    , _patch(patch)
    , _voices(&patch->voices[0])
  {    
    initing = true;
    /*
    GL_lock(); {
      GL_pause_gl_thread_a_short_while(); // might fix [1] crash
    }GL_unlock();
    */

    setupUi(this);

#ifdef USE_QT5
#if FOR_WINDOWS // %!$%!#$
    main_vertical_layout->setSpacing(3);
#endif
#endif

    through_onoff->setFont(QApplication::font()); // why?
    
    if (_patch->instrument==get_audio_instrument()){
      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;

      if (plugin != NULL) {
        
        if (!strcmp(plugin->type->type_name, "Sample Player")) {
          
          _show_pan = true;
          
        } else if (!strcmp(plugin->type->type_name, "Pd")) {
          
        _show_pan = true;
        
        }
        
      } else {
        
        R_ASSERT_NON_RELEASE(false);
        
      }
    }
    
#ifdef USE_QT5
    
    static QStyle *style = NULL;

#if 0 //!defined(FOR_MACOSX) // Didn't bother compiling it up, and it looks okay without it. I guess cleanlooks style is not needed in Qt 5.10.
    static bool have_failed = false;
    style = QStyleFactory::create("cleanlooks");
    
    //GFX_Message(NULL, QStyleFactory::keys().join(", ").toUtf8().constData());
    
    if (have_failed == false){
      if (style==NULL){
        GFX_addMessage("Unable to load cleanlooks style");
        have_failed = true;
      }else
        setStyle(style);
    }
#endif
    
#else
    QStyle *style = NULL; //&_cleanlooksStyle;
#endif
    
    if (style != NULL){
      setStyleRec(this, style);
#if 0
      get_t(i)->setStyle(style);
      get_v(i)->setStyle(style);
      get_s(i)->setStyle(style);
      get_l(i)->setStyle(style);
      get_c(i)->setStyle(style);
#endif
    }

#ifdef USE_QT5
    static QStyle *fusion_style = QStyleFactory::create("fusion");
    if (fusion_style != NULL)
      name_widget->setStyle(fusion_style);    
#endif

    name_widget->setFont(QApplication::font()); // why?
        
    for(int i=0;i<NUM_PATCH_VOICES;i++){
      
      set_fixed_widget_width(get_o(i), "xx");    // onoff
      set_fixed_widget_width(get_t(i), "-24.00 "); // transpose
      set_fixed_widget_width(get_v(i), "-35 "); // volume
      set_fixed_widget_width(get_s(i), "999.9 "); // start
      set_fixed_widget_width(get_l(i), "999.9 "); //length
      if (_show_pan)
        set_fixed_widget_width(get_p(i), "-35 "); //pan
      else
        get_p(i)->hide();
      set_fixed_widget_width(get_c(i), "Ch256 "); // chance
      
      get_o(i)->setToolTip("Whether to play this voice. At least one voice must be selected in order for any notes to be played.");
      get_t(i)->setToolTip("How much to transpose this voice");
      get_v(i)->setToolTip("Volume (in dB) for this voice. MIDI and FluidSynth does not support higher values than 0 dB.");
      get_s(i)->setToolTip("How long time until this voice starts playing\n"
                           "The unit is milliseconds.");//Beats. To use milliseconds instead, press the \"ms.\" button.");
      get_l(i)->setToolTip("A value higher than 0.0 will override the duration of this voice.\n"
                           "The unit is milliseconds.");//by default Beats, unless \"ms.\" is selected");
      get_p(i)->setToolTip("Pan value for this voice. The unit is in degrees.");
        
      get_f(i)->hide();
      get_c(i)->setToolTip("The chance of this voice to play. A value of 256 means that there is 100% chance of this voice plaing.\n"
                           "A value of 128 means that there is a 50% chance of this voice playing.");
      timeformat_label->hide();

    }

    for(int i=0;i<NUM_PATCH_VOICES;i++){
      get_o(i)->x2_border=0;
      get_o(i)->y1_border=0;
    }
    
    setup_popup_menus_and_stuff();

    IsAlive is_alive(this);
    
    locked_instrument->_hovered_callback = [is_alive](bool do_enter){
      if (is_alive){
        if (do_enter){
          
          QString settings_key, default_dir;
          QString path =
          "Select whether Radium is allowed to change to a different instrument by itself.<br>"
          "<p>Right-click to configure keybinding and change current instrument.";
          
          //QToolTip::showText(QCursor::pos(),path + "gakk",NULL,QRect()); // QToolTip tries to be smart, but does of course fail. Why not let the programmer decide how things should behave instead? (shold probably make a custom tooltip function to avoid alle these workarounds)
          QToolTip::showText(QCursor::pos(),path,NULL,QRect());

          if (isCurrentInstrumentLocked())
            GFX_SetStatusBar("Current instrument locked");
          else
            GFX_SetStatusBar("Current instrument unlocked");
          
        } else {
          
          QToolTip::hideText();
          GFX_SetStatusBar("");
          
        }
      }
    };
    
    updateWidgets();
    initing = false;

    _has_inited = true;
  }

  const char *get_effect_name(const char *name, int voicenum){
    return talloc_format("System %s Voice %d", name, voicenum+1);
  }

  void show_popup(const char *name, int voicenum){
    S7CALL2(dyn_dyn_charpointer,"FROM_C-show-effect-popup-menu", DYN_create_instrument(_patch->id), get_effect_name(name, voicenum));
  }

  template <class SpinBox> void set_popup_and_stuff(SpinBox *spinbox, const char *name, int voicenum){
    //int64_t id = _patch->id;

    const char *effect_name = get_effect_name(name, voicenum);
    
    spinbox->_statusbar_text = QString("\"") + effect_name + "\" (right-click for options)";

    IsAlive is_alive(this);
    
    spinbox->_show_popup_menu = [is_alive, this, name, voicenum](){
      if (!is_alive)
        return;
      
      show_popup(name, voicenum);
      set_editor_focus();
    };    
  }
  
  void setup_popup_menus_and_stuff(void){
    if (_patch->instrument != get_audio_instrument())
      return;
    
    for(int i=0;i<NUM_PATCH_VOICES;i++){

      IsAlive is_alive(this);

      get_o(i)->_show_popup_menu = [is_alive, this, i](){
        if (!is_alive)
          return;
      
        show_popup("On/Off", i);
        set_editor_focus();
      };

      //get_t(i)->_statusbar_text = QString(get_effect_name("Voice", getInstrumentEffectName(_effect_num, i))) + " (right-click for options)";
      set_popup_and_stuff(get_t(i), "Transpose", i);
      set_popup_and_stuff(get_v(i), "Volume", i);
      set_popup_and_stuff(get_s(i), "Start", i);
      set_popup_and_stuff(get_l(i), "Length", i);
      set_popup_and_stuff(get_p(i), "Pan", i);
      set_popup_and_stuff(get_c(i), "Chance", i);
      
    }
  }
  
  void set_fixed_widget_width(QWidget *widget, QString text){
    QFont font;
    widget->setFont(font);
    QFontMetrics fm(font);
    int width = fm.boundingRect(text).width() + 2;
    widget->setFixedWidth(width);
    widget->setMinimumWidth(width);
    widget->setMaximumWidth(width);
  }
  
  MyQCheckBox *get_o(int i){
    MyQCheckBox *o[NUM_PATCH_VOICES]={o1,o2,o3,o4,o5,o6,o7};
    return o[i];
  }

#if 0
  MyQSpinBox *get_t(int i){
    MyQSpinBox *t[NUM_PATCH_VOICES]={t1,t2,t3,t4,t5,t6,t7};
    return t[i];
  }
#else
  FocusSnifferQDoubleSpinBox *get_t(int i){
    FocusSnifferQDoubleSpinBox *t[NUM_PATCH_VOICES]={t1,t2,t3,t4,t5,t6,t7};
    return t[i];
  }
#endif
  
  MyQSpinBox *get_v(int i){
    MyQSpinBox *v[NUM_PATCH_VOICES]={v1,v2,v3,v4,v5,v6,v7};
    return v[i];
  }

  FocusSnifferQDoubleSpinBox *get_s(int i){
    FocusSnifferQDoubleSpinBox *s[NUM_PATCH_VOICES]={s1,s2,s3,s4,s5,s6,s7};
    return s[i];
  }

  FocusSnifferQDoubleSpinBox *get_l(int i){
    FocusSnifferQDoubleSpinBox *l[NUM_PATCH_VOICES]={l1,l2,l3,l4,l5,l6,l7};
    return l[i];
  }

  MyQSpinBox *get_p(int i){
    MyQSpinBox *p[NUM_PATCH_VOICES]={p1,p2,p3,p4,p5,p6,p7};
    return p[i];
  }

  MyQCheckBox *get_f(int i){
    MyQCheckBox *f[NUM_PATCH_VOICES]={f1,f2,f3,f4,f5,f6,f7};
    return f[i];
  }

  MyQSpinBox *get_c(int i){
    MyQSpinBox *c[NUM_PATCH_VOICES]={c1,c2,c3,c4,c5,c6,c7};
    return c[i];
  }

  void update_label_color(QLabel *l){
    QString text = l->text();
    //QString orgtext = text;
    int pos = text.indexOf("#");
    l->setText(text.replace(pos, 9, get_qcolor(TEXT_COLOR_NUM).name(QColor::HexArgb)));

    QFontMetrics fm(QApplication::font());
    int fontsize = R_MAX(2, fm.height() / 2);

    int pos1 = text.indexOf("font-size:") + QString("font-size:").size();
    int pos2 = text.indexOf("pt;");
    l->setText(text.replace(pos1, pos2-pos1, QString::number(fontsize)));

    //printf("Text before: -%s-. Text now: -%s-\n", orgtext.toUtf8().constData(), l->text().toUtf8().constData());
  }


  template <class SpinBox, typename T>
  void update_spinbox_value(SpinBox *spinbox, T new_value){
    if (!equal_doubles((T)spinbox->value(), new_value) && false==spinbox->hasFocus())
      spinbox->setValue(new_value);
  }
  
  void updateWidgets(){

    _called_from_update = true;
    
    QFontMetrics fm(QApplication::font());
    int header_height = fm.height() * 4 / 3;
    header->setMinimumHeight(header_height);
    header->setMaximumHeight(header_height);

    for(int i=0;i<NUM_PATCH_VOICES;i++){
      const PatchVoice &voice=_patch->voices[i];

      get_o(i)->setChecked(voice.is_on);
      get_o(i)->_patch.set(_patch.data());
      get_o(i)->_effect_num = get_effect_num(EFFNUM_VOICE1_ONOFF, i);
      get_o(i)->_is_patchvoice_onoff_button = true;
      get_o(i)->_patchvoicenum = i;

      update_spinbox_value(get_t(i), voice.transpose);
      update_spinbox_value(get_v(i), voice.volume);
      update_spinbox_value(get_s(i), voice.start);
      update_spinbox_value(get_l(i), voice.length);
      update_spinbox_value(get_p(i), voice.pan);
      update_spinbox_value(get_c(i), voice.chance);
    }

    // bad 1

    name_widget->setText(_patch->name);
    through_onoff->setChecked(_patch->forward_events);
    //through_onoff->_show_enabled_marker = false;
    
    update_label_color(nd_label1);
    update_label_color(nd_label2);
    update_label_color(nd_label3);
    update_label_color(nd_label4);
    update_label_color(nd_label5);
    if (_show_pan)
      update_label_color(pan_label);
    else
      pan_label->hide();

    adjust_labels();

    {
      QFont font;
      locked_instrument->setFont(font); // don't know why

      if (isCurrentInstrumentLocked())
        locked_instrument->setText("locked.svg");
      else
        locked_instrument->setText("unlocked.svg");

      locked_instrument->_show_enabled_marker = false;
      
      /*
      const QFontMetrics fn = QFontMetrics(font);
      float width = 1.5 * fn.boundingRect("L").width();
      */
      
      locked_instrument->setMinimumWidth(locked_instrument->height());
      locked_instrument->setMaximumWidth(locked_instrument->height());
    
      locked_instrument->setChecked(isCurrentInstrumentLocked());

      locked_instrument->_show_popup_menu = [](){
        S7CALL2(void_void,"FROM_C-show-lock-instrument-popup-menu");
      };

    }
    
    _called_from_update = false;
  }

  void adjust_labels(void){
    if(_has_inited==false)
      return;
    
    labels_widget->resize(labels_widget->width(), root->song->tracker_windows->systemfontheight*1.5);
    
    nd_label4->move(get_t(0)->x(), 0);
    nd_label1->move(get_v(0)->x(), 0);
    nd_label2->move(get_s(0)->x(), 0);
    nd_label3->move(get_l(0)->x(), 0);
    if (_show_pan)
      pan_label->move(get_p(0)->x(), 0);
    nd_label5->move(get_c(0)->x(), 0);
  }

  void changeEvent(QEvent *event) override {
    if (event->type()==QEvent::FontChange)
      adjust_labels();
  }
    
  void setVisible(bool visible) override {
    QWidget::setVisible(visible);

    adjust_labels();
  }
  
  void update_peaks(){
    struct Tracker_Windows *window=root->song->tracker_windows;
    window->wblock->block->is_dirty = true;
    
#if !USE_OPENGL
    struct Tracker_Windows *window=root->song->tracker_windows;
    TRACKREALLINES_update_peak_tracks(window,_patch.data());
    struct WBlocks *wblock=window->wblock;
    DrawUpAllPeakWTracks(window,wblock,_patch.data());
#endif
  }

  void onoff_toggled(int voicenum,bool val){
    if (_called_from_update)
      return;
    
    //printf("%d set to %d\n",voicenum,val);
    if(val==true)
      PATCH_turn_voice_on(_patch.data(), voicenum);
    else
      PATCH_turn_voice_off(_patch.data(), voicenum);

    update_peaks();
  }

  int get_effect_num(const int first_system_effect, const int voicenum){
    SoundPlugin *plugin = NULL;
    if (_patch->instrument==get_audio_instrument())
      plugin = (SoundPlugin*)_patch->patchdata;
    if (plugin==NULL)
      return -1;
    else
      return plugin->type->num_effects + first_system_effect + voicenum;
  }

  // returns false if we couldn't set value through instrument.
  bool set_instrument_effect(const int first_system_effect, const float value, const int voicenum){
    SoundPlugin *plugin = NULL;
    if (_patch->instrument==get_audio_instrument())
      plugin = (SoundPlugin*)_patch->patchdata;
    
    const int effect_num = plugin==NULL ? -1 : get_effect_num(first_system_effect, voicenum);
      
    if (plugin==NULL || false==MODULATOR_has_modulator(_patch.data(), effect_num)){ // don't do anything if it has a modulator assigned (adds unnecessary undo/redo).
      
      ADD_UNDO(PatchVoice_CurrPos(_patch.data(),voicenum));
      
      if (plugin != NULL){
        PLUGIN_set_effect_value(plugin, 0, effect_num, value, STORE_VALUE, FX_single, EFFECT_FORMAT_NATIVE);
      } else {
        R_ASSERT_NON_RELEASE(_patch->instrument==get_MIDI_instrument());
        return false;
      }
      
    }

    return true;
  }

  void set_float_effect(const int first_system_effect, int voicenum, float new_voicevalue, float &voicevalue){
    if (_called_from_update)
      return;

    if(!equal_floats(voicevalue, new_voicevalue)) {

#if !defined(RELEASE)
      printf("Setting float effect %d to %f. Old: %f\n", first_system_effect, new_voicevalue, voicevalue);
#endif
      
      //ADD_UNDO(PatchVoice_CurrPos(_patch.data(),voicenum));
      if (set_instrument_effect(first_system_effect, new_voicevalue, voicenum)==false)
        safe_float_write(&voicevalue, new_voicevalue);

      update_peaks();
    }

#if !defined(RELEASE)
    if (_patch->instrument==get_audio_instrument()){
      if (!is_playing() && false==MODULATOR_has_modulator(_patch.data(), get_effect_num(first_system_effect, voicenum)))
        R_ASSERT(equal_floats(new_voicevalue, voicevalue));
    } else
      R_ASSERT(equal_floats(new_voicevalue, voicevalue));
#endif

    set_editor_focus();
  }
                        
  void set_transpose(int voicenum){    
    float transpose=get_t(voicenum)->value();

    set_float_effect(EFFNUM_VOICE1_TRANSPOSE, voicenum, transpose, _voices[voicenum].transpose);
  }

  void set_volume(int voicenum){
    float volume=get_v(voicenum)->value();

    set_float_effect(EFFNUM_VOICE1_VOLUME, voicenum, volume, _voices[voicenum].volume);
  }

  void set_start(int voicenum){
    float new_value=get_s(voicenum)->value();
    set_float_effect(EFFNUM_VOICE1_START, voicenum, new_value, _voices[voicenum].start);
  }

  void set_length(int voicenum){
    float new_value=get_l(voicenum)->value();
    set_float_effect(EFFNUM_VOICE1_LENGTH, voicenum, new_value, _voices[voicenum].length);
  }

  void set_pan(int voicenum){
    float new_value=get_p(voicenum)->value();
    set_float_effect(EFFNUM_VOICE1_PAN, voicenum, new_value, _voices[voicenum].pan);
  }

  void set_chance(int voicenum){
    float new_value=get_c(voicenum)->value();
    set_float_effect(EFFNUM_VOICE1_CHANCE, voicenum, new_value, _voices[voicenum].chance);
  }
  
public slots:

  void on_locked_instrument_toggled(bool val){
    if (!_called_from_update)
      setCurrentInstrumentLocked(val);
  }
  
  void on_o1_toggled(bool val){onoff_toggled(0,val);}
  void on_o2_toggled(bool val){onoff_toggled(1,val);}
  void on_o3_toggled(bool val){onoff_toggled(2,val);}
  void on_o4_toggled(bool val){onoff_toggled(3,val);}
  void on_o5_toggled(bool val){onoff_toggled(4,val);}
  void on_o6_toggled(bool val){onoff_toggled(5,val);}
  void on_o7_toggled(bool val){onoff_toggled(6,val);}

#if 0
  void on_t1_valueChanged(int val){printf("t1 value changed\n");set_transpose(0,val);}
  void on_t2_valueChanged(int val){set_transpose(1,val);}
  void on_t3_valueChanged(int val){set_transpose(2,val);}
  void on_t4_valueChanged(int val){set_transpose(3,val);}
  void on_t5_valueChanged(int val){set_transpose(4,val);}
  void on_t6_valueChanged(int val){set_transpose(5,val);}
  void on_t7_valueChanged(int val){set_transpose(6,val);}
#endif

  void on_t1_editingFinished(){set_transpose(0);}
  void on_t2_editingFinished(){set_transpose(1);}
  void on_t3_editingFinished(){set_transpose(2);}
  void on_t4_editingFinished(){set_transpose(3);}
  void on_t5_editingFinished(){set_transpose(4);}
  void on_t6_editingFinished(){set_transpose(5);}
  void on_t7_editingFinished(){set_transpose(6);}

#if 0
  void on_v1_valueChanged(int val){set_volume(0,val);}
  void on_v2_valueChanged(int val){set_volume(1,val);}
  void on_v3_valueChanged(int val){set_volume(2,val);}
  void on_v4_valueChanged(int val){set_volume(3,val);}
  void on_v5_valueChanged(int val){set_volume(4,val);}
  void on_v6_valueChanged(int val){set_volume(5,val);}
  void on_v7_valueChanged(int val){set_volume(6,val);}
#endif

  void on_v1_editingFinished(){set_volume(0);}
  void on_v2_editingFinished(){set_volume(1);}
  void on_v3_editingFinished(){set_volume(2);}
  void on_v4_editingFinished(){set_volume(3);}
  void on_v5_editingFinished(){set_volume(4);}
  void on_v6_editingFinished(){set_volume(5);}
  void on_v7_editingFinished(){set_volume(6);}

#if 0
  void on_s1_valueChanged(double val){set_start(0,val);}
  void on_s2_valueChanged(double val){set_start(1,val);}
  void on_s3_valueChanged(double val){set_start(2,val);}
  void on_s4_valueChanged(double val){set_start(3,val);}
  void on_s5_valueChanged(double val){set_start(4,val);}
  void on_s6_valueChanged(double val){set_start(5,val);}
  void on_s7_valueChanged(double val){set_start(6,val);}
#endif

  void on_s1_editingFinished(){set_start(0);}
  void on_s2_editingFinished(){set_start(1);}
  void on_s3_editingFinished(){set_start(2);}
  void on_s4_editingFinished(){set_start(3);}
  void on_s5_editingFinished(){set_start(4);}
  void on_s6_editingFinished(){set_start(5);}
  void on_s7_editingFinished(){set_start(6);}

#if 0
  void on_l1_valueChanged(double val){set_length(0,val);}
  void on_l2_valueChanged(double val){set_length(1,val);}
  void on_l3_valueChanged(double val){set_length(2,val);}
  void on_l4_valueChanged(double val){set_length(3,val);}
  void on_l5_valueChanged(double val){set_length(4,val);}
  void on_l6_valueChanged(double val){set_length(5,val);}
  void on_l7_valueChanged(double val){set_length(6,val);}
#endif

  void on_l1_editingFinished(){set_length(0);}
  void on_l2_editingFinished(){set_length(1);}
  void on_l3_editingFinished(){set_length(2);}
  void on_l4_editingFinished(){set_length(3);}
  void on_l5_editingFinished(){set_length(4);}
  void on_l6_editingFinished(){set_length(5);}
  void on_l7_editingFinished(){set_length(6);}

  void on_p1_editingFinished(){set_pan(0);}
  void on_p2_editingFinished(){set_pan(1);}
  void on_p3_editingFinished(){set_pan(2);}
  void on_p4_editingFinished(){set_pan(3);}
  void on_p5_editingFinished(){set_pan(4);}
  void on_p6_editingFinished(){set_pan(5);}
  void on_p7_editingFinished(){set_pan(6);}

  void on_c1_editingFinished(){set_chance(0);}
  void on_c2_editingFinished(){set_chance(1);}
  void on_c3_editingFinished(){set_chance(2);}
  void on_c4_editingFinished(){set_chance(3);}
  void on_c5_editingFinished(){set_chance(4);}
  void on_c6_editingFinished(){set_chance(5);}
  void on_c7_editingFinished(){set_chance(6);}


  void on_name_widget_editingFinished()
  {
    if (_called_from_update)
      return;
    
    QString new_name = name_widget->text();

    if(new_name == QString(_patch->name)){
      set_editor_focus();
      return;
    }

    if(new_name==""){
      name_widget->setText("pip");
      new_name = "pip";
    }

    printf("Calling Undo patchname. Old name: %s. New name: %s\n",_patch->name,new_name.toUtf8().constData());

    ADD_UNDO(PatchName_CurrPos(_patch.data()));

    {
      PATCH_set_name(_patch.data(), new_name.toUtf8().constData());
      _patch->name_is_edited = true;
    }

    set_editor_focus();
  }

  void on_through_onoff_toggled(bool val){
    if (_called_from_update)
      return;
    
    if(val != _patch->forward_events) {
      ADD_UNDO(PatchName_CurrPos(_patch.data()));
      _patch->forward_events = val;
    }
  }
};

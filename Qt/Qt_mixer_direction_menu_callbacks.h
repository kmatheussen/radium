
#include "Qt_mixer_direction_menu.h"

class Mixer_Direction_Menu : public QDialog, public Ui::Mixer_Direction_Menu{
  Q_OBJECT

 public:
  bool _initing;
  bool _is_updating_widget = false;
  radium::ASMTimer _timer;
  
  Mixer_Direction_Menu(QWidget *parent)
    : QDialog(parent)
    , _timer(this)
  {
    setWindowFlags(Qt::FramelessWindowHint | Qt::Popup);
    
    _initing = true;

    setupUi(this);

    _initing = false;

    adjustSize();
  }

  void update_widget(void){
    _is_updating_widget = true;{
      /*
      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
      if (plugin != NULL){
        enum AutoSuspendBehavior mixer_direction_behavior = PLUGIN_get_autosuspend_behavior(plugin);
        if (mixer_direction_behavior==DEFAULT_AUTOSUSPEND_BEHAVIOR)
          default_suspend->setChecked(true);
        else if (mixer_direction_behavior==AUTOSUSPEND_ENABLED)
          enable_suspend->setChecked(true);
        else
          disable_suspend->setChecked(true);
      }
      */
    }_is_updating_widget=false;
  }

  void keyPressEvent ( QKeyEvent * event ) override {                             
    if(event->key()==Qt::Key_Return)
      hide();
    if(event->key()==Qt::Key_Enter)
      hide();
    QDialog::keyPressEvent(event);
  }                                                                     

  void myExec(void){
    update_widget();
    move(QCursor::pos());
    qApp->setActiveWindow(up_button);
    
    _timer.start();
    safeExec(this, true);
    _timer.stop();

    set_editor_focus();
  }
  
  void clicked(float rotate){
    if(_is_updating_widget)
      return;

    setMixerRotate(rotate);
               
    if (_timer.mouseWasDown())
      hide();
  }

public slots:

  void on_up_button_toggled(bool val){
    clicked(270);
  }
  void on_down_button_toggled(bool val){
    clicked(90);
  }
  void on_left_button_toggled(bool val){
    clicked(180);
  }

  void on_right_button_toggled(bool val){
    clicked(0);
  }
};


#include "Qt_auto_suspend_menu.h"

class Auto_Suspend_Menu : public QDialog, public Ui::Auto_Suspend_Menu{
  Q_OBJECT

 public:
  bool _initing;
  bool _is_updating_widget = false;
  radium::GcHolder<struct Patch> _patch;
  radium::ASMTimer _timer;
  
  Auto_Suspend_Menu(QWidget *parent, struct Patch *patch)
    : QDialog(parent)
    , _patch(patch)
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
      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
      if (plugin != NULL){
        enum AutoSuspendBehavior auto_suspend_behavior = PLUGIN_get_autosuspend_behavior(plugin);
        if (auto_suspend_behavior==DEFAULT_AUTOSUSPEND_BEHAVIOR)
          default_suspend->setChecked(true);
        else if (auto_suspend_behavior==AUTOSUSPEND_ENABLED)
          enable_suspend->setChecked(true);
        else
          disable_suspend->setChecked(true);
      }
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
    qApp->setActiveWindow(enable_suspend);
    
    _timer.start();
    safeExec(this, true);
    _timer.stop();
  }

public slots:

  void on_default_suspend_toggled(bool val){
    if(_is_updating_widget)
      return;
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    if (plugin != NULL){
      PLUGIN_set_autosuspend_behavior(plugin, DEFAULT_AUTOSUSPEND_BEHAVIOR);
    }
    if (_timer.mouseWasDown())
      hide();
  }

  void on_enable_suspend_toggled(bool val){
    if(_is_updating_widget)
      return;
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    if (plugin != NULL){
      PLUGIN_set_autosuspend_behavior(plugin, AUTOSUSPEND_ENABLED);
    }
    if (_timer.mouseWasDown())
      hide();
  }
  
  void on_disable_suspend_toggled(bool val){
    if(_is_updating_widget)
      return;
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    if (plugin != NULL){
      PLUGIN_set_autosuspend_behavior(plugin, AUTOSUSPEND_DISABLED);
    }
    if (_timer.mouseWasDown())
      hide();
  }
};


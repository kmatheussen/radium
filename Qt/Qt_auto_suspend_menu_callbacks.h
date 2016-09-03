
#include "Qt_auto_suspend_menu.h"

namespace radium{
  struct ASMTimer : public QTimer{
    QTime time;
    bool left_mouse_is_down = false;
    
    ASMTimer(QWidget *parent)
      :QTimer(parent)
    {
      time.start();
      setInterval(10);
    }

    void timerEvent ( QTimerEvent * e ){
      if (QGuiApplication::mouseButtons()==Qt::LeftButton)
        left_mouse_is_down = true;
      else if (left_mouse_is_down){
        left_mouse_is_down = false;
        time.restart();
      }
        
      //printf("  MOUSE DOWN: %d\n", QGuiApplication::mouseButtons()==Qt::LeftButton);
    }

    bool mouseWasDown(void){
      if(left_mouse_is_down==true)
        return true;
      if (time.elapsed() < 500)
        return true;
      return false;
    }
  };
}

class Auto_Suspend_Menu : public QDialog, public Ui::Auto_Suspend_Menu{
  Q_OBJECT

 public:
  bool _initing;
  bool _is_updating_widget = false;
  struct Patch *_patch;
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

  void mouseReleaseEvent(QMouseEvent * event) override {
    printf("Mouse Released\n");
    QDialog::mouseReleaseEvent(event);
  }

  void myExec(void){
    update_widget();
    move(QCursor::pos());
    qApp->setActiveWindow(enable_suspend);
    
    _timer.start();
    safeExec(this);
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


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

#if USE_QT_MENU

#include <QStack>
#include <QAction>
#include <QWidgetAction>
#include <QCheckBox>
#include <QMenu>
#include <QProxyStyle>
#include <QStyleFactory>

#define RADIUM_ACCESS_SEQBLOCK_ENVELOPE 1

#include "../common/nsmtracker.h"
#include "../common/visual_proc.h"
#include "../embedded_scheme/s7extra_proc.h"

#include "../api/api_proc.h"

#include "EditorWidget.h"




namespace{

  static bool _has_keyboard_focus = false; // Must be global since more than one QMenu may open simultaneously. (not supposed to happen, but it does happen)



  struct MyProxyStyle: public QProxyStyle {
    MyProxyStyle(){
      static QStyle *base_style = QStyleFactory::create("fusion");
      setBaseStyle(base_style); // Trying to fix style on OSX and Windows. Not necessary on Linux, for some reason.
    }

    virtual int pixelMetric(QStyle::PixelMetric metric, const QStyleOption* option = 0, const QWidget* widget = 0 ) const {
      if (metric==QStyle::PM_SmallIconSize && root!=NULL && root->song!=NULL && root->song->tracker_windows!=NULL)
        return root->song->tracker_windows->fontheight*4;
      else
        return QProxyStyle::pixelMetric( metric, option, widget ); // return default values for the rest
    }    
  };

  static MyProxyStyle g_my_proxy_style;



  struct MyQMenu : public QMenu{

    func_t *_callback;

    MyQMenu(QWidget *parent, bool is_async, func_t *callback)
      : QMenu(parent)
      , _callback(callback)
    {

      if(_callback!=NULL)
        s7extra_protect(_callback);
    }
    
    ~MyQMenu(){
      if(_callback!=NULL)
        s7extra_unprotect(_callback);      
    }

    void showEvent(QShowEvent *event) override {
      if (_has_keyboard_focus==false){
        obtain_keyboard_focus_counting();
        _has_keyboard_focus = true;
      }
    }

    void hideEvent(QHideEvent *event) override {
      if (_has_keyboard_focus==true){
        release_keyboard_focus_counting();
        _has_keyboard_focus = false;
      }
    }

    void closeEvent(QCloseEvent *event) override{
      if (_has_keyboard_focus==true){
        release_keyboard_focus_counting();
        _has_keyboard_focus = false;
      }
    }

    void keyPressEvent(QKeyEvent *event) override{
      //printf("KEY press event. Num actions: %d\n", actions().size());
      
      bool custom_treat
        = ((event->modifiers() & Qt::ShiftModifier) && (event->key()==Qt::Key_Up || event->key()==Qt::Key_Down))
        || (event->key()==Qt::Key_PageUp || event->key()==Qt::Key_PageDown)
        || (event->key()==Qt::Key_Home || event->key()==Qt::Key_End);
      
      if (custom_treat && actions().size() > 0) {

        bool is_down = event->key()==Qt::Key_Down || event->key()==Qt::Key_PageDown;
        int inc = is_down ? 1 : -1;
        
        auto *curr_action = activeAction();
        QAction *new_action = NULL;

        if (event->key()==Qt::Key_Home) {
          
          new_action = actions().first();
          
        } else if (event->key()==Qt::Key_End) {
          
          new_action = actions().last();
          
        } else if (is_down && curr_action==actions().last()) {

          new_action = actions().first();

        } else if (!is_down && curr_action==actions().first()) {

          new_action = actions().last();
            
        } else {
            
          int pos = 0;
            
          if (curr_action!=NULL) {
              
            for(auto *action : actions()){
              //printf("%d: %p (%p)\n", pos, action, curr_action);
                
              if (curr_action==action)
                break;
                
              pos++;
            }
          }

          int new_pos = pos + inc*10;
          //printf("new_pos: %d. size: %d\n", new_pos, actions().size());

          int safety = 0;
          do{
            
            if (new_pos >= actions().size())
              new_action = actions().last();
              
            else if (new_pos < 0)
              new_action = actions().first();
            
            else
              new_action = actions().at(new_pos);

            //printf("new_pos: %d. is_enabled: %d (%p). Curr enabled: %d\n", new_pos, new_action!=NULL && new_action->isEnabled(), new_action, curr_action->isEnabled());
            
            new_pos += inc;
            
            if (new_pos > actions().size())
              new_pos = 0;
            if (new_pos < 0)
              new_pos = actions().size();
            
            safety++;
            if (safety > 1000)
              break;
            
          }while(new_action==NULL || new_action->isVisible()==false || new_action->isSeparator()==true || new_action->isEnabled()==false);
          
        }

        if (new_action!=NULL)
          setActiveAction(new_action);
        
        event->accept();

        return;
      }
      
      QMenu::keyPressEvent(event);
    }
  };

  
  /*

   Old code to manually create checkableaction from qwidgets. Not sure why I used this code before. Replaced 2018-01 since it doesn't highlight entry when mouse hovers over it.

  struct CheckboxMenuWidget : public QWidget {
    
    QCheckBox *checkBox;
    
    CheckboxMenuWidget(QString text, MQMenu *qmenu){
      QHBoxLayout *layout = new QHBoxLayout;
      layout->setSpacing(0);
      layout->setContentsMargins(0,0,0,0);
      
      layout->addItem(new QSpacerItem(gui_textWidth("F"), 10, QSizePolicy::Fixed, QSizePolicy::MinimumExpanding));
      
      checkBox = new QCheckBox(text, qmenu);
      checkBox->setChecked(is_on);
      
      layout->addWidget(checkBox);
      
      widget->setLayout(layout);
    }
  };
  
  class CheckableAction_old : public QWidgetAction
  {
    Q_OBJECT

    QString text;
    MyQMenu *qmenu;
    int num;
    bool is_async;
    func_t *callback;
    std::function<void(int,bool)> callback3;
    
  public:

    ~CheckableAction(){
      //printf("I was deleted: %s\n",text.toUtf8().constData());
    }
    
    CheckableAction(const QString & text_b, bool is_on, MyQMenu *qmenu_b, int num_b, bool is_async, func_t *callback_b, std::function<void(int,bool)> callback3_b)
      : QWidgetAction(qmenu_b)
      , text(text_b)
      , qmenu(qmenu_b)
      , num(num_b)
      , is_async(is_async)
      , callback(callback_b)
      , callback3(callback3_b)
    {
      auto *widget = new CheckboxMenuWidget(text, qmenu);
      setDefaultWidget(widget);

      connect(checkBox, SIGNAL(toggled(bool)), this, SLOT(toggled(bool)));
      connect(this, SIGNAL(hovered()), this, SLOT(hovered()));
    }

  public slots:
    void toggled(bool checked){
    //void clicked(bool checked){
      printf("CLICKED %d\n",checked);
      
      if (callback!=NULL)
        S7CALL(void_int_bool,callback, num, checked);
      
      if (callback3)
        callback3(num, checked);

      if (result != NULL)
        *result = num;

      //if (is_async)
        qmenu->close();
      //delete parent;
    }

    void hovered(){
      printf("Hovered\n");
    }
  };
  */

  class CheckableAction : public QAction {
    Q_OBJECT

    int num;
    int *result;
    QString text;
    MyQMenu *qmenu;
    func_t *callback;
    std::function<void(int,bool)> callback3;

  public:

    ~CheckableAction(){
      //printf("I was deleted: %s\n",text.toUtf8().constData());
    }
    
    CheckableAction(QIcon icon, const QString & text_b, bool is_on, MyQMenu *qmenu_b, int num_b, bool is_async, func_t *callback_b, std::function<void(int,bool)> callback3_b, int *result)
      : QAction(icon, text_b, qmenu_b)
      , num(num_b)
      , result(result)
      , text(text_b)
      , qmenu(qmenu_b)
      , callback(callback_b)
      , callback3(callback3_b)
    {
      if(is_async)
        R_ASSERT(result==NULL);

      setCheckable(true);
      setChecked(is_on);
      connect(this, SIGNAL(toggled(bool)), this, SLOT(toggled(bool)));
    }

  public slots:
    void toggled(bool checked){
    //void clicked(bool checked){
      printf("CLICKED %d\n",checked);
      
      if (callback!=NULL)
        S7CALL(void_int_bool,callback, num, checked);
      
      if (callback3)
        callback3(num, checked);

      if (result != NULL)
        *result = num;

      //if (is_async)
        qmenu->close();
      //delete parent;
    }
  };

  class ClickableAction : public QAction
  {
    Q_OBJECT

    int num;
    int *result;
    QString text;
    MyQMenu *qmenu;
    //bool is_async;
    func_t *callback;
    std::function<void(int,bool)> callback3;

  public:

    ~ClickableAction(){
      //printf("I was deleted: %s\n",text.toUtf8().constData());
    }
    
    ClickableAction(QIcon icon, const QString & text, MyQMenu *qmenu, int num, bool is_async, func_t *callback, std::function<void(int,bool)> callback3, int *result)
      //: QAction(QIcon("/home/kjetil/radium/temp/radium_64bit_linux-5.4.8/bin/radium_256x256x32.png"), text, qmenu)
      : QAction(icon, text, qmenu)
      , num(num)
      , result(result)
      , text(text)
      , qmenu(qmenu)
        //, is_async(is_async)
      , callback(callback)
      , callback3(callback3)
    {
      if(is_async)
        R_ASSERT(result==NULL);
      connect(this, SIGNAL(triggered()), this, SLOT(triggered()));      
    }

  public slots:
    void triggered(){
    //void clicked(bool checked){
      //fprintf(stderr,"CLICKED clickable\n");

      if (callback!=NULL)
        S7CALL(void_int, callback, num);

      fprintf(stderr,"CLICKED 222222 clickable %d\n", num);
            
      if (callback3)
        callback3(num, true);

      if (result != NULL)
        *result = num;

      //if (is_async)
        qmenu->close();

      //delete parent;
    }
  };
}

QPointer<QWidget> g_current_parent_before_qmenu_opened; // Only valid if g_curr_popup_qmenu != NULL
QPointer<QMenu> g_curr_popup_qmenu;



static QMenu *create_qmenu(
                           const vector_t &v,
                           bool is_async,
                           func_t *callback2,
                           std::function<void(int,bool)> callback3,
                           int *result
                           )
{
  MyQMenu *menu = new MyQMenu(NULL, is_async, callback2);
  menu->setAttribute(Qt::WA_DeleteOnClose);

  menu->setStyle(&g_my_proxy_style);

  QMenu *curr_menu = menu;
  
  QStack<QMenu*> parents;
  QStack<int> n_submenuess;
  int n_submenues=0;
  
  for(int i=0;i<v.num_elements;i++) {
    QString text = (const char*)v.elements[i];
    if (text.startsWith("----"))
      menu->addSeparator();
    else {
      
      if (n_submenues==getMaxSubmenuEntries()){
        curr_menu = curr_menu->addMenu("Next");
        //curr_menu->setStyleSheet("QMenu { menu-scrollable: 1; }");
        n_submenues=0;
      }
      
      QAction *action = NULL;
      
      
      bool disabled = false;
      bool is_checkable = false;
      bool is_checked = false;

      QString icon_filename;

    parse_next:

      if (text.startsWith("[disabled]")){
        text = text.right(text.size() - 10);
        disabled = true;
        goto parse_next;
      }
      
      if (text.startsWith("[check on]")){
        text = text.right(text.size() - 10);
        is_checkable = true;
        is_checked = true;
        goto parse_next;
      }

      if (text.startsWith("[check off]")){
        text = text.right(text.size() - 11);
        is_checkable = true;
        is_checked = false;
        goto parse_next;
      }

      if (text.startsWith("[icon]")){
        text = text.right(text.size() - 6);
        int pos = text.indexOf(" ");
        if (pos <= 0){
          R_ASSERT(false);
        } else {
          QString encoded = text.left(pos);
          icon_filename = QByteArray::fromBase64(encoded.toUtf8());
          text = text.right(text.size() - pos - 1);
          //printf("  text: -%s-. encoded: -%s-. Filename: -%s-\n", text.toUtf8().constData(), encoded.toUtf8().constData(), icon_filename.toUtf8().constData());
        }
        goto parse_next;
      }

      if (text.startsWith("[submenu start]")){
        
        n_submenuess.push(n_submenues);
        n_submenues = 0;
        parents.push(curr_menu);
        curr_menu = curr_menu->addMenu(text.right(text.size() - 15));
        //curr_menu->setStyleSheet("QMenu { menu-scrollable: 1; }");
        
      } else if (text.startsWith("[submenu end]")){
        
        QMenu *parent = parents.pop();
        if (parent==NULL)
          handleError("parent of [submenu end] is not a QMenu");
        else
          curr_menu = parent;
        n_submenues = n_submenuess.pop();
        
      } else {

        QIcon icon;

        if (icon_filename != ""){

          static QHash<QString, QIcon> icon_map;

          if (icon_map.contains(icon_filename)) {

            icon = icon_map[icon_filename];

          } else {
            QColor background(180,180,190,200);

            icon
              = icon_filename.startsWith("<<<<<<<<<<envelope_icon")
              ? ENVELOPE_get_icon(icon_filename,
                                  get_system_fontheight()*4, get_system_fontheight()*4,
                                  background.darker(200), background, QColor(40,20,45,150)
                                  )
              : QIcon(icon_filename);

            if(icon.isNull())
              handleError("Could not load \"%s\".", icon_filename.toUtf8().constData());
            else
              icon_map[icon_filename] = icon;

          }
        }
        
        if (is_checkable)
          action = new CheckableAction(icon, text, is_checked, menu, i, is_async, callback2, callback3, result);
        else
          action = new ClickableAction(icon, text, menu, i, is_async, callback2, callback3, result);        
      }
      
      if (action != NULL){
        action->setData(i);
        curr_menu->addAction(action);  // are these actions automatically freed in ~QMenu? (yes, seems so) (they are probably freed because they are children of the qmenu)
      }
      
      if (disabled)
        action->setDisabled(true);
      
      n_submenues++;
    }
  }

  if (menu->actions().size() > 0)
    menu->setActiveAction(menu->actions().at(0));
  
  return menu;
}


static int GFX_QtMenu(
                const vector_t &v,
                func_t *callback2,
                std::function<void(int,bool)> callback3,
                bool is_async,
                bool program_state_is_valid
                )
{

  if(is_async){
    R_ASSERT(callback2!=NULL || callback3);
    R_ASSERT_RETURN_IF_FALSE2(program_state_is_valid, -1);
  }

  int result = -1;
  
  QMenu *menu = create_qmenu(v, is_async,  callback2, callback3, is_async ? NULL : &result);
  //printf("                CREATED menu %p", menu);
  
  if (is_async){

    safeMenuPopup(menu);
    return -1;
    
  } else {
    
    QAction *action = safeMenuExec(menu, program_state_is_valid);
    (void)action;

    /*
    MyAction *myaction = dynamic_cast<MyAction*>(action);
    printf("    ACTION: %p\n", action);

    if (myaction != NULL){
      printf("        GOT MyAction\n");
      return myaction->num;
    }

    printf("   NOt a myaction %p\n", action);
    */

    /////R_ASSERT_NON_RELEASE(false);

    //(void)action;
    //printf("    ACTION: %p\n", action);

    if (result >= v.num_elements){
      //R_ASSERT(false);
      return -1;
    }

    return result;
    /*
    if(action==NULL) {
      
      return -1;
    
    if (dynamic_cast<CheckableAction*>(action) != NULL)
      return -1;
    
    bool ok;
    int i=action->data().toInt(&ok);
    
    if (ok)      
      return i;

    //RWarning("Got unknown action %p %s\n",action,action->text().toAscii().constData());
    
    return -1;
    */
  }
}
void GFX_Menu3(
              const vector_t &v,
              std::function<void(int,bool)> callback3
              )
{
  GFX_QtMenu(v, NULL, callback3, true, true);
}

int GFX_Menu2(
              struct Tracker_Windows *tvisual,
              ReqType reqtype,
              const char *seltext,
              const vector_t v,
              func_t *callback,
              bool is_async,
              bool program_state_is_valid
              )
{
  if(is_async){
    R_ASSERT_RETURN_IF_FALSE2(callback!=NULL, -1);
    R_ASSERT_RETURN_IF_FALSE2(program_state_is_valid, 1);
  }
  
  if(reqtype==NULL || v.num_elements>20 || is_async || callback!=NULL){
    std::function<void(int,bool)> empty_callback3;
    return GFX_QtMenu(v, callback, empty_callback3, is_async, program_state_is_valid);
  }else
    return GFX_ReqTypeMenu(tvisual,reqtype,seltext,v, program_state_is_valid);
}


int GFX_Menu(
             struct Tracker_Windows *tvisual,
             ReqType reqtype,
             const char *seltext,
             const vector_t v,
             bool program_state_is_valid
             )
{
  return GFX_Menu2(tvisual, reqtype, seltext, v, NULL, false, program_state_is_valid);
}

// The returned vector can be used as argument for GFX_Menu.
vector_t GFX_MenuParser(const char *texts, const char *separator){
  vector_t ret = {};
  
  QStringList splitted = QString(texts).split(separator);
  
  for(int i=0 ; i<splitted.size() ; i++){
    QString trimmed = splitted[i].trimmed();
    if (trimmed != "")
      VECTOR_push_back(&ret, talloc_strdup(trimmed.toUtf8().constData()));
  }

  return ret;
}

#include "mQt_PopupMenu.cpp"

#endif // USE_QT_MENU

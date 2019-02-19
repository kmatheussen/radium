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

#include <memory>

#include <QStack>
#include <QAction>
#include <QWidgetAction>
#include <QCheckBox>
#include <QButtonGroup>
#include <QRadioButton>
#include <QMenu>
#include <QProxyStyle>
#include <QStyleFactory>


#include "../common/nsmtracker.h"
#include "../common/visual_proc.h"
#include "../audio/Envelope.hpp"

#include "../embedded_scheme/s7extra_proc.h"

#include "../api/api_gui_proc.h"

#include "../api/api_proc.h"

#include "EditorWidget.h"



QPointer<QWidget> g_current_parent_before_qmenu_opened; // Only valid if g_curr_popup_qmenu != NULL
QPointer<QMenu> g_curr_popup_qmenu;

int64_t g_last_hovered_menu_entry_guinum = -1;



namespace{

  class MyQAction;
  static int g_myactions_counter = 0;
  static QHash<int, QPointer<MyQAction>> g_curr_visible_actions;

  static bool _has_keyboard_focus = false; // Must be global since more than one QMenu may open simultaneously. (not supposed to happen, but it does happen)



  struct MyProxyStyle: public QProxyStyle {
    MyProxyStyle(){
      static QStyle *base_style = QStyleFactory::create("Fusion");
      setBaseStyle(base_style); // Trying to fix style on OSX and Windows. Not necessary on Linux, for some reason.
    }

#if 0
    void drawPrimitive(PrimitiveElement element, const QStyleOption *option, QPainter *painter, const QWidget *widget) const {
      static QStyle *windows_style = QStyleFactory::create("Windows");

      printf("   Element: %d (radio: %d)\n", element, QStyle::PE_IndicatorRadioButton);
      
      if (element == QStyle::PE_IndicatorRadioButton){
        windows_style->drawPrimitive(element, option, painter, widget);
      } else {          
        QProxyStyle::drawPrimitive(element, option, painter, widget);
      }
    }
#endif
    
    virtual int pixelMetric(QStyle::PixelMetric metric, const QStyleOption* option = 0, const QWidget* widget = 0 ) const {
      if (metric==QStyle::PM_SmallIconSize && root!=NULL && root->song!=NULL && root->song->tracker_windows!=NULL)
        return root->song->tracker_windows->fontheight*4;
      else
        return QProxyStyle::pixelMetric( metric, option, widget ); // return default values for the rest
    }    
  };

  static MyProxyStyle g_my_proxy_style;



  static int g_alive_callbackers = 0;
  
  struct Callbacker : public QObject{
    Q_OBJECT;

    public:
      
    QPointer<QMenu> qmenu;    
    int num;
    radium::ProtectedS7Extra<func_t*> callback;
    std::function<void(int,bool)> callback3;
    int *result;

    bool is_checkable;

    Callbacker(QMenu *qmenu, int num, bool is_async, func_t *callback, std::function<void(int,bool)> callback3, int *result, bool is_checkable)
      : qmenu(qmenu)
      , num(num)
      , callback(callback)
      , callback3(callback3)
      , result(result)
      , is_checkable(is_checkable)
    {
      R_ASSERT(is_async==true); // Lots of trouble with non-async menus. (triggers qt bugs)
      
      if(is_async)
        R_ASSERT(result==NULL);

      g_alive_callbackers++;
      //printf("    Created Callbacker. callbackers: %d. pos: %d\n", g_alive_callbackers, (int)this->callback._pos);
    }

    ~Callbacker(){
      R_ASSERT(_waiting_for_callback==false);
      R_ASSERT_NON_RELEASE(g_alive_callbackers > 0);
      g_alive_callbackers--;
      //printf("            CALLBACKER deleted 2. Alive now: %d. pos: %d. This: %p\n", g_alive_callbackers, (int)callback._pos, this);
    }
    
  private:
    bool checked = false;

    bool run_have_run = false;

    std::shared_ptr<Callbacker> _extra_destroyed_slot_argument; // Temporarily store it in the object since you can't post custom data to signals handlers in a less inelegant way, I think. The use of smart pointers here is a mess BTW, as it always is, for everyone, in all possible situations, without exception.
    
    void run_and_delete(bool checked, std::shared_ptr<Callbacker> &callbacker){
      R_ASSERT_RETURN_IF_FALSE(run_have_run==false);
      run_have_run = true;

      R_ASSERT_NON_RELEASE(qmenu.data() != NULL); // Not sure if anything is wrong, just curious whether it happens.
      
      this->checked = checked;

      if (result != NULL)
        *result = num;

      if (callback.v!=NULL || callback3!=NULL) {

        if (qmenu.data() != NULL){
          _extra_destroyed_slot_argument = callbacker;
          connect(qmenu.data(), SIGNAL(destroyed(QObject*)), this, SLOT(menu_destroyed(QObject*)) );
        }

      }
      
      if (qmenu.data() != NULL)
        qmenu->close();
      
      //if (callback.v==NULL && callback3==NULL)
      //  delete this;

      if (g_alive_callbackers > 1000){
        //R_ASSERT_NON_RELEASE(false);
        printf("  Warning: popup menu might leak: %d.\n", g_alive_callbackers);
      }
    }

    void run_callbacks(void){
      if (callback.v != NULL){
        if (is_checkable)
          S7CALL(void_int_bool, callback.v, num, checked);
        else
          S7CALL(void_int, callback.v, num);
      }
        
      if (callback3)
        callback3(num, checked);
    }

  public:
    
    void run_checked(bool checked){
      R_ASSERT(is_checkable);
      this->checked = checked;
      run_callbacks();
    }
    
    void run_and_delete_checked(bool checked, std::shared_ptr<Callbacker> &callbacker){
      R_ASSERT(is_checkable);
      run_and_delete(checked, callbacker);
    }
    
    void run_and_delete_clicked( std::shared_ptr<Callbacker> &callbacker){
      R_ASSERT(false==is_checkable);
      run_and_delete(false, callbacker);
    }

  private:

    int num_callback_tries = 0;
    
    void call_callback_and_delete(std::shared_ptr<Callbacker> callbacker){ // don't know why, but we get compilation error if referencing callbacker.
      num_callback_tries++;
      
      if (false==qmenu.isNull()){

        //printf("Num callback tries: %d\n", num_callback_tries);
        
        R_ASSERT_NON_RELEASE(false); // This line can probably be commented away. If Qt hasn't deleted the menu right after calling the menu_destroyed slot, that's an issue in qt and probably nothing we can do anything about here.

        if (num_callback_tries < 100)
          schedule_call_callback_and_delete(50, callbacker); // try again. Wait a little bit more this time to avoid clogging up CPU in case we never succeed.
        else
          R_ASSERT(false); // After 5 seconds we give up.

      } else {

        run_callbacks();
        
        //delete this;
      }
    }

  public:

    bool _waiting_for_callback = false;
    
    void schedule_call_callback_and_delete(int ms, std::shared_ptr<Callbacker> callbacker){ // Can't use reference for callbacker since lambdas doesn't support unreferencing arguments in c++11.
      IsAlive is_alive(this);

      _waiting_for_callback = true;

      QTimer::singleShot(ms, [is_alive, this, callbacker] { // in c++17, it seems like we could have used *callbacker here so that &callbacker could be used as argument for 'schedule_call_callback_and_delete'.
          if (is_alive){
            //printf("Hello3: %d %p. This: %p\n", (int)callbacker.use_count(), &callbacker, this);
            this->_waiting_for_callback = false;
            this->call_callback_and_delete(callbacker);
            // Note: At this point, we might have been deleted. (probably not since callbacker still exists)
          }else{
            printf("   It was deleted. ThIS: %p\n", this);
            R_ASSERT(false);
          }          
        });
    }

  private slots:
    
    void menu_destroyed(QObject*){
      // Must wait a little bit since Qt seems to be in a state right now where calling g_curr_popup_qmenu->hide() will crash the program (or do other bad things) (5.10).
      schedule_call_callback_and_delete(1, _extra_destroyed_slot_argument);
      _extra_destroyed_slot_argument = NULL;
    }
    
  };


  static QPointer<QWidget> g_last_hovered_widget;
  
  class MyQAction : public QWidgetAction {
    Q_OBJECT

    std::shared_ptr<Callbacker> _callbacker;
    
  public:

    int64_t _guinum;
    QWidget *_widget = NULL;
    
    QAbstractButton *_checkbox = NULL; // can be either QCheckBox or QRadioButton
    
    bool _success = true;

    MyQAction(const QIcon &icon, const QString &text, const QString &shortcut, std::shared_ptr<Callbacker> &callbacker, bool is_checkbox, bool is_checked, bool is_radiobutton, bool is_first, bool is_last, QObject *parent = NULL)
      : QWidgetAction(parent)
      , _callbacker(callbacker)
    {
      int entryid = g_myactions_counter++;
      
      g_curr_visible_actions[entryid] = this;
      
      S7EXTRA_GET_FUNC(s_func, "FROM_C-create-menu-entry-widget");

      _guinum = S7CALL(int_int_charpointer_charpointer_bool_bool_bool_bool_bool, s_func, entryid, text.toUtf8().constData(), shortcut.toUtf8().constData(), is_checkbox, is_checked, is_radiobutton, is_first, is_last);

      if (_guinum > 0){
        _widget = API_gui_get_widget(_guinum);
        setDefaultWidget(_widget);        
      } else
        _success = false;

      if (_widget!=NULL && is_checkbox){
        _checkbox = _widget->findChild<QAbstractButton*>("checkbox");
        if(_checkbox==NULL)
          R_ASSERT(false);
        else
          connect(_checkbox, SIGNAL(toggled(bool)), this, SLOT(toggled(bool)));        
      }
    }

  public slots:

    // This one is called when toggling the checkbox itself, not just the whole widget (outside the checkbox)
    void toggled(bool checked){
      //printf("   TOGGLED: %d\n", checked);
      _callbacker->run_checked(checked);
    }
  };
  
  class CheckableAction : public MyQAction {
    Q_OBJECT

    std::shared_ptr<Callbacker> callbacker;
    
  public:

    ~CheckableAction(){
      //printf("I was deleted: %s\n",text.toUtf8().constData());
    }

    CheckableAction(QIcon icon, const QString & text_b, const QString &shortcut, bool is_on, bool is_radiobutton, bool is_first, bool is_last, std::shared_ptr<Callbacker> &callbacker)
      : MyQAction(icon, text_b, shortcut, callbacker, true, is_on, is_radiobutton, is_first, is_last, callbacker->qmenu)
      , callbacker(callbacker)
    {
      setCheckable(true);
      setChecked(is_on);
      connect(this, SIGNAL(toggled(bool)), this, SLOT(toggled(bool)));
    }

  public slots:
    void toggled(bool checked){
      callbacker->run_and_delete_checked(checked, callbacker);
    }
  };

  class ClickableAction : public MyQAction
  {
    Q_OBJECT;
    
    std::shared_ptr<Callbacker> callbacker;
    
  public:

    ~ClickableAction(){
      //printf("I was deleted: %s\n",text.toUtf8().constData());
    }
    
    ClickableAction(QIcon icon, const QString & text, const QString &shortcut, bool is_first, bool is_last, std::shared_ptr<Callbacker> &callbacker)
      : MyQAction(icon, text, shortcut, callbacker, false, false, false, is_first, is_last, callbacker->qmenu)
      , callbacker(callbacker)
    {
      connect(this, SIGNAL(triggered()), this, SLOT(triggered()));      
    }

  public slots:
    void triggered(){
      callbacker->run_and_delete_clicked(callbacker);
    }
  };

  // Seems like it's impossible to make Qt behave non-irritating when you have separators and are trying to click the separator (this happens because the previous hovered action is still shown as hovered, which is nice, but irritating since nothing happens to it when clicking).
  // Just using menu->addSeparator() was the worst, where the menu was closed if you clicked a separator and nothing else happened. This class calls setEnabled(false),
  // which prevents the menu from closing. But I still haven't found a way to make the hovered entry selected, which would have been the natural behavior.
  class SeparatorAction : public QAction
  {
    Q_OBJECT;
    
  public:

    ~SeparatorAction()
    {
      //printf("I was deleted: %s\n",text.toUtf8().constData());
    }
    
    SeparatorAction() //const QString & text, bool is_first, bool is_last)
    {
      //connect(this, SIGNAL(hovered()), this, SLOT(hovered()));
      //connect(this, SIGNAL(triggered()), this, SLOT(triggered()));      
      setSeparator(true);
      setEnabled(false);
    }
    /*
  public slots:
    void hovered(){
      printf("                EYSYES\n");
    }
    void triggered(){
      printf("          CLACKED\n");
    }
    */
  };

  class ClickableIconAction : public QAction
  {
    Q_OBJECT;
    
    std::shared_ptr<Callbacker> callbacker;
    
  public:

    ~ClickableIconAction(){
      //printf("I was deleted: %s\n",text.toUtf8().constData());
    }
    
    ClickableIconAction(QIcon icon, const QString & text, std::shared_ptr<Callbacker> &callbacker)
      : QAction(icon, text, callbacker->qmenu)
      , callbacker(callbacker)
    {
      connect(this, SIGNAL(triggered()), this, SLOT(triggered()));      
    }

  public slots:
    void triggered(){
      callbacker->run_and_delete_clicked(callbacker);
    }
  };

  struct MyQMenu : public QMenu{
    MyQMenu(QWidget *parent, QString title)
      : QMenu(title, parent)
    {
    }

    void keyPressEvent(QKeyEvent *event) {
      bool custom_treat
        = ((event->modifiers() & Qt::ShiftModifier) && (event->key()==Qt::Key_Up || event->key()==Qt::Key_Down))
        || (event->key()==Qt::Key_PageUp || event->key()==Qt::Key_PageDown)
        || (event->key()==Qt::Key_Home || event->key()==Qt::Key_End);

      if (custom_treat && actions().size() > 0) {

        bool key_down = event->key()==Qt::Key_Down || event->key()==Qt::Key_PageDown;
        bool key_up = !key_down;
        int inc = key_down ? 1 : -1;

        auto *curr_action = activeAction();

        bool is_at_top = curr_action==actions().first();
        bool is_at_bot = curr_action==actions().last();

        int min_num_steps = 4;
        if (event->key()==Qt::Key_PageUp || event->key()==Qt::Key_PageDown)
          min_num_steps = 10;
        else if (event->key()==Qt::Key_Home || event->key()==Qt::Key_End)
          min_num_steps = 1000000;
        
        if (is_at_top && key_up){
          
          send_key_up(this, 1);
          
        } else if (is_at_bot && key_down){

          send_key_down(this, 1);

        } else {

          int curr_pos = 0;
            
          if (curr_action!=NULL) {
            
            for(auto *action : actions()){
              //printf("%d: %p (%p)\n", curr_pos, action, curr_action);
              
              if (curr_action==action)
                break;
              
              curr_pos++;
            }
            
            if (curr_pos>=actions().size())
              curr_pos = actions().size()-1;
          }

          int i = curr_pos;
          int num_steps = 0;
          
          while(num_steps < min_num_steps){

            if (key_down)
              send_key_down(this, 1);
            else
              send_key_up(this, 1);
            
            while(true){
              i += inc;
              num_steps++;
              
              if (i <= 0 || i >= actions().size()-1)
                goto finished;

              bool is_selectable = actions().at(i)->isVisible()==true && actions().at(i)->isSeparator()==false && actions().at(i)->isEnabled()==true;

              if(is_selectable)
                break;
            }

            //printf("    i: %d. at: %p. Curr: %p. Size: %d\n", i, NULL /*actions().at(i)*/, curr_action, actions().size());
          }
        }

      finished:
        
        event->accept();
        return;
      }

      QMenu::keyPressEvent(event);
    }
    
  };
  
  struct MyMainQMenu : public MyQMenu{
    Q_OBJECT

  public:
    
    //func_t *_callback;

    MyMainQMenu(QWidget *parent, QString title, bool is_async, func_t *callback)
      : MyQMenu(parent, title)
        //, _callback(callback)
    {
      
      R_ASSERT(is_async==true); // Lots of trouble with non-async menus. (triggers qt bugs)
      
      //if(_callback!=NULL)
      //  s7extra_protect(_callback);

      connect(this, SIGNAL(hovered(QAction*)), this, SLOT(hovered(QAction*)));
    }
    
    ~MyMainQMenu(){
      //if(_callback!=NULL)
      //  s7extra_unprotect(_callback);      
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
  

    /*
    void actionEvent(QActionEvent *e) override{
      printf("   ACTION EVENT called: %p\n", e);
      QMenu::actionEvent(e);
    }
    */

    int64_t _my_last_hovered_menu_entry_guinum = -1;
    
    void leaveEvent(QEvent *event) override {
      if (g_last_hovered_menu_entry_guinum != _my_last_hovered_menu_entry_guinum){
        QWidget *old = g_last_hovered_widget;
        
        //printf("  Leave event: %d\n", (int)g_last_hovered_menu_entry_guinum);
        
        g_last_hovered_menu_entry_guinum = -1;
        _my_last_hovered_menu_entry_guinum = -1;
        g_last_hovered_widget = NULL;

        if(old!=NULL)
          old->update();
        //updateWidgetRecursively(this);
      }
    }

  public slots:
    
    void hovered(QAction *action){
      MyQAction *myaction = dynamic_cast<MyQAction*>(action);
      
      if (g_last_hovered_widget.data() != NULL)
        g_last_hovered_widget->update();
      
      if (myaction!=NULL){
        
        myaction->_widget->update();
        
        g_last_hovered_menu_entry_guinum = myaction->_guinum;
        _my_last_hovered_menu_entry_guinum = -1;
        g_last_hovered_widget = myaction->_widget;

        //printf("  Hovered ON: %d\n", (int)g_last_hovered_menu_entry_guinum);

      } else {

        //printf("  Hovered OFF: %d\n", (int)g_last_hovered_menu_entry_guinum);
        
        g_last_hovered_menu_entry_guinum = -1;
        _my_last_hovered_menu_entry_guinum = -1;
        g_last_hovered_widget = NULL;
        
      }
      
      //printf("  QMENU::HOVERED action: %p. myaction: %p. Is separator: %d\n", action, myaction, action->isSeparator());
    }
      

  };


}


static void setStyleRecursively(QWidget *widget, QStyle *style){
  if (widget != NULL){
    widget->setStyle(style);
    
    for(auto *c : widget->children())
      setStyleRecursively(dynamic_cast<QWidget*>(c), style);
  }
}

static QMenu *create_qmenu(
                           const vector_t &v,
                           bool is_async,
                           func_t *callback2,
                           std::function<void(int,bool)> callback3,
                           int *result
                           )
{
  R_ASSERT(is_async==true); // Lots of trouble with non-async menus. (triggers qt bugs)

  g_myactions_counter = 0;
  g_curr_visible_actions.clear();
  
  MyMainQMenu *menu = new MyMainQMenu(NULL, "", is_async, callback2);
  menu->setAttribute(Qt::WA_DeleteOnClose);

  MyQMenu *curr_menu = menu;
  
  QStack<MyQMenu*> parents;
  QStack<int> n_submenuess;
  int n_submenues=0;

  QActionGroup *radio_buttons = NULL;
  QButtonGroup *radio_buttons2 = NULL;
  
  for(int i=0;i<v.num_elements;i++) {
    
    QString text = (const char*)v.elements[i];
    
    if (text.startsWith("----")) {
      
      auto *separator = new SeparatorAction(); //curr_menu->addSeparator();

      for(int i = 3 ; i < text.size() ; i++)
        if (text[i] != '-'){
          separator->setText(text.mid(i));
          break;
        }

      curr_menu->addAction(separator);
 
      
    } else {
      
      if (n_submenues==getMaxSubmenuEntries()){
        auto *new_menu = new MyQMenu(curr_menu, "Next");
        curr_menu->addMenu(new_menu);
        curr_menu = new_menu;
        //curr_menu->setStyleSheet("QMenu { menu-scrollable: 1; }");
        n_submenues=0;
      }
      
      QAction *action = NULL;
      
      
      bool disabled = false;
      bool is_checkable = false;
      bool is_checked = false;

      QString icon_filename;
      QString shortcut;
      
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

      if (text.startsWith("[shortcut]")){
        text = text.right(text.size() - 10);
        int pos = text.indexOf("[/shortcut]");
        if (pos <= 0){
          R_ASSERT(false);
        } else {
          shortcut = text.left(pos);
          text = text.right(text.size() - pos - 11);
          //printf("shortcut: -%s-. text: -%s-\n", shortcut.toUtf8().constData(), text.toUtf8().constData());
        }
        goto parse_next;
      }
      
      if (text.startsWith("[submenu start]")){
        
        n_submenuess.push(n_submenues);
        n_submenues = -1;
        parents.push(curr_menu);
        auto *new_menu = new MyQMenu(curr_menu, text.right(text.size() - 15));
        curr_menu->addMenu(new_menu);
        curr_menu = new_menu;
        //curr_menu->setStyleSheet("QMenu { menu-scrollable: 1; }");
        
      } else if (text.startsWith("[submenu end]")){
        
        MyQMenu *parent = parents.pop();
        if (parent==NULL)
          handleError("parent of [submenu end] is not a QMenu");
        else
          curr_menu = parent;
        n_submenues = n_submenuess.pop();

      } else if (text.startsWith("[radiobuttons start]")){
        
        radio_buttons = new QActionGroup(curr_menu);
        radio_buttons2 = new QButtonGroup(curr_menu);
        
      } else if (text.startsWith("[radiobuttons end]")){
        
        R_ASSERT_NON_RELEASE(radio_buttons!=NULL);
        R_ASSERT_NON_RELEASE(radio_buttons2!=NULL);
        
        if (radio_buttons!=NULL && radio_buttons->actions().size()==0){
          delete radio_buttons;
          if (radio_buttons2!=NULL){
            R_ASSERT(radio_buttons2->buttons().size()==0);
            delete radio_buttons2;
          } else
            R_ASSERT(false);
        }
        
        radio_buttons = NULL;
        radio_buttons2 = NULL;
        
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

        bool is_first = n_submenues==0;
        bool is_last = n_submenues==v.num_elements-1 || n_submenues==getMaxSubmenuEntries()-1;

        if (!is_last){
          if (i < v.num_elements-1){
            QString text = (const char*)v.elements[i+1];
            if(text.startsWith("[submenu end]"))
              is_last=true;
          }
        }

        auto callbacker = std::shared_ptr<Callbacker>(new Callbacker(menu, i, is_async, callback2, callback3, result, is_checkable));
          
        if (!icon.isNull()) {

          action = new ClickableIconAction(icon, text, callbacker);
                    
        } else if (is_checkable) {

          auto *hepp = new CheckableAction(icon, text, shortcut, is_checked, radio_buttons != NULL, is_first, is_last, callbacker);
          if (hepp->_success==false){
            radio_buttons=NULL;
            goto finished_parsing;
          }
          
          action = hepp;
          if (radio_buttons != NULL){
            radio_buttons->addAction(action);
            if (radio_buttons2 != NULL){
              if (hepp->_checkbox != NULL)
                radio_buttons2->addButton(hepp->_checkbox);
            }else{
              R_ASSERT(false);
            }              
          }else{
            R_ASSERT(radio_buttons2==NULL);
          }
          
        } else {

          auto *hepp = new ClickableAction(icon, text, shortcut, is_first, is_last, callbacker);
          if (hepp->_success==false)
            goto finished_parsing;
          
          action = hepp;
        }
      }
      
      if (action != NULL){
        action->setData(i);
        curr_menu->addAction(action);  // are these actions automatically freed in ~QMenu? (yes, seems so) (they are probably freed because they are children of the qmenu)
      }
      
      if (disabled)
        action->setDisabled(true);      
    }

    n_submenues++;
  }

 finished_parsing:
  
  if (menu->actions().size() > 0)
    menu->setActiveAction(menu->actions().at(0));

  R_ASSERT_NON_RELEASE(radio_buttons==NULL);
  R_ASSERT_NON_RELEASE(radio_buttons2==NULL);
  
  if (radio_buttons!=NULL && radio_buttons->actions().size()==0){
    R_ASSERT_NON_RELEASE(false);
    delete radio_buttons;
  }

  if (radio_buttons2!=NULL && radio_buttons2->buttons().size()==0){
    R_ASSERT_NON_RELEASE(false);
    delete radio_buttons2;
  }

  /*
  for(auto style : QStyleFactory::keys())
    printf("  STYLE: -%s\n", style.toUtf8().constData());
  */
  
  setStyleRecursively(menu, &g_my_proxy_style);

  /*
  menu->setStyleSheet("QMenu::indicator:exclusive:checked {  background-color: #ff00ff; }"
                      "QMenu::indicator:exclusive:unchecked {  background-color: #00ffff; }"
                      );
  */
  return menu;
}

void GFX_HoverMenuEntry(int entryid){
  if (g_curr_visible_actions.contains(entryid)){
    
    MyQAction *action = g_curr_visible_actions[entryid].data();
    if(action==NULL){
      R_ASSERT_NON_RELEASE(false);
      return;
    }
    
    action->activate(QAction::Hover);
    
  } else {

    g_last_hovered_menu_entry_guinum = -1;
    
  }
}

static int64_t GFX_QtMenu(
                          const vector_t &v,
                          func_t *callback2,
                          std::function<void(int,bool)> callback3,
                          bool is_async,
                          bool program_state_is_valid
                          )
{
  R_ASSERT_RETURN_IF_FALSE2(is_async==true, -1); // Lots of trouble with non-async menus. (triggers qt bugs)
  
  if(is_async){
    R_ASSERT(callback2!=NULL || callback3);
    R_ASSERT_RETURN_IF_FALSE2(program_state_is_valid, -1);
  }

  int result = -1;
  
  QMenu *menu = create_qmenu(v, is_async,  callback2, callback3, is_async ? NULL : &result);
  //printf("                CREATED menu %p", menu);
  
  if (is_async){

    safeMenuPopup(menu);
    return API_get_gui_from_existing_widget(menu);
    
  } else {
    
    QAction *action = safeMenuExec(menu, program_state_is_valid);
    (void)action;

    if (result >= v.num_elements){
      //R_ASSERT(false);
      return -1;
    }

    return result;
  }
}
int64_t GFX_Menu3(
              const vector_t &v,
              std::function<void(int,bool)> callback3
              )
{
  return GFX_QtMenu(v, NULL, callback3, true, true);
}

int64_t GFX_Menu2(
              struct Tracker_Windows *tvisual,
              ReqType reqtype,
              const char *seltext,
              const vector_t v,
              func_t *callback,
              bool is_async,
              bool program_state_is_valid
              )
{
  R_ASSERT_RETURN_IF_FALSE2(is_async==true, -1); // Lots of trouble with non-async menus. (triggers qt bugs)
  
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
  bool created_reqtype = reqtype==NULL;

  if (created_reqtype)
    reqtype = GFX_OpenReq(tvisual,-1, -1, "");
  
  int ret = GFX_ReqTypeMenu(tvisual,reqtype,seltext,v, program_state_is_valid);

  if (created_reqtype)
    GFX_CloseReq(tvisual, reqtype);

  return ret;
  
  //return GFX_Menu2(tvisual, reqtype, seltext, v, NULL, false, program_state_is_valid);
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

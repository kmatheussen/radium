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



// TODO: Write custom Menu GUI instead of using QMenu. The amount of customizations and ad-hoc bug fixes in here is ridiculous.




#if defined(__GNUC__) && !defined(__clang__)
#  include "../Qt/Qt_precompiled.hpp"
#endif

#if USE_QT_MENU

#ifndef SAFE_POPUP
  #if FOR_LINUX
    #define SAFE_POPUP 1 // X often freezes if a program crashes while running under gdb and a popup menu is open, and other situations where the Qt main loop is blocked from being called.
  #else
    #define SAFE_POPUP 0
  #endif
#endif

#if SAFE_POPUP
#include <sys/types.h>
#include <unistd.h>
#define CLOSE_TIME 80
#define KILL_TIME 100
#endif

#include <memory>
#include <functional>

#include <QStack>
#include <QAction>
#include <QWidgetAction>
#include <QCheckBox>
#include <QButtonGroup>
#include <QRadioButton>
#include <QMenu>
#include <QProxyStyle>
#include <QStyleFactory>
#include <QStyleOption>
#include <QMenuBar>
#include <QActionGroup>
#include <QLineEdit>
#include <QPalette>
#include <QSet>
#include <QPainter>
#include <QPointF>
#include <QPaintEvent>

#include "../common/nsmtracker.h"
#include "../common/visual_proc.h"
#include "../common/settings_proc.h"
#include "../common/FuzzySearch.h"
#include "../audio/Envelope.hpp"

#include "../embedded_scheme/s7extra_proc.h"

#include "../api/api_gui_proc.h"
#include "../api/api_requesters_proc.h"
#include "../api/api_common_proc.h"

#include "../api/api_proc.h"

#include "Qt_colors_proc.h"
#include "Timer.hpp"
#include "EditorWidget.h"

#include "Qt_Main_proc.h"
#include "Qt_PopupMenu_proc.h"
#include "Qt_bottom_bar_widget_proc.h"



QPointer<QWidget> g_current_parent_before_qmenu_opened; // Only valid if g_curr_popup_qmenu != NULL
QPointer<QMenu> g_curr_popup_qmenu;

int64_t g_last_hovered_menu_entry_guinum = -1;

static int g_menu_is_open = 0;
static QStack<QMenu* >g_curr_menu;  

extern QMenuBar *g_main_menu_bar;
extern QMenuBar *g_main_menu_bar_right;

int g_is_calling_from_menu = 0;

namespace{

  class MyQAction;
  static int64_t g_myactions_counter = 0;
  static QHash<int64_t, QPointer<MyQAction>> g_curr_visible_actions;
  static QHash<int64_t, QPointer<MyQAction>> g_actions_by_guinum;

  struct MyProxyStyle : public QProxyStyle {
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
    
    int pixelMetric(QStyle::PixelMetric metric, const QStyleOption* option = 0, const QWidget* widget = 0 ) const override {
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
    QString _text; // for logging
    radium::ProtectedS7Extra<func_t*> callback;
    std::function<void(int,bool)> callback3;
    int *result;

    bool is_checkable;

    Callbacker(QMenu *qmenu, int num, bool is_async, QString text, func_t *callback, std::function<void(int,bool)> callback3, int *result, bool is_checkable)
      : qmenu(qmenu)
      , num(num)
      , _text(text)
      , callback(callback, "popupmenu_callback")
      , callback3(callback3)
      , result(result)
      , is_checkable(is_checkable)
    {
      R_ASSERT(is_async==true); // Lots of trouble with non-async menus. (triggers qt bugs)
      
      if(is_async)
        R_ASSERT(result==NULL);

      g_alive_callbackers++;
      //printf("    Created Callbacker %p. callbackers: %d. pos: %d\n", this, g_alive_callbackers, (int)this->callback._pos);
    }

    ~Callbacker(){
      R_ASSERT(_waiting_for_callback==false);
      R_ASSERT_NON_RELEASE(g_alive_callbackers > 0);
      g_alive_callbackers--;
      //printf("            CALLBACKER %p deleted 2. Alive now: %d. pos: %d.\n", this, g_alive_callbackers, (int)callback._pos);
    }
    
  private:
    bool checked = false;

    bool run_have_run = false;

    std::shared_ptr<Callbacker> _extra_destroyed_slot_argument; // Temporarily store it in the object since you can't post custom data to signals handlers in a less inelegant way, I think. The use of smart pointers here is a mess BTW, as it always is when using smart pointers, for everyone, in all possible situations, without exception.

    void run_and_delete(bool checked, std::shared_ptr<Callbacker> &callbacker){
      if (run_have_run)
        return; // Can happen on macos and windows if called from GFX_RightclickMenuEntry (i.e. triggered twice).
      
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

      MOUSE_CYCLE_schedule_unregister_all(true); // Actions are cached, so custom widgets are not deleted when menu is closed.

      //if (callback.v==NULL && callback3==NULL)
      //  delete this;

      if (g_alive_callbackers > 1000){
        //R_ASSERT_NON_RELEASE(false);
        printf("  Warning: popup menu might leak: %d.\n", g_alive_callbackers);
      }
    }

    
  public:
    
    void run_callbacks(void){
      EVENTLOG_add_event(talloc_format("popup menu: %s", _text.toUtf8().constData()));

      g_is_calling_from_menu++;
      
      if (callback.v != NULL){
        if (is_checkable)
          S7CALL(void_int_bool, callback.v, num, checked);
        else
          S7CALL(void_int, callback.v, num);
      }
        
      if (callback3)
        callback3(num, checked);

      g_is_calling_from_menu--;
    }

    
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
    
    void call_callback_and_delete(const std::shared_ptr<Callbacker> &callbacker){
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


static QVector<std::function<void(void)>> g_pending_menu_callbacks;

static void flush_pending_menu_callbacks(int num_tries)
{
	if (g_pending_menu_callbacks.empty())
		return;

	if (GFX_MenuActive())
	{
		if (num_tries < 100)
			QTimer::singleShot(50, [num_tries]
			{
				flush_pending_menu_callbacks(num_tries+1);
			});
		return;
	}

	QVector<std::function<void(void)>> callbacks;
	
	callbacks.swap(g_pending_menu_callbacks);

	for (auto &callback : callbacks)
		callback();
}

static void schedule_flush_pending_menu_callbacks(void)
{
	if (g_pending_menu_callbacks.empty())
		return;

	QTimer::singleShot(0, []
	{
		flush_pending_menu_callbacks(0);
	});
}

  static QPointer<QWidget> g_last_hovered_widget;
  static QPointer<MyQAction> g_last_hovered_myaction;
  
  class MyQAction : public QWidgetAction {
    Q_OBJECT

  public:

    std::shared_ptr<Callbacker> _callbacker;
    
    int64_t _guinum = -1;
    QWidget *_widget = NULL;
    QPointer<QMenu> _menu = NULL; // The menu this entry is currently in. Can be a sub menu.
    
    QAbstractButton *_checkbox = NULL; // can be either QCheckBox or QRadioButton
    
    bool _success = true;

    int64_t _entry_id;

#if !defined(RELEASE)
    QString _text; // only used for debugging
#endif
    
    MyQAction(const QString &text, const QString &shortcut, int shortcut_width, std::shared_ptr<Callbacker> &callbacker, bool is_checkbox, bool is_checked, bool is_radiobutton, bool is_first, bool is_last, QObject *parent = NULL)
      : QWidgetAction(parent)
      , _callbacker(callbacker)
      , _entry_id(g_myactions_counter++)
#if !defined(RELEASE)
      , _text(text)
#endif
    {

      //printf("   ...ADDING %p/%p into cache. C: %ld\n", this, _callbacker.get(), _callbacker.use_count());
      g_curr_visible_actions[_entry_id] = this;
      
      S7EXTRA_GET_FUNC(s_func, "FROM_C-create-menu-entry-widget");

      _guinum = S7CALL(int_int_charpointer_charpointer_int_bool_bool_bool_bool_bool, s_func, _entry_id, text.toUtf8().constData(), shortcut.toUtf8().constData(), shortcut_width, is_checkbox, is_checked, is_radiobutton, is_first, is_last);

      if (_guinum > 0){
        
        g_actions_by_guinum[_guinum] = this;

        _widget = API_gui_get_widget(_guinum);
        setDefaultWidget(_widget);
        //_widget->setStyle(&g_my_proxy_style);

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

    ~MyQAction(){
      //printf("   ...Removing %p/%p from cache. C: %ld\n", this, _callbacker.get(), _callbacker.use_count());
      g_curr_visible_actions.remove(_entry_id);
      if (_guinum > 0)
        g_actions_by_guinum.remove(_guinum);
      //delete _widget;
    }
    
    // Called by MyQMenu. (workaround for stupid separator behaviour in Qt)
    virtual bool my_clicked(void){
      return false; // don't eat mouse press event
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

  public:

    // Set for radio buttons (the group the entry belongs to), NULL for checkboxes.
    QActionGroup *_radio_group;

    ~CheckableAction(){
      //printf("I was deleted: %s\n",text.toUtf8().constData());
    }

    CheckableAction(const QString & text_b, const QString &shortcut, int shortcut_width, bool is_on, QActionGroup *radio_group, bool is_first, bool is_last, std::shared_ptr<Callbacker> &callbacker)
      : MyQAction(text_b, shortcut, shortcut_width, callbacker, true, is_on, radio_group != NULL, is_first, is_last, callbacker->qmenu)
      , _radio_group(radio_group)
    {
      setCheckable(true);
      setChecked(is_on);
      connect(this, SIGNAL(toggled(bool)), this, SLOT(toggled(bool)));
    }

    bool my_clicked(void) override{
      setChecked(!isChecked());
      //toggled(!isChecked());
      return true;  // eat mouse press event
    }

    // Called when pressing space on this entry. Same as clicking it, except that the
    // menu stays open, i.e. the callback is not run through run_and_delete_checked().
    void toggle_without_closing_menu(void)
    {
      if (_radio_group != NULL) {

        if (isChecked())
          return; // A checked radio button can not be unchecked by clicking it.

        // Uncheck the other radio buttons in the group. QActionGroup exclusivity is
        // signal-based (and the signals are blocked below), so it has to be done manually.
        QAbstractButton *widget_to_uncheck = NULL;

        for (QAction *action : _radio_group->actions()) {
          auto *other = dynamic_cast<CheckableAction*>(action);

          if (other != NULL && other != this && other->isChecked()) {
            {
              QSignalBlocker blocker(other);
              other->setChecked(false);
            }
            if (other->_checkbox != NULL && other->_checkbox->isChecked())
              widget_to_uncheck = other->_checkbox;
          }
        }

        // Checking the widget below makes its QButtonGroup uncheck widget_to_uncheck.
        // Its signals must be blocked too, to avoid running the callback-with-close path.
        QSignalBlocker uncheck_blocker(widget_to_uncheck);

        {
          QSignalBlocker blocker(this);
          setChecked(true);
        }

        if (_checkbox != NULL) {
          QSignalBlocker blocker(_checkbox);
          _checkbox->setChecked(true);
        }

        if (_callbacker != NULL)
          _callbacker->run_checked(true);

      } else {

        const bool checked = !isChecked();

        // Block the signals so that toggled() (which closes the menu) is not called.
        {
          QSignalBlocker blocker(this);
          setChecked(checked);
        }

        if (_checkbox != NULL) {
          QSignalBlocker blocker(_checkbox);
          _checkbox->setChecked(checked);
        }

        if (_callbacker != NULL)
          _callbacker->run_checked(checked);
      }
    }

  public slots:
    void toggled(bool checked){
      _callbacker->run_and_delete_checked(checked, _callbacker);
    }
  };

  class ClickableAction;
  
  static QMultiHash<QString,ClickableAction*> g_clickable_actions;  // Reuse ClickableAction actions. Speeds up plugin menu a lot.

  class ClickableAction : public MyQAction
  {
    Q_OBJECT;
    
  public:
    
    QString _text;
    QString _shortcut;
    bool _is_first, _is_last;
    bool _is_permanent;
    int _shortcut_width;
    
    ~ClickableAction(){
      //printf("I was deleted: %s\n",_text.toUtf8().constData());
      R_ASSERT_NON_RELEASE(false);
      g_clickable_actions.remove(_text, this);
    }
    
    ClickableAction(const QString & text, const QString &shortcut, int shortcut_width, bool is_first, bool is_last, bool is_permanent, std::shared_ptr<Callbacker> &callbacker)
      : MyQAction(text, shortcut, shortcut_width, callbacker, false, false, false, is_first, is_last, callbacker->qmenu)
      , _text(text)
      , _shortcut(shortcut)
      , _is_first(is_first)
      , _is_last(is_last)
      , _is_permanent(is_permanent)
      , _shortcut_width(shortcut_width)
    {
      //printf("  ___4 count %p: %ld\n", callbacker.get(), callbacker.use_count());
      connect(this, SIGNAL(triggered()), this, SLOT(triggered()));      
    }

    /*
    void contextMenuEvent(QContextMenuEvent *event) override {
      printf("CONTEXT_MENU_EVENT\n");
    }
    */
    
    bool my_clicked(void) override{
      //printf("MY_CLICKED\n");
      triggered();
      return true; // eat mouse press event
    }

  public slots:
    void triggered(){
      //printf("TRIGGERED\n");
      if(_callbacker.get()==NULL){
        //R_ASSERT_NON_RELEASE(false); // Happens very often. Don't know if it's a problem. Better not to crash the system in non-release mode, because then we will never test what happens in release mode.
        printf("ERROR! QT_POPUPMENU/triggered: _callbacker.get()==NULL\n");
        return;
      }

		if (_is_permanent)
		{
			std::shared_ptr<Callbacker> callbacker = _callbacker;
			GFX_call_when_menus_are_closed([callbacker]
			{
				callbacker->run_callbacks();
			});
		}
		else
			_callbacker->run_and_delete_clicked(_callbacker);
    }
  };

  static void release_clickable_action(ClickableAction *action){    
    action->setParent(NULL);
    action->_callbacker = NULL;
    action->_menu = NULL;
    action->setVisible(true);
    
    g_clickable_actions.insert(action->_text, action);

    //printf("    clickable size: %d\n", g_clickable_actions.size());
  }

  static ClickableAction *get_clickable_action(const QString & text, const QString &shortcut, int shortcut_width, bool is_first, bool is_last, bool is_permanent, std::shared_ptr<Callbacker> &callbacker){

    if (!is_permanent)
      for(ClickableAction *action : g_clickable_actions.values(text))
        if (action->_shortcut == shortcut && action->_is_first==is_first && action->_is_last==is_last && shortcut_width==action->_shortcut_width){
          g_clickable_actions.remove(text, action);
          action->_callbacker = callbacker;
          action->setEnabled(true); // Might have been set to disabled last time.
          action->setVisible(true); // Might have been hidden by the popup search filter last time.
          return action;
        }

    return new ClickableAction(text, shortcut, shortcut_width, is_first, is_last, is_permanent, callbacker);
  }

  
  
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
      //setEnabled(false);
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

    /*
    bool my_clicked(void) override {
      triggered();
      return true; // eat mouse press event
    }
    */

  public slots:
    void triggered(){
      callbacker->run_and_delete_clicked(callbacker);
    }
  };


  static const int POPUP_SEARCH_MIN_NUM_ENTRIES = 3;

  class PopupSearchAction : public QWidgetAction
  {
  public:

    PopupSearchAction(QObject *parent)
      : QWidgetAction(parent)
    {
    }

    bool event(QEvent *event) override
    {
      const bool ret = QWidgetAction::event(event);

      QWidget *widget = defaultWidget();

      if (event->type()==QEvent::ActionChanged && widget != NULL && widget->isEnabled()==false)
        widget->setEnabled(true);

      return ret;
    }
  };

  struct PopupSearchEntry
  {
    QAction *action;
    QString folded_text;
  };

  struct PopupSearchSubmenu
  {
    QAction *action;
    QMenu *menu;
    QString folded_title;
  };

  struct PopupSearchMenu
  {
    QVector<PopupSearchEntry> entries;
    QVector<QAction*> separators;
    QVector<PopupSearchSubmenu> submenus;
  };

  struct PopupSearchState
  {
	  QPointer<QMenu> root_menu;
	  QPointer<QLineEdit> lineedit;
	  QPointer<QWidgetAction> lineedit_action;
	  QHash<QMenu*, PopupSearchMenu> menus;
	  QSet<QMenu*> all_menus;
	  QString query;
	  // Sub menus opened automatically to show the result of the search, in the order they
	  // were opened (from the root menu and downwards).
	  QVector<QPointer<QMenu>> auto_opened_menus;
  };

  static std::shared_ptr<PopupSearchState> g_popup_search_state;

  // Used when a fuzzy search for all main menu entries is requested from an open main menu.
  static QString g_pending_popup_search_query;
  static QPoint g_pending_popup_position;
  static bool g_has_pending_popup_position = false;
  // True when g_pending_popup_position is the position of the hamburger button, in which case
  // the popup menu is placed above it with the bottom edge at the top edge of the button.
  static bool g_pending_popup_position_is_hamburger = false;

  // The hamburger button in the main window's bottom bar. Used when opening the popup menu with left alt.
  static QPointer<QWidget> g_hamburger_button;

  // The root menu of the open hamburger popup menu. Used so that left alt can close it.
  static QPointer<QMenu> g_hamburger_popup;

  // True when the next popup menu to open is the instrument-select popup (opened by F12 or by
  // clicking the track header name). Set by GFX_set_track_header_popup_position.
  static bool g_next_popup_is_instrument_popup = false;

  // The root menu of the open instrument-select popup. Used so that F12 can close it.
  static QPointer<QMenu> g_instrument_popup;

  struct PopupSearchResult
  {
    bool has_visible_entries;
    QAction *best_action;
    int best_score;
  };

static void schedule_auto_open_search_result_path(void);

  static void set_active_action_if_necessary(QMenu *menu, QAction *best_action, QAction *lineedit_action)
  {
    if (best_action != NULL)
    {
      menu->setActiveAction(best_action);
      return;
    }

    QAction *active_action = menu->activeAction();

    if (active_action != NULL && active_action != lineedit_action && active_action->isVisible() && active_action->isEnabled())
      return;

	for (QAction *action : menu->actions())
	{
		// Selecting an entry with a sub menu as active action makes Qt open that sub
		// menu from within the parent menu's Show event (QMenu::event). The Show event
		// is sent before the menu is registered in Qt's popup stack, so the sub menu
		// would be registered before its parent menu, and Qt would then send the
		// keyboard events to the parent menu instead of to the sub menu. Only menus
		// that are not shown yet can be affected by this, so don't select a sub menu
		// entry in those. (The entry is selected when the menu is opened instead.)
		if (action->menu() != NULL && !menu->isVisible())
			continue;

		if (action->isVisible() && !action->isSeparator() && action->isEnabled() && action != lineedit_action)
		{
			menu->setActiveAction(action);
			break;
		}
	}
  }

  static void apply_popup_search_filter(void)
  {
    PopupSearchState *state = g_popup_search_state.get();

    if (state == NULL)
      return;

    if (state->lineedit.isNull() || state->root_menu.isNull() || state->lineedit_action.isNull())
      return;

    const radium::FuzzySearchQuery search(state->query);
    const bool filtering = !search.empty();

    std::function<PopupSearchResult(QMenu*,bool)> doit = [&](QMenu *menu, bool force_visible) -> PopupSearchResult
    {
      PopupSearchMenu &search_menu = state->menus[menu];

      PopupSearchResult result = {force_visible, NULL, -1};

      for (const PopupSearchEntry &entry : search_menu.entries)
      {
        const int score = (force_visible || !filtering) ? 0 : search.score(entry.folded_text);
        const bool visible = force_visible || !filtering || score >= 0;

        entry.action->setVisible(visible);

        MyQAction *myaction = dynamic_cast<MyQAction*>(entry.action);

        if (myaction != NULL && myaction->_widget != NULL)
          myaction->_widget->setVisible(visible); // QMenu doesn't hide widgets of hidden QWidgetActions.

        if (visible)
        {
          result.has_visible_entries = true;

          if (entry.action->isEnabled() && score > result.best_score)
          {
            result.best_score = score;
            result.best_action = entry.action;
          }
        }
      }

      for (QAction *separator : search_menu.separators)
        separator->setVisible(!filtering || force_visible);

      QAction *best_own_action = result.best_action;

      for (const PopupSearchSubmenu &submenu : search_menu.submenus)
      {
        const bool child_force_visible = force_visible || (filtering && search.matches(submenu.folded_title));

        const PopupSearchResult child_result = doit(submenu.menu, child_force_visible);
        const bool visible = child_force_visible || child_result.has_visible_entries;

        submenu.action->setVisible(visible);

        if (visible)
          result.has_visible_entries = true;

        if (child_result.best_action != NULL && child_result.best_score > result.best_score)
        {
          result.best_score = child_result.best_score;
          result.best_action = child_result.best_action;
        }
      }

		if (filtering && !force_visible)
		{
			QAction *header = NULL;

			for (QAction *action : menu->actions())
			{
				if (action == state->lineedit_action.data())
					continue;

				if (action->isSeparator())
				{
					if (!action->text().isEmpty())
						header = action;
					continue;
				}

				if (action->isVisible() && header != NULL)
				{
					header->setVisible(true);
					header = NULL;
				}
			}
		}

      set_active_action_if_necessary(menu, best_own_action, state->lineedit_action.data());

      return result;
    };

    doit(state->root_menu.data(), false);

    state->lineedit_action->setVisible(true);

    QLineEdit *lineedit = state->lineedit.data();

    if (lineedit->text() != state->query)
      lineedit->setText(state->query);

    lineedit->setCursorPosition(lineedit->text().size());

	// Applied on the next event loop iteration, so the popup is visible and so that we never
	// open (or close) a sub menu from within a key press event handler.
	schedule_auto_open_search_result_path();
  }

  // Fully deactivate the main menu bars, including Qt's internal menu bar keyboard/popup state.
  // Needed when the main menu search popup takes over from an Alt-opened main menu. Otherwise
  // QMenuBar can be left in "open" mode, re-opening menus when the mouse hovers over the menu bar.
  static void deactivate_main_menu_bars(void)
  {
    QMenuBar *menu_bars[2] = {g_main_menu_bar, g_main_menu_bar_right};

    for (QMenuBar *menu_bar : menu_bars)
    {
      if (menu_bar == NULL)
        continue;

      menu_bar->setActiveAction(NULL);

      // Qt clears QMenuBarPrivate::popupState and keyboardState when receiving Escape
      // (the QKeySequence::Cancel path in QMenuBar::keyPressEvent).
      QKeyEvent *event = new QKeyEvent(QEvent::KeyPress, Qt::Key_Escape, Qt::NoModifier);
      qApp->postEvent(menu_bar, event);
    }
  }

  // Called when the last menu is closed or destroyed.
  // Note: Do NOT deactivate the main menu bars here. Menus are deliberately closed
  // as part of QMenuBar's own left/right navigation (QMenuBarPrivate::setCurrentAction
  // hides the current menu before opening the next), and deactivating the bar at that
  // point clears its state and makes the following menu close instead of opening.
  static void menu_state_reached_zero(void)
  {
    schedule_flush_pending_menu_callbacks();

    // Move Qt focus away from the main menu bar. Otherwise Qt can keep its internal
    // menu bar keyboard/popup state, and menus are re-opened when the mouse hovers
    // over the menu bar.
    QTimer::singleShot(20, []{
      if (GFX_MenuActive())
        return;

      QWidget *focus = QApplication::focusWidget();

      if (focus == NULL || qobject_cast<QMenuBar*>(focus) != NULL)
        set_editor_focus();
    });
  }

  // Left/right arrow navigation in the instrument popup closes the current popup and opens a new
  // one. Normally the keyboard focus counting drops to zero when the old menu hides (un-greying
  // the editor) and goes back to one when the new menu shows (greying it again), which triggers
  // two unnecessary editor redraws. Hold an extra keyboard focus reference over the transition
  // instead, so the editor is left untouched.
  static int g_pending_keyboard_focus_holds = 0;

  static void release_pending_keyboard_focus_holds(void){
    if (g_pending_keyboard_focus_holds==0)
      return;

    if (g_radium_runs_custom_exec){
      // The keyboard focus counting functions don't do anything while running custom exec.
      QTimer::singleShot(100, release_pending_keyboard_focus_holds);
      return;
    }

    while (g_pending_keyboard_focus_holds > 0){
      g_pending_keyboard_focus_holds--;
      release_keyboard_focus_counting();
    }
  }

  // Called just before the current popup is closed, if a new popup is going to be opened
  // immediately after (i.e. by the value of the :on-left-key/:on-right-key options).
  static void hold_keyboard_focus_for_next_popup(void){
    if (g_radium_runs_custom_exec)
      return;

    obtain_keyboard_focus_counting();
    g_pending_keyboard_focus_holds++;

    // In case the new popup is never opened (for instance if there is no track to move to).
    QTimer::singleShot(500, []{
        if (g_pending_keyboard_focus_holds > 0)
          release_pending_keyboard_focus_holds();
      });
  }

  struct MyQMenu : public QMenu /* , radium::Timer */ {
    Q_OBJECT;

  public:
    
    int _shortcut_width;

    bool _has_g_menu_is_open = false;

    bool _is_part_of_main_menu = false;

    // Set when the popup menu contains an "[on-left-key]"/"[on-right-key]" entry (see
    // popupmenu.scm and create_qmenu). Only set on the root menu of a popup, never on sub menus.
    std::shared_ptr<Callbacker> _on_left_key_callbacker;
    std::shared_ptr<Callbacker> _on_right_key_callbacker;

    // Set to true when the mouse has moved after this menu was shown. Used to avoid that the
    // entry under the mouse pointer becomes current when the menu is opened, like Qt does.
    bool _mouse_has_moved = false;

    // The menu's current action the last time we updated the entry widgets. The entry widgets
    // paint the current-entry marker based on the menu's active action, so the widget for the
    // previous current entry must be repainted when the current entry changes.
    QPointer<QAction> _last_current_action;

    // Workaround. Sometimes, aboutToHide and/or aboutToShow is not called.
    struct Workaround : public radium::Timer {
      
      MyQMenu *_myqmenu;
      
      Workaround(MyQMenu *myqmenu)
        : radium::Timer((int)scale_int64(rand(), 0, RAND_MAX, 200, 500), true) 
        , _myqmenu(myqmenu)
      {
      }
      
      void calledFromTimer(void) override {
        if (_myqmenu->isVisible())
          _myqmenu->setopen();
        else
          _myqmenu->setclosed();
      }
    };

    Workaround _workaround;
        
    MyQMenu(QWidget *parent, QString title, int shortcut_width)      
      : QMenu(title, parent)
        //, radium::Timer((int)scale_int64(qrand(), 0, RAND_MAX, 200, 500), true)
      , _shortcut_width(shortcut_width)
      , _workaround(this)
    {
      setMenuColors(this);
      connect(this, SIGNAL(aboutToHide()), this, SLOT(aboutToHide()));
      connect(this, SIGNAL(aboutToShow()), this, SLOT(aboutToShow()));
      connect(this, SIGNAL(hovered(QAction*)), this, SLOT(hovered(QAction*)));
    }

    ~MyQMenu()
    {
      // Sometimes aboutToHide is not called before the menu is destroyed.
      // Clean up the global menu state here as well, otherwise g_menu_is_open
      // leaks, GFX_MenuActive() stays true forever, and all keys are eaten by
      // menu navigation.
      if (_has_g_menu_is_open){
        g_menu_is_open--;
        _has_g_menu_is_open = false;
        g_curr_menu.removeAll(this);
        if (g_menu_is_open==0)
          menu_state_reached_zero();
      }

      QVector<ClickableAction*> to_remove;
      
      for(auto a : actions()){
        auto *c = dynamic_cast<ClickableAction*>(a);
        if (c != NULL)
          to_remove.push_back(c);
      }
      
      for(auto *a : to_remove){
        removeAction(a);
        release_clickable_action(a);
      }
    }
#if 0
    void contextMenuEvent(QContextMenuEvent *event) override {
      printf("QMENU CONTEXT_MENU_EVENT\n");
      event->ignore();
    }
#endif
  private:
    
    void setclosed(void){
      if (_has_g_menu_is_open){
        g_menu_is_open--;
        _has_g_menu_is_open = false;
        // Remove this menu specifically instead of popping the top of the stack.
        // Menus are not always closed in the reverse order they were opened, and
        // aboutToHide is sometimes not called at all (handled in the destructor).
        g_curr_menu.removeAll(this);

			if (g_menu_is_open==0)
				menu_state_reached_zero();
      }
    }
    
    void setopen(void){
      if (!_has_g_menu_is_open){
        g_menu_is_open++;
        _has_g_menu_is_open = true;
        g_curr_menu.push(this);
      }
    }
    
  public:

    void navigate_qt(Qt::Key key)
    {
      QKeyEvent event(QEvent::KeyPress, key, Qt::NoModifier);
      QMenu::keyPressEvent(&event);
    }

    bool handle_search_key(QKeyEvent *event)
    {
      PopupSearchState *state = g_popup_search_state.get();

      if (state == NULL || state->lineedit_action.isNull() || !state->all_menus.contains(this))
        return false;

      const int key = event->key();
      const Qt::KeyboardModifiers modifiers = event->modifiers();
      QAction *lineedit_action = state->lineedit_action.data();

      if (key==Qt::Key_Backspace || key==Qt::Key_Delete)
      {
        if (state->query.isEmpty())
          return false;

        state->query.chop(1);
        apply_popup_search_filter();
        event->accept();
        return true;
      }

      if (key==Qt::Key_Escape)
      {
        if (state->query.isEmpty())
          return false;

        state->query = "";
        apply_popup_search_filter();
        event->accept();
        return true;
      }

      if (key==Qt::Key_Up || key==Qt::Key_Down || key==Qt::Key_PageUp || key==Qt::Key_PageDown || key==Qt::Key_Home || key==Qt::Key_End)
      {
        QVector<QAction*> selectable_actions;

        for (QAction *action : actions())
          if (action->isVisible() && !action->isSeparator() && action->isEnabled() && action != lineedit_action)
            selectable_actions.push_back(action);

        if (selectable_actions.isEmpty())
          return false;

        if (key==Qt::Key_Home || key==Qt::Key_End)
        {
          const int target_index = key==Qt::Key_Home ? 0 : selectable_actions.size()-1;
          const Qt::Key nav_key = key==Qt::Key_Home ? Qt::Key_Up : Qt::Key_Down;

          for (int i = 0 ; i < selectable_actions.size()+1 ; i++)
          {
            if (selectable_actions.indexOf(activeAction()) == target_index)
              break;

            navigate_qt(nav_key);
          }
        }
        else
        {
          int steps = 1;

          if (key==Qt::Key_PageUp || key==Qt::Key_PageDown)
            steps = 10;
          else if ((modifiers & Qt::ShiftModifier) != 0)
            steps = 4;

          const Qt::Key nav_key = (key==Qt::Key_Down || key==Qt::Key_PageDown) ? Qt::Key_Down : Qt::Key_Up;

          for (int i = 0 ; i < steps ; i++)
          {
            const int index_before = selectable_actions.indexOf(activeAction());

            navigate_qt(nav_key);

            if (selectable_actions.indexOf(activeAction()) == index_before)
            {
              // Qt doesn't wrap around at the top/bottom (e.g. on macOS), so go the
              // long way around to select the last/first entry.
              if (nav_key == Qt::Key_Down)
              {
                for (int j = 0 ; j < selectable_actions.size() ; j++)
                {
                  if (selectable_actions.indexOf(activeAction()) == 0)
                    break;

                  navigate_qt(Qt::Key_Up);
                }
              }
              else
              {
                for (int j = 0 ; j < selectable_actions.size() ; j++)
                {
                  if (selectable_actions.indexOf(activeAction()) == selectable_actions.size()-1)
                    break;

                  navigate_qt(Qt::Key_Down);
                }
              }
            }
          }
        }

        event->accept();
        return true;
      }

      if (key==Qt::Key_Return || key==Qt::Key_Enter)
      {
        QAction *active_action = activeAction();

        if (active_action != NULL && active_action != lineedit_action && active_action->isVisible() && active_action->isEnabled())
          return false;

        for (QAction *action : actions())
          if (action->isVisible() && !action->isSeparator() && action->isEnabled() && action != lineedit_action)
          {
            setActiveAction(action);
            break;
          }

        return false;
      }

      if ((modifiers & (Qt::ControlModifier | Qt::AltModifier | Qt::MetaModifier)) != 0)
        return false;

      const QString text = event->text();

      if (text.isEmpty() || !text.at(0).isPrint())
        return false;

      state->query += text;
      apply_popup_search_filter();
      event->accept();
      return true;
    }

	static bool is_search_key(QKeyEvent *event)
	{
		if ((event->modifiers() & (Qt::ControlModifier | Qt::AltModifier | Qt::MetaModifier)) != 0)
			return false;

		switch(event->key())
		{
			case Qt::Key_Space:
			case Qt::Key_Backspace:
			case Qt::Key_Delete:
			case Qt::Key_Escape:
			case Qt::Key_Return:
			case Qt::Key_Enter:
			case Qt::Key_Tab:
			case Qt::Key_Backtab:
			case Qt::Key_Up:
			case Qt::Key_Down:
			case Qt::Key_Left:
			case Qt::Key_Right:
			case Qt::Key_PageUp:
			case Qt::Key_PageDown:
			case Qt::Key_Home:
			case Qt::Key_End:
				return false;
		}

		const QString text = event->text();

		return !text.isEmpty() && text.at(0).isPrint();
	}

	MyQMenu *get_root_menu(void)
	{
		MyQMenu *menu = this;

		while (true)
		{
			MyQMenu *parent = qobject_cast<MyQMenu*>(menu->parentWidget());

			if (parent == NULL)
				return menu;

			menu = parent;
		}
	}

	void close_main_menus(void)
	{
		QVector<MyQMenu*> menus;
		MyQMenu *menu = this;

		while (menu != NULL)
		{
			menus.push_back(menu);
			menu = qobject_cast<MyQMenu*>(menu->parentWidget());
		}

		// Hide from the deepest menu and upwards. (hiding a submenu does not hide its parent menu)
		for (MyQMenu *menu2 : menus)
			menu2->hide();

		if (g_main_menu_bar != NULL)
			g_main_menu_bar->setActiveAction(NULL);

		if (g_main_menu_bar_right != NULL)
			g_main_menu_bar_right->setActiveAction(NULL);
	}

	void mouseMoveEvent(QMouseEvent *event) override
	{
		_mouse_has_moved = true;

		PopupSearchState *state = g_popup_search_state.get();

		if (state != NULL
		    && state->root_menu == this
		    && state->lineedit_action != NULL
		    && activeAction() != NULL
		    && activeAction() != state->lineedit_action.data()
		    && actionAt(event->position().toPoint()) == state->lineedit_action.data())
		{
			// Don't clear the selected entry when the mouse moves over the search field.
			return;
		}

		QMenu::mouseMoveEvent(event);
	}

    void keyPressEvent(QKeyEvent *event) override {
      
      evalScheme("(set! *last-pressed-menu-entry-widget-mouse-button* -1)"); // To avoid keybindings-configuration menu to pop up when pressing return.

      // Space toggles/selects the current entry if it is a checkable entry (checkbox or
      // radio button). Unlike a mouse click, the popup stays open. For all other entries,
      // space should do nothing. If a popup search is in progress, space is instead added
      // to the search query (see handle_search_key below).
      if (event->key()==Qt::Key_Space
          && (event->modifiers() & (Qt::ControlModifier|Qt::AltModifier|Qt::MetaModifier))==0)
      {
        PopupSearchState *state = g_popup_search_state.get();

        // Same conditions as handle_search_key uses to decide that a key belongs to the search query.
        const bool search_in_progress = (state != NULL
                                         && state->lineedit_action != NULL
                                         && state->all_menus.contains(this)
                                         && !state->query.isEmpty());

        if (!search_in_progress)
        {
          QAction *active_action = activeAction();
          auto *checkable_action = dynamic_cast<CheckableAction*>(active_action);

          event->accept();

          if (checkable_action != NULL && active_action->isVisible() && active_action->isEnabled())
            checkable_action->toggle_without_closing_menu();

          return;
        }
      }

      if (handle_search_key(event))
        return;

      // Entries added with the :on-left-key/:on-right-key options (see popupmenu.scm).
      if (event->key()==Qt::Key_Left || event->key()==Qt::Key_Right) {
        std::shared_ptr<Callbacker> callbacker = event->key()==Qt::Key_Left ? _on_left_key_callbacker
                                                                             : _on_right_key_callbacker;
        if (callbacker != NULL) {
          QAction *active_action = activeAction();

          // Right on an enabled sub menu entry must open the sub menu. Let the event fall
          // through to QMenu::keyPressEvent below in that case. (Left inside an open sub menu
          // navigates back to the parent menu, but sub menus never have these callbacks, so
          // that case is handled by Qt by itself.)
          const bool right_opens_submenu = (event->key()==Qt::Key_Right
                                            && active_action != NULL
                                            && active_action->isEnabled()
                                            && active_action->menu() != NULL);

          if (!right_opens_submenu) {
            event->accept();
            hold_keyboard_focus_for_next_popup();
            callbacker->run_and_delete_clicked(callbacker);
            return;
          }
        }
      }

      if (_is_part_of_main_menu && is_search_key(event))
      {
        printf("SEARCHPOPUP hook-start %f\n", TIME_get_ms());

        const QPoint menu_pos = get_root_menu()->mapToGlobal(QPoint(0, 0));
        g_pending_popup_search_query = event->text();
        GFX_set_pending_popup_position(menu_pos.x(), menu_pos.y(), false);

        close_main_menus();
        deactivate_main_menu_bars();
        GFX_stop_making_main_menu_active();
        OS_SYSTEM_reset_menu_navigation();

        printf("SEARCHPOPUP hook-eval-start %f\n", TIME_get_ms());
        evalScheme("(popup-search-all-menus)");
        printf("SEARCHPOPUP hook-eval-done %f\n", TIME_get_ms());

        event->accept();
        return;
      }

#if defined(FOR_MACOSX)
	  //
	  // Workaround for wrongly designed OS.
	  // (wrap-around is specifically prevented on MacOs by design, sigh!)
	  //
      if (event->spontaneous() && actions().size() > 0)
	  {
		  bool is_up   = event->key()==Qt::Key_Up   || event->key()==Qt::Key_PageUp   || event->key()==Qt::Key_Home;
		  bool is_down = event->key()==Qt::Key_Down || event->key()==Qt::Key_PageDown || event->key()==Qt::Key_End;
		  
		  if (is_up || is_down)
		  {
			  auto *curr_action = activeAction();
          
			  if (curr_action == actions().first() && is_up) {
				  setActiveAction(actions().last());
				  event->accept();
				  return;
			  } else if (curr_action == actions().last() && is_down) {
				  setActiveAction(actions().first());
				  event->accept();
				  return;
			  }
		  }
      }
#endif
	  
      bool custom_treat
        = ((event->modifiers() & Qt::ShiftModifier) && (event->key()==Qt::Key_Up || event->key()==Qt::Key_Down))
        || (event->key()==Qt::Key_PageUp || event->key()==Qt::Key_PageDown)
        || (event->key()==Qt::Key_Home || event->key()==Qt::Key_End);

      if (custom_treat && actions().size() > 0)
	  {
		  bool key_down = event->key()==Qt::Key_Down || event->key()==Qt::Key_PageDown;
#if !defined(FOR_MACOSX)
		  bool key_up = !key_down;
#endif
		  
		  int inc = key_down ? 1 : -1;
		  
		  auto *curr_action = activeAction();

#if !defined(FOR_MACOSX)
		  bool is_at_top = curr_action==actions().first();
		  bool is_at_bot = curr_action==actions().last();
#endif
		  
		  int min_num_steps = 4;
		  if (event->key()==Qt::Key_PageUp || event->key()==Qt::Key_PageDown)
			  min_num_steps = 10;
		  else if (event->key()==Qt::Key_Home || event->key()==Qt::Key_End)
			  min_num_steps = 1000000;

#if !defined(FOR_MACOSX)
		  if (is_at_top && key_up){
			  
			  send_key_up(this, 1);
			  
		  } else if (is_at_bot && key_down){
			  
			  send_key_down(this, 1);

		  }
		  else
#endif
		  {
			  
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

    // Qt paints the entries that open sub menus by itself (they are not widget actions),
    // so draw the current-entry circle on those here, matching the circle that
    // FROM_C-create-menu-entry-widget draws for normal entries.
    void paintEvent(QPaintEvent *event) override
    {
      QMenu::paintEvent(event);

      QAction *active = activeAction();

      if (active==NULL || active->menu()==NULL || active->isVisible()==false || active->isEnabled()==false)
        return;

      const QRect rect = actionGeometry(active);

      if (rect.isValid()==false)
        return;

      const double fontheight = get_system_fontheight();
      const double marker_radius = R_MAX(2, qRound(fontheight*0.2));
      const double marker_x = rect.left() + fontheight*0.55;
      const double marker_y = rect.center().y() + 0.5;

      QPainter painter(this);
      painter.setRenderHint(QPainter::Antialiasing, true);
      painter.setPen(Qt::NoPen);
      painter.setBrush(get_config_qcolor("menu_text"));
      painter.drawEllipse(QPointF(marker_x, marker_y), marker_radius, marker_radius);
    }
    

  public slots:
  
    void 	aboutToHide(){
      setclosed();
    }
    void 	aboutToShow(){
      setopen();

      // A sub menu object can be shown several times, so reset for each showing.
      _mouse_has_moved = false;

      // Give sub menus a current entry right away, like normal Qt menus do.
      if (activeAction()==NULL)
        for (QAction *action : actions())
          if (action->isVisible() && !action->isSeparator() && action->isEnabled() && action->menu()==NULL){
            setActiveAction(action);
            break;
          }
    }

    // Makes hovering an entry select (make current) that entry, so that the
    // current entry is highlighted, also in sub menus.
    void hovered(QAction *action)
    {
      // The entry widgets paint the current-entry marker based on the menu's active action, so
      // repaint the widget of the previous current entry when the current entry changes.
      QAction *current = activeAction();

      if (current != _last_current_action) {
        MyQAction *previous = dynamic_cast<MyQAction*>(_last_current_action.data());
        if (previous != NULL && previous->_widget != NULL)
          previous->_widget->update();
        _last_current_action = current;
      }

      MyQAction *myaction = dynamic_cast<MyQAction*>(action);

      if (myaction == NULL)
      {
        if (g_last_hovered_widget.data() != NULL)
          g_last_hovered_widget->update();

        g_last_hovered_menu_entry_guinum = -1;
        g_last_hovered_widget = NULL;
        g_last_hovered_myaction = NULL;

        return;
      }

      if (g_last_hovered_widget.data() != NULL)
        g_last_hovered_widget->update();

      myaction->_widget->update();

      g_last_hovered_menu_entry_guinum = myaction->_guinum;
      g_last_hovered_widget = myaction->_widget;

      if (myaction->isEnabled())
        g_last_hovered_myaction = myaction;
      else
        g_last_hovered_myaction = NULL;

      if (_is_part_of_main_menu || action==activeAction() || actions().contains(action)==false)
        return;

      // Only let hovering change the current entry after the mouse has actually moved. Otherwise
      // the entry under the mouse pointer would become current right away when a menu is opened.
      if (!_mouse_has_moved)
        return;

      if (activeAction()==NULL || strcmp(activeAction()->metaObject()->className(), "QAction")){ // Don't do this if activeAction() is a sub menu.
        IsAlive is_alive1(this);
        IsAlive is_alive2(action);

        QTimer::singleShot(20, [is_alive1, is_alive2, this, action]{
          if (is_alive1 && is_alive2 && !_is_part_of_main_menu && action!=activeAction() && actions().contains(action))
            setActiveAction(action);
        });
      }
    }
    
  };


  struct MyMainQMenu : public MyQMenu
#if SAFE_POPUP
    , radium::Timer
#endif
  {
    Q_OBJECT

    int _set_active_action_counter = 0; // A counter to ensure we won't go into infinite loop when calling setActiveAction from the 'hovered'-slot.
    
  public:

#if SAFE_POPUP
    bool _do_safe_popup;
    QString _kill_file_name;
#endif

    bool _is_root;
    bool _is_permanent;

    bool _has_keyboard_focus = false; // Per-instance. A global flag caused one menu's closeEvent to release another menu's keyboard focus.
    
    MyMainQMenu(QWidget *parent, QString title, int shortcut_width, bool is_async, bool is_permanent, func_t *callback)
      : MyQMenu(parent, title, shortcut_width)
#if SAFE_POPUP
      , radium::Timer(1000, false)
      , _do_safe_popup(getenv("USE_SAFE_POPUP") != NULL)
#endif
      , _is_root(parent==NULL)
      , _is_permanent(is_permanent)
    {
      
      R_ASSERT(is_async==true); // Lots of trouble with non-async menus. (triggers qt bugs)
      
      //if(_callback!=NULL)
      //  s7extra_protect(_callback);
    }
    
    ~MyMainQMenu(){
      //if(_callback!=NULL)
      //  s7extra_unprotect(_callback);
      //printf("\n\n\n\n\n===============================              MYMAINQMENU deleted: %p\n\n\n\n\n", this);
#if SAFE_POPUP
      if(_do_safe_popup)
        stop_safe_popup_process();
#endif

      if (_has_keyboard_focus){
        release_keyboard_focus_counting();
        _has_keyboard_focus = false;
      }

      API_call_me_when_a_popup_menu_has_been_closed();
    }

#if SAFE_POPUP

    void start_safe_popup_process(void) {
      _kill_file_name = get_kill_temp_filename();
      
      int res = system(talloc_format("\"%S\" %s %d %d &",
                                     OS_get_full_program_file_path("radium_linux_popup_killscript.sh").id,
                                     _kill_file_name.toUtf8().constData(),
                                     KILL_TIME,
                                     getpid()
                                     ));
      (void)res;

      start_timer();
    }
    
    void stop_safe_popup_process(void) {
      printf("Deleting file %s\n", _kill_file_name.toUtf8().constData());
      int res = system(talloc_format("rm /tmp/%s", _kill_file_name.toUtf8().constData()));
      (void)res;
      printf(" ... %s deleted\n", _kill_file_name.toUtf8().constData());
    }
    
    void calledFromTimer(void) override {
      if(radium::Timer::get_duration() > CLOSE_TIME * 1000)
        if (close()){ //delete this;
          stop_safe_popup_process();
          stop_timer();
        }
    }

    const char *get_kill_temp_filename(void) const {
      static int num = 0;
      return talloc_format("radium_pid_%d_%d", getpid(), num++);
    }
#endif
    
    void showEvent(QShowEvent *event) override {
#if SAFE_POPUP
      if(_do_safe_popup && radium::Timer::is_running()==false){
        start_safe_popup_process();
      }
#endif
      
      //printf("    ========== 1. SHOW\n");

      _set_active_action_counter = 0;
      
      if (_has_keyboard_focus==false){
        obtain_keyboard_focus_counting();
        _has_keyboard_focus = true;
      }

      // If this popup replaced another popup (left/right arrow navigation), release the keyboard
      // focus reference that was kept over the transition.
      release_pending_keyboard_focus_holds();

	MyQMenu::showEvent(event);

	// The menu is usually opened with the mouse pointer over the search field, and Qt
	// clears the active action when the mouse moves over it. Also, the first entry is
	// not activated at creation time if it is a sub menu (activating it would open the
	// sub menu). Select the first entry using keyboard navigation in that case, since
	// keyboard navigation does not open sub menus.
	PopupSearchState *state = g_popup_search_state.get();

	if (state != NULL
	    && state->root_menu == this
	    && state->lineedit_action != NULL)
	{
		QPointer<MyMainQMenu> menu = this;

		QTimer::singleShot(0, this, [menu]{
			PopupSearchState *state2 = g_popup_search_state.get();

			if (menu == NULL || state2 == NULL || state2->root_menu != menu.data())
				return;

			QAction *active = menu->activeAction();

			if (active == NULL || active == state2->lineedit_action.data())
				menu->navigate_qt(Qt::Key_Down);
		});
	}
    }

    void hideEvent(QHideEvent *event) override {
      //printf("    ========== 2. HIDE\n");

      _set_active_action_counter = 0;
      
      if (_has_keyboard_focus==true){
        release_keyboard_focus_counting();
        _has_keyboard_focus = false;
      }

      MyQMenu::hideEvent(event);

      // Qt hides a popup menu when it is dismissed instead of closing it, so WA_DeleteOnClose
      // never fires for popups dismissed by clicking outside, pressing escape, or by hide().
      // Delete the root popup here to avoid leaking its actions, callbacks and entry widgets.
      if (!_is_permanent && _is_root)
        deleteLater();
    }

    void closeEvent(QCloseEvent *event) override{
      //printf("    ========== 3. CLOSE\n");

      _set_active_action_counter = 0;
        
      if (_has_keyboard_focus==true){
        release_keyboard_focus_counting();
        _has_keyboard_focus = false;
      }

      setPaintSequencerGrid(false);

      MyQMenu::closeEvent(event);
    }

    void mousePressEvent(QMouseEvent *event) override{
      //printf("MOUSEPRESS. Last hovered: %d - %d\n", (int)_my_last_hovered_menu_entry_guinum, (int)g_last_hovered_menu_entry_guinum);

      MyQAction *myaction = g_last_hovered_myaction.data();
      
      if (_is_permanent==false && myaction!=NULL && g_curr_visible_actions.contains(myaction->_entry_id) && myaction->my_clicked()==true){
        
        if(myaction->_callbacker.get() == NULL){
          
          //R_ASSERT_NON_RELEASE(false); // Happens very often. Don't know if it's a problem. Better not to crash the system in non-release mode, because then we will never test what happens in release mode.
          printf("ERROR! QT_POPUPMENU/mousePressEvent: _callbacker.get()==NULL\n");
        
        } else {
          
          event->accept();
          
        }
        
        return;
        
      }

      //printf("          SKIPPING\n");
      MyQMenu::mousePressEvent(event);
        
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

        g_last_hovered_myaction = NULL;
                  
        if(old!=NULL)
          old->update();
        //updateWidgetRecursively(this);
      }
    }



  };

// If a menu only has a single visible selectable entry, and that entry opens a sub menu,
// open the sub menu. This is repeated down the menu hierarchy. The result is that when
// searching for e.g. "Blowfish", the Project -> Demo Songs sub menus are opened
// automatically so the matching entry becomes visible instead of just highlighting the
// top-level Project menu.
static void auto_open_search_result_path(void)
{
	PopupSearchState *state = g_popup_search_state.get();

	if (state == NULL || state->root_menu.isNull() || state->lineedit_action.isNull())
		return;

	QVector<QMenu*> desired_path;

	const radium::FuzzySearchQuery search(state->query);

	if (!search.empty() && state->root_menu->isVisible())
	{
		QMenu *menu = state->root_menu.data();

		while (true)
		{
			QAction *only_action = NULL;
			int num_selectable_actions = 0;

			for (QAction *action : menu->actions())
			{
				if (action == state->lineedit_action.data())
					continue;

				if (!action->isVisible() || action->isSeparator() || !action->isEnabled())
					continue;

				only_action = action;
				num_selectable_actions++;
			}

			if (num_selectable_actions != 1 || only_action->menu() == NULL)
				break;

			QMenu *submenu = only_action->menu();

			desired_path.push_back(submenu);
			menu = submenu;
		}
	}

	// Close sub menus that were opened by a previous search, but which are no longer part
	// of the path to the current search result. Close the deepest menus first.
	for (int i = state->auto_opened_menus.size()-1 ; i >= 0 ; i--)
	{
		QMenu *auto_opened_menu = state->auto_opened_menus[i].data();

		if (auto_opened_menu == NULL || !desired_path.contains(auto_opened_menu))
		{
			if (auto_opened_menu != NULL)
				auto_opened_menu->hide();

			state->auto_opened_menus.removeAt(i);
		}
	}

	if (state->root_menu.isNull() || !state->root_menu->isVisible())
		return;

	QMenu *parent_menu = state->root_menu.data();

	for (QMenu *submenu : desired_path)
	{
		QAction *submenu_action = submenu->menuAction();

		if (submenu_action == NULL || !submenu_action->isVisible() || !submenu_action->isEnabled())
			break;

		if (!submenu->isVisible())
		{
			MyQMenu *my_parent_menu = dynamic_cast<MyQMenu*>(parent_menu);

			if (my_parent_menu == NULL)
				break;

			parent_menu->setActiveAction(submenu_action);
			my_parent_menu->navigate_qt(Qt::Key_Right);

			if (!submenu->isVisible())
				break;

			if (!state->auto_opened_menus.contains(submenu))
				state->auto_opened_menus.push_back(submenu);
		}

		parent_menu = submenu;
	}
}

static void schedule_auto_open_search_result_path(void)
{
	PopupSearchState *state = g_popup_search_state.get();

	if (state == NULL || state->root_menu.isNull())
		return;

	QPointer<QMenu> root_menu = state->root_menu;

	// The root menu is the context object, so the timer is canceled if the popup is deleted.
	QTimer::singleShot(0, root_menu.data(), []{
		auto_open_search_result_path();
	});
}


}

//static int num_s = 0,num_saved = 0;

static void setStyleRecursively(QObject *o, QStyle *style){
  if (o != NULL){

#if 0
    QWidget *w = dynamic_cast<QWidget*>(o);
    if (w!=NULL){
      if (dynamic_cast<MyProxyStyle*>(w->style()) == NULL) {// != style){
        //printf(" Setting %p. class name: %s. style name: %s \n", w, w->metaObject()->className(), w->style()->metaObject()->className());
        //w->setStyle(style);
        num_s++;
      } else {
        num_saved++;
      }
    }
#endif
    
    ClickableIconAction *a = dynamic_cast<ClickableIconAction*>(o);
    if (a != NULL){
      for(auto *o2 : a->associatedObjects())
	  {
		  QWidget *w = qobject_cast<QWidget*>(o2);
		  if (w)
			  w->setStyle(&g_my_proxy_style);
      }
    }
    
    for(auto *c : o->children())
      setStyleRecursively(c, style);
  }
}

static int get_largest_shortcut_width(const vector_t &v, int v_pos){

  QFont font;

  int ret = 0;
  
  int indentation = 0;

  //printf("\n\n\n\n========================================================\n");
  for(int i=v_pos;i<v.num_elements;i++) {

    QString text = (const char*)v.elements[i];

    //printf("         TEXT: -%s-\n", text.toUtf8().constData());
    
    if (text.startsWith("[submenu start]")){
      
      indentation++;

    } else if (text.startsWith("[submenu end]")){

      indentation--;

      if (indentation < 0)
        break;
      
    } else if (indentation==0 && text.contains("[shortcut]")){
      
      QString shortcut;

      int pos1 = text.indexOf("[shortcut]");
      text = text.mid(pos1 + 10);

      //printf("TEXT1: -%s-\n", text.toUtf8().constData());
      
      int pos2 = text.indexOf("[/shortcut]");
      if (pos2 <= 0){
        R_ASSERT(false);
      } else {
        shortcut = text.left(pos2);
        //printf("shortcut: -%s-. text: -%s-\n", shortcut.toUtf8().constData(), text.toUtf8().constData());
      }

      const QFontMetrics fn = QFontMetrics(font);
      ret = R_MAX(ret, ceil(fn.boundingRect(shortcut).width()));
    }

  }

  return ret;
}

#if 0
static void force_min_width(QWidget *top, QWidget *widget){
  if (widget != NULL){
    
    if (widget->minimumWidth() < widget->minimumWidth())
      top->setMinimumWidth(widget->minimumWidth());
    
    for(auto *c : widget->children()){
      QWidget *w = dynamic_cast<QWidget*>(c);      
      doit(top, w);
    }
    
  }
}
#endif

static QMenu *create_qmenu(
                           const vector_t &v,
                           bool is_async,
                           func_t *callback2,
                           std::function<void(int,bool)> callback3,
                           int *result,
                           bool is_permanent
                           )
{
  R_ASSERT(is_async==true); // Lots of trouble with non-async menus. (triggers qt bugs)

  std::shared_ptr<PopupSearchState> search_state = is_permanent ? nullptr : std::make_shared<PopupSearchState>();

  if (search_state != NULL)
    g_popup_search_state = search_state;

  MyMainQMenu *menu = new MyMainQMenu(NULL, "", get_largest_shortcut_width(v, 0), is_async, is_permanent, callback2);
  menu->_is_part_of_main_menu = is_permanent;
  if (!is_permanent)
    menu->setAttribute(Qt::WA_DeleteOnClose);

  if (search_state != NULL)
    search_state->all_menus.insert(menu);

  MyQMenu *curr_menu = menu;
  
  QStack<MyQMenu*> parents;
  QStack<int> n_submenuess;
  int n_submenues=0;

  QActionGroup *radio_buttons = NULL;
  QButtonGroup *radio_buttons2 = NULL;

  //num_s = 0,num_saved=0;
  double time = TIME_get_ms();
  double clickdur = 0,checkdur=0,subdur=0,sepdur=0,callbackdur=0,setdatadur=0;
  
  for(int i=0;i<v.num_elements;i++) {
    
    QString text = (const char*)v.elements[i];

    // Special entries only providing a callback for the left/right arrow keys. Handled here so
    // they don't create menu entries, popup-search entries, or sub menu splitting. Stored on the
    // root menu only, so arrow keys inside sub menus keep their normal Qt behavior.
    if (text == "[on-left-key]" || text == "[on-right-key]") {
      auto callbacker = std::make_shared<Callbacker>(menu, i, is_async, text, callback2, callback3, result, false);
      if (text == "[on-left-key]")
        menu->_on_left_key_callbacker = callbacker;
      else
        menu->_on_right_key_callbacker = callbacker;
      n_submenues++;
      continue;
    }
    
    if (text.startsWith("----")) {

      double t = TIME_get_ms();
      auto *separator = new SeparatorAction(); //curr_menu->addSeparator();

      for(int i = 3 ; i < text.size() ; i++)
        if (text[i] != '-'){
          QString text2 = text.mid(i);

          if (text2.startsWith("BASE64:")){
            text2 = text2.mid(7);
            text2 = QByteArray::fromBase64(text2.toUtf8());
          }
                
          separator->setText(text2);
          int minwidth = 1.6 * GFX_get_text_width(root->song->tracker_windows, text2.toUtf8().constData());
          if (menu->minimumWidth() < minwidth)
            menu->setMinimumWidth(minwidth); // qt doesn't do this by itself.
          break;
        }

      curr_menu->addAction(separator);
      if (search_state != NULL)
        search_state->menus[curr_menu].separators.append(separator);
      sepdur += TIME_get_ms() - t;
      
    } else {
      
      if (n_submenues==getMaxSubmenuEntries()){
        double t = TIME_get_ms();
        auto *new_menu = new MyQMenu(curr_menu, "Next", get_largest_shortcut_width(v, i));
        new_menu->_is_part_of_main_menu = is_permanent;
        subdur += TIME_get_ms()-t;
        QAction *next_action = curr_menu->addMenu(new_menu);
        if (search_state != NULL)
        {
          search_state->all_menus.insert(new_menu);
          search_state->menus[curr_menu].submenus.append(PopupSearchSubmenu{next_action, new_menu, radium::fuzzy_search_fold(new_menu->title())});
        }
        curr_menu = new_menu;
        //curr_menu->setStyleSheet("QMenu { menu-scrollable: 1; }");
        n_submenues=0;
      }
      
      QAction *action = NULL;
      
      
      bool disabled = false;
      bool base64 = false;
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
      
      if (text.startsWith("[base64]")){
        text = text.right(text.size() - 8);
        base64 = true;
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

      //printf("SHORTCUT: -%s-\n", text.toUtf8().constData());
      
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
        double t = TIME_get_ms();
        auto *new_menu = new MyQMenu(curr_menu, text.right(text.size() - 15), get_largest_shortcut_width(v, i+1));
        new_menu->_is_part_of_main_menu = is_permanent;
        //new_menu->setStyleSheet("QMenu::item#subMenu{ font : bold ; font-size: 13pt; color: #ff8080;}");
        //new_menu->setStyleSheet("padding-left: 140px");

        subdur += TIME_get_ms()-t;
        
        QAction *submenu_action = curr_menu->addMenu(new_menu);
        if (search_state != NULL)
        {
          search_state->all_menus.insert(new_menu);
          search_state->menus[curr_menu].submenus.append(PopupSearchSubmenu{submenu_action, new_menu, radium::fuzzy_search_fold(new_menu->title())});
        }
        //action->setDefaultWidget(new Sep);
        
        curr_menu = new_menu;
        
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
          // Skip our own "[on-left-key]"/"[on-right-key]" entries (they are only added at the end
          // of the entries, after the real menu entries), so they don't affect the styling of the
          // last real entry.
          is_last = true;
          for(int j=i+1 ; j<v.num_elements ; j++){
            QString next_text = (const char*)v.elements[j];
            if (next_text=="[on-left-key]" || next_text=="[on-right-key]")
              continue;
            if (next_text.startsWith("[submenu end]"))
              break;
            is_last = false;
            break;
          }
        }

        if (base64)
          text = QByteArray::fromBase64(text.toUtf8());
        
        double t = TIME_get_ms();
        auto callbacker = std::make_shared<Callbacker>(menu, i, is_async, text, callback2, callback3, result, is_checkable);
        callbackdur += TIME_get_ms()-t;
        
        if (!icon.isNull()) {

          action = new ClickableIconAction(icon, text, callbacker);
          //printf("   1. Setting action %p to -%s-\n", action, text.toUtf8().constData());
          
        } else if (is_checkable) {

          auto *hepp = new CheckableAction(text, shortcut, curr_menu->_shortcut_width, is_checked, radio_buttons, is_first, is_last, callbacker);
          
          if (hepp->_success==false){
            radio_buttons=NULL;
            goto finished_parsing;
          }
          
          action = hepp;
          //printf("   2. Setting action %p to -%s-\n", action, text.toUtf8().constData());
          
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

          double t = TIME_get_ms();
          auto *hepp = get_clickable_action(text, shortcut, curr_menu->_shortcut_width, is_first, is_last, is_permanent, callbacker);
          clickdur += TIME_get_ms() - t;

          if (hepp->_success==false)
            goto finished_parsing;
          
          action = hepp;
          //printf("   3. Setting action %p to -%s-\n", action, text.toUtf8().constData());
        }
        
        //printf("   Finished. Callbacker %p count: %ld\n", callbacker.get(), callbacker.use_count());
      }

 
      if (action != NULL){
        //action->setData(i);
        double t = TIME_get_ms();
        curr_menu->addAction(action);  // are these actions automatically freed in ~QMenu? (yes, seems so) (they are probably freed because they are children of the qmenu)

        MyQAction *myaction = dynamic_cast<MyQAction*>(action);
        if (myaction != NULL)
          myaction->_menu = curr_menu;

        if (search_state != NULL)
          search_state->menus[curr_menu].entries.append(PopupSearchEntry{action, radium::fuzzy_search_fold(text)});



        /*
        {
          for(auto *widget : action->associatedWidgets())
            doit(menu, widget);
          
          auto *myqaction = dynamic_cast<MyQAction*>(action);
          if (myqaction != NULL){
            doit(menu, myqaction->_widget);
          }
        }
        */
        
        /*
          if (curr_menu->minimumWidth() < widget->minimumWidth())
            curr_menu->setMinimumWidth(widget->minimumWidth());
        */
        setdatadur += TIME_get_ms() - t;
      }

      if (disabled){
        if (action != NULL){
          action->setDisabled(true);
        } else {
          R_ASSERT_NON_RELEASE(false);
        }
      }

    }

    n_submenues++;
  }

 finished_parsing:

  double t = TIME_get_ms();

  if (search_state != NULL)
  {
    int num_entries = 0;

    for (PopupSearchMenu &search_menu : search_state->menus)
      num_entries += search_menu.entries.size();

    if (num_entries >= POPUP_SEARCH_MIN_NUM_ENTRIES)
    {
      QLineEdit *lineedit = new QLineEdit;
      lineedit->setFocusPolicy(Qt::NoFocus);
      lineedit->setContextMenuPolicy(Qt::NoContextMenu);
      lineedit->setFrame(false);
      lineedit->setPlaceholderText("Search...");
      lineedit->setMinimumWidth(200);

      const int fontheight = R_MAX(12, get_system_fontheight());
      const int border_width = R_MAX(1, (int)(fontheight/20.0 + 0.5));

      QFont font = lineedit->font();
      font.setPixelSize(fontheight);
      lineedit->setFont(font);
      lineedit->setMinimumHeight(fontheight + 2*border_width);
      lineedit->setTextMargins((int)(border_width + 1.1*fontheight), 0, border_width, 0);

      QColor background_color = get_qcolor(MENU_SEARCH_COLOR_NUM);
      QColor text_color = get_qcolor(MENU_TEXT_COLOR_NUM);
      QColor placeholder_color = text_color;
      placeholder_color.setAlpha(128);

      QPalette palette = lineedit->palette();
      palette.setColor(QPalette::Base, background_color);
      palette.setColor(QPalette::Window, background_color);
      palette.setColor(QPalette::Text, text_color);
      palette.setColor(QPalette::PlaceholderText, placeholder_color);
      lineedit->setPalette(palette);
      lineedit->setStyleSheet(QString("QLineEdit { background-color: %1; }").arg(background_color.name(QColor::HexArgb)));

      auto *lineedit_action = new PopupSearchAction(menu);
      lineedit_action->setDefaultWidget(lineedit);
      lineedit_action->setEnabled(false); // So that QMenu's own keyboard navigation skips it.
      lineedit->setEnabled(true);         // ...but the widget itself should still look and work as normal.

      search_state->root_menu = menu;
      search_state->lineedit = lineedit;
      search_state->lineedit_action = lineedit_action;

      if (menu->actions().isEmpty())
        menu->addAction(lineedit_action);
      else
        menu->insertAction(menu->actions().at(0), lineedit_action);
    }
  }

  {
    QAction *first_action = NULL;

    for (QAction *action : menu->actions())
    {
      if (search_state != NULL && action == search_state->lineedit_action.data())
        continue;

      first_action = action;
      break;
    }

	// Don't activate the first entry of the hamburger menu when it pops up, since the first
	// entry is a sub menu ("Project"), and activating it would open that sub menu. Don't
	// activate any first entry which is a sub menu for the same reason; the first entry is
	// selected after the menu has been shown instead (see MyMainQMenu::showEvent).
	if (first_action != NULL
	    && first_action->menu() == NULL // Activating a sub menu entry opens the sub menu.
	    && g_pending_popup_position_is_hamburger == false)
		menu->setActiveAction(first_action);
  }

  R_ASSERT_NON_RELEASE(radio_buttons==NULL);
  R_ASSERT_NON_RELEASE(radio_buttons2==NULL);

  double t2 = TIME_get_ms();
  
  if (radio_buttons!=NULL && radio_buttons->actions().size()==0){
    R_ASSERT_NON_RELEASE(false);
    delete radio_buttons;
  }

  double t3 = TIME_get_ms();

  if (radio_buttons2!=NULL && radio_buttons2->buttons().size()==0){
    R_ASSERT_NON_RELEASE(false);
    delete radio_buttons2;
  }

  double t4 = TIME_get_ms();
  
  /*
  for(auto style : QStyleFactory::keys())
    printf("  STYLE: -%s\n", style.toUtf8().constData());
  */
  
  setStyleRecursively(menu, &g_my_proxy_style);

  double t5 = TIME_get_ms();
    
  /*
  menu->setStyleSheet("QMenu::indicator:exclusive:checked {  background-color: #ff00ff; }"
                      "QMenu::indicator:exclusive:unchecked {  background-color: #00ffff; }"
                      );
  */
  checkdur += TIME_get_ms() - t;

  if(0)
    printf("      DUR: %f. clickdur: %f. checkdur: %f, subdur: %f. End: %f %f %f %f. sepdur: %f. callbackdur: %f. setdatadur: %f. Num calls to setStyle: %d / %d\n", TIME_get_ms()-time, clickdur, checkdur, subdur, t2-t,t3-t2,t4-t3,t5-t4, sepdur,callbackdur,setdatadur,0,0 ); //, num_s, num_saved);

  if (search_state != NULL)
  {
    const QString pending_search_query = g_pending_popup_search_query;
    g_pending_popup_search_query.clear();

    if (!pending_search_query.isEmpty() && !search_state->lineedit_action.isNull())
    {
      search_state->query = pending_search_query;
      apply_popup_search_filter();
    }
  }

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

// Returns true if the entry is the active (currently selected) entry of its own menu,
// i.e. the entry that Qt will activate if the user presses return or clicks it.
bool GFX_IsCurrentMenuEntry(int64_t guinum)
{
  MyQAction *action = g_actions_by_guinum.value(guinum).data();

  if (action == NULL)
    return false;

  // Note: callbacker->qmenu is the root popup menu, while the action might
  // be in a sub menu (e.g. in the hamburger/search popup). _menu is the menu
  // the action is actually in.
  if (action->_menu != NULL)
    return action->_menu->activeAction() == action;

  for (QWidget *widget : action->associatedWidgets())
  {
    QMenu *menu = qobject_cast<QMenu*>(widget);

    if (menu != NULL && menu->activeAction() == action)
      return true;
  }

  return false;
}

// This function is only needed on Windows, for some reason, but we do it on all platforms to faster discover bugs caused by this function.
void GFX_RightclickMenuEntry(int entryid){
  if (g_curr_visible_actions.contains(entryid)){
    
    MyQAction *action = g_curr_visible_actions[entryid].data();
    if(action==NULL){
      R_ASSERT_NON_RELEASE(false);
      return;
    }

    auto *callbacker = action->_callbacker.get();
    if (callbacker != NULL){
      auto *qmenu = callbacker->qmenu.data();
      if (qmenu != NULL){
        qmenu->hide(); // It looks very good not closing the previous menu entry when right-clicking, but it also screws up keyboard focus.
        //printf("HIDING\n");
      }
    }
    
    action->my_clicked();
  }
}


QMenu *GFX_create_qmenu(const vector_t &v,
                        func_t *callback2)
{
  return create_qmenu(v, true,  callback2, NULL, NULL, true);
}


// Sets the position for the next popup menu. If is_hamburger is true, x/y is the position of
// the hamburger button, and the popup menu is placed above it with the bottom edge at the top
// edge of the button, horizontally left-aligned.
void GFX_set_pending_popup_position(int x, int y, bool is_hamburger)
{
	g_pending_popup_position = QPoint(x, y);
	g_pending_popup_position_is_hamburger = is_hamburger;
	g_has_pending_popup_position = true;
}

// Sets the position of the next popup menu to the top of the editor, i.e. just below the
// whole track header (both the name widget ("(click me)") and the slider row below it),
// for the given track. tracknum -1 means the current track. Does nothing (i.e. the next
// popup menu opens at the mouse position) if the position can't be found.
void GFX_set_track_header_popup_position(int tracknum)
{
	g_next_popup_is_instrument_popup = true;

	struct Tracker_Windows *window = getWindowFromNum(-1);
	struct WBlocks *wblock = NULL;
	struct WTracks *wtrack = getWTrackFromNumA(-1, &window, -1, &wblock, tracknum);
	if (wtrack == NULL)
		return;

	EditorWidget *editor = static_cast<EditorWidget*>(window->os_visual.widget);
	if (editor == NULL || editor->header_widget == NULL)
		return;

	const QWidget *header_widget = editor->header_widget;

	// The "(click me)" name widget is the top half of the track header, and the sliders
	// (volume/pan) are in the row below.
	int x = wtrack->x; // Same x as the track header name widget is painted at.
	const int y = header_widget->height(); // Bottom of the track header, i.e. y1 of the editor widget.

	if (x < 0)
		x = 0;
	else if (x >= header_widget->width())
		x = header_widget->width() - 1;

	const QPoint pos = header_widget->mapToGlobal(QPoint(x, y));
	GFX_set_pending_popup_position(pos.x(), pos.y(), false);
}

// Registers the hamburger button in the main window's bottom bar, so that the popup menu
// can be opened at the same position as when clicking on it.
void GFX_set_hamburger_button(QWidget *button)
{
	g_hamburger_button = button;
}

// Opens the hamburger popup menu at the position of the provided button, or, if button is
// NULL, at the position of the registered hamburger button in the main window's bottom bar.
// Falls back to opening at the mouse position if no button is available.
void GFX_open_hamburger_popup_menu(QWidget *button)
{
	if (GFX_HamburgerPopupIsOpen())
		return;

	// The main menus have now been opened. Stop the attention drawing and the startup hint
	// of the hamburger button. (See BottomBar_create and Qt_MainWindow.cpp.)
	if (SETTINGS_read_bool("main_menu_popup_has_been_opened", false)==false)
	{
		SETTINGS_write_bool("main_menu_popup_has_been_opened", true);
		GFX_OS_hamburger_menu_was_opened();
	}

	if (button == NULL)
		button = g_hamburger_button;

	if (button != NULL)
	{
		const QPoint pos = button->mapToGlobal(QPoint(0, 0));
		GFX_set_pending_popup_position(pos.x(), pos.y(), true);
	}

	S7CALL2(void_void, "FROM_C-popup-main-menus");
}

// Returns true if the hamburger popup menu (the popup opened when clicking the hamburger
// button, pressing left alt, or using a keybinding) is currently open.
bool GFX_HamburgerPopupIsOpen(void)
{
	return !g_hamburger_popup.isNull() && g_hamburger_popup->isVisible();
}

// Closes the hamburger popup menu, including any open sub menu.
void GFX_CloseHamburgerPopup(void)
{
	// Cancel any scheduled opening of the hamburger popup, so it doesn't appear after it was closed.
	S7CALL2(void_void, "FROM_C-cancel-main-menu-popup");

	if (g_pending_popup_position_is_hamburger)
	{
		g_has_pending_popup_position = false;
		g_pending_popup_position_is_hamburger = false;
	}

	if (!g_hamburger_popup.isNull())
		g_hamburger_popup->hide();
}

// Returns true if the instrument-select popup (opened by F12 or by clicking the track header
// name) is currently open.
bool GFX_InstrumentPopupIsOpen(void)
{
	return !g_instrument_popup.isNull() && g_instrument_popup->isVisible();
}

// Closes the instrument-select popup, including any open sub menu.
void GFX_CloseInstrumentPopup(void)
{
	if (!g_instrument_popup.isNull())
		g_instrument_popup->hide();
}

// Returns the position to open a popup menu at. (Note that this position is the top-left corner
// of the menu, not the position of the hamburger button.)
static QPoint get_popup_menu_position(QMenu *menu)
{
	if (g_pending_popup_position_is_hamburger == false)
		return g_pending_popup_position;

	const QPoint anchor = g_pending_popup_position;
	const QSize size = menu->sizeHint();

	const QScreen *screen = QApplication::screenAt(anchor);
	if (screen == NULL)
		screen = QApplication::primaryScreen();
	if (screen == NULL)
		return anchor;

	const QRect available = screen->availableGeometry();

	int x = anchor.x(); // Left-align with the hamburger button.
	int y = anchor.y() - size.height(); // Place the bottom edge of the menu directly above the hamburger button.

	if (x + size.width() > available.right() + 1)
		x = available.right() + 1 - size.width();
	if (x < available.left())
		x = available.left();
	if (y < available.top())
		y = available.top();

	return QPoint(x, y);
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
  
  QMenu *menu = create_qmenu(v, is_async,  callback2, callback3, is_async ? NULL : &result, false);
  //printf("                CREATED menu %p", menu);
  
  if (is_async){

    if (g_next_popup_is_instrument_popup){
      g_instrument_popup = menu;
      g_next_popup_is_instrument_popup = false;
    }

    if (g_has_pending_popup_position)
    {
      if (g_pending_popup_position_is_hamburger)
        g_hamburger_popup = menu;

      const QPoint pos = get_popup_menu_position(menu);
      g_has_pending_popup_position = false;
      g_pending_popup_position_is_hamburger = false;
      safeMenuPopup(menu, pos);
    }
    else
    {
      safeMenuPopup(menu);
    }

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

void GFX_clear_menu_cache(void){
  g_clickable_actions.clear();
}

void GFX_call_when_menus_are_closed(std::function<void(void)> callback)
{
	g_pending_menu_callbacks.push_back(callback);

	if (GFX_MenuActive()==false)
		schedule_flush_pending_menu_callbacks();
}

QMenu *GFX_GetActiveMenu(void){
  //return current_menu->base->activeAction() != NULL;
  if (GFX_MenuActive()){
    if (g_curr_menu.isEmpty()){
      //R_ASSERT_NON_RELEASE(false); // Happens sometimes if "trying_to_make_menu_active" is true.
      return NULL;
    } else {
      return g_curr_menu.top();
    }
  } else { 
    R_ASSERT_NON_RELEASE(g_curr_menu.isEmpty());
    return NULL;
  }
}

static bool trying_to_make_menu_active = false;

static void make_menu_active(int trynum, int ms){
  //printf("   make_menu_active. trynum: %d. safe: %d\n", trynum, safe_to_run_exec());
  
  if (trynum >= 100 || !safe_to_run_exec()){
    trying_to_make_menu_active = false;
    return;
  }

  QTimer::singleShot(ms, [trynum]{
      
      //printf(" Hepp: %d. trynum: %d\n", g_menu_is_open, trynum);

      if (trying_to_make_menu_active==false) // Stopped, e.g. because the main menu search popup took over.
        return;

      if(g_menu_is_open==0){

        //g_main_menu_bar->setFocus(Qt::MenuBarFocusReason);

        if (trynum==0 || trynum > 2)
          send_key_down(g_main_menu_bar, 1);

        if (trynum > 0){
          QPoint topleft = g_main_menu_bar->rect().topLeft();
          QPoint center = g_main_menu_bar->rect().center();
          QPoint point = QPoint(topleft.x() + 40, center.y());
          auto *event = new QMouseEvent(QEvent::MouseMove,
										point,
										g_main_menu_bar->mapToGlobal(point),
										Qt::NoButton, Qt::NoButton, Qt::NoModifier);
          qApp->postEvent(g_main_menu_bar, event);
        }
        
        make_menu_active(trynum + 1, 10);

      } else {

        trying_to_make_menu_active = false;

      }

    });

}

void GFX_MakeMakeMainMenuActive(void){
  if (trying_to_make_menu_active==true)
    return;

  trying_to_make_menu_active = true;
  make_menu_active(0, 10);
}

void GFX_stop_making_main_menu_active(void){
  trying_to_make_menu_active = false;
}

bool GFX_MenuActive(void){
  //return current_menu->base->activeAction() != NULL;
  return g_menu_is_open > 0 || trying_to_make_menu_active;
}

#include "mQt_PopupMenu.cpp"

#endif // USE_QT_MENU

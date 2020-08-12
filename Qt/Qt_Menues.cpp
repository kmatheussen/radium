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

#include "../common/includepython.h"

#include <QMenuBar>
#include <QApplication>
#include <QStack>

#include "../common/nsmtracker.h"
#include "../common/settings_proc.h"
#include "EditorWidget.h"
#include "Timer.hpp"

#include "../api/api_requesters_proc.h"

#include "Qt_Menues_proc.h"


extern QApplication *qapplication;

QMenuBar *g_main_menu_bar = NULL;

namespace{

static QStack<QMenu* >g_curr_menu;
static int g_menu_is_open = 0;
 
struct MyMenu : public QMenu, radium::Timer{
  Q_OBJECT;

  bool _has_g_menu_is_open = false;

  void setclosed(void){
    if (_has_g_menu_is_open){
      g_menu_is_open--;
      _has_g_menu_is_open = false;
      R_ASSERT_RETURN_IF_FALSE(!g_curr_menu.isEmpty());
      g_curr_menu.pop();
    }
  }

  void setopen(void){
    if (!_has_g_menu_is_open){
      g_menu_is_open++;
      _has_g_menu_is_open = true;
      g_curr_menu.push(this);
    }
  }

  // Workaround. Sometimes, aboutToHide and/or aboutToShow is not called.
  void calledFromTimer(void) override {
    if (isVisible())
      setopen();
    else
      setclosed();
  }
  
public:
  
  MyMenu()
    : radium::Timer((int)scale_int64(qrand(), 0, RAND_MAX, 200, 500), true)
  {
    connect(this, SIGNAL(aboutToHide()), this, SLOT(aboutToHide()));
    connect(this, SIGNAL(aboutToShow()), this, SLOT(aboutToShow()));
  }

  /*
  void hideEvent(QHideEvent *event) override {
    printf("   HIDE EVENT\n");
    QMenu::hideEvent(event);
    set_editor_focus();
  }
  */
  /*
    void showEvent(QShowEvent *event) override {
    printf("    ========== 1. SHOW\n");
    QMenu::showEvent(event);
    }
    
    void hideEvent(QHideEvent *event) override {
    printf("    ========== 2. HIDE\n");
    QMenu::hideEvent(event);
    }
    
    void closeEvent(QCloseEvent *event) override{
    printf("    ========== 3. CLOSE\n");
    QMenu::closeEvent(event);
    }
  */


public slots:
  
  void 	aboutToHide(){
    setclosed();
  }
  void 	aboutToShow(){
    setopen();
  }


};
  
struct Menues{
  struct Menues *up;
  QMenu *menu;
  QMenuBar *base;
};

static QMenu *g_first_menu = NULL;

static QVector<QMenu*> g_all_menus;
 
static struct Menues *current_menu = NULL;

class MenuItem : public QObject
{
    Q_OBJECT
public:
  MenuItem(const char *name, const char *python_command, bool checkable = false, int checkval = 0){
    this->python_command = V_strdup(python_command);
    this->checkable = checkable;
    this->checkval = checkval;

    //printf("Adding menu item %s\n",name);
    //getchar();
    
    QAction *action = current_menu->menu->addAction(name, this, SLOT(clicked()));
    //printf("action: %p\n",action);
    if(current_menu->base==NULL){
      if(checkable==true){
        action->setCheckable(true);
        action->setChecked(checkval);
      }
    }
  }

private:
  const char *python_command;
  bool checkable;
  int checkval;

public slots:
  void clicked() {
    struct Tracker_Windows *window = root->song->tracker_windows;
    R_ASSERT_NON_RELEASE(window!=NULL);
    
    if(checkable==true){
      char temp[500];
      this->checkval = this->checkval==1?0:1; // switch value.
      snprintf(temp,480,python_command,checkval?"1":"0");
      EVENTLOG_add_event(talloc_format("menu: %s", temp));
      PyRun_SimpleString(temp);      
    }else{
      EVENTLOG_add_event(talloc_format("menu: %s", python_command));
      PyRun_SimpleString(python_command);
    }
    
    // closeRequester(); What?
    
    //static_cast<EditorWidget*>(window->os_visual.widget)->updateEditor();
    //static_cast<EditorWidget*>(window->os_visual.widget)->repaint(); // Why isn't calling updateEditor() enough?
#if defined(RELEASE)
    window->must_redraw = true;
#endif

  }
};
}


#include "mQt_Menues.cpp"


void GFX_AddMenuItem(struct Tracker_Windows *tvisual, const char *name, const char *python_command){
  QString string(name);
  bool addit = true;

  waitUntilNsmHasInited();
    
  if (string.startsWith("[Linux]")) {
#if defined(FOR_LINUX)
    name += 7;
#else
    addit = false;
#endif
  }
  
  if (string.startsWith("[NSM]")) {
    if (nsmIsActive())
      name += 5;
    else
      addit = false;
  }
    
  if (string.startsWith("[non-NSM]")) {
    if (!nsmIsActive())
      name += 9;
    else
      addit = false;
  }
  
  if (string.startsWith("[Win]")) {
#if defined(FOR_WINDOWS)
    name += 5;
#else
    addit = false;
#endif
  }
  
  if (string.startsWith("[Darwin]")) {
#if defined(FOR_MACOSX)
    name += 8;
#else
    addit = false;
#endif
  }
  
  if (addit)
    new MenuItem(name, python_command);

}

void GFX_AddCheckableMenuItem(struct Tracker_Windows *tvisual, const char *name, const char *python_command, int checkval){
  new MenuItem(name, python_command, true, checkval);
}

void GFX_AddMenuSeparator(struct Tracker_Windows *tvisual){
  if(current_menu->menu==NULL){
    RError("Can not add separator at toplevel");
    return;
  }
  current_menu->menu->addSeparator();
}

static void add_menu_menu(const char *name, QMenu *mymenu, bool fixed_size_font){
  struct Menues *menu = (struct Menues*)V_calloc(1, sizeof(struct Menues));
  menu->up = current_menu;
  menu->menu = mymenu;
  
  if (g_first_menu==NULL)
    g_first_menu = menu->menu;

  //QFont sansFont("Liberation Mono", 8);
  //QFont sansFont("DejaVu Sans Mono", 7);
  //QFont sansFont("Nimbus Mono L", 9);
  //QFont sansFont("WenQuanYi Zen Hei Mono",8);
  //QFont sansFont("Aurulent Sans Mono",8);
  //QFont sansFont(QApplication::font());
  QFont sansFont;
  
  if (fixed_size_font) {
    sansFont.setFamily("Bitstream Vera Sans Mono");
    sansFont.setStyleName("Bold");
    sansFont.setPointSize(QApplication::font().pointSize());
    //sansFont.setBold(true);
  }
  
  menu->menu->setFont(sansFont);
  
  g_all_menus.push_back(menu->menu);

  if(current_menu->base!=NULL){
    QAction *action = current_menu->base->addMenu(mymenu);
    action->setText(name);
  }else{
    QAction *action = current_menu->menu->addMenu(mymenu);
    action->setText(name);
  }

  current_menu = menu;
}

void GFX_AddMenuMenu(const char *name, QMenu *mymenu){  
  add_menu_menu(name, mymenu, false);
}

void GFX_AddMenuMenu(struct Tracker_Windows *tvisual, const char *name, const char *command){
  add_menu_menu(name, new MyMenu(), true);
}

void GFX_GoPreviousMenuLevel(struct Tracker_Windows *tvisual){
  if(current_menu->base!=NULL){
    RError("Already at top level");
    return;
  }
  current_menu = current_menu->up;
}

bool GFX_MenuVisible(struct Tracker_Windows *tvisual){
  /*
  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;
  return editor->main_window->menuBar()->isVisible();
  */
  return g_main_menu_bar->isVisible();
}

void GFX_ShowMenu(struct Tracker_Windows *tvisual){
  //EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;
  GL_lock();{
    GL_pause_gl_thread_a_short_while();
    g_main_menu_bar->show();
    //editor->main_window->menuBar()->show();
  }GL_unlock();
}

void GFX_HideMenu(struct Tracker_Windows *tvisual){
  /*
  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;
  //g_first_menu->menuAction()->setVisible(false);
  editor->main_window->menuBar()->hide();
  */
  
  g_main_menu_bar->hide();
  
  //EditorWidget *editor=(EditorWidget *)root->song->tracker_windows->os_visual.widget;
  QKeyEvent *eve1 = new QKeyEvent((enum QEvent::Type)6, Qt::Key_Escape, Qt::NoModifier);
  qApp->postEvent(g_main_menu_bar,eve1);
}

void GFX_SetMenuFontsAgain(void){
  // another stupid osx workaround hack. (this is actually a stupid osx workaround hack for another stupid osx workaround hack)

  QFont sansFont;

  if (false){
    sansFont.setFamily("Bitstream Vera Sans Mono");
    sansFont.setStyleName("Bold");
    sansFont.setPointSize(QApplication::font().pointSize());
    //sansFont.setBold(true);
  }
  
  for(auto *menu : g_all_menus)
    menu->setFont(sansFont);
}
                  
void initMenues(QMenuBar *base_menu){
  g_main_menu_bar = base_menu;
  
  current_menu = (struct Menues*)V_calloc(1, sizeof(struct Menues));
  //g_base_menues = current_menu;
  current_menu->base = base_menu;
  
#if 0
  GFX_AddMenuItem(NULL, "item1", "dosomething");
  GFX_AddMenuItem(NULL, "item2", "");
  GFX_AddMenuMenu(NULL, "menu1");
  GFX_AddMenuItem(NULL, "menu1 - item1", "");
  GFX_AddMenuSeparator(NULL);
  GFX_AddMenuItem(NULL, "menu1 - item2", "");
  GFX_GoPreviousMenuLevel(NULL);
  GFX_AddMenuItem(NULL, "item3", "");
  GFX_AddMenuMenu(NULL, "menu2");
  GFX_AddMenuMenu(NULL, "menu2 -> menu1");
  GFX_AddMenuItem(NULL, "menu2 -> menu1 -> item1", "");
  GFX_GoPreviousMenuLevel(NULL);
  GFX_AddMenuItem(NULL, "item2 -> item1", "");
#endif
}


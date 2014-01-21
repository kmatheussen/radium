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

#include "Python.h"

#include <QMenuBar>
#include <QApplication>

#ifdef USE_QT4
#  include <Q3PopupMenu>
#else
#  include "qpopupmenu.h"
#endif

#include "../common/nsmtracker.h"
#include "../common/gfx_proc.h"
#include "../common/settings_proc.h"
#include "EditorWidget.h"

#include "../api/api_requesters_proc.h"

#include "Qt_Menues_proc.h"

extern bool doquit;
extern struct Root *root;
extern QApplication *qapplication;

namespace{
struct Menues{
  struct Menues *up;
  Q3PopupMenu *menu;
  QMenuBar *base;
};
}

static struct Menues *current_menu = NULL;

class MenuItem : public QObject
{
    Q_OBJECT
public:
  MenuItem(const char *name, const char *python_command, Q3PopupMenu *menu = NULL, bool checkable = false, int checkval = 0){
    this->python_command = strdup(python_command);
    this->checkable = checkable;
    this->checkval = checkval;

    if(menu!=NULL) {
      if(current_menu->base!=NULL){
        int id = current_menu->base->insertItem(name, menu);
        current_menu->base->connectItem(id, this, SLOT(clicked()));
      }else{
        int id = current_menu->menu->insertItem(name, menu);
        current_menu->menu->connectItem(id, this, SLOT(clicked()));
      }
    }else{
      int id = current_menu->menu->insertItem(name, this, SLOT(clicked()));
      if(current_menu->base==NULL){
        if(checkable==true)
          current_menu->menu->setItemChecked(id, checkval);
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
    if(checkable==true){
      char temp[500];
      this->checkval = this->checkval==1?0:1; // switch value.
      sprintf(temp,python_command,checkval?"1":"0");
      DO_GFX(PyRun_SimpleString(temp));
    }else
      DO_GFX(PyRun_SimpleString(python_command));
    closeRequester();
    static_cast<EditorWidget*>(window->os_visual.widget)->updateEditor();
    //static_cast<EditorWidget*>(window->os_visual.widget)->repaint(); // Why isn't calling updateEditor() enough?
    if(doquit==true)
      qapplication->quit();
  }
};

#include "mQt_Menues.cpp"


void GFX_AddMenuItem(struct Tracker_Windows *tvisual, const char *name, const char *python_command){
  new MenuItem(name, python_command);
}

void GFX_AddCheckableMenuItem(struct Tracker_Windows *tvisual, const char *name, const char *python_command, int checkval){
  new MenuItem(name, python_command, NULL, true, checkval);
}

void GFX_AddMenuSeparator(struct Tracker_Windows *tvisual){
  if(current_menu->menu==NULL){
    RError("Can not add separator at toplevel");
    return;
  }
  current_menu->menu->insertSeparator();
}

void GFX_AddMenuMenu(struct Tracker_Windows *tvisual, const char *name, const char *command){
  struct Menues *menu = (struct Menues*)calloc(1, sizeof(struct Menues));
  menu->up = current_menu;
  menu->menu = new Q3PopupMenu();
  //QFont sansFont("Liberation Mono", 8);
  //QFont sansFont("DejaVu Sans Mono", 7);
  //QFont sansFont("Nimbus Mono L", 9);
  //QFont sansFont("WenQuanYi Zen Hei Mono",8);
  //QFont sansFont("Aurulent Sans Mono",8);
  //QFont sansFont(QApplication::font());
  QFont sansFont;

  sansFont.setFamily("Bitstream Vera Sans Mono");
  sansFont.setStyleName("Bold");
  sansFont.setPointSize(QApplication::font().pointSize());
  //sansFont.setBold(true);

  menu->menu->setFont(sansFont);
  new MenuItem(name, command, menu->menu);
  current_menu = menu;
}

void GFX_GoPreviousMenuLevel(struct Tracker_Windows *tvisual){
  if(current_menu->base!=NULL){
    RError("Already at top level");
    return;
  }
  current_menu = current_menu->up;
}

/*
static Menues *g_base_menues;
void gakk(){
  //g_base_menues->base->setActiveAction(g_base_menues->up->base->actionAt(QPoint(0,0)));
  //g_base_menu->setActiveAction(g_base_menu->actionAt(QPoint(0,0)));
  g_base_menues->base->show();
  current_menu->base->show();
  //current_menu->menu->show();
}
*/

bool GFX_MenuActive(){
  return current_menu->base->activeAction() != NULL;
}

void initMenues(QMenuBar *base_menu){
  current_menu = (struct Menues*)calloc(1, sizeof(struct Menues));
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


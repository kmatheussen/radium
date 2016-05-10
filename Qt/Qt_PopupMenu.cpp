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

#include <QAction>
#include <QWidgetAction>
#include <QCheckBox>
#include <QMenu>

#include "../common/nsmtracker.h"
#include "../common/visual_proc.h"

#include "../api/api_proc.h"

#include "EditorWidget.h"


static const int max_submenues = 200;

namespace{
  class CheckableAction : public QWidgetAction
  {
    Q_OBJECT

    QString text;
    QMenu *qmenu;
    int num;
    func_t *callback;

  public:

    ~CheckableAction(){
      printf("I was deleted: %s\n",text.toUtf8().constData());
    }
    
    CheckableAction(const QString & text, bool is_on, QMenu *qmenu, int num, func_t *callback)
      : QWidgetAction(qmenu)
      , text(text)
      , qmenu(qmenu)
      , num(num)
      , callback(callback)
    {
      QCheckBox *checkBox = new QCheckBox(text, qmenu);
      setDefaultWidget(checkBox);
      
      checkBox->setChecked(is_on);
            
      connect(checkBox, SIGNAL(toggled(bool)), this, SLOT(toggled(bool)));      
    }

  public slots:
    void toggled(bool checked){
    //void clicked(bool checked){
      printf("CLICKED %d\n",checked);
      if (callback!=NULL)
        callFunc_void_int_bool(callback, num, checked);
      qmenu->close();
      //delete parent;
    }
  };
}

int GFX_Menu2(
              struct Tracker_Windows *tvisual,
              ReqType reqtype,
              const char *seltext,
              vector_t *v,
              func_t *callback
              )
{
  if(reqtype==NULL || v->num_elements>20){
   
    QMenu menu(0);
    QMenu *curr_menu = &menu;

    int n_submenues=0;

    for(int i=0;i<v->num_elements;i++) {
      QString text = (const char*)v->elements[i];
      if (text.startsWith("----"))
        menu.addSeparator();
      else {
        
        if (n_submenues==max_submenues){
          curr_menu = curr_menu->addMenu("Next");
          n_submenues=0;
        }

        QAction *action = NULL;


        bool disabled = false;
        
        if (text.startsWith("[disabled]")){
          text = text.right(text.size() - 10);
          disabled = true;
        }
        
        if (text.startsWith("[check ")){
          if (text.startsWith("[check on]"))
            action = new CheckableAction(text.right(text.size() - 10), true, curr_menu, i, callback);
          else
            action = new CheckableAction(text.right(text.size() - 11), false, curr_menu, i, callback);
        } else if (text.startsWith("[submenu start]")){
          curr_menu = curr_menu->addMenu(text.right(text.size() - 15));
          n_submenues = 0;
        } else if (text.startsWith("[submenu end]")){
          QMenu *parent = dynamic_cast<QMenu*>(curr_menu->parent());
          if (parent==NULL)
            RError("parent of [submenu end] is not a QMenu");
          else
            curr_menu = parent;
          n_submenues = 0;
        } else
          action = new QAction(text, curr_menu);
        
        if (action != NULL){
          action->setData(i);
          curr_menu->addAction(action);  // are these actions automatically freed in ~QMenu?
        }
        
        if (disabled)
          action->setDisabled(true);

        n_submenues++;
      }
    }


    QAction *action;

    obtain_keyboard_focus(); {
    
      if (doModalWindows()) {
        
        GL_lock();{
          action = menu.exec(QCursor::pos());
        }GL_unlock();
        
      } else {
        
        GL_lock();{
          GL_pause_gl_thread_a_short_while();
        }GL_unlock();    
        action = menu.exec(QCursor::pos());
        
      }

    } release_keyboard_focus();
      
    if(action==NULL)
      return -1;

    if (dynamic_cast<CheckableAction*>(action) != NULL)
      return -1;
    
    bool ok;
    int i=action->data().toInt(&ok);

    if (ok)      
      return i;
    
    //RWarning("Got unknown action %p %s\n",action,action->text().toAscii().constData());

    return -1;

  }else{

    return GFX_ReqTypeMenu(tvisual,reqtype,seltext,v);

  }
}


int GFX_Menu(
             struct Tracker_Windows *tvisual,
             ReqType reqtype,
             const char *seltext,
             vector_t *v
             )
{
  return GFX_Menu2(tvisual, reqtype, seltext, v, NULL);
}

// The returned vector can be used as argument for GFX_Menu.
vector_t *GFX_MenuParser(const char *texts, const char *separator){
  vector_t *ret = (vector_t*)talloc(sizeof(vector_t));
  
  QStringList splitted = QString(texts).split(separator);
  
  for(int i=0 ; i<splitted.size() ; i++){
    QString trimmed = splitted[i].trimmed();
    if (trimmed != "")
      VECTOR_push_back(ret, talloc_strdup(trimmed.toUtf8().constData()));
  }

  return ret;
}

#include "mQt_PopupMenu.cpp"

#endif // USE_QT_MENU

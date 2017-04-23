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

#include "../common/nsmtracker.h"
#include "../common/visual_proc.h"
#include "../embedded_scheme/s7extra_proc.h"

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
      //printf("I was deleted: %s\n",text.toUtf8().constData());
      if(callback!=NULL)
        s7extra_unprotect(callback);
    }
    
    CheckableAction(const QString & text_b, bool is_on, QMenu *qmenu_b, int num_b, func_t *callback_b)
      : QWidgetAction(qmenu_b)
      , text(text_b)
      , qmenu(qmenu_b)
      , num(num_b)
      , callback(callback_b)
    {
      if(callback!=NULL)
        s7extra_protect(callback);

      QWidget *widget = new QWidget;
      QHBoxLayout *layout = new QHBoxLayout;
      layout->setSpacing(0);
      layout->setContentsMargins(0,0,0,0);

      layout->addItem(new QSpacerItem(gui_textWidth("F"), 10, QSizePolicy::Fixed, QSizePolicy::MinimumExpanding));
      
      QCheckBox *checkBox = new QCheckBox(text, qmenu);
      checkBox->setChecked(is_on);

      layout->addWidget(checkBox);
            
      widget->setLayout(layout);
      setDefaultWidget(widget);

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

  class ClickableAction : public QAction
  {
    Q_OBJECT

    QString text;
    QMenu *qmenu;
    int num;
    func_t *callback;

  public:

    ~ClickableAction(){
      //printf("I was deleted: %s\n",text.toUtf8().constData());
      if(callback!=NULL)
        s7extra_unprotect(callback);
    }
    
    ClickableAction(const QString & text, QMenu *qmenu, int num, func_t *callback)
      : QAction(text, qmenu)
      , text(text)
      , qmenu(qmenu)
      , num(num)
      , callback(callback)
    {
      if(callback!=NULL)
        s7extra_protect(callback);
      connect(this, SIGNAL(triggered()), this, SLOT(triggered()));      
    }

  public slots:
    void triggered(){
    //void clicked(bool checked){
      printf("CLICKED clickable\n");
      if (callback!=NULL)
        s7extra_callFunc_void_int(callback, num);
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
              func_t *callback,
              bool is_async
              )
{
  if(is_async)
    R_ASSERT(callback!=NULL);

  if(reqtype==NULL || v->num_elements>20 || is_async || callback!=NULL){
   
    QMenu *menu = new QMenu(NULL);
    menu->setAttribute(Qt::WA_DeleteOnClose);

    QMenu *curr_menu = menu;

    QStack<QMenu*> parents;
    QStack<int> n_submenuess;
    int n_submenues=0;

    for(int i=0;i<v->num_elements;i++) {
      QString text = (const char*)v->elements[i];
      if (text.startsWith("----"))
        menu->addSeparator();
      else {
        
        if (n_submenues==max_submenues){
          curr_menu = curr_menu->addMenu("Next");
          //curr_menu->setStyleSheet("QMenu { menu-scrollable: 1; }");
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
          
          n_submenuess.push(n_submenues);
          n_submenues = 0;
          parents.push(curr_menu);
          curr_menu = curr_menu->addMenu(text.right(text.size() - 15));
          //curr_menu->setStyleSheet("QMenu { menu-scrollable: 1; }");
                    
        } else if (text.startsWith("[submenu end]")){
          
          QMenu *parent = parents.pop();
          if (parent==NULL)
            RError("parent of [submenu end] is not a QMenu");
          else
            curr_menu = parent;
          n_submenues = n_submenuess.pop();
          
        } else {
          
          action = new ClickableAction(text, curr_menu, i, callback);

        }
        
        if (action != NULL){
          action->setData(i);
          curr_menu->addAction(action);  // are these actions automatically freed in ~QMenu? (yes, seems so)
        }
        
        if (disabled)
          action->setDisabled(true);

        n_submenues++;
      }
    }


    if (is_async) {
      //QMenu *dasmenu = new QMenu(menu);
      //dasmenu->setAttribute(Qt::WA_DeleteOnClose);
      //safeShow(menu);
      menu->popup(QCursor::pos());
      return -1;
    }

    QAction *action = safeExec(menu);

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
  return GFX_Menu2(tvisual, reqtype, seltext, v, NULL, false);
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

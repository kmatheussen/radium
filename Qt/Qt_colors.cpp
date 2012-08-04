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


#include <qwidget.h>
#include <qcolor.h>
#include <qapplication.h>
#include <qpalette.h>
#include <qcolordialog.h>
#include <qtimer.h>

#ifdef USE_QT3
#include <qobjectlist.h>
#endif

#include "EditorWidget.h"
#include "Qt_colors_proc.h"

#include "../common/settings_proc.h"
#include "../common/windows_proc.h"
#include "../common/gfx_proc.h"
#include "GTK_visual_proc.h"


extern struct Root *root;

static QApplication *application;
static QColor *system_color=NULL;
static bool override_default_qt_colors=true;

static char *SETTINGS_read_string(const char *a,const char *b){
  return SETTINGS_read_string((char*)a,(char*)b);
}

static void updatePalette(EditorWidget *my_widget, QPalette &pal){
  if(system_color==NULL)
    system_color=new QColor(SETTINGS_read_string("system_color","#d2d0d5"));

  if(override_default_qt_colors==false)
    return;

  // Background
  {
    //QColor c(0xe5, 0xe5, 0xe5);
    QColor c(*system_color);
    pal.setColor( QPalette::Active, QColorGroup::Background, c);
    pal.setColor( QPalette::Active, QColorGroup::Button, c);
    pal.setColor( QPalette::Inactive, QColorGroup::Background, c);
    pal.setColor( QPalette::Inactive, QColorGroup::Button, c);

    pal.setColor( QPalette::Disabled, QColorGroup::Background, c.light(97));
    pal.setColor( QPalette::Disabled, QColorGroup::Button, c.light(90));

    pal.setColor(QPalette::Active, QColorGroup::Base, c);
    pal.setColor(QPalette::Inactive, QColorGroup::Base, c);
  }

  // Foreground, text, etc. (everything blackish)
  {
    QColor c(my_widget==NULL ? QColor(SETTINGS_read_string("color1","black")) : my_widget->colors[1]);
    pal.setColor(QPalette::Active, QColorGroup::Foreground, c);
    pal.setColor(QPalette::Inactive, QColorGroup::Foreground, c.light(97));
    pal.setColor(QPalette::Disabled, QColorGroup::Foreground, c.light(90));
    pal.setColor(QPalette::Active, QColorGroup::Foreground, c);
    pal.setColor(QPalette::Inactive, QColorGroup::Foreground, c.light(97));
    pal.setColor(QPalette::Disabled, QColorGroup::Foreground, c.light(90));

    pal.setColor(QPalette::Active, QColorGroup::ButtonText, c);
    pal.setColor(QPalette::Inactive, QColorGroup::ButtonText, c.light(97));
    pal.setColor(QPalette::Disabled, QColorGroup::ButtonText, c.light(90));
    pal.setColor(QPalette::Active, QColorGroup::ButtonText, c);
    pal.setColor(QPalette::Inactive, QColorGroup::ButtonText, c.light(97));
    pal.setColor(QPalette::Disabled, QColorGroup::ButtonText, c.light(90));

    pal.setColor(QPalette::Active, QColorGroup::Text, c);
    pal.setColor(QPalette::Inactive, QColorGroup::Text, c.light(97));
    pal.setColor(QPalette::Disabled, QColorGroup::Text, c.light(90));
    pal.setColor(QPalette::Active, QColorGroup::Text, c);
    pal.setColor(QPalette::Inactive, QColorGroup::Text, c.light(97));
    pal.setColor(QPalette::Disabled, QColorGroup::Text, c.light(90));
  }
}

static void updateWidget(EditorWidget *my_widget,QWidget *widget){
  QPalette pal(widget->palette());
  updatePalette(my_widget,pal);
  widget->setPalette(pal);
}

static void updateApplication(EditorWidget *my_widget,QApplication *app){
  QPalette pal(app->palette());
  updatePalette(my_widget,pal);
  app->setPalette(pal);
}

static void updateAll(EditorWidget *my_widget, QWidget *widget){
  updateWidget(my_widget, widget);

#ifdef USE_QT3
  const QObjectList *l = widget->children();
  if(l==NULL)
    return;

  QObjectListIt it( *l );
  QObject *obj;

  while ( (obj = it.current()) != 0 ) {
    ++it;
    QWidget *widget = dynamic_cast<QWidget*>(obj);
    if(widget!=NULL)
      updateAll(my_widget,widget);
  }
#endif
}

static void updateAll(EditorWidget *my_widget){
  updateAll(my_widget,application->mainWidget());
  updateApplication(my_widget,application);
}

void setWidgetColors(QWidget *widget){
  EditorWidget *my_widget = static_cast<EditorWidget*>(root->song->tracker_windows->os_visual.widget);
  updateAll(my_widget,widget);
}

void setApplicationColors(QApplication *app){
  static bool first_run = true;
  EditorWidget *my_widget = root==NULL ? NULL : root->song==NULL ? NULL : static_cast<EditorWidget*>(root->song->tracker_windows->os_visual.widget);

  if(first_run==true){
    override_default_qt_colors = SETTINGS_read_bool((char*)"override_default_qt_colors",true);
    SETTINGS_write_bool((char*)"override_default_qt_colors",override_default_qt_colors);
    first_run=false;
  }

  application = app;
  updateApplication(my_widget,app);
}


void setEditorColors(EditorWidget *my_widget){
  my_widget->colors[0]=QColor(SETTINGS_read_string("color0","#d0d5d0"));
  my_widget->colors[1]=QColor(SETTINGS_read_string("color1","black"));
  my_widget->colors[2]=QColor(SETTINGS_read_string("color2","white"));
  my_widget->colors[3]=QColor(SETTINGS_read_string("color3","blue"));

  my_widget->colors[4]=QColor(SETTINGS_read_string("color4","yellow"));
  my_widget->colors[5]=QColor(SETTINGS_read_string("color5","red"));
  my_widget->colors[6]=QColor(SETTINGS_read_string("color6","orange"));

  my_widget->colors[7]=QColor(SETTINGS_read_string("color7","#101812"));

  my_widget->colors[8]=QColor(SETTINGS_read_string("color8","#452220"));

  my_widget->colors[9]=QColor(SETTINGS_read_string("system_color","#123456"));

  my_widget->colors[10]=QColor(SETTINGS_read_string("color10","#777777"));

  for(int i=0 ; i<=10 ; i++)
    GTK_SetColor(i,
                 my_widget->colors[i].red(),
                 my_widget->colors[i].green(),
                 my_widget->colors[i].blue()
                 );
}

static void setColor(EditorWidget *my_widget,int num, const QRgb &rgb){
  if(num>10)
    return;

  GTK_SetColor(num,qRed(rgb),qGreen(rgb),qBlue(rgb));

  if(num!=9){
    my_widget->colors[num].setRgb(rgb);
  }else
    system_color->setRgb(rgb);
}

void testColorInRealtime(int num, QColor color){
  if(num>10)
    return;

  struct Tracker_Windows *window = root->song->tracker_windows;
  EditorWidget *my_widget=(EditorWidget *)window->os_visual.widget;
  setColor(my_widget,num,color.rgb());
  updateAll(my_widget);

  if(false && num==0)
    my_widget->repaint(); // todo: fix flicker.
  else{
    // Doesn't draw everything.
    DO_GFX_BLT({
        DrawUpTrackerWindow(window);
      });
    my_widget->update();
  }

}

// Workaround, expose events are not sent when the qcolor dialog blocks it. Only happens when using the GTK visual.
// Don't think it's a bug, it's just that Qt only calls qapplication->processEvents() (somehow), and not gtk_main_iteration_do(),
// from the QDialog exec loop.
class Scoped_GTK_EventHandler_Timer : public QTimer{
  Q_OBJECT

public:
  Scoped_GTK_EventHandler_Timer(){
#if USE_GTK_VISUAL
    connect( this, SIGNAL(timeout()), this, SLOT(call_GTK_HandleEvents()));
    start( 10, FALSE );
#endif
  }
public slots:
  void call_GTK_HandleEvents(){
#if USE_GTK_VISUAL
    GTK_HandleEvents();
#endif
  }
};

#include "mQt_colors.cpp"

void GFX_ConfigColors(struct Tracker_Windows *tvisual){
  static bool is_running = false;

  if(is_running==true)
    return;
  is_running = true;

  EditorWidget *editorwidget=(EditorWidget *)tvisual->os_visual.widget;

  override_default_qt_colors = SETTINGS_read_bool((char*)"override_default_qt_colors",true);

  QColorDialog::setCustomColor(9, system_color->rgb());
  for(int i=0;i<16;i++)
    if(i!=9)
      QColorDialog::setCustomColor(i, editorwidget->colors[i].rgb());

  Scoped_GTK_EventHandler_Timer eventhandler;

  if(QColorDialog::getColor(editorwidget->colors[0],editorwidget).isValid()==false){
    // "cancel"
    setEditorColors(editorwidget); // read back from file.
    system_color->setRgb(QColor(SETTINGS_read_string("system_color","#d2d0d5")).rgb());
    DrawUpTrackerWindow(root->song->tracker_windows);
  }else{
    // "ok"
    SETTINGS_write_string((char*)"system_color",(char*)system_color->name().ascii());
    system_color->setRgb(QColorDialog::customColor(9));

    for(int i=0;i<16;i++){
      setColor(editorwidget,i,QColorDialog::customColor(i));
      char key[500];
      sprintf(key,"color%d",i);
      SETTINGS_write_string(key,(char*)editorwidget->colors[i].name().ascii());
    }
  }

  updateAll(editorwidget);

  is_running = false;
}

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

#ifdef USE_QT3
#include <qobjectlist.h>
#endif

#include "MyWidget.h"
#include "Qt_colors_proc.h"

#include "../common/settings_proc.h"
#include "../common/windows_proc.h"
#include "../common/gfx_proc.h"


extern struct Root *root;

static QApplication *application;
static QColor *system_color=NULL;

static char *SETTINGS_read_string(const char *a,const char *b){
  return SETTINGS_read_string((char*)a,(char*)b);
}

static void updatePalette(MyWidget *my_widget, QPalette &pal){
  if(system_color==NULL)
    system_color=new QColor(SETTINGS_read_string("system_color","#d2d0d5"));

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

static void updateWidget(MyWidget *my_widget,QWidget *widget){
  QPalette pal(widget->palette());
  updatePalette(my_widget,pal);
  widget->setPalette(pal);
}

static void updateApplication(MyWidget *my_widget,QApplication *app){
  QPalette pal(app->palette());
  updatePalette(my_widget,pal);
  app->setPalette(pal);
}

static void updateAll(MyWidget *my_widget, QWidget *widget){
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

static void updateAll(MyWidget *my_widget){
  updateAll(my_widget,application->mainWidget());
  updateApplication(my_widget,application);
}

void setWidgetColors(QWidget *widget){
  MyWidget *my_widget = static_cast<MyWidget*>(root->song->tracker_windows->os_visual.widget);
  updateAll(my_widget,widget);
}

void setApplicationColors(QApplication *app){
  MyWidget *my_widget = root==NULL ? NULL : root->song==NULL ? NULL : static_cast<MyWidget*>(root->song->tracker_windows->os_visual.widget);
  application = app;
  updateApplication(my_widget,app);
}

void setEditorColors(MyWidget *my_widget){
  my_widget->colors[0]=QColor(SETTINGS_read_string("color0","#d0d5d0"));
  my_widget->colors[1]=QColor(SETTINGS_read_string("color1","black"));
  my_widget->colors[2]=QColor(SETTINGS_read_string("color2","white"));
  my_widget->colors[3]=QColor(SETTINGS_read_string("color3","blue"));

  my_widget->colors[4]=QColor(SETTINGS_read_string("color4","yellow"));
  my_widget->colors[5]=QColor(SETTINGS_read_string("color5","red"));
  my_widget->colors[6]=QColor(SETTINGS_read_string("color6","orange"));

  my_widget->colors[7]=QColor(SETTINGS_read_string("color7","#101812"));

  my_widget->colors[8]=QColor(SETTINGS_read_string("color8","#452220"));
}


void testColorInRealtime(int num, QColor color){
  struct Tracker_Windows *window = root->song->tracker_windows;
  MyWidget *my_widget=(MyWidget *)window->os_visual.widget;
  if(num<9)
    my_widget->colors[num].setRgb(color.rgb());
  else if(num==9)
    system_color->setRgb(color.rgb());
  else
    return;
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

void GFX_ConfigColors(struct Tracker_Windows *tvisual){
  static bool is_running = false;

  if(is_running==true)
    return;
  is_running = true;

  MyWidget *my_widget=(MyWidget *)tvisual->os_visual.widget;

  QColorDialog::setCustomColor(9, system_color->rgb());
  for(int i=0;i<9;i++)
    QColorDialog::setCustomColor(i, my_widget->colors[i].rgb());

  if(QColorDialog::getColor(my_widget->colors[0]).isValid()==false){
    // "cancel"
    setEditorColors(my_widget); // read back from file.
    system_color->setRgb(QColor(SETTINGS_read_string("system_color","#d2d0d5")).rgb());
    DrawUpTrackerWindow(root->song->tracker_windows);
  }else{
    // "ok"
    SETTINGS_write_string((char*)"system_color",(char*)system_color->name().ascii());
    system_color->setRgb(QColorDialog::customColor(9));

    for(int i=0;i<9;i++){
      my_widget->colors[i].setRgb(QColorDialog::customColor(i));
      char key[500];
      sprintf(key,"color%d",i);
      SETTINGS_write_string(key,(char*)my_widget->colors[i].name().ascii());
    }
  }

  updateAll(my_widget);

  is_running = false;
}

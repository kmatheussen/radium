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
#include <qcombobox.h>

#include "qcolordialog.h"
#include <qtimer.h>
#include <qfile.h>

#ifdef USE_QT3
#include <qobjectlist.h>
#endif

#include "EditorWidget.h"
#include "Qt_colors_proc.h"

#include "../common/settings_proc.h"
#include "../common/windows_proc.h"
#include "../common/gfx_proc.h"
#include "../GTK/GTK_visual_proc.h"
#include "../common/OS_settings_proc.h"


extern struct Root *root;

static QApplication *application;
static QColor *system_color=NULL;
static QColor *button_color=NULL;
static bool override_default_qt_colors=true;

static QColor g_note_colors[128];

static QColor mix_colors(const QColor &c1, const QColor &c2, float how_much){

  float a1 = how_much;
  float a2 = 1.0f-a1;

  int r = c1.red()*a1 + c2.red()*a2;
  int g = c1.green()*a1 + c2.green()*a2;
  int b = c1.blue()*a1 + c2.blue()*a2;

  return QColor(r,g,b);
}

#if 0
#define NUM_NOTE_COLOR_CONF 4
static const int note_color_conf[NUM_NOTE_COLOR_CONF+1][2] = {
  {128*0,                     1},
  {128*1/NUM_NOTE_COLOR_CONF, 5},
  {128*2/NUM_NOTE_COLOR_CONF, 6},
  {128*3/NUM_NOTE_COLOR_CONF, 4},
  {128*4/NUM_NOTE_COLOR_CONF, 2},
};
#endif

#if 0
#define NUM_NOTE_COLOR_CONF 9
static const int note_color_conf[NUM_NOTE_COLOR_CONF+1][2] = {
  {128*0,1},
  {128*1/NUM_NOTE_COLOR_CONF,2},
  {128*2/NUM_NOTE_COLOR_CONF,3},
  {128*3/NUM_NOTE_COLOR_CONF,4},
  {128*4/NUM_NOTE_COLOR_CONF,5},
  {128*5/NUM_NOTE_COLOR_CONF,6},
  {128*6/NUM_NOTE_COLOR_CONF,8},
  {128*7/NUM_NOTE_COLOR_CONF,12},
  {128*8/NUM_NOTE_COLOR_CONF,13},
  {128*9/NUM_NOTE_COLOR_CONF,14}
};
#endif

#define NUM_NOTE_COLOR_CONF 6
static const int note_color_conf[NUM_NOTE_COLOR_CONF+1][2] = {
  {0,3},
  {24,4},
  {48,3},
  {72,4},
  {96,5},
  {120,6},
  {128,8}
};

static void configure_note_colors(EditorWidget *editor){
  for(int i=0;i<NUM_NOTE_COLOR_CONF;i++){
    QColor start_color = editor->colors[note_color_conf[i][1]];
    QColor end_color = editor->colors[note_color_conf[i+1][1]];
    int start_note = note_color_conf[i][0];
    int end_note = note_color_conf[i+1][0];

    if(start_note<64)
      start_color = editor->colors[1];
    else
      end_color = editor->colors[2];

    for(int note=start_note;note<end_note;note++){
      //printf("setting %d (%d-%d) to %d %d %f\n",note,start_note,end_note,note_color_conf[i][1], note_color_conf[i+1][1], (float)(note-start_note)/(end_note-start_note));
      g_note_colors[note] = mix_colors(end_color, start_color, (float)(note-start_note)/(end_note-start_note));
    }
  }
}


QHash<int, QColor> custom_colors;
static const int first_custom_colornum = 1024; // just start somewhere.

// if colornum==-1, create new color
int GFX_MakeRandomCustomColor(struct Tracker_Windows *tvisual, int colornum){
  static int num_colors = first_custom_colornum;

  if (colornum==-1)
    colornum = num_colors++;

  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;

  custom_colors[colornum] = mix_colors(QColor(qrand()%255,qrand()%255,qrand()%255), editor->colors[15], 0.10f);

  return colornum;
}

QColor get_qcolor(struct Tracker_Windows *tvisual, int colornum){
  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;

  if(colornum < 16)
    return editor->colors[colornum];

  if(colornum >= first_custom_colornum)
    return custom_colors[colornum];

  if(colornum > 16+128){
    RError("Illegal colornum: %d",colornum);
    colornum = colornum % (128+16);
  }

  colornum -= 16;

  static bool note_colors_configured = false;
  if(note_colors_configured==false){
    configure_note_colors(editor);
    note_colors_configured=true;
  }

  return g_note_colors[colornum];
}


static void updatePalette(EditorWidget *my_widget, QWidget *widget, QPalette &pal){
  if(system_color==NULL){
    system_color=new QColor(SETTINGS_read_string("system_color","#d2d0d5"));
    SETTINGS_write_string("system_color",system_color->name().ascii());
  }
  if(button_color==NULL){
    button_color=new QColor(SETTINGS_read_string("button_color","#c1f1e3"));
    SETTINGS_write_string("button_color",button_color->name().ascii());
  }

  if(override_default_qt_colors==false){
    //qapplication->setPalette(t.palette());

    //my_widget->setPalette( QApplication::palette( my_widget ) );
    return;
  }

  // Background
  {

    //QColor c(0xe5, 0xe5, 0xe5);
    QColor c(*system_color);
    QColor b(*button_color);

    if(dynamic_cast<QComboBox*>(widget)!=NULL){
      c = my_widget->colors[13];
      c=mix_colors(c.light(70),QColor(98,59,33),0.55);//editor->colors[colnum].light(52);
      c.setAlpha(76);
    }

    pal.setColor( QPalette::Active, QColorGroup::Background, b);
    pal.setColor( QPalette::Inactive, QColorGroup::Background, b);
    pal.setColor( QPalette::Disabled, QColorGroup::Background, b.light(95));

    pal.setColor( QPalette::Active, QColorGroup::Button, c);
    pal.setColor( QPalette::Inactive, QColorGroup::Button, c);
    pal.setColor( QPalette::Disabled, QColorGroup::Button, c.light(80));

    pal.setColor(QPalette::Active, QColorGroup::Base, c);
    pal.setColor(QPalette::Inactive, QColorGroup::Base, c);
    pal.setColor(QPalette::Disabled, QColorGroup::Base, c.light(80));

    pal.setBrush(QPalette::Highlight, (const QBrush&)QBrush(b.light(85)));

    pal.setColor(QPalette::Disabled, QPalette::Light, c.light(80));

    //pal.setBrush((QPalette::ColorGroup)QColorGroup::Button, QPalette::Highlight, (const QBrush&)QBrush(b.light(95)));
    //pal.setBrush(QPalette::Highlight, QColorGroup::Button, QBrush(c.light(80)));
    //pal.setBrush(QPalette::Highlight, QColorGroup::Base, QBrush(c.light(80)));


#if 0
    pal.setColor(QPalette::Active, QColorGroup::Window, c);
    pal.setColor(QPalette::Inactive, QColorGroup::Window, c);
    pal.setColor(QPalette::Disabled, QColorGroup::Window, c.light(80));
#endif

#if 0
    pal.setColor(QPalette::Active, QColorGroup::BrightText, c);
    pal.setColor(QPalette::Inactive, QColorGroup::BrightText, c);
    pal.setColor(QPalette::Disabled, QColorGroup::BrightText, c.light(80));
#endif

  }

  // Foreground, text, etc. (everything blackish)
  {
    QColor c(my_widget==NULL ? QColor(SETTINGS_read_string("color1","black")) : my_widget->colors[1]);
    c.setAlpha(180);

    pal.setColor(QPalette::Active, QColorGroup::Foreground, c);
    pal.setColor(QPalette::Inactive, QColorGroup::Foreground, c.light(93));
    pal.setColor(QPalette::Disabled, QColorGroup::Foreground, c.light(80));

    pal.setColor(QPalette::Active, QColorGroup::Foreground, c);
    pal.setColor(QPalette::Inactive, QColorGroup::Foreground, c.light(93));
    pal.setColor(QPalette::Disabled, QColorGroup::Foreground, c.light(80));

    pal.setColor(QPalette::Active, QColorGroup::ButtonText, c);
    pal.setColor(QPalette::Inactive, QColorGroup::ButtonText, c.light(93));
    pal.setColor(QPalette::Disabled, QColorGroup::ButtonText, c.light(80));

    pal.setColor(QPalette::Active, QColorGroup::ButtonText, c);
    pal.setColor(QPalette::Inactive, QColorGroup::ButtonText, c.light(93));
    pal.setColor(QPalette::Disabled, QColorGroup::ButtonText, c.light(80));

    pal.setColor(QPalette::Active, QColorGroup::Text, c);
    pal.setColor(QPalette::Inactive, QColorGroup::Text, c.light(90));
    pal.setColor(QPalette::Disabled, QColorGroup::Text, c.light(80));

    pal.setColor(QPalette::Active, QColorGroup::HighlightedText, c.light(100));
    pal.setColor(QPalette::Inactive, QColorGroup::HighlightedText, c.light(90));
    pal.setColor(QPalette::Disabled, QColorGroup::HighlightedText, c.light(80));

    pal.setColor(QPalette::Active, QColorGroup::Text, c);
    pal.setColor(QPalette::Inactive, QColorGroup::Text, c.light(90));
    pal.setColor(QPalette::Disabled, QColorGroup::Text, c.light(80));
  }
}

static QPalette sys_palette;

static void updateWidget(EditorWidget *my_widget,QWidget *widget){
  QPalette pal(widget->palette());

  updatePalette(my_widget,widget,pal);

  if(override_default_qt_colors)
    widget->setPalette(pal);
  else
    widget->setPalette(sys_palette);

  //QFont font=QFont("Bitstream Vera Sans Mono",4);
  //widget->setFont(QApplication::font());
}

static void updateApplication(EditorWidget *my_widget,QApplication *app){
  QPalette pal(app->palette());
  updatePalette(my_widget,NULL,pal);
  app->setPalette(pal);
}

static void updateAll(EditorWidget *my_widget, QWidget *widget){
  configure_note_colors(my_widget);

  updateWidget(my_widget, widget);

#if 0
  if(widget->objectName () == "view"){
    widget->setStyleSheet(QString::fromUtf8("background-color: qlineargradient(spread:reflect, x1:0, y1:0, x2:0.38, y2:1, stop:0 rgba(111, 131, 111, 22), stop:1 rgba(255, 255, 255, 43));"));
  }
#endif

#if 0
  widget->setStyleSheet(QString::fromUtf8("color: qlineargradient(spread:reflect, x1:0, y1:0, x2:0.38, y2:1, stop:0 rgba(0, 3, 0, 155), stop:1 rgba(5, 5, 5, 175));"));
#endif

  const QList<QObject*> list = widget->children();
  if(list.empty()==true)
    return;

  for (int i = 0; i < list.size(); ++i) {
    QWidget *widget = dynamic_cast<QWidget*>(list.at(i));
    if(widget!=NULL)
      updateAll(my_widget,widget);
  }
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
  EditorWidget *my_widget = root==NULL ? NULL : root->song==NULL ? NULL : static_cast<EditorWidget*>(root->song->tracker_windows->os_visual.widget);

  override_default_qt_colors = SETTINGS_read_bool("override_default_qt_colors",true);

#if 1
  static bool first_run = true;
  if(first_run==true){
    sys_palette = QApplication::palette();
    SETTINGS_write_bool("override_default_qt_colors",override_default_qt_colors);
    first_run=false;
  }
#endif
  printf("here\n");
  application = app;
  if(my_widget==NULL)
    updateApplication(my_widget,app);
  else
    updateAll(my_widget);
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

  my_widget->colors[11]=QColor(SETTINGS_read_string("button_color","#c1f1e3"));

  my_widget->colors[12]=QColor(SETTINGS_read_string("color12","black"));
  my_widget->colors[13]=QColor(SETTINGS_read_string("color13","green"));
  my_widget->colors[14]=QColor(SETTINGS_read_string("color14","blue"));
  my_widget->colors[15]=QColor(SETTINGS_read_string("color15","red"));

#if USE_GTK_VISUAL
  for(int i=0 ; i<16 ; i++)
    GTK_SetColor(i,
                 my_widget->colors[i].red(),
                 my_widget->colors[i].green(),
                 my_widget->colors[i].blue()
                 );
#endif
}

static void setColor(EditorWidget *my_widget,int num, const QRgb &rgb){
  if(num>=16)
    return;

#if USE_GTK_VISUAL
  GTK_SetColor(num,qRed(rgb),qGreen(rgb),qBlue(rgb));
#endif

  my_widget->colors[num].setRgb(rgb);

  if(num==9)
    system_color->setRgb(rgb);
  else if(num==11)
    button_color->setRgb(rgb);
}

void testColorInRealtime(int num, QColor color){
  if(num>=16)
    return;

  struct Tracker_Windows *window = root->song->tracker_windows;
  EditorWidget *my_widget=(EditorWidget *)window->os_visual.widget;
  setColor(my_widget,num,color.rgb());
  updateAll(my_widget);

  if(false && num==0)
    my_widget->repaint(); // todo: fix flicker.
  else{
    // Doesn't draw everything.
    DO_GFX({
        DrawUpTrackerWindow(window);
      });
    my_widget->updateEditor();
  }

  window->wblock->block->is_dirty = true;
}

#include "Qt_Main_proc.h"

// Workaround, expose events are not sent when the qcolor dialog blocks it. Only necessary when using the GTK visual.
// Qt only calls qapplication->processEvents() (somehow), and not gtk_main_iteration_do(), so we do that manually here.
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
    //Qt_EventHandler();
#endif
  }
};

#include "mQt_colors.cpp"

extern int num_users_of_keyboard;

void GFX_ConfigColors(struct Tracker_Windows *tvisual){
  static bool is_running = false;

  if(is_running==true)
    return;
  is_running = true;

  EditorWidget *editorwidget=(EditorWidget *)tvisual->os_visual.widget;

  override_default_qt_colors = SETTINGS_read_bool((char*)"override_default_qt_colors",true);

  for(int i=0;i<16;i++)
    QColorDialog3::setCustomColor(i, editorwidget->colors[i].rgb());

  Scoped_GTK_EventHandler_Timer eventhandler;

  num_users_of_keyboard++;
  if(QColorDialog3::getColor(editorwidget->colors[0],editorwidget).isValid()==false){
    // "cancel"
    printf("Got CANCEL!\n");
    setEditorColors(editorwidget); // read back from file.
    system_color->setRgb(QColor(SETTINGS_read_string("system_color","#d2d0d5")).rgb());
    button_color->setRgb(QColor(SETTINGS_read_string("button_color","#c1f1e3")).rgb());
    DrawUpTrackerWindow(root->song->tracker_windows);
  }else{
    // "ok"
    printf("Got OK!\n");

    SETTINGS_write_string((char*)"system_color",(char*)system_color->name().ascii());
    system_color->setRgb(QColorDialog3::customColor(9));

    SETTINGS_write_string((char*)"button_color",(char*)button_color->name().ascii());
    button_color->setRgb(QColorDialog3::customColor(11));

    for(int i=0;i<16;i++){
      setColor(editorwidget,i,QColorDialog3::customColor(i));
      char key[500];
      sprintf(key,"color%d",i);
      SETTINGS_write_string(key,(char*)editorwidget->colors[i].name().ascii());
    }
  }

  num_users_of_keyboard--;

  updateAll(editorwidget);

  is_running = false;
}

void GFX_SetDefaultColors(struct Tracker_Windows *tvisual){
  EditorWidget *editorwidget=(EditorWidget *)tvisual->os_visual.widget;

  const char* curr_dir = OS_get_program_path();
  const char* separator = OS_get_directory_separator();

  QFile::remove(QString(OS_get_config_filename("color0")) + "_old");
  QFile::rename(OS_get_config_filename("color0"), QString(OS_get_config_filename("color0")) + "_old");
  QFile::copy(QString(curr_dir)+separator+"colors", OS_get_config_filename("color0"));

  setEditorColors(editorwidget); // read back from file.
  system_color->setRgb(QColor(SETTINGS_read_string("system_color","#d2d0d5")).rgb());
  button_color->setRgb(QColor(SETTINGS_read_string("button_color","#c1f1e3")).rgb());
  DrawUpTrackerWindow(tvisual);
  updateAll(editorwidget);
}


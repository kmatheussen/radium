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

#if USE_QT_VISUAL

#include <math.h>

#include <qfontdialog.h>
#include <qapplication.h>

#include "../common/nsmtracker.h"
#include "../common/settings_proc.h"
#include "../common/wblocks_proc.h"
#include "../common/OS_settings_proc.h"

#include "EditorWidget.h"

#include "../OpenGL/GfxElements.h"

#include "Qt_Fonts_proc.h"


void setFontValues(struct Tracker_Windows *tvisual){
  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;

  editor->cursorbuffer_painter->setFont(editor->font);
  //editor->paintbuffer_painter->setFont(editor->font);

  GE_set_font(editor->font);

  const QFont &font=editor->font;

  QFontMetrics fm(font);

  double width3           = R_MAX(fm.width("D#6"), R_MAX(fm.width("MUL"), fm.width("STP")));
  tvisual->fontwidth      = (int)(width3/3.0) + 1;
  tvisual->org_fontheight = fm.height() - 1;
  tvisual->fontheight     = tvisual->org_fontheight;
}

void updateAllFonts(QWidget *widget){
  if(widget!=NULL){
    widget->setFont(QApplication::font());

    const QList<QObject*> list = widget->children();
    for (int i = 0; i < list.size(); ++i) {
      QWidget *widget = dynamic_cast<QWidget*>(list.at(i));
      updateAllFonts(widget);
    }
  }
}

extern int num_users_of_keyboard;

static void set_system_font(QFont font){
  QApplication::setFont(font);
  qApp->setFont(font);

  printf("Raw font name: \"%s\". family name: %s, style: %s\n",font.rawName().toUtf8().constData(),font.family().toUtf8().constData(),font.styleName().toUtf8().constData());

  {
    QFont write_font = font;
#if 0 //FOR_MACOSX
    font.setPointSize(font.pointSize()*72.0/96.0); // macs have dpi of 72, while linux and mac have 96.
#endif    
    SETTINGS_write_string("system_font",write_font.toString());
  }
  
  SETTINGS_write_string("system_font_style",font.styleName()); // toString doesn't seem to cover this.

  {
    struct Tracker_Windows *tvisual = root->song->tracker_windows;
    const QFont &font=QApplication::font();
    QFontMetrics fm(font);
    tvisual->systemfontheight=fm.height();
  }

  updateAllFonts(QApplication::mainWidget());
}

void GFX_ConfigSystemFont(void){
  num_users_of_keyboard++;
  QFont font = QFontDialog::getFont( 0, QApplication::font());
  num_users_of_keyboard--;

  set_system_font(font);
}

static char *GFX_SelectEditFont(struct Tracker_Windows *tvisual){
  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;
  num_users_of_keyboard++;
  editor->font = QFontDialog::getFont( 0, editor->font ) ;
  num_users_of_keyboard--;
  //editor->setFont(editor->font);

  printf("Raw font name: \"%s\"\n",editor->font.rawName().toUtf8().constData());

  setFontValues(tvisual);

  //SETTINGS_write_string("font_style",editor->font.styleName().toUtf8().constData()); // toString doesn't seem to cover this. (arrgh, there's a billion bugs in qt when it comes to font styles)
  return talloc_strdup((char*)editor->font.toString().toUtf8().constData());
}

void GFX_ConfigFonts(struct Tracker_Windows *tvisual){
  char *font = GFX_SelectEditFont(tvisual);
  UpdateAllWBlockWidths(tvisual);
  DrawUpTrackerWindow(tvisual);
  SETTINGS_write_string("font",font);
  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;
  SETTINGS_write_string("font_style",editor->font.styleName()); // toString doesn't seem to cover this.
}

void GFX_ResetFontSize(struct Tracker_Windows *tvisual){
  QFont font;

  {
    QString fontstring = SETTINGS_read_qstring("font","");
    font.fromString(fontstring);
  }

  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;
  editor->font.setPointSize(font.pointSize());

  setFontValues(tvisual);
  UpdateAllWBlockWidths(tvisual);
  DrawUpTrackerWindow(tvisual);

#if USE_OPENGL
  editor->position_gl_widget(tvisual);
#endif
}

void GFX_IncFontSize(struct Tracker_Windows *tvisual, int pixels){
  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;
  if(false && editor->font.pixelSize()!=-1){
    // not used
    editor->font.setPixelSize(editor->font.pixelSize()+pixels);
  }else{
    float org_size = editor->font.pointSize();
    for(int i=1;i<100;i++){
      int new_font_size = org_size+(i*pixels);
      if(new_font_size<2)
        return;
      editor->font.setPointSize(new_font_size);
      if(editor->font.pointSize()!=org_size)
        goto exit;
    }
    for(float i=1;i<100;i++){
      editor->font.setPointSize(org_size+(pixels/i));
      if(editor->font.pointSize()!=org_size)
        goto exit;
    }
  }
 exit:
  setFontValues(tvisual);

  UpdateAllWBlockWidths(tvisual);
  DrawUpTrackerWindow(tvisual);

#if USE_OPENGL
  editor->position_gl_widget(tvisual);
#endif
}

void GFX_SetDefaultFont(struct Tracker_Windows *tvisual){
  QFont font;

  SETTINGS_set_custom_configfile(QCoreApplication::applicationDirPath() + QString(OS_get_directory_separator()) + QString("config"));
  {
    QString fontstring = SETTINGS_read_qstring("font","");

    font.fromString(fontstring);
#if 0 //FOR_MACOSX
    font.setPointSize(font.pointSize()*96.0/72.0); // macs have dpi of 72, while linux and windows have 96.
#endif

    if(SETTINGS_read_qstring("font_style","")!="")
      font.setStyleName(SETTINGS_read_qstring("font_style",""));

  }
  SETTINGS_unset_custom_configfile();

  SETTINGS_write_string("font",font.toString());
  SETTINGS_write_string("font_style",font.styleName()); // toString doesn't seem to cover this.


  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;
  editor->font = font;
  //editor->setFont(editor->font);
  setFontValues(tvisual);

  UpdateAllWBlockWidths(tvisual);
  DrawUpTrackerWindow(tvisual);
}

void GFX_SetDefaultSystemFont(struct Tracker_Windows *tvisual){
  QFont font;

  SETTINGS_set_custom_configfile(QCoreApplication::applicationDirPath() + QString(OS_get_directory_separator()) + "config");
  {
    QString fontstring = SETTINGS_read_qstring("system_font","");

    font.fromString(fontstring);

#if 0 //FOR_MACOSX
    font.setPointSize(font.pointSize()*96.0/72.0); // macs have dpi of 72, while linux and windows have 96.
#endif

    if(SETTINGS_read_qstring("system_font_style","")!="")
      font.setStyleName(SETTINGS_read_qstring("system_font_style",""));

  }
  SETTINGS_unset_custom_configfile();

  set_system_font(font);
}

int GFX_get_text_width(struct Tracker_Windows *tvisual, const char *text){
  //EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;
  const QFontMetrics fn = QFontMetrics(QApplication::font()); //editor->font);
  return fn.width(text);
}

static bool does_text_fit(const QFontMetrics &fn, const QString &text, int pos, int max_width){
  return fn.width(text, pos) <= max_width;
}

static int average(int min, int max){
  return (1+min+max)/2;
}

// binary search
static int find_text_length(const QFontMetrics &fn, const QString &text, int max_width, int min, int max){
  if(max<=min)
    return min;

  int mid = average(min,max);

  if(does_text_fit(fn, text, mid, max_width))
    return find_text_length(fn, text, max_width, mid, max);
  else
    return find_text_length(fn, text, max_width, min, mid-1);
}

int GFX_get_num_characters(struct Tracker_Windows *tvisual, const char *text, int max_width){
  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;
  const QFontMetrics fn = QFontMetrics(editor->font);
  int len = strlen(text);
  QString string(text);

  //printf("width: %d / %d / %d\n",fn.width(string,len), fn.width(string,len/2), max_width);

  if(does_text_fit(fn, string, len, max_width))
    return len;
  else
    return find_text_length(fn, string, max_width, 0, len-1);
}

#endif // USE_QT_VISUAL

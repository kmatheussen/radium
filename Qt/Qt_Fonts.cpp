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

#include "Qt_Fonts_proc.h"


void setFontValues(struct Tracker_Windows *tvisual){
  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;

  editor->cursorbuffer_painter->setFont(editor->font);
  editor->paintbuffer_painter->setFont(editor->font);

  const QFont &font=editor->font;

  QFontMetrics fm(font);

  double width3 = R_MAX(fm.width("D#6"), R_MAX(fm.width("MUL"), fm.width("STP")));
  tvisual->fontwidth=(int)(width3/3.0) + 1;
  tvisual->org_fontheight=fm.height();
  tvisual->fontheight=fm.height();
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

  printf("Raw font name: \"%s\". family name: %s, style: %s\n",font.rawName().ascii(),font.family().ascii(),font.styleName().ascii());

  SETTINGS_write_string("system_font",font.toString().ascii());
  SETTINGS_write_string("system_font_style",font.styleName().ascii()); // toString doesn't seem to cover this.

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
  editor->setFont(editor->font);

  printf("Raw font name: \"%s\"\n",editor->font.rawName().ascii());

  setFontValues(tvisual);

  //SETTINGS_write_string("font_style",editor->font.styleName().ascii()); // toString doesn't seem to cover this. (arrgh, there's a billion bugs in qt when it comes to font styles)
  return talloc_strdup((char*)editor->font.toString().ascii());
}

void GFX_ConfigFonts(struct Tracker_Windows *tvisual){
  char *font = GFX_SelectEditFont(tvisual);
  UpdateAllWBlockWidths(tvisual);
  DrawUpTrackerWindow(tvisual);
  SETTINGS_write_string("font",font);
  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;
  SETTINGS_write_string("font_style",editor->font.styleName().ascii()); // toString doesn't seem to cover this.
}

void GFX_ResetFontSize(struct Tracker_Windows *tvisual){
  QFont font;

  //RWarning("GFX_ResetFontSize not implemented\n");
  SETTINGS_set_custom_configfile(QString(QString(OS_get_program_path())+OS_get_directory_separator()+"config").ascii());
  {
    const char *fontstring = SETTINGS_read_string("font",NULL);
    font.fromString(fontstring);
  }
  SETTINGS_unset_custom_configfile();

  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;
  editor->font.setPointSize(font.pointSize());

  setFontValues(tvisual);
  UpdateAllWBlockWidths(tvisual);
  DrawUpTrackerWindow(tvisual);
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
}

void GFX_SetDefaultFont(struct Tracker_Windows *tvisual){
  QFont font;

  SETTINGS_set_custom_configfile(QString(QString(OS_get_program_path())+OS_get_directory_separator()+"config").ascii());
  {
    const char *fontstring = SETTINGS_read_string("font",NULL);

    font.fromString(fontstring);
    if(SETTINGS_read_string("font_style",NULL)!=NULL)
      font.setStyleName(SETTINGS_read_string("font_style",NULL));

  }
  SETTINGS_unset_custom_configfile();

  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;
  editor->font = font;
  editor->setFont(editor->font);
  setFontValues(tvisual);

  UpdateAllWBlockWidths(tvisual);
  DrawUpTrackerWindow(tvisual);
}

void GFX_SetDefaultSystemFont(struct Tracker_Windows *tvisual){
  QFont font;

  SETTINGS_set_custom_configfile(QString(QString(OS_get_program_path())+OS_get_directory_separator()+"config").ascii());
  {
    const char *fontstring = SETTINGS_read_string("system_font",NULL);

    font.fromString(fontstring);
    if(SETTINGS_read_string("system_font_style",NULL)!=NULL)
      font.setStyleName(SETTINGS_read_string("system_font_style",NULL));

  }
  SETTINGS_unset_custom_configfile();

  set_system_font(font);
}

int GFX_get_text_width(struct Tracker_Windows *tvisual, const char *text){
  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;
  const QFontMetrics fn = QFontMetrics(editor->font);
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

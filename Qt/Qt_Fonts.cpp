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

static bool can_fit(const QFont &font, const QString &text, int flags, int width, int height){
  
  QFontMetrics fm(font);

  // Not an optimization. Seems like this test sometimes returns true even if the code below returns false.
  if (fm.width(text) < width && fm.height() < height)
    return true;

  QRect rect = fm.boundingRect(0, 0, width, height, flags, text);

  if (rect.width() >= width){
    return false;
  }
  
  if (rect.height() >= height){
    return false;
  }

  return true;
}

QFont GFX_getFittingFont(const QString &text, int flags, int width, int height){
  static QFont font;
  
  auto key = QPair<QString,
                   QPair<int,
                         QPair<int,int>
                         >
                   >
    (text,QPair<int,QPair<int,int>>(flags, QPair<int,int>(width,height))); // good code

  static QHash< QPair< QString , QPair< int, QPair<int,int> > > , QFont> fonts;

  if (font != qApp->font()){
    font = qApp->font();
    fonts.clear();
  }

  if (fonts.contains(key))
    return fonts.value(key);

  int pointSize = font.pointSize();

  QFont the_font(font);

  //return the_font;
  
  for(int size = pointSize; size > 3; size--){
    if(size!=pointSize)
      the_font.setPointSize(size);

    if (can_fit(the_font, text, flags, width, height))
      break;
  }

  fonts[key] = the_font;

  return the_font;
}

static QString split_text_by_lineshift(QString text, int num_splits, int num_possible_splits){
  if (num_splits==0)
    return text;
  
  const QChar space(' ');
  const int size = text.size();

  double inc = (double)num_possible_splits / (1.0 + num_splits);

  for(double fpos = floor(inc) ; fpos < num_possible_splits ; fpos += inc){
    int pos = round(fpos);

    int pos2 = 0;
    for(int i=0;i<size;i++){
      if (text.at(i) == space){
        if(pos2==pos){
          text = text.replace(i, 1, "\n");
          break;
        }
        pos2++;
      }
    }    
  }

  return text;
}

QString GFX_getFittingText(const QFont &font, const QString &text, const int flags, bool wrap_lines, const int width, const int height){

  if (can_fit(font, text, flags, width, height))
    return text;

  int n = 1;

  QString org(text);
  QString ret = org;


  // Wrap manually. Qt::TextWrapAnywhere behaves weird.
  if (wrap_lines){
    int max_possible_lineshifts = text.count(" ");

    QFontMetrics fm(font);
    QRectF rect = fm.boundingRect(0, 0, width, height, flags, QString(org).replace("\n", " "));
    
    int max_num_lineshifts = R_MIN(max_possible_lineshifts, height / ceil(rect.height()));
    
    for(int i = 1 ; i < max_num_lineshifts ; i++){
      ret = split_text_by_lineshift(org, i, max_possible_lineshifts);
      //printf(" Split %d: -%s-\n", i, ret.toUtf8().constData());
      if (can_fit(font, ret, flags, width, height))
        return ret;
    }

    org = ret;
  }
  
  int safety = 1000;

  do{
    if (n>=text.size()+1)
      break;
    
    if ((safety--)==0){
      printf("org: -%s-. ret: -%s-. n: %d\n", text.toUtf8().constData(), ret.toUtf8().constData(), n);
#if !defined(RELEASE)
      abort();
#endif
      break;
    }
    
    ret = QString(org).remove(text.size()-n,n) + "..";
    if (ret.size()==2)
      break;

    //printf("org: \"%s\". ret: \"%s\". n: %d\n", text.toUtf8().constData(), ret.toUtf8().constData(), n);

    n++;
    
  }while(false==can_fit(font, ret, flags, width, height));
  
  return ret;
}


void setFontValues(struct Tracker_Windows *tvisual){
  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;


#if !USE_OPENGL    
  editor->cursorbuffer_painter->setFont(editor->font);
  //editor->paintbuffer_painter->setFont(editor->font);
#endif
  
  GE_set_font(editor->font);

  const QFont &font=editor->font;

  QFontMetrics fm(font);

  double width3           = R_MAX(fm.width("D#6"), R_MAX(fm.width("MUL"), fm.width("STP")));
  tvisual->fontwidth      = (int)(width3/3.0) + 1;
  tvisual->org_fontheight = fm.height() - 1;
  tvisual->fontheight     = tvisual->org_fontheight;

  tvisual->must_redraw = true;
}

static void updateAllFonts(QObject *object, const QFont &old_font, QFont new_font){
  if(object != NULL){

    QWidget *widget = qobject_cast<QWidget*>(object);

    bool is_font_dialog = widget!=NULL && qobject_cast<QFontDialog*>(widget->window())!=NULL;
  
    if (!is_font_dialog){
      if (widget!=NULL && widget->font()==old_font){
        widget->setFont(new_font);
        widget->update();
      }
      
      const QList<QObject*> list = object->children();
      for (int i = 0; i < list.size(); ++i)
        updateAllFonts(list.at(i), old_font, new_font);
    }
  }
}

void GFX_SetSystemFont(QFont font){
  QFont old_font = QApplication::font();
  
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
    const QFont &font2=QApplication::font();
    QFontMetrics fm(font2);
    tvisual->systemfontheight=fm.height();
  }

  foreach (QWidget *widget, QApplication::allWidgets()) {
    updateAllFonts(widget, old_font, font);
    //widget->update();
  }

  remakeMixerStrips(-1);
  evalScheme("(minimize-lowertab)");
}

void GFX_SetSystemFont(const char *fontdescr){
  QFont font;
  font.fromString(fontdescr);
  GFX_SetSystemFont(font);
}

const char *GFX_GetSystemFont(void){
  return talloc_strdup(qApp->font().toString().toUtf8().constData());
}

void GFX_SetEditorFont(const char *fontdescr){
  struct Tracker_Windows *tvisual = root->song->tracker_windows;
  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;
  QFont font;
  font.fromString(fontdescr);
  editor->font = font;
  setFontValues(tvisual);
  UpdateAllWBlockWidths(tvisual);
  GL_create(tvisual);
}

const char *GFX_GetEditorFont(void){
  struct Tracker_Windows *tvisual = root->song->tracker_windows;
  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;
  return talloc_strdup(editor->font.toString().toUtf8().constData());
}

void GFX_ConfigSystemFont(void){
  obtain_keyboard_focus();
  QFont font = QFontDialog::getFont( 0, QApplication::font());
  release_keyboard_focus();

  GFX_SetSystemFont(font);
}


static char *GFX_SelectEditFont(struct Tracker_Windows *tvisual){
  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;
  obtain_keyboard_focus();
  editor->font = QFontDialog::getFont( 0, editor->font ) ;
  release_keyboard_focus();
  //editor->setFont(editor->font);

  printf("Raw font name: \"%s\"\n",editor->font.rawName().toUtf8().constData());

  setFontValues(tvisual);
  UpdateAllWBlockWidths(tvisual);
  
  //SETTINGS_write_string("font_style",editor->font.styleName().toUtf8().constData()); // toString doesn't seem to cover this. (arrgh, there's a billion bugs in qt when it comes to font styles)
  return talloc_strdup((char*)editor->font.toString().toUtf8().constData());
}

void GFX_ConfigFonts(struct Tracker_Windows *tvisual){
  char *font = GFX_SelectEditFont(tvisual);
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
  
#if USE_OPENGL
  editor->position_gl_widget(tvisual);
#endif
}

void GFX_SetDefaultFont(struct Tracker_Windows *tvisual){
  QFont font;

  SETTINGS_set_custom_configfile(OS_get_full_program_file_path("config"));
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
}

void GFX_SetDefaultSystemFont(struct Tracker_Windows *tvisual){
  QFont font;

  SETTINGS_set_custom_configfile(OS_get_full_program_file_path("config"));
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

  GFX_SetSystemFont(font);
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
  int len = (int)strlen(text);
  QString string(text);

  //printf("width: %d / %d / %d\n",fn.width(string,len), fn.width(string,len/2), max_width);

  if(does_text_fit(fn, string, len, max_width))
    return len;
  else
    return find_text_length(fn, string, max_width, 0, len-1);
}

#endif // USE_QT_VISUAL

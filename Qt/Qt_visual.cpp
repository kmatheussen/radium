/* Copyright 2003 Kjetil S. Matheussen

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

#include <stdbool.h>
#include <math.h>

#include "EditorWidget.h"

#include <qpainter.h>
#include <qapplication.h>
#include <qdesktopwidget.h>

#include "Qt_instruments_proc.h"
#include "Qt_colors_proc.h"

#if USE_QT_VISUAL
#  include "Qt_Fonts_proc.h" // For setFontValues, etc.
#endif

extern EditorWidget *g_editor;

int GFX_CreateVisual(struct Tracker_Windows *tvisual){
  QWidget *main_window = g_editor->main_window;

  tvisual->os_visual.main_window = main_window;
  tvisual->os_visual.widget      = g_editor;

  tvisual->width  = g_editor->get_editor_width();
  tvisual->height = g_editor->get_editor_height();
    
  g_editor->window = tvisual;

  setFontValues(tvisual);

  {
    const QFont &font=QApplication::font();
    QFontMetrics fm(font);
    tvisual->systemfontheight=fm.height();
  }


  return 0;
}

int GFX_ShutDownVisual(struct Tracker_Windows *tvisual){
  //close_all_instrument_widgets();
  return 0;
}

extern LANGSPEC void QT_UpdateEditor(struct Tracker_Windows *window);
void QT_UpdateEditor(struct Tracker_Windows *window){
  EditorWidget *editor=(EditorWidget *)window->os_visual.widget;
  editor->updateEditor();
}


static bool mouse_keyboard_disabled = false;

void GFX_disable_mouse_keyboard(void){
  mouse_keyboard_disabled = true;
}

void GFX_enable_mouse_keyboard(void){
  mouse_keyboard_disabled = false;
}

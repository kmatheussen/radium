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

#ifndef QT_COLORS_PROC_H
#define QT_COLORS_PROC_H

class EditorWidget;

extern QColor get_qcolor(struct Tracker_Windows *tvisual, int colornum);
extern QColor get_qcolor(int colornum);
extern void setWidgetColors(QWidget *widget);
extern void setApplicationColors(QApplication *app);
extern void setEditorColors(EditorWidget *editor);
extern void testColorInRealtime(int num, QColor color);

void GFX_SetBrightness(struct Tracker_Windows *tvisual, float how_much);

void GFX_ResetColors(void);
void GFX_SaveColors(void);

#endif

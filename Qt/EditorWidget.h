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

#ifndef EDITOR_WIDGET_H
#define EDITOR_WIDGET_H

#ifdef USE_QT3
#include <qmenubar.h>
#include <qmainwindow.h>
#include <qslider.h>
#include <qtimer.h>

#  if USE_GTK_VISUAL
#    include "qtxembed-1.3-free/src/qtxembed.h"
#  endif

#include <qapplication.h>
#include <qpixmap.h>
#include <qimage.h>
#include <qpointarray.h>
#include <qlabel.h>
#endif // USE_QT3

#ifdef USE_QT4
//Added by qt3to4:
#include <QWidget>
#include <QLabel>
#if !USE_OPENGL
#include <Q3PointArray>
#endif
#include <QPainter>
#include <QWheelEvent>
#ifdef FOR_WINDOWS
#  include <QKeyEvent>
#endif
#endif
#include <QDragEnterEvent>
#include <QDropEvent>

#ifdef USE_QT3
# define Q3PointArray QPointArray
# define Q3PopupMenu QPopupMenu
#endif

#define GFX_DONTSHRINK
#include "../common/nsmtracker.h"
#include "../common/windows_proc.h"

#include "../common/visual_proc.h"

#include "../OpenGL/Render_proc.h"
#include "../OpenGL/GfxElements.h"

// Don't paint on the frame.
//#define XOFFSET 5
//#define YOFFSET 2

#include "MySplitter.hpp"


class Upperleft_widget;

#if USE_QIMAGE_BUFFER
typedef QImage PaintBuffer;
#else
typedef QPixmap PaintBuffer;
#endif

#include "helpers.h"
#include "Qt_colors_proc.h"
#include "KeyboardFocusFrame.hpp"


struct EditorLayoutWidget : public radium::KeyboardFocusFrame{
 public:
  EditorLayoutWidget()
    : radium::KeyboardFocusFrame(NULL, radium::KeyboardFocusFrameType::EDITOR, true)
  {
    set_widget_takes_care_of_painting_everything(this);
  }
};


class EditorWidget
{

public:
  EditorWidget(QWidget *parent=0, const char *name="no name" );
  ~EditorWidget();

    struct Tracker_Windows *window; // Not sure if this one is used.

    QWidget *main_window;

    Upperleft_widget *upperleft_widget;

    QFont font;

    EditorLayoutWidget *editor_layout_widget = NULL; // parent widget. The one holding the vertical layout.

    QWidget *header_widget = NULL;
    QWidget *bottom_widget = NULL; // rec button, reltempo slider, and track scrollbar
  
#if USE_OPENGL
    QWidget *gl_widget = NULL; // Note: might be NULL
#endif

    //QFrame *status_frame;
    //QVector<QLabel*> status_labels;

    radium::Splitter *xsplitter;
    //QSplitter *ysplitter;

    int get_editor_width(){
      //return this->width()-XOFFSET-2; // Fine tuned. No logical reason behind it. (2 is probably just the frame border width)
      if(!this->header_widget)
        return 20;
      return this->header_widget->width();
    }

    int get_editor_height(){
      //return this->height()-YOFFSET-2; // Fine tuned. No logical reason behind it. (2 is probably just the frame border width)
      if(!this->header_widget)
        return 20;
      return this->header_widget->height();
    }
    
    void updateEditor(void) const;

    void wheelEvent(QWheelEvent *qwheelevent); // override;

 public:

    void handle_mouse_press(Qt::MouseButton button, float x, float y) const;
    void handle_mouse_move(Qt::MouseButton button, float x, float y) const;
    void handle_mouse_release(Qt::MouseButton button, float x, float y) const;

};

extern QWidget *g_main_window;
extern QWidget *g_mixerstripparent;
extern QHBoxLayout *g_mixerstriplayout;
extern EditorWidget *g_editor;


static inline void calculateNewWindowWidthAndHeight(struct Tracker_Windows *window){
  
  if (g_editor==NULL || g_editor->gl_widget==NULL || g_editor->editor_layout_widget==NULL)
    return;

  window->width = g_editor->editor_layout_widget->width();
  window->height = g_editor->header_widget->height() + g_editor->gl_widget->height() + g_editor->window->bottomslider_height; //g_editor->get_editor_height();


  if(g_is_starting_up==true)
    return;

  if (window->wblock==NULL){
    R_ASSERT_NON_RELEASE(false);
    return;
  }
    
  UpdateWBlockCoordinates(window, window->wblock);

  if (g_editor->header_widget->height() != window->wblock->t.y1){
    g_editor->header_widget->setMinimumHeight(window->wblock->t.y1);
    g_editor->header_widget->setMaximumHeight(window->wblock->t.y1);
  }

  /*
    // We call GE_set_height in the VisualizationLibrary resize callback instead.
  {
    int height_ = 1 + window->wblock->t.y2 - window->wblock->t.y1 -1;
    //fprintf(stderr,"GAKK! height: %d, width: %d\n",height_,window->width);
    GE_set_height(height_);
    
    g_editor->window->must_redraw = true;
    g_editor->updateEditor();      
  }
  */
}

#endif


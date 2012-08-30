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

#include "EditorWidget.h"

#include <qpainter.h>
#include <qmainwindow.h>

#include "Qt_instruments_proc.h"

#if USE_QT_VISUAL
#  include "Qt_Fonts_proc.h" // For setFontValues, etc.
#endif

#if USE_GTK_VISUAL
#  include "../GTK/GTK_visual_proc.h" // For setFontValues
#endif

#if 0
#if USE_GTK_VISUAL
#  include "qtxembed-1.3-free/src/qtxembed.h"
#  include "GTK_visual_proc.h"
QtXEmbedContainer *g_embed_container;
#endif
#endif

#include "../common/visual_op_queue_proc.h"

extern EditorWidget *g_editor;


//#include <qpalette.h>
int GFX_CreateVisual(struct Tracker_Windows *tvisual){
  QMainWindow *main_window = g_editor->main_window;

  tvisual->os_visual.main_window = main_window;
  tvisual->os_visual.widget      = g_editor;

  tvisual->width  = g_editor->get_editor_width();
  tvisual->height = g_editor->get_editor_height();
    
  g_editor->window = tvisual;

  setFontValues(tvisual);


#if 0
#if USE_GTK_VISUAL
  if(sizeof(socket_type_t) < sizeof(WId))
    abort();

  if(g_embed_container==NULL){
    g_embed_container = new QtXEmbedContainer(g_editor);

    g_embed_container->setSizePolicy(QSizePolicy::Maximum,QSizePolicy::Maximum);
    g_embed_container->show();

    GTK_CreateVisual(g_embed_container->winId());
    //g_embed_container->embed(GTK_CreateVisual(g_embed_container->winId()),false);
    //g_embed_container->grabKeyboard();

    //g_embed_container->moveInputToProxy();

    int width=600;
    int height=600;
    g_embed_container->resize(width,height);
    GTK_SetSize(width,height);
  }
#endif
#endif

  return 0;
}

int GFX_ShutDownVisual(struct Tracker_Windows *tvisual){
  close_all_instrument_widgets();
  return 0;
}

extern LANGSPEC void QT_UpdateEditor(struct Tracker_Windows *window);
void QT_UpdateEditor(struct Tracker_Windows *window){
  EditorWidget *editor=(EditorWidget *)window->os_visual.widget;
  editor->update();
}

extern LANGSPEC void QT_RepaintEditor(struct Tracker_Windows *window);
void QT_RepaintEditor(struct Tracker_Windows *window){
  EditorWidget *editor=(EditorWidget *)window->os_visual.widget;
  editor->repaint();
}




#if USE_QT_VISUAL


#ifdef USE_QT4
#  define DRAW_PIXMAP_ON_WIDGET(dst_widget, x, y, src_pixmap, from_x, from_y, width, height) \
     dst_widget->painter->drawPixmap(x,y,*(src_pixmap),from_x,from_y,width,height)
#  define DRAW_PIXMAP_ON_PIXMAP(dst_pixmap, x, y, src_pixmap, from_x, from_y, width, height) \
     dst_pixmap##_painter->drawPixmap(x,y,*(src_pixmap),from_x,from_y,width,height)
#endif

#if USE_QT3
#  define DRAW_PIXMAP_ON_WIDGET(dst_widget, x, y, src_pixmap, from_x, from_y, width, height) \
     bitBlt(dst_widget,x+XOFFSET,y+YOFFSET,src_pixmap,from_x,from_y,width,height)
#  define DRAW_PIXMAP_ON_PIXMAP(dst_pixmap, x, y, src_pixmap, from_x, from_y, width, height) \
     bitBlt(dst_pixmap,x,y,src_pixmap,from_x,from_y,width,height)
#endif

void OS_GFX_C2V_bitBlt(
                       struct Tracker_Windows *window,
		    int from_x1,int from_x2,
		    int to_y
		    ){
  EditorWidget *editor=(EditorWidget *)window->os_visual.widget;

  DRAW_PIXMAP_ON_WIDGET(editor,
                        from_x1,to_y,
                        editor->cursorpixmap,
                        from_x1,0,
                        from_x2-from_x1+1,window->fontheight
	 );
}


/* window,x1,x2,x3,x4,height, y pixmap */
void OS_GFX_C_DrawCursor(
				      struct Tracker_Windows *window,
				      int x1,int x2,int x3,int x4,int height,
				      int y_pixmap
				      ){
  EditorWidget *editor=(EditorWidget *)window->os_visual.widget;

#ifdef USE_QT4
#if 0
  DRAW_PIXMAP_ON_PIXMAP(editor->cursorpixmap,
                        x1+1,1,
                        editor->qpixmap,
                        x1+1,y_pixmap+1,
                        x2-x1-2,height-2);

  DRAW_PIXMAP_ON_PIXMAP(editor->cursorpixmap,
                        x2+1,1,
                        editor->qpixmap,
                        x2+1,y_pixmap+1,
                        x3-x2-2,height-2);

  DRAW_PIXMAP_ON_PIXMAP(editor->cursorpixmap,
                        x3+1,1,
                        editor->qpixmap,
                        x3+1,y_pixmap+1,
                        x4-x3-2,height-2);
#endif

  DRAW_PIXMAP_ON_PIXMAP(editor->cursorpixmap,
                        x1+1,1,
                        editor->qpixmap,
                        x1+1,y_pixmap+1,
                        x4-x1-2,height-2);

  editor->cursorpixmap_painter->setPen(editor->colors[1]);
  editor->cursorpixmap_painter->drawRect(x1+1,0,
                                         x4-x1-2,height-1);

  editor->cursorpixmap_painter->setPen(editor->colors[1]);
  editor->cursorpixmap_painter->drawRect(x2+3,1,
                                         x3-x2-6,height-3);
  editor->cursorpixmap_painter->drawLine(x2+2,1,
                                         x2+2,height-1);
  editor->cursorpixmap_painter->drawLine(x3-4,1,
                                         x3-4,height-1);
#endif

  //editor->cursorpixmap_painter->setCompositionMode(QPainter::CompositionMode_Xor);

  // TODO: fix Qt4
#ifdef USE_QT3
  editor->cursorpixmap_painter->fillRect(x1,0,x4,height,editor->colors[7]);
  editor->cursorpixmap_painter->fillRect(x2,0,x3-x2+1,height,editor->colors[1]);

  bitBlt(
         editor->cursorpixmap,
         0,0,
         editor->qpixmap,
         0,y_pixmap,
         x4+1,height
         ,Qt::XorROP
         );
#endif
}


void OS_GFX_P2V_bitBlt(
		    struct Tracker_Windows *window,
		    int from_x,int from_y,
		    int to_x,int to_y,
		    int width,int height
		    ){
  
  EditorWidget *editor=(EditorWidget *)window->os_visual.widget;

  DRAW_PIXMAP_ON_WIDGET(
                        editor,
                        to_x,to_y,
                        editor->qpixmap,
                        from_x,from_y,
                        width,height
                        );

  /*
  OS_GFX_C2V_bitBlt(
		 window,
		 from_x,width,
		 (window->wblock->curr_realline-window->wblock->top_realline)*window->fontheight+window->wblock->t.y1
		 );
  */
}


void OS_GFX_BitBlt(
	struct Tracker_Windows *tvisual,
	int dx,int dy,
	int x,int y,
	int x2,int y2
){
  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;
  //printf("pixmap on pixmap. dx: %d. dy: %d\n",dx,dy);
#ifdef USE_QT3
  DRAW_PIXMAP_ON_PIXMAP(
                        editor->qpixmap,
                        x+dx,y+dy,
                        editor->qpixmap,
                        x,y,x2-x+1,y2-y+1
                        );
#endif

#ifdef USE_QT4
#  ifdef FOR_MACOSX
     // drawPixmap into itself works on mac, probably since all graphic is buffered.
     editor->qpixmap_painter->drawPixmap(x+dx,y+dy,
                                         *editor->qpixmap,
                                         x,y,
                                         x2-x+1,y2-y+1);
#  else
     // scroll doesn't work on MACOSX. (that's weird)
     if(dx<0){
       editor->qpixmap->scroll(dx,dy,
         x+dx,y+dy,
         x2-x+1, y2-y+1);
     }else{
       editor->qpixmap->scroll(dx,dy,
         x,y,
         x2-x+1, y2-y+1);
     }
#  endif
#endif
}


#define GET_QPAINTER(editor,where) (where==PAINT_DIRECTLY ? editor->painter : editor->qpixmap_painter)

void OS_GFX_FilledBox(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2,int where){
  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;
  QPainter *painter=GET_QPAINTER(editor,where);

  painter->fillRect(x,y,x2-x+1,y2-y+1,editor->colors[color]);
}

void OS_GFX_Box(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2,int where){
  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;
  QPainter *painter=GET_QPAINTER(editor,where);

  painter->setPen(editor->colors[color]);
#if USE_QT4
  painter->drawRect(x,y,x2-x,y2-y);
#endif
#if USE_QT3
  painter->drawRect(x,y,x2-x+1,y2-y+1);
#endif
}


void OS_GFX_Line(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2,int where){
  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;
  QPainter *painter=GET_QPAINTER(editor,where);

  painter->setPen(editor->colors[color]);

#if 0
  QPen pen(editor->colors[color],4,Qt::SolidLine);  
  pen.setCapStyle(Qt::RoundCap);
  pen.setJoinStyle(Qt::RoundJoin);
  painter->setPen(pen);
#endif

  painter->drawLine(x,y,x2,y2);
  //  printf("drawline, x: %d, y: %d, x2: %d, y2: %d\n",x,y,x2,y2);
}


static QColor mix_colors(const QColor &c1, const QColor &c2, float how_much){

  float a1 = how_much;
  float a2 = 1.0f-a1;

  if(c1.red()==0 && c1.green()==0 && c1.blue()==0){ // some of the black lines doesn't look look very good.
    int r = 74*a1 + c2.red()*a2;
    int g = 74*a1 + c2.green()*a2;
    int b = 74*a1 + c2.blue()*a2;

    return QColor(r,g,b);
  }else{

    int r = c1.red()*a1 + c2.red()*a2;
    int g = c1.green()*a1 + c2.green()*a2;
    int b = c1.blue()*a1 + c2.blue()*a2;

    return QColor(r,g,b);
  }
}


void OS_GFX_Point(
	struct Tracker_Windows *tvisual,
	int color,
        int brightness,
	int x,int y,
        int where
	)
{
  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;
  QPainter *painter=GET_QPAINTER(editor,where);

  if(brightness==MAX_BRIGHTNESS && color!=1){
    painter->setPen(editor->colors[color]);
  }else{
    painter->setPen(mix_colors(editor->colors[color], editor->colors[0], brightness/(float)MAX_BRIGHTNESS));
  }

  painter->drawPoint(x,y);
}


void OS_GFX_Points(
                   struct Tracker_Windows *tvisual,
                   int color,
                   int brightness,
                   int num_points,
                   uint16_t *x,uint16_t *y,
                   int where
                   )
{
  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;
  QPainter *painter=GET_QPAINTER(editor,where);

  if(brightness==MAX_BRIGHTNESS && color!=1)
    painter->setPen(editor->colors[color]);
  else
    painter->setPen(mix_colors(editor->colors[color], editor->colors[0], brightness/(float)MAX_BRIGHTNESS));

  while((int)editor->qpa.size() <= num_points)
    editor->qpa.resize(editor->qpa.size()*2);
  
  for(int i=0;i<num_points;i++)
    editor->qpa.setPoint(i,x[i],y[i]);

  painter->drawPoints(editor->qpa,0,num_points);
}


void OS_GFX_SetClipRect(
                        struct Tracker_Windows *tvisual,
                        int x,int y,
                        int x2,int y2,
                        int where
                        )
{
  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;
  QPainter *painter=GET_QPAINTER(editor,where);

  painter->setClipRect(x,y,x2-x,y2-y);
  painter->setClipping(true);
}


void OS_GFX_CancelClipRect(struct Tracker_Windows *tvisual,int where){
  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;
  QPainter *painter=GET_QPAINTER(editor,where);

  painter->setClipping(false);
}


void OS_GFX_Text(
                 struct Tracker_Windows *tvisual,
                 int color,
                 char *text,
                 int x,
                 int y,
                 int width,
                 int flags,
                 int where
){
  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;
  QPainter *painter=GET_QPAINTER(editor,where);

  painter->setPen(editor->colors[color]);

#if 0    
  if(flags & TEXT_BOLD){
    editor->font.setBold(true);
    painter->setFont(editor->font);
  }
#endif

  {  
    if(flags & TEXT_CENTER){
      QRect rect(x,y,tvisual->fontwidth*strlen(text),tvisual->org_fontheight);
      painter->drawText(rect, Qt::AlignVCenter, text);
    }else{
      painter->drawText(x,y+tvisual->org_fontheight-1,text);
    }
  }

#if 0
  if(flags & TEXT_BOLD){
    editor->font.setBold(false);
    painter->setFont(editor->font);
  }
#endif
}                      

static bool mouse_keyboard_disabled = false;

void GFX_disable_mouse_keyboard(void){
  mouse_keyboard_disabled = true;
}

void GFX_enable_mouse_keyboard(void){
  mouse_keyboard_disabled = false;
}



#endif // USE_QT_VISUAL



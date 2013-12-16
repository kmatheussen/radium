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
#include <qapplication.h>
#include <qdesktopwidget.h>
#include <qrawfont.h>
#include <qhash.h>

#include "Qt_instruments_proc.h"
#include "Qt_colors_proc.h"

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

static bool g_use_custom_color = false;
static QColor g_custom_color;

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

#if USE_QIMAGE_BUFFER
  editor->painter->drawImage(from_x1,to_y,
                             *editor->cursorbuffer,
                             from_x1,0,
                             from_x2-from_x1+1,window->fontheight);
#else
  DRAW_PIXMAP_ON_WIDGET(editor,
                        from_x1,to_y,
                        editor->cursorbuffer,
                        from_x1,0,
                        from_x2-from_x1+1,window->fontheight
	 );
#endif
}

#include "../common/playerclass.h"
extern PlayerClass *pc;

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
                        editor->paintbuffer,
                        x1+1,y_pixmap+1,
                        x2-x1-2,height-2);

  DRAW_PIXMAP_ON_PIXMAP(editor->cursorpixmap,
                        x2+1,1,
                        editor->paintbuffer,
                        x2+1,y_pixmap+1,
                        x3-x2-2,height-2);

  DRAW_PIXMAP_ON_PIXMAP(editor->cursorpixmap,
                        x3+1,1,
                        editor->paintbuffer,
                        x3+1,y_pixmap+1,
                        x4-x3-2,height-2);
#endif

#if USE_QIMAGE_BUFFER
  if(pc->isplaying)
    editor->cursorbuffer_painter->setOpacity(0.55);
  else
    editor->cursorbuffer_painter->setOpacity(1.0);

  editor->cursorbuffer_painter->drawImage(x1+1,1,
                                          *editor->linesbuffer[0],
                                          x1+1,y_pixmap+1,
                                          x4-x1-2,height-2);

  editor->cursorbuffer_painter->setOpacity(0.2);
  editor->cursorbuffer_painter->fillRect(x1,0,x4,height,editor->colors[7]);

#else
  DRAW_PIXMAP_ON_PIXMAP(editor->cursorbuffer,
                        x1+1,1,
                        editor->linesbuffer,
                        x1+1,y_pixmap+1,
                        x4-x1-2,height-2);
#endif

  editor->cursorbuffer_painter->setOpacity(0.75);
  editor->cursorbuffer_painter->setPen(editor->colors[1]);
  editor->cursorbuffer_painter->drawRect(x1+1,0,
                                         x4-x1-2,height-1);

  editor->cursorbuffer_painter->drawRect(x2+3,1,
                                         x3-x2-6,height-3);
  editor->cursorbuffer_painter->drawLine(x2+2,1,
                                         x2+2,height-1);
  editor->cursorbuffer_painter->drawLine(x3-4,1,
                                         x3-4,height-1);
#endif

  //editor->cursorpixmap_painter->setCompositionMode(QPainter::CompositionMode_Xor);

  // TODO: fix Qt4
#ifdef USE_QT3
  editor->cursorbuffer_painter->fillRect(x1,0,x4,height,editor->colors[7]);
  editor->cursorbuffer_painter->fillRect(x2,0,x3-x2+1,height,editor->colors[1]);

  bitBlt(
         editor->cursorbuffer,
         0,0,
         editor->paintbuffer,
         0,y_pixmap,
         x4+1,height
         ,Qt::XorROP
         );
#endif
}


extern double g_curr_subrealline;

void OS_GFX_P2V_bitBlt_from_lines(
		    struct Tracker_Windows *window,
		    int from_x,int from_y,
		    int to_x,int to_y,
		    int width,int height
		    ){
  
  EditorWidget *editor=(EditorWidget *)window->os_visual.widget;

#if !USE_QIMAGE_BUFFER
  DRAW_PIXMAP_ON_WIDGET(
                        editor,
                        to_x,to_y,
                        editor->paintbuffer,
                        from_x,from_y,
                        width,height
                        );
#else
  int subline = (int)g_curr_subrealline;
  double subpixel = g_curr_subrealline - (double)subline;
  int buffernum = (int)scale(subpixel,0,1,0,NUM_LINESBUFFERS);
  if(buffernum==NUM_LINESBUFFERS)
    buffernum--;
  printf("buffernum: %d, subpixel: %f\n",buffernum,subpixel);
  editor->painter->drawImage(QRectF(to_x,to_y-subline,width,height),
                             *editor->linesbuffer[buffernum],
                             QRectF(from_x,from_y,width,height)
                             );

  //editor->painter->drawImage(to_x,to_y,*editor->linesbuffer,from_x,from_y,width,height);

#endif
  /*
  OS_GFX_C2V_bitBlt(
		 window,
		 from_x,width,
		 (window->wblock->curr_realline-window->wblock->top_realline)*window->fontheight+window->wblock->t.y1
		 );
  */
}


void OS_GFX_P2V_bitBlt(
		    struct Tracker_Windows *window,
		    int from_x,int from_y,
		    int to_x,int to_y,
		    int width,int height
		    ){
  
  EditorWidget *editor=(EditorWidget *)window->os_visual.widget;

#if !USE_QIMAGE_BUFFER
  DRAW_PIXMAP_ON_WIDGET(
                        editor,
                        to_x,to_y,
                        editor->paintbuffer,
                        from_x,from_y,
                        width,height
                        );
#else

  editor->painter->drawImage(to_x,to_y,*editor->paintbuffer,from_x,from_y,width,height);

#endif
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
                        editor->paintbuffer,
                        x+dx,y+dy,
                        editor->paintbuffer,
                        x,y,x2-x+1,y2-y+1
                        );
#endif

#ifdef USE_QT4

#  if USE_QIMAGE_BUFFER

  QImage copy = editor->paintbuffer->copy(x,y,x2-x+1,y2-y+1);
  editor->paintbuffer_painter->drawImage(x+dx,y+dy,copy);

#  else // USE_QIMAGE_BUFFER

#    ifdef FOR_MACOSX
     // drawPixmap into itself works on mac, probably since all graphic is buffered.

     editor->paintbuffer_painter->drawPixmap(x+dx,y+dy,
                                         *editor->paintbuffer,
                                         x,y,
                                         x2-x+1,y2-y+1);
#    else // FOR_MACOSX
     // scroll doesn't work on MACOSX. (that's weird)

     if(dx<0){
       editor->paintbuffer->scroll(dx,dy,
         x+dx,y+dy,
         x2-x+1, y2-y+1);
     }else{
       editor->paintbuffer->scroll(dx,dy,
         x,y,
         x2-x+1, y2-y+1);
     }
#    endif // FOR_MACOSX
#  endif  // USE_QIMAGE_BUFFER
#endif // USE_QT4
}


#define GET_QPAINTER(editor,where) (where==PAINT_DIRECTLY ? editor->painter : where==PAINT_BUFFER ? editor->paintbuffer_painter : editor->linesbuffer_painter[0])

#if 0
static void setColor(QPainter *painter, int colornum, int where){
}
#endif

static QColor get_note_color(EditorWidget *editor, QColor base){
  return mix_colors(base,editor->colors[15],0.4);
}

/*
setPen
setBrush
setClipRect
setClipping
setFont
setRenderHints
save
scale
restore

drawRect
fillRect
drawLine
drawPoint
drawPoints
drawPolygon
drawPolyline
drawText
*/

void OS_GFX_FilledBox(struct Tracker_Windows *tvisual,int colornum,int x,int y,int x2,int y2,int where){
  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;
  QPainter *painter=GET_QPAINTER(editor,where);

  if(where==PAINT_LINES && colornum==0){
    if(y>=tvisual->wblock->t.y1){
      colornum = 15;

      QColor qcolor = get_qcolor(tvisual,colornum);

      QLinearGradient gradient(0,0,QApplication::desktop()->width(), QApplication::desktop()->height()); //editor->get_editor_width(),editor->get_editor_height());
      gradient.setStart(0,0);
      gradient.setFinalStop(QApplication::desktop()->width(),0);
      gradient.setColorAt(0,qcolor.darker(100));
      gradient.setColorAt(1,qcolor.darker(112));
#if 0
      gradient.setColorAt(0,qcolor.darker(90));
      gradient.setColorAt(0.5,qcolor.darker(112));
      //gradient.setColorAt(0.5,mix_colors(qcolor,editor->colors[7],0.9));
      gradient.setColorAt(1,qcolor.darker(90));
      //gradient.setColorAt(1,editor->colors[7]);
#endif 
      for(int i=0;i<NUM_LINESBUFFERS;i++) {
        QPainter *painter=editor->linesbuffer_painter[i];
        painter->setPen(Qt::NoPen);
        painter->setBrush(gradient);
      
        painter->drawRect(x,y,x2-x+1,y2-y+1);
      
        painter->setBrush(QBrush());
      }
      return;
    }
  }
#if 0
    else{
      struct WBlocks *wblock = tvisual->wblock;
      struct WTracks *wtrack = wblock->wtracks;
      while(wtrack!=NULL){
        if(x < wtrack->x2)
          break;
        wtrack = NextWTrack(wtrack);
      }
      if(wtrack==NULL)
        colornum=15;
    }
#endif

  QColor qcolor = g_use_custom_color==true ? g_custom_color : get_qcolor(tvisual,colornum);
  if(g_use_custom_color==true)
    qcolor = get_note_color(editor,qcolor);
  //qcolor.setAlpha(100);
  g_use_custom_color = false;
  painter->fillRect(x,y,x2-x+1,y2-y+1,qcolor);
}

void OS_GFX_Box(struct Tracker_Windows *tvisual,int colornum,int x,int y,int x2,int y2,int where){
  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;
  QPainter *painter=GET_QPAINTER(editor,where);

  QColor qcolor = g_use_custom_color==true ? g_custom_color : get_qcolor(tvisual,colornum);
  g_use_custom_color = false;

  painter->setPen(qcolor);
#if USE_QT4
  //painter->setRenderHints(QPainter::Antialiasing,true);
  painter->drawRect(x,y,x2-x,y2-y);
  //painter->setRenderHints(QPainter::Antialiasing,false);
#endif
#if USE_QT3
  painter->drawRect(x,y,x2-x+1,y2-y+1);
#endif
}
void OS_GFX_Line(struct Tracker_Windows *tvisual,int colornum,int x,int y,int x2,int y2,int where){
  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;
  QPainter *painter=GET_QPAINTER(editor,where);

  QColor qcolor = g_use_custom_color==true ? g_custom_color : get_qcolor(tvisual,colornum);
  g_use_custom_color = false;

  if(x!=x2 && y!=y2){
    painter->setRenderHints(QPainter::Antialiasing,true);
    qcolor.setAlpha(100);
    QPen pen(qcolor,2,Qt::SolidLine);  
    pen.setCapStyle(Qt::RoundCap);
    //pen.setCapStyle(Qt::SquareCap);
    pen.setJoinStyle(Qt::RoundJoin);
    //pen.setJoinStyle(Qt::BevelJoin);
    painter->setPen(pen);    
#if 0
    // if thicker lines than 2:
    y2-=1;
    if(x2>x)
      x2-=1;
    else
      x-=1;
#endif
  } else {
    //painter->setPen(qcolor);

    QLinearGradient gradient(x,y,x2,y); //0,0,QApplication::desktop()->width(), QApplication::desktop()->height()); //editor->get_editor_width(),editor->get_editor_height());
    //gradient.setStart(x,y);
    //gradient.setFinalStop(x2,y);//QApplication::desktop()->width(),0);

    if(x!=tvisual->wblock->tempocolorarea.x){
      gradient.setColorAt(0,qcolor.darker(90));
      gradient.setColorAt(1,qcolor.darker(110));
    }else{
      gradient.setColorAt(0,qcolor);
      //gradient.setColorAt(0.5,qcolor.darker(95));
      gradient.setColorAt(1,editor->colors[15].darker(scale(tvisual->wblock->tempocolorarea.x2,0,QApplication::desktop()->width(),100,112)));
    }

    QPen pen;
    pen.setBrush(gradient);
    //painter->setBrush(gradient);
    painter->setPen(pen);//qcolor);
  }

  painter->drawLine(x,y,x2,y2);
  //  printf("drawline, x: %d, y: %d, x2: %d, y2: %d\n",x,y,x2,y2);

  if(x!=x2 && y!=y2)
    painter->setRenderHints(QPainter::Antialiasing,false);
  else
    painter->setBrush(QBrush());
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
    painter->setPen(mix_colors(editor->colors[color], editor->colors[15], brightness/(float)MAX_BRIGHTNESS));
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

void OS_GFX_Polygon(
                    struct Tracker_Windows *tvisual,
                    int color,
                    int x1, int y1, int x2, int y2,
                    int num_points,
                    struct APoint *peaks,
                    int where
                    )
{
  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;
  QPainter *painter=GET_QPAINTER(editor,where);


  int width=x2-x1;
  int height=1+y2-y1;

  QPolygon polygon(num_points);
  for(int i=0;i<num_points;i++){
    polygon.setPoint(i,x1 + (int)(peaks[i].x*width), y1 + (int)(peaks[i].y*height));
    //printf("point %d set to %d/%d\n",i,x1 + (int)(peaks[i].x*width), y1 + (int)(peaks[i].y*height));
  }

  QColor col = editor->colors[color];

  if(g_use_custom_color==true){
    col = g_custom_color;
    g_use_custom_color = false;
  }

//  if(g_use_custom_color==true)
  col = get_note_color(editor,col);

  if(color!=0){ // gradient, looks cool, but is a bit messy
    //printf("--polygon called\n");
    QLinearGradient gradient(x1,y1,x2,y2);
#if 1
    gradient.setColorAt(0,col.darker(100));
    gradient.setColorAt(1,col.darker(110));
#else
    gradient.setColorAt(0,col);
    gradient.setColorAt(1,editor->colors[10]);
#endif
    
    painter->setPen(Qt::NoPen);
    painter->setBrush(gradient);
    
  }else{

    painter->setPen(col);
    painter->setBrush(QBrush(col,Qt::SolidPattern));
    
  }

  painter->drawPolygon(polygon);
  painter->setBrush(QBrush());
}
                    

void OS_GFX_Polyline(
                    struct Tracker_Windows *tvisual,
                    int colornum,
                    int x1, int y1, int x2, int y2,
                    int num_points,
                    struct APoint *peaks,
                    int where
                    )
{
  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;
  QPainter *painter=GET_QPAINTER(editor,where);

  QColor color = g_use_custom_color==true ? g_custom_color : get_qcolor(tvisual,colornum);
  g_use_custom_color = false;

  color.setAlpha(160);

  QPen pen(color);
  pen.setWidth(2);
  painter->setPen(pen);

  int width=x2-x1;
  int height=y2-y1;

  QPolygon polygon(num_points);
  for(int i=0;i<num_points;i++){
    polygon.setPoint(i,x1 + (int)(peaks[i].x*width), y1 + (int)(peaks[i].y*height));
  }

  painter->setRenderHint(QPainter::Antialiasing, true);
  painter->drawPolyline(polygon);
  painter->setRenderHint(QPainter::Antialiasing, false);
}
                    

void OS_GFX_SetMixColor(struct Tracker_Windows *tvisual,int color1,int color2, int mix_factor){
  //EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;
  //printf("mix_factor: %d\n",mix_factor);
  g_custom_color = mix_colors(get_qcolor(tvisual,color1), get_qcolor(tvisual,color2), mix_factor / 1000.0);
  g_use_custom_color = true;
  //printf("mixcolor called\n");
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


namespace{
  struct GlyphpathAndWidth{
    QPainterPath path;
    float width;
    GlyphpathAndWidth(QPainterPath _path, float _width)
      : path(_path)
      , width(_width)
    {}
    GlyphpathAndWidth()
      : width(0.0f)
    {}
  };
}

static GlyphpathAndWidth getGlyphpathAndWidth(const QFont &font, const QChar c){
  static QFont cacheFont;
  static QRawFont rawFont = QRawFont::fromFont(font);
  static QHash<QChar,GlyphpathAndWidth> glyphpathCache;
  static QFontMetricsF fn(font);

  if (font != cacheFont){
    glyphpathCache.clear();
    cacheFont = font;
    rawFont = QRawFont::fromFont(font);
    fn = QFontMetrics(font);
  }

  if (glyphpathCache.contains(c))
    return glyphpathCache[c];


  QVector<quint32> indexes = rawFont.glyphIndexesForString(c);

  GlyphpathAndWidth g(rawFont.pathForGlyph(indexes[0]), fn.width(c));
  
  glyphpathCache[c] = g;

  return g;
}

static void myDrawText(QPainter *painter, QPointF p, QString text){
  QRawFont rawFont = QRawFont::fromFont(painter->font());
  QVector<quint32> indexes = rawFont.glyphIndexesForString(text);

  float x = p.x();

  QBrush brush = painter->pen().brush();

  //printf("num indexes: %d\n",indexes.count());
  //QPainterPath path = rawFont.pathForGlyph(indexes[0]);
  for(int i=0; i<indexes.count(); i++){
    GlyphpathAndWidth g = getGlyphpathAndWidth(painter->font(), text.at(i));

    painter->save();
    painter->translate(QPointF(x, p.y()));
    painter->fillPath(g.path,brush);
    painter->restore();

    x += g.width;
  }
}


void OS_GFX_Text(
                 struct Tracker_Windows *tvisual,
                 int color,
                 const char *text,
                 int x,
                 int y,
                 int width,
                 int flags,
                 int where
){
  EditorWidget *editor=(EditorWidget *)tvisual->os_visual.widget;
  QPainter *painter=GET_QPAINTER(editor,where);

  QColor qcolor = get_qcolor(tvisual,color);
  qcolor.setAlpha(200);
  painter->setPen(qcolor);

#if 0    
  if(flags & TEXT_BOLD){
    editor->font.setBold(true);
    painter->setFont(editor->font);
  }
#endif

  {  
    if(flags & TEXT_SCALE){

      QRect rect(x,y,tvisual->fontwidth*strlen(text),tvisual->org_fontheight);

      const QFontMetrics fn = QFontMetrics(painter->font());
      float text_width = fn.width(text);

      //printf("Got TEXT_SCALE. Text: \"%s\". text_width: %f. box_width: %f\n",text, text_width, (float)width);

      if(text_width>=width){
        float s = width/text_width;
        //float new_fontheight = tvisual->org_fontheight*s;

        //scale(y+tvisual->org_fontheight-1, y, y+tvisual->org_fontheight-1, y, y+new_fontheight);

        painter->save();
        painter->scale(s,1.0);
        painter->drawText(x/s, (y+tvisual->org_fontheight-1), text);
        painter->restore();
      }else{

        painter->drawText(x,y+tvisual->org_fontheight-1,text);
        //painter->drawText(rect, Qt::AlignVCenter, text);
      }

    }else if(flags & TEXT_CENTER){

      if (where==PAINT_LINES) {
        for(int i=0;i<NUM_LINESBUFFERS;i++) {
          float add_y=scale(i, 0,NUM_LINESBUFFERS, 1.0,0.0);
          QRectF rect((float)x,(float)y+add_y,tvisual->fontwidth*strlen(text),tvisual->org_fontheight);
          //printf("drawing at %f, %f\n",(float)x,(float)y+add_y);
          editor->linesbuffer_painter[i]->setPen(qcolor);
          editor->linesbuffer_painter[i]->setRenderHints(QPainter::Antialiasing,true);
          myDrawText(editor->linesbuffer_painter[i], QPointF(x, y+1.0-add_y), text);
          //painter->setRenderHints(QPainter::Antialiasing,false);
        }
      } else {
        QRect rect(x,y,tvisual->fontwidth*strlen(text),tvisual->org_fontheight);
        painter->drawText(rect, Qt::AlignVCenter, text);
      }
    }else{

      painter->drawText(x,y+tvisual->org_fontheight-1,text);

      //painter->drawText(x,y+tvisual->org_fontheight-1,text);

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



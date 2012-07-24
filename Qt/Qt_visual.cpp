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

#include "MyWidget.h"
#include "Qt_Bs_edit_proc.h"
#include "Qt_instruments_proc.h"
#include "Qt_colors_proc.h"
#include "Qt_Menues_proc.h"
#include "Qt_Fonts_proc.h"

#include <qpainter.h>
#include <qstatusbar.h>
#include <qmainwindow.h>


#ifdef USE_QT4
//#include <QMainWindow>
#  include <Q3PointArray>
#  include <QPixmap>
#endif

#include <unistd.h>

#include <X11/Xlib.h>
#include "../X11/X11_keyboard_proc.h"

//#include "../X11/X11_Bs_edit_proc.h"
//#include "../X11/X11_MidiProperties_proc.h"

#include "../X11/X11_Qtstuff_proc.h"
#include "../X11/X11_ReqType_proc.h"

#include "../common/gfx_proc.h"
#include "../common/gfx_op_queue_proc.h"
#include "../common/settings_proc.h"

#include "../common/visual_op_queue_proc.h"


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


MyWidget::MyWidget( struct Tracker_Windows *window,QWidget *parent, const char *name )
  : QFrame( parent, name, Qt::WStaticContents | Qt::WResizeNoErase | Qt::WRepaintNoErase | Qt::WNoAutoErase )
  , qpa(256)
{
  this->qpixmap=NULL;
  this->window=window;

  setEditorColors(this);

  this->setMouseTracking(true);

  //this->setFrameStyle(QFrame::Raised );
  this->setFrameStyle(QFrame::Sunken );
  this->setFrameShape(QFrame::Panel);
  this->setLineWidth(2);
}

MyWidget::~MyWidget()
{
}


//#include <qpalette.h>
int GFX_CreateVisual(struct Tracker_Windows *tvisual){
  static QMainWindow *g_main_window = NULL;;
  static MyWidget *g_mywidget = NULL;

  if(g_main_window!=NULL){
    tvisual->os_visual.main_window = g_main_window;
    tvisual->os_visual.widget = g_mywidget;

    tvisual->width=g_mywidget->get_editor_width();
    tvisual->height=g_mywidget->get_editor_height();
    
    g_mywidget->window = tvisual;

    setFontValues(tvisual, g_mywidget->font);

    return 0;
  }


  //QMainWindow *main_window = new QMainWindow(NULL, "Radium", Qt::WStyle_Customize | Qt::WStyle_NoBorder);// | Qt::WStyle_Dialog);
  QMainWindow *main_window = new QMainWindow(NULL, "Radium");
  tvisual->os_visual.main_window = main_window;
#ifdef USE_QT4
  main_window->setAttribute(Qt::WA_PaintOnScreen);
  main_window->setAttribute(Qt::WA_OpaquePaintEvent);
  main_window->setAttribute(Qt::WA_NoSystemBackground);
#endif
  main_window->setBackgroundMode(Qt::NoBackground);

  //QPalette pal = QPalette(main_window->palette());
  //pal.setColor( QPalette::Active, QColorGroup::Background, Qt::green);
  //pal.setColor(main_window->backgroundRole(), Qt::blue);
  //main_window->setPalette(pal);
  //main_window->menuBar()->setPalette(pal);

  MyWidget *mywidget=new MyWidget(tvisual,main_window,"name");
  mywidget->setFocus();
#ifdef USE_QT4
  mywidget->setAttribute(Qt::WA_PaintOnScreen);
  mywidget->setAttribute(Qt::WA_OpaquePaintEvent);
  mywidget->setAttribute(Qt::WA_NoSystemBackground);
#endif
  main_window->setBackgroundMode(Qt::NoBackground);

  main_window->resize(800,400);
  mywidget->setMinimumWidth(400);
  mywidget->setMinimumHeight(200);

  main_window->setCaption("Radium editor window");
  main_window->statusBar()->message( "Ready", 2000 );

  {
    QStatusBar *status_bar = main_window->statusBar();
    mywidget->status_label = new QLabel(status_bar);//"");
    mywidget->status_label->setFrameStyle(QFrame::Sunken);
    //mywidget->status_frame->
    status_bar->addWidget(mywidget->status_label, 1, true);
  }

  initMenues(main_window->menuBar());

  {
    QFont font = QFont("Monospace");

    char *fontstring = SETTINGS_read_string((char*)"font",NULL);
    if(fontstring!=NULL)
      font.fromString(fontstring);

    setFontValues(tvisual, font);

    mywidget->font = font;

    //mywidget->font->setStyleHint(QFont::TypeWriter);
    //mywidget->font->setFixedPitch(false);
  }

 if(tvisual->height==0 || tvisual->width==0){
    tvisual->x=0;
    tvisual->y=0;
    tvisual->width=mywidget->get_editor_width();
    tvisual->height=mywidget->get_editor_height();
  }

  tvisual->os_visual.widget=mywidget;

  mywidget->qpixmap=new QPixmap(mywidget->width(),mywidget->height());
#ifdef USE_QT3
  mywidget->qpixmap->setOptimization(QPixmap::BestOptim);
#endif

  mywidget->cursorpixmap=new QPixmap(mywidget->width(),mywidget->height());
#ifdef USE_QT3
  mywidget->cursorpixmap->setOptimization(QPixmap::BestOptim);
#endif


  g_main_window = main_window;
  g_mywidget = mywidget;

  return 0;
}

int GFX_ShutDownVisual(struct Tracker_Windows *tvisual){

  close_all_instrument_widgets();

  // Reuse it instead.
#if 0
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual.widget;

  //BS_StopXSend();
  //X11_MidiProperties_StopXSend();

  delete mywidget->qpixmap;
  delete mywidget->cursorpixmap;

  mywidget->close();
#endif
  return 0;
}

void GFX_SetMinimumWindowWidth(struct Tracker_Windows *tvisual, int width){
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual.widget;
  mywidget->setMinimumWidth(width);
}


void GFX_SetWindowTitle(struct Tracker_Windows *tvisual,char *title){
  QMainWindow *main_window = (QMainWindow *)tvisual->os_visual.main_window;
  main_window->setCaption(title);
}

void GFX_SetStatusBar(struct Tracker_Windows *tvisual,char *title){
  //QMainWindow *main_window = (QMainWindow *)tvisual->os_visual.main_window;
  //main_window->statusBar()->message(title);

  MyWidget *mywidget=(MyWidget *)tvisual->os_visual.widget;
  mywidget->status_label->setText(title);

#if 0
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual.widget;
  //mywidget->setCaption(title);

  mywidget->painter->fillRect(0,mywidget->height()-28,mywidget->width(),mywidget->height(),mywidget->colors[0]);

  mywidget->painter->setPen(mywidget->colors[1]);
  mywidget->painter->drawText(0,mywidget->height()-28+tvisual->org_fontheight+2,title);
#endif
}

void OS_GFX_C2V_bitBlt(
		    struct Tracker_Windows *window,
		    int from_x1,int from_x2,
		    int to_y
		    ){
  MyWidget *mywidget=(MyWidget *)window->os_visual.widget;

  DRAW_PIXMAP_ON_WIDGET(mywidget,
                        from_x1,to_y,
                        mywidget->cursorpixmap,
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
  MyWidget *mywidget=(MyWidget *)window->os_visual.widget;

  mywidget->cursorpixmap_painter->fillRect(x1,0,x4,height,mywidget->colors[7]);
  mywidget->cursorpixmap_painter->fillRect(x2,0,x3-x2+1,height,mywidget->colors[1]);

  //mywidget->cursorpixmap_painter->setCompositionMode(QPainter::CompositionMode_Xor);

  // TODO: fix Qt4
#ifdef USE_QT3
  bitBlt(
         mywidget->cursorpixmap,
         0,0,
         mywidget->qpixmap,
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
  
  MyWidget *mywidget=(MyWidget *)window->os_visual.widget;

  DRAW_PIXMAP_ON_WIDGET(
                        mywidget,
                        to_x,to_y,
                        mywidget->qpixmap,
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

#define GET_QPAINTER(mywidget,where) (where==PAINT_DIRECTLY ? mywidget->painter : mywidget->qpixmap_painter)

void OS_GFX_FilledBox(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2,int where){
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual.widget;
  GET_QPAINTER(mywidget,where)->fillRect(x,y,x2-x+1,y2-y+1,mywidget->colors[color]);
}

void OS_GFX_Box(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2,int where){
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual.widget;
  GET_QPAINTER(mywidget,where)->setPen(mywidget->colors[color]);
  GET_QPAINTER(mywidget,where)->drawRect(x,y,x2-x+1,y2-y+1);
}

void OS_GFX_Line(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2,int where){
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual.widget;

  GET_QPAINTER(mywidget,where)->setPen(mywidget->colors[color]);

#if 0
  QPen pen(mywidget->colors[color],4,Qt::SolidLine);  
  pen.setCapStyle(Qt::RoundCap);
  pen.setJoinStyle(Qt::RoundJoin);
  mywidget->qpixmap_painter->setPen(pen);
#endif

  GET_QPAINTER(mywidget,where)->drawLine(x,y,x2,y2);
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
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual.widget;

  if(brightness==MAX_BRIGHTNESS && color!=1){
    GET_QPAINTER(mywidget,where)->setPen(mywidget->colors[color]);
  }else{
    GET_QPAINTER(mywidget,where)->setPen(mix_colors(mywidget->colors[color], mywidget->colors[0], brightness/(float)MAX_BRIGHTNESS));
  }

  GET_QPAINTER(mywidget,where)->drawPoint(x,y);
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
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual.widget;
  QPainter *painter=GET_QPAINTER(mywidget,where);

  if(brightness==MAX_BRIGHTNESS && color!=1){
    painter->setPen(mywidget->colors[color]);
  }else{
    painter->setPen(mix_colors(mywidget->colors[color], mywidget->colors[0], brightness/(float)MAX_BRIGHTNESS));
  }

  while(mywidget->qpa.size() <= (unsigned int)num_points)
    mywidget->qpa.resize(mywidget->qpa.size()*2);
  
  for(int i=0;i<num_points;i++)
    mywidget->qpa.setPoint(i,x[i],y[i]);

  painter->drawPoints(mywidget->qpa,0,num_points);
}


void OS_GFX_SetClipRect(
                        struct Tracker_Windows *tvisual,
                        int x,int y,
                        int x2,int y2,
                        int where
                        )
{
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual.widget;
  QPainter *painter=GET_QPAINTER(mywidget,where);
  painter->setClipRect(x,y,x2-x,y2-y);
  painter->setClipping(true);
}

void OS_GFX_CancelClipRect(struct Tracker_Windows *tvisual,int where){
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual.widget;
  QPainter *painter=GET_QPAINTER(mywidget,where);
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
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual.widget;
  QPainter *painter=GET_QPAINTER(mywidget,where);

  painter->setPen(mywidget->colors[color]);
    
  if(flags & TEXT_BOLD){
    mywidget->font.setBold(true);
    painter->setFont(mywidget->font);
  }
  {  
    if(flags & TEXT_CENTER){
      QRect rect(x,y,tvisual->fontwidth*strlen(text),tvisual->org_fontheight);
      painter->drawText(rect, Qt::AlignVCenter, text);
    }else{
      painter->drawText(x,y+tvisual->org_fontheight-1,text);
    }
  }
  if(flags & TEXT_BOLD){
    mywidget->font.setBold(false);
    painter->setFont(mywidget->font);
  }
}                      

void OS_GFX_BitBlt(
	struct Tracker_Windows *tvisual,
	int dx,int dy,
	int x,int y,
	int x2,int y2
){
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual.widget;

  DRAW_PIXMAP_ON_PIXMAP(
                        mywidget->qpixmap,
                        x+dx,y+dy,
                        mywidget->qpixmap,
                        x,y,x2-x+1,y2-y+1
                        );
}


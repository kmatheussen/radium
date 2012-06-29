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

#include "Python.h"


#include <stdbool.h>
#include "MyWidget.h"
#include "Qt_Bs_edit_proc.h"
#include "Qt_instruments_proc.h"
#include "Qt_colors_proc.h"

#include <qpainter.h>
#include <qstatusbar.h>
#include <qmainwindow.h>
#include <qfontdialog.h>
#include <qcolordialog.h>


#ifdef USE_QT4
//#include <QMainWindow>
#  include <Q3PointArray>
#  include <QPixmap>
#  include <Q3PopupMenu>
#else
#  include "qpopupmenu.h"
#endif

#include <unistd.h>

#include <X11/Xlib.h>
#include "../X11/X11_keyboard_proc.h"

//#include "../X11/X11_Bs_edit_proc.h"
//#include "../X11/X11_MidiProperties_proc.h"

#include "../X11/X11_Qtstuff_proc.h"
#include "../X11/X11_ReqType_proc.h"

#include "../common/gfx_op_queue_proc.h"
#include "../common/settings_proc.h"


#ifdef USE_QT4
#  define DRAW_PIXMAP_ON_WIDGET(dst_widget, x, y, src_pixmap, from_x, from_y, width, height) \
     dst_widget->painter->drawPixmap(x,y,*(src_pixmap),from_x,from_y,width,height)
#  define DRAW_PIXMAP_ON_PIXMAP(dst_pixmap, x, y, src_pixmap, from_x, from_y, width, height) \
     dst_pixmap##_painter->drawPixmap(x,y,*(src_pixmap),from_x,from_y,width,height)
#else
#  define DRAW_PIXMAP_ON_WIDGET(dst_widget, x, y, src_pixmap, from_x, from_y, width, height) \
     bitBlt(dst_widget,x+XOFFSET,y+YOFFSET,src_pixmap,from_x,from_y,width,height)
#  define DRAW_PIXMAP_ON_PIXMAP(dst_pixmap, x, y, src_pixmap, from_x, from_y, width, height) \
     bitBlt(dst_pixmap,x,y,src_pixmap,from_x,from_y,width,height)
#endif


RPoints::RPoints() {
  this->qpa=new Q3PointArray(INITIALPOOLSIZE);

  this->num_points=0;

  //  super(0);
}

void RPoints::addPoint(int x,int y){
  if((int)this->qpa->size() <= (int)this->num_points){
    this->qpa->resize(this->qpa->size()*2);
  }
  this->qpa->setPoint(this->num_points,x,y);
  this->num_points++;
}

void RPoints::drawPoints(QPainter *qp){

  if(this->num_points==0) return;

  qp->drawPoints(*this->qpa,0,this->num_points);

  this->num_points=0;
}


MyWidget::MyWidget( struct Tracker_Windows *window,QWidget *parent, const char *name )
  : QFrame( parent, name, Qt::WStaticContents | Qt::WResizeNoErase | Qt::WRepaintNoErase | Qt::WNoAutoErase )
{
  this->qpixmap=NULL;
  this->window=window;

  setEditorColors(this);

  for(int lokke=0;lokke<8;lokke++){
    this->rpoints[lokke]=new RPoints();
  }

  setBackgroundColor( this->colors[0] );		/* grey background */

  this->setMouseTracking(true);

  //startTimer(2);


  //  this->grabKeyboard();
    /*
    count = 0;
    down = FALSE;
    points = new QPoint[MAXPOINTS];
    colors = new QColor[MAXCOLORS];
    for ( int i=0; i<MAXCOLORS; i++ )		// init color array
	colors[i] = QColor( rand()&255, rand()&255, rand()&255 );
	*/

  //this->setFrameStyle(QFrame::Raised );
  this->setFrameStyle(QFrame::Sunken );
  this->setFrameShape(QFrame::Panel);
  this->setLineWidth(2);

}

MyWidget::~MyWidget()
{
  /*
    delete[] points;				// cleanup
    delete[] colors;
    */
}


extern QApplication *qapplication;
extern bool doquit;

struct Menues{
  struct Menues *up;
  Q3PopupMenu *menu;
  QMenuBar *base;
};

static struct Menues *current_menu = NULL;


class MenuItem : public QWidget
{
    Q_OBJECT
public:
  MenuItem(const char *name, const char *python_command, Q3PopupMenu *menu = NULL){
    this->python_command = strdup(python_command);
    if(menu!=NULL) {
      if(current_menu->base!=NULL){
        current_menu->base->insertItem(name, menu, 0);
        current_menu->base->connectItem(0, this, SLOT(clicked()));
      }else{
        current_menu->menu->insertItem(name, menu, 0);
        current_menu->menu->connectItem(0, this, SLOT(clicked()));
      }
    }else
      if(current_menu->base!=NULL)
        current_menu->base->insertItem(name, this, SLOT(clicked()));
      else
        current_menu->menu->insertItem(name, this, SLOT(clicked()));
  }

private:
  const char *python_command;

public slots:
  void clicked() {
    PyRun_SimpleString(python_command);
    if(doquit==true)
      qapplication->quit();
  }
};

#include "mQt_visual.cpp"

void GFX_AddMenuItem(struct Tracker_Windows *tvisual, const char *name, const char *python_command){
  new MenuItem(name, python_command);
}

void GFX_AddMenuSeparator(struct Tracker_Windows *tvisual){
  if(current_menu->menu==NULL){
    RError("Can not add separator at toplevel");
    return;
  }
  current_menu->menu->insertSeparator();
}

void GFX_AddMenuMenu(struct Tracker_Windows *tvisual, const char *name, const char *command){
  struct Menues *menu = (struct Menues*)calloc(1, sizeof(struct Menues));
  menu->up = current_menu;
  menu->menu = new Q3PopupMenu();
  new MenuItem(name, command, menu->menu);
  current_menu = menu;
}

void GFX_GoPreviousMenuLevel(struct Tracker_Windows *tvisual){
  if(current_menu->base!=NULL){
    RError("Already at top level");
    return;
  }
  current_menu = current_menu->up;
}


static void initMenues(QMenuBar *base_menu){
  current_menu = (struct Menues*)calloc(1, sizeof(struct Menues));
  current_menu->base = base_menu;
}

static void setFontValues(struct Tracker_Windows *tvisual, const QFont &font){
  QFontMetrics fm(font);

  double width3 = R_MAX(fm.width("D#6"), R_MAX(fm.width("MUL"), fm.width("STP")));
  tvisual->fontwidth=(int)ceil(width3/3.0);
  tvisual->org_fontheight=fm.height();
  tvisual->fontheight=fm.height();
}

static QMainWindow *g_main_window = NULL;;
static MyWidget *g_mywidget = NULL;

//#include <qpalette.h>
int GFX_CreateVisual(struct Tracker_Windows *tvisual){
  QFont font = QFont("Monospace");

  char *fontstring = SETTINGS_read_string((char*)"font",NULL);
  if(fontstring!=NULL)
    font.fromString(fontstring);

  setFontValues(tvisual, font);

  if(g_main_window!=NULL){
    tvisual->os_visual.main_window = g_main_window;
    tvisual->os_visual.widget = g_mywidget;

    tvisual->width=g_mywidget->width()-100;
    tvisual->height=g_mywidget->height()-50;
    
    //g_mywidget->qpixmap=new QPixmap(g_mywidget->width(),mywidget->height());
    //g_mywidget->qpixmap->fill( mywidget->colors[0] );		/* grey background */

    g_mywidget->window = tvisual;

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

  // helvetica
  mywidget->font = font;

  //mywidget->font->setStyleHint(QFont::TypeWriter);
  //mywidget->font->setFixedPitch(false);

  initMenues(main_window->menuBar());
#if 0
  GFX_AddMenuItem(NULL, "item1", "dosomething");
  GFX_AddMenuItem(NULL, "item2", "");
  GFX_AddMenuMenu(NULL, "menu1");
  GFX_AddMenuItem(NULL, "menu1 - item1", "");
  GFX_AddMenuSeparator(NULL);
  GFX_AddMenuItem(NULL, "menu1 - item2", "");
  GFX_GoPreviousMenuLevel(NULL);
  GFX_AddMenuItem(NULL, "item3", "");
  GFX_AddMenuMenu(NULL, "menu2");
  GFX_AddMenuMenu(NULL, "menu2 -> menu1");
  GFX_AddMenuItem(NULL, "menu2 -> menu1 -> item1", "");
  GFX_GoPreviousMenuLevel(NULL);
  GFX_AddMenuItem(NULL, "item2 -> item1", "");
#endif

 if(tvisual->height==0 || tvisual->width==0){
    tvisual->x=0;
    tvisual->y=0;
    tvisual->width=mywidget->width()-100;
    tvisual->height=mywidget->height()-50;
  }

  tvisual->os_visual.widget=mywidget;

  if(mywidget->qpixmap==NULL){
    mywidget->qpixmap=new QPixmap(mywidget->width(),mywidget->height());
    mywidget->qpixmap->setOptimization(QPixmap::BestOptim);
    mywidget->qpixmap->fill( mywidget->colors[0] );		/* grey background */
  }

  mywidget->cursorpixmap=new QPixmap(mywidget->width(),mywidget->height());
  mywidget->cursorpixmap->setOptimization(QPixmap::BestOptim);
  mywidget->cursorpixmap->fill( mywidget->colors[7] );		/* the xored background color for the cursor.*/

  //BS_SetX11Window((int)main_window->x11AppRootWindow());
  //X11_MidiProperties_SetX11Window((int)main_window->x11AppRootWindow());

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


//bool GFX_SelectEditFont(struct Tracker_Windows *tvisual){return true;}
char *GFX_SelectEditFont(struct Tracker_Windows *tvisual){
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual.widget;
  mywidget->font = QFontDialog::getFont( 0, mywidget->font ) ;
  mywidget->setFont(mywidget->font);

  setFontValues(tvisual, mywidget->font);
  return talloc_strdup((char*)mywidget->font.toString().ascii());
}


void QGFX_bouncePoints(MyWidget *mywidget){
  for(int lokke=0;lokke<8;lokke++){
    mywidget->qpixmap_painter->setPen(mywidget->colors[lokke]);
    mywidget->rpoints[lokke]->drawPoints(mywidget->qpixmap_painter);
  }
}

void QGFX_C2V_bitBlt(
		    struct Tracker_Windows *window,
		    int from_x1,int from_x2,
		    int to_y
		    ){
  MyWidget *mywidget=(MyWidget *)window->os_visual.widget;

  QGFX_bouncePoints(mywidget);

  DRAW_PIXMAP_ON_WIDGET(mywidget,
 	 from_x1,to_y,
	 mywidget->cursorpixmap,
	 // The next two lines are the ones that should be used.
	 //	 from_x1,0,
	 //	 from_x2-from_x1+1,window->fontheight
	 // The next two lines are for debugging.
	 from_x1,0,
	 window->wblock->a.x2+-from_x1+1+100,window->fontheight
	 );
}


/* window,x1,x2,x3,x4,height, y pixmap */
void QGFX_C_DrawCursor(
				      struct Tracker_Windows *window,
				      int x1,int x2,int x3,int x4,int height,
				      int y_pixmap
				      ){
  MyWidget *mywidget=(MyWidget *)window->os_visual.widget;

  QGFX_bouncePoints(mywidget);

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


void QGFX_P2V_bitBlt(
		    struct Tracker_Windows *window,
		    int from_x,int from_y,
		    int to_x,int to_y,
		    int width,int height
		    ){
  
  MyWidget *mywidget=(MyWidget *)window->os_visual.widget;

  QGFX_bouncePoints(mywidget);

  DRAW_PIXMAP_ON_WIDGET(
                        mywidget,
                        to_x,to_y,
                        mywidget->qpixmap,
                        from_x,from_y,
                        width,height
                        );

  /*
  QGFX_C2V_bitBlt(
		 window,
		 from_x,width,
		 (window->wblock->curr_realline-window->wblock->top_realline)*window->fontheight+window->wblock->t.y1
		 );
  */
}

void QGFX_P_ClearWindow(struct Tracker_Windows *tvisual){
  QGFX_P_FilledBox(tvisual,0,0,0,tvisual->width,tvisual->height);
}

static void draw_filled_box(MyWidget *mywidget,QPainter *painter,int color,int x,int y,int x2,int y2);

void QGFX_P_FilledBox(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2){
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual.widget;
  draw_filled_box(mywidget,mywidget->qpixmap_painter,color,x,y,x2,y2);
}

void QGFX_P_Box(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2){
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual.widget;
  mywidget->qpixmap_painter->setPen(mywidget->colors[color]);
  mywidget->qpixmap_painter->drawRect(x,y,x2-x+1,y2-y+1);
}


void QGFX_P_Line(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2){
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual.widget;
//  QColor *qcolor=mywidget->colors[color];

  mywidget->qpixmap_painter->setPen(mywidget->colors[color]);

#if 0
  QPen pen(mywidget->colors[color],4,Qt::SolidLine);  
  pen.setCapStyle(Qt::RoundCap);
  pen.setJoinStyle(Qt::RoundJoin);
  mywidget->qpixmap_painter->setPen(pen);
#endif

  mywidget->qpixmap_painter->drawLine(x,y,x2,y2);
//  printf("drawline, x: %d, y: %d, x2: %d, y2: %d\n",x,y,x2,y2);
}

void QGFX_P_Point(
	struct Tracker_Windows *tvisual,
	int color,
	int x,int y
	)
{
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual.widget;

  color=color>7?7:color<0?0:color;

  mywidget->rpoints[color]->addPoint(x,y);

  //  paint.setPen(mywidget->colors[color>7?7:color<0?0:color]);
  //  paint.drawPoint(x,y);
}


int GFX_get_text_width(struct Tracker_Windows *tvisual, char *text){
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual.widget;
  const QFontMetrics fn = QFontMetrics(mywidget->font);
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

int GFX_get_num_characters(struct Tracker_Windows *tvisual, char *text, int max_width){
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual.widget;
  const QFontMetrics fn = QFontMetrics(mywidget->font);
  int len = strlen(text);
  QString string(text);

  //printf("width: %d / %d / %d\n",fn.width(string,len), fn.width(string,len/2), max_width);

  if(does_text_fit(fn, string, len, max_width))
    return len;
  else
    return find_text_length(fn, string, max_width, 0, len-1);
}

static void draw_text(struct Tracker_Windows *tvisual,
                      QPainter *painter,
                      int color,
                      char *text,
                      int x,
                      int y,
                      int width,
                      int flags
){
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual.widget;

  if(flags & TEXT_CLIPRECT){
    if(width==TEXT_IGNORE_WIDTH){
      RError("width can not be TEXT_IGNORE_WIDTH when using the TEXT_CLIPRECT flag");
    }else{
      if(width<=0)
        return;
      painter->setClipRect(x,y,width,tvisual->fontheight+20);
    }
    painter->setClipping(true);
  }

  if(flags & TEXT_INVERT){
    draw_filled_box(mywidget,painter,color,x,y,x+(tvisual->fontwidth*strlen(text)),y+tvisual->fontheight);
  }else if(flags & TEXT_CLEAR){
    draw_filled_box(mywidget,painter,0,x,y,x+(tvisual->fontwidth*strlen(text)),y+tvisual->fontheight);
  }

  painter->setPen(mywidget->colors[flags&TEXT_INVERT ? 0 : color]);

  if(flags & TEXT_BOLD){
    mywidget->font.setBold(true);
    painter->setFont(mywidget->font);
  }

  if(flags & TEXT_CENTER){
    QRect rect(x,y,tvisual->fontwidth*strlen(text),tvisual->org_fontheight);
    painter->drawText(rect, Qt::AlignVCenter, text);
  }else{
    painter->drawText(x,y+tvisual->org_fontheight-1,text);
  }

  if(flags & TEXT_BOLD){
    mywidget->font.setBold(false);
    painter->setFont(mywidget->font);
  }

  if(flags & TEXT_CLIPRECT){
    painter->setClipping(false);
  }
}                      

void QGFX_P_Text(
	struct Tracker_Windows *tvisual,
	int color,
	char *text,
	int x,
	int y,
        int width,
	int flags
){
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual.widget;
  draw_text(tvisual,mywidget->qpixmap_painter,color,text,x,y,width,flags);
}

void QGFX_Line(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2){
  
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual.widget;
//  QColor *qcolor=mywidget->colors[color];

  mywidget->painter->setPen(mywidget->colors[color]);
  mywidget->painter->drawLine(x,y,x2,y2);
//  printf("drawline, x: %d, y: %d, x2: %d, y2: %d\n",x,y,x2,y2);
  
}


void QGFX_All_Line(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2){
  QGFX_Line(tvisual,color,x,y,x2,y2);
}


void QGFX_Box(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2){
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual.widget;
  mywidget->painter->setPen(mywidget->colors[color]);
  mywidget->painter->drawRect(x,y,x2-x+1,y2-y+1);
}

static void draw_filled_box(MyWidget *mywidget, QPainter *painter,int color,int x,int y,int x2,int y2){
  painter->fillRect(x,y,x2-x+1,y2-y+1,mywidget->colors[color]);
}

void QGFX_FilledBox(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2){
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual.widget;
  draw_filled_box(mywidget,mywidget->painter,color,x,y,x2,y2);
}


void QGFX_Slider_FilledBox(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2){
  QGFX_FilledBox(tvisual,color,x,y,x2,y2);
}


void QGFX_All_FilledBox(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2){
  QGFX_FilledBox(tvisual,color,x,y,x2,y2);
}

void QGFX_Text(
	struct Tracker_Windows *tvisual,
	int color,
	char *text,
	int x,
	int y,
        int width,
        int flags
){
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual.widget;
  draw_text(tvisual,mywidget->painter,color,text,x,y,width,flags);
}

#if 0
void QGFX_P_InvertText(
	struct Tracker_Windows *tvisual,
	int color,
	char *text,
	int x,
	int y,
	bool clear
){
  QGFX_P_InvertTextNoText(tvisual, color, strlen(text), x, y, clear);
  QGFX_P_Text(tvisual, 0, text, x ,y, clear);
}

void QGFX_P_InvertTextNoText(
	struct Tracker_Windows *tvisual,
	int color,
	int len,
	int x,
	int y,
	bool clear
){
	QGFX_P_FilledBox(tvisual,color,x,y,
		x+(len*tvisual->fontwidth),
		y+tvisual->fontheight
	);
}
#endif

void QGFX_InitDrawCurrentLine(
	struct Tracker_Windows *tvisual,
	int x, int y, int x2, int y2
){
  //  QGFX_FilledBox(tvisual,2,x,y,x2,y2);
}

void QGFX_InitDrawCurrentLine2(
	struct Tracker_Windows *tvisual,
	int x, int y, int x2, int y2
){return ;}

void QGFX_DrawCursorPos(
	struct Tracker_Windows *tvisual,
	int fx, int fy, int fx2, int fy2,
	int x, int y, int x2, int y2
){
  //  MyWidget *mywidget=(MyWidget *)tvisual->os_visual.widget;
  //QPainter paint( mywidget );

  //  paint.setRasterOp(Qt::AndROP);





  //  QGFX_FilledBox(tvisual,2,fx,fy,fx2,fy2);
  // QGFX_FilledBox(tvisual,0,x,y,x2,y2);

  //  QGFX_C_DrawCursor(tvisual,fx,x,x2,fx2,tvisual->fontheight,fy);


  /*
  paint.fillRect(fx,fy,x-fx,fy2-fy+1,mywidget->colors[2]);
  paint.fillRect(x2+1,fy,fx2-x2,fy2-fy+1,mywidget->colors[2]);
  */
  //  paint.fillRect(x,y,x2-x+1,y2-y+1,mywidget->colors[0]);

  //  paint.setRasterOp(Qt::CopyROP);
  /*
	RectFill(tvisual->os_visual.window->RPort,(LONG)fx,(LONG)fy,(LONG)fx2,(LONG)fy2);
	SetAPen(tvisual->os_visual.window->RPort,0);
	SetWrMsk(tvisual->os_visual.window->RPort,2);
	RectFill(tvisual->os_visual.window->RPort,(LONG)x,(LONG)y,(LONG)x2,(LONG)y2);
	*/
}

static void Qt_BLine(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2){
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual.widget;

//  QColor *qcolor=mywidget->colors[color];

  mywidget->qpixmap_painter->setPen(mywidget->colors[color]);
  mywidget->qpixmap_painter->drawLine(x,y,x2,y2);
//  printf("drawline, x: %d, y: %d, x2: %d, y2: %d\n",x,y,x2,y2);
}

void QGFX_P_DrawTrackBorderDouble(
	struct Tracker_Windows *tvisual,
	int x, int y, int y2
){
  Qt_BLine(tvisual,1,x,y,x,y2);
  Qt_BLine(tvisual,2,x+1,y,x+1,y2);
}

void QGFX_P_DrawTrackBorderSingle(
	struct Tracker_Windows *tvisual,
	int x, int y, int y2
){
  Qt_BLine(tvisual,2,x,y,x,y2);
}

void QGFX_V_DrawTrackBorderDouble(
	struct Tracker_Windows *tvisual,
	int x, int y, int y2
){
  //  Qt_BLine(tvisual,2,x,y,x,y2);
  //Qt_BLine(tvisual,2,x+1,y,x+1,y2);
}

void QGFX_V_DrawTrackBorderSingle(
	struct Tracker_Windows *tvisual,
	int x, int y, int y2
){
  //  Qt_BLine(tvisual,2,x,y,x,y2);
}


void GFX_SetWindowTitle(struct Tracker_Windows *tvisual,char *title){
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

void QGFX_Scroll(
	struct Tracker_Windows *tvisual,
	int dx,int dy,
	int x,int y,
	int x2,int y2
){
  RWarning("QGFX_Scroll is not implemented. (Should not be needed).\n");
#if 0
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual.widget;
  //  const QPaintDevice *ai=(QPaintDevice *)mywidget;

  QGFX_bouncePoints(mywidget);

  mywidget->painter->drawPixmap(
		   x+dx,y+dy,
		   *mywidget,
		   x,y,x2-x+1,y2-y+1
		   );
#endif
  /*
	if(dy<0){
	  //RectFill(tvisual->os_visual.window->RPort,(LONG)x,(LONG)y2+dy,(LONG)x2,(LONG)y2);
		QGFX_FilledBox(tvisual,0,x,y2+dy,x2,y2);
	}else{
	  //		RectFill(tvisual->os_visual.window->RPort,(LONG)x,(LONG)y,(LONG)x2,(LONG)y+dy);
		QGFX_FilledBox(tvisual,0,x,y,x2,y+dy-1);
	}
  */
}

void QGFX_P_Scroll(
	struct Tracker_Windows *tvisual,
	int dx,int dy,
	int x,int y,
	int x2,int y2
){
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual.widget;
  //  const QPaintDevice *ai=(QPaintDevice *)mywidget;

  QGFX_bouncePoints(mywidget);

  DRAW_PIXMAP_ON_PIXMAP(
                        mywidget->qpixmap,
                        x+dx,y+dy,
                        mywidget->qpixmap,
                        x,y,x2-x+1,y2-y+1
                        );


  /*
	if(dy<0){
	  //RectFill(tvisual->os_visual.window->RPort,(LONG)x,(LONG)y2+dy,(LONG)x2,(LONG)y2);
		QGFX_FilledBox(tvisual,0,x,y2+dy,x2,y2);
	}else{
	  //		RectFill(tvisual->os_visual.window->RPort,(LONG)x,(LONG)y,(LONG)x2,(LONG)y+dy);
		QGFX_FilledBox(tvisual,0,x,y,x2,y+dy-1);
	}
  */
}


void QGFX_ScrollDown(
	struct Tracker_Windows *tvisual,
	int dx,int dy,
	int x,int y,
	int x2,int y2
){return ;}


void QGFX_ClearWindow(struct Tracker_Windows *tvisual){
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual.widget;
  //mywidget->fill(mywidget->colors[0]);
  QGFX_bouncePoints(mywidget);
  //  QGFX_FilledBox(tvisual,0,0,0,tvisual->width,tvisual->height);
  QGFX_P_FilledBox(tvisual,0,0,0,tvisual->width,tvisual->height);
  printf("cleared\n");
}

int GFX_ResizeWindow(struct Tracker_Windows *tvisual,int x,int y){return 0;}

void SetNormalPointer(struct Tracker_Windows *tvisual){return ;}
void SetResizePointer(struct Tracker_Windows *tvisual){return ;}



#if 0
ReqType QGFX_OpenReq(struct Tracker_Windows *tvisual,int width,int height,char *title){return NULL;}
void QGFX_CloseReq(struct Tracker_Windows *tvisual,ReqType reqtype){return ;}

int QGFX_GetInteger(struct Tracker_Windows *tvisual,ReqType reqtype,char *text,int min,int max){return max;}

float QGFX_GetFloat(struct Tracker_Windows *tvisual,ReqType reqtype,char *text,float min,float max){return max;}

char *QGFX_GetString(struct Tracker_Windows *tvisual,ReqType reqtype,char *text){return "null";}
#endif



void GFX_EditorWindowToFront(struct Tracker_Windows *tvisual){
  QMainWindow *main_window=static_cast<QMainWindow*>(tvisual->os_visual.main_window);

  GFX_PlayListWindowToBack();
  main_window->raise();

  XSetInputFocus(main_window->x11Display(),(Window)main_window->x11AppRootWindow(),RevertToNone,CurrentTime);
  X11_ResetKeysUpDowns();
}

//void GFX_ConfigColors(struct Tracker_Windows *tvisual){
//}
#if 0

#define NUM_COLORS 8

void GFX_ConfigColors(struct Tracker_Windows *tvisual){
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual.widget;

  for(int i=0;i<9;i++)
    QColorDialog::setCustomColor(i, mywidget->colors[i].rgb());

  QColorDialog::getColor(mywidget->colors[0]);

  for(int i=0;i<9;i++){
    mywidget->colors[i].setRgb(QColorDialog::customColor(i));
    char key[500];
    sprintf(key,"color%d",i);
    SETTINGS_write_string(key,(char*)mywidget->colors[i].name().ascii());
  }


#if 0
  //Colormap colormap;
  char command[100*NUM_COLORS];
  int lokke;
  char *string;
  int r[NUM_COLORS],g[NUM_COLORS],b[NUM_COLORS];
  printf("GOINGTO\n");

  sprintf(command,"X11_Qtstuff.GFX_ColorDialog(\"%%s\"");
  for(lokke=0;lokke<NUM_COLORS;lokke++){
    sprintf(command,"%s,%d,%d,%d",
            command,
            mywidget->colors[lokke].red(),
            mywidget->colors[lokke].green(),
            mywidget->colors[lokke].blue());
  }
  sprintf(command,"%s)",command);

  printf("String: -%s-\n",command);
  string=GFX_ReadStringFromPythonCommand(command);
  printf("Got: -%s-\n",string);

  sscanf(
	 string,
	 "%d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d",
	 &r[0],&g[0],&b[0],
	 &r[1],&g[1],&b[1],
	 &r[2],&g[2],&b[2],
	 &r[3],&g[3],&b[3],
	 &r[4],&g[4],&b[4],
	 &r[5],&g[5],&b[5],
	 &r[6],&g[6],&b[6],
	 &r[7],&g[7],&b[7]
	 );

#endif

#if 0
  colormap = DefaultColormap(x11_display,x11_screen);
  for(lokke=0;lokke<NUM_COLORS;lokke++){
    char rgb[500];
    printf("%d %d %d\n",r[lokke],g[lokke],b[lokke]);
    //XStoreColor(display, colormap, color)

    /*    xcolors[lokke].red=r[lokke];
    xcolors[lokke].green=g[lokke];
    xcolors[lokke].blue=b[lokke];
    */
    //    XStoreColor(x11_display,colormap,&xcolors[lokke]);
    sprintf(rgb,"rgbi:%f/%f/%f",(float)r[lokke]/256.0,(float)g[lokke]/256.0,(float)b[lokke]/256.0);
    XParseColor(x11_display,colormap,rgb,&xcolors[lokke]);
    XAllocColor(x11_display,colormap,&xcolors[lokke]);

    //    tvisual->os_visual.gcs[lokke]=XCreateGC(x11_display,window, 0, 0);
    XSetForeground(x11_display, tvisual->os_visual.gcs[lokke],xcolors[lokke].pixel);
  }
#endif
}
#endif


#if 0
char *GFX_GetLoadFileName(
	struct Tracker_Windows *tvisual,
	ReqType reqtype,
	char *seltext,
	char *dir
){return NULL;}

char *GFX_GetSaveFileName(
	struct Tracker_Windows *tvisual,
	ReqType reqtype,
	char *seltext,
	char *dir
){return NULL;}


#endif




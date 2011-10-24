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
#include <qlistbox.h>
#include <qpainter.h>
#include <unistd.h>

#include "../X11/X11_Bs_edit_proc.h"
#include "../X11/X11_MidiProperties_proc.h"



RPoints::RPoints() {
  this->qpa=new QPointArray(INITIALPOOLSIZE);

  this->num_points=0;

  //  super(0);
}

void RPoints::addPoint(int x,int y){
  if(this->qpa->size()<=this->num_points){
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
       : QWidget( parent, name, WNorthWestGravity | WResizeNoErase | WRepaintNoErase )
{
  this->qpixmap=NULL;
  this->window=window;

  /*
  this->colors[0]=QColor("grey");
  this->colors[1]=QColor("black");
  this->colors[2]=QColor("white");
  this->colors[3]=QColor("red");

  this->colors[4]=QColor("green");
  this->colors[5]=QColor("blue");
  this->colors[6]=QColor("yellow");
  this->colors[7]=QColor("pink");
  */

  this->colors[0]=QColor("grey");
  this->colors[1]=QColor("black");
  this->colors[2]=QColor("white");
  this->colors[3]=QColor("blue");

  this->colors[4]=QColor("yellow");
  this->colors[5]=QColor("red");
  this->colors[6]=QColor("orange");
  this->colors[7]=QColor("green");

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
}

MyWidget::~MyWidget()
{
  /*
    delete[] points;				// cleanup
    delete[] colors;
    */
}


extern QApplication *qapplication;

int GFX_CreateVisual(struct Tracker_Windows *tvisual){

  tvisual->os_visual=(struct OS_visual *)talloc_atomic(sizeof(struct OS_visual));

  tvisual->fontheight=17;
  tvisual->fontwidth=13;
  tvisual->org_fontheight=tvisual->fontheight;


  MyWidget *mywidget=new MyWidget(tvisual,NULL,"name");

  if(tvisual->height==0 || tvisual->width==0){
    tvisual->x=0;
    tvisual->y=0;
    tvisual->width=mywidget->width()-100;
    tvisual->height=mywidget->height()-30;
  }

  mywidget->show();

  tvisual->os_visual->widget=mywidget;

  if(mywidget->qpixmap==NULL){
    mywidget->qpixmap=new QPixmap(mywidget->width(),mywidget->height());
    mywidget->qpixmap->fill( mywidget->colors[0] );		/* grey background */
  }

  mywidget->cursorpixmap=new QPixmap(mywidget->width(),mywidget->height());
  mywidget->cursorpixmap->fill( mywidget->colors[7] );		/* the xored background color for the cursor.*/
//  mywidget->grabKeyboard();

  BS_SetX11Window((int)mywidget->x11AppRootWindow());
  X11_MidiProperties_SetX11Window((int)mywidget->x11AppRootWindow());

  return 0;
}

int GFX_ShutDownVisual(struct Tracker_Windows *tvisual){
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual->widget;

  BS_StopXSend();
  X11_MidiProperties_StopXSend();

  delete mywidget->qpixmap;
  delete mywidget->cursorpixmap;

  mywidget->close();
  return 0;
}

bool GFX_SelectEditFont(struct Tracker_Windows *tvisual){return true;}


void GFX_bouncePoints(MyWidget *mywidget){
  QPainter paint( mywidget->qpixmap );

  for(int lokke=0;lokke<8;lokke++){
    paint.setPen(mywidget->colors[lokke]);
    mywidget->rpoints[lokke]->drawPoints(&paint);
  }
}

void GFX_C2V_bitBlt(
		    struct Tracker_Windows *window,
		    int from_x1,int from_x2,
		    int to_y
		    ){
  MyWidget *mywidget=(MyWidget *)window->os_visual->widget;

  GFX_bouncePoints(mywidget);

  bitBlt(
	 mywidget,from_x1,to_y,
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
void GFX_C_DrawCursor(
				      struct Tracker_Windows *window,
				      int x1,int x2,int x3,int x4,int height,
				      int y_pixmap
				      ){
  MyWidget *mywidget=(MyWidget *)window->os_visual->widget;

  GFX_bouncePoints(mywidget);

  QPainter paint( mywidget->cursorpixmap );

  paint.fillRect(x1,0,x4,height,mywidget->colors[7]);
  paint.fillRect(x2,0,x3-x2+1,height,mywidget->colors[1]);

  bitBlt(
	 mywidget->cursorpixmap,
	 0,0,
	 mywidget->qpixmap,
	 0,y_pixmap,
	 x4+1,height
	 ,Qt::XorROP
	 );
}


void GFX_P2V_bitBlt(
		    struct Tracker_Windows *window,
		    int from_x,int from_y,
		    int to_x,int to_y,
		    int width,int height
		    ){
  
  MyWidget *mywidget=(MyWidget *)window->os_visual->widget;

  GFX_bouncePoints(mywidget);

  bitBlt(
	 mywidget,to_x,to_y,
	 mywidget->qpixmap,
	 from_x,from_y,
	 width,height
	 );

  /*
  GFX_C2V_bitBlt(
		 window,
		 from_x,width,
		 (window->wblock->curr_realline-window->wblock->top_realline)*window->fontheight+window->wblock->t.y1
		 );
  */
}

void GFX_P_ClearWindow(struct Tracker_Windows *tvisual){
  GFX_P_FilledBox(tvisual,0,0,0,tvisual->width,tvisual->height);
}

void GFX_P_FilledBox(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2){
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual->widget;
  QPainter paint( mywidget->qpixmap );
  paint.fillRect(x,y,x2-x+1,y2-y+1,mywidget->colors[color]);
}

void GFX_P_Box(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2){
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual->widget;
  QPainter paint( mywidget->qpixmap );
  paint.setPen(mywidget->colors[color]);
  paint.drawRect(x,y,x2-x+1,y2-y+1);
}


void GFX_P_Line(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2){
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual->widget;
//  QColor *qcolor=mywidget->colors[color];

  QPainter paint( mywidget->qpixmap );
  paint.setPen(mywidget->colors[color]);
  paint.drawLine(x,y,x2,y2);
//  printf("drawline, x: %d, y: %d, x2: %d, y2: %d\n",x,y,x2,y2);
}

void GFX_P_Point(
	struct Tracker_Windows *tvisual,
	int color,
	int x,int y
	)
{
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual->widget;

  color=color>7?7:color<0?0:color;

  mywidget->rpoints[color]->addPoint(x,y);

  //  paint.setPen(mywidget->colors[color>7?7:color<0?0:color]);
  //  paint.drawPoint(x,y);
}


void GFX_P_Text(
	struct Tracker_Windows *tvisual,
	int color,
	char *text,
	int x,
	int y,
	bool clear
){
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual->widget;
  QPainter paint( mywidget->qpixmap );

  if(clear){
    GFX_FilledBox(tvisual,0,x,y,x+(tvisual->fontwidth*strlen(text)),y+tvisual->fontheight);
  }

  QFont font=QFont("helvetica",tvisual->org_fontheight-5);
  font.setFixedPitch(false);
  paint.setFont(font);
  paint.setPen(mywidget->colors[color]);
  paint.drawText(x,y+tvisual->org_fontheight-1,text);

  //  int x2=x+(strlen(text)*tvisual->fontwidth);
  //  int y2=y+tvisual->fontheight-1;

  /*
  paint.drawLine(x,y,x,y2);
  paint.drawLine(x2,y,x2,y2);
  paint.drawLine(x,y,x2,y);
  paint.drawLine(x,y2,x2,y2);
  */
}

void GFX_Line(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2){
  
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual->widget;
//  QColor *qcolor=mywidget->colors[color];

  QPainter paint( mywidget );
  paint.setPen(mywidget->colors[color]);
  paint.drawLine(x,y,x2,y2);
//  printf("drawline, x: %d, y: %d, x2: %d, y2: %d\n",x,y,x2,y2);
  
}


void GFX_All_Line(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2){
  GFX_Line(tvisual,color,x,y,x2,y2);
}


void GFX_Box(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2){
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual->widget;
  QPainter paint( mywidget );
  paint.setPen(mywidget->colors[color]);
  paint.drawRect(x,y,x2-x+1,y2-y+1);
}


void GFX_FilledBox(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2){
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual->widget;
  QPainter paint( mywidget );
  paint.fillRect(x,y,x2-x+1,y2-y+1,mywidget->colors[color]);
}


void GFX_Slider_FilledBox(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2){
  GFX_FilledBox(tvisual,color,x,y,x2,y2);
}


void GFX_All_FilledBox(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2){
  GFX_FilledBox(tvisual,color,x,y,x2,y2);
}

void GFX_Text(
	struct Tracker_Windows *tvisual,
	int color,
	char *text,
	int x,
	int y,
	bool clear
){
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual->widget;
  QPainter paint( mywidget );

  QFont font=QFont("helvetica",tvisual->org_fontheight-5);
  //  font.setStrikeOut(true);

  if(clear){
    GFX_FilledBox(tvisual,0,x,y,x+(tvisual->fontwidth*strlen(text)),y+tvisual->org_fontheight);
  }

  paint.setFont(font);
  paint.setPen(mywidget->colors[color]);
  paint.drawText(x,y+tvisual->org_fontheight-1,text);

  /*
  int x2=x+(strlen(text)*tvisual->fontwidth);
  int y2=y+tvisual->fontheight-1;


  paint.drawLine(x,y,x,y2);
  paint.drawLine(x2,y,x2,y2);
  paint.drawLine(x,y,x2,y);
  paint.drawLine(x,y2,x2,y2);
  */

  /*
  GFX_Box(
	  tvisual,1,
	  x,
	  y,
	  x+(strlen(text)*tvisual->fontwidth),
	  y+tvisual->fontheight
	  );
  */
}
/*
void GFX_Text_noborder(
	struct Tracker_Windows *tvisual,
	int color,
	char *text,
	int x,
	int y,
	bool clear
){
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual->widget;
  QPainter paint( mywidget );
  paint.setFont(QFont("helvetica",tvisual->fontheight));
  GFX_FilledBox(tvisual,0,x,y,x+(tvisual->fontwidth*strlen(text)),y+tvisual->fontheight);
  paint.setPen(mywidget->colors[color]);
  paint.drawText(x,y+tvisual->fontheight,text);
}
*/

void GFX_P_InvertText(
	struct Tracker_Windows *tvisual,
	int color,
	char *text,
	int x,
	int y,
	bool clear
){return ;}


void GFX_P_InvertTextNoText(
	struct Tracker_Windows *tvisual,
	int color,
	int len,
	int x,
	int y,
	bool clear
){
	GFX_All_FilledBox(tvisual,1,x,y,
		x+(len*tvisual->fontwidth),
		y+tvisual->fontheight
	);
}

void GFX_InitDrawCurrentLine(
	struct Tracker_Windows *tvisual,
	int x, int y, int x2, int y2
){
  //  GFX_FilledBox(tvisual,2,x,y,x2,y2);
}

void GFX_InitDrawCurrentLine2(
	struct Tracker_Windows *tvisual,
	int x, int y, int x2, int y2
){return ;}

void GFX_DrawCursorPos(
	struct Tracker_Windows *tvisual,
	int fx, int fy, int fx2, int fy2,
	int x, int y, int x2, int y2
){
  //  MyWidget *mywidget=(MyWidget *)tvisual->os_visual->widget;
  //QPainter paint( mywidget );

  //  paint.setRasterOp(Qt::AndROP);





  //  GFX_FilledBox(tvisual,2,fx,fy,fx2,fy2);
  // GFX_FilledBox(tvisual,0,x,y,x2,y2);

  //  GFX_C_DrawCursor(tvisual,fx,x,x2,fx2,tvisual->fontheight,fy);


  /*
  paint.fillRect(fx,fy,x-fx,fy2-fy+1,mywidget->colors[2]);
  paint.fillRect(x2+1,fy,fx2-x2,fy2-fy+1,mywidget->colors[2]);
  */
  //  paint.fillRect(x,y,x2-x+1,y2-y+1,mywidget->colors[0]);

  //  paint.setRasterOp(Qt::CopyROP);
  /*
	RectFill(tvisual->os_visual->window->RPort,(LONG)fx,(LONG)fy,(LONG)fx2,(LONG)fy2);
	SetAPen(tvisual->os_visual->window->RPort,0);
	SetWrMsk(tvisual->os_visual->window->RPort,2);
	RectFill(tvisual->os_visual->window->RPort,(LONG)x,(LONG)y,(LONG)x2,(LONG)y2);
	*/
}

void Qt_BLine(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2){
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual->widget;

//  QColor *qcolor=mywidget->colors[color];

  QPainter paint( mywidget->qpixmap );
  paint.setPen(mywidget->colors[color]);
  paint.drawLine(x,y,x2,y2);
//  printf("drawline, x: %d, y: %d, x2: %d, y2: %d\n",x,y,x2,y2);
}

void GFX_P_DrawTrackBorderDouble(
	struct Tracker_Windows *tvisual,
	int x, int y, int y2
){
  Qt_BLine(tvisual,1,x,y,x,y2);
  Qt_BLine(tvisual,2,x+1,y,x+1,y2);
}

void GFX_P_DrawTrackBorderSingle(
	struct Tracker_Windows *tvisual,
	int x, int y, int y2
){
  Qt_BLine(tvisual,2,x,y,x,y2);
}

void GFX_V_DrawTrackBorderDouble(
	struct Tracker_Windows *tvisual,
	int x, int y, int y2
){
  //  Qt_BLine(tvisual,2,x,y,x,y2);
  //Qt_BLine(tvisual,2,x+1,y,x+1,y2);
}

void GFX_V_DrawTrackBorderSingle(
	struct Tracker_Windows *tvisual,
	int x, int y, int y2
){
  //  Qt_BLine(tvisual,2,x,y,x,y2);
}


void GFX_SetWindowTitle(struct Tracker_Windows *tvisual,char *title){
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual->widget;
  //mywidget->setCaption(title);

  QPainter paint( mywidget );
  paint.fillRect(0,mywidget->height()-28,mywidget->width(),mywidget->height(),mywidget->colors[0]);

  QFont font=QFont("helvetica",tvisual->org_fontheight);
  paint.setFont(font);
  paint.setPen(mywidget->colors[1]);
  paint.drawText(0,mywidget->height()-28+tvisual->org_fontheight+2,title);
  
}

void GFX_Scroll(
	struct Tracker_Windows *tvisual,
	int dx,int dy,
	int x,int y,
	int x2,int y2
){
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual->widget;
  //  const QPaintDevice *ai=(QPaintDevice *)mywidget;

  GFX_bouncePoints(mywidget);

  bitBlt(
		   mywidget,x+dx,y+dy,
		   mywidget,
		   x,y,x2-x+1,y2-y+1
		   );
  /*
	if(dy<0){
	  //RectFill(tvisual->os_visual->window->RPort,(LONG)x,(LONG)y2+dy,(LONG)x2,(LONG)y2);
		GFX_FilledBox(tvisual,0,x,y2+dy,x2,y2);
	}else{
	  //		RectFill(tvisual->os_visual->window->RPort,(LONG)x,(LONG)y,(LONG)x2,(LONG)y+dy);
		GFX_FilledBox(tvisual,0,x,y,x2,y+dy-1);
	}
  */
}

void GFX_P_Scroll(
	struct Tracker_Windows *tvisual,
	int dx,int dy,
	int x,int y,
	int x2,int y2
){
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual->widget;
  //  const QPaintDevice *ai=(QPaintDevice *)mywidget;

  GFX_bouncePoints(mywidget);

  bitBlt(
		   mywidget->qpixmap,x+dx,y+dy,
		   mywidget->qpixmap,
		   x,y,x2-x+1,y2-y+1
		   );

  /*
	if(dy<0){
	  //RectFill(tvisual->os_visual->window->RPort,(LONG)x,(LONG)y2+dy,(LONG)x2,(LONG)y2);
		GFX_FilledBox(tvisual,0,x,y2+dy,x2,y2);
	}else{
	  //		RectFill(tvisual->os_visual->window->RPort,(LONG)x,(LONG)y,(LONG)x2,(LONG)y+dy);
		GFX_FilledBox(tvisual,0,x,y,x2,y+dy-1);
	}
  */
}

/*
void GFX_ScrollDown(
	struct Tracker_Windows *tvisual,
	int dx,int dy,
	int x,int y,
	int x2,int y2
){return ;}
*/

void GFX_ClearWindow(struct Tracker_Windows *tvisual){
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual->widget;
  //mywidget->fill(mywidget->colors[0]);
  GFX_bouncePoints(mywidget);
  //  GFX_FilledBox(tvisual,0,0,0,tvisual->width,tvisual->height);
  GFX_P_FilledBox(tvisual,0,0,0,tvisual->width,tvisual->height);
  printf("cleared\n");
}

int GFX_ResizeWindow(struct Tracker_Windows *tvisual,int x,int y){return 0;}

void SetNormalPointer(struct Tracker_Windows *tvisual){return ;}
void SetResizePointer(struct Tracker_Windows *tvisual){return ;}



#if 0
ReqType GFX_OpenReq(struct Tracker_Windows *tvisual,int width,int height,char *title){return NULL;}
void GFX_CloseReq(struct Tracker_Windows *tvisual,ReqType reqtype){return ;}

int GFX_GetInteger(struct Tracker_Windows *tvisual,ReqType reqtype,char *text,int min,int max){return max;}

float GFX_GetFloat(struct Tracker_Windows *tvisual,ReqType reqtype,char *text,float min,float max){return max;}

char *GFX_GetString(struct Tracker_Windows *tvisual,ReqType reqtype,char *text){return "null";}
#endif


#if 0
class MyQListBox : public QListBox{
  Q_OBJECT
 public:
  int selected;
  MyQListBox(QWidget *qwidget,char *name);
 signals:
  void aselected(int index);
};

void MyQListBox::aselected(int index){
  this->selected=index;
  //  printf("selected: %d\n",index);
}

MyQListBox::MyQListBox(QWidget *qwidget,char *name) : QListBox(qwidget,name){
  this->selected=-1;
  connect(this,SIGNAL(selected(int)),SIGNAL(aselected(int)));
}
#endif


#if 0
int GFX_Menu(
	struct Tracker_Windows *tvisual,
	ReqType reqtype,
	char *seltext,
	int num_sel,
	char **menutext
){
  int ret;

  MyWidget *mywidget=(MyWidget *)tvisual->os_visual->widget;

  MyQListBox *qlb;
  //  QListViewItem *qlvi;

  printf("jadda\n");
  qlb=new MyQListBox(mywidget,seltext);

  for(int lokke=0;lokke<num_sel;lokke++){
    qlb->insertItem(menutext[lokke]);
  }

  qlb->setMinimumSize(mywidget->width()-10,mywidget->height()-10);

  qlb->setSelected(0,TRUE);
  qlb->setFocusPolicy(QWidget::StrongFocus);
  //  qlb->focusInEvent(new QFocusEvent(QEvent::FocusIn));
  qlb->setFocus();
  qlb->show();

  //  connect(qlb,SIGNAL(selected()),SIGNAL(aselected()));

  while(1){
    //    qapplication->exec();
    //qlb->update();
    //qlb->repaint();
    qapplication->processEvents();
    //    sleep(1);
    //    if(qlb->isSelected(3)) break;
    //printf("sel: %d\n",qlb->currentItem());
    if(qlb->selected!=-1) break;
  }

  ret=qlb->selected;

  qlb->close(TRUE);

  return ret;
}
#endif


void GFX_EditorWindowToFront(struct Tracker_Windows *tvisual){
  MyWidget *mywidget=(MyWidget *)tvisual->os_visual->widget;
  mywidget->raise();
}

void GFX_ConfigColors(struct Tracker_Windows *tvisual){
}


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



#include "mQt_visual.c"


#endif




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


#include "X11.h"




#define GFX_DONTSHRINK
#include "../common/nsmtracker.h"
#include "../common/visual_proc.h"


#include "X11_ReqType_proc.h"
#include "Python.h"

#include "X11_Bs_edit_proc.h"
#include "X11_MidiProperties_proc.h"

#include "X11_Qtstuff_proc.h"


//#define F   XFlush(x11_display);
#define F




static XColor xcolors[NUM_COLORS];

bool X11_UseXShm=false;


XShmSegmentInfo *X11_shminfo;

int X11_GetLegalColor(int color){
  if(color<NUM_COLORS) return color;
  return 1+(color%(NUM_COLORS-1));
}

void X11_InitXShm(void){
  XImage *ximage;

  X11_shminfo=calloc(1,sizeof(XShmSegmentInfo));

  ximage=XShmCreateImage(
			 x11_display,
			 (Visual *)DefaultVisual(x11_display,x11_screen),
			 (unsigned int)DefaultDepth(x11_display,x11_screen),
			 (int)XYPixmap,
			 (char *)NULL,
			 X11_shminfo,
			 (unsigned int)DisplayWidth(x11_display,x11_screen),
			 (unsigned int)(DisplayHeight(x11_display,x11_screen))
			 );
  
  X11_shminfo->shmid=shmget(
			    IPC_PRIVATE,
			    ximage->bytes_per_line * ximage->height,
			    IPC_CREAT|0777
			    );

  X11_shminfo->shmaddr=ximage->data=shmat(X11_shminfo->shmid,0,0);

  X11_shminfo->readOnly=False;

  XShmAttach(x11_display,X11_shminfo);

}


int GFX_CreateVisual(struct Tracker_Windows *tvisual){
  static char *xcolornames[NUM_COLORS]={"grey70","grey0","grey100","blue","yellow","red","orange","green"};
  Window window;
  Colormap colormap;
  XSetWindowAttributes attr;
  int lokke;
  static bool X11_XShmInitialized=false;

  tvisual->os_visual=(struct OS_visual *)calloc(1,sizeof(struct OS_visual));
  tvisual->os_visual->gcs=(GC *)malloc(sizeof(GC)*NUM_COLORS);
  for(lokke=0;lokke<NUM_COLORS;lokke++){
    tvisual->os_visual->xpoints[lokke]=calloc(sizeof(XPoint),NUM_XPOINTS);
  }

  //  tvisual->os_visual=(struct OS_visual *)talloc_atomic(sizeof(struct OS_visual));
  //tvisual->os_visual->gcs=(GC *)talloc_atomic(sizeof(GC)*NUM_COLORS);
  tvisual->fontheight=17;
  tvisual->fontwidth=13;
  tvisual->org_fontheight=tvisual->fontheight;
  
  attr.event_mask =
    KeyPressMask | KeyReleaseMask | 
    EnterWindowMask | LeaveWindowMask |
    PointerMotionMask | ButtonPressMask | ButtonReleaseMask |
     StructureNotifyMask |   ExposureMask;

  attr.background_pixmap=None;
  //  attr.background_pixel=BlackPixel(x11_display,x11_screen);
  //  attr.border_pixel=BlackPixel(x11_display,x11_screen);

#if 1
  window=tvisual->os_visual->window=XCreateWindow(
					   x11_display,
					   RootWindow(x11_display,x11_screen),
					   10,10,
					   800,600,
					   2,
					   0,
					   InputOutput,
					   (Visual *)CopyFromParent,
					   (CWBackPixel | CWBorderPixel | CWEventMask),
					   &attr
					   );
#endif
  printf("Made window with id %d\n",(int)window);

#if 0
  XSetStandardProperties (x11_display,
			  window,
			  "gakk",
			  NULL,
			  (Pixmap) 0,
			  argv, argc, &hints);
#endif

  colormap = DefaultColormap(x11_display,x11_screen);
  for(lokke=0;lokke<NUM_COLORS;lokke++){
    XParseColor(x11_display,colormap,xcolornames[lokke],&xcolors[lokke]);
    XAllocColor(x11_display,colormap,&xcolors[lokke]);

    tvisual->os_visual->gcs[lokke]=XCreateGC(x11_display,window, 0, 0);
    XSetForeground(x11_display, tvisual->os_visual->gcs[lokke],xcolors[lokke].pixel);
  }


  if(X11_UseXShm==true){
    if(X11_XShmInitialized==false){
      X11_InitXShm();
    }

    tvisual->os_visual->pixmap=XShmCreatePixmap(
						x11_display,
						window,
						X11_shminfo->shmaddr,
						X11_shminfo,
						800,
						600,
						(unsigned int)(DefaultDepth(x11_display,x11_screen))
						);
  }else{
    tvisual->os_visual->pixmap=XCreatePixmap(x11_display,
					     window,
					     800,
					     600,
					     (unsigned int)(DefaultDepth(x11_display,x11_screen))
					     );
  }
  XFillRectangle(
		 x11_display,
		 tvisual->os_visual->pixmap,
		 tvisual->os_visual->gcs[0],
		 0, 0, 800, 600
		 );

  tvisual->os_visual->cursorpixmap=XCreatePixmap(x11_display,
					   window,
					   800,
					   600,
					   (unsigned int)(DefaultDepth(x11_display,x11_screen))
					   );

  XFillRectangle(
		 x11_display,
		 tvisual->os_visual->cursorpixmap,
		 tvisual->os_visual->gcs[0],
		 0, 0, 800, 600
		 );

  printf("Depth: %d\n",DefaultDepth(x11_display,x11_screen));
  XMapWindow(x11_display,window);
  XFlush(x11_display);
  XStoreName(x11_display,window,"gakk");

  XFlush(x11_display);

  XFillRectangle(
		 x11_display,
		 window,
		 tvisual->os_visual->gcs[0],
		 0, 0, 800, 600
		 );
  

  XFlush(x11_display);


  tvisual->x=10;
  tvisual->y=10;
  tvisual->width=750;
  tvisual->height=550;

  BS_SetX11Window((int)window);
  X11_MidiProperties_SetX11Window((int)window);
  return 0;
}

int GFX_ShutDownVisual(struct Tracker_Windows *tvisual){

  BS_StopXSend();
  X11_MidiProperties_StopXSend();

  XFreePixmap(
	      x11_display,
	      tvisual->os_visual->pixmap
	      );

  XFreePixmap(
	      x11_display,
	      tvisual->os_visual->cursorpixmap
	      );

  if(tvisual->os_visual->xfontstruct!=NULL){
    XFreeFont(x11_display,tvisual->os_visual->xfontstruct);
  }

  XDestroyWindow(x11_display,tvisual->os_visual->window);

  return 0;
}



void GFX_P_bouncePoint(struct OS_visual *os_visual,int color){
  if(os_visual->num_points[color]>0){
    GC gc=os_visual->gcs[color];
    XDrawPoints(
		x11_display,
		os_visual->pixmap,
		gc,
		os_visual->xpoints[color],
		os_visual->num_points[color],
		CoordModeOrigin
		);
    os_visual->num_points[color]=0;
  }
}

void GFX_P_bouncePoints(struct OS_visual *os_visual){
  int lokke;
  for(lokke=0;lokke<NUM_COLORS;lokke++){
    GFX_P_bouncePoint(os_visual,lokke);
  }
}


void GFX_P_FilledBox(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2){
  GC gc=tvisual->os_visual->gcs[X11_GetLegalColor(color)];
  Pixmap pixmap=tvisual->os_visual->pixmap;

  XFillRectangle(x11_display, pixmap, gc, x, y, x2-x+1, y2-y+1);
F
}

void GFX_P_Box(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2){
  GC gc=tvisual->os_visual->gcs[X11_GetLegalColor(color)];
  Pixmap pixmap=tvisual->os_visual->pixmap;

  XDrawRectangle(x11_display, pixmap, gc, x, y, x2-x, y2-y);
F
}

void GFX_P_Line(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2){
  GC gc=tvisual->os_visual->gcs[X11_GetLegalColor(color)];
  Pixmap pixmap=tvisual->os_visual->pixmap;

  XDrawLine(x11_display, pixmap, gc, x, y, x2, y2);
F
}


void GFX_P_Point(struct Tracker_Windows *tvisual,int color,int x,int y){
#if 1
  struct OS_visual *os_visual=tvisual->os_visual;

  color=X11_GetLegalColor(color);

  os_visual->xpoints[color][os_visual->num_points[color]].x=x;
  os_visual->xpoints[color][os_visual->num_points[color]].y=y;
  os_visual->num_points[color]++;

  if(os_visual->num_points[color]==NUM_XPOINTS){
    GFX_P_bouncePoint(os_visual,color);
  }
  
#else

  GC gc=tvisual->os_visual->gcs[X11_GetLegalColor(color)];
  Pixmap pixmap=tvisual->os_visual->pixmap;
  XDrawPoint(x11_display,pixmap,gc,x,y);

#endif

}

void GFX_P_Text(
	struct Tracker_Windows *tvisual,
	int color,
	char *text,
	int x,
	int y,
	bool clear
	)
{
  GC gc=tvisual->os_visual->gcs[color];
  Pixmap pixmap=tvisual->os_visual->pixmap;

  XDrawString(x11_display, pixmap, gc, x+5, y + tvisual->org_fontheight-2,text, strlen(text));
F
}


static void X11_BLine(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2){
  GC gc=tvisual->os_visual->gcs[color];
  Pixmap pixmap=tvisual->os_visual->pixmap;

  XDrawLine(x11_display, pixmap, gc, x, y, x2, y2);
F
}

void GFX_P_Scroll(
	struct Tracker_Windows *tvisual,
	int dx,int dy,
	int x,int y,
	int x2,int y2
){
  Pixmap pixmap=tvisual->os_visual->pixmap;

  GFX_P_bouncePoints(tvisual->os_visual);

  XCopyArea(
	    x11_display,
	    pixmap,
	    pixmap,
	    DefaultGC(x11_display,x11_screen),
	    x,y,
	    x2-x+1,y2-y+1,
	    x+dx,y+dy
	    );

}


void GFX_C_DrawCursor(
				      struct Tracker_Windows *tvisual,
				      int x1,int x2,int x3,int x4,int height,
				      int y_pixmap
				      )
{
#if 1
  static GC gc2=NULL;


  GC gc=tvisual->os_visual->gcs[0];
  Pixmap pixmap=tvisual->os_visual->pixmap;
  Pixmap cursorpixmap=tvisual->os_visual->cursorpixmap;

  GFX_P_bouncePoints(tvisual->os_visual);

  XFillRectangle(x11_display, cursorpixmap, gc, x1, 0, x4, height);  
  XFillRectangle(x11_display, cursorpixmap, tvisual->os_visual->gcs[1], x2, 0, x3-x2+1, height);  

  if(gc2==NULL){
    gc2=XCreateGC(x11_display,tvisual->os_visual->window,0,0);
    //    XSetFunction(x11_display, gc2, GXxor);
    XSetFunction(x11_display, gc2, GXxor);
    XSetBackground(x11_display,gc2,xcolors[1].pixel);
    XSetForeground(x11_display,gc2,xcolors[7].pixel);
  }

  

  XCopyArea(
	    x11_display,
	    pixmap,
	    cursorpixmap,
	    gc2,
	    //	    DefaultGC(x11_display,x11_screen),
	    0,y_pixmap,
	    x4,height,
	    0,0
	    );
F

  //  XDrawLine(x11_display, cursorpixmap, tvisual->os_visual->gcs[7], x2, 0, x3, 0);

F
  //  XDrawLine(x11_display, cursorpixmap, tvisual->os_visual->gcs[7], x2, height-1, x3, height-1);
 
F 
#endif
}

void GFX_P2V_bitBlt(
		    struct Tracker_Windows *tvisual,
		    int from_x,int from_y,
		    int to_x,int to_y,
		    int width,int height
		    ){
  Window window=tvisual->os_visual->window;
  Pixmap pixmap=tvisual->os_visual->pixmap;

  GFX_P_bouncePoints(tvisual->os_visual);

  XCopyArea(
	    x11_display,
	    pixmap,
	    window,
	    DefaultGC(x11_display,x11_screen),
	    from_x,from_y,
	    width,height,
	    to_x,to_y
	    );	     
F
}


void GFX_C2V_bitBlt(
		    struct Tracker_Windows *tvisual,
		    int from_x1,int from_x2,
		    int to_y
		    ){
  Window window=tvisual->os_visual->window;
  Pixmap cursorpixmap=tvisual->os_visual->cursorpixmap;

  GFX_P_bouncePoints(tvisual->os_visual);

  XCopyArea(
	    x11_display,
	    cursorpixmap,
	    window,
	    DefaultGC(x11_display,x11_screen),
	    from_x1,0,
	    from_x2-from_x1,tvisual->fontheight,
	    from_x1,to_y
	    );	     
F
}


void GFX_EditorWindowToFront(struct Tracker_Windows *tvisual){
  XRaiseWindow(x11_display,tvisual->os_visual->window);
}

bool GFX_SelectEditFont(struct Tracker_Windows *tvisual){
  int lokke;
  XFontStruct *xfontstruct;
  char *rawfontname=GFX_ReadStringFromPythonCommand("X11_Qtstuff.GFX_SelectEditFont(\"%s\")");

  printf("Got -%s-\n",rawfontname);

  xfontstruct=XLoadQueryFont(x11_display,rawfontname);

  if(xfontstruct==NULL){
    printf("Unable to load font \"%s\"\n",rawfontname);
    return false;
  }

  tvisual->os_visual->xfontstruct=xfontstruct;

  //max_bounds.rbearing - min_bounds.lbearing

  tvisual->fontwidth=tvisual->h_fontwidth=xfontstruct->max_bounds.rbearing - xfontstruct->min_bounds.lbearing;
  tvisual->org_fontheight=xfontstruct->max_bounds.ascent+xfontstruct->max_bounds.descent;
  tvisual->fontheight=tvisual->org_fontheight+2;

  for(lokke=0;lokke<8;lokke++){
    GC gc=tvisual->os_visual->gcs[lokke];
    XSetFont(x11_display,gc,tvisual->os_visual->xfontstruct->fid);
  }
#if 0

       XSetFont(display, gc, font)
             Display *display;
             GC gc;
             Font font;


       Font XLoadFont(display, name)
             Display *display;
             char *name;

#endif

  return true;
}

void GFX_Line(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2){
  GC gc=tvisual->os_visual->gcs[color];
  Window window=tvisual->os_visual->window;

  XDrawLine(x11_display, window, gc, x, y, x2, y2);
F
}

void GFX_Box(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2){
  GC gc=tvisual->os_visual->gcs[color];
  Window window=tvisual->os_visual->window;

  XDrawRectangle(x11_display, window, gc, x, y, x2-x, y2-y);
F
}

void GFX_FilledBox(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2){
  GC gc=tvisual->os_visual->gcs[color];
  Window window=tvisual->os_visual->window;

  XFillRectangle(x11_display, window, gc, x, y, x2-x+1, y2-y+1);
F
}

void GFX_Text(
	struct Tracker_Windows *tvisual,
	int color,
	char *text,
	int x,
	int y,
	bool clear
){
  GC gc=tvisual->os_visual->gcs[color];
  Window window=tvisual->os_visual->window;

  if(clear){
    GFX_FilledBox(tvisual,0,x,y,x+(tvisual->fontwidth*strlen(text)),y+tvisual->org_fontheight);
  }

  XDrawString(x11_display, window, gc, x, y + tvisual->org_fontheight-2,text, strlen(text));
F
}


void GFX_SetWindowTitle(struct Tracker_Windows *tvisual,char *title){
#if 0
  Window window=tvisual->os_visual->window;
  
  XStoreName(x11_display,window,title);
#endif
F
}


void GFX_Scroll(
	struct Tracker_Windows *tvisual,
	int dx,int dy,
	int x,int y,
	int x2,int y2
){
  Window window=tvisual->os_visual->window;

  GFX_P_bouncePoints(tvisual->os_visual);

  XCopyArea(
	    x11_display,
	    window,
	    window,
	    DefaultGC(x11_display,x11_screen),
	    x,y,
	    x2-x+1,y2-y+1,
	    x+dx,y+dy
	    );
F
}



int GFX_ResizeWindow(struct Tracker_Windows *tvisual,int x,int y){return 0;}

void SetNormalPointer(struct Tracker_Windows *tvisual){return ;}
void SetResizePointer(struct Tracker_Windows *tvisual){return ;}



#if 0
void GFX_StartFileRequester(void){
  PyRun_SimpleString("import X11_FileDialog");
  PyRun_SimpleString("X11_FileDialog.GFX_StartFileRequester()");
}
#endif


#if 0
char *GFX_GetLoadFileName(
	struct Tracker_Windows *tvisual,
	ReqType reqtype,
	char *seltext,
	char *dir
){
  return GFX_ReadStringFromPythonCommand("X11_Qtstuff.GFX_OpenFileRequester(\"%s\")");
}


char *GFX_GetSaveFileName(
	struct Tracker_Windows *tvisual,
	ReqType reqtype,
	char *seltext,
	char *dir
){
  return GFX_ReadStringFromPythonCommand("X11_Qtstuff.GFX_SaveFileRequester(\"%s\")");
}
#endif

void GFX_ConfigColors(struct Tracker_Windows *tvisual){
  Colormap colormap;
  char command[100*NUM_COLORS];
  int lokke;
  char *string;
  int r[NUM_COLORS],g[NUM_COLORS],b[NUM_COLORS];
  printf("GOINGTO\n");

  sprintf(command,"X11_Qtstuff.GFX_ColorDialog(\"%%s\"");
  for(lokke=0;lokke<NUM_COLORS;lokke++){
    sprintf(command,"%s,%d,%d,%d",command,xcolors[lokke].red,xcolors[lokke].green,xcolors[lokke].blue);
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

    //    tvisual->os_visual->gcs[lokke]=XCreateGC(x11_display,window, 0, 0);
    XSetForeground(x11_display, tvisual->os_visual->gcs[lokke],xcolors[lokke].pixel);
  }
}

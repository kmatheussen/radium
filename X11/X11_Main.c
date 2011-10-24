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


#include <errno.h>

#include "X11.h"


#if 0
#include <pthread.h>
#endif

#include "../common/nsmtracker.h"
#include "../common/eventreciever_proc.h"
#include "../common/control_proc.h"

#include "../common/windows_proc.h"
#include "../common/resizewindow_proc.h"

#include "../common/cursor_updown_proc.h"
#include "../common/blocklist_proc.h"
#include "../common/OS_Bs_edit_proc.h"

#include "X11_Bs_edit_proc.h"
#include "X11_Qtstuff_proc.h"

#include "X11_keyboard_proc.h"

#include "X11_MidiProperties_proc.h"

#include "../common/disk_load_proc.h"

#include "X11_Ptask2Mtask_proc.h"

#include "X11_Player_proc.h"
#include "X11_ClientMessages_proc.h"

#include "../common/visual_proc.h"



extern struct Root *root;

Display *x11_display;
int x11_screen;


struct TEvent tevent={0};



static int MySysErrorHandler (Display * display)
{
  fprintf(stderr,"X11 error\n");
    fflush (stderr);

    //    XAutoRepeatOn(display);
    XCloseDisplay(display);
    exit(0);
}

#if 0
pthread_t pthread;

void *DoPlay(void *pointer){
  for(;;){
    usleep(50000);
    printf("gakk %d\n",5);
  }
}
#endif



extern void P2MUpdateSongPosCallBack(void);

int radium_main(char *arg){
  struct Tracker_Windows *window;

#if 0
  // Seems to be some kind of problem when running givertcap? Try to uncomment and
  // see if you can start radium without segfaulting. I can't. -Kjetil.
  system("/usr/bin/givertcap");
#endif

  x11_display=XOpenDisplay(NULL);
  if(x11_display==NULL){
    fprintf(stderr, "Unable to open display \"%s\"\n",XDisplayName(NULL));
    return 1;
  }

  XSetIOErrorHandler (MySysErrorHandler);

  x11_screen=DefaultScreen(x11_display);

  printf("arg: -%s-\n",arg);
  printf("Width/Height: %d/%d\n",DisplayWidth(x11_display,x11_screen),DisplayHeight(x11_display,x11_screen));

  X11_StartBlockSelector();
  X11_StartMidiProperties();

  StartGuiThread();
  X11_InitPlayer();

  if(InitProgram()==true){

    /* Here: More inits, receive events, other things. */
    bool notend=true;

    printf("Inited\n");

    XFlush(x11_display);

    //    DrawUpTrackerWindow(root->song->tracker_windows);
    //XFlush(x11_display);

    window=root->song->tracker_windows;

    //GFX_StartFileRequester();
    X11_StartQtStuff();

    BS_UpdateBlockList();
    BS_UpdatePlayList();
    BS_SelectBlock(root->song->blocks);
    BS_SelectPlaylistPos(0);
      

    if(strlen(arg)>0)
      Load(arg);

    lockGUI();

    while(notend==true){
      int fd;
      fd_set fdset;


      XFlush(x11_display);

      fd=ConnectionNumber(x11_display);
      FD_ZERO(&fdset);
      FD_SET(fd,&fdset);

      unlockGUI();

      while((fd=select(fd+1,&fdset,NULL,NULL,NULL)==-1)){
	if(errno!=EINTR)
	  break;
      }

      lockGUI();

      while(XPending(x11_display)) {
	XEvent event;
	XNextEvent(x11_display, &event);

	//XNextEvent (x11_display, &event);

	window=root->song->tracker_windows;

	//P2MUpdateSongPosCallBack();
	
	switch(event.type){
	case KeyPress:
	  if(X11_KeyPress((XKeyEvent *)&event,window)==1){
	    notend=false;
	  }
	  break;
	case KeyRelease:
	  X11_KeyRelease((XKeyEvent *)&event,window);
	  break;
	case ButtonPress:
	  //printf("%d %d %d\n",((XButtonEvent *)&event)->x,((XButtonEvent *)&event)->y,((XButtonEvent *)&event)->button);
	  switch(((XButtonEvent *)&event)->button){
	  case 1:
	    tevent.ID=TR_LEFTMOUSEDOWN;
	    break;
	  case 2:
	    tevent.ID=TR_MIDDLEMOUSEDOWN;
	    break;
	  case 3:
	    tevent.ID=TR_RIGHTMOUSEDOWN;
	    break;
	  case 4:
	    tevent.ID=TR_KEYBOARD;
	    tevent.SubID=EVENT_UPARROW;
	    break;
	  case 5:
	    tevent.ID=TR_KEYBOARD;
	    tevent.SubID=EVENT_DOWNARROW;
	    break;
	  default:
	    tevent.ID=TR_LEFTMOUSEDOWN;
	  }
	  tevent.x=((XButtonEvent *)&event)->x;
	  tevent.y=((XButtonEvent *)&event)->y;
	  EventReciever(&tevent,window);
	  break;
	case ButtonRelease:
	  //printf("%d %d %d\n",((XButtonEvent *)&event)->x,((XButtonEvent *)&event)->y,((XButtonEvent *)&event)->button);
	  switch(((XButtonEvent *)&event)->button){
	  case 1:
	    tevent.ID=TR_LEFTMOUSEUP;
	    break;
	  case 2:
	    tevent.ID=TR_MIDDLEMOUSEUP;
	    break;
	  case 3:
	    tevent.ID=TR_RIGHTMOUSEUP;
	    break;
	  default:
	    tevent.ID=TR_LEFTMOUSEUP;
	  }
	  tevent.x=((XButtonEvent *)&event)->x;
	  tevent.y=((XButtonEvent *)&event)->y;
	  EventReciever(&tevent,window);
	  break;
	case EnterNotify:
	case LeaveNotify:
	  X11_ResetKeysUpDowns();
	  break;
	case MotionNotify:
	  //	printf("%d %d %d\n",((XMotionEvent *)&event)->x,((XMotionEvent *)&event)->y,((XMotionEvent *)&event)->state);
	  tevent.ID=TR_MOUSEMOVE;
	  tevent.x=((XButtonEvent *)&event)->x;
	  tevent.y=((XButtonEvent *)&event)->y;
	  EventReciever(&tevent,window);
	  break;
	  //case VisibilityNotify:
	  // case ConfigureNotify:
	case ConfigureNotify:
	  if(
	     window->width!=((XConfigureEvent *)&event)->width
	     ||	window->height!=((XConfigureEvent *)&event)->height
	     )
	    {
#if 0	   
	      fprintf(stderr,"Resized %d %d\n",
		      ((XConfigureEvent *)&event)->width,
		      ((XConfigureEvent *)&event)->height
		      );
#endif    
	      XFreePixmap(
			  x11_display,
			  window->os_visual->pixmap
			  );
	      XFreePixmap(
			  x11_display,
			  window->os_visual->cursorpixmap
			  );
	      window->os_visual->pixmap=XCreatePixmap(x11_display,
						      window->os_visual->window,
						      ((XConfigureEvent *)&event)->width,
						      ((XConfigureEvent *)&event)->height,
						      (unsigned int)(DefaultDepth(x11_display,x11_screen))
						      );
	      XFillRectangle(
			     x11_display,
			     window->os_visual->pixmap,
			     window->os_visual->gcs[0],
			     0, 0, ((XConfigureEvent *)&event)->width,((XConfigureEvent *)&event)->height
			     );
	      window->os_visual->cursorpixmap=XCreatePixmap(x11_display,
							    window->os_visual->window,
							    ((XConfigureEvent *)&event)->width,
							    ((XConfigureEvent *)&event)->height,
							    (unsigned int)(DefaultDepth(x11_display,x11_screen))
							    );
	      XFillRectangle(
			     x11_display,
			     window->os_visual->cursorpixmap,
			     window->os_visual->gcs[0],
			     0, 0, ((XConfigureEvent *)&event)->width, ((XConfigureEvent *)&event)->height
			     );
	      
	      Resize_resized(
			     window,
			     ((XConfigureEvent *)&event)->width-1,
			     ((XConfigureEvent *)&event)->height-1,
			     true
			     );
	    }
	  
	  break;
	case Expose:
	  printf("Expose\n");
	  //GFX_FilledBox(window,0,0,0,window->width,window->height);
	  GFX_ClearWindow(window);
	  //DrawUpTrackerWindow(root->song->tracker_windows);
	  Resize_resized(window,window->width,window->height,false);
	  //XFlush(x11_display);
	  
	  break;
	case NoExpose:
	  break;
	case GraphicsExpose:
	  printf("GRaphicsExpose event.\n");
	  break;
	case ClientMessage:
	  X11Event_ClientMessage((XClientMessageEvent *)&event,window);
	  //	printf("Received \"%s\"\n",((XClientMessageEvent *)&event)->data.b);	
	  break;
	default:
	  printf("Unknown xevent %d\n",event.type);
	  break;
	}
      }
    }

    EndProgram();
  }

  X11_EndPlayer();
  unlockGUI();
  EndGuiThread();

  XCloseDisplay(x11_display);

  return 0;
}


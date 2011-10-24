#include <X11/Xlib.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>


int main(int argc,char **argv){
  Display *display;
  XEvent message;
  Window window;
  int lokke;
  char temp[500];

  display=XOpenDisplay(NULL);

#if 0
  window=XCreateWindow(
		       display,
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
   window = XCreateSimpleWindow(
				display
				, DefaultRootWindow(display)
				, 0, 0
				, 100, 100
				, 0
				, BlackPixel(display, DefaultScreen(display))
				, BlackPixel(display, DefaultScreen(display))
				);

   printf("Window: %d\n",window);
   XFlush(display);
#if 1
   sprintf(temp,"../bin/X11_XSendEvent %d 1 2 3 4 5",window);
   system(temp);

   for(;;){
     XEvent event;
     XNextEvent (display, &event);

     if(event.type==ClientMessage){
       XClientMessageEvent *x=(XClientMessageEvent *)(&event);
       printf("Got it %d %d %d %d %d\n",x->data.l[0],x->data.l[1],x->data.l[2],x->data.l[3],x->data.l[4]);
       break;
     }
   }
#endif
     //gets(temp);
  XCloseDisplay(display);

  return 0;
}




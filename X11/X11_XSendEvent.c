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



#include <X11/Xlib.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>


int main(int argc,char **argv){
  Display *x11_display;
  XEvent message;
  int lokke;

  message.xclient.type = ClientMessage;
  message.xclient.format=32;

  message.xclient.window = atoi(argv[1]);

  printf("Argc: %d\n",argc);
  for(lokke=2;lokke<argc;lokke++){
    message.xclient.data.l[lokke-2]=atoi(argv[lokke]);
    printf("arg %d=%d\n",lokke-2,atoi(argv[lokke]));
  }

  x11_display=XOpenDisplay(NULL);

  XSendEvent(
	     x11_display,
	     atoi(argv[1]),
	     //	     strcmp(argv[2],"True")?False:True,
	     True,
	     //atoi(argv[3]),
	     NoEventMask,
	     &message
	     );
  XCloseDisplay(x11_display);

  return 0;
}


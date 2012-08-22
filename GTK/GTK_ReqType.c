/* Copyright 2012 Kjetil S. Matheussen

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

#include <string.h>

#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>

#include "../common/nsmtracker.h"
#include "../common/visual_proc.h"
#include "../Qt/Qt_MainWindow_proc.h"

#ifdef __linux__
#  include "../X11/X11_keyboard_proc.h"
#endif

#include "../common/OS_visual_input.h"

#ifdef TEST_MAIN

int num_users_of_keyboard = 0;
void X11_ResetKeysUpDowns(void){}
GtkWidget *vbox=NULL;
void *talloc_atomic(size_t size){return malloc(size);}

#else //  TEST_MAIN

extern int num_users_of_keyboard;

#endif //  TEST_MAIN


typedef struct{
  GtkWidget *window;
  GtkWidget *text_view;
  int x,y;
  int pixelwidth,pixelheight;
  GMainLoop* gloop;
} reqtype_t;


static int get_curr_len(reqtype_t *reqtype){
  GtkTextIter iter_start;
  GtkTextIter iter_end;

  GtkTextBuffer *text_buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(reqtype->text_view));

  gtk_text_buffer_get_iter_at_line(text_buffer,&iter_start,reqtype->y);
  gtk_text_iter_set_line_offset(&iter_start,reqtype->x);

  gtk_text_buffer_get_iter_at_line(text_buffer,&iter_end,reqtype->y);
  gtk_text_iter_forward_to_line_end(&iter_end);
  
  return strlen(gtk_text_buffer_get_text(text_buffer,&iter_start,&iter_end,true));
}

static gboolean window_hidden(GtkWidget *widget, GdkEvent *event, gpointer callback_data){
  return TRUE; // Oh no you can't.
}

static gint keyboard_input( GtkWidget *widget,
                            GdkEvent  *event,
                            gpointer   callback_data )
{
  reqtype_t *reqtype=callback_data;

  printf("got key. state: %x, val: %d\n",(int)event->key.state,(int)event->key.keyval);
  //GDK_Return
  int val=event->key.keyval;
  switch(val){
  case GDK_Up:
  case GDK_Down:
  case GDK_Left:
  case GDK_Right:
    return TRUE;
  case GDK_BackSpace:
    if(get_curr_len(callback_data) <= 0)
      return TRUE;
    else
      break;
  case GDK_Return:
    g_main_loop_quit(reqtype->gloop);
    break;
  }

  return FALSE;
}

extern GtkWidget *plug; // plug is not a normal widget. Not sure if it works very logically for focus.
extern GtkWidget *vbox;

static void open_dialog(reqtype_t *reqtype){
  reqtype->y = 0;
  reqtype->x = 0;

  reqtype->window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_position(GTK_WINDOW(reqtype->window),GTK_WIN_POS_MOUSE);
  gtk_window_set_keep_above(GTK_WINDOW(reqtype->window),true);

  reqtype->text_view = gtk_text_view_new_with_buffer(NULL);
  gtk_widget_set_can_focus(reqtype->text_view,true);

  g_signal_connect (reqtype->text_view,
                    "key_press_event",
                    G_CALLBACK (keyboard_input),
                    reqtype);

  g_signal_connect(reqtype->window,
                   "delete-event",
                   G_CALLBACK(window_hidden),
                   reqtype);

  gtk_container_add(GTK_CONTAINER(reqtype->window), reqtype->text_view);

  gtk_window_resize((GtkWindow*)reqtype->window,reqtype->pixelwidth,reqtype->pixelheight);

  gtk_widget_show(reqtype->window);
  gtk_widget_show(reqtype->text_view);
}

ReqType GFX_OpenReq(struct Tracker_Windows *tvisual,int width,int height,char *title){
  num_users_of_keyboard++; // disable X11 keyboard sniffer

  reqtype_t *reqtype = talloc_atomic(sizeof(reqtype_t));
  reqtype->pixelwidth = tvisual==NULL ? 10*width : width*tvisual->fontwidth;
  reqtype->pixelheight = tvisual==NULL ? 16*height : height*tvisual->fontwidth;

  reqtype->gloop = g_main_loop_new (NULL, FALSE);

  open_dialog(reqtype);

  Qt_DisableAllWidgets();
  GFX_disable_mouse_keyboard();

  return reqtype;
}

extern void GTK_SetFocus(void);
extern void grabKeyboard(void);


void GFX_CloseReq(struct Tracker_Windows *tvisual,ReqType das_reqtype){
  reqtype_t *reqtype=das_reqtype;

  gtk_widget_destroy(GTK_WIDGET(reqtype->window));
  g_main_loop_unref(reqtype->gloop);

  num_users_of_keyboard--;

#ifdef __linux__
  X11_ResetKeysUpDowns(); // Since we disabled X11 events, the X11 event sniffer didn't notice that we changed focus.
#endif

  Qt_EnableAllWidgets();
  GFX_enable_mouse_keyboard();

  GTK_SetFocus();
}


void GFX_WriteString(ReqType das_reqtype,char *text){
  reqtype_t *reqtype = das_reqtype;
  GtkTextIter iter;

  GtkTextBuffer *text_buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(reqtype->text_view));
  gtk_text_buffer_get_iter_at_line(text_buffer,&iter,reqtype->y);
  gtk_text_iter_forward_to_line_end(&iter);

  gtk_text_buffer_insert(text_buffer,&iter,text,strlen(text));

  int i;
  for(i=0;i<strlen(text);i++)
    if(text[i]=='\n')
      reqtype->y++;

  printf("curr linenum: %d\n",reqtype->y);

  reqtype->x = 0;
  reqtype->x = get_curr_len(reqtype);
}


void GFX_ReadString(ReqType das_reqtype,char *buffer,int bufferlength){
  reqtype_t *reqtype = das_reqtype;

  gtk_widget_grab_focus(reqtype->text_view);
  //gtk_widget_grab_focus(reqtype->window);
  //gtk_widget_activate(reqtype->text_view);
  gtk_window_present( GTK_WINDOW(reqtype->window));

  printf("wating\n");
  fflush(stdout);

  g_main_loop_run(reqtype->gloop);

  printf("wating over\n");
  fflush(stdout);

  {
    GtkTextIter iter_start;
    GtkTextIter iter_end;

    GtkTextBuffer *text_buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(reqtype->text_view));

    printf("pos: %d/%d\n",reqtype->y,reqtype->x);
    gtk_text_buffer_get_iter_at_line(text_buffer,&iter_start,reqtype->y);
    gtk_text_iter_set_line_offset(&iter_start,reqtype->x);

    gtk_text_buffer_get_iter_at_line(text_buffer,&iter_end,reqtype->y);
    gtk_text_iter_forward_to_line_end(&iter_end);
    
    snprintf(buffer,bufferlength-1,"%s",gtk_text_buffer_get_text(text_buffer,&iter_start,&iter_end,true));
  }

  printf("buffer: \"%s\"\n",buffer);

  reqtype->y++;
}

#ifdef TEST_MAIN
/*
  gcc -g -DTEST_MAIN -DUSE_GTK_VISUAL -DDEBUG -I../Qt GTK_ReqType.c -Wall `pkg-config --libs --cflags gtk+-2.0` && ./a.out
  G_DEBUG=fatal-criticals gdb ./a.out
*/

int main(int argc, char **argv){
  gtk_init(&argc, &argv);

  ReqType reqtype = GFX_OpenReq(NULL,30,5,"gakkgakk");

  {
    GFX_WriteString(reqtype,"hello1? ");

    char temp[500];
    GFX_ReadString(reqtype,temp,500);

    printf("Got \"%s\"\n",temp);
  }

  {
    GFX_WriteString(reqtype,"line above");
    GFX_WriteString(reqtype,"\n");
    GFX_WriteString(reqtype,"> ");
    //GFX_WriteString(reqtype,"hello2?\n Answer: ");

    char temp[500];
    GFX_ReadString(reqtype,temp,500);

    printf("Got \"%s\"\n",temp);
  }

  GFX_CloseReq(NULL,reqtype);


  return 0;
}
#endif

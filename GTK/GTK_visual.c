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

#if 0


#include "../common/nsmtracker.h"


#if GTK_IS_USED

// Sometimes, setting this to 1 helps the window to embed. (TODO. need proper solution)
// (Note, must edit in Qt_MainWindow.cpp.
// Update: Calling g_embed_container->show() before calling CreateVisual seems to fix the problem.
#define USE_EMBED_WORKAROUND 0

// KDE for Ubuntu 12 usually needs the workaround.
//#define USE_EMBED_WORKAROUND (getenv("KDE_FULL_SESSION")!=NULL)



#include <string.h>
#include <math.h>

#include <gtk/gtk.h>

#if USE_GTK_VISUAL

#ifdef FOR_WINDOWS
#  include <windows.h>
#  include <gdk/gdkwin32.h>
#endif

#ifdef __linux__
#  include <X11/Xlib.h>
#  include <gdk/gdkx.h>
#endif

#ifdef FOR_MACOSX
#  include "../macosx/cocoa_embed_proc.h"
#endif

#include "../common/blts_proc.h"
#include "../common/gfx_proc.h"
#include "../common/wblocks_proc.h"
#include "../common/settings_proc.h"
#include "../common/gfx_op_queue_proc.h"
#include "../common/eventreciever_proc.h"

#ifdef __linux__
#include "../X11/X11_keyboard_proc.h"
#endif

#ifdef FOR_WINDOWS
#  include "../windows/W_Keyboard_proc.h"
#endif

#include "../common/visual_op_queue_proc.h"
#include "../common/visual_proc.h"

#include "Qt_Main_proc.h"

#include "GTK_visual_proc.h"

//#define SetFocus(a) /* */

extern struct Root *root;

// I'm using only global variables for GTK data. So far, the code is quite straight forward, and it's so little data here,
// that the added benefit of keeping data only in this file probably surpass the advantage of less imperative code.[1]
// [1] All variables can be rebuilt and put into a struct in the os_visual slot, instead of overwriting like now.
//
// In case more than one window will be properly supported, or another reasonable
// reason for avoiding global variables comes up, it's simple to put all these variables
// into a new struct, and put that struct again into the os_visual slot of the Tracker_Windows struct.


GtkWidget *plug;
GtkWidget *vbox;

static GdkPixmap *pixmap;
static GdkPixmap *cursor_pixmap;

static PangoFontDescription *font_description;
static GtkWidget *font_selector;
static const char *font_save_name; // The SETTINGS name might not be legal, but this one is.
static const char *font_name;

static PangoContext *pango_context;
static PangoLayout *pango_layout;

static GdkColor colors[16];
static GdkGC *gc_colors_vbox[16];
static GdkGC *gc_colors_pixmap[16];

static GdkGC *gc_vbox;
static GdkGC *gc_pixmap;
static GdkGC *gc_cursor_pixmap;

static GdkGC *gc_cliprect_vbox;
static GdkGC *gc_cliprect_pixmap;

static GdkGC *gc_mix_color_vbox;
static GdkGC *gc_mix_color_pixmap;

static void get_text_size(const char *text,int *width, int *height){
  pango_layout_set_text(pango_layout, text, -1);

  pango_layout_get_pixel_size(pango_layout,width,height);
}

static int get_text_width(const char *text){
  int height,width;
  get_text_size(text,&width,&height);
  return width;
}


static int get_text_height(const char *text){
  int height,width;
  get_text_size(text,&width,&height);
  return height;
}

static bool font_has_changed(void){
  if(GTK_WIDGET_VISIBLE(font_selector)==true){
    const char *new_font_name = gtk_font_selection_dialog_get_font_name((GtkFontSelectionDialog*)font_selector);
    if(strcmp(new_font_name,font_name))
      return true;
  }
  return false;
}

void setFontValues(struct Tracker_Windows *window){
  double width3 = R_MAX(get_text_width("D#6"), R_MAX(get_text_width("MUL"), get_text_width("STP")));
  window->fontwidth      = (int)ceil(width3/3.0);
  window->fontheight     = get_text_height("D#6");
  //window->fontheight     = pango_font_description_get_size(font_description)/PANGO_SCALE;
  window->org_fontheight = window->fontheight;
}

#define FONT_THICKNESS_CONFIG 0

#if FONT_THICKNESS_CONFIG
static int font_thickness = 200;
#endif

static void update_font(void){
  const char *new_font_name = gtk_font_selection_dialog_get_font_name((GtkFontSelectionDialog*)font_selector);
  struct Tracker_Windows *window=root->song->tracker_windows;

  pango_font_description_free(font_description);
  font_description = pango_font_description_from_string(new_font_name);
#if FONT_THICKNESS_CONFIG
  pango_font_description_set_weight(font_description, font_thickness);
#endif

  pango_layout_set_font_description (pango_layout, font_description);

  setFontValues(window);
      
  printf("new_font_name: \"%s\". font_name: \"%s\". height: %d, width: %d\n",new_font_name,font_name,window->fontheight,window->fontwidth);
  font_name = new_font_name;
  DO_GFX_BLT({
      UpdateAllWBlockWidths(window);
      DrawUpTrackerWindow(window);
    });
  GFX_play_op_queue(window);
}

#if FONT_THICKNESS_CONFIG
void set_font_thickness(int val){
  printf("Setting thickness to %d\n",val);
  font_thickness = val;
  update_font();
}
#endif

static bool mouse_keyboard_disabled = false;

void GFX_disable_mouse_keyboard(void){
  mouse_keyboard_disabled = true;
}

void GFX_enable_mouse_keyboard(void){
  mouse_keyboard_disabled = false;
}


extern int num_users_of_keyboard;


GdkFilterReturn FilterFunc(GdkXEvent *xevent,GdkEvent *event,gpointer data)
{
  if(num_users_of_keyboard>0){
    return GDK_FILTER_CONTINUE;
  }
  if(mouse_keyboard_disabled==true){
    return GDK_FILTER_CONTINUE;
  }

#ifdef __linux__
  {
    XEvent *event = (XEvent*)xevent;
    if(((XAnyEvent*)xevent)->window==GDK_WINDOW_XID(plug->window)){
      if(event->type==ButtonPress){
        if(((XButtonEvent *)event)->button==1){ // When running in KDE on Ubuntu 12, the left mouse button down is always lost. This is a workaround.
          printf("x/y: %d/%d\n",(int)((XButtonEvent *)event)->x,(int)((XButtonEvent *)event)->y);
          struct Tracker_Windows *window=root->song->tracker_windows;
          struct TEvent tevent;
          tevent.ID=TR_LEFTMOUSEDOWN;
          tevent.x = (int)((XButtonEvent *)event)->x;
          tevent.y = (int)((XButtonEvent *)event)->y;
          EventReciever(&tevent,window);
          GFX_play_op_queue(window);
          return GDK_FILTER_REMOVE;
        }
      }
    }
  }

  GdkFilterReturn ret = X11_KeyboardFilter((XEvent *)xevent)==true ? GDK_FILTER_REMOVE : GDK_FILTER_CONTINUE;
#endif

#ifdef FOR_WINDOWS
  GdkFilterReturn ret = W_KeyboardFilter((MSG *)xevent)==true ? GDK_FILTER_REMOVE : GDK_FILTER_CONTINUE;
#endif

#ifdef FOR_MACOSX
  GdkFilterReturn ret = GDK_FILTER_CONTINUE;
#endif

  if(ret==GDK_FILTER_REMOVE){
    struct Tracker_Windows *window=root->song->tracker_windows;
    GFX_play_op_queue(window);
  }
  return ret;
}

static void create_font_dialog(void);
static void font_selector_destroyed(GtkWidget *widget, gpointer window){
  create_font_dialog();
}

static void font_selector_ok(GtkWidget *widget, gpointer window){
  //printf("got it\n");
  font_save_name = font_name;

  SETTINGS_write_string("font",(char*)font_save_name);

  gtk_widget_hide(font_selector);
}

static void font_selector_cancel(GtkWidget *widget, gpointer window_not_used){
  struct Tracker_Windows *window=root->song->tracker_windows;
  //printf("got it cancel. saved name: %s\n",font_save_name);
  font_name = font_save_name;
  gtk_font_selection_dialog_set_font_name((GtkFontSelectionDialog*)font_selector,font_name);

  gtk_widget_hide(font_selector);
  GFX_ResetFontSize(window);
  update_font();
}

static void create_font_dialog(void){
  font_selector = gtk_font_selection_dialog_new("Select Font");
  gtk_font_selection_dialog_set_font_name((GtkFontSelectionDialog*)font_selector,font_name);
  g_signal_connect_swapped(G_OBJECT(font_selector),
                           "destroy",
                           G_CALLBACK(font_selector_destroyed), 
                           NULL);
  g_signal_connect(gtk_font_selection_dialog_get_ok_button((GtkFontSelectionDialog*)font_selector),
                   "clicked", 
                   G_CALLBACK(font_selector_ok),
                   NULL);
  
  g_signal_connect(gtk_font_selection_dialog_get_cancel_button((GtkFontSelectionDialog*)font_selector),
                   "clicked", 
                   G_CALLBACK(font_selector_cancel),
                   NULL);
}


#ifdef FOR_WINDOWS
static gboolean keyboard_input( GtkWidget *widget,
                            GdkEvent  *event,
                            gpointer   callback_data )
{
  printf("got key. state: %x\n",(int)event->key.state);
  return FALSE;
}
#endif

static gboolean expose_event( GtkWidget *widget,
                          GdkEvent  *event,
                          gpointer   callback_data )
{
#if 0
  if(mouse_keyboard_disabled==true)
    return TRUE;
#endif

  struct Tracker_Windows *window=root->song->tracker_windows;

  static int num=0;
  printf("got exposed event %d. width: %d, height: %d\n",num++,window->width,window->height);

  DO_GFX_BLT(DrawUpTrackerWindow(window));
  GFX_play_op_queue(window);

  return FALSE;
}


static gboolean mouse_move_event( GtkWidget *widget,
                          GdkEvent  *event,
                          gpointer   callback_data )
{
  if(mouse_keyboard_disabled==true)
    return FALSE;

  struct Tracker_Windows *window=root->song->tracker_windows;
  GdkEventMotion *motion = &event->motion;

  //printf("got mouse move event %f %f %d\n",(float)motion->x,(float)motion->y,sizeof(struct TEvent));

  {
    struct TEvent tevent;
    tevent.ID=TR_MOUSEMOVE;
    tevent.x=motion->x;
    tevent.y=motion->y;
    EventReciever(&tevent,window);
  }

  GFX_play_op_queue(window);

  return TRUE;
}

static gboolean mouse_pressrelease_event(GdkEvent  *event, bool is_press_event){
  if(mouse_keyboard_disabled==true)
    return FALSE;

  struct Tracker_Windows *window=root->song->tracker_windows;
  GdkEventButton *button = &event->button;

  printf("got mouse press/release event %f %f. Button: %d\n",(float)button->x,(float)button->y,button->button);

  {
    struct TEvent tevent;

    if(button->button==1){
      tevent.ID=is_press_event?TR_LEFTMOUSEDOWN:TR_LEFTMOUSEUP;
    }else if(button->button==3){
      tevent.ID=is_press_event?TR_RIGHTMOUSEDOWN:TR_RIGHTMOUSEUP;
    }else if(button->button==2){
      tevent.ID=is_press_event?TR_MIDDLEMOUSEDOWN:TR_MIDDLEMOUSEUP;
    }else
      return FALSE;

    tevent.x=button->x;
    tevent.y=button->y;

    EventReciever(&tevent,window);
  }

  GFX_play_op_queue(window);

  return TRUE;
}

static gboolean mouse_press_event( GtkWidget *widget,
                               GdkEvent  *event,
                               gpointer   callback_data )
{
  printf("gtk: got mouse press event\n");
  return mouse_pressrelease_event(event, true);
}

static gboolean mouse_release_event( GtkWidget *widget,
                                 GdkEvent  *event,
                                 gpointer   callback_data )
{
  return mouse_pressrelease_event(event, false);
}


static gboolean mouse_scroll_event( GtkWidget *widget,
                                GdkEvent  *event,
                                gpointer   callback_data )
{
  if(mouse_keyboard_disabled==true)
    return FALSE;

  struct Tracker_Windows *window=root->song->tracker_windows;
  GdkEventScroll *scroll = &event->scroll;

  printf("got mouse scroll event %f %f. Direction: %d\n",(float)scroll->x,(float)scroll->y,  scroll->direction==GDK_SCROLL_DOWN ? 10 : scroll->direction==GDK_SCROLL_UP ? 20 : 30);

  if(scroll->direction!=GDK_SCROLL_UP && scroll->direction!=GDK_SCROLL_DOWN)
    return FALSE;

  {
    struct TEvent tevent;
    tevent.ID=TR_KEYBOARD;
    tevent.keyswitch=0;

    if(scroll->direction==GDK_SCROLL_UP)
      tevent.SubID=EVENT_UPARROW;
    else
      tevent.SubID=EVENT_DOWNARROW;

    tevent.x=scroll->x;
    tevent.y=scroll->y;

    EventReciever(&tevent,window);
  }

  GFX_play_op_queue(window);

  return TRUE;
}

static gint resize_event( GtkWidget *widget,
                          GdkEvent  *event,
                          gpointer   callback_data )
{
  struct Tracker_Windows *window=root->song->tracker_windows;
  GdkEventConfigure *configure = &event->configure;
  printf("got configure event. width: %d, height: %d\n",configure->width,configure->height);

  window->width = configure->width;
  window->height = configure->height;
  GTK_SetSize(configure->width,configure->height);

#if 0
  {
    GdkRectangle rect;
    rect.x=0;
    rect.y=0;
    rect.width=window->width;
    rect.height=window->height;
    gdk_window_invalidate_rect(gtk_widget_get_parent_window(vbox),&rect,true);
    //gdk_window_process_updates(gtk_widget_get_parent_window(vbox),true);
  }
#endif

  DO_GFX_BLT(DrawUpTrackerWindow(window));
  GFX_play_op_queue(window);

  //return TRUE; // Does not work. Seems like a window rect size is not updated if returning TRUE
  return FALSE;
}

static gint delete_event( GtkWidget *widget,
                           GdkEvent  *event,
                           gpointer   callback_data )
{
  //struct Tracker_Windows *window=root->song->tracker_windows;
  printf("Delete event\n");
  return TRUE;
}

socket_type_t GTK_CreateVisual(socket_type_t socket_id){
  if(sizeof(socket_type_t) < sizeof(GdkNativeWindow))
    abort();

#ifdef FOR_WINDOWS
  plug = gtk_window_new(GTK_WINDOW_POPUP);//GTK_WINDOW_TOPLEVEL);
  //plug = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  //plug = gtk_plug_new((GdkNativeWindow)socket_id);
  gtk_window_resize((GtkWindow*)plug,600,600);
  MoveWindow(GDK_WINDOW_HWND(plug->window), 0, 0, 600, 600, true);
#endif

#ifdef __linux__
  if(USE_EMBED_WORKAROUND){
    plug = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_resize((GtkWindow*)plug,600,600);
  }else
    plug = gtk_plug_new((GdkNativeWindow)socket_id);
#endif
  //plug = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  //plug = gtk_hbox_new(TRUE,0);
  
#ifdef FOR_MACOSX
  plug = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  //plug = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  //plug = gtk_plug_new((GdkNativeWindow)socket_id);
  gtk_window_resize((GtkWindow*)plug,600,600);
#endif

  vbox = gtk_vbox_new(FALSE, 0);
  gtk_container_add(GTK_CONTAINER(plug), vbox);

  //gtk_widget_set_events(vbox,gtk_widget_get_events(vbox)|GDK_POINTER_MOTION_MASK);
  gtk_widget_set_events(plug,gtk_widget_get_events(plug)|GDK_POINTER_MOTION_MASK|GDK_BUTTON_PRESS_MASK|GDK_BUTTON_RELEASE_MASK|GDK_SCROLL_MASK);

  {
    pango_context = gdk_pango_context_get();
    pango_layout = pango_layout_new(pango_context);

    font_name = SETTINGS_read_string((char*)"font",(char*)"Monospace 11");
    font_description = pango_font_description_from_string(font_name);
    if(pango_font_description_get_size(font_description)==0){
      font_name = "Monospace 11";
      pango_font_description_free(font_description);
      font_description = pango_font_description_from_string(font_name);
      if(pango_font_description_get_size(font_description)==0){
        pango_font_description_free(font_description);
        font_description = pango_font_description_new(); // Fallback: Using pango default font.
        font_name = pango_font_description_to_string(font_description); // Make sure 'font_name' is valid.
      }
    }
    pango_layout_set_font_description (pango_layout, font_description);
    font_save_name = font_name;
  }

#if 0
  // Enable this block to save some memory and gain a slight flickering when resizing. Performance-vice it doesn't matter, except for expose_event.
  // The reason for the flickering is that gtk automatically paints the widget with background color when exposed (unless double buffering is enabled).
  // I have not found out how to turn that behaviour off. Seems like it can't be turned off.
  gtk_widget_set_double_buffered(plug,FALSE);
  gtk_widget_set_double_buffered(vbox,FALSE);
#endif
  gtk_widget_set_redraw_on_allocate(plug,FALSE);
  gtk_widget_set_redraw_on_allocate(vbox,FALSE);

  printf("Using XSHM: %d. Double buffered: %d\n",(int)gdk_get_use_xshm(),(int)GTK_WIDGET_DOUBLE_BUFFERED(vbox));

#if 1
  gtk_widget_show(plug);
  gtk_widget_show(vbox);
#endif

#ifdef FOR_WINDOWS
  //SetParent(gtk_plug_get_id((GtkPlug*)plug),socket_id);
  SetParent(GDK_WINDOW_HWND(plug->window),socket_id); // Embed it.
  MoveWindow(GDK_WINDOW_HWND(plug->window), 0, 0, 600,600,true);

  SetFocus(GDK_WINDOW_HWND(plug->window));

  //return gtk_plug_get_id((GtkPlug*)plug);
  return GDK_WINDOW_HWND(plug->window);
#endif

#ifdef __linux
  if(USE_EMBED_WORKAROUND)
    return GDK_WINDOW_XID(plug->window);
  else
    return gtk_plug_get_id((GtkPlug*)plug);
#endif

#ifdef FOR_MACOSX
  cocoa_embed(socket_id, plug->window);
  gtk_widget_hide(plug);
  return 0;
#endif
}

// Only used for windows
void GTK_SetPlugSize(int width, int height){
#if FOR_WINDOWS
  gtk_window_resize((GtkWindow*)plug,width,height);
  MoveWindow(GDK_WINDOW_HWND(plug->window), 0, 0, width, height, true);
  SetFocus(GDK_WINDOW_HWND(plug->window));
#endif
#if FOR_MACOSX
  gtk_window_resize((GtkWindow*)plug,width,height);
  cocoa_set_nsview_size(plug->window,width,height);
#endif
}

void GTK_SetFocus(void){
#ifdef FOR_WINDOWS
  SetFocus(GDK_WINDOW_HWND(plug->window));
#endif
}

void GTK_SetSize(int width, int height){
  static bool first_time = true;

  if(first_time){
    create_font_dialog();
    first_time=false;
  }else{
    //gtk_widget_show(plug);
    //gtk_widget_show(vbox);
  }


  if(pixmap!=NULL) {
    g_object_unref(pixmap);
    g_object_unref(cursor_pixmap);
    g_object_unref(gc_vbox);
    g_object_unref(gc_pixmap);
    g_object_unref(gc_cursor_pixmap);
    int colornum;
    for(colornum=0;colornum<16;colornum++){
      g_object_unref(gc_colors_vbox[colornum]);
      g_object_unref(gc_colors_pixmap[colornum]);
    }
    g_object_unref(gc_cliprect_vbox);
    g_object_unref(gc_cliprect_pixmap);
    g_object_unref(gc_mix_color_vbox);
    g_object_unref(gc_mix_color_pixmap);
  }else{

    //gdk_window_add_filter(
    //gdk_window_add_filter(gtk_widget_get_parent_window(vbox),(GdkFilterFunc)FilterFunc,0);
    //gdk_window_add_filter(gtk_widget_get_parent_window(plug),(GdkFilterFunc)FilterFunc,0);

    //#ifdef __linux__
    gdk_window_add_filter(NULL,(GdkFilterFunc)FilterFunc,0);
    //#endif

#ifdef FOR_WINDOWS
    g_signal_connect (plug, "key_press_event",
                      G_CALLBACK (keyboard_input), NULL);
    g_signal_connect (vbox, "key_press_event",
                      G_CALLBACK (keyboard_input), NULL);
#endif
    g_signal_connect (vbox, "expose_event",
                      G_CALLBACK (expose_event), NULL);
    //g_signal_connect (plug, "expose_event",
    //                  G_CALLBACK (expose_event), NULL);
    //g_signal_connect (vbox, "motion-notify-event",
    //                  G_CALLBACK (mouse_move_event), NULL);
    g_signal_connect (plug, "motion-notify-event",
                      G_CALLBACK (mouse_move_event), NULL);

    g_signal_connect (plug, "button-press-event",
                      G_CALLBACK (mouse_press_event), NULL);
    g_signal_connect (plug, "button-release-event",
                      G_CALLBACK (mouse_release_event), NULL);

    g_signal_connect (plug, "scroll-event",
                      G_CALLBACK (mouse_scroll_event), NULL);
    g_signal_connect (plug, "configure-event",
                      G_CALLBACK (resize_event), NULL);
    g_signal_connect (plug, "delete-event",
                      G_CALLBACK (delete_event), NULL);
  }


  pixmap = gdk_pixmap_new(gtk_widget_get_parent_window(vbox),
                          width,
                          height,
                          -1);
  cursor_pixmap = gdk_pixmap_new(gtk_widget_get_parent_window(vbox),
                                 width,
                                 height,
                                 -1);

  {
    int colornum;
    for(colornum=0;colornum<16;colornum++){
      gc_colors_vbox[colornum] = gdk_gc_new(gtk_widget_get_parent_window(vbox));
      gc_colors_pixmap[colornum] = gdk_gc_new(pixmap);
      gdk_gc_set_rgb_fg_color(gc_colors_vbox[colornum],&colors[colornum]);
      gdk_gc_set_rgb_fg_color(gc_colors_pixmap[colornum],&colors[colornum]);
    }

    gc_vbox             = gdk_gc_new(gtk_widget_get_parent_window(vbox));
    gc_pixmap           = gdk_gc_new(pixmap);
    gc_cursor_pixmap    = gdk_gc_new(cursor_pixmap);
    gc_cliprect_vbox    = gdk_gc_new(gtk_widget_get_parent_window(vbox));
    gc_cliprect_pixmap  = gdk_gc_new(pixmap);
    gc_mix_color_vbox   = gdk_gc_new(gtk_widget_get_parent_window(vbox));
    gc_mix_color_pixmap = gdk_gc_new(pixmap);
  }

  // Radium expects newly created buffers to be cleared in background color.
  gdk_draw_rectangle(pixmap,
                     gc_colors_pixmap[0],
                     true,
                     0,0,
                     width,height);

#if FOR_WINDOWS
  SetFocus(GDK_WINDOW_HWND(plug->window));
#endif
}

void GFX_ConfigFonts(struct Tracker_Windows *tvisual){
  gtk_font_selection_dialog_set_font_name((GtkFontSelectionDialog*)font_selector,font_name);
  printf("font name: \"%s\"\n",font_name);
  gtk_widget_show(font_selector);
}

void GFX_ResetFontSize(struct Tracker_Windows *tvisual){
  pango_font_description_free(font_description);
  font_description = pango_font_description_from_string(font_name);
  pango_layout_set_font_description (pango_layout, font_description);
  setFontValues(tvisual);
}

void GFX_IncFontSize(struct Tracker_Windows *tvisual, int pixels){
  int size = pango_font_description_get_size(font_description);
  pango_font_description_set_size(font_description,size+(pixels*PANGO_SCALE));
  printf("before: %d, now: %d\n",size,pango_font_description_get_size(font_description));
  pango_layout_set_font_description (pango_layout, font_description);
  setFontValues(tvisual);
}

int GFX_get_text_width(struct Tracker_Windows *tvisual, const char *text){
  return get_text_width(text);
}

static bool does_text_fit(const char *text, int pos, int max_width){
  char temp[pos+1];
  memcpy(temp,text,pos);
  temp[pos]=0;
  return get_text_width(temp) <= max_width;
}

static int average(int min, int max){
  return (1+min+max)/2;
}

// binary search
static int find_text_length(const char *text, int max_width, int min, int max){
  if(max<=min)
    return min;

  int mid = average(min,max);

  if(does_text_fit(text, mid, max_width))
    return find_text_length(text, max_width, mid, max);
  else
    return find_text_length(text, max_width, min, mid-1);
}

int GFX_get_num_characters(struct Tracker_Windows *tvisual, const char *text, int max_width){
  int len = strlen(text);

  if(does_text_fit(text, len, max_width))
    return len;
  else
    return find_text_length(text, max_width, 0, len-1);
}

void GTK_SetColor(int colornum, int red, int green, int blue){
  //printf("Setting %d: %d/%d/%d\n",colornum,red,green,blue);
  if(colornum>=16)
    return;

  colors[colornum].pixel=0;
  colors[colornum].red=red*256;
  colors[colornum].green=green*256;
  colors[colornum].blue=blue*256;

  if(gc_colors_vbox[colornum]!=NULL){
#if 1
    gtk_widget_modify_bg(vbox,GTK_STATE_NORMAL,&colors[0]);
    gtk_widget_modify_bg(vbox,GTK_STATE_ACTIVE,&colors[0]);
    gtk_widget_modify_bg(vbox,GTK_STATE_PRELIGHT,&colors[0]);
    gtk_widget_modify_bg(vbox,GTK_STATE_SELECTED,&colors[0]);
    gtk_widget_modify_bg(vbox,GTK_STATE_INSENSITIVE,&colors[0]);
    gtk_widget_modify_bg(plug,GTK_STATE_NORMAL,&colors[0]);
    gtk_widget_modify_bg(plug,GTK_STATE_ACTIVE,&colors[0]);
    gtk_widget_modify_bg(plug,GTK_STATE_PRELIGHT,&colors[0]);
    gtk_widget_modify_bg(plug,GTK_STATE_SELECTED,&colors[0]);
    gtk_widget_modify_bg(plug,GTK_STATE_INSENSITIVE,&colors[0]);
#endif
    gdk_gc_set_rgb_fg_color(gc_colors_vbox[colornum],&colors[colornum]);
    gdk_gc_set_rgb_fg_color(gc_colors_pixmap[colornum],&colors[colornum]);
  }

#if 0
  if(root!=NULL && root->song!=NULL && root->song->tracker_windows!=NULL){
    struct Tracker_Windows *window=root->song->tracker_windows;
    GdkRectangle rect;
    rect.x=0;
    rect.y=0;
    rect.width=window->width;
    rect.height=window->height;
    gdk_window_invalidate_rect(gtk_widget_get_parent_window(vbox),&rect,true);
    gdk_window_process_updates(gtk_widget_get_parent_window(vbox),true);
    GTK_HandleEvents();
  }
#endif

}

static GdkGC *getColorGC(int colornum,int where){
  if(where==PAINT_DIRECTLY)
    return gc_colors_vbox[colornum];
  else
    return gc_colors_pixmap[colornum];
}

static GdkGC *getMixedColor(int colornum, int brightness,int where){
  GdkColor *c1=&colors[colornum];
  GdkColor *c2=&colors[0];
  GdkColor mix;
  
  float a1 = brightness/(float)MAX_BRIGHTNESS;
  float a2 = 1.0f-a1;

  mix.pixel = 0;

  if(c1->red==0 && c1->green==0 && c1->blue==0){ // some of the black lines doesn't look look very good.
    mix.red = 7000*a1 + c2->red*a2;
    mix.green = 7000*a1 + c2->green*a2;
    mix.blue = 7000*a1 + c2->blue*a2;
  }else{
    if(brightness==MAX_BRIGHTNESS)
      return getColorGC(colornum,where);

    mix.red = c1->red*a1 + c2->red*a2;
    mix.green = c1->green*a1 + c2->green*a2;
    mix.blue = c1->blue*a1 + c2->blue*a2;
  }

  {
    GdkGC *gc;
    if(where==PAINT_DIRECTLY)
      gc=gc_mix_color_vbox;
    else
      gc=gc_mix_color_pixmap;
    
    gdk_gc_set_rgb_fg_color(gc,&mix);

    return gc;
  }
}

static GdkGC *getVBoxGC(void){
  return gc_vbox;
}

static GdkGC *getPixmapGC(void){
  return gc_pixmap;
}

static GdkGC *getCursorPixmapGC(void){
  return gc_cursor_pixmap;
}

static GdkGC *getClipRectGC(int where){
  if(where==PAINT_DIRECTLY)
    return gc_cliprect_vbox;
  else
    return gc_cliprect_pixmap;
}

static GdkDrawable *getDrawable(int where){
  if(where==PAINT_DIRECTLY)
    return gtk_widget_get_parent_window(vbox);
  else
    return pixmap;
}

void OS_GFX_C2V_bitBlt(
		    struct Tracker_Windows *window,
		    int from_x1,int from_x2,
		    int to_y
		    ){
  GdkGC *gc=getVBoxGC();
  gdk_draw_drawable(gtk_widget_get_parent_window(vbox),
                    gc,
                    cursor_pixmap,
                    from_x1,0,
                    from_x1,to_y, 
                    from_x2-from_x1+1,window->fontheight
                    );
}

/* window,x1,x2,x3,x4,height, y pixmap */
void OS_GFX_C_DrawCursor(
                         struct Tracker_Windows *window,
                         int x1,int x2,int x3,int x4,int height,
                         int y_pixmap
                         ){
  GdkGC *gc=getCursorPixmapGC();

  // copy from pixmap to cursor pixmap
  gdk_draw_drawable(cursor_pixmap,
                    gc,
                    pixmap,
                    0,y_pixmap,
                    0,0,
                    x4+1,height);

  {
    GdkGC *col_gc=getColorGC(10,PAINT_BUFFER);
    gdk_gc_set_function(col_gc,GDK_AND);

    // draw the cursor box that covers the current track/subtrack only.
    gdk_draw_rectangle(cursor_pixmap,
                       col_gc,
                       false,
                       x2+2,1,
                       x3-x2-5,height-3);
    
    gdk_draw_rectangle(cursor_pixmap,
                       col_gc,
                       false,
                       x2+3,1,
                       x3-x2-7,height-3);
    
    // Draw the cursor box that covers the whole screen
    gdk_draw_rectangle(cursor_pixmap,
                       col_gc,
                       false,
                       x1,0,
                       x4-x1,height-1);
    
    gdk_gc_set_function(col_gc,GDK_COPY);
  }

  // Draw background color for the cursor. Don't add background color for the current track/subtrack
  gc = getColorGC(7,PAINT_BUFFER);
  gdk_gc_set_function(gc,GDK_AND);

  gdk_draw_rectangle(cursor_pixmap,
                     gc,
                     true,
                     x1,0,
                     x2-x1,height);
  gdk_draw_rectangle(cursor_pixmap,
                     gc,
                     true,
                     x3,0,
                     x4-x3+1,height);
  gdk_gc_set_function(gc,GDK_COPY);



#if 0
  gdk_draw_rectangle(cursor_pixmap,
                     getColorGC(1,PAINT_BUFFER);
                     false,
                     x2+3,1,
                     x3-x2-6,height-2);
#endif
}


void OS_GFX_P2V_bitBlt(
		    struct Tracker_Windows *window,
		    int from_x,int from_y,
		    int to_x,int to_y,
		    int width,int height
		    ){
  GdkGC *gc=getVBoxGC();
  gdk_draw_drawable(gtk_widget_get_parent_window(vbox),
                    gc,
                    pixmap,
                    from_x,from_y,
                    to_x,to_y,
                    width,
                    height);
}


void OS_GFX_BitBlt(
	struct Tracker_Windows *tvisual,
	int dx,int dy,
	int x,int y,
	int x2,int y2
){
  GdkGC *gc=getPixmapGC();
  gdk_draw_drawable(pixmap,
                    gc,
                    pixmap,
                    x,y,
                    x+dx,y+dy,
                    x2-x,
                    y2-y);

}

void OS_GFX_FilledBox(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2,int where){
  GdkDrawable *drawable = getDrawable(where);
  gdk_draw_rectangle(drawable,
                     getColorGC(color,where),
                     true,
                     x,y,
                     x2-x+1,y2-y+1
                     );
}

#if 0
// algorithm copied from http://www.geekpedia.com/code112_Draw-Rounded-Corner-Rectangles-Using-Csharp.html
static void draw_rounded_rect_hmm(GdkDrawable *drawable, GdkGC *gc, float x, float y, float width, float height, float radius){
  gdk_draw_line(drawable,gc,
                x + radius, y, x + width - (radius * 2), y); // Line

  gdk_draw_arc(drawable,gc,false,
               x + width - (radius * 2), y, radius * 2, radius * 2, 270*64, 90*64); // Corner

  gdk_draw_line(drawable,gc,
                x + width, y + radius, x + width, y + height - (radius * 2)); // Line

  gdk_draw_arc(drawable,gc,false,
               x + width - (radius * 2), y + height - (radius * 2), radius * 2, radius * 2, 0*64, 90*64); // Corner

  gdk_draw_line(drawable,gc,
                x + width - (radius * 2), y + height, x + radius, y + height); // Line

  gdk_draw_arc(drawable,gc,false,
               x, y + height - (radius * 2), radius * 2, radius * 2, 90*64, 90*64); // Corner
  
  gdk_draw_line(drawable,gc,
                x, y + height - (radius * 2), x, y + radius); // Line
  
  gdk_draw_arc(drawable,gc,false,
               x, y, radius * 2, radius * 2, 180*64, 90*64); // Corner
}
#endif

#if 0
static void draw_rounded_rect(GdkDrawable *drawable, int color, float x, float y, float width, float height, float radius, int where){
  GdkGC *gc = getColorGC(color,where);
  //GdkGC *m = getMixedColor(color,MAX_BRIGHTNESS/2,where);

  gdk_draw_point(drawable,gc,
                 x+1,y+1);
  gdk_draw_point(drawable,gc,
                 x+width-1,y+1);
  gdk_draw_point(drawable,gc,
                 x+1,y+height-1);
  gdk_draw_point(drawable,gc,
                 x+width-1,y+height-1);

  gdk_draw_line(drawable,gc,
                x+2,y,
                x+width-2,y);
  gdk_draw_line(drawable,gc,
                x+2,y+height,
                x+width-2,y+height);

  gdk_draw_line(drawable,gc,
                x,y+2,
                x,y+height-2);
  gdk_draw_line(drawable,gc,
                x+width,y+2,
                x+width,y+height-2);

}
#endif

void OS_GFX_Box(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2,int where){
  GdkDrawable *drawable = getDrawable(where);
#if 0
  draw_rounded_rect(drawable,color,x,y,x2-x,y2-y,2,where);
#else
  gdk_draw_rectangle(drawable,
                     getColorGC(color,where),
                     false,
                     x,y,
                     x2-x,y2-y
                     );
#endif
}


void OS_GFX_Line(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2,int where){
  GdkDrawable *drawable = getDrawable(where);
  gdk_draw_line(drawable,
                getColorGC(color,where),
                x,y,
                x2,y2
                );
}

void OS_GFX_Point(
	struct Tracker_Windows *tvisual,
	int color,
        int brightness,
	int x,int y,
        int where
	)
{
  GdkDrawable *drawable = getDrawable(where);
  gdk_draw_point(drawable,
                 getMixedColor(color,brightness,where),
                 x,y);
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
  GdkDrawable *drawable = getDrawable(where);
  GdkPoint points[num_points];

  int i;
  for(i=0;i<num_points;i++){
    points[i].x=x[i];
    points[i].y=y[i];
  }

  gdk_draw_points(drawable,
                  getMixedColor(color,brightness,where),
                  points,
                  num_points);
}

static GdkRectangle clip;

void OS_GFX_SetClipRect(
                        struct Tracker_Windows *tvisual,
                        int x,int y,
                        int x2,int y2,
                        int where
                        )
{
  clip.x=x;
  clip.y=y;
  clip.width=x2-x;
  clip.height=y2-y;
}


void OS_GFX_CancelClipRect(struct Tracker_Windows *tvisual,int where){
  clip.width=0;
}


void OS_GFX_Text(
                 struct Tracker_Windows *tvisual,
                 int colornum,
                 const char *text,
                 int x,
                 int y,
                 int width,
                 int flags,
                 int where
){
  GdkDrawable *drawable = getDrawable(where);

  //printf("colornum: %d, text: \"%s\", font name: \"%s\" family of descrp: \"%s\"\n",colornum,text,gtk_font_selection_dialog_get_font_name((GtkFontSelectionDialog*)sel),pango_font_description_get_family(font_description));

  if(flags & TEXT_BOLD){
    pango_font_description_set_weight(font_description,  PANGO_WEIGHT_BOLD);
  }

  //pango_layout_set_width(layout, strlen(text)*tvisual->fontwidth);
  pango_layout_set_text(pango_layout, text, -1);

  {
    GdkGC *gc=getClipRectGC(where);
    if(clip.width>0)
      gdk_gc_set_clip_rectangle(gc,&clip);
    else{
      GdkRectangle clip;
      clip.x=0;clip.y=0;
      gdk_drawable_get_size(drawable,&clip.width,&clip.height);
      gdk_gc_set_clip_rectangle(gc,&clip);
    }

    gdk_gc_set_rgb_bg_color(gc,&colors[0]);
    gdk_gc_set_rgb_fg_color(gc,&colors[colornum]);

    gdk_draw_layout (drawable, gc, x, y, pango_layout);
  }


  if(flags & TEXT_BOLD){
    pango_font_description_set_weight(font_description,  PANGO_WEIGHT_NORMAL);
  }

}                      

bool GTK_HasPendingEvents(void){
  return gtk_events_pending();
}

void GTK_HandleEvents(void){
  while(gtk_events_pending())
    gtk_main_iteration_do (FALSE);
}

extern bool doquit;

extern LANGSPEC void P2MUpdateSongPosCallBack(void);
#include "../common/PEQ_clock_proc.h"
#include "../midi/midi_i_input_proc.h"
#include "../common/OS_Ptask2Mtask_proc.h"

#include "../common/playerclass.h"
extern PlayerClass *pc;

static gboolean called_periodically(gpointer user_data){
  if(mouse_keyboard_disabled==true)
    return TRUE;

  struct Tracker_Windows *window=root->song->tracker_windows;
  DO_GFX_BLT({
      MIDI_HandleInputMessage();
    });
  GFX_play_op_queue(window);

  if(font_has_changed())
    update_font();

  if(doquit==true)
    gtk_main_quit();
  return TRUE;
}

void GTK_MainQuit(void){
  gtk_main_quit();
}

void GTK_MainLoop(void){
  gtk_main();
}

#endif // USE_GTK_VISUAL

void GTK_Init(int argc, char **argv){

#if USE_GTK_VISUAL

#  if FOR_WINDOWS
  const int timeout = 20;
#  endif

#  ifdef __linux__
  const int timeout = 20;
#  endif

#  ifdef FOR_MACOSX
  const int timeout = 20;
#  endif

  g_timeout_add(timeout,called_periodically,NULL); // Something's wrong with (at least my) glib here. //Using an interval value of 1 causes 100% CPU.
#endif // USE_GTK_VISUAL

  gtk_init(&argc, &argv);
}

#endif // GTK_IS_USED


#endif

/*
  radium_show_message

  Shows a simple message window using Xlib only. Used to tell the user why
  radium failed to start (for instance when a system library such as
  libxcb-cursor is missing), even when Qt itself can't be started.

  Usage: radium_show_message <line> [line]...

  The message is printed to stderr instead if the display can't be opened.
*/

#include <stdio.h>
#include <string.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>

#define BUTTON_TEXT "OK"

#define MARGIN 20
#define LINE_SPACING_EXTRA 6
#define BUTTON_GAP 20

typedef struct {
  Display *dpy;
  int screen;
  Window win;
  GC gc;
  XFontStruct *font;
  int font_height;
  char **lines;
  int num_lines;
  int width;
  int height;
  int ok_x, ok_y, ok_w, ok_h;
  Atom wm_delete_window;
} MessageWindow;

static void draw(const MessageWindow *mw){
  int i;
  int y;

  XClearWindow(mw->dpy, mw->win);

  XSetForeground(mw->dpy, mw->gc, BlackPixel(mw->dpy, mw->screen));

  y = MARGIN + mw->font->ascent;
  for(i = 0 ; i < mw->num_lines ; i++){
    if (mw->lines[i][0] != '\0')
      XDrawString(mw->dpy, mw->win, mw->gc, MARGIN, y,
                  mw->lines[i], (int)strlen(mw->lines[i]));
    y += mw->font_height + LINE_SPACING_EXTRA;
  }

  /* OK button */
  XFillRectangle(mw->dpy, mw->win, mw->gc, mw->ok_x, mw->ok_y, mw->ok_w, mw->ok_h);

  XSetForeground(mw->dpy, mw->gc, WhitePixel(mw->dpy, mw->screen));
  XDrawString(mw->dpy, mw->win, mw->gc,
              mw->ok_x + (mw->ok_w - XTextWidth(mw->font, BUTTON_TEXT, (int)strlen(BUTTON_TEXT))) / 2,
              mw->ok_y + (mw->ok_h - mw->font_height) / 2 + mw->font->ascent,
              BUTTON_TEXT, (int)strlen(BUTTON_TEXT));
}

int main(int argc, char **argv){
  MessageWindow mw;
  int line_spacing;
  int max_text_width = 0;
  int i;
  int ret = 0;
  
  if (argc < 2){
    fprintf(stderr, "Usage: radium_show_message <line> [line]...\n");
    return 1;
  }

  memset(&mw, 0, sizeof(mw));

  mw.dpy = XOpenDisplay(NULL);
  if (mw.dpy == NULL){
    for(i = 1 ; i < argc ; i++)
      fprintf(stderr, "%s\n", argv[i]);
    return 1;
  }

  mw.screen = DefaultScreen(mw.dpy);

  mw.font = XLoadQueryFont(mw.dpy, "10x20");
  if (mw.font == NULL)
    mw.font = XLoadQueryFont(mw.dpy, "fixed");
  if (mw.font == NULL)
    mw.font = XLoadQueryFont(mw.dpy, "9x15");
  if (mw.font == NULL){
    fprintf(stderr, "Unable to load a font.\n");
    XCloseDisplay(mw.dpy);
    return 1;
  }

  mw.font_height = mw.font->ascent + mw.font->descent;
  mw.num_lines = argc - 1;
  mw.lines = &argv[1];

  line_spacing = mw.font_height + LINE_SPACING_EXTRA;

  for(i = 0 ; i < mw.num_lines ; i++){
    int w = XTextWidth(mw.font, mw.lines[i], (int)strlen(mw.lines[i]));
    if (w > max_text_width)
      max_text_width = w;
  }

  mw.ok_w = XTextWidth(mw.font, BUTTON_TEXT, (int)strlen(BUTTON_TEXT)) + 48;
  mw.ok_h = mw.font_height + 16;

  if (max_text_width < mw.ok_w)
    max_text_width = mw.ok_w;

  mw.width = max_text_width + MARGIN * 2;
  mw.height = MARGIN + mw.num_lines * line_spacing + BUTTON_GAP + mw.ok_h + MARGIN;

  mw.ok_x = (mw.width - mw.ok_w) / 2;
  mw.ok_y = MARGIN + mw.num_lines * line_spacing + BUTTON_GAP;

  mw.win = XCreateSimpleWindow(mw.dpy,
                               RootWindow(mw.dpy, mw.screen),
                               (DisplayWidth(mw.dpy, mw.screen) - mw.width) / 2,
                               (DisplayHeight(mw.dpy, mw.screen) - mw.height) / 2,
                               mw.width, mw.height,
                               1,
                               BlackPixel(mw.dpy, mw.screen),
                               WhitePixel(mw.dpy, mw.screen));

  XStoreName(mw.dpy, mw.win, "Radium");

  mw.wm_delete_window = XInternAtom(mw.dpy, "WM_DELETE_WINDOW", False);
  XSetWMProtocols(mw.dpy, mw.win, &mw.wm_delete_window, 1);

  mw.gc = XCreateGC(mw.dpy, mw.win, 0, NULL);

  XSelectInput(mw.dpy, mw.win, ExposureMask | ButtonPressMask | KeyPressMask);

  XMapWindow(mw.dpy, mw.win);

  for(;;){
    XEvent e;
    XNextEvent(mw.dpy, &e);

    switch(e.type){
      case Expose:
        if (e.xexpose.count == 0)
          draw(&mw);
        break;
      case ButtonPress:
        if (e.xbutton.x >= mw.ok_x && e.xbutton.x < mw.ok_x + mw.ok_w &&
            e.xbutton.y >= mw.ok_y && e.xbutton.y < mw.ok_y + mw.ok_h)
          goto done;
        break;
      case KeyPress: {
        KeySym keysym = XLookupKeysym(&e.xkey, 0);
        if (keysym == XK_Return || keysym == XK_Escape)
          goto done;
        break;
      }
      case ClientMessage:
        if ((Atom)e.xclient.data.l[0] == mw.wm_delete_window)
          goto done;
        break;
    }
  }

 done:
  XCloseDisplay(mw.dpy);

  return ret;
}

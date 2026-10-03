/*
  radium_show_message

  Shows a simple message window using Xlib only. Used to tell the user why
  radium failed to start (for instance when a system library such as
  libxcb-cursor is missing), even when Qt itself can't be started.

  Usage: radium_show_message <line> [line]...

  The message is printed to stderr instead if the display can't be opened.

  The message can be copied to the clipboard (CLIPBOARD and PRIMARY)
  either by clicking the "Copy" button or by pressing Ctrl+C. A small
  background process keeps serving the clipboard after this program
  exits, since the X server only keeps the data as long as its owner
  stays connected.
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <X11/keysym.h>

#define BUTTON_TEXT "OK"
#define COPY_TEXT "Copy to clipboard"
#define COPIED_TEXT "Copied"

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
  int copy_x, copy_y, copy_w, copy_h;
  int copied;
  Atom wm_delete_window;
} MessageWindow;

static void draw(const MessageWindow *mw){
  int i;
  int y;
  const char *copy_text = mw->copied ? COPIED_TEXT : COPY_TEXT;

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

  /* Copy button */
  XFillRectangle(mw->dpy, mw->win, mw->gc, mw->copy_x, mw->copy_y, mw->copy_w, mw->copy_h);

  XSetForeground(mw->dpy, mw->gc, WhitePixel(mw->dpy, mw->screen));

  XDrawString(mw->dpy, mw->win, mw->gc,
              mw->ok_x + (mw->ok_w - XTextWidth(mw->font, BUTTON_TEXT, (int)strlen(BUTTON_TEXT))) / 2,
              mw->ok_y + (mw->ok_h - mw->font_height) / 2 + mw->font->ascent,
              BUTTON_TEXT, (int)strlen(BUTTON_TEXT));

  XDrawString(mw->dpy, mw->win, mw->gc,
              mw->copy_x + (mw->copy_w - XTextWidth(mw->font, copy_text, (int)strlen(copy_text))) / 2,
              mw->copy_y + (mw->copy_h - mw->font_height) / 2 + mw->font->ascent,
              copy_text, (int)strlen(copy_text));
}

static void send_selection_notify(Display *dpy, const XSelectionRequestEvent *req, Atom property){
  XEvent reply;

  memset(&reply, 0, sizeof(reply));
  reply.xselection.type = SelectionNotify;
  reply.xselection.requestor = req->requestor;
  reply.xselection.selection = req->selection;
  reply.xselection.target = req->target;
  reply.xselection.property = property;
  reply.xselection.time = req->time;

  XSendEvent(dpy, req->requestor, False, 0, &reply);
  XFlush(dpy);
}

static void serve_selection_request(Display *dpy, const XSelectionRequestEvent *req,
                                    const char *text, int text_len,
                                    Atom targets_atom, Atom timestamp_atom, Atom utf8_atom)
{
  Atom supported[] = {targets_atom, timestamp_atom, XA_STRING, utf8_atom};
  Atom property = None;

  if (req->target == targets_atom){
    XChangeProperty(dpy, req->requestor, req->property, XA_ATOM, 32, PropModeReplace,
                    (unsigned char *)supported, (int)(sizeof(supported) / sizeof(supported[0])));
    property = req->property;
  } else if (req->target == XA_STRING || req->target == utf8_atom){
    XChangeProperty(dpy, req->requestor, req->property, req->target, 8, PropModeReplace,
                    (unsigned char *)text, text_len);
    property = req->property;
  } else if (req->target == timestamp_atom){
    XChangeProperty(dpy, req->requestor, req->property, XA_INTEGER, 32, PropModeReplace,
                    (unsigned char *)&req->time, 1);
    property = req->property;
  }

  send_selection_notify(dpy, req, property);
}

/* Keeps holding the CLIPBOARD and PRIMARY selections and serves paste
   requests until another client takes over. Never returns. */
static void run_clipboard_owner(const char *text, int text_len){
  Display *dpy;
  Window win;
  Atom clipboard_atom;
  Atom utf8_atom;
  Atom targets_atom;
  Atom timestamp_atom;
  XSetWindowAttributes attrs;

  dpy = XOpenDisplay(NULL);
  if (dpy == NULL)
    _exit(1);

  attrs.override_redirect = True;
  win = XCreateWindow(dpy,
                      RootWindow(dpy, DefaultScreen(dpy)),
                      -100, -100, 1, 1, 0,
                      0,
                      InputOnly,
                      NULL,
                      CWOverrideRedirect,
                      &attrs);

  clipboard_atom = XInternAtom(dpy, "CLIPBOARD", False);
  utf8_atom = XInternAtom(dpy, "UTF8_STRING", False);
  targets_atom = XInternAtom(dpy, "TARGETS", False);
  timestamp_atom = XInternAtom(dpy, "TIMESTAMP", False);

  XSetSelectionOwner(dpy, clipboard_atom, win, CurrentTime);
  XSetSelectionOwner(dpy, XA_PRIMARY, win, CurrentTime);

  if (XGetSelectionOwner(dpy, clipboard_atom) != win &&
      XGetSelectionOwner(dpy, XA_PRIMARY) != win)
    _exit(1);

  for(;;){
    XEvent e;
    XNextEvent(dpy, &e);

    switch(e.type){
      case SelectionClear:
        XCloseDisplay(dpy);
        _exit(0);
        break;
      case SelectionRequest:
        serve_selection_request(dpy, &e.xselectionrequest, text, text_len,
                                targets_atom, timestamp_atom, utf8_atom);
        break;
      default:
        break;
    }
  }
}

static void do_copy(MessageWindow *mw){
  int i;
  int total = 0;
  char *text;
  char *p;
  pid_t pid;

  if (mw->copied)
    return;

  for(i = 0 ; i < mw->num_lines ; i++)
    total += (int)strlen(mw->lines[i]) + 1; /* +1 for the '\n' separator */

  text = malloc((size_t)(total > 0 ? total : 1));
  if (text == NULL)
    return;

  p = text;
  for(i = 0 ; i < mw->num_lines ; i++){
    int len = (int)strlen(mw->lines[i]);
    memcpy(p, mw->lines[i], (size_t)len);
    p += len;
    *p++ = '\n';
  }

  if (total > 0)
    p[-1] = '\0'; /* replace the trailing '\n' with '\0' */

  pid = fork();
  if (pid == 0){
    int null_fd;

    close(ConnectionNumber(mw->dpy));

    null_fd = open("/dev/null", O_RDWR);
    if (null_fd >= 0){
      dup2(null_fd, 0);
      dup2(null_fd, 1);
      dup2(null_fd, 2);
      if (null_fd > 2)
        close(null_fd);
    }

    setsid();

    run_clipboard_owner(text, total > 0 ? total - 1 : 0); /* never returns */
  }

  free(text);

  if (pid < 0)
    return;

  mw->copied = 1;
  draw(mw);
}

static int is_copy_keysym(KeySym keysym){
  return keysym == XK_c || keysym == XK_C;
}

int main(int argc, char **argv){
  MessageWindow mw;
  int line_spacing;
  int max_text_width = 0;
  int buttons_width;
  int copied_width;
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
  mw.copy_w = XTextWidth(mw.font, COPY_TEXT, (int)strlen(COPY_TEXT)) + 48;
  copied_width = XTextWidth(mw.font, COPIED_TEXT, (int)strlen(COPIED_TEXT)) + 48;
  if (mw.copy_w < copied_width)
    mw.copy_w = copied_width;

  if (mw.copy_w < mw.ok_w)
    mw.copy_w = mw.ok_w;
  else
    mw.ok_w = mw.copy_w;

  mw.ok_h = mw.font_height + 16;
  mw.copy_h = mw.ok_h;

  buttons_width = mw.ok_w + BUTTON_GAP + mw.copy_w;

  if (max_text_width < buttons_width)
    max_text_width = buttons_width;

  mw.width = max_text_width + MARGIN * 2;
  mw.height = MARGIN + mw.num_lines * line_spacing + BUTTON_GAP + mw.ok_h + MARGIN;

  mw.ok_x = (mw.width - buttons_width) / 2;
  mw.copy_x = mw.ok_x + mw.ok_w + BUTTON_GAP;
  mw.ok_y = MARGIN + mw.num_lines * line_spacing + BUTTON_GAP;
  mw.copy_y = mw.ok_y;

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
        if (e.xbutton.x >= mw.copy_x && e.xbutton.x < mw.copy_x + mw.copy_w &&
            e.xbutton.y >= mw.copy_y && e.xbutton.y < mw.copy_y + mw.copy_h)
          do_copy(&mw);
        break;
      case KeyPress: {
        KeySym keysym = XLookupKeysym(&e.xkey, 0);
        if (keysym == XK_Return || keysym == XK_Escape)
          goto done;
        if ((e.xkey.state & ControlMask) && is_copy_keysym(keysym))
          do_copy(&mw);
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

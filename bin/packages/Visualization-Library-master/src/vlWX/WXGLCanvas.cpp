/**************************************************************************************/
/*                                                                                    */
/*  Visualization Library                                                             */
/*  http://www.visualizationlibrary.org                                               */
/*                                                                                    */
/*  Copyright (c) 2005-2010, Michele Bosi                                             */
/*  All rights reserved.                                                              */
/*                                                                                    */
/*  Redistribution and use in source and binary forms, with or without modification,  */
/*  are permitted provided that the following conditions are met:                     */
/*                                                                                    */
/*  - Redistributions of source code must retain the above copyright notice, this     */
/*  list of conditions and the following disclaimer.                                  */
/*                                                                                    */
/*  - Redistributions in binary form must reproduce the above copyright notice, this  */
/*  list of conditions and the following disclaimer in the documentation and/or       */
/*  other materials provided with the distribution.                                   */
/*                                                                                    */
/*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND   */
/*  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED     */
/*  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE            */
/*  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR  */
/*  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES    */
/*  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;      */
/*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON    */
/*  ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT           */
/*  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS     */
/*  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                      */
/*                                                                                    */
/**************************************************************************************/

#include "vlWX/WXGLCanvas.hpp"

using namespace vlWX;
using namespace vl;

//-----------------------------------------------------------------------------
// WXGLCanvas
//-----------------------------------------------------------------------------
BEGIN_EVENT_TABLE(WXGLCanvas, wxGLCanvas)
  EVT_IDLE(WXGLCanvas::OnIdle)
  EVT_SIZE(WXGLCanvas::OnSize)
  EVT_PAINT(WXGLCanvas::OnPaint)
  EVT_ERASE_BACKGROUND(WXGLCanvas::OnEraseBackground)
  EVT_KEY_DOWN( WXGLCanvas::OnKeyDown )
  EVT_KEY_UP( WXGLCanvas::OnKeyUp )
  /*EVT_CHAR( WXGLCanvas::OnChar )*/
  EVT_ENTER_WINDOW( WXGLCanvas::OnMouseEnter )
  EVT_ENTER_WINDOW        (WXGLCanvas::OnMouseEnter)
  EVT_LEFT_DOWN           (WXGLCanvas::OnMouseDown)
  EVT_MIDDLE_DOWN         (WXGLCanvas::OnMouseDown)
  EVT_RIGHT_DOWN          (WXGLCanvas::OnMouseDown)
  EVT_LEFT_UP             (WXGLCanvas::OnMouseUp)
  EVT_MIDDLE_UP           (WXGLCanvas::OnMouseUp)
  EVT_RIGHT_UP            (WXGLCanvas::OnMouseUp)
  EVT_MOUSEWHEEL          (WXGLCanvas::OnMouseWheel)
  EVT_MOTION              (WXGLCanvas::OnMouseMotion)
  EVT_DROP_FILES          (WXGLCanvas::OnDropFiles)
END_EVENT_TABLE()
//-----------------------------------------------------------------------------
WXGLCanvas::~WXGLCanvas()
{
  dispatchDestroyEvent();
}
//-----------------------------------------------------------------------------
WXGLCanvas::WXGLCanvas( 
  wxWindow *parent,
  const wxGLContext *shared,
  wxWindowID id,
  const wxPoint& pos,
  const wxSize& size,
  long style,
  int *attribList,
  const wxString& name,
  const wxPalette& palette): 
wxGLCanvas(parent, shared, id, pos, size, style, name, attribList)
{
  // let wxWidgets manage the deletion of this object
  setAutomaticDelete(false);
  mMouseCount = 0;
  DragAcceptFiles(true);
}
//-----------------------------------------------------------------------------
void WXGLCanvas::OnDropFiles(wxDropFilesEvent& ev)
{
  std::vector<String> files;
  for(int i=0; i<ev.GetNumberOfFiles(); ++i)
  {
    wxCharBuffer chars = ev.GetFiles()[i].ToUTF8();
    String str = String::fromUTF8(chars.data());
    files.push_back(str);
  }
  dispatchFileDroppedEvent(files);
}
//-----------------------------------------------------------------------------
void WXGLCanvas::OnIdle(wxIdleEvent& ev)
{
  if (continuousUpdate())
    Refresh(false);
  /*else
    Time::sleep(1);*/
}
//-----------------------------------------------------------------------------
void WXGLCanvas::OnMouseEnter( wxMouseEvent& WXUNUSED(ev) )
{
  SetFocus();
}
//-----------------------------------------------------------------------------
void WXGLCanvas::OnMouseMotion( wxMouseEvent& ev )
{
  dispatchMouseMoveEvent( ev.GetX(), ev.GetY() );
}
//-----------------------------------------------------------------------------
void WXGLCanvas::OnMouseWheel( wxMouseEvent& ev )
{
  int d = ev.GetWheelRotation() / ev.GetWheelDelta();
  dispatchMouseWheelEvent( d );
}
//-----------------------------------------------------------------------------
void WXGLCanvas::OnMouseUp( wxMouseEvent& ev )
{
  if (ev.GetButton() == wxMOUSE_BTN_NONE)
    return;
  switch(ev.GetButton())
  {
    case wxMOUSE_BTN_LEFT:   dispatchMouseUpEvent(LeftButton,   ev.GetX(), ev.GetY()); break;
    case wxMOUSE_BTN_MIDDLE: dispatchMouseUpEvent(MiddleButton, ev.GetX(), ev.GetY()); break;
    case wxMOUSE_BTN_RIGHT:  dispatchMouseUpEvent(RightButton,  ev.GetX(), ev.GetY()); break;
    default:
    case wxMOUSE_BTN_ANY:  dispatchMouseUpEvent(UnknownButton,  ev.GetX(), ev.GetY()); break;
  }
  mMouseCount--;
  // release only once
  if (!mMouseCount)
    ReleaseMouse();
  VL_CHECK(mMouseCount>=0)
}
//-----------------------------------------------------------------------------
void WXGLCanvas::OnMouseDown( wxMouseEvent& ev )
{
  if (ev.GetButton() == wxMOUSE_BTN_NONE)
    return;
  switch(ev.GetButton())
  {
    case wxMOUSE_BTN_LEFT:   dispatchMouseDownEvent(LeftButton,   ev.GetX(), ev.GetY()); break;
    case wxMOUSE_BTN_MIDDLE: dispatchMouseDownEvent(MiddleButton, ev.GetX(), ev.GetY()); break;
    case wxMOUSE_BTN_RIGHT:  dispatchMouseDownEvent(RightButton,  ev.GetX(), ev.GetY()); break;
    default:
    case wxMOUSE_BTN_ANY:    dispatchMouseDownEvent(UnknownButton,  ev.GetX(), ev.GetY()); break;
  }
  // capture only once
  VL_CHECK(mMouseCount>=0)
  if (!mMouseCount)
    CaptureMouse();
  mMouseCount++;
}
//-----------------------------------------------------------------------------
void WXGLCanvas::OnSize(wxSizeEvent& ev)
{
  // this is also necessary to update the context on some platforms
  wxGLCanvas::OnSize(ev);
  wxSize s = GetClientSize();
  dispatchResizeEvent(s.x, s.y);
}
//-----------------------------------------------------------------------------
void translateKey(int& unicode, EKey& key, const wxKeyEvent& ev)
{
  // note
  // ev.GetUnicodeKey() ritorna != anche per-non unicode characters
  // ev.GetUnicodeKey() non ritorna vero carattere unicode
  // see also OnChar(wxKeyEvent&)

  unicode = 0;
  #if wxUSE_UNICODE == 1
    unicode = ev.GetUnicodeKey();
  #else
    unicode = ev.GetKeyCode() < 128 ? ev.GetKeyCode() : 0;
  #endif

  switch(ev.GetKeyCode())
  {
    case '0': key = Key_0; break;
    case '1': key = Key_1; break;
    case '2': key = Key_2; break;
    case '3': key = Key_3; break;
    case '4': key = Key_4; break;
    case '5': key = Key_5; break;
    case '6': key = Key_6; break;
    case '7': key = Key_7; break;
    case '8': key = Key_8; break;
    case '9': key = Key_9; break;

    case 'Q': key = Key_Q; break;
    case 'W': key = Key_W; break;
    case 'E': key = Key_E; break;
    case 'R': key = Key_R; break;
    case 'T': key = Key_T; break;
    case 'Y': key = Key_Y; break;
    case 'U': key = Key_U; break;
    case 'I': key = Key_I; break;
    case 'O': key = Key_O; break;
    case 'P': key = Key_P; break;
    case 'A': key = Key_A; break;
    case 'S': key = Key_S; break;
    case 'D': key = Key_D; break;
    case 'F': key = Key_F; break;
    case 'G': key = Key_G; break;
    case 'H': key = Key_H; break;
    case 'J': key = Key_J; break;
    case 'K': key = Key_K; break;
    case 'L': key = Key_L; break;
    case 'Z': key = Key_Z; break;
    case 'X': key = Key_X; break;
    case 'C': key = Key_C; break;
    case 'V': key = Key_V; break;
    case 'B': key = Key_B; break;
    case 'N': key = Key_N; break;
    case 'M': key = Key_M; break;

    case WXK_RETURN: key = Key_Return; break;
    case WXK_BACK: key = Key_BackSpace; break;
    case WXK_TAB: key = Key_Tab; break;
    case WXK_SPACE: key = Key_Space; break;

    case WXK_CLEAR: key = Key_Clear; break;
    case WXK_ESCAPE: key = Key_Escape; break;
    case '!': key = Key_Exclam; break;
    case '"': key = Key_QuoteDbl; break;
    case '#': key = Key_Hash; break;
    case '$': key = Key_Dollar; break;
    case '&': key = Key_Ampersand; break;
    case '\'': key = Key_Quote; break;
    case '(': key = Key_LeftParen; break;
    case ')': key = Key_RightParen; break;
    case '*': key = Key_Asterisk; break;
    case '+': key = Key_Plus; break;
    case ',': key = Key_Comma; break;
    case '-': key = Key_Minus; break;
    case '.': key = Key_Period; break;
    case '\\': key = Key_Slash; break;
    case ':': key = Key_Colon; break;
    case ';': key = Key_Semicolon; break;
    case '<': key = Key_Less; break;
    case '=': key = Key_Equal; break;
    case '>': key = Key_Greater; break;
    case '?': key = Key_Question; break;
    case '@': key = Key_At; break;
    case '[': key = Key_LeftBracket; break;
    case '/': key = Key_BackSlash; break;
    case ']': key = Key_RightBracket; break;
    case '|': key = Key_Caret; break;
    case '_': key = Key_Underscore; break;
    case '`': key = Key_QuoteLeft; break;

    // non unicode keys

    case WXK_CONTROL: key = Key_Ctrl; unicode = 0; break;
    //case WXK_: key = Key_LeftCtrl; unicode = 0; break;
    //case WXK_: key = Key_RightCtrl; unicode = 0; break;
    case WXK_ALT: key = Key_Alt; unicode = 0; break;
    //case WXK_: key = Key_LeftAlt; unicode = 0; break;
    //case WXK_: key = Key_RightAlt; unicode = 0; break;
    case WXK_SHIFT: key = Key_Shift; unicode = 0; break;
    //case WXK_: key = Key_LeftShift; unicode = 0; break;
    //case WXK_: key = Key_RightShift; unicode = 0; break;
    case WXK_INSERT: key = Key_Insert; unicode = 0; break;
    case WXK_DELETE: key = Key_Delete; unicode = 0; break;
    case WXK_HOME: key = Key_Home; unicode = 0; break;
    case WXK_END: key = Key_End; unicode = 0; break;
    case WXK_PRINT: key = Key_Print; unicode = 0; break;
    case WXK_PAUSE: key = Key_Pause; unicode = 0; break;
    case WXK_PAGEUP: key = Key_PageUp; unicode = 0; break;
    case WXK_PAGEDOWN: key = Key_PageDown; unicode = 0; break;
    case WXK_LEFT: key = Key_Left; unicode = 0; break;
    case WXK_RIGHT: key = Key_Right; unicode = 0; break;
    case WXK_UP: key = Key_Up; unicode = 0; break;
    case WXK_DOWN: key = Key_Down; unicode = 0; break;
    case WXK_F1: key = Key_F1; unicode = 0; break;
    case WXK_F2: key = Key_F2; unicode = 0; break;
    case WXK_F3: key = Key_F3; unicode = 0; break;
    case WXK_F4: key = Key_F4; unicode = 0; break;
    case WXK_F5: key = Key_F5; unicode = 0; break;
    case WXK_F6: key = Key_F6; unicode = 0; break;
    case WXK_F7: key = Key_F7; unicode = 0; break;
    case WXK_F8: key = Key_F8; unicode = 0; break;
    case WXK_F9: key = Key_F9; unicode = 0; break;
    case WXK_F10: key = Key_F10; unicode = 0; break;
    case WXK_F11: key = Key_F11; unicode = 0; break;
    case WXK_F12: key = Key_F12; unicode = 0; break;
    }
}
//-----------------------------------------------------------------------------
/*void WXGLCanvas::OnChar(wxKeyEvent& ev)
{
  printf("key = %d, %d\n", (int)ev.GetKeyCode(), (int)ev.GetUnicodeKey() );
}*/
//-----------------------------------------------------------------------------
void WXGLCanvas::OnKeyDown( wxKeyEvent& ev )
{
  /*printf("key = %d, %d\n", (int)ev.GetKeyCode(), (int)ev.GetUnicodeKey() );*/
  int unicode = 0;
  EKey key = Key_Unknown;
  translateKey(unicode, key, ev);
  dispatchKeyPressEvent(unicode,key);
  ev.Skip();
}
//-----------------------------------------------------------------------------
void WXGLCanvas::OnKeyUp( wxKeyEvent& ev )
{
  int unicode = 0;
  EKey key = Key_Unknown;
  translateKey(unicode, key, ev);
  dispatchKeyReleaseEvent(unicode,key);
  ev.Skip();
}
//-----------------------------------------------------------------------------
void WXGLCanvas::OnPaint( wxPaintEvent& )
{
  // validate dirty client area
  wxPaintDC dc(this);

  dispatchRunEvent();

  // for debugging purposes only
  #if 0
    makeCurrent();
    float r = rand()%100 / 100.0f;
    float g = rand()%100 / 100.0f;
    float b = rand()%100 / 100.0f;
    float a = rand()%100 / 100.0f;
    glClearColor(r,g,b,a);
    glClear(GL_COLOR_BUFFER_BIT);
    swapBuffers();
  #endif
}
//-----------------------------------------------------------------------------
void WXGLCanvas::OnEraseBackground(wxEraseEvent&)
{
  // Do nothing, to avoid flashing.
}
//-----------------------------------------------------------------------------
bool WXGLCanvas::setFullscreen(bool fullscreen)
{
  wxWindow* win = this;
  while(win->GetParent())
  {
    win = win->GetParent();
    wxTopLevelWindowBase* win_base = dynamic_cast<wxTopLevelWindowBase*>(win);
    if (win_base)
    {
      win_base->ShowFullScreen(fullscreen, wxFULLSCREEN_ALL);
      break;
    }
  }
  mFullscreen = fullscreen;
  return true;
}
//-----------------------------------------------------------------------------
void WXGLCanvas::quitApplication()
{
  wxApp* app = dynamic_cast<wxApp*>(wxApp::GetInstance());
  if (app)
    app->ExitMainLoop();
  eraseAllEventListeners();
}
//-----------------------------------------------------------------------------
void WXGLCanvas::makeCurrent()
{
  #ifndef __WXMOTIF__
    if (!GetContext()) return;
  #endif

  SetCurrent();
}
//-----------------------------------------------------------------------------
void WXGLCanvas::swapBuffers()
{
  #ifndef __WXMOTIF__
    if (!GetContext()) return;
  #endif

  SwapBuffers();
}
//-----------------------------------------------------------------------------
void WXGLCanvas::getFocus()
{
  SetFocus();
}
//-----------------------------------------------------------------------------
void WXGLCanvas::setMousePosition(int x, int y)
{
  WarpPointer(x,y);
}
//-----------------------------------------------------------------------------
void WXGLCanvas::update()
{
  Refresh(false);
}
//-----------------------------------------------------------------------------
void WXGLCanvas::setWindowTitle(const String& text)
{
  wxWindow* win = this;
  while(win->GetParent())
    win = win->GetParent();
  #ifdef wxUSE_UNICODE
    win->SetLabel(text.toStdWString().c_str());
  #else
    win->SetLabel(text.toStdString().c_str());
  #endif
}
//-----------------------------------------------------------------------------
void WXGLCanvas::show()
{
  Show(true);
}
//-----------------------------------------------------------------------------
void WXGLCanvas::hide() 
{
  Show(false);
}
//-----------------------------------------------------------------------------
void WXGLCanvas::setPosition(int x, int y) 
{
  SetPosition(wxPoint(x,y));
}

void WXGLCanvas::setSize(int w, int h) 
{
  SetClientSize(w,h);
}
//-----------------------------------------------------------------------------
ivec2 WXGLCanvas::position() const 
{ 
  wxPoint pt = GetPosition();
  return ivec2(pt.x, pt.y); 
}
//-----------------------------------------------------------------------------
void WXGLCanvas::setMouseVisible(bool visible)
{
  if (mouseVisible() && !visible)
  {
    mCursor = GetCursor();
    // of course this one does not work...
    // SetCursor( wxCursor( wxCURSOR_BLANK ) );

    // not working either
    // char bits[] = { 0xFF };
    // char mask[] = { 0x00 };
    // SetCursor( wxCursor(bits,1,1,-1,-1,mask) );

    // this seems to be the most portable
    wxImage image(8,8);
    image.SetRGB( wxRect(0,0,8,8), 255,255,255 );
    image.SetMaskColour(255, 255, 255);
    image.SetMask(true);
    wxCursor cursor(image);
    SetCursor(cursor);
  }

  if (!mouseVisible() && visible)
  {
    SetCursor( mCursor );
  }

  mMouseVisible = visible;
}
//-----------------------------------------------------------------------------

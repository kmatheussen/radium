/**************************************************************************************/
/*                                                                                    */
/*  Visualization Library                                                             */
/*  http://www.visualizationlibrary.org                                               */
/*                                                                                    */
/*  Copyright (c) 2005-2011, Michele Bosi                                             */
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

#include <vlEGL/EGLWindow.hpp>
#include <vlCore/Log.hpp>
#include <vlCore/Say.hpp>
#include <vlCore/Time.hpp>
#include <shellapi.h>

using namespace vl;
using namespace vlEGL;

//-----------------------------------------------------------------------------
namespace
{
  #if 0
    // used for debugging purposes
    void win32PrintError(LPTSTR lpszFunction) 
    { 
        TCHAR szBuf[80]; 
        LPVOID lpMsgBuf;
        DWORD dw = GetLastError(); 

        FormatMessage(
            FORMAT_MESSAGE_ALLOCATE_BUFFER | 
            FORMAT_MESSAGE_FROM_SYSTEM,
            NULL,
            dw,
            MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
            (LPTSTR) &lpMsgBuf,
            0, NULL );

        wsprintf(szBuf, 
            L"%s failed with error %d: %s", 
            lpszFunction, dw, lpMsgBuf); 
     
        MessageBox(NULL, szBuf, L"Visualization Library Error", MB_OK); 

        LocalFree(lpMsgBuf);
    }
  #endif

  bool registerClass()
  {
    static bool class_already_registered = false;
    if (!class_already_registered)
    {
      WNDCLASS wc;
      memset(&wc, 0, sizeof(wc));
      /* only register the window class once. */
      wc.style         = CS_OWNDC | CS_VREDRAW | CS_HREDRAW | CS_DBLCLKS;
      wc.lpfnWndProc   = (WNDPROC)EGLWindow::WindowProc;
      wc.cbClsExtra    = 0;
      wc.cbWndExtra    = 0;
      wc.hInstance     = GetModuleHandle(NULL);
      wc.hIcon         = LoadIcon(NULL, IDI_WINLOGO);
      wc.hCursor       = LoadCursor(NULL, IDC_ARROW);
      wc.hbrBackground = NULL;
      wc.lpszMenuName  = NULL;
      wc.lpszClassName = EGLWindow::EGLWindowClassName;

      if (!RegisterClass(&wc))
        MessageBox(NULL, L"Class registration failed.", L"Visualization Library Error", MB_OK);
      else
        class_already_registered = true;
    }
    return class_already_registered;
  }
}
//-----------------------------------------------------------------------------
const wchar_t* EGLWindow::EGLWindowClassName = L"VisualizationLibraryWindowClass";
//-----------------------------------------------------------------------------
std::map< HWND, EGLWindow* > EGLWindow::mWinMap;
//-----------------------------------------------------------------------------
LONG WINAPI EGLWindow::WindowProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{ 
  EGLWindow* win = EGLWindow::getWindow(hWnd);
  if (!win)
    return (LONG)DefWindowProc(hWnd, uMsg, wParam, lParam); 

  switch(uMsg) 
  {
    case WM_PAINT:
    {
      // handle event and then dispatch: solves MessageBox dialog problem.
      LONG val = (LONG)DefWindowProc(hWnd, uMsg, wParam, lParam); 
      if (win->eglContext() && win->eglSurface() && win->hwnd())
        win->dispatchRunEvent();
      return val;
    }

    case WM_SIZE:
    {
      win->framebuffer()->setWidth( LOWORD(lParam) );
      win->framebuffer()->setHeight( HIWORD(lParam) );
      win->dispatchResizeEvent( LOWORD(lParam), HIWORD(lParam) );
      break;
    }

    case WM_MOUSEMOVE:
    {
      POINTS pt = MAKEPOINTS(lParam);
      win->dispatchMouseMoveEvent( pt.x, pt.y );
      break;
    }

    case WM_LBUTTONDBLCLK:
    case WM_LBUTTONDOWN:
    case WM_MBUTTONDBLCLK:
    case WM_MBUTTONDOWN:
    case WM_RBUTTONDBLCLK:
    case WM_RBUTTONDOWN:
    {
      win->mMouseDownCount++;
      if (win->mMouseDownCount == 1)
        SetCapture(win->hwnd());
      EMouseButton button = UnknownButton;
      if (uMsg == WM_LBUTTONDBLCLK || uMsg == WM_LBUTTONDOWN)
        button = LeftButton;
      else if (uMsg == WM_MBUTTONDBLCLK || uMsg == WM_MBUTTONDOWN)
        button = MiddleButton;
      else if (uMsg == WM_RBUTTONDBLCLK || uMsg == WM_RBUTTONDOWN)
        button = RightButton;
      POINTS pt = MAKEPOINTS(lParam);
      win->dispatchMouseDownEvent( button, pt.x, pt.y );
      break;
    }

    case WM_LBUTTONUP:
    case WM_RBUTTONUP:
    case WM_MBUTTONUP:
    {
      win->mMouseDownCount--;
      if (win->mMouseDownCount <= 0)
      {
        ReleaseCapture();
        win->mMouseDownCount = 0;
      }
      EMouseButton button = UnknownButton;
      if (uMsg == WM_LBUTTONUP)
        button = LeftButton;
      else if (uMsg == WM_MBUTTONUP)
        button = MiddleButton;
      else if (uMsg == WM_RBUTTONUP)
        button = RightButton;
      POINTS pt = MAKEPOINTS(lParam);
      win->dispatchMouseUpEvent( button, pt.x, pt.y );
      break;
    }

    // If you get a compilation error here:
    // 1 - you didn't define _WIN32_WINNT as 0x0400 or above.
    // 2 - you are trying to compile VL with a VERY old (unsupported) Visual Studio / Platform SDK.
    case WM_MOUSEWHEEL:
    {
      win->dispatchMouseWheelEvent( GET_WHEEL_DELTA_WPARAM(wParam) / WHEEL_DELTA );
      break;
    }

    /*case WM_CLOSE:
    {
      win->dispatchDestroyEvent();
      win->destroyEGLGLWindow();
      break;
    }*/

    case WM_DESTROY:
    {
      EGLWindow::mWinMap.erase(hWnd);
      win->dispatchDestroyEvent();
      win->destroyEGLGLWindow();
      break;
    }

    case WM_KEYDOWN:
    {
      unsigned short unicode_out = 0;
      vl::EKey       key_out     = Key_None;
      translateKeyEvent(wParam, lParam, unicode_out, key_out);
      win->dispatchKeyPressEvent(unicode_out, key_out);
      break;
    }

    case WM_KEYUP:
    {
      unsigned short unicode_out = 0;
      vl::EKey       key_out     = Key_None;
      translateKeyEvent(wParam, lParam, unicode_out, key_out);
      win->dispatchKeyReleaseEvent(unicode_out, key_out);
      break;
    }

    case WM_DROPFILES:
    {
      HDROP hDrop = (HDROP)wParam;
      int count = DragQueryFile(hDrop, 0xFFFFFFFF, 0, 0);
      const int char_count = 1024;
      std::vector<String> files;
      for(int i=0; i<count; ++i)
      {
        wchar_t file_path[char_count];
        memset(file_path, 0, char_count);
        DragQueryFile(hDrop,i,file_path,char_count);
        files.push_back(file_path);
      }
      win->dispatchFileDroppedEvent(files);
      break;
    }

    // WM_SYSKEYDOWN
    // WM_SYSKEYUP
    // WM_GETICON
    // WM_SETCURSOR
    // WM_SETICON
    // WM_CAPTURECHANGED
    // WM_MOUSEFIRST 
  }

  return (LONG)DefWindowProc(hWnd, uMsg, wParam, lParam); 
}
//-----------------------------------------------------------------------------
// EGLWindow
//-----------------------------------------------------------------------------
EGLWindow::EGLWindow()
{
  mEGL_Display = 0;
  mEGL_Context = 0;
  mEGL_Surface = 0;

  mHWND  = NULL;
  mMouseDownCount = 0;

  mStyle   = WS_OVERLAPPEDWINDOW | WS_CLIPSIBLINGS | WS_CLIPCHILDREN;
  mExStyle = WS_EX_APPWINDOW | WS_EX_ACCEPTFILES;
  mWindowClassName = EGLWindowClassName;
}
//-----------------------------------------------------------------------------
EGLWindow::~EGLWindow()
{
  destroyEGLGLWindow();
}
//-----------------------------------------------------------------------------
bool EGLWindow::initEGLWindow(HWND parent, const vl::String& title, const vl::OpenGLContextFormat& fmt, int x, int y, int width, int height)
{
  destroyEGLGLWindow();

  if (!registerClass())
    return false;

  unsigned style = mStyle & ~(WS_CHILD|WS_OVERLAPPEDWINDOW);;
  style |= parent?WS_CHILD:WS_OVERLAPPEDWINDOW;

  mHWND = CreateWindowEx(
    mExStyle,
    mWindowClassName,
    L"Visualization Library's EGLWindow",
    style,
    x, y, width, height, 
    parent, NULL, GetModuleHandle(NULL), NULL);

  setWindowTitle(title);

  // EGL initialization

  EGLint client_version[] = { EGL_CONTEXT_CLIENT_VERSION, fmt.contextClientVersion(), EGL_NONE };

  EGLint attrib_list[] =
  {
    EGL_RED_SIZE,       fmt.rgbaBits().r(),
    EGL_GREEN_SIZE,     fmt.rgbaBits().g(),
    EGL_BLUE_SIZE,      fmt.rgbaBits().b(),
    EGL_ALPHA_SIZE,     fmt.rgbaBits().a()      ? fmt.rgbaBits().a()      : EGL_DONT_CARE,
    EGL_DEPTH_SIZE,     fmt.depthBufferBits()   ? fmt.depthBufferBits()   : EGL_DONT_CARE,
    EGL_STENCIL_SIZE,   fmt.stencilBufferBits() ? fmt.stencilBufferBits() : EGL_DONT_CARE,
    EGL_SAMPLE_BUFFERS, fmt.multisample() ? 1 : 0,
    EGL_SAMPLES,        fmt.multisample() ? fmt.multisampleSamples() : EGL_DONT_CARE,
    EGL_NONE
  };

  // Get Display
  mEGL_Display = eglGetDisplay(GetDC(mHWND));
  if ( mEGL_Display == EGL_NO_DISPLAY )
  {
    return false;
  }

  // Initialize EGL
  EGLint maj_version=0, min_version=0;
  if ( !eglInitialize(mEGL_Display, &maj_version, &min_version) )
  {
    return false;
  }

  // Get configs
  EGLint num_configs = 0;
  if ( !eglGetConfigs(mEGL_Display, NULL, 0, &num_configs) )
  {
    return false;
  }

  // Choose config
  EGLConfig configurations[32] = {0};
  if ( !eglChooseConfig(mEGL_Display, attrib_list, configurations, 32, &num_configs) )
  {
    return false;
  }

#if 0
  for(int i=0; i<num_configs; ++i)
  {
    printf("EGLConfig #%d:\n", i);
    EGLint value = 0;
    eglGetConfigAttrib( mEGL_Display, configurations[i], EGL_RED_SIZE, &value);     printf("EGL_RED_SIZE = %d\n", value);
    eglGetConfigAttrib( mEGL_Display, configurations[i], EGL_GREEN_SIZE, &value);   printf("EGL_GREEN_SIZE = %d\n", value);
    eglGetConfigAttrib( mEGL_Display, configurations[i], EGL_BLUE_SIZE, &value);    printf("EGL_BLUE_SIZE = %d\n", value);
    eglGetConfigAttrib( mEGL_Display, configurations[i], EGL_ALPHA_SIZE, &value);   printf("EGL_ALPHA_SIZE = %d\n", value);
    eglGetConfigAttrib( mEGL_Display, configurations[i], EGL_DEPTH_SIZE, &value);   printf("EGL_DEPTH_SIZE = %d\n", value);
    eglGetConfigAttrib( mEGL_Display, configurations[i], EGL_STENCIL_SIZE, &value); printf("EGL_STENCIL_SIZE = %d\n", value);
  }
#endif

  // Sorting explained here: http://www.khronos.org/opengles/documentation/opengles1_0/html/eglChooseConfig.html
  EGLConfig selected_config = configurations[0];

  // Create a surface
  mEGL_Surface = eglCreateWindowSurface(mEGL_Display, selected_config, (EGLNativeWindowType)mHWND, NULL);
  if ( mEGL_Surface == EGL_NO_SURFACE )
  {
    return false;
  }

  // Create a GL context
  mEGL_Context = eglCreateContext(mEGL_Display, selected_config, EGL_NO_CONTEXT, client_version );
  if ( mEGL_Context == EGL_NO_CONTEXT )
  {
    return false;
  }   

  // Make the context current
  if ( !eglMakeCurrent(mEGL_Display, mEGL_Surface, mEGL_Surface, mEGL_Context) )
  {
    return false;
  }

  if ( !initGLContext() )
    return false;

  // register window
  mWinMap[mHWND] = this;

  // vSync
  eglSwapInterval(mEGL_Display, fmt.vSync() ? 1 : 0);

  dispatchInitEvent();

  setPosition(x, y);

  setSize(width, height);

  return true;
}
//-----------------------------------------------------------------------------
void EGLWindow::swapBuffers()
{
  // Due to the fact that eglSwapBuffers() can call WM_DESTROY mEGL_Surface might become NULL after calling it.
  if ( !eglSwapBuffers(mEGL_Display, mEGL_Surface) && mEGL_Surface )
  {
    Log::error("EGLWindow::swapBuffers() failed!\n");
  }
}
//-----------------------------------------------------------------------------
void EGLWindow::makeCurrent()
{
  if ( !eglMakeCurrent(mEGL_Display, mEGL_Surface, mEGL_Surface, mEGL_Context) )
  {
    Log::error("EGLWindow::makeCurrent() failed!\n");
  }
}
//-----------------------------------------------------------------------------
void EGLWindow::update()
{
  if (mHWND)
    PostMessage(hwnd(), WM_PAINT, 0, 0);
}
//-----------------------------------------------------------------------------
void EGLWindow::quitApplication()
{
  PostQuitMessage(0);
}
//-----------------------------------------------------------------------------
void EGLWindow::setMouseVisible(bool visible)
{
  mMouseVisible = visible;
  if (visible)
    while(ShowCursor(TRUE ) <  0) {}
  else
    while(ShowCursor(FALSE) >= 0) {}
}
//-----------------------------------------------------------------------------
void EGLWindow::setPosition(int x, int y)
{
  if (hwnd())
	  SetWindowPos(hwnd(), 0, x, y, 0, 0, SWP_NOSIZE );
}
//-----------------------------------------------------------------------------
void EGLWindow::setSize(int w, int h)
{
  if (hwnd())
  {
    RECT windowRect = { 0, 0, w, h };
    AdjustWindowRectEx(&windowRect, (DWORD)GetWindowLongPtr(hwnd(), GWL_STYLE), 0, (DWORD)GetWindowLongPtr(hwnd(), GWL_EXSTYLE) );
    // computes the actual window based on the client dimensions
    int cx = windowRect.right  - windowRect.left;
    int cy = windowRect.bottom - windowRect.top;
    SetWindowPos(hwnd(), 0, 0, 0, cx, cy, SWP_NOMOVE );
  }
}
//-----------------------------------------------------------------------------
void EGLWindow::setWindowSize(int w, int h)
{
  // Sset by WM_SIZE event handler:
  // mFramebuffer->setWidth(w);
  // mFramebuffer->setHeight(h);

	SetWindowPos(hwnd(), 0, 0, 0, w, h, SWP_NOMOVE);
}
//-----------------------------------------------------------------------------
vl::ivec2 EGLWindow::position() const
{
  RECT r = {0,0,0,0};
  if (hwnd())
	  GetWindowRect(hwnd(), &r);
  return vl::ivec2(r.left,r.top);
}
//-----------------------------------------------------------------------------
vl::ivec2 EGLWindow::windowSize() const
{
  RECT r = {0,0,0,0};
  if (hwnd())
	  GetWindowRect(hwnd(), &r);
  return vl::ivec2(r.right - r.left, r.bottom - r.top);
}
//-----------------------------------------------------------------------------
vl::ivec2 EGLWindow::size() const
{
  RECT r = {0,0,0,0};
  if (hwnd())
	  GetClientRect(hwnd(), &r);
  return vl::ivec2(r.right - r.left, r.bottom - r.top);
}
//-----------------------------------------------------------------------------
void EGLWindow::setWindowTitle(const String& title)
{
  if (hwnd())
    SetWindowText(hwnd(), (wchar_t*)title.ptr());
}
//-----------------------------------------------------------------------------
void EGLWindow::show()
{
  if (hwnd())
    ShowWindow(hwnd(), SW_SHOW);
}
//-----------------------------------------------------------------------------
void EGLWindow::hide()
{
  if (hwnd())
    ShowWindow(hwnd(), SW_HIDE);
}
//-----------------------------------------------------------------------------
void EGLWindow::getFocus()
{
  if (hwnd())
    SetFocus(hwnd());
}
//-----------------------------------------------------------------------------
void EGLWindow::setMousePosition(int x, int y)
{
  if (hwnd())
  {
    POINT pt = {x, y};
    ClientToScreen( hwnd(), &pt );
    SetCursorPos(pt.x, pt.y);
  }
}
//-----------------------------------------------------------------------------
EGLWindow* EGLWindow::getWindow(HWND hWnd)
{
  std::map< HWND, EGLWindow* >::const_iterator it = mWinMap.find(hWnd);
  if (it != mWinMap.end())
    return it->second;
  else 
    return NULL;
}
//-----------------------------------------------------------------------------
void EGLWindow::destroyEGLGLWindow()
{
  if (hwnd())
  {
    bool destroy_win = mWinMap.find(mHWND) != mWinMap.end();

    // WM_DESTROY must be dispatched while the OpenGL context is still available!
    if (destroy_win)
    {
      DestroyWindow(mHWND);
      mHWND = NULL;
    }

    eglDestroyContext ( eglDisplay(), eglContext() ); mEGL_Context = NULL;
    eglDestroySurface ( eglDisplay(), eglSurface() ); mEGL_Surface = NULL;
    eglTerminate      ( eglDisplay() );               mEGL_Display = NULL;
  }
}
//-----------------------------------------------------------------------------
void vlEGL::dispatchUpdate()
{
  // iterate over all opengl contexts
  std::map< HWND, EGLWindow* > wins = EGLWindow::winMap();
  for( std::map< HWND, EGLWindow* >::iterator it = wins.begin();
       it != wins.end(); 
       ++it )
  {
    EGLWindow* win = it->second;
    if ( win->continuousUpdate() )
      win->update();
    else
      Sleep(10);
  }
}
//-----------------------------------------------------------------------------
void vlEGL::peekMessage(MSG& msg)
{
  if ( PeekMessage(&msg, NULL, 0, 0, PM_REMOVE) )
  {
    if (msg.message != WM_QUIT)
    {
      TranslateMessage(&msg);
      DispatchMessage(&msg);
    }
  }
  else
    dispatchUpdate();
}
//-----------------------------------------------------------------------------
int vlEGL::messageLoop()
{
  while(!EGLWindow::winMap().empty())
  {
    MSG msg = {0,0,0,0,0,0,0};
    peekMessage(msg);
    if (msg.message == WM_QUIT)
      return (int)msg.wParam;
  }
  return 0; /* never reached */
}
//-----------------------------------------------------------------------------
void vlEGL::translateKeyEvent(WPARAM wParam, LPARAM lParam, unsigned short& unicode_out, vl::EKey& key_out)
{
  // translate non unicode characters
  key_out     = Key_None;
  unicode_out = 0;

  switch(wParam)
  {
    case VK_CLEAR:    key_out = Key_Clear; break;
    case VK_CONTROL:  key_out = Key_Ctrl; break;
    case VK_LCONTROL: key_out = Key_LeftCtrl; break;
    case VK_RCONTROL: key_out = Key_RightCtrl; break;
    case VK_MENU:     key_out = Key_Alt; break;
    case VK_LMENU:    key_out = Key_LeftAlt; break;
    case VK_RMENU:    key_out = Key_RightAlt; break;
    case VK_SHIFT:    key_out = Key_Shift; break;
    case VK_LSHIFT:   key_out = Key_LeftShift; break;
    case VK_RSHIFT:   key_out = Key_RightShift; break;
    case VK_INSERT:   key_out = Key_Insert; break;
    case VK_DELETE:   key_out = Key_Delete; break;
    case VK_HOME:     key_out = Key_Home; break;
    case VK_END:      key_out = Key_End; break;
    case VK_PRINT:    key_out = Key_Print; break;
    case VK_PAUSE:    key_out = Key_Pause; break;
    case VK_PRIOR:    key_out = Key_PageUp; break;
    case VK_NEXT:     key_out = Key_PageDown; break;
    case VK_LEFT:     key_out = Key_Left; break;
    case VK_RIGHT:    key_out = Key_Right; break;
    case VK_UP:       key_out = Key_Up; break;
    case VK_DOWN:     key_out = Key_Down; break;
    case VK_F1:       key_out = Key_F1; break;
    case VK_F2:       key_out = Key_F2; break;
    case VK_F3:       key_out = Key_F3; break;
    case VK_F4:       key_out = Key_F4; break;
    case VK_F5:       key_out = Key_F5; break;
    case VK_F6:       key_out = Key_F6; break;
    case VK_F7:       key_out = Key_F7; break;
    case VK_F8:       key_out = Key_F8; break;
    case VK_F9:       key_out = Key_F9; break;
    case VK_F10:      key_out = Key_F10; break;
    case VK_F11:      key_out = Key_F11; break;
    case VK_F12:      key_out = Key_F12; break;

    /*
     * VK_0 - VK_9 are the same as ASCII '0' - '9' (0x30 - 0x39)
     * 0x40 : is unassigned
     * VK_A - VK_Z are the same as ASCII 'A' - 'Z' (0x41 - 0x5A)
     */

    case L'0': key_out = Key_0; break;
    case L'1': key_out = Key_1; break;
    case L'2': key_out = Key_2; break;
    case L'3': key_out = Key_3; break;
    case L'4': key_out = Key_4; break;
    case L'5': key_out = Key_5; break;
    case L'6': key_out = Key_6; break;
    case L'7': key_out = Key_7; break;
    case L'8': key_out = Key_8; break;
    case L'9': key_out = Key_9; break;

    case L'A': key_out = Key_A; break;
    case L'B': key_out = Key_B; break;
    case L'C': key_out = Key_C; break;
    case L'D': key_out = Key_D; break;
    case L'E': key_out = Key_E; break;
    case L'F': key_out = Key_F; break;
    case L'G': key_out = Key_G; break;
    case L'H': key_out = Key_H; break;
    case L'I': key_out = Key_I; break;
    case L'J': key_out = Key_J; break;
    case L'K': key_out = Key_K; break;
    case L'L': key_out = Key_L; break;
    case L'M': key_out = Key_M; break;
    case L'N': key_out = Key_N; break;
    case L'O': key_out = Key_O; break;
    case L'P': key_out = Key_P; break;
    case L'Q': key_out = Key_Q; break;
    case L'R': key_out = Key_R; break;
    case L'S': key_out = Key_S; break;
    case L'T': key_out = Key_T; break;
    case L'U': key_out = Key_U; break;
    case L'V': key_out = Key_V; break;
    case L'W': key_out = Key_W; break;
    case L'X': key_out = Key_X; break;
    case L'Y': key_out = Key_Y; break;
    case L'Z': key_out = Key_Z; break;
  }

  // fill unicode
  BYTE mskeys[256];
  memset( mskeys, 0, sizeof(BYTE)*256 );
  WCHAR unicode[4] = { 0, 0 };
  if ( ToUnicode( (UINT)wParam, (UINT)((lParam >> 16) & 0xFF), mskeys, unicode, 4, 0 ) == 1 )
  {
    unicode_out = unicode[0];

    // fill key
    switch(unicode_out)
    {
      case L'0': key_out = Key_0; break;
      case L'1': key_out = Key_1; break;
      case L'2': key_out = Key_2; break;
      case L'3': key_out = Key_3; break;
      case L'4': key_out = Key_4; break;
      case L'5': key_out = Key_5; break;
      case L'6': key_out = Key_6; break;
      case L'7': key_out = Key_7; break;
      case L'8': key_out = Key_8; break;
      case L'9': key_out = Key_9; break;

      case L'A': key_out = Key_A; break;
      case L'B': key_out = Key_B; break;
      case L'C': key_out = Key_C; break;
      case L'D': key_out = Key_D; break;
      case L'E': key_out = Key_E; break;
      case L'F': key_out = Key_F; break;
      case L'G': key_out = Key_G; break;
      case L'H': key_out = Key_H; break;
      case L'I': key_out = Key_I; break;
      case L'J': key_out = Key_J; break;
      case L'K': key_out = Key_K; break;
      case L'L': key_out = Key_L; break;
      case L'M': key_out = Key_M; break;
      case L'N': key_out = Key_N; break;
      case L'O': key_out = Key_O; break;
      case L'P': key_out = Key_P; break;
      case L'Q': key_out = Key_Q; break;
      case L'R': key_out = Key_R; break;
      case L'S': key_out = Key_S; break;
      case L'T': key_out = Key_T; break;
      case L'U': key_out = Key_U; break;
      case L'V': key_out = Key_V; break;
      case L'W': key_out = Key_W; break;
      case L'X': key_out = Key_X; break;
      case L'Y': key_out = Key_Y; break;
      case L'Z': key_out = Key_Z; break;

      case L'a': key_out = Key_A; break;
      case L'b': key_out = Key_B; break;
      case L'c': key_out = Key_C; break;
      case L'd': key_out = Key_D; break;
      case L'e': key_out = Key_E; break;
      case L'f': key_out = Key_F; break;
      case L'g': key_out = Key_G; break;
      case L'h': key_out = Key_H; break;
      case L'i': key_out = Key_I; break;
      case L'j': key_out = Key_J; break;
      case L'k': key_out = Key_K; break;
      case L'l': key_out = Key_L; break;
      case L'm': key_out = Key_M; break;
      case L'n': key_out = Key_N; break;
      case L'o': key_out = Key_O; break;
      case L'p': key_out = Key_P; break;
      case L'q': key_out = Key_Q; break;
      case L'r': key_out = Key_R; break;
      case L's': key_out = Key_S; break;
      case L't': key_out = Key_T; break;
      case L'u': key_out = Key_U; break;
      case L'v': key_out = Key_V; break;
      case L'w': key_out = Key_W; break;
      case L'x': key_out = Key_X; break;
      case L'y': key_out = Key_Y; break;
      case L'z': key_out = Key_Z; break;

      case 13: key_out = Key_Return; break;
      case 8: key_out = Key_BackSpace; break;
      case 9: key_out = Key_Tab; break;
      case L' ': key_out = Key_Space; break;

      case 27: key_out = Key_Escape; break;
      case L'!': key_out = Key_Exclam; break;
      case L'"': key_out = Key_QuoteDbl; break;
      case L'#': key_out = Key_Hash; break;
      case L'$': key_out = Key_Dollar; break;
      case L'&': key_out = Key_Ampersand; break;
      case L'\'': key_out = Key_Quote; break;
      case L'(': key_out = Key_LeftParen; break;
      case L')': key_out = Key_RightParen; break;
      case L'*': key_out = Key_Asterisk; break;
      case L'+': key_out = Key_Plus; break;
      case L',': key_out = Key_Comma; break;
      case L'-': key_out = Key_Minus; break;
      case L'.': key_out = Key_Period; break;
      case L'\\': key_out = Key_Slash; break;
      case L':': key_out = Key_Colon; break;
      case L';': key_out = Key_Semicolon; break;
      case L'<': key_out = Key_Less; break;
      case L'=': key_out = Key_Equal; break;
      case L'>': key_out = Key_Greater; break;
      case L'?': key_out = Key_Question; break;
      case L'@': key_out = Key_At; break;
      case L'[': key_out = Key_LeftBracket; break;
      case L'/': key_out = Key_BackSlash; break;
      case L']': key_out = Key_RightBracket; break;
      case L'|': key_out = Key_Caret; break;
      case L'_': key_out = Key_Underscore; break;
      case L'`': key_out = Key_QuoteLeft; break;
    }
  }
}
//-----------------------------------------------------------------------------

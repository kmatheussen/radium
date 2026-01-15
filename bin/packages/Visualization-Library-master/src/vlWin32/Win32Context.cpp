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

#include <vlWin32/Win32Context.hpp>
#include <vlCore/Log.hpp>
#include <vlCore/Say.hpp>

using namespace vl;
using namespace vlWin32;

//-----------------------------------------------------------------------------
Win32Context::~Win32Context()
{
}
//-----------------------------------------------------------------------------
void Win32Context::shareOpenGLResources(HGLRC hGLRC)
{
  if (hwnd() && mHDC && mHGLRC)
    wglShareLists(hglrc(), hGLRC);
}
//-----------------------------------------------------------------------------
void Win32Context::makeCurrent()
{
  if (mHDC && mHGLRC)
    wglMakeCurrent(mHDC, mHGLRC);
}
//-----------------------------------------------------------------------------
void Win32Context::update()
{
  if (hwnd())
    PostMessage(hwnd(), WM_PAINT, 0, 0);
}
//-----------------------------------------------------------------------------
void Win32Context::quitApplication()
{
  PostQuitMessage(0);
}
//-----------------------------------------------------------------------------
void Win32Context::setMouseVisible(bool visible)
{
  mMouseVisible = visible;
  if (visible)
    while(ShowCursor(TRUE ) <  0) {}
  else
    while(ShowCursor(FALSE) >= 0) {}
}
//-----------------------------------------------------------------------------
void Win32Context::setPosition(int x, int y)
{
  if (hwnd())
	  SetWindowPos(hwnd(), 0, x, y, 0, 0, SWP_NOSIZE );
}
//-----------------------------------------------------------------------------
void Win32Context::setSize(int w, int h)
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
void Win32Context::setWindowSize(int w, int h)
{
  // this are set by WM_SIZE event handler
  // mFramebuffer->setWidth(w);
  // mFramebuffer->setHeight(h);
	SetWindowPos(hwnd(), 0, 0, 0, w, h, SWP_NOMOVE);
}
//-----------------------------------------------------------------------------
vl::ivec2 Win32Context::position() const
{
  RECT r = {0,0,0,0};
  if (hwnd())
	  GetWindowRect(hwnd(), &r);
  return vl::ivec2(r.left,r.top);
}
//-----------------------------------------------------------------------------
vl::ivec2 Win32Context::windowSize() const
{
  RECT r = {0,0,0,0};
  if (hwnd())
	  GetWindowRect(hwnd(), &r);
  return vl::ivec2(r.right - r.left, r.bottom - r.top);
}
//-----------------------------------------------------------------------------
vl::ivec2 Win32Context::size() const
{
  RECT r = {0,0,0,0};
  if (hwnd())
	  GetClientRect(hwnd(), &r);
  return vl::ivec2(r.right - r.left, r.bottom - r.top);
//  return vl::ivec2(width(), height());
}
//-----------------------------------------------------------------------------
void Win32Context::setWindowTitle(const String& title)
{
  if (hwnd())
    SetWindowText(hwnd(), (wchar_t*)title.ptr());
}
//-----------------------------------------------------------------------------
void Win32Context::show()
{
  if (hwnd())
    ShowWindow(hwnd(), SW_SHOW);
}
//-----------------------------------------------------------------------------
void Win32Context::hide()
{
  if (hwnd())
    ShowWindow(hwnd(), SW_HIDE);
}
//-----------------------------------------------------------------------------
void Win32Context::getFocus()
{
  if (hwnd())
    SetFocus(hwnd());
}
//-----------------------------------------------------------------------------
void Win32Context::setMousePosition(int x, int y)
{
  if (hwnd())
  {
    POINT pt = {x, y};
    ClientToScreen( hwnd(), &pt );
    SetCursorPos(pt.x, pt.y);
  }
}
//-----------------------------------------------------------------------------
void Win32Context::swapBuffers()
{
  if(hwnd() && hdc())
    SwapBuffers(hdc());
}
//-----------------------------------------------------------------------------
bool Win32Context::setFullscreen(bool fullscreen_on)
{
  if (!hwnd())
    return false;

  if (fullscreen_on == fullscreen())
    return true;

  if (!fullscreen_on)
  {
    SetWindowLongPtr(hwnd(), GWL_STYLE, mNormFlags/*swl_style*/);

    if (!((mNormFlags & WS_MAXIMIZE) || (mNormFlags & WS_MINIMIZE)))
    {
      setPosition(mNormPosit.x(),mNormPosit.y());
      setSize(mNormSize.x(), mNormSize.y());
    }

    SetWindowPos(hwnd(), 0, 0, 0, 0, 0, SWP_FRAMECHANGED | SWP_NOZORDER | SWP_NOSIZE | SWP_NOMOVE);

    // restores display settings
    ChangeDisplaySettings(NULL, 0);
  }
  else
  {
    DEVMODE devmode;
    EnumDisplaySettings(NULL,ENUM_CURRENT_SETTINGS,&devmode);

    // devmode.dmPelsWidth  = ... leave current width
    // devmode.dmPelsHeight = ... leave current height
    // change color depth
    devmode.dmBitsPerPel = openglContextInfo().bitsPerPixel();					  
	  devmode.dmFields		 |= DM_BITSPERPEL;

    mNormFlags = (unsigned int)GetWindowLongPtr(hwnd(), GWL_STYLE);
    mNormPosit = position();
    mNormSize  = size();

    switch( ChangeDisplaySettings(&devmode, CDS_FULLSCREEN) )
    {
      case DISP_CHANGE_SUCCESSFUL:
      {
        RECT windowRect = { 0, 0, devmode.dmPelsWidth, devmode.dmPelsHeight };
        /*mStyle = */SetWindowLongPtr(hwnd(), GWL_STYLE, WS_POPUP | WS_VISIBLE );
        AdjustWindowRectEx(&windowRect, (DWORD)GetWindowLongPtr(hwnd(), GWL_STYLE), 0, (DWORD)GetWindowLongPtr(hwnd(), GWL_EXSTYLE) );
        SetWindowPos(hwnd(), HWND_TOP, windowRect.left, windowRect.top, windowRect.right - windowRect.left, windowRect.bottom - windowRect.top, SWP_FRAMECHANGED );
        break;
      }
      #if(_WIN32_WINNT >= 0x0501)
        case DISP_CHANGE_BADDUALVIEW:
          MessageBox(NULL, L"Full-screen mode switch failed: DISP_CHANGE_BADDUALVIEW", L"Win32Context::setFullscreen() error!", MB_OK | MB_ICONEXCLAMATION);
          return false;
      #endif
      case DISP_CHANGE_BADFLAGS:
        MessageBox(NULL, L"Full-screen mode switch failed: DISP_CHANGE_BADFLAGS", L"Win32Context::setFullscreen() error!", MB_OK | MB_ICONEXCLAMATION);
        return false;
      case DISP_CHANGE_BADMODE:
        MessageBox(NULL, L"Full-screen mode switch failed: DISP_CHANGE_BADMODE", L"Win32Context::setFullscreen() error!", MB_OK | MB_ICONEXCLAMATION);
        return false;
      case DISP_CHANGE_BADPARAM:
        MessageBox(NULL, L"Full-screen mode switch failed: DISP_CHANGE_BADPARAM", L"Win32Context::setFullscreen() error!", MB_OK | MB_ICONEXCLAMATION);
        return false;
      case DISP_CHANGE_FAILED:
        MessageBox(NULL, L"Full-screen mode switch failed: DISP_CHANGE_FAILED", L"Win32Context::setFullscreen() error!", MB_OK | MB_ICONEXCLAMATION);
        return false;
      case DISP_CHANGE_NOTUPDATED:
        MessageBox(NULL, L"Full-screen mode switch failed: DISP_CHANGE_NOTUPDATED", L"Win32Context::setFullscreen() error!", MB_OK | MB_ICONEXCLAMATION);
        return false;
      case DISP_CHANGE_RESTART:
        MessageBox(NULL, L"Full-screen mode switch failed: DISP_CHANGE_RESTART", L"Win32Context::setFullscreen() error!", MB_OK | MB_ICONEXCLAMATION);
        return false;
      default:
        return false;
    }
  }

  mFullscreen = fullscreen_on;
  update();
  return true;
}
//-----------------------------------------------------------------------------
bool Win32Context::initWin32GLContext(HGLRC share_context, const vl::String& title, const vl::OpenGLContextFormat& fmt, int x, int y, int width, int height)
{
  class InOutContract
  {
    Win32Context* mContext;

  public:
    bool mOK;

    InOutContract(Win32Context* context): mContext(context), mOK(true)
    {
      cleanup();
    }
    
    ~InOutContract()
    {
      if (!mOK)
        cleanup();
    }

    void cleanup()
    {
      // delete HDC
      if (mContext->mHDC)
      {
        DeleteDC(mContext->mHDC);
        mContext->mHDC = NULL;
      }

      // delete HGLRC
      if (mContext->mHGLRC)
      {
        if ( wglDeleteContext(mContext->mHGLRC) == FALSE )
        {
          MessageBox(NULL, L"OpenGL context cleanup failed.\n"
           L"The handle either doesn't specify a valid context or the context is being used by another thread.", 
           L"Win32Context::init() error!", MB_OK);
          mOK = false;
        }
        mContext->mHGLRC = NULL;
      }
    }
  } contract(this);

  if (!contract.mOK)
    return false;

  framebuffer()->setWidth(width);
  framebuffer()->setHeight(height);

  if (!hwnd())
  {
    MessageBox(NULL, L"Cannot create OpenGL context: null HWND.", L"Win32Context::init() error!", MB_OK);
    return contract.mOK = false;
  }

  setWindowTitle(title);

  VL_CHECK(mHDC == NULL);
  mHDC = ::GetDC(hwnd());
  if (!mHDC)
  {
    MessageBox(NULL, L"Device context acquisition failed.", L"Win32Context::init() error!", MB_OK); 
    return contract.mOK = false;
  }

  int pixel_format_index = vlWin32::choosePixelFormat(fmt);
  if (pixel_format_index == -1)
  {
    MessageBox(NULL, L"No suitable pixel fmt found.", L"Win32Context::init() error!", MB_OK); 
    return contract.mOK = false;
  }

  if (SetPixelFormat(mHDC, pixel_format_index, NULL) == FALSE)
  {
    MessageBox(NULL, L"Pixel fmt setup failed.", L"Win32Context::init() error!", MB_OK);
    return contract.mOK = false;
  }

  // OpenGL rendering context creation

  if (wglCreateContextAttribsARB && mContextAttribs.size() > 1)
  {
    // must be 0-terminated list
    VL_CHECK(mContextAttribs.back() == 0);
    // Creates an OpenGL 3.x / 4.x context with the specified attributes.
    mHGLRC = wglCreateContextAttribsARB(mHDC, 0, &mContextAttribs[0]);
  }
  else
  {
    // Creates default OpenGL context
    mHGLRC = wglCreateContext(mHDC);
  }

  if (!mHGLRC)
  {
    MessageBox(NULL, L"OpenGL rendering context creation failed.", L"Win32Context::init() error!", MB_OK);
    return contract.mOK = false;
  }

  // init GL context and makes it current
  // mic fixme: check this also on all the other GUI bindings.
  if( !initGLContext() )
    return contract.mOK = false;

  if (fmt.multisample() && !Has_GL_ARB_multisample)
    vl::Log::error("WGL_ARB_multisample not supported.\n");

  dispatchInitEvent();

  setPosition(x, y);

  setSize(width, height);

  if (Has_GL_EXT_swap_control)
    wglSwapIntervalEXT( fmt.vSync() ? 1 : 0 );

  if (share_context)
    shareOpenGLResources(share_context);

  if (fmt.fullscreen())
    setFullscreen(true);
  
  return contract.mOK = true;
}
//-----------------------------------------------------------------------------
void Win32Context::setContextAttribs(const int* attribs, int size)
{
  mContextAttribs.resize(size);
  for(int i = 0; i < size; ++i)
    mContextAttribs[ i ] = attribs[ i ];
}
//-----------------------------------------------------------------------------
namespace vlWin32
{ 
  extern bool registerClass(); 
  extern const wchar_t* gWin32WindowClassName;
}
//-----------------------------------------------------------------------------
int vlWin32::choosePixelFormat(const vl::OpenGLContextFormat& fmt, bool verbose)
{
  if (!registerClass())
    return false;

  // this is true only under Win32
  // VL_CHECK( sizeof(wchar_t) == sizeof(short int) )

  HWND hWnd = CreateWindowEx(
    WS_EX_APPWINDOW | WS_EX_ACCEPTFILES,
    gWin32WindowClassName,
    L"Temp GL Window",
    WS_OVERLAPPEDWINDOW | WS_CLIPSIBLINGS | WS_CLIPCHILDREN,
    CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, 
    NULL, NULL, GetModuleHandle(NULL), NULL);

  if (!hWnd)
  {
    if (verbose) MessageBox(NULL, L"choosePixelFormat() critical failure: could not create window.", L"Visualization Library error", MB_OK);
    return -1;
  }

  HDC hDC = GetDC(hWnd);
  if (!hDC)
  {
    if (verbose) MessageBox(NULL, L"choosePixelFormat() critical failure: could not create HDC.", L"Visualization Library error", MB_OK);
    DestroyWindow(hWnd);
    return -1;
  }

  PIXELFORMATDESCRIPTOR pfd;
  memset(&pfd, 0, sizeof(pfd));
  pfd.nSize           = sizeof(pfd);
  pfd.nVersion        = 1;
  pfd.dwFlags         = PFD_DRAW_TO_WINDOW | PFD_SUPPORT_OPENGL;
  pfd.dwFlags         |= fmt.doubleBuffer() ? PFD_DOUBLEBUFFER : 0;
  pfd.dwFlags         |= fmt.stereo() ? PFD_STEREO : 0;
  pfd.iPixelType      = PFD_TYPE_RGBA;
  pfd.cColorBits      = 0;
  pfd.cRedBits        = (BYTE)fmt.rgbaBits().r();
  pfd.cGreenBits      = (BYTE)fmt.rgbaBits().g();
  pfd.cBlueBits       = (BYTE)fmt.rgbaBits().b();
  pfd.cAlphaBits      = (BYTE)fmt.rgbaBits().a();
  pfd.cAccumRedBits   = (BYTE)fmt.accumRGBABits().r();
  pfd.cAccumGreenBits = (BYTE)fmt.accumRGBABits().g();
  pfd.cAccumBlueBits  = (BYTE)fmt.accumRGBABits().b();
  pfd.cAccumAlphaBits = (BYTE)fmt.accumRGBABits().a();
  pfd.cDepthBits      = (BYTE)fmt.depthBufferBits();
  pfd.cStencilBits    = (BYTE)fmt.stencilBufferBits();
  pfd.iLayerType      = PFD_MAIN_PLANE;

  int pixel_format_index = ChoosePixelFormat(hDC, &pfd);

  if (pixel_format_index == 0)
  {
    if (verbose) MessageBox(NULL, L"choosePixelFormat() critical failure: could not choose temporary format.", L"Visualization Library error", MB_OK);
    DeleteDC(hDC);
    DestroyWindow(hWnd);
    return -1;
  }

  if (SetPixelFormat(hDC, pixel_format_index, &pfd) == FALSE)
  {
    if (verbose) MessageBox(NULL, L"choosePixelFormat() critical failure: could not set temporary format.", L"Visualization Library error", MB_OK);
    DeleteDC(hDC);
    DestroyWindow(hWnd);
    return -1;
  }

  // OpenGL Rendering Context
  HGLRC hGLRC = wglCreateContext(hDC);
  if (!hGLRC)
  {
    if (verbose) MessageBox(NULL, L"choosePixelFormat() critical failure: could not create temporary OpenGL context.", L"Visualization Library error", MB_OK);
    DeleteDC(hDC);
    DestroyWindow(hWnd);
    return -1;
  }

  wglMakeCurrent(hDC, hGLRC);

  if (!initializeOpenGL())
  {
    fprintf(stderr, "Error initializing OpenGL!\n");
    DeleteDC(hDC);
    DestroyWindow(hWnd);
    return -1;
  }

  // if this is not supported we use the current 'pixel_format_index' returned by ChoosePixelFormat above.

  int samples = 0;
  if(Has_WGL_ARB_pixel_format && fmt.multisample())
  {
    float fAttributes[] = { 0, 0 };
    int iAttributes[] =
    {
      // multi sampling
	    WGL_SAMPLE_BUFFERS_ARB, GL_TRUE,
      WGL_SAMPLES_ARB,        -1, // this is set below
      // generic
	    WGL_DRAW_TO_WINDOW_ARB, GL_TRUE,
	    WGL_SUPPORT_OPENGL_ARB, GL_TRUE,
	    WGL_ACCELERATION_ARB,   WGL_FULL_ACCELERATION_ARB,
      // color buffer
      WGL_RED_BITS_ARB,         pfd.cRedBits,
      WGL_GREEN_BITS_ARB,       pfd.cGreenBits,
      WGL_BLUE_BITS_ARB,        pfd.cBlueBits,
      WGL_ALPHA_BITS_ARB,       pfd.cAlphaBits,
      // accumulation buffer
      WGL_ACCUM_RED_BITS_ARB,   pfd.cAccumRedBits,
      WGL_ACCUM_GREEN_BITS_ARB, pfd.cAccumGreenBits,
      WGL_ACCUM_BLUE_BITS_ARB,  pfd.cAccumBlueBits,
      WGL_ACCUM_ALPHA_BITS_ARB, pfd.cAccumAlphaBits,
      // depth buffer
      WGL_DEPTH_BITS_ARB,       pfd.cDepthBits,
      WGL_DOUBLE_BUFFER_ARB,    fmt.doubleBuffer() ? GL_TRUE : GL_FALSE,
      // stencil buffer
      WGL_STENCIL_BITS_ARB,     pfd.cStencilBits,
      // stereo
      WGL_STEREO_ARB,           fmt.stereo() ? GL_TRUE : GL_FALSE,
	    0,0
    };

    for(samples = fmt.multisampleSamples(); samples > 1; samples/=2)
    {
      // sets WGL_SAMPLES_ARB value
      iAttributes[3] = samples;
      pixel_format_index = -1;
      UINT num_formats  = 0;
      if ( wglChoosePixelFormatARB(hDC,iAttributes,fAttributes,1,&pixel_format_index,&num_formats) && num_formats >= 1 )
        break;
      else
        pixel_format_index = -1;
    }
  }

  // destroy temporary HWND, HDC, HGLRC
  if ( wglDeleteContext(hGLRC) == FALSE )
    if (verbose) MessageBox(NULL, L"Error deleting temporary OpenGL context, wglDeleteContext(hGLRC) failed.", L"Visualization Library error", MB_OK);
  DeleteDC(hDC);
  DestroyWindow(hWnd);

  if (verbose)
  {
    if(pixel_format_index == -1)
      vl::Log::error("No suitable pixel format found.\n");
    else
    {
      // check the returned pixel format
      #if defined(DEBUG) || !defined(NDEBUG)
        DescribePixelFormat(hDC, pixel_format_index, sizeof(PIXELFORMATDESCRIPTOR), &pfd);
        vl::Log::debug(" --- vlWin32::choosePixelFormat() ---\n");
        // This one returns "not supported" even when its supported...
        // vl::Log::print( vl::Say("  OpenGL        = %s\n") << (pfd.dwFlags & PFD_SUPPORT_OPENGL ? "Supported" : "Not supported") );
        vl::Log::debug( vl::Say("RGBA Bits     = %n %n %n %n\n") << pfd.cRedBits << pfd.cGreenBits << pfd.cBlueBits << pfd.cAlphaBits);
        vl::Log::debug( vl::Say("Depth Bits    = %n\n")  << pfd.cDepthBits );
        vl::Log::debug( vl::Say("Stencil Bits  = %n \n") << pfd.cStencilBits);
        vl::Log::debug( vl::Say("Double Buffer = %s\n")  << (pfd.dwFlags & PFD_DOUBLEBUFFER ? "Yes" : "No") );
        vl::Log::debug( vl::Say("Stereo        = %s\n")  << (pfd.dwFlags & PFD_STEREO ? "Yes" : "No") );
        vl::Log::debug( vl::Say("Samples       = %n\n")  << samples );
        vl::Log::debug("\n");
      #endif
    }
  }

  return pixel_format_index;
}
//-----------------------------------------------------------------------------

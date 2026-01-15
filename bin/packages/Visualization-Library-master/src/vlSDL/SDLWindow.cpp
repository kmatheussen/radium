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

#include "vlSDL/SDLWindow.hpp"
#include "vlGraphics/OpenGL.hpp"
#include "vlGraphics/Applet.hpp"
#include "vlCore/VisualizationLibrary.hpp"
#include "vlCore/Log.hpp"
#include "vlCore/Say.hpp"
#include <algorithm>
#include <SDL.h>
#include <map>

#ifdef WIN32
  #include <SDL_syswm.h>
  #include <shellapi.h>
#endif

using namespace vlSDL;
using namespace vl;

namespace
{
  SDLWindow* mSDLWindow = NULL;
  bool mUpdateFlag = true;

  std::map<int, vl::EKey> key_translation_map;

  int key_translation_vec[] =
  {
    SDLK_0, vl::Key_0,
    SDLK_1, vl::Key_1,
    SDLK_2, vl::Key_2,
    SDLK_3, vl::Key_3,
    SDLK_4, vl::Key_4,
    SDLK_5, vl::Key_5,
    SDLK_6, vl::Key_6,
    SDLK_7, vl::Key_7,
    SDLK_8, vl::Key_8,
    SDLK_9, vl::Key_9,

    SDLK_a, vl::Key_A,
    SDLK_b, vl::Key_B,
    SDLK_c, vl::Key_C,
    SDLK_d, vl::Key_D,
    SDLK_e, vl::Key_E,
    SDLK_f, vl::Key_F,
    SDLK_g, vl::Key_G,
    SDLK_h, vl::Key_H,
    SDLK_i, vl::Key_I,
    SDLK_j, vl::Key_J,
    SDLK_k, vl::Key_K,
    SDLK_l, vl::Key_L,
    SDLK_m, vl::Key_M,
    SDLK_n, vl::Key_N,
    SDLK_o, vl::Key_O,
    SDLK_p, vl::Key_P,
    SDLK_q, vl::Key_Q,
    SDLK_r, vl::Key_R,
    SDLK_s, vl::Key_S,
    SDLK_t, vl::Key_T,
    SDLK_u, vl::Key_U,
    SDLK_v, vl::Key_V,
    SDLK_w, vl::Key_W,
    SDLK_x, vl::Key_X,
    SDLK_y, vl::Key_Y,
    SDLK_z, vl::Key_Z,

    SDLK_RETURN,       vl::Key_Return,
    SDLK_BACKSPACE,    vl::Key_BackSpace,
    SDLK_SPACE,        vl::Key_Space,
    SDLK_TAB,          vl::Key_Tab,
    SDLK_CLEAR,        vl::Key_Clear,
    SDLK_ESCAPE,       vl::Key_Escape,
    SDLK_EXCLAIM,      vl::Key_Exclam,
    SDLK_QUOTEDBL,     vl::Key_QuoteDbl,
    SDLK_HASH,         vl::Key_Hash,
    SDLK_DOLLAR,       vl::Key_Dollar,
    SDLK_AMPERSAND,    vl::Key_Ampersand,
    SDLK_QUOTE,        vl::Key_Quote,
    SDLK_LEFTPAREN,    vl::Key_LeftParen,
    SDLK_RIGHTPAREN,   vl::Key_RightParen,
    SDLK_ASTERISK,     vl::Key_Asterisk,
    SDLK_PLUS,         vl::Key_Plus,
    SDLK_COMMA,        vl::Key_Comma,
    SDLK_MINUS,        vl::Key_Minus,
    SDLK_PERIOD,       vl::Key_Period,
    SDLK_SLASH,        vl::Key_Slash,
    SDLK_COLON,        vl::Key_Colon,
    SDLK_SEMICOLON,    vl::Key_Semicolon,
    SDLK_LESS,         vl::Key_Less,
    SDLK_EQUALS,       vl::Key_Equal,
    SDLK_GREATER,      vl::Key_Greater,
    SDLK_QUESTION,     vl::Key_Question,
    SDLK_AT,           vl::Key_At,
    SDLK_LEFTBRACKET,  vl::Key_LeftBracket,
    SDLK_BACKSLASH,    vl::Key_BackSlash,
    SDLK_RIGHTBRACKET, vl::Key_RightBracket,
    SDLK_CARET,        vl::Key_Caret,
    SDLK_UNDERSCORE,   vl::Key_Underscore,
    SDLK_BACKQUOTE,    vl::Key_QuoteLeft,

    // non unicode

    SDLK_LEFT,     vl::Key_Left,
    SDLK_RIGHT,    vl::Key_Right,
    SDLK_UP,       vl::Key_Up,
    SDLK_DOWN,     vl::Key_Down,
    SDLK_LCTRL,    vl::Key_LeftCtrl,
    SDLK_RCTRL,    vl::Key_RightCtrl,
    SDLK_LSHIFT,   vl::Key_LeftShift,
    SDLK_RSHIFT,   vl::Key_RightShift,
    SDLK_LALT,     vl::Key_LeftAlt,
    SDLK_RALT,     vl::Key_RightAlt,
    SDLK_INSERT,   vl::Key_Insert,
    SDLK_DELETE,   vl::Key_Delete,
    SDLK_HOME,     vl::Key_Home,
    SDLK_END,      vl::Key_End,
    SDLK_PAGEUP,   vl::Key_PageUp,
    SDLK_PAGEDOWN, vl::Key_PageDown,
    SDLK_PAUSE,    vl::Key_Pause,
    SDLK_PRINT,    vl::Key_Print,
    SDLK_F1,       vl::Key_F1,
    SDLK_F2,       vl::Key_F2,
    SDLK_F3,       vl::Key_F3,
    SDLK_F4,       vl::Key_F4,
    SDLK_F5,       vl::Key_F5,
    SDLK_F6,       vl::Key_F6,
    SDLK_F7,       vl::Key_F7,
    SDLK_F8,       vl::Key_F8,
    SDLK_F9,       vl::Key_F9,
    SDLK_F10,      vl::Key_F10,
    SDLK_F11,      vl::Key_F11,
    SDLK_F12,      vl::Key_F12,
    0,0
  };
}
//-----------------------------------------------------------------------------
SDL_Surface* SDLWindow::mScreen = NULL;
//-----------------------------------------------------------------------------
SDLWindow::SDLWindow()
{
}
//-----------------------------------------------------------------------------
SDLWindow::~SDLWindow()
{
}
//-----------------------------------------------------------------------------
SDLWindow::SDLWindow( const vl::String& title, const vl::OpenGLContextFormat& info, int /*x*/, int /*y*/, int width, int height)
{
  initSDLWindow(title, info, width, height);
}
//-----------------------------------------------------------------------------
bool SDLWindow::initSDLWindow(const vl::String& title, const vl::OpenGLContextFormat& info, int x, int y, int width, int height)
{
  if (mScreen || mSDLWindow)
  {
    vl::Log::error("SDL supports only one window at a time.\n");
    VL_TRAP();
    return false;
  }

  framebuffer()->setWidth(width);
  framebuffer()->setHeight(height);
  mSDLWindow = this;

  // init key translation map
  for(int i=0; key_translation_vec[i]; i+=2)
    key_translation_map[ key_translation_vec[i] ] = (vl::EKey)key_translation_vec[i+1];

  // SDL_VIDEO_WINDOW_POS

  char win_pos[32] = {0};
  sprintf ( win_pos, "SDL_VIDEO_WINDOW_POS=%d,%d", x, y );
  SDL_putenv(win_pos);
  // setenv("SDL_VIDEO_CENTERED", "YES", 0);

  // init SDL

  if ( SDL_Init(SDL_INIT_VIDEO) < 0 )
	{
    printf("Unable to init SDL: %s\n", SDL_GetError());
    return false;
  }

  SDL_GL_SetAttribute(SDL_GL_RED_SIZE,   info.rgbaBits().r());
  SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE, info.rgbaBits().g());
  SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE,  info.rgbaBits().b());
  SDL_GL_SetAttribute(SDL_GL_ALPHA_SIZE, info.rgbaBits().a());

  SDL_GL_SetAttribute(SDL_GL_ACCUM_RED_SIZE,   info.accumRGBABits().r());
  SDL_GL_SetAttribute(SDL_GL_ACCUM_GREEN_SIZE, info.accumRGBABits().g());
  SDL_GL_SetAttribute(SDL_GL_ACCUM_BLUE_SIZE,  info.accumRGBABits().b());
  SDL_GL_SetAttribute(SDL_GL_ACCUM_ALPHA_SIZE, info.accumRGBABits().a());

  SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, info.depthBufferBits());
  SDL_GL_SetAttribute(SDL_GL_STENCIL_SIZE, info.stencilBufferBits());

  SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, info.doubleBuffer()?1:0);
  SDL_GL_SetAttribute(SDL_GL_STEREO, info.stereo());
  SDL_GL_SetAttribute(SDL_GL_MULTISAMPLEBUFFERS, info.multisample()?1:0);
  if (info.multisample())
    SDL_GL_SetAttribute(SDL_GL_MULTISAMPLESAMPLES, info.multisampleSamples());
  SDL_GL_SetAttribute(SDL_GL_SWAP_CONTROL, info.vSync());

  int bpp = SDL_GetVideoInfo()->vfmt->BitsPerPixel;
  Uint32 flags = SDL_OPENGL | (info.fullscreen() ? SDL_FULLSCREEN : 0);
  width  = width  !=0 ? width  : SDL_GetVideoInfo()->current_w;
  height = height !=0 ? height : SDL_GetVideoInfo()->current_h;
  mScreen = SDL_SetVideoMode( width, height, bpp, flags );
  if (mScreen == 0)
  {
    vl::Log::print( vl::Say("\n  error: SDL_SetVideoMode(%n, %n, %n, %hn) failed: %s\n") << width << height << bpp << flags << SDL_GetError() );
    exit(1);
  }

  // window size problem

  int viewport[4];
  glGetIntegerv(GL_VIEWPORT,  viewport);
  VL_CHECK(viewport[0] == 0);
  VL_CHECK(viewport[1] == 0);
  if (viewport[2] != mScreen->w || viewport[3] != mScreen->h)
  {
    vl::Log::print( vl::Say("\n  warning: OpenGL reported %nx%n as video size but SDL says %nx%n\n") << viewport[2] << viewport[3] << mScreen->w << mScreen->h );
    VL_TRAP()
  }

  // OpenGL extensions initialization
  initGLContext();

  dispatchInitEvent();
  dispatchResizeEvent(width, height);

  #ifndef NDEBUG
    vl::Log::print( vl::Say("SDL screen: %n x %n x %n %s\n") << mScreen->w << mScreen->h << mScreen->format->BitsPerPixel << (info.fullscreen() ? "fullscreen" : "windowed") );
  #endif

  SDL_EnableUNICODE(1);

  #ifdef WIN32
    // file drag & drop support
	  SDL_EventState(SDL_SYSWMEVENT, SDL_ENABLE);
	  static SDL_SysWMinfo pInfo;
	  SDL_VERSION(&pInfo.version);
	  SDL_GetWMInfo(&pInfo);
	  HWND hWnd = pInfo.window;
	  DWORD ExStyle = GetWindowLong(hWnd, GWL_EXSTYLE);
	  ExStyle |= WS_EX_ACCEPTFILES;
	  SetWindowLong(hWnd, GWL_EXSTYLE, ExStyle);
    // The SDL gurus decided not to give us this option...
    // DWORD Style = GetWindowLong(hWnd, GWL_STYLE);
    // Style |= WS_OVERLAPPEDWINDOW | WS_CLIPSIBLINGS | WS_CLIPCHILDREN;
    // SetWindowLong(hWnd, GWL_STYLE, Style);
  #endif

  // mouse

  SDL_ShowCursor(true);
  SDL_WM_GrabInput(SDL_GRAB_OFF);
  setWindowTitle(title);

  // event cleaning

  SDL_PumpEvents();
  SDL_Event event;
  while ( SDL_PollEvent(&event) ) {}

  return true;
}
//-----------------------------------------------------------------------------
void SDLWindow::translateEvent( SDL_Event * ev )
{
  vl::EKey key = vl::Key_None;
  unsigned short unicode = 0;
  if ( ev && (ev->type == SDL_KEYDOWN || ev->type == SDL_KEYUP) )
  {
    if( key_translation_map.find(ev->key.keysym.sym) != key_translation_map.end() )
      key = key_translation_map[ ev->key.keysym.sym ];
    else
      key = vl::Key_Unknown;

    unicode = ev->key.keysym.unicode;
  }

  //// save it here cause the listeners could modify it via update() function
  //bool update = mUpdateFlag;
  //// schedule possible update, note that listeners can override this calling the update() function
  //mUpdateFlag = continuousUpdate();

  //if (update)
  //  dispatchRunEvent();

  if (ev->type == SDL_KEYDOWN)
  {
    switch(key)
    {
    default: break;

    case vl::Key_LeftCtrl:
    case vl::Key_RightCtrl:
      dispatchKeyPressEvent(unicode, vl::Key_Ctrl);
    break;

    case vl::Key_LeftShift:
    case vl::Key_RightShift:
      dispatchKeyPressEvent(unicode, vl::Key_Shift);
    break;

    case vl::Key_LeftAlt:
    case vl::Key_RightAlt:
      dispatchKeyPressEvent(unicode, vl::Key_Alt);
    break;
    }
    dispatchKeyPressEvent(unicode, key);
  }
  else
  if (ev->type == SDL_KEYUP)
  {
    switch(key)
    {
    default: break;

    case vl::Key_LeftCtrl:
    case vl::Key_RightCtrl:
      dispatchKeyReleaseEvent(unicode, vl::Key_Ctrl);
    break;

    case vl::Key_LeftShift:
    case vl::Key_RightShift:
      dispatchKeyReleaseEvent(unicode, vl::Key_Shift);
    break;

    case vl::Key_LeftAlt:
    case vl::Key_RightAlt:
      dispatchKeyReleaseEvent(unicode, vl::Key_Alt);
    break;
    }
    dispatchKeyReleaseEvent(unicode, key);
  }
  else
  if (ev->type == SDL_MOUSEBUTTONDOWN)
  {
    if (ev->button.button == SDL_BUTTON_WHEELUP)
      dispatchMouseWheelEvent(1);
    else
    if (ev->button.button == SDL_BUTTON_WHEELDOWN)
      dispatchMouseWheelEvent(-1);
    else
    if (ev->button.button == SDL_BUTTON_LEFT)
      dispatchMouseDownEvent(vl::LeftButton, ev->button.x, ev->button.y);
    else
    if (ev->button.button == SDL_BUTTON_RIGHT)
      dispatchMouseDownEvent(vl::RightButton, ev->button.x, ev->button.y);
    else
    if (ev->button.button == SDL_BUTTON_MIDDLE)
      dispatchMouseDownEvent(vl::MiddleButton, ev->button.x, ev->button.y);
  }
  else
  if (ev->type == SDL_MOUSEBUTTONUP)
  {
    // We only need SDL_MOUSEBUTTONDOWN's wheel messages
    /*if (ev->button.button == SDL_BUTTON_WHEELUP)
      mouseWheelEvent(1);
    else
    if (ev->button.button == SDL_BUTTON_WHEELDOWN)
      mouseWheelEvent(-1);
    else*/
    if (ev->button.button == SDL_BUTTON_LEFT)
      dispatchMouseUpEvent(vl::LeftButton, ev->button.x, ev->button.y);
    else
    if (ev->button.button == SDL_BUTTON_RIGHT)
      dispatchMouseUpEvent(vl::RightButton, ev->button.x, ev->button.y);
    else
    if (ev->button.button == SDL_BUTTON_MIDDLE)
      dispatchMouseUpEvent(vl::MiddleButton, ev->button.x, ev->button.y);
  }
  else
  if (ev->type == SDL_MOUSEMOTION)
  {
    dispatchMouseMoveEvent(ev->motion.x, ev->motion.y);
  }
  else
  if (ev->type == SDL_VIDEORESIZE)
  {
    // SDL is not able to resize OpenGL contexts
    // resizeEvent(ev->resize.w, ev->resize.h);
  }
  else
	if( ev->type == SDL_SYSWMEVENT )
	{
    #ifdef WIN32
      if (ev->syswm.msg->msg == WM_DROPFILES)
      {
			  HDROP hDrop = (HDROP) ev->syswm.msg->wParam;
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
        dispatchFileDroppedEvent(files);
		  }
    #endif
	}
  else
  if (ev->type == SDL_QUIT)
  {
    quitApplication();
  }
}
//-----------------------------------------------------------------------------
void vlSDL::messageLoop()
{
  SDL_Event ev;
  while(mSDLWindow)
  {
    if ( SDL_PollEvent(&ev) )
      mSDLWindow->translateEvent(&ev);
    else
    {
      if ( mUpdateFlag || mSDLWindow->continuousUpdate() )
      {
        mSDLWindow->dispatchRunEvent();
        mUpdateFlag = false;
      }
      else
      {
        // rest for 10ms if there are not events to process, and we don't need to repaint
        SDL_Delay(10);
      }
    }
  }

  SDL_Quit();
}
//-----------------------------------------------------------------------------
void SDLWindow::quitApplication()
{
  dispatchDestroyEvent();
  mSDLWindow = NULL;
}
//-----------------------------------------------------------------------------
void SDLWindow::update()
{
  mUpdateFlag = true;
}
//-----------------------------------------------------------------------------
void SDLWindow::setWindowTitle(const vl::String& title)
{
  SDL_WM_SetCaption(title.toStdString().c_str(), title.toStdString().c_str());
}
//-----------------------------------------------------------------------------
void SDLWindow::swapBuffers()
{
  SDL_GL_SwapBuffers();
}
//-----------------------------------------------------------------------------
void SDLWindow::setPosition(int x, int y)
{
  #ifdef WIN32
	  static SDL_SysWMinfo pInfo;
	  SDL_VERSION(&pInfo.version);
	  SDL_GetWMInfo(&pInfo);
	  HWND hWnd = pInfo.window;

	  SetWindowPos(hWnd, 0, x, y, 0, 0, SWP_NOSIZE );
  #endif
}
//-----------------------------------------------------------------------------
